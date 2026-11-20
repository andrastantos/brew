#!/bin/bash
# Builds the Brew toolchain: binutils/gdb/sim, then GCC with newlib+libgloss
# built in-tree (combined tree) in a single pass.
#
# Lives in the brew repo; expects brew-binutils-gdb, brew-gcc and brew-newlib to be checked
# out next to it (siblings of brew/). Build dirs and logs are created there too.
#
# Two flavors share the binutils-gdb and newlib checkouts:
#   FLAVOR=stable (default): GCC from brew-gcc/ (releases/gcc-16), installed to ~/brew-toolset.
#   FLAVOR=trunk:            GCC from brew-gcc-trunk/ (a worktree of brew-gcc master),
#                            installed to ~/brew-toolset-trunk.
# Each flavor has its own build dirs and a complete prefix (binutils included).
#
# Usage: [FLAVOR=trunk] brew/build-toolchain.sh [binutils] [gcc]     (default: both, in that order)
# Env:   PREFIX (default per flavor), TARGET (default brew-none-elf),
#        CLEAN=1 to wipe a step's build directory and reconfigure from scratch.
#
# Build dirs: ../brew-binutils-build[-trunk]/, ../brew-gcc-build[-trunk]/   Logs: ../build-logs/
set -eo pipefail
TOP=$(cd "$(dirname "$0")/.." && pwd)  # the directory holding all the repo checkouts
TARGET=${TARGET:-brew-none-elf}
case ${FLAVOR:=stable} in
  stable) GCC_SRC=brew-gcc;       SUFFIX= ;;
  trunk)  GCC_SRC=brew-gcc-trunk; SUFFIX=-trunk ;;
  *) echo "unknown FLAVOR: $FLAVOR (stable or trunk)" >&2; exit 1 ;;
esac
PREFIX=${PREFIX:-$HOME/brew-toolset$SUFFIX}
LOGS=$TOP/build-logs
J=-j$(nproc)
export PATH=$PREFIX/bin:$PATH
# Host GCC >= 15 defaults to C23, which breaks old K&R-style sources (readline etc.)
export CFLAGS="-O2 -g -std=gnu17" CFLAGS_FOR_BUILD="-O2 -g -std=gnu17"

STEPS=("$@"); [ ${#STEPS[@]} -eq 0 ] && STEPS=(binutils gcc)
mkdir -p "$LOGS"

# Combined tree: GCC's top-level build picks up newlib/ and libgloss/ from its source dir.
link_newlib() {
  local d src=$TOP/$GCC_SRC
  local exclude; exclude=$(git -C "$src" rev-parse --path-format=absolute --git-common-dir)/info/exclude
  for d in newlib libgloss; do
    if [ -L "$src/$d" ]; then
      [ "$(readlink "$src/$d")" = "../brew-newlib/$d" ] && continue
      echo "repointing $GCC_SRC/$d (was $(readlink "$src/$d"))"; rm "$src/$d"
    elif [ -e "$src/$d" ]; then echo "$GCC_SRC/$d exists and is not a symlink" >&2; exit 1; fi
    ln -s "../brew-newlib/$d" "$src/$d"
    grep -qxF "/$d" "$exclude" || echo "/$d" >> "$exclude"
  done
}

# Configure GCC (run in the build dir).  libgcc, and so libgcov, is built before newlib, so
# without newlib headers at configure time GCC sets inhibit_libc and libgcov is a stub
# (no coverage, ~1,900 failing gcov tests).  --with-headers gives it the headers of the
# newlib already installed in the prefix (staged without libstdc++'s c++/); the very first
# build of a prefix has none, so build twice (CLEAN=1 the second time) to get a real libgcov.
gcc_configure() {
  local inc=$PREFIX/$TARGET/include hdr=()
  if [ -f "$inc/newlib.h" ]; then
    rm -rf newlib-headers; mkdir newlib-headers
    (cd "$inc" && tar cf - --exclude=./c++ .) | tar xf - -C newlib-headers
    hdr=(--with-headers="$PWD/newlib-headers")
  fi
  "$TOP/$GCC_SRC/configure" "$@" "${hdr[@]}"
}

step() { # name build-dir configure-cmd...
  local name=$1 dir=$TOP/$2; shift 2
  [ "${CLEAN:-0}" = 1 ] && rm -rf "$dir"
  mkdir -p "$dir"; cd "$dir"
  if [ ! -f config.status ]; then
    echo "=== $(date +%T) $name: configure"
    "$@" > "$LOGS/$name-configure.log" 2>&1
  fi
  echo "=== $(date +%T) $name: make"
  make $J > "$LOGS/$name-make.log" 2>&1
  echo "=== $(date +%T) $name: install"
  make install > "$LOGS/$name-install.log" 2>&1
  echo "=== $(date +%T) $name: done"
}

for s in "${STEPS[@]}"; do
  case $s in
    binutils)
      step binutils$SUFFIX brew-binutils-build$SUFFIX "$TOP/brew-binutils-gdb/configure" \
        --target=$TARGET --prefix=$PREFIX --no-recursion --enable-sim-profile=yes --disable-werror \
        --with-python=python3 ;;
    gcc)
      link_newlib
      if [ ! -f "$PREFIX/$TARGET/include/newlib.h" ] &&
         { [ "${CLEAN:-0}" = 1 ] || [ ! -f "$TOP/brew-gcc-build$SUFFIX/config.status" ]; }; then
        echo "WARNING: no newlib headers in $PREFIX/$TARGET/include yet, so libgcov will be a stub;" \
             "run CLEAN=1 $0 gcc again after this build" >&2
      fi
      step gcc$SUFFIX brew-gcc-build$SUFFIX gcc_configure \
        --target=$TARGET --prefix=$PREFIX --enable-languages=c,c++ --with-newlib \
        --disable-shared --disable-libssp --disable-threads --disable-libgomp --disable-libatomic \
        --disable-werror
      # configure copied the staged headers to sys-include, which the driver searches before
      # include/; drop it so the freshly installed newlib headers are the only ones.
      rm -rf "$PREFIX/$TARGET/sys-include" ;;
    *) echo "unknown step: $s" >&2; exit 1 ;;
  esac
done
echo "=== ALL DONE"
