#!/bin/bash
# Builds the Brew toolchain: binutils/gdb/sim, then GCC with newlib+libgloss
# built in-tree (combined tree) in a single pass.
#
# Lives in the brew repo; expects brew-binutils-gdb, brew-gcc and brew-newlib to be checked
# out next to it (siblings of brew/). Build dirs and logs are created there too.
#
# Usage: brew/build-toolchain.sh [binutils] [gcc]     (default: both, in that order)
# Env:   PREFIX (default ~/brew-toolset), TARGET (default brew-none-elf),
#        CLEAN=1 to wipe a step's build directory and reconfigure from scratch.
#
# Build dirs: ../brew-binutils-build/, ../brew-gcc-build/   Logs: ../build-logs/
set -eo pipefail
TOP=$(cd "$(dirname "$0")/.." && pwd)  # the directory holding all the repo checkouts
TARGET=${TARGET:-brew-none-elf}
PREFIX=${PREFIX:-$HOME/brew-toolset}
LOGS=$TOP/build-logs
J=-j$(nproc)
export PATH=$PREFIX/bin:$PATH
# Host GCC >= 15 defaults to C23, which breaks old K&R-style sources (readline etc.)
export CFLAGS="-O2 -g -std=gnu17" CFLAGS_FOR_BUILD="-O2 -g -std=gnu17"

STEPS=("$@"); [ ${#STEPS[@]} -eq 0 ] && STEPS=(binutils gcc)
mkdir -p "$LOGS"

# Combined tree: GCC's top-level build picks up newlib/ and libgloss/ from its source dir.
link_newlib() {
  local d
  for d in newlib libgloss; do
    if [ -L "$TOP/brew-gcc/$d" ]; then continue; fi
    if [ -e "$TOP/brew-gcc/$d" ]; then echo "brew-gcc/$d exists and is not a symlink" >&2; exit 1; fi
    ln -s "../brew-newlib/$d" "$TOP/brew-gcc/$d"
    grep -qxF "/$d" "$TOP/brew-gcc/.git/info/exclude" || echo "/$d" >> "$TOP/brew-gcc/.git/info/exclude"
  done
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
      step binutils brew-binutils-build "$TOP/brew-binutils-gdb/configure" \
        --target=$TARGET --prefix=$PREFIX --no-recursion --enable-sim-profile=yes --disable-werror ;;
    gcc)
      link_newlib
      step gcc brew-gcc-build "$TOP/brew-gcc/configure" \
        --target=$TARGET --prefix=$PREFIX --enable-languages=c,c++ --with-newlib \
        --disable-shared --disable-libssp --disable-threads --disable-libgomp --disable-libatomic \
        --disable-werror ;;
    *) echo "unknown step: $s" >&2; exit 1 ;;
  esac
done
echo "=== ALL DONE"
