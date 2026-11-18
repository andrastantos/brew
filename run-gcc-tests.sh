#!/bin/bash
# Runs the GCC testsuites (gcc, g++, libstdc++) in brew-gcc-build/ on the
# simulator and collects the results into test-results/<name>/
# (default name: gcc-<gcc short sha>-newlib-<newlib short sha>).
# Lives in the brew repo; brew-gcc, brew-newlib, brew-gcc-build/ (see
# build-toolchain.sh) and test-results/ are siblings of brew/.
#
# Usage: [PHASES="gcc g++ libstdc++"] [BLD=build-dir] [PREFIX=install-prefix] brew/run-gcc-tests.sh [name]
# The testsuites use the compiler and target libraries in the build tree, not
# the installed ones; PREFIX only supplies binutils and the simulator.
# PHASES re-runs a subset into an existing result dir (other results are kept).
set -o pipefail
TOP=$(cd "$(dirname "$0")/.." && pwd)  # the directory holding all the repo checkouts
BLD=${BLD:-$TOP/brew-gcc-build}
PREFIX=${PREFIX:-$HOME/brew-toolset}
export PATH=$PREFIX/bin:$PATH
export DEJAGNU=$TOP/brew/dejagnu/site.exp  # makes the brew-sim board available
# DejaGnu runs the compiler on a pty.  Since GCC 14, diagnostics about
# command-line options are printed before -fdiagnostics-plain-output takes
# effect, so on a capable terminal (e.g. TERM=screen-256color) they get
# OSC 8 hyperlinks and tests matching those messages fail
# (gcc.dg/Werror-13.c, Wcomplain-wrong-lang-*.c, pr91172.c).
export TERM=dumb GCC_URLS=no
J=-j$(nproc)
if ! command -v brew-none-elf-run > /dev/null; then
  echo "error: brew-none-elf-run not found in $PREFIX/bin" >&2; exit 1
fi

# Identify the sources the build dir was configured from (it may be a
# combined tree whose newlib/ and libgloss/ are symlinks into another checkout).
SRC=$(cd "$BLD" && cd "$(sed -n 's/^srcdir = //p' Makefile)" && pwd)
GSRC=$(readlink -f "$SRC/gcc/..")
NSRC=$(readlink -f "$SRC/newlib/..")
GSHA=$(git -C "$GSRC" rev-parse --short HEAD)
NSHA=$(git -C "$NSRC" rev-parse --short HEAD)
NAME=${1:-gcc-$GSHA-newlib-$NSHA}
OUT=$TOP/test-results/$NAME
mkdir -p "$OUT"

PHASES=${PHASES:-gcc g++ libstdc++}
RTF="--target_board=brew-sim"
cd "$BLD"
STAMP=$OUT/.start-stamp; touch "$STAMP"
run() { # phase make-args...
  local p=$1; shift
  [[ " $PHASES " == *" $p "* ]] || return 0
  echo "=== $(date +%T) $p testsuite"
  make -k "$@" > "$OUT/make-check-$p.log" 2>&1; echo "exit=$?" >> "$OUT/make-check-$p.log"
}
# check-gcc-c / check-gcc-c++ are the gcc and g++ halves of check-gcc; each is
# split into $J parallel runtest invocations and merged back into one .sum.
run gcc       $J check-gcc-c   RUNTESTFLAGS="$RTF"
run g++       $J check-gcc-c++ RUNTESTFLAGS="$RTF"
run libstdc++ $J check-target-libstdc++-v3 RUNTESTFLAGS="$RTF"

echo "=== $(date +%T) collecting results"
# Keep build-tree-relative paths so results from different runs can be diffed.
find gcc/testsuite brew-none-elf/libstdc++-v3/testsuite -maxdepth 2 \
     \( -name '*.sum' -o -name '*.log' \) -newer "$STAMP" -print 2>/dev/null | while read -r f; do
  mkdir -p "$OUT/$(dirname "$f")"; cp "$f" "$OUT/$f"
done
{
  echo "gcc    $(git -C "$GSRC" rev-parse HEAD) ($(git -C "$GSRC" branch --show-current), $GSRC)"
  echo "newlib $(git -C "$NSRC" rev-parse HEAD) ($(git -C "$NSRC" branch --show-current), $NSRC)"
  echo "uncommitted changes: gcc $(git -C "$GSRC" status --porcelain -uno | wc -l), newlib $(git -C "$NSRC" status --porcelain -uno | wc -l) files"
  echo "binutils: $("$PREFIX/bin/brew-none-elf-as" --version | head -1)  (from $PREFIX)"
  echo "run: $(date -Iseconds)  host gcc: $(gcc -dumpfullversion)  dejagnu: $(runtest --version | head -1)"
  echo
  for s in $(cd "$OUT" && find . -name '*.sum' | sort); do
    echo "== $s"; grep -E '^# of ' "$OUT/$s"
  done
} > "$OUT/SUMMARY.txt"
cat "$OUT/SUMMARY.txt"
echo "=== $(date +%T) done: $OUT"
