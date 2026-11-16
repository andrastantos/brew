#!/bin/bash
# Runs all binutils-gdb testsuites in brew-binutils-build/ and collects the
# results into test-results/<name>/ (default name: <branch>-<short sha>).
# Lives in the brew repo; brew-binutils-gdb, brew-binutils-build/ (see
# build-toolchain.sh) and test-results/ are siblings of brew/.
#
# Usage: [PHASES="host sim gdb"] brew/run-binutils-tests.sh [name]
# gdb is tested on the simulator using the 'brew-sim' board; everything else
# uses the default (unix) board, as usual for a cross binutils.
# PHASES re-runs a subset into an existing result dir (other results are kept).
# The sim testsuite runs serially: in parallel mode its tests get a './' path
# prefix, sim_arch() misidentifies the arch and every test reports UNTESTED.
set -o pipefail
TOP=$(cd "$(dirname "$0")/.." && pwd)  # the directory holding all the repo checkouts
SRC=$TOP/brew-binutils-gdb
BLD=$TOP/brew-binutils-build
PREFIX=${PREFIX:-$HOME/brew-toolset}
export PATH=$PREFIX/bin:$PATH
export DEJAGNU=$TOP/brew/dejagnu/site.exp  # makes the brew-sim board available
J=-j$(nproc)

SHA=$(git -C "$SRC" rev-parse --short HEAD)
NAME=${1:-$(git -C "$SRC" branch --show-current)-$SHA}
OUT=$TOP/test-results/$NAME
mkdir -p "$OUT"

PHASES=${PHASES:-host sim gdb}
# Every check target except sim and gdb (run separately below).
HOST_CHECKS="check-bfd check-opcodes check-binutils check-gas check-ld check-gprof
  check-libctf check-libiberty check-libbacktrace check-gdbsupport
  check-gnulib check-zlib check-readline check-libdecnumber check-intl check-etc"

cd "$BLD"
STAMP=$OUT/.start-stamp; touch "$STAMP"
run() { # phase make-args...
  local p=$1; shift
  [[ " $PHASES " == *" $p "* ]] || return 0
  echo "=== $(date +%T) $p testsuites"
  make -k "$@" > "$OUT/make-check-$p.log" 2>&1; echo "exit=$?" >> "$OUT/make-check-$p.log"
}
run host $J $HOST_CHECKS
run sim  check-sim
run gdb  $J check-gdb RUNTESTFLAGS="--target_board=brew-sim"

echo "=== $(date +%T) collecting results"
[[ " $PHASES " == *" sim "* ]] && rm -rf "$OUT/sim"
# Keep build-tree-relative paths so results from different runs can be diffed.
find . \( -name '*.sum' -o -name '*.log' \) ! -name config.log -newer "$STAMP" -print | while read -r f; do
  mkdir -p "$OUT/$(dirname "$f")"; cp "$f" "$OUT/$f"
done
{
  echo "binutils-gdb $(git -C "$SRC" rev-parse HEAD) ($(git -C "$SRC" branch --show-current))"
  echo "run: $(date -Iseconds)  host gcc: $(gcc -dumpfullversion)  dejagnu: $(runtest --version | head -1)"
  echo
  for s in $(cd "$OUT" && find . -name '*.sum' | sort); do
    echo "== $s"; grep -E '^# of ' "$OUT/$s"
  done
} > "$OUT/SUMMARY.txt"
cat "$OUT/SUMMARY.txt"
echo "=== $(date +%T) done: $OUT"
