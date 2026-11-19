#!/bin/bash
# Runs all binutils-gdb testsuites in brew-binutils-build/ and collects the
# results into test-results/<name>/ (default name: <branch>-<short sha>).
# Lives in the brew repo; brew-binutils-gdb, brew-binutils-build/ (see
# build-toolchain.sh) and test-results/ are siblings of brew/.
#
# Usage: [FLAVOR=trunk] [PHASES="host sim gdb"] [BLD=build-dir] [PREFIX=install-prefix] brew/run-binutils-tests.sh [name]
# FLAVOR=trunk (see build-toolchain.sh) defaults BLD/PREFIX to the trunk flavor's build dir and prefix.
# gdb is tested on the simulator using the 'brew-sim' board; everything else
# uses the default (unix) board, as usual for a cross binutils.
# PHASES re-runs a subset into an existing result dir (other results are kept).
# The sim testsuite runs serially: in parallel mode its tests get a './' path
# prefix, sim_arch() misidentifies the arch and every test reports UNTESTED.
set -o pipefail
TOP=$(cd "$(dirname "$0")/.." && pwd)  # the directory holding all the repo checkouts
SRC=$TOP/brew-binutils-gdb
case ${FLAVOR:=stable} in stable) SUFFIX= ;; trunk) SUFFIX=-trunk ;; *) echo "unknown FLAVOR: $FLAVOR" >&2; exit 1 ;; esac
BLD=${BLD:-$TOP/brew-binutils-build$SUFFIX}  # override to test another build dir
PREFIX=${PREFIX:-$HOME/brew-toolset$SUFFIX}
export PATH=$PREFIX/bin:$PATH
export DEJAGNU=$TOP/brew/dejagnu/site.exp  # makes the brew-sim board available
J=-j$(nproc)
# Target compiler for the binutils/ld/libctf tests that compile C. Pass it
# explicitly: top-level configure only records it if it was on PATH at
# configure time (it isn't when binutils is built before gcc), and otherwise
# falls back to a non-existent brew-none-elf-cc, which silently turns those
# tests into UNTESTED/UNSUPPORTED.
CC_FOR_TARGET=${CC_FOR_TARGET:-brew-none-elf-gcc}
if ! command -v "$CC_FOR_TARGET" > /dev/null; then
  echo "error: target compiler '$CC_FOR_TARGET' not found on PATH (build gcc first, or set CC_FOR_TARGET)" >&2
  exit 1
fi

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
run host $J $HOST_CHECKS CC_FOR_TARGET="$CC_FOR_TARGET"
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
