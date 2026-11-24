#!/bin/bash
# Builds and runs the code-generation test cases in benchmarks/opt/ (plus
# dhrystone) with the installed toolchain and records, per test and
# optimization level, what generic Brew optimization work aims to reduce:
#   results.txt     status, executed instructions, taken and untaken
#                   branches, the size of the test's own code, and cycles
#                   on the simulator's Espresso cycle model
#                   (--timing=espresso: DRAM, no wait states)
#   mix/<t><opt>.txt  instruction mix (the simulator's --profile-insn report)
#   timing/<t><opt>.txt where the cycles went (the --timing=espresso report)
#   asm/<t><opt>.s  the compiler's assembly output for the test, for diffing
# into test-results/<name>/opt/ (default name: gcc-<gcc sha>-newlib-<newlib
# sha>, as run-gcc-tests.sh).  Compare two runs with compare-opt-tests.py.
#
# "taken" counts every instruction that redirected the PC: taken
# conditional branches, jumps, calls, returns and syscalls.  "untaken"
# counts conditional branches that fell through.  On Espresso a taken
# branch costs 9 extra cycles and a fall-through nothing, so for equal
# instruction counts fewer taken branches is better.  empty.c measures the
# startup cost included in every other test.
#
# Every test checks its own result: status FAIL means wrong code (or a
# crash), not a slowdown.  dhrystone's status is not checked ('-').
# Library code (newlib, libgcc) is the installed one: rebuild the
# toolchain with CLEAN=1 to measure a compiler change on it too.
#
# Usage: [FLAVOR=trunk] [PREFIX=install-prefix] [OPTS="-O2 -Os"]
#        [OPT_CFLAGS=...] [TESTS="crc32_table loops"] brew/run-opt-tests.sh [name]
# OPT_CFLAGS (e.g. -mtune=espresso) is added to every compile and link.
set -eo pipefail
TOP=$(cd "$(dirname "$0")/.." && pwd)
case ${FLAVOR:=stable} in
  stable) SUFFIX= GCC_SRC=brew-gcc ;;
  trunk) SUFFIX=-trunk GCC_SRC=brew-gcc-trunk ;;
  *) echo "unknown FLAVOR: $FLAVOR" >&2; exit 1 ;;
esac
PREFIX=${PREFIX:-$HOME/brew-toolset$SUFFIX}
export PATH=$PREFIX/bin:$PATH
T=brew-none-elf
OPTS=${OPTS:--O2 -Os}
GSHA=$(git -C "$TOP/$GCC_SRC" rev-parse --short HEAD)
NSHA=$(git -C "$TOP/brew-newlib" rev-parse --short HEAD)
NAME=${1:-gcc-$GSHA-newlib-$NSHA}
OUT=$TOP/test-results/$NAME/opt
SRC=$TOP/brew/benchmarks/opt
DHRY=$TOP/brew/benchmarks/dhrystone
if [ -z "$TESTS" ]; then
  TESTS="$(cd "$SRC" && ls *.c | sed 's/\.c$//' | tr '\n' ' ')dhrystone"
fi
rm -rf "$OUT"; mkdir -p "$OUT/build" "$OUT/mix" "$OUT/timing" "$OUT/asm"; cd "$OUT/build"

fails=0
printf "%-16s %-4s %-6s %10s %9s %9s %7s %10s\n" "# test" opt status insns taken untaken text cycles > ../results.txt
for opt in $OPTS; do
  for t in $TESTS; do
    if [ "$t" = dhrystone ]; then
      srcs="$DHRY/dhry_1.c $DHRY/dhry_2.c" flags="-DSIM -w"
    else
      srcs="$SRC/$t.c" flags="-Wall"
    fi
    objs=()
    for s in $srcs; do
      o=$t$opt-$(basename "${s%.c}").o
      $T-gcc $opt $OPT_CFLAGS $flags -c "$s" -o "$o"
      $T-gcc $opt $OPT_CFLAGS $flags -S "$s" -o - >> "../asm/$t$opt.s"
      objs+=("$o")
    done
    $T-gcc $opt $OPT_CFLAGS "${objs[@]}" -lc -lm -o "$t$opt.elf"
    text=$($T-size "${objs[@]}" | awk 'NR > 1 { n += $1 } END { print n }')
    # 'run' prints its profile only with -v.  stdout goes to /dev/null, as in
    # run-benchmarks.sh (newlib's stdio buffering depends on it).
    rc=0
    $T-run -v --profile-model --profile-insn --timing=espresso --profile-file "$t$opt.prof" "$t$opt.elf" > /dev/null 2>&1 || rc=$?
    sed -n '/^Instruction Statistics/,/^\(Espresso Timing\|Simulator Execution Speed\)/p' "$t$opt.prof" | sed '$d' > "../mix/$t$opt.txt"
    sed -n '/^Espresso Timing/,/^Simulator Execution Speed/p' "$t$opt.prof" | sed '$d' > "../timing/$t$opt.txt"
    num() { sed -n "s/^ *$1: *\([0-9,]*\).*/\1/p" "$t$opt.prof" | head -1 | tr -d ,; }
    insns=$(num Total) taken=$(num "Taken branches") untaken=$(num "Untaken branches")
    cycles=$(num "Total cycles")
    if [ -z "$insns" ] || [ -z "$taken" ] || [ -z "$cycles" ]; then
      echo "error: no profile from $T-run for $t$opt (needs --profile-insn and --timing=espresso, binutils-gdb 2026-11-24)" >&2
      exit 1
    fi
    if [ "$t" = dhrystone ]; then status=-
    elif [ $rc -eq 0 ]; then status=ok
    else status=FAIL; fails=$((fails + 1)); fi
    printf "%-16s %-4s %-6s %10d %9d %9d %7d %10d\n" "$t" "${opt#-}" "$status" "$insns" "$taken" "$untaken" "$text" "$cycles" >> ../results.txt
  done
done
{ echo "gcc $GSHA  newlib $NSHA  $($T-as --version | head -1)"; echo "OPT_CFLAGS=$OPT_CFLAGS"; } > ../VERSION.txt
cat ../results.txt; echo "results: $OUT"
if [ $fails -ne 0 ]; then echo "$fails test(s) FAILED" >&2; exit 1; fi
