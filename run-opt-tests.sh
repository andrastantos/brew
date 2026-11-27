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
# Code placement: Espresso's fetch bursts end at 256-byte page boundaries, so
# moving the same code by a few bytes changes a test's cycles by 2-5% (up to
# ~14% when a hot loop straddles a boundary), and geometric means by ~0.3%;
# executed instructions and branches don't change.  PADS=N links every test
# N times, with 0, 256/N, 2*256/N, ... bytes of padding in front of the
# test's own code (and so of the libraries linked after it), runs all N, and
# reports the mean cycles in results.txt; cycles-pads.txt has each offset's
# cycles and the spread.  The mix, timing and asm reports are from offset 0.
# Use PADS=8 or so to judge small cycle differences.
#
# Usage: [FLAVOR=trunk] [PREFIX=install-prefix] [OPTS="-O2 -Os"] [PADS=1]
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
PADS=${PADS:-1}
PAGE=256   # Espresso's fetch page (InstBuffer page_bits = 7 16-bit words)
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

# Padding objects for PADS > 1: offset k * PAGE / PADS, rounded down to even
# (instructions are 16-bit aligned).  Offset 0 links no padding object.
pads=()
for ((k = 0; k < PADS; k++)); do
  p=$(( (k * PAGE / PADS) & ~1 ))
  pads+=("$p")
  if [ "$p" -gt 0 ]; then
    printf '\t.text\n\t.skip %d\n\t.section .note.GNU-stack,"",@progbits\n' "$p" > "pad$p.s"
    $T-as "pad$p.s" -o "pad$p.o"
  fi
done
[ "$PADS" -gt 1 ] && printf "%-16s %-4s %10s %7s   %s\n" "# test" opt mean spread "cycles at padding ${pads[*]}" > ../cycles-pads.txt

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
    text=$($T-size "${objs[@]}" | awk 'NR > 1 { n += $1 } END { print n }')
    # Link and run once per padding offset, in parallel.  'run' prints its
    # profile only with -v.  stdout goes to /dev/null, as in run-benchmarks.sh
    # (newlib's stdio buffering depends on it).
    for p in "${pads[@]}"; do
      e=$t$opt-p$p
      padobj=(); [ "$p" -gt 0 ] && padobj=("pad$p.o")
      ( $T-gcc $opt $OPT_CFLAGS "${padobj[@]}" "${objs[@]}" -lc -lm -o "$e.elf"
        rc=0
        $T-run -v --profile-model --profile-insn --timing=espresso --profile-file "$e.prof" "$e.elf" > /dev/null 2>&1 || rc=$?
        echo $rc > "$e.rc" ) &
    done
    wait
    num() { sed -n "s/^ *$1: *\([0-9,]*\).*/\1/p" "$2" | head -1 | tr -d ,; }
    p0=$t$opt-p0
    sed -n '/^Instruction Statistics/,/^\(Espresso Timing\|Simulator Execution Speed\)/p' "$p0.prof" | sed '$d' > "../mix/$t$opt.txt"
    sed -n '/^Espresso Timing/,/^Simulator Execution Speed/p' "$p0.prof" | sed '$d' > "../timing/$t$opt.txt"
    insns=$(num Total "$p0.prof") taken=$(num "Taken branches" "$p0.prof") untaken=$(num "Untaken branches" "$p0.prof")
    rc=0 all=() sum=0
    for p in "${pads[@]}"; do
      e=$t$opt-p$p
      c=$(num "Total cycles" "$e.prof")
      if [ -z "$(num Total "$e.prof")" ] || [ -z "$c" ]; then
        echo "error: no profile from $T-run for $e (needs --profile-insn and --timing=espresso, binutils-gdb 2026-11-24)" >&2
        exit 1
      fi
      if [ "$(num Total "$e.prof")" != "$insns" ] || [ "$(num "Taken branches" "$e.prof")" != "$taken" ]; then
        echo "error: $t$opt executes differently with $p bytes of padding" >&2
        exit 1
      fi
      [ "$(cat "$e.rc")" -ne 0 ] && rc=$(cat "$e.rc")
      all+=("$c"); sum=$((sum + c))
    done
    cycles=$(( (sum + PADS / 2) / PADS ))
    if [ "$PADS" -gt 1 ]; then
      spread=$(printf '%s\n' "${all[@]}" | sort -n | awk 'NR == 1 { lo = $1 } { hi = $1 } END { printf "%.2f%%", 100 * (hi / lo - 1) }')
      printf "%-16s %-4s %10d %7s   %s\n" "$t" "${opt#-}" "$cycles" "$spread" "${all[*]}" >> ../cycles-pads.txt
    fi
    if [ "$t" = dhrystone ]; then status=-
    elif [ $rc -eq 0 ]; then status=ok
    else status=FAIL; fails=$((fails + 1)); fi
    printf "%-16s %-4s %-6s %10d %9d %9d %7d %10d\n" "$t" "${opt#-}" "$status" "$insns" "$taken" "$untaken" "$text" "$cycles" >> ../results.txt
  done
done
{ echo "gcc $GSHA  newlib $NSHA  $($T-as --version | head -1)"; echo "OPT_CFLAGS=$OPT_CFLAGS"
  echo "PADS=$PADS (padding ${pads[*]}; cycles are the mean)"; } > ../VERSION.txt
cat ../results.txt; echo "results: $OUT"
if [ $fails -ne 0 ]; then echo "$fails test(s) FAILED" >&2; exit 1; fi
