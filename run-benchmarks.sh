#!/bin/bash
# Records a code-quality/library baseline for the installed toolchain into
# test-results/<name>/bench/ (default name: gcc-<gcc sha>-newlib-<newlib sha>,
# matching run-gcc-tests.sh):
#   sizes.txt   .text/.data/.bss of each benchmark's own objects and linked ELF
#   insns.txt   dynamic instruction counts on the simulator (--trace-insn)
#   abi.txt     newlib type sizes/offsets and constants (abi/abi-layout.c)
# Diff the directory against an earlier run.
#
# Only dhrystone runs: linpack's clock() hits the unimplemented times syscall
# (SIM_ASSERT in sim/brew/models), and llloops' timer calibration never ends
# without a working clock.  Built with -DSIM, dhrystone skips its timing and
# result printing, so the run checks completion and instruction counts only.
#
# Usage: [PREFIX=install-prefix] [OPTS="-O0 -O2 -Os"] brew/run-benchmarks.sh [name]
set -eo pipefail
TOP=$(cd "$(dirname "$0")/.." && pwd)
PREFIX=${PREFIX:-$HOME/brew-toolset}
export PATH=$PREFIX/bin:$PATH
T=brew-none-elf
OPTS=${OPTS:--O0 -O2 -Os}
GSHA=$(git -C "$TOP/brew-gcc" rev-parse --short HEAD)
NSHA=$(git -C "$TOP/brew-newlib" rev-parse --short HEAD)
NAME=${1:-gcc-$GSHA-newlib-$NSHA}
OUT=$TOP/test-results/$NAME/bench
B=$TOP/brew/benchmarks
rm -rf "$OUT"; mkdir -p "$OUT/build"; cd "$OUT/build"

# bench  compiler  extra-flags  sources...
benches=(
  "dhrystone gcc -DSIM dhrystone/dhry_1.c dhrystone/dhry_2.c"
  "linpack gcc -DSIM_-msoft-float linpack/linpack.c"
  "llloops g++ -DSIM_-msoft-float llloops/llloops.cpp"
)
: > ../sizes.txt; : > ../insns.txt
for opt in $OPTS; do
  for spec in "${benches[@]}"; do
    read -r name cc flags srcs <<< "$spec"; flags=${flags//_/ }
    objs=()
    for s in $srcs; do
      o=$name$opt-$(basename "${s%.*}").o
      $T-$cc $opt $flags -w -c "$B/$s" -o "$o"; objs+=("$o")
    done
    $T-$cc $opt "${objs[@]}" -lc -lm -lbrew -o "$name$opt.elf"
    $T-size "${objs[@]}" "$name$opt.elf" | sed 1d | awk '{printf "%-28s text %7d data %6d bss %7d\n", $6, $1, $2, $3}' >> ../sizes.txt
  done
  # The exit status is not checked: dhrystone's main() has no return statement.
  n=$({ $T-run --trace-insn "dhrystone$opt.elf" 2>&1 >/dev/null || true; } | grep -c '^insn:')
  printf "%-20s %10d insns\n" "dhrystone$opt" "$n" >> ../insns.txt
done
$T-gcc -O1 "$TOP/brew/abi/abi-layout.c" -o abi.elf
$T-run abi.elf > ../abi.txt
{ echo "gcc $GSHA  newlib $NSHA  $($T-as --version | head -1)"; } > ../VERSION.txt
cat ../insns.txt; echo "results: $OUT"
