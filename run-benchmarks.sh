#!/bin/bash
# Records a code-quality/library baseline for the installed toolchain into
# test-results/<name>/bench/ (default name: gcc-<gcc sha>-newlib-<newlib sha>,
# matching run-gcc-tests.sh):
#   sizes.txt   .text/.data/.bss of each benchmark's own objects and linked ELF
#   insns.txt   dynamic instruction counts on the simulator
#   mix-<opt>.txt instruction mix of each dhrystone build, by class and by
#               form (the simulator's --profile-insn report)
#   abi.txt     newlib type sizes/offsets and constants (abi/abi-layout.c)
# Diff the directory against an earlier run.
#
# Only dhrystone runs: linpack takes minutes on the simulator, and llloops'
# timer calibration never ends there.  Built with -DSIM, dhrystone skips its
# timing and result printing, so the run checks completion and instruction
# counts only.  The program's stdout goes to /dev/null: newlib's stdio picks
# its buffering from fstat, so a pipe or a terminal changes the counts.
#
# Usage: [FLAVOR=trunk] [PREFIX=install-prefix] [OPTS="-O0 -O2 -Os"] brew/run-benchmarks.sh [name]
# FLAVOR=trunk (see build-toolchain.sh) defaults PREFIX to ~/brew-toolset-trunk.
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
OPTS=${OPTS:--O0 -O2 -Os}
# The recorded commits are the flavor's checkouts; with another PREFIX they may not match.
GSHA=$(git -C "$TOP/$GCC_SRC" rev-parse --short HEAD)
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
    $T-$cc $opt "${objs[@]}" -lc -lm -o "$name$opt.elf"
    $T-size "${objs[@]}" "$name$opt.elf" | sed 1d | awk '{printf "%-28s text %7d data %6d bss %7d\n", $6, $1, $2, $3}' >> ../sizes.txt
  done
  # The exit status is not checked: dhrystone's main() has no return statement.
  # 'run' prints its profile only with -v; keep the instruction statistics
  # (the report's other sections hold host timings).
  $T-run -v --profile-insn --profile-file "dhrystone$opt.prof" "dhrystone$opt.elf" >/dev/null 2>&1 || true
  sed -n '/^Instruction Statistics/,/^Simulator Execution Speed/p' "dhrystone$opt.prof" | sed '$d' > "../mix$opt.txt"
  n=$(sed -n 's/^  Total: \([0-9,]*\) insns$/\1/p' "../mix$opt.txt" | tr -d ,)
  if [ -z "$n" ]; then
    echo "error: no instruction profile from $T-run (needs binutils-gdb with --profile-insn, 2026-11-22)" >&2
    exit 1
  fi
  printf "%-20s %10d insns\n" "dhrystone$opt" "$n" >> ../insns.txt
done
$T-gcc -O1 "$TOP/brew/abi/abi-layout.c" -o abi.elf
$T-run abi.elf > ../abi.txt
{ echo "gcc $GSHA  newlib $NSHA  $($T-as --version | head -1)"; } > ../VERSION.txt
cat ../insns.txt; echo "results: $OUT"
