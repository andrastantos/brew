# ----------------------------------------------------------------------------------------
# Simulator test for basic end-to-end execution of the binutils chain.
# ----------------------------------------------------------------------------------------

.global _start

.text
_start:
  $r1 <- $r1 - $r1
  $r2 <- $r1
  $r3 <- $r1 + 1
  $pc <- _loop
  SII
_loop:
  FILL

