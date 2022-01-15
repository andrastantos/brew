# ----------------------------------------------------------------------------------------
# Writes "Hello, World" to the console using only system calls. Runs on 64-bit Linux only.
# To assemble and run:
#
#     gcc -c hello.s && ld hello.o && ./a.out
#
# or
#
#     gcc -nostdlib hello.s && ./a.out
# ----------------------------------------------------------------------------------------

  .global _start

  .text
_start:
  FENCE
  SII
  MEM8[_start, $fr0] <- $r0
  MEM8[$fr0] <- $r0
  $sr0 <- MEM8[$r1,message]
  $tpc <- MEM8[$r1]
  $r13 <- MEM16[$pc]
  if $r3[12] == 1 $pc <- 0
  if $r3[12] == 0 $pc <- message
  if $r3 >= $r1 $pc <- 0
  if $r3 <= $r1 $pc <- 0
  if $sr3 <= 0 $pc <- message
  $r5 <- $r3 ^ $r1
  $r5 <- $r3
  $r5 <- 123
  $pc <- 111
  $tpc <- 444
  $r1 <- 3 + $r2
  $r1 <- $r2 + 4
  $r1 <- 3 - $r2
  $r1 <- $r2 - 4
  $r6 <- ~$r5
  $sr1 <- -$sr4
  $r1 <- $r2 + 4
  $r1 <- $r2 - 4
  $r1 <- $r2 + 1
  $r1 <- $r2 - 1
  $sr1 <- upper $sr3 * $sr2
  $sr1 <- $sr3 * $sr2
  $r1 <- upper $r3 * $r2
  $r1 <- $r3 * $r2
  $fr1 <- $fr3 + $fr2
  $fr1 <- $fr3 - $fr2
  $fr1 <- $fr3 * $fr2
  $fr3 <- 1 / $fr1
  $fr3 <- RSQRT $fr1
  $sr3 <- floor $fr1
  $fr3 <- $sr10
  $r2 <- bswap $r4
  $r2 <- wswap $r4
  $tpc <- $r3
  $r3 <- $tpc
  #$pc <- $tpc # This should err out
  $fr2 <- $fr1 + 3.1415
  #$r2 <- $r1 + 1.2 # This should err out
  #MEM8[$fr0, message+1] <- $r0 #This fails: we need to put parens around expressions
  MEM8[$fr0, message] <- $r0
  MEM8[$fr0, (message+1)] <- $r0

message:
  .word 0, 0, 0 , 0
  .float 3.1415
        #.ascii  "Hello, world\n"
