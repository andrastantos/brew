.text
    .global _start
    _start:
        if signed $sp <= 0 $pc <- _start
        if $r2[3] == 0 $pc <- _start
        if $r0 == $r0 $pc <- _start
lll:
        if $r0 == $r0 $pc <- lll
#	.p2align	1
#        type $r1 <- int16x2
#        $r1 <- 0x0100ffff
#        $r2 <- 0x02000001
#        $r3 <- $r1 + $r2
#        $r4 <- $r2 + $r1
#        fill
#        swi 5
#        $pc <- mem[$r2]
#        $pc <- mem[$r3+15]
#        $pc <- mem[0x1315]
#        $pc <- _start+12
#        sii
#        $tpc <- short 0x3ead
#        type $r11 <- $r3
#        type $r10 <- type fp32
#        $r3 <- $pc + 4
#        $r5 <- $pc
#        $r7 <- $tpc
#        $r1 <- $r2 * $r3
#        $r2 <- $r10 + 0x1234
#        $r2 <- 0x1234 + $r10
#        $r3 <- short $r9 + 0x1234
#        $r3 <- short 0x1234 + $r9
#        $r3 <- $r10 > 0
#        $r5 <- ~$r7&$r10
#        $r1 <- - $r5
#        $r5 <- full $r1 * $r2 >>> 0xe
#        $r5 <- lane_swizzle $r2, 0123
#        $r3 <- tiny $r5 + -1
#        $r3 <- tiny $r5 + 3
#        $r5 <- interpolate $r1, $r2
#
#        $r4 <- smem8[0x1234]
#        $r4 <- mem8[0x1234]
#        $r4 <- smem16[0x1234]
#        $r4 <- mem16[0x1234]
#        $r4 <- mem32[0x1234]
#        $r4 <- mem[0x1234]
#        $r4 <- memll[0x1234]
#        $r4 <- memll32[0x1234]
#
#        smem8[0x1234] <- $r4
#        mem8[0x1234] <- $r4
#        smem16[0x1234] <- $r4
#        mem16[0x1234] <- $r4
#        mem32[0x1234] <- $r4
#        mem[0x1234] <- $r4
#        memsr[0x1234] <- $r4
#        memsr32[0x1234] <- $r4
#        inv[0x1234]
#
#        smem8[$r1--0x3412] <- $r4
#        mem8[$r1+0x3412] <- $r4
#        smem16[$r1+0x3412] <- $r4
#        mem16[$r1+0x3412] <- $r4
#        mem32[$r1+0x3412] <- $r4
#        mem[$r1+0x3412] <- $r4
#        memsr[$r1+0x3412] <- $r4
#        memsr32[$r1+0x3412] <- $r4
#        inv[$r1+0x3412]
#
#        smem8[$r1] <- $r4
#        mem8[$r1] <- $r4
#        smem16[$r1] <- $r4
#        mem16[$r1] <- $r4
#        mem32[$r1] <- $r4
#        mem[$r1] <- $r4
#        memsr[$r1] <- $r4
#        memsr32[$r1] <- $r4
#        inv[$r1]
#        if $r0 == 0 $pc <- _start
#lll:
#        $pc <- lll
#        $r0 <- _start
#        $r4 <- short 0x1234
#        sii
#        if $r2[3] == 0 $pc <- _start
#        $r5 <- 1 / $r4
#        mem[$sp + tiny 8] <- $r4
#        $r6 <- mem[$fp + tiny 32]
#        type $r8 ... $r14 <- mem[$r10 + 12] & 0011001
#        mem[$r8 +-- 8] <- type $r0 ... $r7
#        $r6 <- mem[$fp + tiny -4]
#        $r6 <- mem[$fp + tiny 4]
#        $r6 <- mem[$fp + tiny 0]
#        type $r8,$r9,$fp <- mem[$r10 + 12]
#        type $r0,$r1,$r5,$r2 <- int32, fp32, int8x4, fp16x2
#        type $sp,$fp,$r13,$r9 <- int32, fp32, int8x4, fp16x2
#        type $r0 <- int32
#        $fp <- data
#        $r1 <- mem[$fp + tiny 4]
#        $r2 <- 0x01020304
#        $r3 <- lane_swizzle $r2, 0123
#        sii
#mem[$r13 +-4]<-$r12
#
#.data
#data:
#        .ascii "12345678This is a string of some characters"
