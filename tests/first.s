.text
    .global _start
    _start:
	#sii
	#fill
	#swi 5
        #$pc <- mem[$r2]
        #$pc <- mem[$r3+15]
        #$pc <- mem[0x1315]
#        $pc <- _start+12
#        #$tpc <- short 0x3ead
#        #type $r12 <- $r3
#        #type $r10 <- type fp32
#        #$r3 <- $pc + 4
#        #$r5 <- $pc
#        #$r7 <- $tpc
#	#$r1 <- $r2 * $r3
#        #$r2 <- $r10 + 0x1234
#        #$r2 <- 0x1234 + $r10
#        #$r3 <- short $r9 + 0x1234
#        #$r3 <- short 0x1234 + $r9
#        #$r3 <- tiny $r5 + -1
#        #$r3 <- $r10 > 0
#        #$r5 <- ~$r7&$r10
#        #$r1 <- - $r5
#        #$r5 <- interpolate $r1, $r2
#        #$r5 <- full $r1 * $r2 >>> 0xe
#        #$r5 <- lane_swizzle $r2, 0123
#
#        #$r4 <- smem8[0x1234]
#        #$r4 <- mem8[0x1234]
#        #$r4 <- smem16[0x1234]
#        #$r4 <- mem16[0x1234]
#        #$r4 <- mem32[0x1234]
#        #$r4 <- mem[0x1234]
#        #$r4 <- memll[0x1234]
#        #$r4 <- memll32[0x1234]
#
#        #smem8[0x1234] <- $r4
#        #mem8[0x1234] <- $r4
#        #smem16[0x1234] <- $r4
#        #mem16[0x1234] <- $r4
#        #mem32[0x1234] <- $r4
#        #mem[0x1234] <- $r4
#        #memsr[0x1234] <- $r4
#        #memsr32[0x1234] <- $r4
#        #inv[0x1234]
#
#        smem8[$r1--0x3412] <- $r4
#        #mem8[$r1+0x3412] <- $r4
#        #smem16[$r1+0x3412] <- $r4
#        #mem16[$r1+0x3412] <- $r4
#        #mem32[$r1+0x3412] <- $r4
#        #mem[$r1+0x3412] <- $r4
#        #memsr[$r1+0x3412] <- $r4
#        #memsr32[$r1+0x3412] <- $r4
#        #inv[$r1+0x3412]
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