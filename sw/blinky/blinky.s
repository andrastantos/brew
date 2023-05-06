.set rom_base,  0x00000000
.set gpio_base, 0x00010000
.set dram_base, 0x80000000

.global _start

.text
_start:
    $r5 <- tiny 0
    $r7 <- 10
loop:
    $r6 <- $r5 >> $r7
    mem8[gpio_base] <- $r6
    $r5 <- tiny $r5 + 1
    $pc <- loop

