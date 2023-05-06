.set rom_base,  0x00000000
.set gpio_base, 0x00010000
.set dram_base, 0x80000000

.global _start

.text

_start:
    $pc <- dram_base
