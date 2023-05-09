.PRECIOUS: %.o %.rom_elf %.dram_elf %.dram %.rom

all: rom.rom_mef blinky.dram0_mef blinky.dram1_mef

%.o: %.s
	brew-none-elf-as $^ -o $@

%.rom_elf: %.o
	brew-none-elf-ld --script rom.lds $^ -o $@

%.dram_elf: %.o
	brew-none-elf-ld --script dram.lds $^ -o $@

%.rom: %.rom_elf
	brew-none-elf-objcopy --strip-all --output-target binary $^ $@

%.dram: %.dram_elf
	brew-none-elf-objcopy --strip-all --output-target binary $^ $@

%.dram_mef: %.dram
	hexdump -e '/1 "%02x" "\n"' -v $^ > $@

%.dram0_mef: %.dram_mef
	sed -ne 'w $@' -e 'n;' $^

%.dram1_mef: %.dram_mef
	sed -ne '' -e 'n; w $@' $^

%.rom_mef: %.rom
	hexdump -e '/1 "%02x" "\n"' -v $^ > $@
