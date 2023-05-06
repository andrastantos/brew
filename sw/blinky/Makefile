.PRECIOUS: %.o %.rom_elf %.dram_elf

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
