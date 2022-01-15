.PRECIOUS: %.o %.elf

%.o: %.s
	brew-none-elf-as $^ -o $@

%.elf: %.o
	brew-none-elf-ld $^ -o $@

%.bin: %.elf
	brew-none-elf-objcopy --strip-all --output-target binary $^ $@
