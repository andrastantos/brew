.PRECIOUS: %.o %.elf

%.o: %.s
	brew-none-elf-as $^ -o $@

%.elf: %.o
	brew-none-elf-gcc $^ -lc -lbrew -lm -o $@

%.bin: %.elf
	brew-none-elf-objcopy --strip-all --output-target binary $^ $@

%.o: %.S
	brew-none-elf-gcc -c $^ -o $@

%.o: %.c
	brew-none-elf-gcc -c $^ -o $@

%.o: %.cc
	brew-none-elf-gcc -c $^ -o $@

