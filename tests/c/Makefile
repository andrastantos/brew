.PRECIOUS: %.o %.elf %.sdmp %.dmp %.bdmp %.trace %.host %.host.out

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

%.sdmp: %.c
	brew-none-elf-gcc -S $^ -o $@

%.sdmp: %.cc
	brew-none-elf-gcc -S $^ -o $@

%.bdmp: %.elf
	brew-none-elf-objdump -dr --prefix-address --show-raw-ins $^ > $@

%.dmp: %.o
	brew-none-elf-objdump -dr --prefix-address --show-raw-ins $^ > $@

%.trace: %.elf
	brew-none-elf-run --trace-insn $^ 2>&1 | tee $@

%.out: %.elf
	brew-none-elf-run $^ 2>&1 | tee $@

%.test: %.out %.host.out
	diff $^

%.host: %.c
	gcc $^ -o $@

%.host.out: %.host
	$(abspath $^) 2>&1 | tee $@
