.PRECIOUS: %.o %.elf %.sdmp %.dmp %.bdmp %.trace %.host %.host.out

all: linpack.elf

SRC = linpack.o

CF+= -Os

%.o: ../%.s
	$(TARGET)-as $^ -o $@

%.o: ../%.S
	$(TARGET)-gcc -c $^ $(CF) -o $@

%.o: ../%.c
	$(TARGET)-gcc $(CF) $(CF) -c $^ -o $@

%.o: ../%.cc
	$(TARGET)-gcc $(CF) $(CF) -c $^ -o $@

%.bin: %.elf
	$(TARGET)-objcopy --strip-all --output-target binary $^ $@

%.sdmp: ../%.c
	$(TARGET)-gcc $(CF) $(CF) -S $^ -o $@

%.sdmp: ../%.cc
	$(TARGET)-gcc $(CF) -S $^ -o $@

%.bdmp: %.elf
	$(TARGET)-objdump -dr --prefix-address --show-raw-ins $^ > $@

%.dmp: %.o
	$(TARGET)-objdump -dr -S --prefix-address --show-raw-ins $^ > $@

%.trace: %.elf
	$(TARGET)-run --trace-insn $^ 2>&1 | tee $@

%.out: %.elf
	$(TARGET)-run $^ 2>&1 | tee $@

%.test: %.out %.host.out
	diff $^

%.host: %.c
	gcc $^ -o $@

%.host.out: %.host
	$(abspath $^) 2>&1 | tee $@


linpack.elf: $(SRC)
	$(TARGET)-gcc $^ -lc -lm $(LF) -o $@ -Xlinker -Map $@.map
