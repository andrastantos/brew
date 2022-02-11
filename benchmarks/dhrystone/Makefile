.PRECIOUS: %.o %.elf %.sdmp %.dmp %.bdmp %.trace %.host %.host.out


ifndef TARGET
TARGET=brew-none-elf
endif

ifeq ($(TARGET), brew-none-elf)
CF=-DSIM
LF=-lbrew
else
ifeq ($(TARGET), moxie-none-elf)
CF=-DSIM
LF=-T sim.ld
else
ifeq ($(TARGET), cris-none-elf)
CF=-DSIM
LF=-lsyssim
endif
endif
endif

all: dhry.elf

SRC = dhry_1.o dhry_2.o
ifeq ($(TARGET), riscv-none-elf)
SRC += strcmp.o
endif


%.o: %.s
	$(TARGET)-as $^ -o $@

%.bin: %.elf
	$(TARGET)-objcopy --strip-all --output-target binary $^ $@

%.o: %.S
	$(TARGET)-gcc -c $^ $(CF) -o $@

%.o: %.c
	$(TARGET)-gcc $(CF) $(CF) -c $^ -o $@

%.o: %.cc
	$(TARGET)-gcc $(CF) $(CF) -c $^ -o $@

%.sdmp: %.c
	$(TARGET)-gcc $(CF) $(CF) -S $^ -o $@

%.sdmp: %.cc
	$(TARGET)-gcc $(CF) -S $^ -o $@

%.bdmp: %.elf
	$(TARGET)-objdump -dr --prefix-address --show-raw-ins $^ > $@

%.dmp: %.o
	$(TARGET)-objdump -dr --prefix-address --show-raw-ins $^ > $@

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


dhry.elf: $(SRC)
	$(TARGET)-gcc $^ -lc -lm $(LF) -o $@ -Xlinker -Map $@.map
