Comparing to the competition
============================

Linpack.o: 3411 assembly lines, 6640 bytes of code. That's pretty close to 16-bits per instruction. And that's before the spill/reload optimizations. That alone would save 774 words or 1584 bytes. That would make the code segment 5056 bytes long, so below 16 bits per instruction (which of course must be a measurement error, I can't average below 16.)

The problem is in the code segment size. Whatever is disassembled is only 11498 bytes long, because it contains both .text and .text.startup. So, the real ratio used to be 27 bits/instruction.

After the changes it should be around 23.2 bits/instruction. A 32-bit ISA obviously would not be able to beat that, but it might still be better if it somehow was significantly more compact.

One reason for that to be would be the possibility of 31 GP registers. On the flip-side, loading a generic 32-bit constant would require 64-bits of instruction space, whereas this ISA gets the job done if 48 bits.

Overall, I have a hard time seeing how any 32-bit ISA would be able to beat this for compactness. But then again, there's RISCV...

The same metrics for that code: 2137 instructions, in 8540 bytes. That's quite precisely 32-bits per instruction, which of course is the expectation. Though the comparison isn't fair: the RISCV code uses hard-floats. Correcting for that, we end up with 3334 instructions, 13328 bytes. Now we're talking! the RISCV ISA now doesn't match up to even my current implementation, let alone to the optimized one!

The other 32-bit architectures, NIOS2 and MICROBLAZE were no better either. That leaves ARM (32), which appears to still be slightly better.

That stems from the fact that they manage to accomplish the task in a mere 2724 instructions. However we do know that ARM is very compact due to the their multi-register push/pull instructions, which I'm not willing to entertain. And with the optimized spill/reload sequences, I'll be able to beat ARM as well, even though not THUMB.

The typed ISA though is *painful*!
