Introduction
============

Brew (as in home-brew) is an Instruction Set Architecture to explore a few ideas that have been rattling in my head for a while. It is a RISC-style design at its heart with a few twists.

It is a 32-bit data-path design with variable instruction length. The smallest instructions is 16-bits long, the largest (without extension groups) 48 bits. For an common instruction sequence, the average instruction length is around 24 bits.

The ISA defines 14 general purpose registers plus a pair of program counters.

Each register has a type associated with it. This type is set and propagated by instructions. They can be loaded and saved to memory.

The instruction set doesn't define any privileged instruction, yet process isolation (not yet virtualization) is fully supported, if paired with an appropriate memory management unit (MMU).

The instruction set is supported by a port of BinUtils, GCC (C and C++ front-ends are tested) and NewLib. An instruction set simulator for user-mode processes is part of BinUtils. GDB is not yet ported to the architecture.

The first RTL implementation for the Brew architecture is the Espresso core, which is part of the Anachronistic Computer project.

