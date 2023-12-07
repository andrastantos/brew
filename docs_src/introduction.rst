Introduction
============

Brew (as in home-brew) is an Instruction Set Architecture to explore a few ideas that have been rattling in my head for a while. It is a RISC-style design at its heart with a few twists.

Brew has a 32-bit data-path with a variable instruction length. The smallest instructions are 16-bits long, the largest are 64 bits. For an common instruction sequence, the average instruction length is around 24 bits.

The ISA defines 15 general purpose registers plus a pair of program counters. An instruction can specify up to two source registers and a single destination register.

Each register has a type associated with it. This type is set and propagated by instructions. While types can be loaded and stored in memory, they are not automatically maintained by regular data load and store instructions.

In this documentation you'll find what is the fourth major iteration of the architecture. In its current incarnation it is better in code-density then RISC-V, better than ARM, almost as good as THUMB. The first RTL implementation of the Brew architecture is the Espresso core, which is part of the Anachronistic Computer project.

The type-less subset of Brew is supported by a port of BinUtils, GCC (C and C++ front-ends are tested) and NewLib. An instruction set simulator with GDB support is part of BinUtils.

