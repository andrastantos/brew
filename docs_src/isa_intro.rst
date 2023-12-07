Instruction Set Principles
==========================

Brew uses instructions of variable length. Its instruction length varies from 16 bits to 64 bits. It can directly place 32-bit immediate values into the instruction stream while providing compact encoding for often used small constants. In almost all cases, the first 16-bits of an instruction is sufficient to fully decode the instruction; the rest of up to 32 bits contain an immediate field. Exceptions to this rule are:

#. Extension groups for rarely used operations, in which case the first 16 bits identify the extension group and the subsequent 16 bits are used to decode the operation
#. Prefix instructions, which can modify the operation of any subsequent instruction. In this case, if the subsequent instruction is an extension group, potentially 48 bits needs to be investigated for instruction decode to completely succeed.

The instruction set mostly follows a nibble-based decode organization: the first 16 bits of an instruction is divided into 4 nibbles. These contain the source and destination register indices and the code for the operation to be performed. The value of 0xf is often used in these nibbles to denote alternate instruction encodings such as the existence of an immediate field.
