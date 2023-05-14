Instruction Set
===============

A small excursion into an old topic: what would a 16-bit ISA variant look like?
The idea here is that we would still support 32-bit addressing through register-pairs, but try to eliminate all 48-bit instructions. This would add an extra cycle to decode when 32-bit addresses are needed. It would make far jumps and calls difficult, but here's the thing: if we target a machine to earlier then even '83, the memory shipped with the machine would not be more than 128k. With the protection features and unified memory, it's actually not all that likely that your code-space would be greater than 32k, in which case 16-bit PC-relative jumps are just fine.

Finally, maybe there's even a way to make this ISA forwards compatible by designating a 16-bit register type field.

Register pairs are denoted as $drX, where X is an even number. So $dr0 := [$r1:$r0], $dr2 := [$r3:$r2] etc. For instructions where double registers are used, the top 3 bits of the register field denotes the $dr index. The bottom bit is set to 1 for register, 0 for immediate extension options. This allows for using 19-bit immediate offsets, or 512k addressable memory without the use of registers.

Paired with some intelligent address space decoding, that should work for these early systems.

The more I think about it, this instruction set is viable, but not as a 'dumbed down' version of the 32-bit ISA. It's a *different* and unique ISA and should not attempt to remain forwards compatible.

This ISA needs a lot more work. I think it's quite possible to encode all odd and even register pairs and have (for immediate ld/st) a 4-bit address-extension field. That would allow for 1MB direct-addressable memory.

Encoding overview
-----------------

There are three encoding variants:

16-bit instructions::

    +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
    |    FIELD_D    |    FIELD_C    |    FIELD_B    |    FIELD_A    |
    +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

32-bit instructions::

    +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
    |    FIELD_D    |    FIELD_C    |    FIELD_B    |    FIELD_A    |
    +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

    +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
    |                         FIELD_E                               |
    +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

FIELD_D:
  normally contains the register index of the [D]estination
FIELD_C:
  normally contains the instruction op-[C]ode
FIELD_B:
  normally contains the register index of the second operand (operand [B])
FIELD_A:
  normally contains the register index of the first operand (operand [A])
FIELD_E:
  normally contains an immediate or a memory offset

The whole 16 bit instruction is referred to as the INST.

The '[]' operator represents sub-fields in verilog notation.

The '{}' operator represents concatenation in verilog notation.

The ':' operator represents range (when that context make sense)

Register indexes:

===========    =============
Filed value    Register name
===========    =============
0x0            $r0
0x1            $r1
0x2            $r2
0x3            $r3
0x4            $r4
0x5            $r5
0x6            $r6
0x7            $r7
0x8            $r8
0x9            $r9
0xa            $r10
0xb            $r11
0xc            $r12
0xd            $r13
0xe            $r14
0xf            reserved
===========    =============

===========    =============
Filed value    Register name
===========    =============
0x1            $dr0
0x3            $dr1
0x5            $dr2
0x7            $dr3
0x9            $dr4
0xb            $dr5
0xd            $dr6
===========    =============

Register aliases:
    $sp:  alias to $dr5
    $fp:  alias to $dr6
    $lr:  alias to $dr4

.. note::
  Aliases have (almost) nothing to do with HW and only make assembly unambiguous and/or easier to read. The only exception is that the ISA supports compact load-store operations with $sp and $fp as their base registers. There's no functional difference between these compact and the full-format load/store operations, but the intent is that by using these instructions to access stack-local variables allows much more compact code-size (close to ARM THUMB compactness).

Special registers:

$pc:
 denotes the current-context program counter, the pointer to the currently executing instruction.

$tpc:
 denotes the task-mode program counter. If the current execution context is TASK mode, $tpc is the same as $pc. In SCHEDULER mode, it's not.

.. note:: $pc and $tpc are 31-bit registers: the LSB is ignored on write and always reads as 0.

Register Types
--------------

The type of the data held in a register is stored as side-band information next to the register data. The meaning of various instruction opcodes depend on the register types they operate on.

.. note::
  Since compilers (at least GCC) don't differentiate between signed and unsigned integer types, the HW doesn't do that either. This means that certain integer operations have a signed and an unsigned version.

There are up to 15 register types supported by the ISA, but only the following are in defined at the moment:

==========    =========   ==========
Type code     Type        Note
==========    =========   ==========
0x0           INT32       32-bit integer
0x1           INT16       16-bit integer: this is the default type of all registers after reset
0x2           INT16X2     2-way 16-bit integer vector
0x3           INT8X4      4-way 8-bit integer vector
0x4           UINT16X2S   Unsigned, saturated version on INT16X2
0x5           SINT16X2S   Signed, saturated version on INT16X2
0x6           UINT8X4S    Unsigned, saturated version on INT8X4
0x7           SINT8X4S    Signed, saturated version on INT8X4
0x8           FP32        32-bit float
0x9           FP16X2      2-way 16-bit float vector
0xf           mask        prevents target type changes during type ... <- ... type operations
==========    =========   ==========

Instruction Set Summary
-----------------------

In the following tables

'.':
  means any value in [0x0:0xe], unless specifically listed as a special case. Can be a different number at every occurrence.
'*':
 means any value in [0x0:0xf] Can be a different number at every occurrence.

Instructions are fully decoded. Any instruction not explicitly mentioned in the tables below generate an 'invalid instruction exception' and is functionally equivalent to the SII instruction.

Exception group
~~~~~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       0       |       0       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

All instructions in this group enter SCHEDULER mode. After execution $tpc points to the current instruction (the one generating the exception)

=================  ========    ============      ==================
Instruction code   Assembly    Alternative       Operation
=================  ========    ============      ==================
0x0000             SWI 0       FILL              Used to fill unused code-pages;
0x1000             SWI 1       BREAK             Used for software breakpoints
0x2000             SWI 2       SYSCALL           Used to implement system calls
0x3000             SWI 3
0x4000             SWI 4
0x5000             SWI 5
0x6000             SWI 6
0x7000             SWI 7       SII               Functionally equivalent to invalid instruction exception
=================  ========    ============      ==================

.. TODO::
  The toolset might still think SII is 0x6000 and HWI is 0x7000! Need to follow-up

Mode change and power management group
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       0       |       0       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


=================  ========    ==================
Instruction code   Assembly    Operation
=================  ========    ==================
0x8000             STM         Enters TASK mode, enables interrupts; $spc points to the NEXT instruction
0x9000             WOI         Wake on interrupt
0xa000             SII
0xb000             SII
0xc000             SII
0xd000             SII
0xe000             SII
=================  ========    ==================

Atomic group
~~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       0       |       1       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


=================  ========
Instruction code   Assembly
=================  ========
0x0001             FENCE_RW_RW
0x1001             FENCE__W_RW
0x2001             FENCE_R__RW
0x3001             FENCE____RW
0x4001             FENCE_RW__W
0x5001             FENCE__W__W
0x6001             FENCE_R___W
0x7001             FENCE_____W
0x8001             FENCE_RW_R_
0x9001             FENCE__W_R_
0xa001             FENCE_R__R_
0xb001             FENCE____R_
0xc001             FENCE_RW___
0xd001             FENCE__W___
0xe001             FENCE_R____
=================  ========

Every instruction in this group implements a fence, or an ordering between loads and stores. The top-most 4 bits of the instruction code is used the encode the fence type:

==========   ============
Bit-field    Meaning
==========   ============
12           ~R-before
13           ~W-before
14           ~R-after
15           ~W-after
==========   ============

.. note::
  bit-values are inverted to make FIELD_D==0xf an invalid encoding (i.e. no fence specification)

.. note::
  Fences have no effect on cache contents. In particular, fences don't invalidate the instruction cache (if exists) and cannot be exclusively used to implement coherency between data and instruction stream such as needed for self-modifying code.

.. important::
  Depending on the implementation, some or all of these fence operations might be no-ops. Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.

PC manipulation group
~~~~~~~~~~~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       0       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=================  ===================    ==================
Instruction code   Assembly               Operation
=================  ===================    ==================
0x.002             $pc <- [$drD]          Indirect jump
0x.003             $tpc <- [$drD]         Update $tpc
0x.004             [$drD] <- $pc          Load $pc into register
0x.005             [$drD] <- $tpc         Load $tpc into register
0x.006
0x.007
0x.008
0x.009
0x.00a
0x.00b
0x.00c
0x.00d
0x.00e
=================  ===================    ==================

.. note::
  If LSB of FIELD_D is clear (immediate extension), the operation is reserved and an SII is raised.

.. note::
  All instruction codes in this group are treated as jump instructions by the branch predictor, if exists. After warming up, some will always be predicted taken, some will not be. In TASK mode indirect jump (0x.002) and $tpc update (0x.003) instructions have the exact same behavior, however might have different latencies.

.. todo::
  We might want to shift encoding to 0x.004 ... 0x.007 to make the branch predictors job easier at recognizing this class.

Unary group
~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=================  ================================    ==================
Instruction code   Assembly               Operation
=================  ================================    ==================
0x.01.             $rD <- tiny FIELD_A                 Load $rD with constant [#note0xX01X]_
0x.02.             [$drD] <- $pc + FIELD_A*2           Call return address calculation [#note0xX02X]_
0x.03.             $rD <- -$rA                         Negative operation, depending on type
0x.04.             $rD <- ~$rA                         Binary inversion
0x.05.             $rD <- bse $rA                      Sign-extend from byte [#note0xX05X]_
0x.06.             RESERVED                            [#note_reserved]_
0x.07.             RESERVED                            [#note_reserved]_
0x.08.             RESERVED                            [#note_reserved]_
0x.09.             RESERVED                            [#note_reserved]_
0x.0a.             RESERVED                            [#note_reserved]_
0x.0b.             RESERVED                            [#note_reserved]_
0x.0c.             RESERVED                            [#note_types]_
0x.0d.             RESERVED                            [#note_types]_
0x.0e.             RESERVED                            [#note_types]_
=================  ================================    ==================

.. [#note0xX01X] FIELD_A is one-s complement; range is -7...7
.. [#note0xX02X] FIELD_A is one-s complement; range is -7...7; NOTE: WE COULD MAKE THE RANGE A LITTLE HIGHER IF NOT ALLOW 0
.. [#note0xX05X] For vector types, operation is per-lane. Floating point types are treated as integer
.. [#note0xX09X] Operation is RESERVED for integer types.
.. [#note0xX0aX] Operation is RESERVED for integer types.
.. [#note_reserved] These operations are reserved for future use and should **NEVER** appear in the 16-bit ISA instruction stream.
                    If they do, no backwards compatibility is guaranteed.
.. [#note_types] These operations are reserved for future introduction of types. They are **NOT TO BE USED** in the 16-bit ISA


Binary ALU group
~~~~~~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


=================  ==========================  ==================
Instruction code   Assembly                    Operation
=================  ==========================  ==================
0x.1..             $rD <- $rA ^ $rB            Bit-wise 'xor' [#note_logical]_
0x.2..             $rD <- $rA | $rB            Bit-wise 'or'  [#note_logical]_
0x.3..             $rD <- $rA & $rB            Bit-wise 'and' [#note_logical]_
0x.4..             $rD <- $rA + $rB            Type-dependent add
0x.5..             $rD <- $rA - $rB            Type-dependent subtract
0x.6..             $rD <- $rA << $rB           Binary left-shift [#note_binary_shift]_
0x.7..             $rD <- $rA >> $rB           Binary right-shift [#note_binary_shift]_
0x.8..             $rD <- $rA >>> $rB          Arithmetic right-shift [#note_binary_shift]_
0x.9..             $rD <- $rA * $rB            Type-dependent multiply
0x.a..             $rD <- ~$rA & $rB           Bit-wise 'not'-'and' [#note0xXaXX]_
0x.b..             $rD <- tiny $rB + FIELD_A   Integer add [#note0xXbXX]_
0x.c..             see below (stack ops)
0x.d..             see below (stack ops)
0x.e..             see below (mem ops)
=================  ==========================  ==================

.. [#note_logical] This operation ignore type info, but sets destination type to be the same as that of $rA
.. [#note_binary_shift] This operation only uses the lane-setup part of the type information. It sets the destination type to that of $rA
.. [#note0xXaXX] This operation is useful for lane-combining with an inverted predicate
.. [#note0xXbXX] FIELD_A is one's complement-coded; range is -7...7. This operation only uses the lane-setup part of the type information. It sets the destination type to that of $rA

.. note::
  If swizzle muxes are inline in the pipeline (as opposed to their own execution unit), it's possible to deal with scalar-vector combinations, where the scalar gets automatically replicated into the right number of lanes before the operation is performed. Similarly, a 2-lane-and-4-lane vector operation can replicate the 2-lane vector into 4 lanes before executing the operation.

.. todo::
  What should the behavior be for unsupported type-combinations? One would probably want an exception so that SW emulation can fill the gaps on lower-end processors, but then again, that makes almost all operations a possible exception source, and thus forces the pipeline to be more conservative.

.. note:: Output type is the type of $rA

.. note:: Pseudo instructions
  NOP: encodes to 0x2222, which is $r2 = $r2 | $r2
  $rD = $rS: encodes to 0xD2SS


Short load immediate group
~~~~~~~~~~~~~~~~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |       f       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=========================  ========================    ==================
Instruction code           Assembly                    Operation
=========================  ========================    ==================
0x.0f0 0x****              $rD  <- short VALUE         Load 16-bit immediate
0x20fe 0x****              $pc  <- short VALUE         Immediate short jump (value is zero-extended)
0x30fe 0x****              $tpc <- short VALUE         Load zero-extended value into $tpc
=========================  ========================    ==================

.. note::
  There are a lot of holes in this space. Those are reserved for ISA expansion and should generate an SII exception. Such as 0x.01f:0x.0df; 0x40ef:0xe0ef.

.. note::
  Destination type is not changed.

Short constant ALU group
~~~~~~~~~~~~~~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |       f       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=========================  =================================    ==================
Instruction code           Assembly                             Operation
=========================  =================================    ==================
0x.1f. 0x****              $rD <- short FIELD_E ^ $rA           Bit-wise 'xor' [#note_logical]_
0x.2f. 0x****              $rD <- short FIELD_E | $rA           Bit-wise 'or'  [#note_logical]_
0x.3f. 0x****              $rD <- short FIELD_E & $rA           Bit-wise 'and' [#note_logical]_
0x.4f. 0x****              $rD <- short FIELD_E + $rA           Type-dependent add
0x.5f. 0x****              $rD <- short FIELD_E - $rA           Type-dependent subtract
0x.6f. 0x****              $rD <- short $rA << FIELD_E          Binary left-shift [#note_binary_shift]_
0x.7f. 0x****              $rD <- short $rA >> FIELD_E          Binary right-shift [#note_binary_shift]_
0x.8f. 0x****              $rD <- short $rA >>> FIELD_E         Arithmetic right-shift [#note_binary_shift]_
0x.9f. 0x****              $rD <- short FIELD_E * $rA           Type-dependent multiply
0x.af. 0x****              RESERVED                             [#note_reserved]_
0x.bf. 0x****              SII                                  Reserved for future ISA expansion
0x.cf. 0x****              see below (stack ops)
0x.df. 0x****              see below (stack ops)
0x.ef. 0x****              see below (mem ops)
=========================  =================================    ==================

.. note::
  FIELD_E is treated as an unsigned 16-bit value.

Zero-compare conditional branch group
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |       f       |    FIELD_C    |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=========================  ===============================================    ==================
Instruction code           Assembly                                           Operation
=========================  ===============================================    ==================
0xf00. 0x****              if any $rA == 0  $pc <- $pc + unmunge(FIELD_E)
0xf01. 0x****              if any $rA != 0  $pc <- $pc + unmunge(FIELD_E)
0xf02. 0x****              if any $rA < 0   $pc <- $pc + unmunge(FIELD_E)     signed compare
0xf03. 0x****              if any $rA >= 0  $pc <- $pc + unmunge(FIELD_E)     signed compare
0xf04. 0x****              if any $rA > 0   $pc <- $pc + unmunge(FIELD_E)     signed compare
0xf05. 0x****              if any $rA <= 0  $pc <- $pc + unmunge(FIELD_E)     signed compare
0xf06. 0x****              SII
0xf07. 0x****              SII
0xf08. 0x****              if all $rA == 0  $pc <- $pc + unmunge(FIELD_E)
0xf09. 0x****              if all $rA != 0  $pc <- $pc + unmunge(FIELD_E)
0xf0b. 0x****              if all $rA >= 0  $pc <- $pc + unmunge(FIELD_E)     signed compare
0xf0c. 0x****              if all $rA > 0   $pc <- $pc + unmunge(FIELD_E)     signed compare
0xf0d. 0x****              if all $rA <= 0  $pc <- $pc + unmunge(FIELD_E)     signed compare
0xf0a. 0x****              if all $rA < 0   $pc <- $pc + unmunge(FIELD_E)     signed compare
0xf0e. 0x****              SII
=========================  ===============================================    ==================

.. note::
  For scalar types, FIELD_C MSB (inst[15]) is irrelevant; In other words, any/all selection doesn't matter
.. note:: unmunge: replicate LSB to bit positions [31:16], replace LSB with 0.

Conditional branch group
~~~~~~~~~~~~~~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |       f       |    FIELD_C    |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=========================  =====================================================    ==================
Instruction code           Assembly                                                 Operation
=========================  =====================================================    ==================
0xf1.. 0x****              if any $rB == $rA   $pc <- $pc + unmunge(OFFSET)
0xf2.. 0x****              if any $rB != $rA   $pc <- $pc + unmunge(OFFSET)
0xf3.. 0x****              if any signed $rB < $rA  $pc <- $pc + unmunge(OFFSET)    signed compare
0xf4.. 0x****              if any signed $rB >= $rA $pc <- $pc + unmunge(OFFSET)    signed compare
0xf5.. 0x****              if any $rB < $rA    $pc <- $pc + unmunge(OFFSET)
0xf6.. 0x****              if any $rB >= $rA   $pc <- $pc + unmunge(OFFSET)
0xf7.. 0x****              SII
0xf8.. 0x****              SII
0xf9.. 0x****              if all $rB == $rA   $pc <- $pc + unmunge(OFFSET)
0xfa.. 0x****              if all $rB != $rA   $pc <- $pc + unmunge(OFFSET)
0xfb.. 0x****              if all signed $rB < $rA  $pc <- $pc + unmunge(OFFSET)    signed compare
0xfc.. 0x****              if all signed $rB >= $rA $pc <- $pc + unmunge(OFFSET)    signed compare
0xfd.. 0x****              if all $rB < $rA    $pc <- $pc + unmunge(OFFSET)
0xfe.. 0x****              if all $rB >= $rA   $pc <- $pc + unmunge(OFFSET)
=========================  =====================================================    ==================

.. note::
  For scalar types, FIELD_C MSB (inst[15]) is irrelevant; In other words, any/all selection doesn't matter

.. note::
  Comparison type is determined by type of $rA. Type of $rB is ignored and assumed to match that of $rA

.. todo::
  Maybe we can do lane-replication in case of lane-count mismatch? After all, these are using the ALUs, the same way as binary ops do...

PSEUDO OPS:
    if signed $rB >= $rA $pc <- $pc + unmunge(OFFSET)
    if signed $rB < $rA  $pc <- $pc + unmunge(OFFSET)
    if $rB >= $rA   $pc <- $pc + unmunge(OFFSET)
    if $rB < $rA    $pc <- $pc + unmunge(OFFSET)

.. note:: unmunge: replicate LSB to bit positions [31:16], replace LSB with 0.

Bit-set-test conditional branch group
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |       f       |    FIELD_C    |       f       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=========================  =====================================================    ==================
Instruction code           Assembly                                                 Operation
=========================  =====================================================    ==================
0xf0f. 0x****              if $rA[0]  == 1 $pc <- $pc + unmunge(OFFSET)
0xf1f. 0x****              if $rA[1]  == 1 $pc <- $pc + unmunge(OFFSET)
0xf2f. 0x****              if $rA[2]  == 1 $pc <- $pc + unmunge(OFFSET)
0xf3f. 0x****              if $rA[3]  == 1 $pc <- $pc + unmunge(OFFSET)
0xf4f. 0x****              if $rA[4]  == 1 $pc <- $pc + unmunge(OFFSET)
0xf5f. 0x****              if $rA[5]  == 1 $pc <- $pc + unmunge(OFFSET)
0xf6f. 0x****              if $rA[6]  == 1 $pc <- $pc + unmunge(OFFSET)
0xf7f. 0x****              if $rA[7]  == 1 $pc <- $pc + unmunge(OFFSET)
0xf8f. 0x****              if $rA[8]  == 1 $pc <- $pc + unmunge(OFFSET)
0xf9f. 0x****              if $rA[9]  == 1 $pc <- $pc + unmunge(OFFSET)
0xfaf. 0x****              if $rA[10] == 1 $pc <- $pc + unmunge(OFFSET)
0xfbf. 0x****              if $rA[11] == 1 $pc <- $pc + unmunge(OFFSET)
0xfcf. 0x****              if $rA[12] == 1 $pc <- $pc + unmunge(OFFSET)
0xfdf. 0x****              if $rA[14] == 1 $pc <- $pc + unmunge(OFFSET)
0xfef. 0x****              if $rA[15] == 1 $pc <- $pc + unmunge(OFFSET)
=========================  =====================================================    ==================

.. note:: unmunge: replicate LSB to bit positions [31:16], replace LSB with 0.

Bit-clear-test conditional branch group
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |       f       |    FIELD_C    |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=========================  =====================================================    ==================
Instruction code           Assembly                                                 Operation
=========================  =====================================================    ==================
0xf0.f 0x****              if $rB[0]  == 0 $pc <- $pc + unmunge(OFFSET)
0xf1.f 0x****              if $rB[1]  == 0 $pc <- $pc + unmunge(OFFSET)
0xf2.f 0x****              if $rB[2]  == 0 $pc <- $pc + unmunge(OFFSET)
0xf3.f 0x****              if $rB[3]  == 0 $pc <- $pc + unmunge(OFFSET)
0xf4.f 0x****              if $rB[4]  == 0 $pc <- $pc + unmunge(OFFSET)
0xf5.f 0x****              if $rB[5]  == 0 $pc <- $pc + unmunge(OFFSET)
0xf6.f 0x****              if $rB[6]  == 0 $pc <- $pc + unmunge(OFFSET)
0xf7.f 0x****              if $rB[7]  == 0 $pc <- $pc + unmunge(OFFSET)
0xf8.f 0x****              if $rB[8]  == 0 $pc <- $pc + unmunge(OFFSET)
0xf9.f 0x****              if $rB[9]  == 0 $pc <- $pc + unmunge(OFFSET)
0xfa.f 0x****              if $rB[10] == 0 $pc <- $pc + unmunge(OFFSET)
0xfb.f 0x****              if $rB[11] == 0 $pc <- $pc + unmunge(OFFSET)
0xfc.f 0x****              if $rB[12] == 0 $pc <- $pc + unmunge(OFFSET)
0xfd.f 0x****              if $rB[14] == 0 $pc <- $pc + unmunge(OFFSET)
0xfe.f 0x****              if $rB[15] == 0 $pc <- $pc + unmunge(OFFSET)
=========================  =====================================================    ==================

.. note:: unmunge: replicate LSB to bit positions [31:16], replace LSB with 0.

Stack group
~~~~~~~~~~~

While stack operations (as in push/pull) are not supported by the ISA, special load/store instructions are provided with small offsets and $r0 ($sp) and $r1 ($lr) as the base register to support compact form of common stack-load and store- operations. The supported offset range us -256 to +252 bytes.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |            OFS            | A |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

==================  ======================================    ==================
Instruction code    Assembly                                  Operation
==================  ======================================    ==================
0x.c**              MEM16[$drA + tiny OFS*4] <- $rD           Store $rD in memory
0x.d**              $rD <- MEM16[$drA + tiny OFS*4]           Load $rD from memory
==================  ======================================    ==================

.. warning::
  The encoding of field-A is special: A=0 denotes $dr5, A=1 denotes $dr6

.. note::
  the existence of these ops complicate memory op decode as well as operation size decode, but save a *huge* amount of code-space, allowing almost all register spills and fills to be done in two bytes.

Indirect load/Store group
~~~~~~~~~~~~~~~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

==================  ============================    ==================
Instruction code    Assembly                        Operation
==================  ============================    ==================
0x.e4.              $rD <- MEM8[$drA]               8-bit unsigned load from MEM[$drA] into $rD
0x.e5.              $rD <- MEM16[$drA]              16-bit unsigned load from MEM[$drA] into $rD
0x.e6.              $drD <- MEM32[$drA]             32-bit load from MEM[$drA] into $rD
0x.e7.              $drD <- MEMLL32[$drA]           32-bit unsigned load-reserve (exclusive load)
0x.e8.              MEM8[$drA] <- $rD               8-bit store to MEM[$drA] from $rD
0x.e9.              MEM16[$drA] <- $rD              16-bit store to MEM[$drA] from $rD
0x.ea.              MEM32[$drA] <- $drD             32-bit store to MEM[$drA] from $rD
0x.eb.              MEMSR32[$drA] <- $drD           32-bit store-release (exclusive store)
0x.ec.              $rD <- SMEM8[$drA]              8-bit signed load from MEM[$drA] into $rD
0x.ed.              RESERVED                        [#note_reserved]_
==================  ============================    ==================

.. note::
  Loads don't change the type of their destination register.


Indirect jump group
~~~~~~~~~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

==================  ============================    ==================
Instruction code    Assembly                        Operation
==================  ============================    ==================
0x1ee.              RESERVED                        [#note_reserved]_
0x2ee.              $pc <- MEM32[$drA]              32-bit load from MEM[$rA] into $PC
0x3ee.              $tpc <- MEM32[$drA]             32-bit load from MEM[$rA] into $TPC
==================  ============================    ==================

.. note::
  Cache invalidation applies to all caches and to all levels of caches: L1D L1I; L2, if exists. System-level caches (L3) are not invalidated. In a multi-processor system, only local caches (caches that are in the path-to-memory for the core executing the instruction) are invalidated.

Offset-indirect type load/store group
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       f       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

==================  ======================================    ==================
Instruction code    Assembly                                  Operation
==================  ======================================    ==================
0x.f0. 0x****       type $r0...$r7  <- MEM[$rD, FIELD_A*4]    with FIELD_E as mask
0x.f1. 0x****       type $r8...$r14 <- MEM[$rD, FIELD_A*4]    with FIELD_E as mask
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv NOTE NOTE NOTE THESE ARE CHANGED!!!!! TO BE CHECKED WITH COMPILER/ASSEMBLER!!!!!!!
0x*f2* 0x****       Store multiple offset: {FIELD_D[3:1], FIELD_A} * 4, destination is FIELD_D[0], FIELD_E[15]: include types; FIELD_E[14:0]: register mask
0x*f3* 0x****       Load multiple  offset: {FIELD_D[3:1], FIELD_A} * 4, destination is FIELD_D[0], FIELD_E[15]: include types; FIELD_E[14:0]: register mask
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ NOTE NOTE NOTE THESE ARE CHANGED!!!!! TO BE CHECKED WITH COMPILER/ASSEMBLER!!!!!!!
==================  ======================================    ==================

.. note::
  FIELD_A is ones-complement coded
.. note::
  FIELD_E is used as an 8-bit mask: 1 denotes setting the type for a register, 0 denotes leaving the type unchanged. LSB corresponds to the lowest order register, bit-8 to the highest order. The upper byte of FIELD_E is ignored.
.. note::
  A 32-bit value is loaded from memory and is used to set the types. Ignored types are 'stepped over', their bits in memory are still occupied.

Offset-indirect load/store group
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       f       |    FIELD_B    | FIELD_A   | F |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

If FIELD_F is '1':

==================  ====================================    ==================
Instruction code    Assembly                                Operation
==================  ====================================    ==================
0x.f4. 0x****       $rD <- MEM8[$drA+FIELD_E]               8-bit unsigned load from MEM[$rA+FIELD_E] into $rD
0x.f5. 0x****       $rD <- MEM16[$drA+FIELD_E]              16-bit unsigned load from MEM[$rA+FIELD_E] into $rD
0x.f6. 0x****       $drD <- MEM32[$drA+FIELD_E]             32-bit load from MEM[$rA+FIELD_E] into $rD
0x.f7. 0x****       $drD <- MEMLL32[$drA+FIELD_E]           32-bit unsigned load-reserve (exclusive load)
0x.f8. 0x****       MEM8[$drA+FIELD_E] <- $rD               8-bit store to MEM[$rA+FIELD_E] from $rD
0x.f9. 0x****       MEM16[$drA+FIELD_E] <- $rD              16-bit store to MEM[$rA+FIELD_E] from $rD
0x.fa. 0x****       MEM32[$drA+FIELD_E] <- $drD             32-bit store to MEM[$rA+FIELD_E] from $rD
0x.fb. 0x****       MEMSR32[$drA+FIELD_E] <- $drD           32-bit store-release (exclusive store)
0x.fc. 0x****       $rD <- SMEM8[$drA+FIELD_E]              8-bit signed load from MEM[$rA+FIELD_E] into $rD
0x.fd. 0x****       SII
==================  ====================================    ==================

.. note:: FIELD_E is sign-extended before addition

If FIELD_F is '0':

==================  ====================================    ==================
Instruction code    Assembly                                Operation
==================  ====================================    ==================
0x.f4. 0x****       $rD <- MEM8[FIELD_EA]                   8-bit unsigned load from MEM[FIELD_EA] into $rD
0x.f5. 0x****       $rD <- MEM16[FIELD_EA]                  16-bit unsigned load from MEM[FIELD_EA] into $rD
0x.f6. 0x****       $drD <- MEM32[FIELD_EA]                 32-bit load from MEM[FIELD_EA] into $rD
0x.f7. 0x****       $drD <- MEMLL32[FIELD_EA]               32-bit unsigned load-reserve (exclusive load)
0x.f8. 0x****       MEM8[FIELD_EA] <- $rD                   8-bit store to MEM[FIELD_EA] from $rD
0x.f9. 0x****       MEM16[FIELD_EA] <- $rD                  16-bit store to MEM[FIELD_EA] from $rD
0x.fa. 0x****       MEM32[FIELD_EA] <- $drD                 32-bit store to MEM[FIELD_EA] from $rD
0x.fb. 0x****       MEMSR32[FIELD_EA] <- $drD               32-bit store-release (exclusive store)
0x.fc. 0x****       $rD <- SMEM8[FIELD_EA]                  8-bit signed load from MEM[FIELD_EA] into $rD
0x.fd. 0x****       SII
==================  ====================================    ==================

.. note:: FIELD_EA is a (zero-extend) concatenation of {FIELD_A, FIELD_E}. This allows addressing up to 512kB of memory.

Offset-indirect jump group
~~~~~~~~~~~~~~~~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       f       |    FIELD_B    |  FIELD_A  | F |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

If FIELD_F is '1':

==================  ====================================    ==================
Instruction code    Assembly                                Operation
==================  ====================================    ==================
0x1fe. 0x****       RESERVED                                [#note_reserved]_
0x2fe. 0x****       $pc <- MEM32[$drA+FIELD_E]              32-bit load from MEM[$drA+FIELD_E] into $PC
0x3fe. 0x****       $tpc <- MEM32[$drA+FIELD_E]             32-bit load from MEM[$drA+FIELD_E] into $TPC
==================  ====================================    ==================

If FIELD_F is '0':

==================  ====================================    ==================
Instruction code    Assembly                                Operation
==================  ====================================    ==================
0x1fe. 0x****       RESERVED                                [#note_reserved]_
0x2fe. 0x****       $pc <- MEM32[FIELD_EA]                  32-bit load from MEM[FIELD_EA] into $PC
0x3fe. 0x****       $tpc <- MEM32[FIELD_EA]                 32-bit load from MEM[FIELD_EA] into $TPC
==================  ====================================    ==================

.. note:: FIELD_EA is a (zero-extend) concatenation of {FIELD_A, FIELD_E}. This allows addressing up to 512kB of memory.







NOT USED, BUT RESERVED
=======================






Load immediate group
~~~~~~~~~~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...
  |                         FIELD_E  lower 16 bits              ...
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...

  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  ...                       FIELD_E   upper 16 bits               |
  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=========================  ========================    ==================
Instruction code           Assembly                    Operation
=========================  ========================    ==================
0x.00f 0x**** 0x****       $rD  <- VALUE               Load immediate
0x20ef 0x**** 0x****       $pc   <- VALUE              Unconditional jump
0x30ef 0x**** 0x****       $tpc  <- VALUE              Load immediate to $tpc
0x80ef 0x**** 0x****       type $r0...$r7 <- VALUE     Load immediate type values [#note_immedate_types]_
0x90ef 0x**** 0x****       type $r8...$r14 <- VALUE    Load immediate type values
=========================  ========================    ==================

.. note::
  There are a lot of holes in this space. Those are reserved for ISA expansion and should generate an SII exception. Such as 0x.01f:0x.0df; 0x40ef:0x70ef, 0xa0ef:0xe0ef.

.. note::
  Destination type is not changed, except of course for type load operations.

.. [#note_immedate_types]
  Types for each register are encoded in 4-bit nibbles. Lowest 4 bits determine the type of the lowest indexed register. Highest 4 bits determine the type of the highest indexed register.

Constant ALU group
~~~~~~~~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...
  |                         FIELD_E  lower 16 bits              ...
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...

  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  ...                       FIELD_E   upper 16 bits               |
  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=========================  ========================    ==================
Instruction code           Assembly                    Operation
=========================  ========================    ==================
0x.1.f 0x**** 0x****       $rD <- FIELD_E ^ $rB        Bit-wise 'xor' [#note_logical]_
0x.2.f 0x**** 0x****       $rD <- FIELD_E | $rB        Bit-wise 'or'  [#note_logical]_
0x.3.f 0x**** 0x****       $rD <- FIELD_E & $rB        Bit-wise 'and' [#note_logical]_
0x.4.f 0x**** 0x****       $rD <- FIELD_E + $rB        Type-dependent add
0x.5.f 0x**** 0x****       $rD <- FIELD_E - $rB        Type-dependent subtract
0x.6.f 0x**** 0x****       $rD <- FIELD_E << $rB       Binary left-shift [#note_binary_shift]_
0x.7.f 0x**** 0x****       $rD <- FIELD_E >> $rB       Binary right-shift [#note_binary_shift]_
0x.8.f 0x**** 0x****       $rD <- FIELD_E >>> $rB      Arithmetic right-shift [#note_binary_shift]_
0x.9.f 0x**** 0x****       $rD <- FIELD_E * $rB        Type-dependent multiply
0x.a.f 0x**** 0x****       SII                         Reserved for future ISA expansion
0x.b.f 0x**** 0x****       SII                         Reserved for future ISA expansion
0x.c.f 0x**** 0x****       see below (stack ops)
0x.d.f 0x**** 0x****       see below (stack ops)
0x.e.f 0x**** 0x****       see below (mem ops)
=========================  ========================    ==================

.. note:: Result type is that of $rB (even for shifts). FIELD_E is assumed to have the same type as $rB

.. note::
  << and >> operations where opB is constant can be expressed by multiplies. Because of that, these operations only have one form. This does mean though, that the constant needed for certain shifts is larger than what would normally be required (i.e. 32-bit instead of 16).

Indirect type load/store group
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

==================  ========================================    ==================
Instruction code    Assembly                                    Operation
==================  ========================================    ==================
0x.e0.              type $r0...$r7  <- MEM[$drD + FIELD_A*4]
0x.e1.              type $r8...$r14 <- MEM[$drD + FIELD_A*4]
0x.e2.              MEM[$drD + FIELD_A*4] <- type $r0...$r7
0x.e3.              MEM[$drD + FIELD_A*4] <- type $r8...$r14
==================  ========================================    ==================

.. note::
  FIELD_A is ones-complement coded




Absolute load/store group
~~~~~~~~~~~~~~~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       f       |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...
  |                         FIELD_E  lower 16 bits              ...
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...

  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  ...                       FIELD_E   upper 16 bits               |
  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=========================  ==========================  ==================
Instruction code           Assembly                    Operation
=========================  ==========================  ==================
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv NOTE NOTE NOTE THESE ARE CHANGED!!!!! TO BE CHECKED WITH COMPILER/ASSEMBLER!!!!!!!
0x.f2f 0x****              see store multiple
0x.f3f 0x****              see load multiple
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ NOTE NOTE NOTE THESE ARE CHANGED!!!!! TO BE CHECKED WITH COMPILER/ASSEMBLER!!!!!!!
0x.f4f 0x**** 0x****       $rD <- MEM8[FIELD_E]        8-bit unsigned load from MEM[FIELD_E] into $rD
0x.f5f 0x**** 0x****       $rD <- MEM16[FIELD_E]       16-bit unsigned load from MEM[FIELD_E] into $rD
0x.f6f 0x**** 0x****       $rD <- MEM[32][FIELD_E]     32-bit load from MEM[FIELD_E] into $rD
0x.f7f 0x**** 0x****       $rD <- MEMLL[32][FIELD_E]   32-bit unsigned load-reserve (exclusive load)
0x.f8f 0x**** 0x****       MEM8[FIELD_E] <- $rD        8-bit store to MEM[FIELD_E] from $rD
0x.f9f 0x**** 0x****       MEM16[FIELD_E] <- $rD       16-bit store to MEM[FIELD_E] from $rD
0x.faf 0x**** 0x****       MEM[32][FIELD_E] <- $rD     32-bit store to MEM[FIELD_E] from $rD
0x.fbf 0x**** 0x****       MEMSR[32][FIELD_E] <- $rD   32-bit store-release (exclusive store)
0x.fcf 0x**** 0x****       $rD <- SMEM8[FIELD_E]       8-bit signed load from MEM[FIELD_E] into $rD
0x.fdf 0x**** 0x****       $rD <- SMEM16[FIELD_E]      16-bit signed load from MEM[FIELD_E] into $rD
=========================  ==========================  ==================

.. note:: Loads don't change the type of a register.

Absolute jump group
~~~~~~~~~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       f       |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...
  |                         FIELD_E  lower 16 bits              ...
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...

  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  ...                       FIELD_E   upper 16 bits               |
  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=========================  ========================    ==================
Instruction code           Assembly                    Operation
=========================  ========================    ==================
0x1fef 0x**** 0x****       INV[32][FIELD_E]            invalidate cache line for address FIELD_E
0x2fef 0x**** 0x****       $pc <- MEM[32][FIELD_E]     32-bit load from MEM[FIELD_E] into $PC
0x3fef 0x**** 0x****       $tpc <- MEM[32][FIELD_E]    32-bit load from MEM[FIELD_E] into $TPC
=========================  ========================    ==================

.. note::
  Cache invalidation applies to all caches and to all levels of caches: L1D L1I; L2, if exists. System-level caches (L3) are not invalidated. In a multi-processor system, only local caches (caches that are in the path-to-memory for the core executing the instruction) are invalidated.

Extension groups
----------------

Extension groups allow for extending the instruction set by utilizing otherwise unused portions of the 16-bit instruction code-space, followed by a second 16-bit instruction code. These extension groups allow for expressing seldom used or specialized instructions while not impacting the compactness of the base ISA.

Lane predicate generation group
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 0 | 0 | 0 | 0 |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=========================  ========================    ==================
Instruction code           Assembly                    Operation
=========================  ========================    ==================
0xfff0 0x.00.              $rD <- $rA == 0
0xfff0 0x.01.              $rD <- $rA != 0
0xfff0 0x.02.              $rD <- $rA < 0                signed compare
0xfff0 0x.03.              $rD <- $rA >= 0               signed compare
0xfff0 0x.04.              $rD <- $rA > 0                signed compare
0xfff0 0x.05.              $rD <- $rA <= 0               signed compare
0xfff0 0x.1..              $rD <- $rB == $rA
0xfff0 0x.2..              $rD <- $rB != $rA
0xfff0 0x.3..              $rD <- signed $rB < $rA       signed compare
0xfff0 0x.4..              $rD <- signed $rB >= $rA      signed compare
0xfff0 0x.5..              $rD <- $rB < $rA
0xfff0 0x.6..              $rD <- $rB >= $rA
=========================  ========================    ==================

These instructions perform lane-wise comparisons of the prescribed type. The result (0 for FALSE, 1 for TRUE) is replicated across the length of each lane (8- 16- or 32-times) and placed in the destination register.

Linear interpolation group
~~~~~~~~~~~~~~~~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 0 | 0 | 0 | 1 |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=========================  ============================  ==================
Instruction code           Assembly                      Operation
=========================  ============================  ==================
0xfff1 0x.0..              $rD <- interpolate $rA, $rB
=========================  ============================  ==================

This instruction performs linear interpolation between adjacent lanes of $rA using the value of $rB as a fractional 32-bit value.

A 2-lane operation is as follows::

  $rD(0) <- $rA(0) *    $rB  + $rA(1) * (1-$rB)
  $rD(1) <- $rA(0) * (1-$rB) + $rA(1) *    $rB

A 4-lane operation is as follows::

  $rD(0) <- $rA(0) *    $rB  + $rA(1) * (1-$rB)
  $rD(1) <- $rA(0) * (1-$rB) + $rA(1) *    $rB
  $rD(3) <- $rA(3) *    $rB  + $rA(4) * (1-$rB)
  $rD(4) <- $rA(3) * (1-$rB) + $rA(4) *    $rB


Scaled multiply group
~~~~~~~~~~~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 0 | 0 | 0 |      FIELD_F      |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=========================  ==================================  ==================
Instruction code           Assembly                            Operation
=========================  ==================================  ==================
0xff0* 0x.0..              $rD <- full $rA * $rB >>> FIELD_F
0xff1* 0x.0..              $rD <- full $rA * $rB >>> FIELD_F
0xff0* 0x.1..              $rD <- full $rA * $rB >> FIELD_F
0xff1* 0x.1..              $rD <- full $rA * $rB >> FIELD_F
=========================  ==================================  ==================

.. todo::
  This group can be implemented in the 0xfff. style in the following way: 0xfff8/0xfff9: arithmetic shift by FIELD_C[+32], 0xfffa/0xfffb: logical shift by FIELD_C[+32]. That would free up this whole extension group and result in a much more compact encoding, but the binutils is not built this way at the moment.

Prefix instructions
-------------------

Prefix instructions can precede any other instruction to modify their behavior.

.. note::
  *Exception behavior*: If a prefixed instruction throws an exception, $tpc points to the (first) prefix instruction after entering SCHEDULER mode. This allows the recovery code to decode and potentially retry the excepted instruction.

.. note::
  *Interrupt behavior*: If an interrupt is handled during the execution of a prefixed instruction, $tpc points to the (first) prefix instruction after entering SCHEDULER mode. None of the side-effects of the prefixed instruction take effect. If any of the side-effects of the prefixed instruction have taken effect, the whole instruction must be carried to completion and $tpc points to the subsequent instruction after entering SCHEDULER mode. In other words, under no circumstances can $tpc point anywhere between the first prefix and it's corresponding instruction when entering SCHEDULER mode.

.. note::
  *Prefix concatenation*: Every processor implementation has a maximum instruction length it supports. In this version of the spec, it's 64 bits. So, if the instruction (pre-)decode stage finds an instruction longer then that maximum, it raises an invalid instruction exception (or more precisely, it replaces the decoded instruction with that of the SII instruction. Without this provision it would be possible to create arbitrarily long instruction sequences in TASK mode. That in turn would prevent interrupts from being raised, effectively locking up the system (at least up to the point of exhausting the addressable RAM space).

Type override
~~~~~~~~~~~~~

This prefix instruction allows for the changing the way the subsequent operation interprets source operand types. It doesn't actually change the source register types. It also allows for explicit control of whether the destination type is written or not.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |     TYPE_A    | 1 | 1 | 1 | D | 1 | 1 | 1 | 1 |    TYPE_B     | ...
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

Type override for $rA (TYPE_A) and $rB (TYPE_B). If D is set, $rD type is written
back into the register file. If cleared, $rD's type is not changed.


Unused instruction groups
-------------------------

All of the following instruction groups are explicitly reserved for future use. All processor implementations must raise an invalid instruction exception upon encountering them. The instruction size is shown as guidance for future uses: a simple instruction size decode logic would identify these groups as the shown size. It is not guaranteed that they are going to be used in such a fashion nor does it really matter for processor implementations following this version of the ISA specification.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  | 1 | 1 | 1 | 1 |    FIELD_C    | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

.. note:: FIELD_C != 0xf
.. note::
  The branch predictor is allowed to treat this group as a conditional branch

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 |    FIELD_B    | 1 | 1 | 1 | 1 |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

.. note:: FIELD_B != 0xf

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...
  |                         FIELD_E  lower 16 bits              ...
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...

  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  ...                       FIELD_E   upper 16 bits               |
  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

.. note:: FIELD_C != 0xc; FIELD_C != 0xd; FIELD_C != 0xf; FIELD_D != 0xf

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...
  |                         FIELD_E  lower 16 bits              ...
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...

  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  ...                       FIELD_E   upper 16 bits               |
  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

.. note:: FIELD_D != 0xf

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    | 1 | 1 | 1 | 0 |    FIELD_B    | 1 | 1 | 1 | 1 |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...
  |                         FIELD_E  lower 16 bits              ...
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...

  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  ...                       FIELD_E   upper 16 bits               |
  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

.. note:: FIELD_D != 0xf; FIELD_B != 0xf

.. note::
  if FIELD_D == 0x2 and FIELD_B == 0xe, the branch predictor is allowed to treat this instruction code as a conditional branch.

Cache invalidation
==================

There are instructions to invalidate individual data-, instruction- and L2 cache lines. These are encoded in the various load/store groups. There is no way to distinguish which cache we intend to invalidate. There is also no instruction provided for complete cache invalidation: this functionality is to be provided through memory-mapped CSRs.







