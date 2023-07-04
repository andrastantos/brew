Instruction Set Details
=======================

SWI 0
-----

*Instruction code*: 0x0000

*Alternative syntax*: FILL

*Exceptions*: SWI 0

*Type variants*: No

Description
~~~~~~~~~~~

Raises the SWI 0 exception. Used to fill unused space.

SWI 1
-----

*Instruction code*: 0x1000

*Alternative syntax*: BREAK

*Exceptions*: SWI 1

*Type variants*: No

Description
~~~~~~~~~~~

Raises the SWI 1 exception. Used to implement software breakpoints.

SWI 2
-----

*Instruction code*: 0x2000

*Alternative syntax*: SYSCALL

*Exceptions*: SWI 2

*Type variants*: No

Description
~~~~~~~~~~~

Raises the SWI 2 exception. Used to implement operating system calls

SWI 3
-----

*Instruction code*: 0x3000

*Exceptions*: SWI 3

*Type variants*: No

Description
~~~~~~~~~~~

Raises the SWI 3 exception.

SWI 4
-----

*Instruction code*: 0x4000

*Exceptions*: SWI 4

*Type variants*: No

Description
~~~~~~~~~~~

Raises the SWI 4 exception.

SWI 5
-----

*Instruction code*: 0x5000

*Exceptions*: SWI 5

*Type variants*: No

Description
~~~~~~~~~~~

Raises the SWI 5 exception.

SWI 6
-----

*Instruction code*: 0x6000

*Exceptions*: SWI 6

*Type variants*: No

Description
~~~~~~~~~~~

Raises the SWI 6 exception.

SWI 7
-----

*Instruction code*: 0x7000

*Exceptions*: SWI 7

*Type variants*: No

Description
~~~~~~~~~~~

Raises the SWI 7 exception. SWI 7 is the same exception that is raised by a core if an unimplemented instruction is encountered.

STM
---

*Instruction code*: 0x8000

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Returns execution to TASK mode. If already in TASK mode, the instruction as no effect. Execution continues in TASK mode from the address pointed to by :code:`$tpc`

WOI
---

*Instruction code*: 0x9000

*Exceptions*: HWI

*Type variants*: No

Description
~~~~~~~~~~~

Wake-on-interrupt. The processor enters a low-power state and waits for an interrupt. When an interrupt occurs, the processor continues execution. This operation waits for an interrupt, even if executed in SCHEDULER mode. In TASK mode, of course once execution is resumed, the processor switches to SCHEDULER mode, due to the pending interrupt.

PFLUSH
------

*Instruction code*: 0xa000

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

This instruction flushes the internal pipeline. Subsequent instructions must be fetched anew from at least L1 instruction cache. This instruction can be used to enforce proper operation for self-modifying code or for instance when a new executable image is loaded from storage.

.. todo:: PFLUSH is not implemented anywhere. Not in BINUTILS, not in Espresso.

FENCE_*
------

*Instruction code*: 0x.001

*Exceptions*: None

*Type variants*: No

*Assembly mnemonics*: FENCE_RW_RW; FENCE__W_RW; FENCE_R__RW; FENCE____RW; FENCE_RW__W; FENCE__W__W; FENCE_R___W; FENCE_____W; FENCE_RW_R_; FENCE__W_R_; FENCE_R__R_; FENCE____R_; FENCE_RW___; FENCE__W___; FENCE_R____; FENCE_RW_RW;

Description
~~~~~~~~~~~

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

Fences have no effect on cache contents. In particular, fences don't invalidate the instruction cache (if exists) and cannot be exclusively used to implement coherency between data and instruction stream such as needed for self-modifying code.

Depending on the implementation, some or all of these fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.

$pc <- $rD
----------

*Instruction code*: 0x.002

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       0       |       2       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: implementation defined

*Type variants*: No

Description
~~~~~~~~~~~

:code:`$pc` is loaded with the value in :code:`$rD`. This is an indirect jump. The LSB of :code:`$rD` carries implementation-defined meaning.

*Notes*: The CPU implementation can define various exceptions, based on its memory protection mechanism employed. The LSB of the target address carries implementation-defined meaning. If such a meaning is not defined, an implementation ignores the LSB.


$tpc <- $rD
-----------

*Instruction code*: 0x.003

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       0       |       3       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: implementation defined

*Type variants*: No

Description
~~~~~~~~~~~

:code:`$tpc` is loaded with the value in :code:`$rD`. This is an indirect jump in TASK mode. The LSB of :code:`$rD` carries implementation-defined meaning.

*Notes*: The CPU implementation can define various exceptions, based on its memory protection mechanism. These exceptions only apply in TASK mode. If the instruction is executed in SCHEDULER-mode, the only exceptions that are allowed to be raised are the ones related to the LSB of :code:`$rD`. The LSB of the target address carries implementation-defined meaning. If such a meaning is not defined, an implementation ignores the LSB.

$rD <- $pc
----------

*Instruction code*: 0x.004

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       0       |       4       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

:code:`$rD` is loaded with the value in :code:`$pc`, which points to the currently executing instruction. The LSB loaded into :code:`$rD` carries implementation-defined meaning. There's no guarantee that the LSB is preserved between a pair of :code:`$pc <- $rD` and :code:`$rD <- $pc` instructions.

$rD <- $tpc
-----------

*Instruction code*: 0x.005

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       0       |       4       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

:code:`$rD` is loaded with the value in :code:`$tpc`. In TASK-mode, this will be the address of the currently executing instruction. The LSB loaded into :code:`$rD` carries implementation-defined meaning. There's no guarantee that the LSB is preserved between a pair of :code:`$tpc <- $rD` and :code:`$rD <- $tpc` instructions.



$rD <- tiny CONST
--------------------------

*Instruction code*: 0x.01.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       1       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Load $rD with constant stored in FIELD_A. Constant value is one-s complement of FIELD_A. Range is -7 to +7. The destination type is not altered.


$rD <- $pc + CONST
--------------------------

*Instruction code*: 0x.02.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       2       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Load $rD with $pc + constant stored in FIELD_A. Constant value is twice the one-s complement of FIELD_A. Range is -14 to +14. Useful for call return address calculation. The destination type is set to INT32.



$rD <- -$rA
--------------------------

*Instruction code*: 0x.03.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       3       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

Negative operation. The actual negation depends on the type. For vector types lane-wise negation is used. For floating point types, floating-point negation is used. The destination type is set to that of :code:`$rA`.

$rD <- ~$rA
--------------------------

*Instruction code*: 0x.04.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       4       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Binary inversion. Destination type is set to that of :code:`$rA`.



$rD <- bse $rA
--------------------------

*Instruction code*: 0x.05.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       5       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

Sign-extend from byte. For vector types, operation is per-lane. Floating point types are treated as integer. Destination type is set to that of :code:`$rA`



$rD <- wse $rA
--------------------------

*Instruction code*: 0x.06.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       6       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

Sign-extend from word. For vector types, this operation is per-lane. Floating point types are treated as integer. Destination type is set to that of :code:`$rA`



$rD <- float $rA
--------------------------

*Instruction code*: 0x.07.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       7       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

Convert to float. No-op if $rA is already a float


$rD <- int $rA
--------------------------

*Instruction code*: 0x.08.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       8       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

Convert to integer. No-op if $rA is already integer

$rD <- 1 / $rA
--------------------------

*Instruction code*: 0x.09.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       9       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: SII

*Type variants*: Yes

Description
~~~~~~~~~~~

Reciprocal if :code:`$rA` is of a float type. Otherwise, an invalid instruction exception is thrown


$rD <- rsqrt $rA
--------------------------

*Instruction code*: 0x.0a.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       a       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

Reciprocal square-root if :code:`$rA` is of a float type. Otherwise, an invalid instruction exception is thrown.


$rD <- sum $rA
--------------------------

*Instruction code*: 0x.0b.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       b       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

Reduction sum if :code:`$rA` is of a vector type. Otherwise and invalid instruction exception is thrown.

.. todo:: Detail result type.


type $rD <- $rA
--------------------------

*Instruction code*: 0x.0c.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       c       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

Sets type of $rD as denoted by $rA. All 32 bits of :code:`$rA` are meaningful in this instruction. If an unsupported type is used, an invalid instruction exception is thrown. This instruction doesn't change the bit-pattern stored in :code:`$rD`.


$rD <- type $rA
--------------------------

*Instruction code*: 0x.0d.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       d       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

Loads type value of $rA into $rD



type $rD <- FIELD_A
--------------------------

*Instruction code*: 0x.0e.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       e       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Sets type of $rD as denoted by FIELD_A.

.. todo:: assembly should use descriptive type names, instead of numeric values.



$rD <- VALUE
--------------------------

*Instruction code*: 0x.00f 0x**** 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads :code:`$rD` with the value of FIELD_E. The type of :code:`$rD` is not changed.


$pc <- VALUE
--------------------------

*Instruction code*: 0x20ef 0x**** 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads :code:`$pc` with the value of FIELD_E. The LSB of the value has an implementation-defined meaning. This is an absolute jump operation. The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.


$tpc  <- VALUE
--------------------------

*Instruction code*: 0x30ef 0x**** 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads :code:`$tpc` with the value of FIELD_E. The LSB of the value has an implementation-defined meaning. This is an absolute jump operation in TASK mode. In TASK mode the implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.


type $r0...$r7 <- VALUE
--------------------------

*Instruction code*: 0x80ef 0x**** 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads the type of :code:`$r0` through :code:`$r7` with the types encoded in each 4-bit niggle of FIELD_E. The lowest 4 bits determine the type of :code:`$r0`. The highest 4 bits determine the type of :code:`$r7`. If a nibble has a value of 0xf, the type of the corresponding register is not changed.


type $r8...$r14 <- VALUE
--------------------------

*Instruction code*: 0x90ef 0x**** 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads the type of :code:`$r8` through :code:`$r14` with the types encoded in each 4-bit niggle of FIELD_E. The lowest 4 bits determine the type of :code:`$r8`. The next-to-highest 4 bits determine the type of :code:`$r14`. The highest nibble of FIELD_E is ignored. If a nibble has a value of 0xf, the type of the corresponding register is not changed.





$rD <- short VALUE
--------------------------

*Instruction code*: 0x.0f0 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads :code:`$rD` with the sign-extended value of FIELD_E. The type of :code:`$rD` is not changed.


$pc <- short VALUE
--------------------------

*Instruction code*: 0x20fe 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads :code:`$pc` with the sign-extended value of FIELD_E. The LSB of the value has an implementation-defined meaning. This is an absolute jump operation. The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.


$tpc <- short VALUE
--------------------------

*Instruction code*: 0x30fe 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads :code:`$tpc` with the sign-extended value of FIELD_E. The LSB of the value has an implementation-defined meaning. This is an absolute jump operation in TASK mode. In TASK mode the implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.





if any $rA == 0  $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf00. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Conditional branch operation. If any lanes of :code:`$rA` is equal to 0, the instruction flow is branched. The comparison is type-dependent in theory, but in practice all supported types represent the value 0 with the bit-pattern 0. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65535, in increments of 2 bytes.

The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.


if any $rA != 0  $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf01. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Conditional branch operation. If any lanes of :code:`$rA` is non-0, the instruction flow is branched. The comparison is type-dependent in theory, but in practice all supported types represent the value 0 with the bit-pattern 0. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65535, in increments of 2 bytes.

The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.


if any $rA < 0   $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf02. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Conditional branch operation. If any lanes of :code:`$rA` is less than 0, the instruction flow is branched. The comparison is type-dependent, for integral types, it's always carried out using signed arithmetic. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65535, in increments of 2 bytes.

The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.


if any $rA >= 0  $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf03. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Conditional branch operation. If any lanes of :code:`$rA` is greater than or equal to 0, the instruction flow is branched. The comparison is type-dependent, for integral types, it's always carried out using signed arithmetic. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65535, in increments of 2 bytes.

The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.


if any $rA > 0   $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf04. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Conditional branch operation. If any lanes of :code:`$rA` is greater than 0, the instruction flow is branched. The comparison is type-dependent, for integral types, it's always carried out using signed arithmetic. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65535, in increments of 2 bytes.

The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.


if any $rA <= 0  $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf05. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Conditional branch operation. If any lanes of :code:`$rA` is less than or equal to 0, the instruction flow is branched. The comparison is type-dependent, for integral types, it's always carried out using signed arithmetic. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65535, in increments of 2 bytes.

The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.


if all $rA == 0  $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf08. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Conditional branch operation. If all lanes of :code:`$rA` is equal to 0, the instruction flow is branched. The comparison is type-dependent in theory, but in practice all supported types represent the value 0 with the bit-pattern 0. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65535, in increments of 2 bytes.

The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.


if all $rA != 0  $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf09. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Conditional branch operation. If all lanes of :code:`$rA` is non-0, the instruction flow is branched. The comparison is type-dependent in theory, but in practice all supported types represent the value 0 with the bit-pattern 0. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65535, in increments of 2 bytes.

The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.


if all $rA < 0   $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf0a. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Conditional branch operation. If all lanes of :code:`$rA` is less than 0, the instruction flow is branched. The comparison is type-dependent, for integral types, it's always carried out using signed arithmetic. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65535, in increments of 2 bytes.

The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.


if all $rA >= 0  $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf0b3. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Conditional branch operation. If all lanes of :code:`$rA` is greater than or equal to 0, the instruction flow is branched. The comparison is type-dependent, for integral types, it's always carried out using signed arithmetic. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65535, in increments of 2 bytes.

The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.


if all $rA > 0   $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf0c. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Conditional branch operation. If all lanes of :code:`$rA` is greater than 0, the instruction flow is branched. The comparison is type-dependent, for integral types, it's always carried out using signed arithmetic. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65535, in increments of 2 bytes.

The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.


if all $rA <= 0  $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf0d. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Conditional branch operation. If all lanes of :code:`$rA` is less than or equal to 0, the instruction flow is branched. The comparison is type-dependent, for integral types, it's always carried out using signed arithmetic. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65535, in increments of 2 bytes.

The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.



if any $rB == $rA   $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf1.. 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~
Conditional branch operation. If any lanes of :code:`$rA` is equal to the same lane of :code:`$rB`, the instruction flow is branched. The comparison is type-dependent. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65535, in increments of 2 bytes.

The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.



if any $rB != $rA   $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf2.. 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~
Conditional branch operation. If any lanes of :code:`$rA` is unequal to the same lane of :code:`$rB`, the instruction flow is branched. The comparison is type-dependent. The type is determined by the type of :code:`$rA`. The type of :code:`$rB` is ignored and assumed to be the same as that of :code:`$rA`. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65535, in increments of 2 bytes.

The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.



if any signed $rB < $rA  $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf3.. 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~
Conditional branch operation. If any lanes of :code:`$rB` is less then the same lane of :code:`$rA`, the instruction flow is branched. The comparison is type-dependent. The type is determined by the type of :code:`$rA`. The type of :code:`$rB` is ignored and assumed to be the same as that of :code:`$rA`. For integral types, the comparison is done using signed arithmetic. For floating point types, a normal floating-point comparison is performed. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65535, in increments of 2 bytes.

The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.


if any signed $rB >= $rA $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf4.. 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~
Conditional branch operation. If any lanes of :code:`$rB` is greater then or equal to the same lane of :code:`$rA`, the instruction flow is branched. The comparison is type-dependent. The type is determined by the type of :code:`$rA`. The type of :code:`$rB` is ignored and assumed to be the same as that of :code:`$rA`. For integral types, the comparison is done using signed arithmetic. For floating point types, a normal floating-point comparison is performed. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65535, in increments of 2 bytes.

The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.



if any $rB < $rA    $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf5.. 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~
Conditional branch operation. If any lanes of :code:`$rB` is less then the same lane of :code:`$rA`, the instruction flow is branched. The comparison is type-dependent. The type is determined by the type of :code:`$rA`. The type of :code:`$rB` is ignored and assumed to be the same as that of :code:`$rA`. For integral types, the comparison is done using unsigned arithmetic. For floating point types, a normal floating-point comparison is performed. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65535, in increments of 2 bytes.

The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.


if any $rB >= $rA   $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf6.. 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~
Conditional branch operation. If any lanes of :code:`$rB` is greater then or equal to the same lane of :code:`$rA`, the instruction flow is branched. The comparison is type-dependent. The type is determined by the type of :code:`$rA`. The type of :code:`$rB` is ignored and assumed to be the same as that of :code:`$rA`. For integral types, the comparison is done using unsigned arithmetic. For floating point types, a normal floating-point comparison is performed. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65535, in increments of 2 bytes.

The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.












if all $rB == $rA   $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf9.. 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~
Conditional branch operation. If all lanes of :code:`$rA` is equal to the same lane of :code:`$rB`, the instruction flow is branched. The comparison is type-dependent. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65535, in increments of 2 bytes.

The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.



if all $rB != $rA   $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xfa.. 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~
Conditional branch operation. If all lanes of :code:`$rA` is unequal to the same lane of :code:`$rB`, the instruction flow is branched. The comparison is type-dependent. The type is determined by the type of :code:`$rA`. The type of :code:`$rB` is ignored and assumed to be the same as that of :code:`$rA`. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65535, in increments of 2 bytes.

The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.



if all signed $rB < $rA  $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xfb.. 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~
Conditional branch operation. If all lanes of :code:`$rB` is less then the same lane of :code:`$rA`, the instruction flow is branched. The comparison is type-dependent. The type is determined by the type of :code:`$rA`. The type of :code:`$rB` is ignored and assumed to be the same as that of :code:`$rA`. For integral types, the comparison is done using signed arithmetic. For floating point types, a normal floating-point comparison is performed. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65535, in increments of 2 bytes.

The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.


if all signed $rB >= $rA $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xfc.. 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~
Conditional branch operation. If all lanes of :code:`$rB` is greater then or equal to the same lane of :code:`$rA`, the instruction flow is branched. The comparison is type-dependent. The type is determined by the type of :code:`$rA`. The type of :code:`$rB` is ignored and assumed to be the same as that of :code:`$rA`. For integral types, the comparison is done using signed arithmetic. For floating point types, a normal floating-point comparison is performed. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65535, in increments of 2 bytes.

The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.



if all $rB < $rA    $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xfd.. 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~
Conditional branch operation. If all lanes of :code:`$rB` is less then the same lane of :code:`$rA`, the instruction flow is branched. The comparison is type-dependent. The type is determined by the type of :code:`$rA`. The type of :code:`$rB` is ignored and assumed to be the same as that of :code:`$rA`. For integral types, the comparison is done using unsigned arithmetic. For floating point types, a normal floating-point comparison is performed. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65535, in increments of 2 bytes.

The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.


if all $rB >= $rA   $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xfe.. 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~
Conditional branch operation. If all lanes of :code:`$rB` is greater then or equal to the same lane of :code:`$rA`, the instruction flow is branched. The comparison is type-dependent. The type is determined by the type of :code:`$rA`. The type of :code:`$rB` is ignored and assumed to be the same as that of :code:`$rA`. For integral types, the comparison is done using unsigned arithmetic. For floating point types, a normal floating-point comparison is performed. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65535, in increments of 2 bytes.

The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.



if $rA[C]  == 1 $pc <- $pc + VALUE
---------------------------------------------

*Instruction code*: 0xf.f. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Conditional branch operation. If bit-position C of :code:`$rA` is set, the instruction flow is branched. The comparison is type-independent. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65535, in increments of 2 bytes.

The value of 'C' is coded in FIELD_C in using the following table:

======= ===============
C       FIELD_C
======= ===============
0       0
1       1
2       2
3       3
4       4
5       5
6       6
7       7
8       8
9       9
a       14
b       15
c       16
d       30
e       31
======= ===============

The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.



if $rB[C]  == 0 $pc <- $pc + VALUE
---------------------------------------------

*Instruction code*: 0xf..f 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Conditional branch operation. If bit-position C of :code:`$rB` is not set, the instruction flow is branched. The comparison is type-independent. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65535, in increments of 2 bytes.

The value of 'C' is coded in FIELD_C in using the following table:

======= ===============
C       FIELD_C
======= ===============
0       0
1       1
2       2
3       3
4       4
5       5
6       6
7       7
8       8
9       9
a       14
b       15
c       16
d       30
e       31
======= ===============

The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.





MEM[$rS + tiny VALUE] <- $rD
---------------------------------------------

*Instruction code*: 0x.c**

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |            OFS            | S |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Store the 32-bit value of :code:`$rD` at memory location :code:`$rS + VALUE`. The field OFS is computed by dividing VALUE by four, then truncating it to 7 bits. Thus, the offset range of -256 to 252 is supported in steps of 4. The base register, :code:`$rS` is :code:`$r12` (:code:`$fp`) if field S is 0, :code:`$r13` (:code:`$sp`) otherwise. This instruction is useful for stack-frame manipulations.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an unaligned access exception is thrown.


$rD <- MEM[$rA + tiny VALUE]
---------------------------------------------

*Instruction code*: 0x.c**

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |            OFS            | S |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Load the 32-bit value into :code:`$rD` from memory location :code:`$rS + VALUE`. The type of :code:`$rD` is not modified. The field OFS is computed by dividing VALUE by four, then truncating it to 7 bits. Thus, the offset range of -256 to 252 is supported in steps of 4. The base register, :code:`$rS` is :code:`$r12` (:code:`$fp`) if field S is 0, :code:`$r13` (:code:`$sp`) otherwise. This instruction is useful for stack-frame manipulations.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an unaligned access exception is thrown.



type $r0...$r7  <- MEM[$rD + VALUE]
---------------------------------------------

*Instruction code*: 0x.e0.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |       0       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
The types of registers :code:`$r0` through :code:`$r7` are loaded from memory. The value of FIELD_A is computed by dividing VALUE by 4, and encoded as a ones-complement value. Thus, offset ranges of -28 to 28 are supported in steps of 4. Each nibble of the loaded value determines the type of one of the registers: the lowest 4 bits determine the type of :code:`$r0`; the highest 4 bits determine the type of :code:`$r7`. If a nibble has a value of 0xf, the type of the corresponding register is not changed.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an unaligned access exception is thrown.

type $r8...$r14  <- MEM[$rD + VALUE]
---------------------------------------------

*Instruction code*: 0x.e1.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |       1       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
The types of registers :code:`$r8` through :code:`$r14` are loaded from memory. The value of FIELD_A is computed by dividing VALUE by 4, and encoded as a ones-complement value. Thus, offset ranges of -28 to 28 are supported in steps of 4. Each nibble of the loaded value determines the type of one of the registers: the lowest 4 bits determine the type of :code:`$r0`; the next-to-highest 4 bits determine the type of :code:`$r8`. The highest nibble of the loaded value is ignored. If a nibble has a value of 0xf, the type of the corresponding register is not changed.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an unaligned access exception is thrown.




MEM[$rD + FIELD_A*4] <- type $r0...$r7
---------------------------------------------

*Instruction code*: 0x.e2.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |       2       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
The types of registers :code:`$r0` through :code:`$r7` are stored in memory. The value of FIELD_A is computed by dividing VALUE by 4, and encoded as a ones-complement value. Thus, offset ranges of -28 to 28 are supported in steps of 4. Each nibble of the stored value contains the type of one of the registers: the lowest 4 bits contain the type of :code:`$r0`; the highest 4 bits contain the type of :code:`$r7`.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an unaligned access exception is thrown.

MEM[$rD + FIELD_A*4] <- type $r8...$r14
---------------------------------------------

*Instruction code*: 0x.e3.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |       3       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
The types of registers :code:`$r8` through :code:`$r14` are stored in memory. The value of FIELD_A is computed by dividing VALUE by 4, and encoded as a ones-complement value. Thus, offset ranges of -28 to 28 are supported in steps of 4. Each nibble of the stored value contains the type of one of the registers: the lowest 4 bits contain the type of :code:`$r8`; the next-to-highest 4 bits contain the type of :code:`$r14`. The highest nibble of the stored value is implementation-defined.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an unaligned access exception is thrown.



type $r0...$r7  <- MEM[$rD + VALUE] & MASK
-----------------------------------------

*Instruction code*: 0x.f0. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
The types of registers :code:`$r0` through :code:`$r7` are loaded from memory. The value of FIELD_A is computed by dividing VALUE by 4, and encoded as a ones-complement value. Thus, offset ranges of -28 to 28 are supported in steps of 4. Each nibble of the loaded value determines the type of one of the registers: the lowest 4 bits determine the type of :code:`$r0`; the highest 4 bits determine the type of :code:`$r7`. If a nibble has a value of 0xf, the type of the corresponding register is not changed. Furthermore, each loaded value is masked by the corresponding bit in FIELD_E: bit 0 corresponds to :code:`$r0`, bit 7 to :code:`$r7`. If a bit is not set, the corresponding register type is not altered.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an unaligned access exception is thrown.

type $r8...$r14  <- MEM[$rD + VALUE] & MASK
---------------------------------------------

*Instruction code*: 0x.f1. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
The types of registers :code:`$r8` through :code:`$r14` are loaded from memory. The value of FIELD_A is computed by dividing VALUE by 4, and encoded as a ones-complement value. Thus, offset ranges of -28 to 28 are supported in steps of 4. Each nibble of the loaded value determines the type of one of the registers: the lowest 4 bits determine the type of :code:`$r0`; the next-to-highest 4 bits determine the type of :code:`$r8`. The highest nibble of the loaded value is ignored. If a nibble has a value of 0xf, the type of the corresponding register is not changed.Furthermore, each loaded value is masked by the corresponding bit in FIELD_E: bit 0 corresponds to :code:`$r8`, bit 6 to :code:`$r14`. If a bit is not set, the corresponding register type is not altered.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an unaligned access exception is thrown.










INV[$rA]
---------------------

*Instruction code*: 0x1ee.

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Invalidates any data- or instruction-cache lines that contain the logical address corresponding to the value of :code:`$rA`. Cache invalidation applies to both L1 and L2 level caches. System-level caches (L3) are not invalidated. In a multi-processor system, only local caches (caches that are in the path-to-memory for the core executing the instruction) are invalidated.

Dirty lines in data-caches are flushed to memory as they are invalidated.

The implementation is not allowed to throw exceptions even if the memory location violates access permissions. In these cases, the invalidation request is silently ignored.


$pc <- MEM32[$rA]
---------------------------------------------

*Instruction code*: 0x2ee.

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 32-bit value from memory location pointed to by :code:`$rA`. The value stored in :code:`$pc`. This is an indirect branch.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an unaligned access exception is thrown.


$tpc <- MEM32[$rA]
---------------------------------------------

*Instruction code*: 0x3ee.

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 32-bit value from memory location pointed to by :code:`$rA`. The value stored in :code:`$tpc`. This is an indirect branch in TASK mode.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an unaligned access exception is thrown.









INV[$rA + VALUE]
---------------------

*Instruction code*: 0x1fe. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Invalidates any data- or instruction-cache lines that contain the logical address corresponding to the value of :code:`$rA + VALUE`. Cache invalidation applies to both L1 and L2 level caches. System-level caches (L3) are not invalidated. In a multi-processor system, only local caches (caches that are in the path-to-memory for the core executing the instruction) are invalidated.

The value of FIELD_E is computed by truncating VALUE to 16 bits. The implementation sign-extend the value of FIELD_E prior to addition to the base register :code:`$rA`. Thus an offset range of -32768 to 32767 is supported.

Dirty lines in data-caches are flushed to memory as they are invalidated.

The implementation is not allowed to throw exceptions even if the memory location violates access permissions. In these cases, the invalidation request is silently ignored.


$pc <- MEM32[$rA + VALUE]
---------------------------------------------

*Instruction code*: 0x2fe. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 32-bit value from memory location pointed to by :code:`$rA + VALUE`. The value stored in :code:`$pc`. This is an indirect branch.

The value of FIELD_E is computed by truncating VALUE to 16 bits. The implementation sign-extend the value of FIELD_E prior to addition to the base register :code:`$rA`. Thus an offset range of -32768 to 32767 is supported.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an unaligned access exception is thrown.


$tpc <- MEM32[$rA + VALUE]
---------------------------------------------

*Instruction code*: 0x3fe. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 32-bit value from memory location pointed to by :code:`$rA + VALUE`. The value stored in :code:`$tpc`. This is an indirect branch in TASK mode.

The value of FIELD_E is computed by truncating VALUE to 16 bits. The implementation sign-extend the value of FIELD_E prior to addition to the base register :code:`$rA`. Thus an offset range of -32768 to 32767 is supported.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an unaligned access exception is thrown.







INV[VALUE]
---------------------

*Instruction code*: 0x1fef 0x**** 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Invalidates any data- or instruction-cache lines that contain the logical address :code:`VALUE`. Cache invalidation applies to both L1 and L2 level caches. System-level caches (L3) are not invalidated. In a multi-processor system, only local caches (caches that are in the path-to-memory for the core executing the instruction) are invalidated.

FIELD_E simply stores VALUE.

Dirty lines in data-caches are flushed to memory as they are invalidated.

The implementation is not allowed to throw exceptions even if the memory location violates access permissions. In these cases, the invalidation request is silently ignored.


$pc <- MEM32[VALUE]
---------------------------------------------

*Instruction code*: 0x2fef 0x**** 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 32-bit value from memory location pointed to by :code:`VALUE`. The value stored in :code:`$pc`. This is an indirect branch.

FIELD_E simply stores VALUE.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an unaligned access exception is thrown.


$tpc <- MEM32[VALUE]
---------------------------------------------

*Instruction code*: 0x3fef 0x**** 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 32-bit value from memory location pointed to by :code:`VALUE`. The value stored in :code:`$tpc`. This is an indirect branch in TASK mode.

FIELD_E simply stores VALUE.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an unaligned access exception is thrown.















$rD <- MEM8[$rA]
---------------------------------------------

*Instruction code*: 0x.e4.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |       4       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 8-bit value from memory location pointed to by :code:`$rA`. The value is zero-extended and stored in :code:`$rD`. The type of :code:`$rD` is not modified.

The implementation is allowed to throw exceptions if the memory access violates access permissions.

$rD <- MEM16[$rA]
---------------------------------------------

*Instruction code*: 0x.e5.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |       5       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 16-bit value from memory location pointed to by :code:`$rA`. The value is zero-extended and stored in :code:`$rD`. The type of :code:`$rD` is not modified.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 16-bit word boundary, an unaligned access exception is thrown.

$rD <- MEM32[$rA]
---------------------------------------------

*Instruction code*: 0x.e6.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |       6       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 32-bit value from memory location pointed to by :code:`$rA`. The value stored in :code:`$rD`. The type of :code:`$rD` is not modified.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an unaligned access exception is thrown.

$rD <- MEMLL32[$rA]
---------------------------------------------

*Instruction code*: 0x.e7.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |       7       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 32-bit value from memory location pointed to by :code:`$rA`. The value stored in :code:`$rD`. The type of :code:`$rD` is not modified. A load-lock is placed on the memory location.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an unaligned access exception is thrown. If an exception is thrown, no lock is placed.

MEM8[$rA] <- $rD
---------------------------------------------

*Instruction code*: 0x.e8.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |       8       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
The lowest 8 bits of :code:`$rD` is stored in the memory location pointed to by :code:`$rA`.

The implementation is allowed to throw exceptions if the memory access violates access permissions.

MEM16[$rA] <- $rD
---------------------------------------------

*Instruction code*: 0x.e9.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |       9       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
The lowest 16 bits of :code:`$rD` is stored in the memory location pointed to by :code:`$rA`.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 16-bit word boundary, an unaligned access exception is thrown.


MEM[32][$rA] <- $rD
---------------------------------------------

*Instruction code*: 0x.ea.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |       a       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
The value of :code:`$rD` is stored in the memory location pointed to by :code:`$rA`.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an unaligned access exception is thrown.



MEMSC[32][$rA] <- $rD
---------------------------------------------

*Instruction code*: 0x.e9.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |       b       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
The value of :code:`$rD` is stored in the memory location pointed to by :code:`$rA`, if and only if a still valid load-lock exists for the same address for the same processor. If such a lock is not found, the store fails and no memory update is performed.

The value of :code:`$rD` is set to 0 if the store succeeded and to non-zero if it failed. The actual non-zero value is implementation-defined and is not required to be constant, only that it is never zero. The type of :code:`$rD` is not changed.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an unaligned access exception is thrown. In case of an exception, neither the existence of a lock nor the value stored in memory is altered.



$rD <- SMEM8[$rA]
---------------------------------------------

*Instruction code*: 0x.ec.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |       c       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 8-bit value from memory location pointed to by :code:`$rA`. The value is sign-extended and stored in :code:`$rD`. The type of :code:`$rD` is not modified.

The implementation is allowed to throw exceptions if the memory access violates access permissions.

$rD <- SMEM16[$rA]
---------------------------------------------

*Instruction code*: 0x.ed.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |       d       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 16-bit value from memory location pointed to by :code:`$rA`. The value is sign-extended and stored in :code:`$rD`. The type of :code:`$rD` is not modified.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 16-bit word boundary, an unaligned access exception is thrown.

















Offset-indirect load/store group
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       f       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

==================  ====================================    ==================
Instruction code    Assembly                                Operation
==================  ====================================    ==================
0x.f4. 0x****       $rD <- MEM8[$rA+FIELD_E]                8-bit unsigned load from MEM[$rA+FIELD_E] into $rD
0x.f5. 0x****       $rD <- MEM16[$rA+FIELD_E]               16-bit unsigned load from MEM[$rA+FIELD_E] into $rD
0x.f6. 0x****       $rD <- MEM[32][$rA+FIELD_E]             32-bit load from MEM[$rA+FIELD_E] into $rD
0x.f7. 0x****       $rD <- MEMLL[32][$rA+FIELD_E]           32-bit unsigned load-reserve (exclusive load)
0x.f8. 0x****       MEM8[$rA+FIELD_E] <- $rD                8-bit store to MEM[$rA+FIELD_E] from $rD
0x.f9. 0x****       MEM16[$rA+FIELD_E] <- $rD               16-bit store to MEM[$rA+FIELD_E] from $rD
0x.fa. 0x****       MEM[32][$rA+FIELD_E] <- $rD             32-bit store to MEM[$rA+FIELD_E] from $rD
0x.fb. 0x****       MEMSR[32][$rA+FIELD_E] <- $rD           32-bit store-release (exclusive store)
0x.fc. 0x****       $rD <- SMEM8[$rA+FIELD_E]               8-bit signed load from MEM[$rA+FIELD_E] into $rD
0x.fd. 0x****       $rD <- SMEM16[$rA+FIELD_E]              16-bit signed load from MEM[$rA+FIELD_E] into $rD
==================  ====================================    ==================

.. note:: FIELD_E is sign-extended before addition
.. note:: Loads don't change the type of a register.



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


.. include:: isa_details_binary_alu.rst
.. include:: isa_details_ext_groups.rst
.. include:: isa_details_prefix_groups.rst
