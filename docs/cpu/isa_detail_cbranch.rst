




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

