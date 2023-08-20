GPU extension
=============

The GPU extension a custom type to support 3D graphics-oriented workloads.

New types
----------------

==========    =========  ============== ==================== ========================== ============ ==========
Type code     Type name  Scalar/Vector  Fixed/Floating point Compatible type            Logic type   Note
==========    =========  ============== ==================== ========================== ============ ==========
0x8           VFP16      Vector         Float                FP64, FP32                 VINT16       16-bit float vector
==========    =========  ============== ==================== ========================== ============ ==========


Updates to instruction behavior
-------------------------------

Type broadcast
~~~~~~~~~~~~~~

Type broadcast behavior is extended for the new types as follows:

For broadcasting FP32 into VFP16, the bottom 3 bits of the exponent and the bottom 13 bits of the mantissa are truncated. The resulting FP16 number is then copied to all lanes.


Instruction set changes
-----------------------

$rD <- float $rA
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*Instruction code*: 0x.07.

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
...........

The conversion operation is extended over the standard implementation as follows:

================   ===============
Input logic type   Output type
================   ===============
VINT16             VFP16
================   ===============
