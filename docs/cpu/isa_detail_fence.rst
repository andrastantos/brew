.. _fence_rw_rw:

FENCE_RW_RW
--------------

*Instruction code*: 0x0001

*Exceptions*: None

*Type variants*: No

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

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.



.. _fence__w_rw:

FENCE__W_RW
--------------

*Instruction code*: 0x1001

*Exceptions*: None

*Type variants*: No

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

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.




.. _fence_r__rw:

FENCE_R__RW
--------------

*Instruction code*: 0x2001

*Exceptions*: None

*Type variants*: No

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

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.



.. _fence____rw:

FENCE____RW
--------------

*Instruction code*: 0x3001

*Exceptions*: None

*Type variants*: No

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

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.




.. _fence_rw__w:

FENCE_RW__W
--------------

*Instruction code*: 0x4001

*Exceptions*: None

*Type variants*: No

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

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.




.. _fence__w__w:

FENCE__W__W
--------------

*Instruction code*: 0x5001

*Exceptions*: None

*Type variants*: No

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

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.




.. _fence_r___w:

FENCE_R___W
--------------

*Instruction code*: 0x6001

*Exceptions*: None

*Type variants*: No

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

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.




.. _fence_____w:

FENCE_____W
--------------

*Instruction code*: 0x7001

*Exceptions*: None

*Type variants*: No

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

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.




.. _fence_rw_r\\_:

FENCE_RW_R\_
--------------

*Instruction code*: 0x8001

*Exceptions*: None

*Type variants*: No

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

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.




.. _fence__w_r\\_:

FENCE__W_R\_
--------------

*Instruction code*: 0x9001

*Exceptions*: None

*Type variants*: No

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

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.




.. _fence_r__r\\_:

FENCE_R__R\_
--------------

*Instruction code*: 0xa001

*Exceptions*: None

*Type variants*: No

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

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.




.. _fence____r\\_:

FENCE____R\_
--------------

*Instruction code*: 0xb001

*Exceptions*: None

*Type variants*: No

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

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.




.. _fence_rw__\_:

FENCE_RW___
--------------

*Instruction code*: 0xc001

*Exceptions*: None

*Type variants*: No

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

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.




.. _fence__w__\_:

FENCE__W___
--------------

*Instruction code*: 0xd001

*Exceptions*: None

*Type variants*: No

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

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.




.. _fence_r___\_:

FENCE_R____
--------------

*Instruction code*: 0xe001

*Exceptions*: None

*Type variants*: No

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

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.



