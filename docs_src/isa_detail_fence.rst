.. _fence_rw_rw:

FENCE_RW_RW
--------------

*Instruction code*: 0x0001

*Short form*: ``fence_rw_rw``

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

The assembler accepts two spellings of each fence. The one used in this document gives both sides as a two-character field (``rw``, ``r_``, ``_w`` or ``__``): FENCE_<before>_<after>, for instance FENCE_R__RW. The short form drops the padding, so each side is ``rw``, ``r``, ``w`` or empty: ``fence_r_rw`` for FENCE_R__RW, ``fence__w`` for FENCE_____W, ``fence_rw_`` for FENCE_RW___. The disassembler prints the short form.

Fences have no effect on cache contents. In particular, fences don't invalidate the instruction cache (if exists) and cannot be exclusively used to implement coherency between data and instruction stream such as needed for self-modifying code.

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. 'Destination' in this context is anything outside the CPU core that can report a completion of the store. For instance, L1 and L2 caches can report completion in case of a write-back implementation. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.

.. note::
  Fences also can't be trusted for cache-coherency between cores or HW threads if the implementation doesn't explicitly have such guarantees.



.. _fence__w_rw:

FENCE__W_RW
--------------

*Instruction code*: 0x1001

*Short form*: ``fence_w_rw``

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

The assembler accepts two spellings of each fence. The one used in this document gives both sides as a two-character field (``rw``, ``r_``, ``_w`` or ``__``): FENCE_<before>_<after>, for instance FENCE_R__RW. The short form drops the padding, so each side is ``rw``, ``r``, ``w`` or empty: ``fence_r_rw`` for FENCE_R__RW, ``fence__w`` for FENCE_____W, ``fence_rw_`` for FENCE_RW___. The disassembler prints the short form.

Fences have no effect on cache contents. In particular, fences don't invalidate the instruction cache (if exists) and cannot be exclusively used to implement coherency between data and instruction stream such as needed for self-modifying code.

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. 'Destination' in this context is anything outside the CPU core that can report a completion of the store. For instance, L1 and L2 caches can report completion in case of a write-back implementation. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.

.. note::
  Fences also can't be trusted for cache-coherency between cores or HW threads if the implementation doesn't explicitly have such guarantees.


.. _fence_r__rw:

FENCE_R__RW
--------------

*Instruction code*: 0x2001

*Short form*: ``fence_r_rw``

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

The assembler accepts two spellings of each fence. The one used in this document gives both sides as a two-character field (``rw``, ``r_``, ``_w`` or ``__``): FENCE_<before>_<after>, for instance FENCE_R__RW. The short form drops the padding, so each side is ``rw``, ``r``, ``w`` or empty: ``fence_r_rw`` for FENCE_R__RW, ``fence__w`` for FENCE_____W, ``fence_rw_`` for FENCE_RW___. The disassembler prints the short form.

Fences have no effect on cache contents. In particular, fences don't invalidate the instruction cache (if exists) and cannot be exclusively used to implement coherency between data and instruction stream such as needed for self-modifying code.

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. 'Destination' in this context is anything outside the CPU core that can report a completion of the store. For instance, L1 and L2 caches can report completion in case of a write-back implementation. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.

.. note::
  Fences also can't be trusted for cache-coherency between cores or HW threads if the implementation doesn't explicitly have such guarantees.


.. _fence____rw:

FENCE____RW
--------------

*Instruction code*: 0x3001

*Short form*: ``fence__rw``

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

The assembler accepts two spellings of each fence. The one used in this document gives both sides as a two-character field (``rw``, ``r_``, ``_w`` or ``__``): FENCE_<before>_<after>, for instance FENCE_R__RW. The short form drops the padding, so each side is ``rw``, ``r``, ``w`` or empty: ``fence_r_rw`` for FENCE_R__RW, ``fence__w`` for FENCE_____W, ``fence_rw_`` for FENCE_RW___. The disassembler prints the short form.

Fences have no effect on cache contents. In particular, fences don't invalidate the instruction cache (if exists) and cannot be exclusively used to implement coherency between data and instruction stream such as needed for self-modifying code.

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. 'Destination' in this context is anything outside the CPU core that can report a completion of the store. For instance, L1 and L2 caches can report completion in case of a write-back implementation. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.

.. note::
  Fences also can't be trusted for cache-coherency between cores or HW threads if the implementation doesn't explicitly have such guarantees.



.. _fence_rw__w:

FENCE_RW__W
--------------

*Instruction code*: 0x4001

*Short form*: ``fence_rw_w``

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

The assembler accepts two spellings of each fence. The one used in this document gives both sides as a two-character field (``rw``, ``r_``, ``_w`` or ``__``): FENCE_<before>_<after>, for instance FENCE_R__RW. The short form drops the padding, so each side is ``rw``, ``r``, ``w`` or empty: ``fence_r_rw`` for FENCE_R__RW, ``fence__w`` for FENCE_____W, ``fence_rw_`` for FENCE_RW___. The disassembler prints the short form.

Fences have no effect on cache contents. In particular, fences don't invalidate the instruction cache (if exists) and cannot be exclusively used to implement coherency between data and instruction stream such as needed for self-modifying code.

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. 'Destination' in this context is anything outside the CPU core that can report a completion of the store. For instance, L1 and L2 caches can report completion in case of a write-back implementation. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.

.. note::
  Fences also can't be trusted for cache-coherency between cores or HW threads if the implementation doesn't explicitly have such guarantees.



.. _fence__w__w:

FENCE__W__W
--------------

*Instruction code*: 0x5001

*Short form*: ``fence_w_w``

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

The assembler accepts two spellings of each fence. The one used in this document gives both sides as a two-character field (``rw``, ``r_``, ``_w`` or ``__``): FENCE_<before>_<after>, for instance FENCE_R__RW. The short form drops the padding, so each side is ``rw``, ``r``, ``w`` or empty: ``fence_r_rw`` for FENCE_R__RW, ``fence__w`` for FENCE_____W, ``fence_rw_`` for FENCE_RW___. The disassembler prints the short form.

Fences have no effect on cache contents. In particular, fences don't invalidate the instruction cache (if exists) and cannot be exclusively used to implement coherency between data and instruction stream such as needed for self-modifying code.

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. 'Destination' in this context is anything outside the CPU core that can report a completion of the store. For instance, L1 and L2 caches can report completion in case of a write-back implementation. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.

.. note::
  Fences also can't be trusted for cache-coherency between cores or HW threads if the implementation doesn't explicitly have such guarantees.



.. _fence_r___w:

FENCE_R___W
--------------

*Instruction code*: 0x6001

*Short form*: ``fence_r_w``

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

The assembler accepts two spellings of each fence. The one used in this document gives both sides as a two-character field (``rw``, ``r_``, ``_w`` or ``__``): FENCE_<before>_<after>, for instance FENCE_R__RW. The short form drops the padding, so each side is ``rw``, ``r``, ``w`` or empty: ``fence_r_rw`` for FENCE_R__RW, ``fence__w`` for FENCE_____W, ``fence_rw_`` for FENCE_RW___. The disassembler prints the short form.

Fences have no effect on cache contents. In particular, fences don't invalidate the instruction cache (if exists) and cannot be exclusively used to implement coherency between data and instruction stream such as needed for self-modifying code.

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. 'Destination' in this context is anything outside the CPU core that can report a completion of the store. For instance, L1 and L2 caches can report completion in case of a write-back implementation. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.

.. note::
  Fences also can't be trusted for cache-coherency between cores or HW threads if the implementation doesn't explicitly have such guarantees.




.. _fence_____w:

FENCE_____W
--------------

*Instruction code*: 0x7001

*Short form*: ``fence__w``

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

The assembler accepts two spellings of each fence. The one used in this document gives both sides as a two-character field (``rw``, ``r_``, ``_w`` or ``__``): FENCE_<before>_<after>, for instance FENCE_R__RW. The short form drops the padding, so each side is ``rw``, ``r``, ``w`` or empty: ``fence_r_rw`` for FENCE_R__RW, ``fence__w`` for FENCE_____W, ``fence_rw_`` for FENCE_RW___. The disassembler prints the short form.

Fences have no effect on cache contents. In particular, fences don't invalidate the instruction cache (if exists) and cannot be exclusively used to implement coherency between data and instruction stream such as needed for self-modifying code.

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. 'Destination' in this context is anything outside the CPU core that can report a completion of the store. For instance, L1 and L2 caches can report completion in case of a write-back implementation. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.

.. note::
  Fences also can't be trusted for cache-coherency between cores or HW threads if the implementation doesn't explicitly have such guarantees.



.. _fence_rw_r\\_:

FENCE_RW_R\_
--------------

*Instruction code*: 0x8001

*Short form*: ``fence_rw_r``

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

The assembler accepts two spellings of each fence. The one used in this document gives both sides as a two-character field (``rw``, ``r_``, ``_w`` or ``__``): FENCE_<before>_<after>, for instance FENCE_R__RW. The short form drops the padding, so each side is ``rw``, ``r``, ``w`` or empty: ``fence_r_rw`` for FENCE_R__RW, ``fence__w`` for FENCE_____W, ``fence_rw_`` for FENCE_RW___. The disassembler prints the short form.

Fences have no effect on cache contents. In particular, fences don't invalidate the instruction cache (if exists) and cannot be exclusively used to implement coherency between data and instruction stream such as needed for self-modifying code.

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. 'Destination' in this context is anything outside the CPU core that can report a completion of the store. For instance, L1 and L2 caches can report completion in case of a write-back implementation. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.

.. note::
  Fences also can't be trusted for cache-coherency between cores or HW threads if the implementation doesn't explicitly have such guarantees.




.. _fence__w_r\\_:

FENCE__W_R\_
--------------

*Instruction code*: 0x9001

*Short form*: ``fence_w_r``

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

The assembler accepts two spellings of each fence. The one used in this document gives both sides as a two-character field (``rw``, ``r_``, ``_w`` or ``__``): FENCE_<before>_<after>, for instance FENCE_R__RW. The short form drops the padding, so each side is ``rw``, ``r``, ``w`` or empty: ``fence_r_rw`` for FENCE_R__RW, ``fence__w`` for FENCE_____W, ``fence_rw_`` for FENCE_RW___. The disassembler prints the short form.

Fences have no effect on cache contents. In particular, fences don't invalidate the instruction cache (if exists) and cannot be exclusively used to implement coherency between data and instruction stream such as needed for self-modifying code.

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. 'Destination' in this context is anything outside the CPU core that can report a completion of the store. For instance, L1 and L2 caches can report completion in case of a write-back implementation. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.

.. note::
  Fences also can't be trusted for cache-coherency between cores or HW threads if the implementation doesn't explicitly have such guarantees.




.. _fence_r__r\\_:

FENCE_R__R\_
--------------

*Instruction code*: 0xa001

*Short form*: ``fence_r_r``

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

The assembler accepts two spellings of each fence. The one used in this document gives both sides as a two-character field (``rw``, ``r_``, ``_w`` or ``__``): FENCE_<before>_<after>, for instance FENCE_R__RW. The short form drops the padding, so each side is ``rw``, ``r``, ``w`` or empty: ``fence_r_rw`` for FENCE_R__RW, ``fence__w`` for FENCE_____W, ``fence_rw_`` for FENCE_RW___. The disassembler prints the short form.

Fences have no effect on cache contents. In particular, fences don't invalidate the instruction cache (if exists) and cannot be exclusively used to implement coherency between data and instruction stream such as needed for self-modifying code.

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. 'Destination' in this context is anything outside the CPU core that can report a completion of the store. For instance, L1 and L2 caches can report completion in case of a write-back implementation. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.

.. note::
  Fences also can't be trusted for cache-coherency between cores or HW threads if the implementation doesn't explicitly have such guarantees.



.. _fence____r\\_:

FENCE____R\_
--------------

*Instruction code*: 0xb001

*Short form*: ``fence__r``

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

The assembler accepts two spellings of each fence. The one used in this document gives both sides as a two-character field (``rw``, ``r_``, ``_w`` or ``__``): FENCE_<before>_<after>, for instance FENCE_R__RW. The short form drops the padding, so each side is ``rw``, ``r``, ``w`` or empty: ``fence_r_rw`` for FENCE_R__RW, ``fence__w`` for FENCE_____W, ``fence_rw_`` for FENCE_RW___. The disassembler prints the short form.

Fences have no effect on cache contents. In particular, fences don't invalidate the instruction cache (if exists) and cannot be exclusively used to implement coherency between data and instruction stream such as needed for self-modifying code.

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. 'Destination' in this context is anything outside the CPU core that can report a completion of the store. For instance, L1 and L2 caches can report completion in case of a write-back implementation. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.

.. note::
  Fences also can't be trusted for cache-coherency between cores or HW threads if the implementation doesn't explicitly have such guarantees.




.. _fence_rw__\_:

FENCE_RW___
--------------

*Instruction code*: 0xc001

*Short form*: ``fence_rw_``

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

The assembler accepts two spellings of each fence. The one used in this document gives both sides as a two-character field (``rw``, ``r_``, ``_w`` or ``__``): FENCE_<before>_<after>, for instance FENCE_R__RW. The short form drops the padding, so each side is ``rw``, ``r``, ``w`` or empty: ``fence_r_rw`` for FENCE_R__RW, ``fence__w`` for FENCE_____W, ``fence_rw_`` for FENCE_RW___. The disassembler prints the short form.

Fences have no effect on cache contents. In particular, fences don't invalidate the instruction cache (if exists) and cannot be exclusively used to implement coherency between data and instruction stream such as needed for self-modifying code.

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. 'Destination' in this context is anything outside the CPU core that can report a completion of the store. For instance, L1 and L2 caches can report completion in case of a write-back implementation. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.

.. note::
  Fences also can't be trusted for cache-coherency between cores or HW threads if the implementation doesn't explicitly have such guarantees.




.. _fence__w__\_:

FENCE__W___
--------------

*Instruction code*: 0xd001

*Short form*: ``fence_w_``

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

The assembler accepts two spellings of each fence. The one used in this document gives both sides as a two-character field (``rw``, ``r_``, ``_w`` or ``__``): FENCE_<before>_<after>, for instance FENCE_R__RW. The short form drops the padding, so each side is ``rw``, ``r``, ``w`` or empty: ``fence_r_rw`` for FENCE_R__RW, ``fence__w`` for FENCE_____W, ``fence_rw_`` for FENCE_RW___. The disassembler prints the short form.

Fences have no effect on cache contents. In particular, fences don't invalidate the instruction cache (if exists) and cannot be exclusively used to implement coherency between data and instruction stream such as needed for self-modifying code.

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. 'Destination' in this context is anything outside the CPU core that can report a completion of the store. For instance, L1 and L2 caches can report completion in case of a write-back implementation. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.

.. note::
  Fences also can't be trusted for cache-coherency between cores or HW threads if the implementation doesn't explicitly have such guarantees.





.. _fence_r___\_:

FENCE_R____
--------------

*Instruction code*: 0xe001

*Short form*: ``fence_r_``

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

The assembler accepts two spellings of each fence. The one used in this document gives both sides as a two-character field (``rw``, ``r_``, ``_w`` or ``__``): FENCE_<before>_<after>, for instance FENCE_R__RW. The short form drops the padding, so each side is ``rw``, ``r``, ``w`` or empty: ``fence_r_rw`` for FENCE_R__RW, ``fence__w`` for FENCE_____W, ``fence_rw_`` for FENCE_RW___. The disassembler prints the short form.

Fences have no effect on cache contents. In particular, fences don't invalidate the instruction cache (if exists) and cannot be exclusively used to implement coherency between data and instruction stream such as needed for self-modifying code.

Depending on the implementation, some or all fence operations might be no-ops.

'Before' fence operations ensure that all reads and writes complete before the processor is allowed to continue execution. For writes, it is guaranteed that all stored values reached their final destination. 'Destination' in this context is anything outside the CPU core that can report a completion of the store. For instance, L1 and L2 caches can report completion in case of a write-back implementation. For reads, it is guaranteed that all values have reached the processor core (but might not yet be committed to the register file).

'After' fence operations ensure that none of the reads or writes start execution prior to the completion of the fence instruction. This is a potential issue for out-of-order implementations, where the apparent instruction execution order is different from the actual one. 'After' fences can be used to ensure that loads and stores maintain their program-order even in an out-of-order machine.

Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.

.. note::
  Fences also can't be trusted for cache-coherency between cores or HW threads if the implementation doesn't explicitly have such guarantees.



