FENCE_*
-------

*Instruction code*: 0x.001

*Exceptions*: None

*Type variants*: No

*Assembly mnemonics*: FENCE_RW_RW; FENCE__W_RW; FENCE_R__RW; FENCE____RW; FENCE_RW__W; FENCE__W__W; FENCE_R___W; FENCE_____W; FENCE_RW_R\_; FENCE__W_R\_; FENCE_R__R\_; FENCE____R\_; FENCE_RW___; FENCE__W___; FENCE_R____; FENCE_RW_RW;

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

