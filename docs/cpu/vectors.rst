Vector operations
=================

Here are some initial thoughts on how vector operations should work.

The `RiscV vector extension <https://inst.eecs.berkeley.edu/~cs152/sp20/handouts/sp20/riscv-v-spec.pdf>`_ is a good source for how others are doing this.

In Brew, vector registers are any register with a vector type associated with it. These registers have an implementation-defined length, which must be a power of two bits and a multiple of 32 bits.

Consequently there are up to 15 vector registers.

There is some additional internal state corresponding to vector operations:

#. VSTART: which is a byte pointer to the first element in the vector register to be processed (it must be aligned to an element boundary)
#. VEND: which is the byte pointer to after the last element in the vector register to be processed (it must be aligned to an element boundary)
#. VLEN: implementation-defined length of a vector register in bytes. (this is a static, read-only value)

Only elements between VSTART and VEND are altered or modified by an operation. Load/store element offsets however always calculated from element 0, even if VSTART is non-0. (In other words VSTART only impacts byte-enable signals, but not offset calculations).

There is an instruction, which, given a desired vector length (in bytes) returns the number of bytes to be processed::

    $rD <- SETVEND($rS)

This instruction returns (and also sets VEND to) the greater of VLEN and $rS.

.. todo:: do we want to set these things in bytes or in elements?

Let's say we want to add two vectors: V1, V2 and put the result into V3. Let's say also that these vectors contain 16-bit floats.

::

    # $r4: points to V1
    # $r5: points to V2
    # $r6: points to V3
    # $r7: contains vector length (in bytes)
        type $r0 <- VFP16
        type $r1 <- VFP16
    loop:
        $r2 <- SETVEND($r7)      # Set and store number of elements *actually* processed in the iteration in $r2
        $r0 <- MEM[$r4]          # Load source
        $r1 <- MEM[$r5]          # Load source
        $r1 <- $r0 + $r1         # Do math
        MEM[$r6] <- $r1          # Store result
        $r4 <- $r4 + $r2         # Increment pointers
        $r5 <- $r5 + $r2         # Increment pointers
        $r6 <- $r6 + $r2         # Increment pointers
        $r7 <- $r7 - $r2         # Decrement count
        if $r7 != 0 $pc <- loop  # Loop, if needed

Notice, how we re-set VEND in every iteration of the loop. That's the crucial idea behind scalable vector implementations, of which Brew is one example.

Also notice how the type-system allows us to use the vector extension with the definition of a single custom instructions: SETVEND.

Exception handling
------------------

If an exception (or interrupt) is raised during any vector operation, VSTART will be set to point to the next element to be processed. This is especially important for load-store operations in cases when vector length is longer then the bus interface length. :code:`$tpc` points to the offending instruction. If a retry is attempted at that point VSTART will ensure that only the remaining portion of the vector operation is going to be performed.

State management
----------------

This is the ugly part: we now have two internal state registers (VSTART and VEND) that needs to be maintained during context switches. RiscV simply delegates these to CSRs and throws its arms in the air. That's certainly a way to deal (or rather not to deal) with the problem.

Loads, stores and memory alignment
----------------------------------

Vector storage alignment must be the smaller of:

* The actual vector length
* 256 bits

In other words: if I'm dealing with a vector of 32-bit length (4 8-bit values), it needs to be aligned to 32-bit boundaries. If I'm dealing with a vector of 256 FP32 elements, it needs to be aligned to a 256-bit boundary.

This requirement is a compromise: if there is an implementation that uses a wider than 256-bit external bus, it must deal with unaligned storage and related issues for large vectors. However, an implementation of that complexity can presumably afford it.

Special vector operations
-------------------------

In terms of actual operations, the following special operations need to be considered:

=============================================   ==========================================
Operation                                       Notes
=============================================   ==========================================
Widening operations                             These are type-casts, implemented as an extension group
Narrowing operations                            These are type-casts, implemented as an extension group
Lane swizzle operations                         Implemented as an extension group
Gather loads                                    Same as normal loads, if $rA is of a vector type
Scatter stores                                  Same as normal stores, if $rA is of a vector type
Masked loads and stores                         Loads can't be supported as it would need 3 operands; dropped
Vector compress                                 Implemented as an extension group
Mask generation                                 Implemented as an extension group
Predication of operations                       Not supported due to the required extra read port. Use lane-selection instead
Mask-based lane-selection                       Has been part of the ISA from the get go (two instructions due to the 2-read-port restriction)
=============================================   ==========================================


Type conflicts, lane-count conflicts
------------------------------------

We need to define what happens if an operation encounters incompatible types. Either due to the element type being incompatible or the lane-count being incompatible. I think the sane thing to do is that in general:

#. Require that element types are the same (i.e. can't add a float to an integer)
#. Require that lane counts are the same, except to allow for scalar broadcasting.
