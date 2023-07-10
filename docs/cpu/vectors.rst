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
        $r2 <- SET_VEND $r7      # Set and store number of elements *actually* processed in the iteration in $r2
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

Lane predication, or the lack of it
-----------------------------------

Brew doesn't have lane predicated operations, but has instructions to compute predication masks. These can later be used to combine vector lanes.

For instance, let's assume we want to compute the element-wise square of a vector, but only if the elements are greater then 1. Otherwise we leave the elements alone.

::
    $r5 <- 1
    $r0 <- if $r4 > $r5 # Here $r5 gets broadcast to all lanes during the comparison
    $r6 <- $r4 * $r4
    $r6 <- ~$r0 & $r6 # Zero out the elements that were less than 1
    $r4 <- $r0 & $r4 # Zero out the elements that were greater than 1
    $r4 <- $r4 | $r6 # Combine the results

This of course can be put in an SVI loop for larger vectors.

.. note:: since operations are not predicated, exceptions can still fire for elements that should be ignored.

.. todo:: I don't yet know how to deal with floating point exceptions (IEEE in that regard is painful, I believe), but load-stores could also be problematic.

Context changes
---------------

There is an inherent problem with vector ISAs: they hold a lot of state. This of course is great for performance as state needs to be spilled into memory much less frequently and even when it does, it can be done much more efficiently. However, this state is a problem whenever the execution context needs to change.

Drawing on the Cray experience: on the one hand, one could say that if a code doesn't touch vector registers, it's context doesn't need to include them, on the other, the Cray libraries made extensive use of vector registers for very mundane tasks, such as memcpy or strlen. These are so commonly used, it's hard to imagine many programs that would not touch vector registers.

What can be said though is that there could be significant sections of execution when no vector registers are touched. If a context switch happens in those sections, the previously saved vector values are still valid, no need to update them.

The way Cray dealt with this was to provide a 'vector-registers-are-dirty' bit that could be cleared by the kernel and set by the CPU whenever a vector register was touched. For them, this was a bit in the memory-held state block, but it could be wherever.

Load/store
----------

We have an even bigger problem, actually: the amount of data loaded/stored depends on the pre-set register type. This is very difficult to handle in - for example - stack frames, where $sp would need to be adjusted according to the total number of bytes stored, but that isn't known, at least not statically.

Arguably even worse, every load/store now works at arbitrary sizes, which is a *huge* security hole! If one can inject the wrong type into a library or program, that code can either overwrite things it's not supposed to, or load stuff it should not have access to. This later can be used to reveal sensitive information, even if the type gets corrected later on: the extra values still exist in the registers, so re-casting the register to the right type would unmask the hidden context.

The load/store problem could be solved by providing a vector load/store variant. This would mean that all existing load/stores would only accept (or care about) the first 32 bits of data. The vector loads/store variants would store a whole register. Their size would be something that is known for a given implementation. This can be done as an (almost) prefix instruction that can go in front of any load/store to make them vectorized.

.. todo:: Not sure of the 8- and 16-bit size and zero-extension versions make much sense. They are rather difficult to implement, probable better left for a load+widening operation.

Type changes must touch values
------------------------------

Otherwise there's is security hole in here: Let's say that kernel code does a sensitive memcpy using vector registers. Then, it changes context to a user-task. This change involves changing the type of these registers to scalar and restoring their values. Now, in user-land, we can change the types back to vector ones, and voila: we have the values of a potentially rather large section of kernel space.

To solve this, type changes need to be required to zero out top bits of the registers, or at least pretend to do so. One way of implementing this cheaply is each register (on top of its type) to have a size field.

When a register is type-cast to a shorter type, the size field is adjusted. When a register is type-cast to a larger type, the size field is *not* adjusted. When a value is stored in the register, the size field is adjusted. When a value is used from a register, bits beyond the limit indicated by the size field are masked to 0.

.. note:: the type-override prefix instruction uses the shorter of the size field in the register and the size field associated with the type override.

Load/store multiple and stacks
------------------------------

Oh, dear, this is difficult: the problem is that the *length* of the stored registers now depend on the type. So, in order to make the load/store process even remotely reasonable, we would need to start with loading/storing the types. This, however runs havoc with exception handling: we can't update the types until we're certain we have the value as well. Not only that, but what about the typeless ISA variant? Waste 64 bits of state?

Regardless of implementation headaches, the problem of context save/restore pops up in two major ways: when we swap execution contexts and when we do a function call. The common problem in both cases is that we don't know the types of the registers we want to save/restore, thus we don't know how much storage we need. Being conservative is wasteful, but if we aren't, we have a dynamic stack-frame size issue. Not only that, but every stack-operation after the first unknown sized store have dynamic addresses. Same for loads in reverse.

We can wrap all this complexity into the load/store multiple, but that makes that instruction incredibly complex. Still worth it, given the alternative is to provide true push/pull instructions. That is a pandoras box, still requires dealing with types and still generates a rather long and complex prolog/epilog section.

So, what we should say is that load/store multiple works this way:

#. Gets a mask of registers to touch
#. Types for these registers are also touched, no ifs or buts.
#. The instruction uses an opaque (i.e. implementation defined) structure to store values. The only requirement is that if the same mask is given for the load as the store, the result should be 'as expected'.
   #. This might be problematic: in a context switch world we might want to restore a subset to maintain ex. return values. BTW: this is a problem even in the current variant of this instruction-pair. We will address that in a minute.
#. Somehow we would need to be notified about the amount of space taken up by the storage being used.
#. We incorporate a 'dirty' mask: each register maintains a sticky 'dirty' bit, which is set on every value or type update. It is cleared (or set) by a special instruction. There is a store multiple variant that skips registers with the 'dirty' bit cleared (still reserves storage, but doesn't actually store). The load variant of this is that *only* dirty registers are loaded.

This is insanely complex. Certainly needs several cycles and a sequencer to accomplish.

Right now we have these variants:

=========================  =======================================    ==================
Instruction code           Assembly                                   Operation
=========================  =======================================    ==================
0x.0ff 0x**** 0x****       $r0...$r14 <- MEM[$rD +]                   load any combination of registers with FIELD_E as mask, incrementing
0x.1ff 0x**** 0x****       MEM[$rD +] <- $r0...$r14                   store any combination of registers with FIELD_E as mask, incrementing
0x.2ff 0x**** 0x****       $r0...$r14 <- MEM[$rD -]                   load any combination of registers with FIELD_E as mask, decrementing
0x.3ff 0x**** 0x****       MEM[$rD -] <- $r0...$r14                   store any combination of registers with FIELD_E as mask, decrementing
=========================  =======================================    ==================

What we would need instead is something like this:

=========================  =======================================    ==================
Instruction code           Assembly                                   Operation
=========================  =======================================    ==================
0x.f0f 0x****              $r0...$r14 <- MEM[$rD +]                   load any combination of registers with FIELD_E as mask, incrementing
0x.f1f 0x****              MEM[$rD +] <- $r0...$r14                   store any combination of registers with FIELD_E as mask, incrementing
0x.f2f 0x****              $r0...$r14 <- MEM[$rD -]                   load any combination of registers with FIELD_E as mask, decrementing
0x.f3f 0x****              MEM[$rD -] <- $r0...$r14                   store any combination of registers with FIELD_E as mask, decrementing
0x.f4f 0x****              $rD; $r0...$r14 <- MEM[$rD +]              load any combination of registers with FIELD_E as mask, incrementing, return updated $rD
0x.f5f 0x****              $rD; MEM[$rD +] <- $r0...$r14              store any combination of registers with FIELD_E as mask, incrementing, return updated $rD
0x.f6f 0x****              $rD; $r0...$r14 <- MEM[$rD -]              load any combination of registers with FIELD_E as mask, decrementing, return updated $rD
0x.f7f 0x****              $rD; MEM[$rD -] <- $r0...$r14              store any combination of registers with FIELD_E as mask, decrementing, return updated $rD
=========================  =======================================    ==================

Where 'dirty' semantics would be controlled by the MSB of FIELD_E. The later four variants are effectively push/pull operations, so much so that maybe we should use push/pull syntax. For pop operations that touch $rD, the return value is still the updated base. Setting the 'D' bit for a pop might still be needed to mimic whatever the corresponding push did.

The usage would be:

For stack operations, we would always use the push/pull semantics. That is, we would always set the MSB of field_e would never save/restore $sp itself, and would always use $sp as the base register.

For context switches we would never use the push/pull semantics and would always use the 'dirty' bit semantics.

Now, the nice thing in this world is that the 'blob' is opaque. So a typeless variant doesn't have to waste space storing the types. By manipulating the dirty bits, we can also control restore in a SYSCALL return to not restore registers which we want to keep the new values for - this however makes 'dirty' implementation at least in some simple form mandatory.

Register metadata
-----------------

So, the metadata we have with registers is the following:

TYPE  - 4 bits, describing the type
SIZE  - 1 bit (maybe more, if we have more complex types at some point), determining if the *value* in the register is scalar or vector
DIRTY - 1 bit, saying if the value of the register was modified.

Vector metadata
---------------

vrlen:  the architectural vector length, that is the number of bits/bytes/words/whatever a HW vector register stores.
vstart: the first byte index to be touched by a vector operation
vend:   the last byte index to be touched by a vector operation
