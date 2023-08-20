Vector operations
=================

.. note::
    The `RiscV vector extension <https://inst.eecs.berkeley.edu/~cs152/sp20/handouts/sp20/riscv-v-spec.pdf>`_ is a good source for how others are doing this.

The vector architecture of the Brew ISA uses variable operational vector lengths and combines them with fixed HW vector register length. Brew integrates vector functionality using its type system. Many instructions perform both scalar and vector operations based on the register types they are presented with.

Vector registers have an implementation-defined length, which must be a power of two number of bits and at least 32. Any register can be associated with a scalar or vector type, consequently there are up to 15 vector registers.

To support vector operations, the following status registers are maintained:

#. :code:`vstart`: This optional register controls the first byte of a vector registers that is processed in load-store operations. See :ref:`vstart handling<vstart_handling>` for details.
#. :code:`vend`: This register controls the last byte of the vector registers that is processed, effectively setting the active vector length. See :ref:`vend handling<vend_handling>` for details.
#. :code:`vlen`: This implementation-defined constant defines length of the HW vector registers in bytes


Normally vector operations disregard :code:`vstart` and access elements from byte 0 through :code:`vend`. There are some exceptions to this rule, namely special load/store operations exist that ignore the value held in :code:`vend`. These instructions are useful for saving and restoring context.

If :code:`vstart` is implemented, Load/store operations skip element below :code:`vstart`, but their offsets always calculated from element 0.

To understand the basic concept, let's look at a simple example: we want to add two vectors: V1, V2 and put the result into V3. Let's say also that these vectors contain 32-bit floats. Their length is the same and known, but can be arbitrarily large or small.

::

    # $r4: points to V1
    # $r5: points to V2
    # $r6: points to V3
    # $r7: contains vector length (in bytes)
        type $r0 <- VFP32
        type $r1 <- VFP32
    loop:
        $r2 <- set_vend $r7      # Set and store number of elements *actually* processed in the current iteration in $r2
        $r0 <- MEM[$r4]          # Load source
        $r1 <- MEM[$r5]          # Load source
        $r1 <- $r0 + $r1         # Do math
        MEM[$r6] <- $r1          # Store result
        $r4 <- $r4 + $r2         # Increment pointers
        $r5 <- $r5 + $r2         # Increment pointers
        $r6 <- $r6 + $r2         # Increment pointers
        $r7 <- $r7 - $r2         # Decrement count
        if $r7 != 0 $pc <- loop  # Loop, if needed

Notice, how we re-compute :code:`vend` in every iteration of the loop. That's the crucial idea behind scalable vector implementations, of which Brew is one example.

Exception handling for vector instructions
------------------------------------------

The :ref:`exception handling<exception_handling>` rules guarantee that all base-set exception with the exception of :code:`exc_unaligned` are raised at instruction boundaries. That is, a vector instruction is either fully executed or not executed at all.

:code:`exc_unaligned` and other, implementation-defined memory-access exceptions (such as access right violations) can potentially be raised mid-execution for vector load-store operations, where some but not all the side-effects of the instruction have taken place.

In those cases, :code:`vstart` will be set to point to the element which caused the exception. If a retry is attempted later on, :code:`vstart` will ensure that only the remaining portion of the vector operation is going to be performed.

.. note:: :code:`vstart` implementation is optional. If not implemented, a retry will restart the full load-store operation. If :code:`vstart` is implemented, it requires sequential processing of elements in a vector.

State management
----------------

With the introduction of vector types, he (effective) length of a register is not constant anymore; it depends on its type.

Because of this, saving and restoring machine state is difficult. This problem occurs under a few circumstances:

#. At function prologs/epilogs where callee-saved registers must be spilled to the stack
#. Upon entering/laving SCHEDULER mode where the TASK-mode context needs to be saved/restored
#. Spilling of registers inside a function to relieve register pressure

In the first two cases, the type of the registers - and consequently the storage space required for them - is not statically known. Upper bounds of course exist, but they are wasteful both in terms of memory usage and memory access.

:ref:`Store/load multiple and PUSH/POP<load_store_multiple>` operations are provided as an optimal - albeit complex - solution to these issues.

The problem in the third case is that :code:`vend` may or may not be applicable to the register value we're spilling or restoring.

A set of :ref:`load<full_rd_eq_mem_value>`/:ref:`store<mem_value_eq_full_rd>` operations exist that work on the *full* register and ignore the value of :code:`vend` to help with this situation.

Loads, stores and memory alignment
----------------------------------

Loading and storing of vector registers is allowed 32-bit aligned addresses.

:code:`VSTART`/:code:`VEND` alignment
-------------------------------------

:code:`VEND` is always truncated to element size increments. So, for instance if the type is :code:`VINT32`, but :code:`vend` is set to 13, it is treated as if it was set to 12.

Unaligned :code:`VSTART` handling is implementation defined: under normal circumstances SW never touches the value of this register, so no reason to define general guidelines.

Special vector operations
-------------------------

In terms of actual operations, the following special operations need to be considered:

=============================================   ==========================================
Operation                                       Notes
=============================================   ==========================================
Widening operations                             These are type-casts, implemented as an extension group
Narrowing operations                            These are type-casts, implemented as an extension group
Lane swizzle operations                         Implemented as an extension group
Gather loads                                    ::TODO:: NEED TO BE IMPLEMENTED The only thing that makes sense is MEM[VALUE+$rA], where VALUE is 32-bits
Scatter stores                                  ::TODO:: NEED TO BE IMPLEMENTED The only thing that makes sense is MEM[VALUE+$rA], where VALUE is 32-bits
Masked loads and stores                         Loads can't be supported as it would need 3 operands; dropped
Vector compress                                 Implemented as an extension group
Mask generation                                 Implemented as an extension group
Predication of operations                       Not supported due to the required extra read port. Use lane-selection instead
Mask-based lane-selection                       Has been part of the ISA from the get go (two instructions due to the 2-read-port restriction)
Lane injection/extraction                       ::TODO:: NEED TO BE IMPLEMENTED
=============================================   ==========================================

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

.. note:: since operations themselves are not predicated, it's not possible to do masked loads/stores. Those would need to be implemented as read-modify-writes; which has potentially different exception semantics.

Context changes
---------------

There is an inherent problem with vector ISAs: they hold a lot of state. This of course is great for performance as state needs to be spilled into memory much less frequently and even when it is, it can be done much more efficiently - using burst transfers. However, this state is a problem whenever the execution context needs to change.

Drawing on the Cray experience: on the one hand, one could say that if a code doesn't touch vector registers, it's context doesn't need to include them, on the other, the Cray libraries made extensive use of vector registers for very mundane tasks, such as memcpy or strlen. These are so commonly used it's hard to imagine many programs that would not touch vector registers.

What can be said though is that there could be significant sections of execution when no vector registers are touched. If a context switch happens in those sections, the previously saved vector values are still valid, no need to update them.

The way Cray dealt with this was to provide a 'vector-registers-are-dirty' bit that could be cleared by the kernel and set by the CPU whenever a vector register was touched. For them, this was a bit in the memory-held state block, but it could be wherever.

In Brew, we have a dirty bit for each register. During context switch, we can use the dirty mask to not store back registers whose value didn't change. Of course we also have to store and re-load the dirty map during the context switch, otherwise it's value can't be trusted.

Just as on the Cray though, this is a massively complex operation, really not of the 'RISC' creed. However, doing it in SW is even more complex and would be hopelessly slow.

Function prologs and epilogs
----------------------------

We have a big problem in this arena too: the amount of data loaded/stored depends on the pre-set register type. This is very difficult to handle in - for example - stack frames, where $sp would need to be adjusted according to the total number of bytes stored, but that isn't known, at least not statically. To handle this, PUSH/POP multiple operations are provided which can be used to spill a specified part of the architectural register state onto the stack. These create an implementation-defined structure on the stack and return the updated stack pointer. They can be used to spill/restore any combination of registers, solving two problems at once: the function prolog/epilog is very short now and the fact that the size of the stack space needed depends on the (run-time) types of the registers can be handled in the layout of the implementation defined blob.

The down-side of course is that these instructions are extremely complex. They certainly are not single-cycle, need a complex FSM to implement, and made even more complex by the need of precise (restartable) exception handling.

Security holes everywhere
-------------------------

If the previous problem wasn't big enough, we have another one coming on its heels: every load/store now works on run-time defined sizes, which is a *huge* security hole! If one can inject the wrong type into a library or program, that code can either overwrite things it's not supposed to, or load stuff it should not have access to. This later can be used to reveal sensitive information, even if the type gets corrected later on: the extra values still exist in the registers, so re-casting the register to the right type would unmask the hidden context (this latter issue is dealt with in the next chapter).

To mitigate this problem, a set of quick check instructions are provided that allow for checking if a register (or block of registers) is of a given type. These instructions can be deployed for instance in function prologs to test that register-passed arguments are of an assumed type. Then, the assumed register types can be quickly loaded by :code:`type $r0...$r7 <- VALUE` instructions.

.. note:: Maybe we don't care about the type check and simply load the assumed types?

.. note:: Since we use push/pop multiple to save caller context, the types of callee-saved registers are preserved. Call-clobbered registers don't provide any type persistence guarantees anyway, so blowing them away is kosher behavior.

Another problem arises when we try to save/restore individual vector registers: normal load/stores use VSTART/VEND to guide their behavior, but that's not what we want here: we want to preserve the full HW value. A set of loads and stores are thus provided that ignore VSTART and VEND.

.. todo:: Not sure of the 8- and 16-bit size and zero-extension versions make much sense. They are rather difficult to implement, probable better left for a load+widening operation.

.. todo:: RiscV provides strided loads/stores. These are highly useful for loading transposed matrices, but are complex to implement. Right now we're not supporting them, but should we? We can actually simulate these with scatter/gather loads/stores. Once the indices are set up, the vector register can be changed by adding a scalar to it, which would get broadcast across all elements. The setting up of the stride is a chore though.

.. todo:: We do support scatter/gather loads and stores using the MEM[$r1] <- $rB instruction, if $r1 happens to be a vector register.

Type changes must touch values
------------------------------

This is where we left off in the previous topic: Let's say that kernel code does a sensitive memcpy using vector registers. Then, it changes context to a user-task. This change involves changing the type of these registers to scalar and restoring their values. Now, in user-land, we can change the types back to vector ones. If type-changes don't touch values the user task would suddenly have unmasked values of a potentially rather large section of kernel space.

To solve this, type changes are required to zero out top bits of the registers, or at least pretend to do so. One way of implementing this cheaply is each register (on top of its type) to have a size field.

When a register is type-cast to a shorter type, the size field is adjusted. When a register is type-cast to a larger type, the size field is *not* adjusted. When a value is stored in the register, the size field is adjusted. When a value is used from a register, bits beyond the limit indicated by the size field are masked to 0.

.. note:: the type-override prefix instruction uses the shorter of the size field in the register and the size field associated with the type override.

Load/store/push/pull multiple
------------------------------

Oh, dear, this is difficult: the problem is that the *length* of the stored registers now depend on the type. So, in order to make the load/store process even remotely reasonable, we would need to start with loading/storing the types. This, however runs havoc with exception handling: we can't update the types until we're certain we have the value as well. Not only that, but what about the typeless ISA variant? Waste 64 bits of state?

Regardless of implementation headaches, the problem of context save/restore pops up in two major ways: when we swap execution contexts and when we do a function call. The common problem in both cases is that we don't know the types of the registers we want to save/restore, thus we don't know how much storage we need. Being conservative is wasteful, but if we aren't, we have a dynamic stack-frame size issue. Not only that, but every stack-operation after the first unknown sized store have dynamic addresses. Same for loads in reverse.

We can wrap all this complexity into the load/store/push/pop multiple, but that makes that instruction incredibly complex. Still worth it, given the alternatives.

For these operations, we provide the following inputs:

#. A mask of which registers to involve
#. An optional skip-mask (in the form of a register). These registers are skipped for updated/storage
#. An address to load/store/pus/pop the contents from in the form of a pointer register

Given these, the CPU creates an implementation-defined blob in the pointed location with the following guarantees:

#. The block layout is well documented
#. The block contains space for all registers in the mask
#. The block contains types and values for all registers that are in the mask and not skipped
#. Loading a blob with the same mask is always possible independent of the skip field content.

Variants of the instructions can use an implementation-defined 'dirty' bit and skip registers that are/are not dirty.

On top of all this PUSH/POP variants are to update the blob pointer with the size of the created/consumed blob. The blob structure should allow for POP to operate, given it's pointer points to after the end of the blob. For instance, the last word in the blob could be a size field, so POP can read that and find the beginning of the blob.

During the load/store of vector registers VSTART/VEND should not be modified or consulted: the whole length of the HW register is accessed.

Exceptions further complicate this process: there must be a way to restart a partial load/store/push/pop multiple. I don't know how to do that at the moment!

Needless to say, this is insanely complex. Certainly needs several cycles and a sequencer to accomplish.

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

Context switch
--------------

With all these, a context change in SCHEDULER-mode would look something like this:

::
    .text
        run_task:
            # We're about to return to a task.
            # The task context pointer is in $r0.
            # The register mask we want to return to the task is in $r1.
            # $lr can't be returned, it will always be restored from the saved context.
            # $r2 is throw-away. We clobber $r1 as well.
            # Upon return, $r0 still points to the (newly updated) context pointer.
            # There are many other things we care about, such as:
            # - vstart, vend
            # - MMU base address (or base/limit registers)
            # - OS related info

            $r2 <- sched_context
            MEM[$r0] <- $r0, $r3 ... $r14 # Save full SCHEDULER context
            MEM[cur_context] <- $r0
            $lr <- $r0 + cur_context_size
            $r0 ... $r13 <- MEM[$r0], SKIP=$r1
            # At this point the only register we can manipulate is $lr.
            # The address to finally restore it from is in $lr itself
            MEM[lr_save] <- $lr
            $lr <- MEM[$lr + cur_context_lr_size] # Load DIRTY mask
            DIRTY <- $lr
            $lr <- MEM[$lr + cur_context_lr_size + 4] # Load VSTART
            VSTART <- $lr
            $lr <- MEM[$lr + cur_context_lr_size] # Load VEND
            VEND <- $lr
            $lr <- MEM[lr_save]
            $lr ... $lr <- MEM[$lr]
            # At this point the full context of the task is restored, we're ready to return to TASK mode
            stm

            # We need a register. Use $lr as that's the most likely to be a scalar.
            # We need to save it (and it's type) to a static location before we can move on.
            MEM[lr_save] <- full $lr
            $lr <- type $lr
            MEM[lr_type_save] <- $lr
            # Load context pointer and save everything (in two steps)
            $lr <- MEM[cur_context]
            MEM[$lr] <- $r0 ... $r13, DIRTY # Save all dirty registers (except $lr) to context
            $sp <- $lr + cur_context_size # Use SP here because we're going to ruin it's DIRTY bit. It is likely dirty anyway
            $lr <- MEM[lr_type_save]
            type $lr <- $lr
            $lr <- MEM[lr_save]
            MEM[$sp] <- $lr ... $lr
            $lr <- DIRTY
            MEM[$sp + cur_context_lr_size] <- $lr
            $lr <- VSTART
            MEM[$sp + cur_context_lr_size + 4] <- $lr
            $lr <- VEND
            MEM[$sp + cur_context_lr_size + 8] <- $lr
            # At this point we've saved off the current context. We can restore the context of the SCHEDULER
            $r0 <- sched_context
            $r0, $r3 ... $r14 <- MEM[$r0] # Restore everything we might care about
            $pc <- $lr

    .bss:
        sched_context: # enough storage for the full SCHEDULER context
            dw ...
        cur_context: # pointer to the current TASK context
            dw 0
        lr_save: # enough storage for the largest HW register
            dw ...
        lr_type_save: # Just a 32-bit integer
            dw 0

This is not short, but maybe acceptable. It's 25 instructions, of course some of them are many many cycles long.


Vectors simplified
==================

Let's see if we can simplify things. One thing we can try is *not* to have multiple load/store, instead have the following:

This is how you would push a single value::

    $rD <- size $rA  # This would load the run-time size of $rA in bytes into $rD
    mem[$sp] <- full $rA
    $sp <- $sp - $rD
    $rD <- type $rA
    mem[$sp] <- $rD
    $sp <- $sp - 4

And the corresponding pop:

    $sp <- $sp + 4
    type $rD <- INT32 # Might not be needed if can be guaranteed
    $rD <- mem[$sp]
    type $rA <- $rD
    $rD <- size $rA
    $sp <- $sp - $rD
    full $rA <- mem[$sp]

This is 6 instructions (each) to pop/pull a single value!

The context change variant is::

    .text
        run_task:
            # We're about to return to a task.
            # The task context pointer is in $r0, type INT32
            # The register mask we want to return to the task is in $r1, type INT32
            # $lr can't be returned, it will always be restored from the saved context.
            # $r2 is throw-away. We clobber $r1 as well.
            # Upon return, $r0 still points to the (newly updated) context pointer.
            # There are many other things we care about, such as:
            # - vstart, vend
            # - MMU base address (or base/limit registers)
            # - OS related info


            type $r2 <- INT32
            $r2 <- sched_context

            MEM[$r2 + type_ofs_1] <- type $r8 ... $r14
            MEM[$r2 + slot_size*6] <- full $r8
            MEM[$r2 + slot_size*7] <- full $r9
            MEM[$r2 + slot_size*8] <- full $r10
            MEM[$r2 + slot_size*9] <- full $r11
            MEM[$r2 + slot_size*10] <- full $r12
            MEM[$r2 + slot_size*11] <- full $r13
            MEM[$r2 + slot_size*12] <- full $r14

            $lr <- DIRTY
            MEM[$r2 + dirty_ofs] <- $lr
            $lr <- VSTART
            MEM[$r2 + vstart_ofs] <- $lr
            $lr <- VEND
            MEM[$r2 + vend_ofs] <- $lr

            MEM[$r2 + type_ofs_0] <- type $r0 ... $r7
            MEM[$r2 + slot_size*0] <- full $r0
            MEM[$r2 + slot_size*1] <- full $r3
            MEM[$r2 + slot_size*2] <- full $r4
            MEM[$r2 + slot_size*3] <- full $r5
            MEM[$r2 + slot_size*4] <- full $r6
            MEM[$r2 + slot_size*5] <- full $r7

            MEM[cur_context] <- $r0
            $lr <- $r0

            $r0 <- MEM[$lr + dirty_ofs]
            DIRTY <- $r0
            $r0 <- MEM[$lr + vstart_ofs]
            VSTART <- $r0
            $r0 <- MEM[$lr + vend_ofs]
            VEND <- $r0


            # We have a big problem here: we can't really restore the type!!!
            # At least not selectively: we need to have the types set before the loads
            # but what about skips? In those cases we would not want the types set.
            # A type-setting from vector to scalar is a destructive operation, we
            # loose the upper bits irrevocably. Our only choice it seems is to re-create
            # the full type mask and re-load the register values from sched_context.
            # That is just painful!!!!
            if $r1[0] == 1 $pc <- skip_r0
            full $r0 <- MEM[$lr + slit_size*0]
            $pc <- cont_r0
        skip_r0:
            $r0 <- $r0
        cont_r0:
            if $r1[1] == 1 $pc <- skip_r1
            full $r1 <- MEM[$lr + slit_size*1]
            $pc <- cont_r1
        skip_r1:
            $r1 <- $r1
        cont_r1:
            ...
        cont_r7:








            # We need a register. Use $lr as that's the most likely to be a scalar.
            # We need to save it (and it's type) to a static location before we can move on.
            MEM[lr_save] <- full $lr
            $lr <- type $lr
            MEM[lr_type_save] <- $lr
            # Load context pointer
            type $lr <- INT32
            $lr <- MEM[cur_context]
            # Save the context
            MEM[$lr + slot_size*0]  <- full $r0
            MEM[$lr + slot_size*1]  <- full $r1
            MEM[$lr + slot_size*2]  <- full $r2
            MEM[$lr + slot_size*3]  <- full $r3
            MEM[$lr + slot_size*4]  <- full $r4
            MEM[$lr + slot_size*5]  <- full $r5
            MEM[$lr + slot_size*6]  <- full $r6
            MEM[$lr + slot_size*7]  <- full $r7
            MEM[$lr + type_ofs_0] <- type $r0 ... $r7

            $r0 <- DIRTY # Changes type to INT32
            MEM[$lr + dirty_ofs] <- $r0

            $r0 <- $lr
            $lr <- MEM[lr_type_save]
            type $lr <- $lr
            full $lr <- MEM[lr_save]

            MEM[$r0 + slot_size*8]  <- full $r8
            MEM[$r0 + slot_size*9]  <- full $r9
            MEM[$r0 + slot_size*10] <- full $r10
            MEM[$r0 + slot_size*11] <- full $r11
            MEM[$r0 + slot_size*12] <- full $r12
            MEM[$r0 + slot_size*13] <- full $r13
            MEM[$r0 + slot_size*14] <- full $r14
            MEM[$r0 + type_ofs_1] <- type $r8 ... $r14

            $lr <- VSTART
            MEM[$r0 + vstart_ofs] <- $lr
            $lr <- VEND
            MEM[$r0 + vend_ofs] <- $lr

            # At this point we've saved off the current context. We can restore the context of the SCHEDULER
            type $r1 <- INT32
            $r1 <- sched_context
            type $r8 ... $r14 <- MEM[$r1 + type_ofs_1]
            full $r8 <- MEM[$r1 + slot_size*6]
            full $r9 <- MEM[$r1 + slot_size*7]
            full $r10 <- MEM[$r1 + slot_size*8]
            full $r11 <- MEM[$r1 + slot_size*9]
            full $r12 <- MEM[$r1 + slot_size*10]
            full $r13 <- MEM[$r1 + slot_size*11]
            full $r14 <- MEM[$r1 + slot_size*12]

            $r2 <- MEM[$r1 + vstart_ofs]
            VSTART <- $r2
            $r2 <- MEM[$r1 + vend_ofs]
            VEND <- $r2

            type $r0 ... $r7 <- MEM[$r1 + type_ofs_0] # We assume $r1-s restored type is also INT32
            full $r0 <- MEM[$r1 + slot_size*0]
            full $r3 <- MEM[$r1 + slot_size*1]
            full $r4 <- MEM[$r1 + slot_size*2]
            full $r5 <- MEM[$r1 + slot_size*3]
            full $r6 <- MEM[$r1 + slot_size*4]
            full $r7 <- MEM[$r1 + slot_size*5]

            $r2 <- MEM[$r1 + dirty_ofs]
            DIRTY <- $r2
            $pc <- $lr

OK, so this is hopelessly complex. Even with all the FSM and exception nightmare, it's better to have the load/store multiple instructions by a mile.

