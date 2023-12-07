Memory Model
============

The Brew specification sets rather loose standards on the memory model implemented by a particular CPU.

From a single processors (or thread within a multi-threaded implementation) point of view, the following requirement must be met:

**Load from an address must return the last value written to the same address by a store in program execution order.**

This requirement doesn't mean that stores must be committed to memory before the load from the same address can proceed. Only that the returned value behaves *as-if* the value was committed to memory. This requirement also doesn't mean that stores (even to the same address) can't be re-ordered or quashed. Only that, if a read is performed to the same address it must *look* as if the last write to that address succeeded.

Other then that, there is no guarantee to the order of loads or stores (including load-lock and store-conditional instructions) among themselves or across groups.

A particular implementation of course can set more strict requirements on the ordering of loads and stores.

Non-cacheable I/O read operations are guaranteed to fetch their data from the I/O device, if there are no pending writes to the same address.

Barrier instructions
--------------------

Barrier instructions (FENCE_*) can be used to enforce ordering of loads and stores. These instructions can force the CPU pipeline (not the cache subsystem) to ensure loads and/or stores have completed before execution resumes. The same instructions can also ensure subsequent loads and/or stores have not started execution before execution resumes.

Barrier instructions of the 'write before' kind guarantee that the side-effects of writes reach their target, or at least a target that provides the appropriate completion response. What this actually means exactly is left to the implementation to define. If the target address is cacheable and is resident, it is possible that the write only reaches the L1 data cache for instance. For non-cacheable I/O references, Barrier instructions of the 'write before' kind guarantee that the write reached the I/O device.

Notes of I/O
------------

Brew specifies that all I/O registers (including CSRs) are memory mapped. For these devices the order of loads and stores, even the fact that loads and stores *actually occur* is important. Software is responsible to make sure these requirements are met by the use of barrier instructions. The safest way is to issue a :code:`FENCE_RW_RW` instruction after every access to an I/O device, but that might be too conservative and slow.

I/O address ranges should also be marked as non-cacheable if caches are implemented in the system. Brew guarantees that such loads and stores - combined with the appropriate barrier instructions - will ensure that the I/O device accesses happen as expected. On the HW implementation side, this guarantee might come from not only the CPU core but the system implementation. For instance, no entity in the interconnect should provide early completion responses to write to I/O devices.

Multi-processor implications
----------------------------

In a multi-processor environment synchronization among multiple, parallel execution streams must be provided. While barrier instructions are a good means for ensuring that the view of the memory in one execution context matches that of another one, more is needed. Brew uses the `load-lock/store-conditional <https://en.wikipedia.org/wiki/Load-link/store-conditional>`__ paradigm.

In practice however, the actual implementation of the locking and releasing/checking for the locks is often largely delegated to the system level and is not part of the CPU, let alone the Brew specification. Brew only provides instructions to drive the right control signals along with loads and stores that are needed by a system to implement such primitives.

Implementation Notes
~~~~~~~~~~~~~~~~~~~~

A compliant Brew implementation can take advantage of AXI4 `exclusive transactions <https://developer.arm.com/documentation/102202/0200/Atomic-accesses>` to implement the load-lock/store-conditional instructions. If such an implementation is used to implement store-conditional operations, these instructions will have to await completion until the write-response arrives from the interconnect: the store-conditional instruction returns a success-code.

AXI4 largely pushes the burden of actually making exclusive transactions work to the memory controller(s). If these controllers don't support such concepts, the following can be done:

#. Let's have a BRAM in 1-bit configuration. The BRAM address is an X-bit hash of the transaction address.
#. The data is a single 'valid' bit.


Operation:

#. On exclusive load, the valid bit is set for the corresponding address.
#. On exclusive store, the valid bit is checked and the store is cancelled if the bit is clear. The valid bit is cleared either way. The appropriate :code:`OKAY` or :code:`EXOKAY` response is given as required by the AXI4 specification.

If there are multiple ports to a memory, each port will have to have a variant of the above mechanism for *each port*. That is to say, the number of BRAMs needed is the number of ports squared. Let's call these memories BRAM_x_y, where x and y can be any number from 0 to the number of ports minus one. In that case:

#. Each exclusive load on port A sets all the valid bits in BRAM_A_i for all valid i-s.
#. Each exclusive store on poart A does two things:
   #. Checks if all the bits are set in BRAM_A_i for all valid i-s. If any is cleared, the write fails.
   #. Simultaneously, clears the valid bit in BRAM_i_A and BRAM_A_i for all valid i-s.

If multiple writes to the same (hashed) address happen in the same cycle, they cannot all succeed. This can be ensured by either failing all such writes, or implementing a priority scheme. Either way, the content of the memory must accurately reflect the decision made.

If some writes and a reads to the same (hashed) address happen in the same cycle, the conflict must be resolved in a coherent manner: if reads return old data, the valid bits must reflect the state as if the reads happen before writes; If the reads return new data, the valid bits must reflect the state as if writes happen before the reads. Either way, conflicting between writes must be resolved according to the previous paragraph.

Implementation of atomics
-------------------------

As an example, an atomic increment can be implemented in the following way:

retry:
    $rD <- MEMLL[$rA + <ofs>]
    $rD <- $rD + 1
    $rD <- MEMSC[$rA + <ofs>] <- $rD
    if $rD != 0 $pc <- retry

.. note:: Fence is not necessary due to the carried dependency between the load and the store.

More complex primitives, can also be built in a similar manner.

Self-modifying code
~~~~~~~~~~~~~~~~~~~

Brew doesn't guarantee any coherency between instruction and data-caches or require that the processor monitors stores to addresses that are already fetched and are in progress in the instruction pipeline.

Cache line invalidation instructions are provided to make sure that instruction caches are properly invalidated and data caches are flushed.

This still isn't enough for self-modifying code though: the prefetch queue of the processor can contain already fetched instructions, which are not impacted by cache invalidation. A special instruction (:code:`PFLUSH`) is provided to flush the pipeline and guarantee that subsequent instructions are fetched anew from at least L1 instruction cache.

.. admonition:: Why?

    One could assume that branch instructions with cache invalidation would be sufficient to ensure proper execution of self-modifying code. However, with deep pre-fetch queues and advanced branch-predictors this might not be the case. The branch predictor can properly predict the target address and direct the pre-fetcher to starts fetching instruction words before the execution of the cache invalidation instructions. It would be cumbersome to reconcile pre-fetcher state with cache invalidation instructions. The :code:`PFLUSH` instruction makes the SW intent unambiguous.

Caches
------

Coherency
~~~~~~~~~

Brew doesn't contain any coherency guarantees. This is the domain of the implementation. In fact, Brew specifically doesn't guarantee coherency between the instruction and data-caches of even a single CPU core.

Cache invalidation
~~~~~~~~~~~~~~~~~~

There are instructions to invalidate individual data-, instruction- cache lines both in L1 and L2. There is no way to distinguish which cache is to be invalidated: all caches in line from the CPU executing the line invalidation instruction to memory are searched for a hit and invalidated as needed. If the line being invalidated is dirty, it is flushed.

There is no instruction provided for complete cache invalidation: this functionality, if needed, is to be provided through CSRs.

Alignment
---------

Brew doesn't support unaligned loads and stores. 16-bit loads and stores must occur on 16-bit word boundaries, 32-bit ones on 32-bit boundaries. Instructions must be aligned to 16-bit word boundaries.

When a load or store is attempted to an aligned address, an exception is thrown. If a branch is attempted to an unaligned address, the LSB of the target address carries implementation-defined meaning. Certain implementations are allowed to ignore the LSB of the branch-target.
