System level considerations
===========================

On top of the core, the following system components are important for the implementation:

Operating System
----------------

The 'scheduler' context is primarily used for interrupt processing (that is to decide which task to schedule after a particular interrupt)

OS functions are implemented as tasks, though of course the scheduler context is part of it as well. That is to say, the only reason OS tasks have more privilege then user ones is because of the way the MMU is set up, since everything (including CSRs) are memory-mapped.

.. todo::
   The SCHEDULER context might actually be special in that it doesn't go through the MMU: it's logical and physical addresses are the same. This simplifies a few things, but makes booting more difficult.

Since interrupts are always disabled in SCHEDULE-mode, most of the OS must live in TASK-mode. The consequence of this design is that SYSCALLs need to transition into scheduler mode and out of it again (and the same on the way back). This essentially mean twice as many task switches as a reguler ring-based (monolithic kernel) OS design would need. To remedy this problem somewhat very quick SYSCALLs can be handled in the scheduler completely, if the fact that interrupts are disabled the whole time is an acceptable trade-off.

.. todo::
  I'm not sure how much of hit this actually is: One would think that the OS spends an inordinate amount of time validating inputs from SYSCALLs anyway, and the overhead of actually reaching the kernel is relatively minor. This needs some quantification though which I don't have the tools to do just yet.

To further minimize the overhead, task context switch should be very quick in order to make this architecture anywhere remotely performant:

#. Save registers into the task control block
#. Determine source of interrupt (read interrupt cause register)
#. Determine task to handle interrupt
#. Re-jiggle MMU config for new task
#. Load registers from new tasks control block
#. STM - this returns control to the new task
#. jump to step 1.

Most of the penalty comes from the load/restore of register content and the fact that we're changing the MMU config (which might
wreck havoc with the caches).

Interconnect
------------

We should have self-describing HW, I think. That would mean that the highest few bytes of anything in the address space should
say what that thing is.

Now, this is not possible for all things (memories for example), so the interconnect should step in for those items.

'Things' are identified by UUIDs, which are 128-bit long.

The interconnect also contains a descriptor of the following format:

32-bit region length (32-bit aligned)
32-bit region start (32-bit aligned)
Optional 128-bit UUID for region, if LSB of region start is set

The table is read backwards from the highest offset (which is the interconnect UUID) and read until region-length 0 is encountered. Regions must not be overlapping, but they are not necessarily listed in any particular order.

Region length 0 terminates the scan.

Each subsection either contains its own UUID or the UUID is in the interconnect descriptor one level above.

This setup allows SW to completely scan and understand the address map of the HW without any prior knowledge. (NOTE: since the tables and IDs are hard-coded, there's no HW complexity involved in coding them, except of course for the need of the actual storage)

.. note::
  Most peripherals simply need to have a 128-bit read-only register, containing their UUID decoded at their highest addressable I/O region. If peripherals also have memory mapped memories, those are described by the interconnect.

.. todo::
  This needs thought, way more though. The UUID approach gives you exact HW versioning, but not revisioning or any sort of capability listing. Thus, any minor HW change would require a complete SW recompile. There's no backwards compatibility what so ever. So, maybe a list of compatible UUIDs? But then how long is the list? What if there's partial compatibility with some other IP? (Such as two interconnects that have completely different control mechanisms (thus different UUIDs), but would still need to support the above discovery process? How about a backwards compatible, but increased functionality serial port of instance?

Booting
-------

If SCHEDULER-mode goes through the MMU, the following process works: on reset, we start in SCHEDULER mode, at (logical) address 0. This generates a TLB mis-compare upon address translation. The MMU page table address is also set to 0, so the first entry of the top-level page table is loaded from physical address 0. Based on that, the second-level (if that's how it is set up) page table entry is also loaded, from whatever address (say 4096). At this point the physical address for the first instruction can be determined (say 8192) and the fetch can progress.

If SCHEDULE-mode uses physical addresses, the MMU is not involved, so we can still simply start executing from address 0. Even though the MMU top level page table also points to address 0, that only starts playing a role when we enter TASK mode. So, boot code simply need to make sure to set up the MMU properly before exiting to the first task.

The end result is that we can boot the machine with all registers defaulting to 0.

I/O AND CSR
-----------

The process doesn't have a separate address space for I/Os and CSRs. This means that all such things need to be memory mapped. They probably would occupy high ranges of the physical address space, so that they don't interfere with booting. The difference between CSRs and I/O is that there is one copy of CSRs for each processor (in a multi-processor system) while there is only one copy of I/O. This is something that can be handled on the interconnect level (CSR peripherals are replicated and the CPUID is pre-pended to the physical address coming out of the CPUs).

CSRs occupy the top physical page, that is PA_FFFFE000...PA_FFFFFFFF

The following CSRs are defined:

Cache and TLB
~~~~~~~~~~~~~

TBASE - see above
SBASE - see above
TLB_LA1
TLB_DATA1
TLB_LA2
TLB_DATA2
TINV  - if written, invalidates TASK mode TLB entries
SINV  - if written, invalidates SCHEDULER mode TLB entries
CINV  - bit 0: invalidate INST CACHE, bit 1: invalidate DATA CACHE

Perf counters
~~~~~~~~~~~~~

PERF_CNT0
PERF_CNT1
PERF_CNT2
PERF_CNT3
PERF_CFG0
PERF_CFG1
PERF_CFG2
PERF_CFG3

Interrupt / reset cause
~~~~~~~~~~~~~~~~~~~~~~~

ECAUSE - exception cause
EADDR - exception address
RCAUSE - reset cause
RADDR

.. todo::
  We have the exception code (read/write/execute) as well. We can probably put that in ECAUSE.

.. todo::
  How to handle interrupts in a multi-core system? This could be just an interrupt routing problem...

.. todo::
  What of the above is truly replicated per core?
