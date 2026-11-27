Appendix E: Todo
================

Toolchain
---------

.. todo:: We'll need to make sure we use sign-extend loads appropriately: apparently Moxie didn't have sign-extend loads

.. todo:: brew.opt needs to be updated to actually generate mfloat and mno-float options.

.. todo:: add floating point instructions (probably for something like fastmath only)

.. todo:: figure out if we really need 'upper' versions of multiplies and how to efficiently use them. Without them (the ``mulsi3_highpart``/``umulsi3_highpart`` patterns in ``brew.md`` and the decoder's "full" multiplies, ``$rD <- full $rA * $rB >>> N``, are commented out), GCC builds 64-bit products from several 32-bit multiplies and additions (or calls ``__muldi3``), and a division by a constant can't use the usual multiply-by-reciprocal sequence: ``x / 10`` calls ``__udivsi3``, a full division loop. 64-bit multiplication itself works (``gcc.c-torture/execute/arith-rand-ll.c``).

.. todo::
   we probably want to control register allocation order. Some comment from code::

      /* The order in which registers should be allocated.
         It is better to use the registers the caller need not save.
         Allocate r0 through r3 in reverse order since r3 is least likely
         to contain a function parameter; in addition results are returned
         in r0.  It is quite good to use lr since other calls may clobber
         it anyway.  */
      #define REG_ALLOC_ORDER						\

.. todo::
   we should use this::

      CALL_REALLY_USED_REGISTERS instead of CALL_USED_REGISTERS -> that's legacy.

.. todo::
   there's a GCC macro: :code:`__REGISTER_PREFIX__`. If there's a GAS equivalent, maybe we can coerce GAS expression parser to stop at register names? I actually think this is outdated. I have a completely re-written parser at this point which doesn't depend on the demented GAS expression parser. It identifies expression boundaries on its own and calls the GAS parser for only the appropriate segments.


Multiple Load-Stores
--------------------

This is a new idea that I'm toying with: would it be too difficult to add a pair of (multi-cycle) instructions that could load and store any combination of registers? This goes against the RISC approach, it's clearly a complex concept. However, ARM has it and for good reason: it collapses function prologs and epilogs, results in very compact code and - if implemented properly - results in pretty nice memory access patterns with high efficiencies. I have found a right-sized hole in the instruction space to fit these instructions in, but the implementation complexity is rather high. I'm not yet sure if it's worth to have them, I probably need to model it first.


Open items from the toolchain work (as of 2026-11-27)
-----------------------------------------------------

Collected from the toolchain bring-up (GCC 16 and trunk, binutils/gdb and
newlib merges, Espresso tuning). The workspace ``CLAUDE.md`` has the
measurements and commit references behind each item.

Espresso performance
^^^^^^^^^^^^^^^^^^^^

.. todo::
   Fetch page size. The fetch unit ends bursts at 256-byte boundaries
   (``page_bits = 7``, counted in 16-bit words), but the external bus allows
   bursts within a 512-byte DRAM page. On the cycle model, 512 bytes halves the
   average cost of page ends (+0.30% to +0.16% against no page effects over
   the code-generation tests); 1024 bytes adds little. Confirm that every
   supported DRAM configuration has at least 512-byte pages, then change
   ``page_bits`` (RTL, cycle model, regenerated Verilog, rig suite).

.. todo::
   Placement noise in measurements. Moving the same code by a few bytes changes
   single tests' cycles by 2-5% (up to 14%) because of fetch page ends, and
   geometric means by ~0.3%. ``run-opt-tests.sh`` should be able to average
   each test over several padding offsets (``scratch-page/page.py`` does it),
   so that tuning decisions don't pick up placement luck.

.. todo::
   Remaining Espresso cycle budget at -O2: ~43% refills after taken branches,
   ~16% fetch starvation, ~10% register waits. Ideas not yet evaluated:
   ending bursts early at fetched branches (the RTL's
   ``break_on_branch_early`` option, currently off), cheaper restarts after a
   taken branch, and the cost of the 3-cycle result latency.

.. todo::
   Branchless selects don't pay on Espresso without a conditional move or
   set-on-compare (a branch over a move costs 3-5 cycles, a mask select 4-9
   cycles and more on a full compare). Revisit if the ISA gains a select
   instruction, or once the scheduler can overlap independent select chains.

.. todo::
   Loop unrolling and peeling gain 3-5% cycles at 6-30% more code and are left
   to users (``-funroll-loops``, ``-fpeel-loops``). Decide whether a capped
   default makes sense for -O2.

.. todo::
   The cycle model does not cover DMA, interrupts, exceptions beyond a taken
   branch, CSR wait states, or the timing of cores without a multiplier or
   shifter.

Cores without a multiplier or barrel shifter
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. todo::
   Performance of the reduced variant is unmeasured. Shifts call
   ``__brew_ashl``/``__brew_ashr``/``__brew_lshr`` (one loop iteration per
   bit), multiplications ``__mulsi3``. Measure on the cycle model, then
   consider faster routines (fewer iterations for common counts), inline
   loops, and expanding constant right shifts better.

.. todo::
   Only one reduced library variant exists (``min/``: neither unit). A core
   with only one of the two units links with it too; separate ``nomul`` and
   ``noshift`` multilibs would let it use the unit it has.

.. todo::
   13 libstdc++ tests time out on the reduced variant (special functions,
   ``to_chars``, random distributions). Either raise the time limit for that
   board or accept them as known.

GCC
^^^

.. todo::
   Send upstream: the ``assign_parm_adjust_stack_rtl`` fix for addressable
   over-aligned parameters (``gcc.dg/pr97473.c``; on trunk only, and
   backported to ``releases/gcc-16``).

.. todo::
   Report upstream: trunk's match.pd fold (``27b2a84e580``, ``X +- C1 ==/!= C2``
   to ``X ==/!= 0`` with other uses of ``X +- C1``) moves an -Os loop's exit
   test before the decrement, two taken branches per iteration
   (``sort_insertion`` -Os).

.. todo::
   ABI decision: ``PREFERRED_STACK_BOUNDARY`` is 32 bits. 64 or 128 would make
   ``alloca`` results 8-aligned (``gdb.base/eh_return.exp``) and over-aligned
   locals cheaper, at the cost of stack space; it changes the ABI.

.. todo::
   ``_stack`` is the top of simulator memory, so nothing lies above ``main``'s
   frame except crt0's argv block. ``gcc.dg/ssp-1.c`` and
   ``stackprotectexplicit1.c`` overrun into it and fault before the canary
   check (xfailed). Decide whether to move ``_stack`` down (e.g. 4 KB), and
   update those tests' xfail comments either way.

.. todo::
   Size defaults: the jump threader of GCC 14+ makes libraries larger
   (``_vfprintf_r`` 6.5 KB to 8 KB) for a slight speed gain. Decide whether
   Brew wants ``--param=max-jump-thread-paths`` or ``-Os``-built libraries.

.. todo::
   Trunk ran 0.8% more dhrystone -O2 instructions than ``basepoints/gcc-17``
   (not bisected). Probably moot now that tuning is judged by Espresso cycles;
   check once with cycles.

.. todo::
   Optimizer dump scans failing since GCC 15, probably cost-model effects:
   ``gcc.dg/loop-9.c``, ``crc-from-fedora-packages-2/24.c``,
   ``field-merge-17.c``.

.. todo::
   64-bit atomics call ``__atomic_*_8``, which nothing provides (libatomic is
   disabled, and there is no 64-bit ``memll``/``memsc``).

.. todo::
   ``std::stacktrace`` returns no frames (libstdc++ ``19_diagnostics/stacktrace``),
   probably because libbacktrace is asked for a threaded state while built
   without threads. Check upstream before changing anything.

.. todo::
   ``std::timespec_get`` is missing: libstdc++'s newlib configuration never
   checks for ``timespec_get``, which libgloss provides now.

.. todo::
   Regular maintenance merges: upstream trunk into brew-gcc ``master`` about
   monthly (last 2026-11-18), each new 16.x tag into ``releases/gcc-16``.

Run-time environments and newlib
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. todo::
   Operating environment (bare metal): no DejaGnu board tests
   ``-menvironment=operating`` yet; there is no linker script for Espresso's
   ROM/RAM map; ``.data`` isn't copied from ROM for ROM-resident programs; the
   console isn't initialized (baud rate) for real hardware.

.. todo::
   ``anachron/sw_new``'s ROM links but doesn't run: on the ISS the espresso
   model lacks the bus-interface CSR (0x200) and the 0xffff_fc00 DRAM alias; on
   the RTL test bench it ends after ~160 us with exit code ``x``.

.. todo::
   llloops never finishes on the simulator (its timer calibration is too
   slow); it needs a smaller calibration target or a ``-DSIM`` path.

Simulator
^^^^^^^^^

.. todo::
   Operating environment: an optional ``--scheduler-exceptions=reset``
   (restart at 0 instead of stopping); the devices' interrupts aren't wired;
   a console DATA read with no input fails instead of waiting, as the RTL does.

.. todo::
   The ``field_e``-decoded extension groups in ``brew-decode.c`` are disabled;
   enabling them needs ``field_e`` in the instruction profile's key.

Documentation
^^^^^^^^^^^^^

.. todo::
   ``docs/html`` was last generated in December 2023 (Docutils 0.17); a
   rebuild with today's Sphinx rewrites 34 files, so the HTML lags the
   sources.
