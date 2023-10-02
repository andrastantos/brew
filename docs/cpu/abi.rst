ABI
===

Register Use Conventions
------------------------

The following register usage is defined:

========     =====================================
Register     Functionality
========     =====================================
$r0          call-clobbered general purpose register; used in thunks for virtual inheritance. Must be call-clobbered
$r1          call-clobbered general purpose register; struct value address (return value area pointer for large return values). EH_RETURN_STACKADJ_RTX BREW_STACKADJ_REG. Must be call-clobbered
$r2          call-clobbered general purpose register; static chain register
$r3          call-clobbered general purpose register
$r4          call-clobbered first argument/return value register.
$r5          call-clobbered second argument/return value register.
$r6          call-clobbered third argument/return value register;
$r7          call-clobbered fourth argument/return value register;

$r8          call-saved general purpose register; EH_RETURN_DATA_REGNO
$r9          call-saved general purpose register; EH_RETURN_DATA_REGNO
$r10         call-saved general purpose register
$r11         call-saved general purpose register
$r12         call-saved register a.k.a. $fp - frame pointer. NOTE: brew_dynamic_chain_address assumes it's in the 1st save slot in the stack.
$r13         call-saved register a.k.a. $sp - stack pointer.
$r14         call-saved register a.k.a. $lr - link register. NOTE: brew_return_addr_rtx assumes it's in the 2nd save slot in the stack.
========     =====================================

.. note::
  $lr is actually call-saved at the moment, but I don't think that's necessary.

.. note::
  if this changes, it needs to be followed-up in:
    - :code:`brew-newlib/newlib/libc/machine/brew/setjmp.S` (setjmp/longjmp implementation)
    - :code:`brew-newlib/newlib/libc/include/machine/setjmp.h` (size of jump buffer in _JBLEN)
    - :code:`brew-gcc/gcc/config/brew/brew.h` (actual register use definition for GCC)
    - :code:`brew-glibc/sysdeps/brew/bits/setjmp.h`
    - :code:`brew-binutils-gdb/opcodes/brew-decode.c`
    - :code:`brew-binutils-gdb/include/opcode/brew-abi.h`

EH_RETURN_STACKADJ_RTX:
  This is a register that is used to communicate to the epilog of functions that might have exception handlers in them that they need to adjust $sp by more than the usual amount. It is 0-ed out for normal control-flow, and filled in with a potentially non-0 value for the exception path.

EH_RETURN_DATA_REGNO:
  :code:`exc_ptr_reg` and :code:`filter_reg` for the region. These are used in the landing pad (the catch block) to finish the unwinding of the frame.

static chain register:
  funny name, but essentially contains the frame pointer for the outer function's frame inside nested functions. What's going on here is that the nested function, independent of it's own frame needs to have access to variables stored in the outer functions frame. This becomes especially tricky when the nested function is called through a function pointer from outside the outer function. This is what mostly trampolines are for.

.. todo:: we should have the same number of caller and callee-saved registers.


Calls
-----

Since the ISA doesn't include any call or return instructions, these need to be emulated in SW. The following instruction sequence implements a call::

  $lr <- $pc+<offset>
  $pc <- <callee>

Here, :code:`<offset>` could be 4, 6 or 8 depending on the encoding size of the call.

Upon entry callee saves :code:`$lr` if needed (along with all other callee-saved registers per the ABI).

To return, all saved registers (including :code:`$lr`) are restored, then::

  $pc <- $lr

Notice how this is only one instructions longer than the traditional link approach as the stack-adjustment and the save/restore of the link register would be there in most cases anyways. As a further savings (not that GCC does this at the moment) all conditional branches can be used to call functions as well.

.. _function_arguments:

Function arguments and return values
------------------------------------

Arguments (and return values) are passed, first in registers, then on the stack. Stack area is reserved for all arguments independent of their storage location. Arguments can span multiple registers, but always start at a register boundary. On the stack, they are also aligned (and in cases where it makes sense promoted) to 32-bit boundaries. It's also possible for an argument to be partially in registers and partially on the stack.

.. note:: The only reason an argument can partially be in registers is if there isn't enough registers left to keep it fully in registers. Similarly, an argument is placed on the stack only if we completely ran out of argument-passing registers. Consequently, if an argument is partially or fully on the stack, all subsequent arguments are fully on the stack.

.. note:: Some arguments are passed by reference no matter what. These should be 'large' or unknown sized arguments. In those cases, a pointer to them is put on the stack or in registers, whichever makes sense by the previous rules.

The registers used for argument passing in increasing register index are: :code:`$r4`, :code:`$r5`, :code:`$r6` and :code:`$r7`.

Return values are handled the same way.

Exception handling returns are in :code:`$r4`...:code:`$r7` (described in :code:`EH_RETURN_DATA_REGNO`)

Syscalls
--------

Syscalls follow the same calling convention as function calls do, except that :code:`$lr` contains a syscall-dependent information (usually :code:`errno`). The :code:`syscall` instruction is used to transfer control to the executive. The syscall number is stored as a 16-bit code after the :code:`syscall` instruction, in the instruction-stream. Upon entering SCHEDULER mode, :code:`$tpc` points to the current instruction, which is to say, it points to :code:`syscall`. The SCHEDULER needs to increment :code:`$tpc` by 4 before returning execution to task mode.

.. note:: syscall number is 16-bit so there won't be any alignment problems reading it.

.. note:: most sysclass take in the current value of :code:`errno` from the calling TASK-mode application. Syscalls can decide to return a modified :code:`errno` value if they choose to. The caller cannot depend on :code:`$lr` preserved through a syscall. The caller however *can* assume that :code:`$r0` through :code:`$r3` - which are normally call-clobbered - are preserved through a syscall.

Stack layout
------------

Stack management is also the responsibility of SW: there are no instructions that define or constrain the way the stack is organized. However, there are special load/store instructions with very compact encodings that use :code:`$r12` (a.k.a. :code:`$sp`) and :code:`$r13` (a.k.a. :code:`$fp`), so any reasonable ABI would use these two registers for stack management.

The stack is growing down, and the stack pointer is pre-decrement for push, and post-increment for pop. This means that :code:`$sp` points to the last valid value.

.. important::
  since the stack is SW managed, pushes and pops are not atomic. Exception handlers can't assume that :code:`$sp` actually points to the top of the stack.

Stack frame layout upon enter to function::

  <arg 0>
  <arg 1>
  ...
  <arg n> <-- $sp

Upon enter, :code:`$fp` is saved on the stack, then :code:`$fp <- $sp` is executed. Finally locals are allocated. After all of that, the stack looks as follows::

  <arg 0>
  <arg 1>
  ...
  <arg n> <-- $fp
  <old $fp>
  <local 0>
  <local 1>
  ...
  <local n> <-- $sp

.. note::
  As discussed in the :ref:`Function Arguments <function_arguments>` chapter, the first few arguments are passed in registers, even though stack-space is allocated for them.

Trampoline
----------

Trampolines are used to call nested functions through a function pointer. Most of the machinery for that is handled by GCC, but there's a little platform-specific piece of code that's needed:

The static chain is maintained in :code:`$r8` (:code:`STATIC_CHAIN_REGNUM`).

The trampoline is a little piece of code that's copied into a memory buffer then modified. The template for it is the following::

    $r8 <- mem32[.Lstatic_chain]
    $pc <- mem32[.Lfunc_address]
  .Lstatic_chain:
    .long 0
  .Lfunc_address:
    .long 0

This template is first copied to a memory buffer, :code:`.Lstatic_chain` and :code:`.Lfunc_address` are then filled in with the correct value and finally a jump is generated to the beginning of the buffer.

.. todo::
  Trampoline should deal with cache-invalidation, but I don't think it does at the moment.

User code memory layout in MMU-based systems
--------------------------------------------

- Page 0 is reserved (to catch NULL-ptr dereference)
- Entry point is at 0x1000
- Stack starts at 0x40000000
- Heap starts at the end of the :code:`static data` segment

.. note::
  This is set in :code:`interp.c` in :code:`sim_open` as the highest addressable memory address. This is also defined in the linker scripts through the :code:`.stack` section, which
  is ultimately set in :code:`ld/emulparams/elf32brew.sh`.

C++ Exceptions
--------------

The documentation is wrong in several key points.

First: we *have to make sure* that if a function calls :code:`__builtin_eh_return`, it saves and restores on the stack both the return address (the link register for us) and the registers in :code:`EH_RETURN_DATA_REGNO`: the Dwarf-based exception handling works by patching in the values for these registers in the stack frame, then going through the epilog.

Second: the documentation says that architectures have to provide at least two :code:`EH_RETURN_DATA_REGNO` registers, but can provide more. While that's technically correct, there's no reason to provide more than two. Especially combined with the first point, it just makes for more pushes and pops from the stack.

Third: the documentation says that it's best if the :code:`EH_RETURN_DATA_REGNO` registers are call-clobbered. That, I don't think is correct, in fact, the reverse is better: it's best if these registers are caller-saved. The reason for that is the first point above: since they need to be saved and restored in functions calling :code:`__builtin_eh_return`, we have the best chances of having them already in the frame, if they are caller-saved. In fact, it's probably best if we reserved the first two caller-saved registers for this purpose.

Fourth: many architectures use (a subset of) the argument/return value registers for :code:`EH_RETURN_DATA_REGNO`. This is WRONG in most cases, though probably doesn't actually cause problems: the generated stack-frame will potentially destroy the original return value on non-exception paths. The reason it is mostly a benign problem is that functions that call :code:`__builtin_eh_return` (at least in the runtime) tend to return a single integer. So, as long as it's not the first return value register that we use as :code:`EH_RETURN_DATA_REGNO`, we're good.

In epilog generation we need to know if we have to save the aforementioned registers. We can consult :code:`crtl->calls_eh_return` to determine that.

With that, right now I'm using :code:`$r8` and :code:`$r9` for :code:`EH_RETURN_DATA_REGNO`

.. todo::

  At some point I should probably change :code:`EH_RETURN_DATA_REGNO` to use lower-ordered registers, or change the allocation priority to go from top down. This doesn't seem to be documented, but there's a simple way of ensuring this::

    /* We need this on CRIS, because call-used regs should be used first,
    (so we don't need to push).  Else start using registers from r0 and up.
        This preference is mainly because if we put call-used-regs from r0
    and up, then we can't use movem to push the rest, (which have to be
    saved if we use them, and movem has to start with r0).
    Change here if you change which registers to use as call registers.

    The actual need to explicitly prefer call-used registers improved the
    situation a lot for 2.1, but might not actually be needed anymore.
    Still, this order reflects what GCC should find out by itself, so it
    probably does not hurt.

    Order of preference: Call-used-regs first, then r0 and up, last fp &
    sp & pc as fillers.
    Call-used regs in opposite order, so they will cause less conflict if
    a function has few args (<= 3) and it wants a scratch reg.
        Use struct-return address first, since very few functions use
    structure return values so it is likely to be available.  */
    #define REG_ALLOC_ORDER \
        {9, 13, 12, 11, 10, 0, 1, 2, 3, 4, 5, 6, 7, 8, 14, 15, 17, 16, 18, 19, 20}

