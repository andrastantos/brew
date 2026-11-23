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
$r2          call-clobbered general purpose register; static chain register; EH_RETURN_DATA_REGNO
$r3          call-clobbered general purpose register; EH_RETURN_DATA_REGNO
$r4          call-clobbered first argument/return value register.
$r5          call-clobbered second argument/return value register.
$r6          call-clobbered third argument/return value register;
$r7          call-clobbered fourth argument/return value register;

$r8          call-saved general purpose register
$r9          call-saved general purpose register
$r10         call-saved general purpose register
$r11         call-saved general purpose register
$r12         call-saved register a.k.a. $fp - frame pointer in functions that need one, otherwise a general purpose register (GCC then prints it as $r12). NOTE: brew_dynamic_chain_address assumes it's in the 1st save slot in the stack.
$r13         call-saved register a.k.a. $sp - stack pointer.
$r14         call-saved register a.k.a. $lr - link register. NOTE: in functions with a frame pointer, brew_return_addr_rtx and brew_eh_return_handler_rtx assume it's in the 2nd save slot in the stack.
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

Return values are handled the same way: if they fit in these 4 registers, they are returned in registers, otherwise a memory location is used.

Exception handling data (the exception pointer and the handler selector) is passed to landing pads in :code:`$r2` and :code:`$r3` (described in :code:`EH_RETURN_DATA_REGNO`)

Syscalls
--------

Syscalls follow the same calling convention as function calls do, except that :code:`$lr` contains a syscall-dependent information (usually :code:`errno`). The :code:`syscall` instruction is used to transfer control to the executive. The syscall number is stored as a 16-bit code after the :code:`syscall` instruction, in the instruction-stream. Upon entering SCHEDULER mode, :code:`$tpc` points to the current instruction, which is to say, it points to :code:`syscall`. The SCHEDULER needs to increment :code:`$tpc` by 4 before returning execution to task mode.

.. note:: syscall number is 16-bit so there won't be any alignment problems reading it.

.. note:: most sysclass take in the current value of :code:`errno` from the calling TASK-mode application. Syscalls can decide to return a modified :code:`errno` value if they choose to. The caller cannot depend on :code:`$lr` preserved through a syscall. The caller however *can* assume that :code:`$r0` through :code:`$r3` - which are normally call-clobbered - are preserved through a syscall.

Stack layout
------------

Stack management is also the responsibility of SW: there are no instructions that define or constrain the way the stack is organized. However, there are special load/store instructions with very compact encodings that use :code:`$r12` (a.k.a. :code:`$fp`) and :code:`$r13` (a.k.a. :code:`$sp`), so any reasonable ABI would use these two registers for stack management.

The stack is growing down, and the stack pointer is pre-decrement for push, and post-increment for pop. This means that :code:`$sp` points to the last valid value.

.. important::
  since the stack is SW managed, pushes and pops are not atomic. Exception handlers can't assume that :code:`$sp` actually points to the top of the stack.

Stack frame layout upon enter to function::

  <arg 0>
  <arg 1>
  ...
  <arg n> <-- $sp

The prologue stores the registers the function saves below :code:`$sp` (the incoming :code:`$sp` is the CFA), :code:`$fp` first and :code:`$lr` next when they are saved, then the other call-saved registers. In a function with a frame pointer it then executes :code:`$fp <- $sp`. Finally it moves :code:`$sp` down past the saved registers, the locals and the outgoing argument area in one step. After all of that, the stack looks as follows::

  <arg 0>
  <arg 1>
  ...
  <arg n> <-- $fp (in functions with a frame pointer)
  <old $fp> (in functions with a frame pointer)
  <$lr> (if saved)
  <other saved registers>
  <local 0>
  <local 1>
  ...
  <local n>
  <outgoing arguments> <-- $sp

The epilogue moves :code:`$sp` back up first and then reloads the saved registers from below it.

GCC sets up a frame pointer only when it needs one: at :code:`-O0` (or with :code:`-fno-omit-frame-pointer`), and in functions that call :code:`alloca` (or use variable-length arrays), call :code:`__builtin_eh_return`, or walk the frame chain (:code:`__builtin_frame_address`, :code:`__builtin_return_address` with a non-zero count). Elsewhere locals are addressed off :code:`$sp`, :code:`$r12` is one more call-saved register (saved, in the first slot, only if the function uses it), and :code:`$lr` otherwise takes the first save slot. Code that unwinds the stack (the C++ unwinder, debuggers) must follow the CFI rather than a chain of saved :code:`$fp` values.

.. note::
  As discussed in the :ref:`Function Arguments <function_arguments>` chapter, the first few arguments are passed in registers, even though stack-space is allocated for them.

Trampoline
----------

Trampolines are used to call nested functions through a function pointer. Most of the machinery for that is handled by GCC, but there's a little platform-specific piece of code that's needed:

The static chain is maintained in :code:`$r2` (:code:`STATIC_CHAIN_REGNUM`).

The trampoline is a little piece of code that's copied into a memory buffer then modified. The template for it is the following::

    $r2 <- mem32[.Lstatic_chain]
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

Third: the :code:`EH_RETURN_DATA_REGNO` registers must be call-clobbered, as the documentation says. The unwinder enters a landing pad with these registers overwritten, so the function containing the landing pad clobbers them. If they were call-saved, every function with a landing pad (any :code:`catch` or cleanup) would have to save and restore them for its caller. (Brew used :code:`$r8` and :code:`$r9` until GCC 15 exposed that this wasn't done: the caller's :code:`$r9` was silently replaced by the handler selector.) Landing pads are only entered from calls, which clobber call-clobbered registers anyway, so those cost nothing.

Fourth: many architectures use (a subset of) the argument/return value registers for :code:`EH_RETURN_DATA_REGNO`. That needs care: because of the first point, functions calling :code:`__builtin_eh_return` restore these registers in their epilog on the normal return path too, which would overwrite the return value (e.g. the error code :code:`_Unwind_RaiseException` returns). :code:`$r1` is not available either: it is :code:`EH_RETURN_STACKADJ_RTX`, which is live at the same time.

In epilog generation we need to know if we have to save the aforementioned registers. We can consult :code:`crtl->calls_eh_return` to determine that.

With that, Brew uses :code:`$r2` and :code:`$r3` for :code:`EH_RETURN_DATA_REGNO`. :code:`$r2` is also the static chain register, but the two uses don't meet: the static chain is only live on entry to a nested function, while the EH data is only live at the start of a landing pad (reached from a call) and in the unwinder's own :code:`__builtin_eh_return` functions, which aren't nested.

