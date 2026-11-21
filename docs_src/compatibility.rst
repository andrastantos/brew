.. _family_compatibility:

Family compatibility
====================

The Brew specification doesn't strive for complete cross-family binary compatibility. It leaves many details to the implementation, such as MMU, CSRs and I/O. As a consequence, processors implementing the Brew architecture in general are not going to be compatible with one another on the system software level. The goal is that - combined with the right system software - these processors could still execute the same, unmodified user applications.

The main differentiating feature between various implementations is the subset of supported types. The type-support limitations have some consequences on the instruction set support as well of course.

When only a subset of the types are supported, any attempt to change the type of a register to an unsupported one should raise a :code:`exc_type` exception.

Subtle ISA-level variability is also possible of course, due to micro-architectural differences. For instance, an implementation with no caches has no reason to have cache invalidation instructions. A core developed for single-processor environment needs only a minimal form of load-lock/store-conditional semantics, and a simple in-order implementation has no reason to have any of the :code:`FENCE` variants. The overriding principle should be that if an instruction is left unimplemented, it should do the expected default behavior. For instance :code:`FENCE` instructions should simply be no-ops. Store-conditional operations can't simply always succeed, even on a single processor: a task can be interrupted between its load-lock and its store-conditional, and another task can update the location in the meantime. The minimum is a 'lock valid' bit and the locked address in the core: :code:`MEMLL` sets the bit and records its address; :code:`MEMSC`, a change of execution mode and reset clear the bit; :code:`MEMSC` stores and reports success only if the bit is set and its address matches the recorded one (see :ref:`load-lock lifetime<load_lock>`). Alternatively, if no such sensible default behavior is possible - or feasible - the operation should raise an :code:`exc_unknown_inst` exception.

All of these deviations from the Brew specification should be clearly documented by the implementations datasheet.
