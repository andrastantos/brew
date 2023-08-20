.. _family_compatibility:

Family compatibility
====================

The Brew specification doesn't strive for complete cross-family binary compatibility. It leaves many details to the implementation, such as MMU, CSRs and I/O. As a consequence, processors implementing the Brew architecture in general are not going to be compatible with one another on the system software level. The goal is that - combined with the right system software - these processors could still execute the same, unmodified user applications.

The main differentiating feature between various implementations is the subset of supported types. The type-support limitations have some consequences on the instruction set support as well of course.

When only a subset of the types are supported, any attempt to change the type of a register to an unsupported one should raise a :code:`exc_type` exception.

Subtle ISA-level variability is also possible of course, due to micro-architectural differences. For instance, an implementation with no caches has no reason to have cache invalidation instructions. A core developed for single-processor environment has no use for load-lock/store-conditional semantics, and simple in-order implementation has no reason to have any of the :code:`FENCE` variants. The overriding principle should be that if an instruction is left unimplemented, it should do the expected default behavior. For instance :code:`FENCE` instructions should simply be no-ops, while store-conditional operations should always return success. Alternatively, if no such sensible default behavior is possible - or feasible - the operation should raise an :code:`exc_unknown_inst` exception.

All of these deviations from the Brew specification should be clearly documented by the implementations datasheet.

To cut down on the number of variations a set of standard profiles are defined. It must be noted that many profiles can be combined in an implementation though some profiles are supersets of another.

.. _mandatory_set:

Mandatory set
-------------

The mandatory set of instructions and functionality include the following simplifications:

#. No type support. All registers are :code:`INT32` in type. Any attempt to change the type results in an :code:`exc_type` exception. Type-testing branches are not implemented and result in no-ops, that is to say they never branch.
#. load/store multiple (and push/pop) are optional, they raise an :code:`exc_unknown_inst` exception if not implemented.
#. :code:`VLEN` is set to 0, :code:`VSTART` and :code:`VEND` are not implemented. :code:`set_vlen` raises an :code:`exc_unknown_inst`

.. _floating_point_set:

Floating point addition
-----------------------

The addition of :code:`FP32` as a supported type gets us a floating-point implementation. Type changes (including the type override prefix) are now mandatory. Of course all operations need to support both integer and floating-point types.

.. note:: Support for :code:`FP64` is recommended, but not mandatory in this profile.

.. _integer_vector_set:

Integer vector support
----------------------

Integer vector implementation adds support for :code:`VINT32` and optionally to :code:`VINT16` and :code:`VINT8` types.

This can be combined with any vector length, but if :code:`VLEN` is set to 4, the implementation is substantially simplified.

Load/store multiple (and push/pop) operations are now mandatory as we enter the world of variable length registers.

.. _floating_point_vector_set:

Floating point vector support
-----------------------------

A further step up on the complexity ladder is to add support for the :code:`VFP32` and the optional :code:`VFP64` types. If an implementation supports floating point vectors, it must support :ref:'integer vectors<integer_vector_set> as well.

Detailed support by profile
---------------------------

.. include:: isa_classes.rst
