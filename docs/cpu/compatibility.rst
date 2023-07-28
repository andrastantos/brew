Family compatibility
====================

The Brew specification doesn't strive for complete cross-family binary compatibility. It leaves many details to the implementation, such as MMU, CSRs and I/O. As a consequence, processors implementing the Brew architecture in general are not going to be compatible to one another on the system software level. The goal is that - combined with the right system software - these processors could still execute the same, unmodified user applications.

Base implementation
-------------------

The most basic implementation of the Brew architecture would involve the following simplifications:

#. No type support. All registers are :code:`INT32` in type. Any attempt to change the type results in either an :code:`exc_type` or an :code:`exc_unknown_inst' exception.
#. No caches, so no cache invalidation: these instructions are no-ops
#. No fence instructions: these also result in no-ops
#. No load-lock/store-conditional semantics: loads never set locks, store-conditionals always report success
#. No extension groups: since all of these instructions deal with vector operations, they are not implemented and raise an :code:`exc_unknown_inst` exception instead.
#. No prefix instructions: since there are no types to be talked about, the only prefix instruction that makes sense is the one that overrides the types to be :code:`INT32`. (0xff00, 0xfff0, 0xff0f, 0xffff). These are treated as no-ops, any other operation raises an :code:`exc_unknown_inst` exception. Since the prefixes that are accepted have no effect, they can be treated as individual instructions, decode doesn't have to combine them with the subsequent instruction word.
#. load/store multiple (and push/pop) are not implemented, they raise an :code:`exc_unknown_inst` exception.
#. :code:`VLEN` is set to 0, :code:`VSTART` and :code:`VEND` are not implemented. :code:`set_vlen` raises an :code:`exc_unknown_inst`

Floating point addition
-----------------------

The addition of :code:`FP32` as a supported type gets us a floating-point implementation. Type changes (including the type override prefix) are now mandatory. Of course all operations need to support both integer and floating-point types.

Support for :code:`FP64` is optional. If added though, support for load/store multiple (and push/pop) is mandatory: we now enter the realm where registers have type-dependent sizes.

Trivial vector support
----------------------

The most trivial vector support is the added support for :code:`VINT32` type. This can be combined with any vector length, but if :code:`VLEN` is set to 4, the implementation is rather trivial:

#. Broadcasting is not really a thing as both scalar and vector registers are 32-bit long and elements are also 32-bits in size
#. :code:`VSTART` and :code:`VEND` are still not needed, as their only possible values are 0 and 4 respectively.
#. :code:`set_vlen` returns a constant 4.
#. Extension groups are needed for predication

Type handling instructions however need to differentiate between vector- and scalar- registers, which adds some extra bookkeeping.

Load/store multiple (and push/pop) operations are optional

32-bit integer vector support
-----------------------------

A slightly more complete vector implementation is possible if (some of the) non-32-bit vector element support is added. The vector length still remains at 32-bits, but now 2-element and 4-element vector operations are possible.

#. Broadcasting now becomes mandatory
#. :code:`VEND` needs to be implemented, though :code:`VSTART` still isn't a thing
#. :code:`set_vlen` of course also needs an implementation
#. The ALU becomes significantly more complex