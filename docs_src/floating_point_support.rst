Floating point support
======================

Brew only defines support for single-precision floating point numbers, in the form of the FP32 register type.

Floating point computation follows the IEEE754 standard.

A status register, :code:`csr_fpstat` is provided for controlling rounding behavior and floating point exception reporting.

Exception handling
------------------

No floating point operation raises an actual hardware exception. Instead, if results cannot be produced for an operation, a 'sticky' status bit is set. These bits can later be interrogated by SW to see if any exceptional floating-point situations were encountered. The status bits auto-clear on read.

Under these exception results a special value (NaN for invalid results, Inf for overflow and a denormalized value for underflow) is put in the result.

For vector operations each element is treated independently with regards to exceptions: just because one lane results in an exceptional sitation, all other elements are computed and normal results are stored.

.. _fpstat:

csr_fpstat register
-------------------

The following bits are defined:


.. wavedrom::

  {config: {bits: 32}, config: {hspace: 500},
  reg: [
      { "bits": 24 },
      { "name": "frnd", bits: 3, attr: "rounding mode" },
      { "name": "fnv", bits: 1, attr: "invalid operation" },
      { "name": "fdz", bits: 1, attr: "division by zero" },
      { "name": "fof", bits: 1, attr: "overflow" },
      { "name": "fuf", bits: 1, attr: "underflow" },
      { "name": "fnx", bits: 1, attr: "inexact" },
  ],
  }

Rounding mode bit-fields are defined as follows:

========  ==========  ==============
Name      Value       Meaning
========  ==========  ==============
frm_ne    0           round to nearest, ties to even
frm_tz    1           round towards zero
frm_dn    2           round down
frm_up    3           round up
frm_mm    4           round to nearest, ties to max magnitude
========  ==========  ==============

The lowest 5 bits (:code:`fnv`, :code:`fdz`, :code:`fof`, :code:`fuf` and :code:`fnx`) have the following 'sticky' behavior:

#. When a floating-point exception of the appropriate kind is encountered while executing a floating-point operation, the bit is set.
#. When the register is read, these bits are cleared.
#. When the register is written, these bits are loaded by their corresponding value.

.. note:: The above layout matches that of RiscV to simply software porting.

.. note:: Since there's no separate :code:`csr_fpstat` register for TASK and SCHEDULER modes, the context-switching code must store and load the register, including the 'sticky' exception bits.

.. note:: Under normal operation (that is except for context switches) the exception status bits auto-clear to save the extra clearing operation. This auto-clear behavior is not problematic for context-switches as the new values are loaded from memory anyway.
