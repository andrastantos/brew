Floating point support
======================

There are three basic floating point types supported by Brew:

FP64: IEEE754 double-precision floating point type
FP32: IEEE754 single precision floating point type
FP16: IEEE754 half-precision floating point type

The latter of the three types is only supported in vector form.

Computation of the results for each type follow the IEEE754 standard.

A status register, :code:`FPSTAT` is provided for controlling rounding behavior and floating point exception reporting.

Exception handling
------------------

No floating point operation raises an actual hardware exception. Instead, if results cannot be produced for an operation, a 'sticky' status bit is set. These bits can later be interrogated by SW to see if any exceptional floating-point situations were encountered. The status bits auto-clear on read.

Under these exception results a special value (NaN for invalid results, Inf for overflow and a denormalized value for underflow) is put in the result.

For vector operations each element is treated independently with regards to exceptions: just because one lane results in an exceptional sitation, all other elements are computed and normal results are stored.

.. _csr_fpstat:

FPSTAT
------

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

The lowest 5 bits are 'sticky' bits: they are set by any floating-point operation with the an associated exception. The values are cleared by reading them.

The value of FPSTAT is saved and restored by the :ref:`load/store multiple<load_store_multiple>` instructions.

.. note:: The above layout matches that of RiscV to simply software porting.
