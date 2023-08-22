
.. _csr:

CSRs
====

Control and Status registers (CSRs) provide configuration and status information for a processor. This address space is unique to every processor and hardware thread in the system, while some of the underlying registers may be shared amongst them.

The address space uses a 16-bit address, each address corresponding to a 32-bit value. Most addresses are not used. The MSB of the address is always set for CSR accesses issued from TASK mode. In SCHEDULER-mode, all 16-bits of the address are under SW control; this allows for arranging for sensitive CSRs to be only accessible from SCHEDULER-mode, albeit at the cost of more complicated address layout.

Implementations can provide more fine-grained access control through implementation-defined means so long as they maintain full access to the CSR space from SCHEDULER-mode. One technique would be to translate CSR accesses into a memory access, in which case conventional MMU-based access control can be employed. If permission checks fail, the implementation can raise the appropriate access violation exception.


CSR registers
-------------

Currently the following registers are defined:

Registers only accessible from SCHEDULER mode:

========== ============== ============== ===================================================
Address    Name           Access type    Description
========== ============== ============== ===================================================
0x0000     csr_ecause     R              Exception cause register
0x0001     csr_eaddr      R              Exception address register
========== ============== ============== ===================================================

Registers accessible from both TASK and SCHEDULER mode:

========== ============== ============== ===================================================
Address    Name           Access type    Description
========== ============== ============== ===================================================
0x8000     csr_mach_arch  R              Machine architecture and version register
0x8001     csr_capability R              Capability bit-field
0x8400     csr_fpstat     RW             Floating point status and control register
========== ============== ============== ===================================================

Implementation-defined CSRs
---------------------------

An implementation is almost certainly going to need to implement custom CSRs on the top of the existing ones. This should be done carefully:

#. Making CSRs visible (especially writable) from TASK mode can open security holes in the implementation
#. At the same time, having CSRs only accessible from SCHEDULER mode might be too slow and restrictive (remember, most OS services are running in TASK mode on Brew)
#. Making CSRs visible from TASK mode (especially without further access restrictions) makes them part of the user-space machine definition. This in turn allows for SW developers to exploit implementation-defined behavior, reducing application-level compatibility.

The CSR address region 0x4000 to 0x7fff is reserved for implementation-defined CSRs, accessible only from SCHEDULER mode.
The CSR address region 0xc000 to 0xffff is reserved for implementation-defined CSRs, accessible from both SCHEDULER and task mode.

Examples of implementation-defined CSRs include:

#. MMU setup registers
#. Cache controller registers
#. Performance counters

CSR register definitions
------------------------

csr_ecause
^^^^^^^^^^

Contains the exception code for the latest exception as described in the :ref:`exception handling<exception_handling>` chapter.

csr_eaddr
^^^^^^^^^

Contains the exception address for the latest exception as described in the :ref:`exception handling<exception_handling>` chapter.

csr_mach_arch
^^^^^^^^^^^^^

.. wavedrom::

  {config: {bits: 32}, config: {hspace: 500},
  reg: [
      { "name": "ISA_REV",        "bits": 4, attr: "0" },
      { "name": "CORE_VER_MAJOR", "bits": 4 },
      { "name": "CORE_VER_MINOR", "bits": 4 },
      { "name": "CORE_STEPPING",  "bits": 4 },
      { "name": "CORE_FAMILY",    "bits": 16 },
  ]}


The :code:`ISA_REV` field contains the revision of the Brew spec (what you're reading right now) the core adheres to. The current spec defines this field having the numeric value of 0.

The :code:`CORE_VER_MAJOR` field contains the implementation-defined major version of the CPU core.

The :code:`CORE_VER_MINOR` field contains the implementation-defined minor version of the CPU core.

The :code:`CORE_STEPPING` field contains the implementation-defined silicon stepping of the CPU core.

The :code:`CORE_FAMILY` field contains the family of the CPU core. The value of this field is globally coordinated and must be unique for all CPU cores within a family.

.. todo:: need to add CORE_FAMILY coordination table.

csr_capability
^^^^^^^^^^^^^^

.. wavedrom::

  {config: {bits: 32}, config: {hspace: 500},
  reg: [
      { "bits": 31, attr: 0 },
      { "name": "FP_SUPPORT",     "bits": 1 },
  ]}

This register contains individual capabilities, that are independently defined from the content of the :code:`csr_mach_arch` register. In other words, these are common capabilities that relate to optional features in the Brew spec.

The only such feature for this revision of the spec is float-point support. This is reflected in the :code:`FP_SUPPORT` bit.

For all bits, the value '0' means the feature is not implemented, '1' means the feature is available.

The top 31-bits of the register is thus reserved and read as 0 for this revision of the spec.

csr_fpstat
^^^^^^^^^^

Contains the floating-point status and control register bits as described in the :ref:`csr_fpstat register<fpstat>` chapter.
