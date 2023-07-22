.. _rd_eq_csr_addr:

$rD <- CSR[ADDR]
----------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "8",         "bits": 4 },
      { "name": "f",         "bits": 4 },
      { "name": "0",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ]}

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16 },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       f       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Instruction code*: 0x.0f8 0x****

*Exceptions*: implementation defined

*Type variants*: No

Description
~~~~~~~~~~~

Load CSR value, specified by FIELD_E into $rD. In TASK mode the MSB of the CSR address is always set to 1. In SCHEDULER mode, the MSB is determined by bit 15 of FIELD_E. This way, only CSRs between 0x8000 and 0xffff are accessible from TASK mode, while all 65536 CSR locations are accessible for SCHEDULE mode code.

An implementation is free to translate this instruction into a memory access, in which case more granular access control can be implemented. If permission checks fail, the implementation can raise the appropriate access violation exception.

All CSRs are 32-bit wide. The type of :code:`$rD` is ignored and the value is stored in the lowest 32-bits of the register. The type of :code:`$rD` is not altered however.

.. todo:: this is not how loads and stores work with regards to types at all. If we want CSRs to be some kind of special memory, we should treat them as if they were loads and stores into a special memory region.


.. _csr_addr_eq_rd:

CSR[ADDR] <- $rD
----------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "9",         "bits": 4 },
      { "name": "f",         "bits": 4 },
      { "name": "0",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ]}

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16 },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       f       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Instruction code*: 0x.0f9 0x****

*Exceptions*: implementation defined

*Type variants*: No

Description
~~~~~~~~~~~

Store $rD in the specified CSR specified by FIELD_E. In TASK mode the MSB of the CSR address is always set to 1. In SCHEDULER mode, the MSB is determined by bit 15 of FIELD_E. This way, only CSRs between 0x8000 and 0xffff are accessible from TASK mode, while all 65536 CSR locations are accessible for SCHEDULE mode code.

An implementation is free to translate this instruction into a memory access, in which case more granular access control can be implemented. If permission checks fail, the implementation can raise the appropriate access violation exception.

All CSRs are 32-bit wide. The type of :code:`$rD` is ignored and is treated as INT32.

.. todo:: this is not how loads and stores work with regards to types at all. If we want CSRs to be some kind of special memory, we should treat them as if they were loads and stores into a special memory region.

