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

*Instruction code*: 0x.0f8 0x****

*Exceptions*: implementation defined

*Type variants*: No

Description
~~~~~~~~~~~

Load :ref:`CSR<csr>` value, specified by FIELD_E into $rD. In TASK mode the MSB of the CSR address is always set to 1. In SCHEDULER mode, the MSB is determined by bit 15 of FIELD_E. This way, only CSRs between 0x8000 and 0xffff are accessible from TASK mode, while all 65536 CSR locations are accessible for SCHEDULE mode code.

All CSRs are 32-bit wide. The type of :code:`$rD` is handled per :ref:`load type handling<load_type_handling>` rules. The type of :code:`$rD` is not altered.


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

*Instruction code*: 0x.0f9 0x****

*Exceptions*: implementation defined

*Type variants*: No

Description
~~~~~~~~~~~

Store $rD in the specified :ref:`CSR<csr>` specified by FIELD_E. In TASK mode the MSB of the CSR address is always set to 1. In SCHEDULER mode, the MSB is determined by bit 15 of FIELD_E. This way, only CSRs between 0x8000 and 0xffff are accessible from TASK mode, while all 65536 CSR locations are accessible for SCHEDULE mode code.

All CSRs are 32-bit wide. The type of :code:`$rD` is handled per :ref:`store type handling<store_type_handling>` rules.

