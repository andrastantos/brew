.. _rd_eq_cpu_id:

$rD <- CPU_ID
-------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "8",         "bits": 4, attr: "op kind" },
      { "name": "0",         "bits": 4 },
      { "name": "0",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ]}


Load 'dirty' mask into $rD

.. note::
  All instruction codes in this group are treated as jump instructions by the branch predictor, if exists. After warming up, some will always be predicted taken, some will not be. In TASK mode indirect jump (0x.002) and $tpc update (0x.003) instructions have the exact same behavior, however might have different latencies.

.. todo::
  What are the consequences of manipulating VSTART/VEND/DIRTY in TASK mode?

.. _dirty_eq_rd:

DIRTY <- $rD
------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "op kind" },
      { "name": "0",         "bits": 4 },
      { "name": "0",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ]}

Set 'dirty' mask based on $rD

.. note::
  All instruction codes in this group are treated as jump instructions by the branch predictor, if exists. After warming up, some will always be predicted taken, some will not be. In TASK mode indirect jump (0x.002) and $tpc update (0x.003) instructions have the exact same behavior, however might have different latencies.

.. todo::
  What are the consequences of manipulating VSTART/VEND/DIRTY in TASK mode?

.. _rd_eq_vstart:

$rD <- VSTART
-------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "op kind" },
      { "name": "0",         "bits": 4 },
      { "name": "0",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ]}

Load 'vstart' into $rD

.. note::
  All instruction codes in this group are treated as jump instructions by the branch predictor, if exists. After warming up, some will always be predicted taken, some will not be. In TASK mode indirect jump (0x.002) and $tpc update (0x.003) instructions have the exact same behavior, however might have different latencies.

.. todo::
  What are the consequences of manipulating VSTART/VEND/DIRTY in TASK mode?

.. _vstart_eq_rd:

VSTART <- $rD
-------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "op kind" },
      { "name": "0",         "bits": 4 },
      { "name": "0",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ]}

Set 'vstart' from $rD

.. note::
  All instruction codes in this group are treated as jump instructions by the branch predictor, if exists. After warming up, some will always be predicted taken, some will not be. In TASK mode indirect jump (0x.002) and $tpc update (0x.003) instructions have the exact same behavior, however might have different latencies.

.. todo::
  What are the consequences of manipulating VSTART/VEND/DIRTY in TASK mode?

.. _rd_eq_vend:

$rD <- VEND
-----------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "op kind" },
      { "name": "0",         "bits": 4 },
      { "name": "0",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ]}

Load 'vend' into $rD

.. note::
  All instruction codes in this group are treated as jump instructions by the branch predictor, if exists. After warming up, some will always be predicted taken, some will not be. In TASK mode indirect jump (0x.002) and $tpc update (0x.003) instructions have the exact same behavior, however might have different latencies.

.. todo::
  What are the consequences of manipulating VSTART/VEND/DIRTY in TASK mode?

.. _vend_eq_rd:

VEND <- $rD
-----------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "op kind" },
      { "name": "0",         "bits": 4 },
      { "name": "0",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ]}

Set 'vend' from $rD

.. note::
  All instruction codes in this group are treated as jump instructions by the branch predictor, if exists. After warming up, some will always be predicted taken, some will not be. In TASK mode indirect jump (0x.002) and $tpc update (0x.003) instructions have the exact same behavior, however might have different latencies.

.. todo::
  What are the consequences of manipulating VSTART/VEND/DIRTY in TASK mode?

.. _rd_eq_vlen:

$rD <- VLEN
-----------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "op kind" },
      { "name": "0",         "bits": 4 },
      { "name": "0",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ]}


Load HW vector length into $rD

.. note::
  All instruction codes in this group are treated as jump instructions by the branch predictor, if exists. After warming up, some will always be predicted taken, some will not be. In TASK mode indirect jump (0x.002) and $tpc update (0x.003) instructions have the exact same behavior, however might have different latencies.

.. todo::
  What are the consequences of manipulating VSTART/VEND/DIRTY in TASK mode?
