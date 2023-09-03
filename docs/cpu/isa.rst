Instruction Set Summary
=======================

In the following tables, instruction codes are listed as hexadecimal numbers, with the following extra conventions:

=============== ==========================
Character       Meaning
=============== ==========================
\.              any value in in the range [0x0:0xe]. Can be a different number at every occurrence.
\*              any value in in the range [0x0:0xf]. Can be a different number at every occurrence.
=============== ==========================

Instructions are fully decoded. Any instruction code not explicitly mentioned in the tables below generate an :code:`exc_unknown_inst` exception.

Instructions presented here in groups, based on their rough function and encoding similarities.


Exception group
---------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "0",         "bits": 4 },
      { "name": "0",         "bits": 4 },
      { "name": "0",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "op kind" },
  ]}

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       0       |       0       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

All instructions in this group enter SCHEDULER mode. After execution :code:`$tpc` points to the current instruction (the one generating the exception).

If executed in SCHEDULER mode, execution jumps to address 0 per standard :ref:`exception handling<exception_handling>` rules.

==================== =========== ========================================================
Instruction code     Assembly    Operation
==================== =========== ========================================================
:ref:`0x0000<swi_0>` SWI 0       Used to fill unused code-pages
:ref:`0x1000<swi_1>` SWI 1       Used for software breakpoints
:ref:`0x2000<swi_2>` SWI 2       Used to implement system calls
:ref:`0x3000<swi_3>` SWI 3
:ref:`0x4000<swi_4>` SWI 4
:ref:`0x5000<swi_5>` SWI 5
:ref:`0x6000<swi_6>` SWI 6
:ref:`0x7000<swi_7>` SWI 7
==================== =========== ========================================================

.. note:: While the HW doesn't put any limitations on what each of these instructions are used for, the first three SWI levels are allocated by convention.

Mode change and power management group
--------------------------------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "0",         "bits": 4 },
      { "name": "0",         "bits": 4 },
      { "name": "0",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "op kind" },
  ]}

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       0       |       0       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


===================== =========== =========================================================================
Instruction code      Assembly    Operation
===================== =========== =========================================================================
:ref:`0x8000<stm>`    STM         Enters TASK mode, enables interrupts
:ref:`0x9000<woi>`    WOI         Wake on interrupt. Waits for interrupt in both TASK and SCHEDULER mode
:ref:`0xa000<pflush>` PFLUSH      Flushes the pipeline
===================== =========== =========================================================================

.. todo:: PFLUSH is new. Needs testing in toolset/Espresso.

Atomic group
------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "1",         "bits": 4 },
      { "name": "0",         "bits": 4 },
      { "name": "0",         "bits": 4 },
      { "name": "RB",        "bits": 1 },
      { "name": "WB",        "bits": 1 },
      { "name": "RA",        "bits": 1 },
      { "name": "WA",        "bits": 1 },
  ]}

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       0       |       1       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


============================ =============== ============================================
Instruction code             Assembly        Operation
============================ =============== ============================================
:ref:`0x0001<fence_rw_rw>`   FENCE_RW_RW     Fence reads/writes before reads/writes after
:ref:`0x1001<fence__w_rw>`   FENCE__W_RW     Fence writes before reads/writes after
:ref:`0x2001<fence_r__rw>`   FENCE_R__RW     Fence reads before reads/writes after
:ref:`0x3001<fence____rw>`   FENCE____RW     Fence reads/writes after
:ref:`0x4001<fence_rw__w>`   FENCE_RW__W     Fence reads/writes before writes after
:ref:`0x5001<fence__w__w>`   FENCE__W__W     Fence writes before writes after
:ref:`0x6001<fence_r___w>`   FENCE_R___W     Fence reads before writes after
:ref:`0x7001<fence_____w>`   FENCE_____W     Fence writes after
:ref:`0x8001<fence_rw_r\\_>` FENCE_RW_R\_    Fence reads/writes before reads after
:ref:`0x9001<fence__w_r\\_>` FENCE__W_R\_    Fence writes before reads after
:ref:`0xa001<fence_r__r\\_>` FENCE_R__R\_    Fence reads before reads after
:ref:`0xb001<fence____r\\_>` FENCE____R\_    Fence reads after
:ref:`0xc001<fence_rw__\_>`  FENCE_RW___     Fence reads/writes before
:ref:`0xd001<fence__w__\_>`  FENCE__W___     Fence writes before
:ref:`0xe001<fence_r___\_>`  FENCE_R____     Fence reads before
============================ =============== ============================================

Every instruction in this group implements a fence, or an ordering between loads and stores. The top-most 4 bits of the instruction code is used to encode the fence type:

==========   ============
Bit-field    Meaning
==========   ============
12           ~R-before
13           ~W-before
14           ~R-after
15           ~W-after
==========   ============

.. note::
  bit-values are inverted to make FIELD_D==0xf an invalid encoding (i.e. no fence specification)

PC manipulation group
---------------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "op kind" },
      { "name": "0",         "bits": 4 },
      { "name": "0",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ]}


..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       0       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

======================== ============== =======================
Instruction code         Assembly       Operation
======================== ============== =======================
:ref:`0x.002<pc_eq_rd>`  $pc <- $rD     Indirect jump
:ref:`0x.003<tpc_eq_rd>` $tpc <- $rD    Update $tpc
:ref:`0x.004<rd_eq_pc>`  $rD <- $pc     Load $pc into register
:ref:`0x.005<rd_eq_tpc>` $rD <- $tpc    Load $tpc into register
======================== ============== =======================




CSR access group
----------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "op kind" },
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


==================================== ========================== =======================
Instruction code                     Assembly                   Operation
==================================== ========================== =======================
:ref:`0x.0f8 0x****<rd_eq_csr_addr>` $rD <- CSR[ADDR]           Load CSR value into $rD
:ref:`0x.0f9 0x****<csr_addr_eq_rd>` CSR[ADDR] <- $rD           Store $rD in CSR
==================================== ========================== =======================

The :code:`ADDR` field equals to :code:`FIELD_E` in SCHEDULER-mode. In task mode the MSB of :code:`ADDR` is forced to 1.

.. todo:: This is a new pair of instructions, no toolset support or Espresso implementation.

Unary group
-----------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "$rA" },
      { "name": "FIELD_B",   "bits": 4, attr: "op kind" },
      { "name": "0",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ]}


..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

================================== =========================== ===================================================
Instruction code                   Assembly                    Operation
================================== =========================== ===================================================
:ref:`0x.01.<rd_eq_tiny_const>`    $rD <- tiny CONST           Load $rD with constant [#note0xX01X]_
:ref:`0x.02.<rd_eq_pc_plus_const>` $rD <- $pc + CONST          Call return address calculation [#note0xX02X]_
:ref:`0x.03.<rd_eq_minus_ra>`      $rD <- -$rA                 Negative operation, depending on type
:ref:`0x.04.<rd_eq_notra>`         $rD <- ~$rA                 Binary inversion
:ref:`0x.05.<rd_eq_bse_ra>`        $rD <- bse $rA              Sign-extend from byte
:ref:`0x.06.<rd_eq_wse_ra>`        $rD <- wse $rA              Sign-extend from word
:ref:`0x.07.<rd_eq_popcnt_ra>`     $rD <- popcnt $rA           Counts number of bits set in $rA
:ref:`0x.08.<rd_eq_1_/_ra>`        $rD <- 1 / $rA              Reciprocal for floats
:ref:`0x.09.<rd_eq_rsqrt_ra>`      $rD <- rsqrt $rA            Reciprocal square-root for floats
:ref:`0x.0c.<type_rd_eq_ra>`       type $rD <- $rA             Sets type of $rD as denoted by $rA
:ref:`0x.0d.<rd_eq_type_ra>`       $rD <- type $rA             Loads type value of $rA into $rD
:ref:`0x.0e.<type_rd_eq_field_a>`  type $rD <- FIELD_A         Sets type of $rD
================================== =========================== ===================================================

.. [#note0xX01X] CONST=FIELD_A. FIELD_A is one-s complement; range is -7...7
.. [#note0xX02X] CONST=FIELD_A*2. FIELD_A is one-s complement; range is -7...7; NOTE: WE COULD MAKE THE RANGE A LITTLE HIGHER IF NOT ALLOW 0
.. [#note0xX0cX] All 32 bits of $rA are used. Any value above 0xe is RESERVED

.. todo:: Some of these instructions are not strictly necessary. Negation, inversion for instance can be done using binary operations with an immediate. Need profiling data to justify their existence.

.. todo:: $rD <- popcnt $rA and above is not implemented in Espresso

Binary ALU group
----------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "$rA" },
      { "name": "FIELD_B",   "bits": 4, attr: "$rB" },
      { "name": "FIELD_C",   "bits": 4, attr: "op kind" },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ]}


..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


======================================= =========================== ============================================
Instruction code                        Assembly                    Operation
======================================= =========================== ============================================
:ref:`0x.1..<rd_eq_ra_xor_rb>`          $rD <- $rA ^ $rB            Bit-wise 'xor'
:ref:`0x.2..<rd_eq_ra_or_rb>`           $rD <- $rA | $rB            Bit-wise 'or'
:ref:`0x.3..<rd_eq_ra_and_rb>`          $rD <- $rA & $rB            Bit-wise 'and'
:ref:`0x.4..<rd_eq_ra_plus_rb>`         $rD <- $rA + $rB            Type-dependent add
:ref:`0x.5..<rd_eq_ra_minus_rb>`        $rD <- $rA - $rB            Type-dependent subtract
:ref:`0x.6..<rd_eq_ra_lsl_rb>`          $rD <- $rA << $rB           Binary left-shift
:ref:`0x.7..<rd_eq_ra_lsr_rb>`          $rD <- $rA >> $rB           Binary right-shift
:ref:`0x.8..<rd_eq_ra_asr_rb>`          $rD <- $rA >>> $rB          Arithmetic right-shift
:ref:`0x.9..<rd_eq_ra_times_rb>`        $rD <- $rA * $rB            Type-dependent multiply
:ref:`0x.a..<rd_eq_type_rb>`            $rD <- TYPE_NAME $rB        Type conversion
:ref:`0x.b..<rd_eq_tiny_rb_plus_const>` $rD <- tiny $rB + CONST     Integer add [#note0xXbXX]_
0x.c..                                  see below (stack ops)
0x.d..                                  see below (stack ops)
0x.e..                                  see below (mem ops)
======================================= =========================== ============================================

.. [#note0xXbXX] CONST is FIELD_A is one's complement-coded; range is -7...7.

.. todo:: The TYPE_NAME instructions is not yet implemented in ESPRESSO

Load immediate group
--------------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "0",         "bits": 4 },
      { "name": "0",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E lower 16 bits", "bits": 16, attr: "VALUE lower 16 bits" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E upper 16 bits", "bits": 16, attr: "VALUE upper 16 bits" },
  ]
  }


or

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "e",         "bits": 4 },
      { "name": "0",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "op kind" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E lower 16 bits", "bits": 16, attr: "VALUE lower 16 bits" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E upper 16 bits", "bits": 16, attr: "VALUE upper 16 bits" },
  ]
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...
  |                         FIELD_E  lower 16 bits              ...
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...

  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  ...                       FIELD_E   upper 16 bits               |
  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=================================================== =========================== ==================================================
Instruction code                                    Assembly                    Operation
=================================================== =========================== ==================================================
:ref:`0x.00f 0x**** 0x****<rd_eq_value>`            $rD <- VALUE                Load immediate
:ref:`0x20ef 0x**** 0x****<pc_eq_value>`            $pc <- VALUE                Unconditional jump
:ref:`0x30ef 0x**** 0x****<tpc_eq_value>`           $tpc <- VALUE               Load immediate to $tpc
:ref:`0x80ef 0x**** 0x****<type_r0...r7_eq_value>`  type $r0...$r7 <- VALUE     Load immediate type values [#note_immedate_types]_
:ref:`0x90ef 0x**** 0x****<type_r8...r14_eq_value>` type $r8...$r14 <- VALUE    Load immediate type values [#note_immedate_types]_
=================================================== =========================== ==================================================

.. [#note_immedate_types]
  Types for each register are encoded in 4-bit nibbles. Lowest 4 bits determine the type of the lowest indexed register. Highest 4 bits determine the type of the highest indexed register. If nibble is set to 0xf, type of corresponding register is not changed.

Constant ALU group
------------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "FIELD_B",   "bits": 4, attr: "$rB" },
      { "name": "FIELD_C",   "bits": 4, attr: "op kind" },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E lower 16 bits", "bits": 16, attr: "VALUE lower 16 bits" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E upper 16 bits", "bits": 16, attr: "VALUE upper 16 bits" },
  ]
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...
  |                         FIELD_E  lower 16 bits              ...
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...

  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  ...                       FIELD_E   upper 16 bits               |
  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

================================================= =========================== ============================================
Instruction code                                  Assembly                    Operation
================================================= =========================== ============================================
:ref:`0x.1.f 0x**** 0x****<rd_eq_value_xor_rb>`   $rD <- VALUE ^ $rB          Bit-wise 'xor'
:ref:`0x.2.f 0x**** 0x****<rd_eq_value_or_rb>`    $rD <- VALUE | $rB          Bit-wise 'or'
:ref:`0x.3.f 0x**** 0x****<rd_eq_value_and_rb>`   $rD <- VALUE & $rB          Bit-wise 'and'
:ref:`0x.4.f 0x**** 0x****<rd_eq_value_plus_rb>`  $rD <- VALUE + $rB          Type-dependent add
:ref:`0x.5.f 0x**** 0x****<rd_eq_value_minus_rb>` $rD <- VALUE - $rB          Type-dependent subtract
:ref:`0x.6.f 0x**** 0x****<rd_eq_value_lsl_rb>`   $rD <- VALUE << $rB         Binary left-shift
:ref:`0x.7.f 0x**** 0x****<rd_eq_value_lsr_rb>`   $rD <- VALUE >> $rB         Binary right-shift
:ref:`0x.8.f 0x**** 0x****<rd_eq_value_asr_rb>`   $rD <- VALUE >>> $rB        Arithmetic right-shift
:ref:`0x.9.f 0x**** 0x****<rd_eq_value_times_rb>` $rD <- VALUE * $rB          Type-dependent multiply
0x.c.f 0x**** 0x****                              see below (stack ops)
0x.d.f 0x**** 0x****                              see below (stack ops)
0x.e.f 0x**** 0x****                              see below (mem ops)
================================================= =========================== ============================================

Short load immediate group
--------------------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "0",         "bits": 4 },
      { "name": "f",         "bits": 4 },
      { "name": "0",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16, attr: "VALUE" },
  ],
  }

or

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "e",         "bits": 4 },
      { "name": "f",         "bits": 4 },
      { "name": "0",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "op kind" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16, attr: "VALUE" },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |       f       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

======================================== =========================== =============================================
Instruction code                         Assembly                    Operation
======================================== =========================== =============================================
:ref:`0x.0f0 0x****<rd_eq_short_value>`  $rD <- short VALUE          Load sign-extended 16-bit immediate
:ref:`0x20fe 0x****<pc_eq_short_value>`  $pc <- short VALUE          Immediate short jump (value is sign-extended)
:ref:`0x30fe 0x****<tpc_eq_short_value>` $tpc <- short VALUE         Load sign-extended value into $tpc
======================================== =========================== =============================================



Short constant ALU group
------------------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "$rA" },
      { "name": "f",         "bits": 4 },
      { "name": "FIELD_C",   "bits": 4, attr: "op kind" },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16, attr: "VALUE" },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |       f       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

================================================ ==================================== ============================================
Instruction code                                 Assembly                             Operation
================================================ ==================================== ============================================
:ref:`0x.1f. 0x****<rd_eq_short_value_xor_ra>`   $rD <- short VALUE ^ $rA             Bit-wise 'xor'
:ref:`0x.2f. 0x****<rd_eq_short_value_or_ra>`    $rD <- short VALUE | $rA             Bit-wise 'or'
:ref:`0x.3f. 0x****<rd_eq_short_value_and_ra>`   $rD <- short VALUE & $rA             Bit-wise 'and'
:ref:`0x.4f. 0x****<rd_eq_short_value_plus_ra>`  $rD <- short VALUE + $rA             Type-dependent add
:ref:`0x.5f. 0x****<rd_eq_short_value_minus_ra>` $rD <- short VALUE - $rA             Type-dependent subtract
:ref:`0x.6f. 0x****<rd_eq_short_ra_lsl_value>`   $rD <- short $rA << VALUE            Binary left-shift
:ref:`0x.7f. 0x****<rd_eq_short_ra_lsr_value>`   $rD <- short $rA >> VALUE            Binary right-shift
:ref:`0x.8f. 0x****<rd_eq_short_ra_asr_value>`   $rD <- short $rA >>> VALUE           Arithmetic right-shift
:ref:`0x.9f. 0x****<rd_eq_short_value_times_ra>` $rD <- short VALUE * $rA             Type-dependent multiply
0x.cf. 0x****                                    see below (stack ops)
0x.df. 0x****                                    see below (stack ops)
0x.ef. 0x****                                    see below (mem ops)
================================================ ==================================== ============================================

.. note::
  FIELD_E is *always* sign-extended to 32-bits before applying it to the operation.

.. todo::
  We might want to zero-extend for certain operations, such as logical ops.

.. note::
  Sign-extending a 16-bit constant, then treating it as a float almost certainly don't make any sense. These operations make little sense for floating-point types.

Zero-compare conditional branch group
-------------------------------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "$rA" },
      { "name": "FIELD_B",   "bits": 4, attr: "op kind" },
      { "name": "0",         "bits": 4 },
      { "name": "f",         "bits": 4 },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16, attr: "VALUE" },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |       f       |    FIELD_C    |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

========================================================== ================================================== ==================
Instruction code                                           Assembly                                           Operation
========================================================== ================================================== ==================
:ref:`0xf00. 0x****<if_any_ra_eq_0__pc_eq_pc_plus_value>`  if any $rA == 0  $pc <- $pc + VALUE
:ref:`0xf01. 0x****<if_any_ra_ne_0__pc_eq_pc_plus_value>`  if any $rA != 0  $pc <- $pc + VALUE
:ref:`0xf02. 0x****<if_any_ra_lt_0___pc_eq_pc_plus_value>` if any $rA < 0   $pc <- $pc + VALUE                signed compare
:ref:`0xf03. 0x****<if_any_ra_ge_0__pc_eq_pc_plus_value>`  if any $rA >= 0  $pc <- $pc + VALUE                signed compare
:ref:`0xf04. 0x****<if_any_ra_gt_0___pc_eq_pc_plus_value>` if any $rA > 0   $pc <- $pc + VALUE                signed compare
:ref:`0xf05. 0x****<if_any_ra_le_0__pc_eq_pc_plus_value>`  if any $rA <= 0  $pc <- $pc + VALUE                signed compare
:ref:`0xf08. 0x****<if_all_ra_eq_0__pc_eq_pc_plus_value>`  if all $rA == 0  $pc <- $pc + VALUE
:ref:`0xf09. 0x****<if_all_ra_ne_0__pc_eq_pc_plus_value>`  if all $rA != 0  $pc <- $pc + VALUE
:ref:`0xf0a. 0x****<if_all_ra_lt_0___pc_eq_pc_plus_value>` if all $rA < 0   $pc <- $pc + VALUE                signed compare
:ref:`0xf0b. 0x****<if_all_ra_ge_0__pc_eq_pc_plus_value>`  if all $rA >= 0  $pc <- $pc + VALUE                signed compare
:ref:`0xf0c. 0x****<if_all_ra_gt_0___pc_eq_pc_plus_value>` if all $rA > 0   $pc <- $pc + VALUE                signed compare
:ref:`0xf0d. 0x****<if_all_ra_le_0__pc_eq_pc_plus_value>`  if all $rA <= 0  $pc <- $pc + VALUE                signed compare
========================================================== ================================================== ==================

.. note::
  For scalar types, FIELD_C MSB (inst[15]) is irrelevant; In other words, any/all selection doesn't matter

.. note:: VALUE computation: replicate LSB of FIELD_E to bit positions [31:16], replace LSB with 0.

Conditional branch group
------------------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "$rA" },
      { "name": "FIELD_B",   "bits": 4, attr: "$rB" },
      { "name": "FIELD_C",   "bits": 4, attr: "op kind" },
      { "name": "f",         "bits": 4 },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16, attr: "VALUE" },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |       f       |    FIELD_C    |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

================================================================= ======================================================== ==================
Instruction code                                                  Assembly                                                 Operation
================================================================= ======================================================== ==================
:ref:`0xf1.. 0x****<if_any_rb_eq_ra___pc_eq_pc_plus_value>`       if any $rB == $rA   $pc <- $pc + VALUE
:ref:`0xf2.. 0x****<if_any_rb_ne_ra___pc_eq_pc_plus_value>`       if any $rB != $rA   $pc <- $pc + VALUE
:ref:`0xf3.. 0x****<if_any_signed_rb_lt_ra__pc_eq_pc_plus_value>` if any signed $rB < $rA  $pc <- $pc + VALUE
:ref:`0xf4.. 0x****<if_any_signed_rb_ge_ra_pc_eq_pc_plus_value>`  if any signed $rB >= $rA $pc <- $pc + VALUE
:ref:`0xf5.. 0x****<if_any_rb_lt_ra____pc_eq_pc_plus_value>`      if any $rB < $rA    $pc <- $pc + VALUE
:ref:`0xf6.. 0x****<if_any_rb_ge_ra___pc_eq_pc_plus_value>`       if any $rB >= $rA   $pc <- $pc + VALUE
:ref:`0xf9.. 0x****<if_all_rb_eq_ra___pc_eq_pc_plus_value>`       if all $rB == $rA   $pc <- $pc + VALUE
:ref:`0xfa.. 0x****<if_all_rb_ne_ra___pc_eq_pc_plus_value>`       if all $rB != $rA   $pc <- $pc + VALUE
:ref:`0xfb.. 0x****<if_all_signed_rb_lt_ra__pc_eq_pc_plus_value>` if all signed $rB < $rA  $pc <- $pc + VALUE
:ref:`0xfc.. 0x****<if_all_signed_rb_ge_ra_pc_eq_pc_plus_value>`  if all signed $rB >= $rA $pc <- $pc + VALUE
:ref:`0xfd.. 0x****<if_all_rb_lt_ra____pc_eq_pc_plus_value>`      if all $rB < $rA    $pc <- $pc + VALUE
:ref:`0xfe.. 0x****<if_all_rb_ge_ra___pc_eq_pc_plus_value>`       if all $rB >= $rA   $pc <- $pc + VALUE
================================================================= ======================================================== ==================

.. note::
  For scalar types, FIELD_C MSB (inst[15]) is irrelevant; In other words, any/all selection doesn't matter

.. note:: VALUE computation: replicate LSB of FIELD_E to bit positions [31:16], replace LSB with 0.

Bit-set-test conditional branch group
-------------------------------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "$rA" },
      { "name": "f",         "bits": 4 },
      { "name": "FIELD_C",   "bits": 4, attr: "bit sel" },
      { "name": "f",         "bits": 4 },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16, attr: "VALUE" },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |       f       |    FIELD_C    |       f       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

========================== ======================================================== ==================
Instruction code           Assembly                                                 Operation
========================== ======================================================== ==================
0xf0f. 0x****              if $rA[0]  == 1 $pc <- $pc + VALUE
0xf1f. 0x****              if $rA[1]  == 1 $pc <- $pc + VALUE
0xf2f. 0x****              if $rA[2]  == 1 $pc <- $pc + VALUE
0xf3f. 0x****              if $rA[3]  == 1 $pc <- $pc + VALUE
0xf4f. 0x****              if $rA[4]  == 1 $pc <- $pc + VALUE
0xf5f. 0x****              if $rA[5]  == 1 $pc <- $pc + VALUE
0xf6f. 0x****              if $rA[6]  == 1 $pc <- $pc + VALUE
0xf7f. 0x****              if $rA[7]  == 1 $pc <- $pc + VALUE
0xf8f. 0x****              if $rA[8]  == 1 $pc <- $pc + VALUE
0xf9f. 0x****              if $rA[9]  == 1 $pc <- $pc + VALUE
0xfaf. 0x****              if $rA[14] == 1 $pc <- $pc + VALUE
0xfbf. 0x****              if $rA[15] == 1 $pc <- $pc + VALUE
0xfcf. 0x****              if $rA[16] == 1 $pc <- $pc + VALUE
0xfdf. 0x****              if $rA[30] == 1 $pc <- $pc + VALUE
0xfef. 0x****              if $rA[31] == 1 $pc <- $pc + VALUE
========================== ======================================================== ==================

.. note:: VALUE computation: replicate LSB of FIELD_E to bit positions [31:16], replace LSB with 0.


Bit-clear-test conditional branch group
---------------------------------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "FIELD_B",   "bits": 4, attr: "$rB" },
      { "name": "FIELD_C",   "bits": 4, attr: "bit sel" },
      { "name": "f",         "bits": 4 },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16, attr: "VALUE" },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |       f       |    FIELD_C    |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

========================== ======================================================== ==================
Instruction code           Assembly                                                 Operation
========================== ======================================================== ==================
0xf0.f 0x****              if $rB[0]  == 0 $pc <- $pc + VALUE
0xf1.f 0x****              if $rB[1]  == 0 $pc <- $pc + VALUE
0xf2.f 0x****              if $rB[2]  == 0 $pc <- $pc + VALUE
0xf3.f 0x****              if $rB[3]  == 0 $pc <- $pc + VALUE
0xf4.f 0x****              if $rB[4]  == 0 $pc <- $pc + VALUE
0xf5.f 0x****              if $rB[5]  == 0 $pc <- $pc + VALUE
0xf6.f 0x****              if $rB[6]  == 0 $pc <- $pc + VALUE
0xf7.f 0x****              if $rB[7]  == 0 $pc <- $pc + VALUE
0xf8.f 0x****              if $rB[8]  == 0 $pc <- $pc + VALUE
0xf9.f 0x****              if $rB[9]  == 0 $pc <- $pc + VALUE
0xfa.f 0x****              if $rB[14] == 0 $pc <- $pc + VALUE
0xfb.f 0x****              if $rB[15] == 0 $pc <- $pc + VALUE
0xfc.f 0x****              if $rB[16] == 0 $pc <- $pc + VALUE
0xfd.f 0x****              if $rB[30] == 0 $pc <- $pc + VALUE
0xfe.f 0x****              if $rB[31] == 0 $pc <- $pc + VALUE
========================== ======================================================== ==================

.. note:: VALUE computation: replicate LSB of FIELD_E to bit positions [31:16], replace LSB with 0.


Stack group
-----------

While simple stack operations (as in push/pull) are not supported by the ISA, special load/store instructions are provided with small offsets and $r12 ($fp) and $r13 ($sp) as the base register to support a compact form of common stack operations. The supported offset range is -256 to +252 bytes.

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "S",         "bits": 1, attr: "$rS" },
      { "name": "OFS",       "bits": 7, attr: "OFFSET" },
      { "name": "FIELD_C",   "bits": 4, attr: "op kind" },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |            OFS            | S |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

============================================ ================================ ====================
Instruction code                             Assembly                         Operation
============================================ ================================ ====================
:ref:`0x.c**<mem_rs_plus_tiny_offset_eq_rd>` MEM32[$rS + tiny OFFSET] <- $rD  Store $rD in memory
:ref:`0x.d**<rd_eq_mem_rs_plus_tiny_offset>` $rD <- MEM32[$rS + tiny OFFSET]  Load $rD from memory
============================================ ================================ ====================

.. warning::
  The encoding of field S is special: A=0 denotes $r12, A=1 denotes $r13

.. note::
  OFFSET must be 32-bit aligned, so it's lowest two bits are not stored. The supported offset range is from -512 to 508

.. note::
  the existence of these ops complicate memory op decode as well as operation size decode, but save a *huge* amount of code-space, allowing almost all register spills and fills to be done in two bytes.

Indirect load/Store group
-------------------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "$rA" },
      { "name": "FIELD_B",   "bits": 4, attr: "op kind" },
      { "name": "e",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

============================== =============================== ===========================================
Instruction code               Assembly                        Operation
============================== =============================== ===========================================
:ref:`0x.e4.<rd_eq_mem8_ra>`   $rD <- MEM8[$rA]                8-bit unsigned load from MEM[$rA] into $rD
:ref:`0x.e5.<rd_eq_mem16_ra>`  $rD <- MEM16[$rA]               16-bit unsigned load from MEM[$rA] into $rD
:ref:`0x.e6.<rd_eq_mem_ra>`    $rD <- MEM32[$rA]               32-bit load from MEM[$rA] into $rD
:ref:`0x.e7.<rd_eq_memll_ra>`  $rD <- MEMLL[$rA]               32-bit unsigned load-lock (exclusive load)
:ref:`0x.e8.<mem8_ra_eq_rd>`   MEM8[$rA] <- $rD                8-bit store to MEM[$rA] from $rD
:ref:`0x.e9.<mem16_ra_eq_rd>`  MEM16[$rA] <- $rD               16-bit store to MEM[$rA] from $rD
:ref:`0x.ea.<mem_ra_eq_rd>`    MEM32[$rA] <- $rD               32-bit store to MEM[$rA] from $rD
:ref:`0x.eb.<memsc_ra_eq_rd>`  MEMSC[$rA] <- $rD               32-bit store-conditional (exclusive store)
:ref:`0x.ec.<rd_eq_smem8_ra>`  $rD <- SMEM8[$rA]               8-bit signed load from MEM[$rA] into $rD
:ref:`0x.ed.<rd_eq_smem16_ra>` $rD <- SMEM16[$rA]              16-bit signed load from MEM[$rA] into $rD
============================== =============================== ===========================================



Indirect jump group
-------------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "$rA" },
      { "name": "e",         "bits": 4 },
      { "name": "e",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "op kind" },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

============================ =============================== =====================================
Instruction code             Assembly                        Operation
============================ =============================== =====================================
:ref:`0x1ee.<inv_ra>`        INV[$rA]                        invalidate cache line for address $rA
:ref:`0x2ee.<pc_eq_mem_ra>`  $pc <- MEM32[$rA]               32-bit load from MEM[$rA] into $PC
:ref:`0x3ee.<tpc_eq_mem_ra>` $tpc <- MEM32[$rA]              32-bit load from MEM[$rA] into $TPC
============================ =============================== =====================================



Offset-indirect load/store group
--------------------------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "$rA" },
      { "name": "FIELD_B",   "bits": 4, attr: "op kind" },
      { "name": "f",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16, attr: "VALUE" },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       f       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

================================================ ======================================= =================================================
Instruction code                                 Assembly                                Operation
================================================ ======================================= =================================================
:ref:`0x.f4. 0x****<rd_eq_mem8_ra_plus_value>`   $rD <- MEM8[$rA + VALUE]                8-bit unsigned load from MEM[$rA+VALUE] into $rD
:ref:`0x.f5. 0x****<rd_eq_mem16_ra_plus_value>`  $rD <- MEM16[$rA + VALUE]               16-bit unsigned load from MEM[$rA+VALUE] into $rD
:ref:`0x.f6. 0x****<rd_eq_mem_ra_plus_value>`    $rD <- MEM32[$rA + VALUE]               32-bit load from MEM[$rA+VALUE] into $rD
:ref:`0x.f7. 0x****<rd_eq_memll_ra_plus_value>`  $rD <- MEMLL[$rA + VALUE]               32-bit unsigned load-lock (exclusive load)
:ref:`0x.f8. 0x****<mem8_ra_plus_value_eq_rd>`   MEM8[$rA + VALUE] <- $rD                8-bit store to MEM[$rA+VALUE] from $rD
:ref:`0x.f9. 0x****<mem16_ra_plus_value_eq_rd>`  MEM16[$rA + VALUE] <- $rD               16-bit store to MEM[$rA+VALUE] from $rD
:ref:`0x.fa. 0x****<mem_ra_plus_value_eq_rd>`    MEM32[$rA + VALUE] <- $rD               32-bit store to MEM[$rA+VALUE] from $rD
:ref:`0x.fb. 0x****<memsc_ra_plus_value_eq_rd>`  MEMSC[$rA + VALUE] <- $rD               32-bit store-conditional (exclusive store)
:ref:`0x.fc. 0x****<rd_eq_smem8_ra_plus_value>`  $rD <- SMEM8[$rA + VALUE]               8-bit signed load from MEM[$rA+VALUE] into $rD
:ref:`0x.fd. 0x****<rd_eq_smem16_ra_plus_value>` $rD <- SMEM16[$rA + VALUE]              16-bit signed load from MEM[$rA+VALUE] into $rD
================================================ ======================================= =================================================

.. note:: FIELD_E is sign-extended before addition


Offset-indirect jump group
--------------------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "$rA" },
      { "name": "e",         "bits": 4 },
      { "name": "f",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "op kind" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16, attr: "VALUE" },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       f       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

============================================== ======================================= =============================================
Instruction code                               Assembly                                Operation
============================================== ======================================= =============================================
:ref:`0x1fe. 0x****<inv_ra_plus_value>`        INV[$rA + VALUE]                        invalidate cache line for address $rA+FIELD_E
:ref:`0x2fe. 0x****<pc_eq_mem_ra_plus_value>`  $pc <- MEM32[$rA + VALUE]               32-bit load from MEM[$rA+VALUE] into $PC
:ref:`0x3fe. 0x****<tpc_eq_mem_ra_plus_value>` $tpc <- MEM32[$rA + VALUE]              32-bit load from MEM[$rA+VALUE] into $TPC
============================================== ======================================= =============================================

.. note:: FIELD_E is sign-extended before addition

Absolute load/store group
-------------------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "FIELD_B",   "bits": 4, attr: "op kind" },
      { "name": "f",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E lower 16 bits", "bits": 16, attr: "VALUE lower 16 bits" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E upper 16 bits", "bits": 16, attr: "VALUE upper 16 bits" },
  ]
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       f       |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...
  |                         FIELD_E  lower 16 bits              ...
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...

  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  ...                       FIELD_E   upper 16 bits               |
  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=============================================== =========================== =============================================
Instruction code                                Assembly                    Operation
=============================================== =========================== =============================================
:ref:`0x.f4f 0x**** 0x****<rd_eq_mem8_value>`   $rD <- MEM8[VALUE]          8-bit unsigned load from MEM[VALUE] into $rD
:ref:`0x.f5f 0x**** 0x****<rd_eq_mem16_value>`  $rD <- MEM16[VALUE]         16-bit unsigned load from MEM[VALUE] into $rD
:ref:`0x.f6f 0x**** 0x****<rd_eq_mem_value>`    $rD <- MEM32[VALUE]         32-bit load from MEM[VALUE] into $rD
:ref:`0x.f7f 0x**** 0x****<rd_eq_memll_value>`  $rD <- MEMLL[VALUE]         32-bit unsigned load-lock (exclusive load)
:ref:`0x.f8f 0x**** 0x****<mem8_value_eq_rd>`   MEM8[VALUE] <- $rD          8-bit store to MEM[VALUE] from $rD
:ref:`0x.f9f 0x**** 0x****<mem16_value_eq_rd>`  MEM16[VALUE] <- $rD         16-bit store to MEM[VALUE] from $rD
:ref:`0x.faf 0x**** 0x****<mem_value_eq_rd>`    MEM32[VALUE] <- $rD         32-bit store to MEM[VALUE] from $rD
:ref:`0x.fbf 0x**** 0x****<memsc_value_eq_rd>`  MEMSC[VALUE] <- $rD         32-bit store-conditional (exclusive store)
:ref:`0x.fcf 0x**** 0x****<rd_eq_smem8_value>`  $rD <- SMEM8[VALUE]         8-bit signed load from MEM[VALUE] into $rD
:ref:`0x.fdf 0x**** 0x****<rd_eq_smem16_value>` $rD <- SMEM16[VALUE]        16-bit signed load from MEM[VALUE] into $rD
=============================================== =========================== =============================================


Absolute jump group
-------------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "e",         "bits": 4 },
      { "name": "f",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "op kind" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E lower 16 bits", "bits": 16, attr: "VALUE lower 16 bits" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E upper 16 bits", "bits": 16, attr: "VALUE upper 16 bits" },
  ]
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       f       |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...
  |                         FIELD_E  lower 16 bits              ...
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...

  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  ...                       FIELD_E   upper 16 bits               |
  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

============================================= =========================== =========================================
Instruction code                              Assembly                    Operation
============================================= =========================== =========================================
:ref:`0x1fef 0x**** 0x****<inv_value>`        INV[VALUE]                  invalidate cache line for address FIELD_E
:ref:`0x2fef 0x**** 0x****<pc_eq_mem_value>`  $pc <- MEM32[VALUE]         32-bit load from MEM[VALUE] into $PC
:ref:`0x3fef 0x**** 0x****<tpc_eq_mem_value>` $tpc <- MEM32[VALUE]        32-bit load from MEM[VALUE] into $TPC
============================================= =========================== =========================================



Prefix instructions
-------------------

Prefix instructions can precede any other instruction to modify their behavior.

.. note::
  *Exception behavior*: If a prefixed instruction throws an exception, $tpc points to the (first) prefix instruction after entering SCHEDULER mode. This allows the recovery code to decode and potentially retry the excepted instruction.

.. note::
  *Interrupt behavior*: If an interrupt is handled during the execution of a prefixed instruction, $tpc points to the (first) prefix instruction after entering SCHEDULER mode. None of the side-effects of the prefixed instruction take effect. If any of the side-effects of the prefixed instruction have taken effect, the whole instruction must be carried to completion and $tpc points to the subsequent instruction after entering SCHEDULER mode. In other words, under no circumstances can $tpc point anywhere between the first prefix and it's corresponding instruction when entering SCHEDULER mode.

.. note::
  *Prefix concatenation*: Every processor implementation has a maximum instruction length it supports. In this version of the spec, it's 64 bits. If an instruction with all its prefixes exceeds this limit, the processor raises an :code:`exc_unknown_inst` exception, with $tpc pointing to the first prefix instruction. Without this provision it would be possible to create arbitrarily long instruction sequences in TASK mode. That in turn would prevent interrupts from being raised, effectively locking up the system (at least up to the point of exhausting the addressable memory space). The ISA puts further restrictions on what prefix instructions can be cascaded. As a general rule, prefixes of the same kind can appear only once in a prefix cascade.

Type override
~~~~~~~~~~~~~

This prefix instruction allows for the changing the way the subsequent operation interprets source operand types. It doesn't actually change the source register types. The result type that is written into the destination along with its value is the result type obtained after the type overrides.

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "TYPE_A",    "bits": 4, attr: "type override A" },
      { "name": "TYPE_B",    "bits": 4, attr: "type override B" },
      { "name": "f",         "bits": 4 },
      { "name": "f",         "bits": 4 },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |       f       |       f       |     TYPE_A    |    TYPE_B     | ...
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

========================================== ====================== ===========================================================
Instruction code                           Assembly               Operation
========================================== ====================== ===========================================================
:ref:`0xff** ...<type_overrides_detail>`   Type override (<type>) Type override for $rA (TYPE_A) and $rB (TYPE_B).
========================================== ====================== ===========================================================

Type override for $rA (TYPE_A) and $rB (TYPE_B).

If either TYPE_A or TYPE_B is set to 0xf, the corresponding register type is not overridden: the type from the register file is used during the subsequent operation.

.. todo:: type overrides are not supported by the toolchain