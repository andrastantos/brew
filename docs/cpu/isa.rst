Instruction Set Summary
=======================

In the following tables

'.':
  means any value in [0x0:0xe], unless specifically listed as a special case. Can be a different number at every occurrence.
'*':
 means any value in [0x0:0xf] Can be a different number at every occurrence.

Instructions are fully decoded. Any instruction not explicitly mentioned in the tables below generate an 'invalid instruction exception' and is functionally equivalent to the SII instruction.

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

All instructions in this group enter SCHEDULER mode. After execution $tpc points to the current instruction (the one generating the exception)

=================  ========    ============      ==================
Instruction code   Assembly    Alternative       Operation
=================  ========    ============      ==================
0x0000             SWI 0       FILL              Used to fill unused code-pages;
0x1000             SWI 1       BREAK             Used for software breakpoints
0x2000             SWI 2       SYSCALL           Used to implement system calls
0x3000             SWI 3
0x4000             SWI 4
0x5000             SWI 5
0x6000             SWI 6
0x7000             SWI 7       SII               Functionally equivalent to invalid instruction exception
=================  ========    ============      ==================

.. TODO::
  The toolset might still think SII is 0x6000 and HWI is 0x7000! Need to follow-up

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


=================  ========    ==================
Instruction code   Assembly    Operation
=================  ========    ==================
0x8000             STM         Enters TASK mode, enables interrupts; $spc points to the NEXT instruction
0x9000             WOI         Wake on interrupt. Waits for interrupt in both TASK and SCHEDULER mode
0xa000             PFLUSH      Flushes the pipeline
0xb000             SII
0xc000             SII
0xd000             SII
0xe000             SII
=================  ========    ==================

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


=================  ========
Instruction code   Assembly
=================  ========
0x0001             FENCE_RW_RW
0x1001             FENCE__W_RW
0x2001             FENCE_R__RW
0x3001             FENCE____RW
0x4001             FENCE_RW__W
0x5001             FENCE__W__W
0x6001             FENCE_R___W
0x7001             FENCE_____W
0x8001             FENCE_RW_R\_
0x9001             FENCE__W_R\_
0xa001             FENCE_R__R\_
0xb001             FENCE____R\_
0xc001             FENCE_RW___
0xd001             FENCE__W___
0xe001             FENCE_R____
=================  ========

Every instruction in this group implements a fence, or an ordering between loads and stores. The top-most 4 bits of the instruction code is used the encode the fence type:

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

.. note::
  Fences have no effect on cache contents. In particular, fences don't invalidate the instruction cache (if exists) and cannot be exclusively used to implement coherency between data and instruction stream such as needed for self-modifying code.

.. important::
  Depending on the implementation, some or all of these fence operations might be no-ops. Care should be taken to ensure proper fence behavior for writes that leave in-order but have their side-effects out-of-order due to latency-differences through the interconnect.

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

=================  ===========    ==================
Instruction code   Assembly       Operation
=================  ===========    ==================
0x.002             $pc <- $rD     Indirect jump
0x.003             $tpc <- $rD    Update $tpc
0x.004             $rD <- $pc     Load $pc into register
0x.005             $rD <- $tpc    Load $tpc into register
0x.006
0x.007
0x.008             $rD <- DIRTY   Load 'dirty' mask into $rD
0x.009             DIRTY <- $rD   Set 'dirty' mask based on $rD
0x.00a             $rD <- VSTART  Load 'vstart' into $rD
0x.00b             VSTART <- $rD  Set 'vstart' from $rD
0x.00c             $rD <- VEND    Load 'vend' into $rD
0x.00d             VEND <- $rD    Set 'vend' from $rD
0x.00e             $rD <- VLEN    Load HW vector length into $rD
=================  ===========    ==================

.. note::
  All instruction codes in this group are treated as jump instructions by the branch predictor, if exists. After warming up, some will always be predicted taken, some will not be. In TASK mode indirect jump (0x.002) and $tpc update (0x.003) instructions have the exact same behavior, however might have different latencies.

.. todo::
  We might want to shift encoding to 0x.004 ... 0x.007 to make the branch predictors job easier at recognizing this class.

.. todo::
  What are the consequences of manipulating VSTART/VEND/DIRTY in TASK mode?

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

=================  ========================    ==================
Instruction code   Assembly                    Operation
=================  ========================    ==================
0x.01.             $rD <- tiny FIELD_A         Load $rD with constant [#note0xX01X]_
0x.02.             $rD <- $pc + FIELD_A*2      Call return address calculation [#note0xX02X]_
0x.03.             $rD <- -$rA                 Negative operation, depending on type
0x.04.             $rD <- ~$rA                 Binary inversion
0x.05.             $rD <- bse $rA              Sign-extend from byte [#note0xX05X]_
0x.06.             $rD <- wse $rA              Sign-extend from word
0x.07.             $rD <- float $rA            Convert to float. No-op if $rA is already a float
0x.08.             $rD <- int $rA              Convert to integer. No-op if $rA is already integer
0x.09.             $rD <- 1 / $rA              Reciprocal for floats [#note0xX09X]_
0x.0a.             $rD <- rsqrt $rA            Reciprocal square-root for floats [#note0xX0aX]_
0x.0b.
0x.0c.             type $rD <- $rA             Sets type of $rD as denoted by $rA [#note0xX0cX]_
0x.0d.             $rD <- type $rA             Loads type value of $rA into $rD
0x.0e.             type $rD <- FIELD_A         Sets type of $rD
=================  ========================    ==================

.. [#note0xX01X] FIELD_A is one-s complement; range is -7...7
.. [#note0xX02X] FIELD_A is one-s complement; range is -7...7; NOTE: WE COULD MAKE THE RANGE A LITTLE HIGHER IF NOT ALLOW 0
.. [#note0xX05X] For vector types, operation is per-lane. Floating point types are treated as integer
.. [#note0xX09X] Operation is RESERVED for integer types.
.. [#note0xX0aX] Operation is RESERVED for integer types.
.. [#note0xX0bX] This is a rather odd-ball instruction. Only meaningful for vector source types.
.. [#note0xX0cX] All 32 bits of $rA are used. Any value above 0xe is RESERVED


.. note::
  We only have reduction sum. Is there any other *really* important reduction op we need?

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


=================  ==========================  ==================
Instruction code   Assembly                    Operation
=================  ==========================  ==================
0x.1..             $rD <- $rA ^ $rB            Bit-wise 'xor' [#note_logical]_
0x.2..             $rD <- $rA | $rB            Bit-wise 'or'  [#note_logical]_
0x.3..             $rD <- $rA & $rB            Bit-wise 'and' [#note_logical]_
0x.4..             $rD <- $rA + $rB            Type-dependent add
0x.5..             $rD <- $rA - $rB            Type-dependent subtract
0x.6..             $rD <- $rA << $rB           Binary left-shift [#note_binary_shift]_
0x.7..             $rD <- $rA >> $rB           Binary right-shift [#note_binary_shift]_
0x.8..             $rD <- $rA >>> $rB          Arithmetic right-shift [#note_binary_shift]_
0x.9..             $rD <- $rA * $rB            Type-dependent multiply
0x.a..             $rD <- ~$rA & $rB           Bit-wise 'not'-'and' [#note0xXaXX]_
0x.b..             $rD <- tiny $rB + FIELD_A   Integer add [#note0xXbXX]_
0x.c..             see below (stack ops)
0x.d..             see below (stack ops)
0x.e..             see below (mem ops)
=================  ==========================  ==================

.. [#note_logical] This operation ignore type info, but sets destination type to be the same as that of $rA
.. [#note_binary_shift] This operation only uses the lane-setup part of the type information. It sets the destination type to that of $rA
.. [#note0xXaXX] This operation is useful for lane-combining with an inverted predicate
.. [#note0xXbXX] FIELD_A is one's complement-coded; range is -7...7. This operation only uses the lane-setup part of the type information. It sets the destination type to that of $rA

.. note::
  If swizzle muxes are inline in the pipeline (as opposed to their own execution unit), it's possible to deal with scalar-vector combinations, where the scalar gets automatically replicated into the right number of lanes before the operation is performed. Similarly, a 2-lane-and-4-lane vector operation can replicate the 2-lane vector into 4 lanes before executing the operation.

.. todo::
  What should the behavior be for unsupported type-combinations? One would probably want an exception so that SW emulation can fill the gaps on lower-end processors, but then again, that makes almost all operations a possible exception source, and thus forces the pipeline to be more conservative.

.. note:: Output type is the type of $rA

.. note:: Pseudo instructions
  NOP: encodes to 0x2222, which is $r2 <- $r2 | $r2
  $rD <- $rS: encodes to 0xD2SS

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
      { "name": "FIELD_E lower 16 bits", "bits": 16 },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E upper 16 bits", "bits": 16 },
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
      { "name": "FIELD_E lower 16 bits", "bits": 16 },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E upper 16 bits", "bits": 16 },
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

=========================  ========================    ==================
Instruction code           Assembly                    Operation
=========================  ========================    ==================
0x.00f 0x**** 0x****       $rD  <- VALUE               Load immediate
0x20ef 0x**** 0x****       $pc   <- VALUE              Unconditional jump
0x30ef 0x**** 0x****       $tpc  <- VALUE              Load immediate to $tpc
0x80ef 0x**** 0x****       type $r0...$r7 <- VALUE     Load immediate type values [#note_immedate_types]_
0x90ef 0x**** 0x****       type $r8...$r14 <- VALUE    Load immediate type values [#note_immedate_types]_
=========================  ========================    ==================

.. note::
  There are a lot of holes in this space. Those are reserved for ISA expansion and should generate an SII exception. Such as 0x.01f:0x.0df; 0x40ef:0x70ef, 0xa0ef:0xe0ef.

.. note::
  Destination type is not changed, except of course for type load operations.

.. [#note_immedate_types]
  Types for each register are encoded in 4-bit nibbles. Lowest 4 bits determine the type of the lowest indexed register. Highest 4 bits determine the type of the highest indexed register.

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
      { "name": "FIELD_E lower 16 bits", "bits": 16 },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E upper 16 bits", "bits": 16 },
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

=========================  ========================    ==================
Instruction code           Assembly                    Operation
=========================  ========================    ==================
0x.1.f 0x**** 0x****       $rD <- FIELD_E ^ $rB        Bit-wise 'xor' [#note_logical]_
0x.2.f 0x**** 0x****       $rD <- FIELD_E | $rB        Bit-wise 'or'  [#note_logical]_
0x.3.f 0x**** 0x****       $rD <- FIELD_E & $rB        Bit-wise 'and' [#note_logical]_
0x.4.f 0x**** 0x****       $rD <- FIELD_E + $rB        Type-dependent add
0x.5.f 0x**** 0x****       $rD <- FIELD_E - $rB        Type-dependent subtract
0x.6.f 0x**** 0x****       $rD <- FIELD_E << $rB       Binary left-shift [#note_binary_shift]_
0x.7.f 0x**** 0x****       $rD <- FIELD_E >> $rB       Binary right-shift [#note_binary_shift]_
0x.8.f 0x**** 0x****       $rD <- FIELD_E >>> $rB      Arithmetic right-shift [#note_binary_shift]_
0x.9.f 0x**** 0x****       $rD <- FIELD_E * $rB        Type-dependent multiply
0x.a.f 0x**** 0x****       SII                         Reserved for future ISA expansion
0x.b.f 0x**** 0x****       SII                         Reserved for future ISA expansion
0x.c.f 0x**** 0x****       see below (stack ops)
0x.d.f 0x**** 0x****       see below (stack ops)
0x.e.f 0x**** 0x****       see below (mem ops)
=========================  ========================    ==================

.. note:: Result type is that of $rB (even for shifts). FIELD_E is assumed to have the same type as $rB

.. note::
  << and >> operations where opB is constant can be expressed by multiplies. Because of that, these operations only have one form. This does mean though, that the constant needed for certain shifts is larger than what would normally be required (i.e. 32-bit instead of 16).

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
      { "name": "FIELD_E", "bits": 16 },
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
      { "name": "FIELD_E", "bits": 16 },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |       f       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=========================  ========================    ==================
Instruction code           Assembly                    Operation
=========================  ========================    ==================
0x.0f0 0x****              $rD  <- short VALUE         Load sign-extended 16-bit immediate
0x20fe 0x****              $pc  <- short VALUE         Immediate short jump (value is sign-extended)
0x30fe 0x****              $tpc <- short VALUE         Load sign-extended value into $tpc
=========================  ========================    ==================

.. note::
  There are a lot of holes in this space. Those are reserved for ISA expansion and should generate an SII exception. Such as 0x.01f:0x.0df; 0x40ef:0xe0ef.

.. note::
  Destination type is not changed.

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
      { "name": "FIELD_E", "bits": 16 },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |       f       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=========================  =================================    ==================
Instruction code           Assembly                             Operation
=========================  =================================    ==================
0x.1f. 0x****              $rD <- short FIELD_E ^ $rA           Bit-wise 'xor' [#note_logical]_
0x.2f. 0x****              $rD <- short FIELD_E | $rA           Bit-wise 'or'  [#note_logical]_
0x.3f. 0x****              $rD <- short FIELD_E & $rA           Bit-wise 'and' [#note_logical]_
0x.4f. 0x****              $rD <- short FIELD_E + $rA           Type-dependent add
0x.5f. 0x****              $rD <- short FIELD_E - $rA           Type-dependent subtract
0x.6f. 0x****              $rD <- short $rA << FIELD_E          Binary left-shift [#note_binary_shift]_
0x.7f. 0x****              $rD <- short $rA >> FIELD_E          Binary right-shift [#note_binary_shift]_
0x.8f. 0x****              $rD <- short $rA >>> FIELD_E         Arithmetic right-shift [#note_binary_shift]_
0x.9f. 0x****              $rD <- short FIELD_E * $rA           Type-dependent multiply
0x.cf. 0x****              see below (stack ops)
0x.df. 0x****              see below (stack ops)
0x.ef. 0x****              see below (mem ops)
=========================  =================================    ==================

.. note::
  FIELD_E is assumed to be of matching scalar type for $rA. It is sign-extended to 32-bits, then replicated for each lane.

.. note::
  result type is that of $rA

.. note::
  FIELD_E is *always* sign-extended to 32-bits before applying it to the operation.

.. todo::
  We might want to zero-extend for certain operations, such as logical ops.

.. note::
  Sign-extending a 16-bit constant, then treating it as a float almost certainly don't make any sense.

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
      { "name": "FIELD_E", "bits": 16 },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |       f       |    FIELD_C    |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=========================  ===============================================    ==================
Instruction code           Assembly                                           Operation
=========================  ===============================================    ==================
0xf00. 0x****              if any $rA == 0  $pc <- $pc + unmunge(FIELD_E)
0xf01. 0x****              if any $rA != 0  $pc <- $pc + unmunge(FIELD_E)
0xf02. 0x****              if any $rA < 0   $pc <- $pc + unmunge(FIELD_E)     signed compare
0xf03. 0x****              if any $rA >= 0  $pc <- $pc + unmunge(FIELD_E)     signed compare
0xf04. 0x****              if any $rA > 0   $pc <- $pc + unmunge(FIELD_E)     signed compare
0xf05. 0x****              if any $rA <= 0  $pc <- $pc + unmunge(FIELD_E)     signed compare
0xf06. 0x****              SII
0xf07. 0x****              SII
0xf08. 0x****              if all $rA == 0  $pc <- $pc + unmunge(FIELD_E)
0xf09. 0x****              if all $rA != 0  $pc <- $pc + unmunge(FIELD_E)
0xf0a. 0x****              if all $rA < 0   $pc <- $pc + unmunge(FIELD_E)     signed compare
0xf0b. 0x****              if all $rA >= 0  $pc <- $pc + unmunge(FIELD_E)     signed compare
0xf0c. 0x****              if all $rA > 0   $pc <- $pc + unmunge(FIELD_E)     signed compare
0xf0d. 0x****              if all $rA <= 0  $pc <- $pc + unmunge(FIELD_E)     signed compare
0xf0e. 0x****              SII
=========================  ===============================================    ==================

.. note::
  For scalar types, FIELD_C MSB (inst[15]) is irrelevant; In other words, any/all selection doesn't matter

.. note:: unmunge: replicate LSB to bit positions [31:16], replace LSB with 0.

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
      { "name": "FIELD_E", "bits": 16 },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |       f       |    FIELD_C    |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=========================  =====================================================    ==================
Instruction code           Assembly                                                 Operation
=========================  =====================================================    ==================
0xf1.. 0x****              if any $rB == $rA   $pc <- $pc + unmunge(OFFSET)
0xf2.. 0x****              if any $rB != $rA   $pc <- $pc + unmunge(OFFSET)
0xf3.. 0x****              if any signed $rB < $rA  $pc <- $pc + unmunge(OFFSET)    signed compare
0xf4.. 0x****              if any signed $rB >= $rA $pc <- $pc + unmunge(OFFSET)    signed compare
0xf5.. 0x****              if any $rB < $rA    $pc <- $pc + unmunge(OFFSET)
0xf6.. 0x****              if any $rB >= $rA   $pc <- $pc + unmunge(OFFSET)
0xf7.. 0x****              SII
0xf8.. 0x****              SII
0xf9.. 0x****              if all $rB == $rA   $pc <- $pc + unmunge(OFFSET)
0xfa.. 0x****              if all $rB != $rA   $pc <- $pc + unmunge(OFFSET)
0xfb.. 0x****              if all signed $rB < $rA  $pc <- $pc + unmunge(OFFSET)    signed compare
0xfc.. 0x****              if all signed $rB >= $rA $pc <- $pc + unmunge(OFFSET)    signed compare
0xfd.. 0x****              if all $rB < $rA    $pc <- $pc + unmunge(OFFSET)
0xfe.. 0x****              if all $rB >= $rA   $pc <- $pc + unmunge(OFFSET)
=========================  =====================================================    ==================

.. note::
  For scalar types, FIELD_C MSB (inst[15]) is irrelevant; In other words, any/all selection doesn't matter

.. note::
  Comparison type is determined by type of $rA. Type of $rB is ignored and assumed to match that of $rA

.. todo::
  Maybe we can do lane-replication in case of lane-count mismatch? After all, these are using the ALUs, the same way as binary ops do...

*pseudo ops*:

* if any signed $rB >= $rA $pc <- $pc + unmunge(OFFSET)
* if any signed $rB < $rA  $pc <- $pc + unmunge(OFFSET)
* if any $rB >= $rA   $pc <- $pc + unmunge(OFFSET)
* if any $rB < $rA    $pc <- $pc + unmunge(OFFSET)
* if all signed $rB >= $rA $pc <- $pc + unmunge(OFFSET)
* if all signed $rB < $rA  $pc <- $pc + unmunge(OFFSET)
* if all $rB >= $rA   $pc <- $pc + unmunge(OFFSET)
* if all $rB < $rA    $pc <- $pc + unmunge(OFFSET)

.. note:: unmunge: replicate LSB to bit positions [31:16], replace LSB with 0.

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
      { "name": "FIELD_E", "bits": 16 },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |       f       |    FIELD_C    |       f       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=========================  =====================================================    ==================
Instruction code           Assembly                                                 Operation
=========================  =====================================================    ==================
0xf0f. 0x****              if $rA[0]  == 1 $pc <- $pc + unmunge(OFFSET)
0xf1f. 0x****              if $rA[1]  == 1 $pc <- $pc + unmunge(OFFSET)
0xf2f. 0x****              if $rA[2]  == 1 $pc <- $pc + unmunge(OFFSET)
0xf3f. 0x****              if $rA[3]  == 1 $pc <- $pc + unmunge(OFFSET)
0xf4f. 0x****              if $rA[4]  == 1 $pc <- $pc + unmunge(OFFSET)
0xf5f. 0x****              if $rA[5]  == 1 $pc <- $pc + unmunge(OFFSET)
0xf6f. 0x****              if $rA[6]  == 1 $pc <- $pc + unmunge(OFFSET)
0xf7f. 0x****              if $rA[7]  == 1 $pc <- $pc + unmunge(OFFSET)
0xf8f. 0x****              if $rA[8]  == 1 $pc <- $pc + unmunge(OFFSET)
0xf9f. 0x****              if $rA[9]  == 1 $pc <- $pc + unmunge(OFFSET)
0xfaf. 0x****              if $rA[14] == 1 $pc <- $pc + unmunge(OFFSET)
0xfbf. 0x****              if $rA[15] == 1 $pc <- $pc + unmunge(OFFSET)
0xfcf. 0x****              if $rA[16] == 1 $pc <- $pc + unmunge(OFFSET)
0xfdf. 0x****              if $rA[30] == 1 $pc <- $pc + unmunge(OFFSET)
0xfef. 0x****              if $rA[31] == 1 $pc <- $pc + unmunge(OFFSET)
=========================  =====================================================    ==================

.. note:: unmunge: replicate LSB to bit positions [31:16], replace LSB with 0.

.. note:: The type of $rA is ignored.

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
      { "name": "FIELD_E", "bits": 16 },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |       f       |    FIELD_C    |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=========================  =====================================================    ==================
Instruction code           Assembly                                                 Operation
=========================  =====================================================    ==================
0xf0.f 0x****              if $rB[0]  == 0 $pc <- $pc + unmunge(OFFSET)
0xf1.f 0x****              if $rB[1]  == 0 $pc <- $pc + unmunge(OFFSET)
0xf2.f 0x****              if $rB[2]  == 0 $pc <- $pc + unmunge(OFFSET)
0xf3.f 0x****              if $rB[3]  == 0 $pc <- $pc + unmunge(OFFSET)
0xf4.f 0x****              if $rB[4]  == 0 $pc <- $pc + unmunge(OFFSET)
0xf5.f 0x****              if $rB[5]  == 0 $pc <- $pc + unmunge(OFFSET)
0xf6.f 0x****              if $rB[6]  == 0 $pc <- $pc + unmunge(OFFSET)
0xf7.f 0x****              if $rB[7]  == 0 $pc <- $pc + unmunge(OFFSET)
0xf8.f 0x****              if $rB[8]  == 0 $pc <- $pc + unmunge(OFFSET)
0xf9.f 0x****              if $rB[9]  == 0 $pc <- $pc + unmunge(OFFSET)
0xfa.f 0x****              if $rB[14] == 0 $pc <- $pc + unmunge(OFFSET)
0xfb.f 0x****              if $rB[15] == 0 $pc <- $pc + unmunge(OFFSET)
0xfc.f 0x****              if $rB[16] == 0 $pc <- $pc + unmunge(OFFSET)
0xfd.f 0x****              if $rB[30] == 0 $pc <- $pc + unmunge(OFFSET)
0xfe.f 0x****              if $rB[31] == 0 $pc <- $pc + unmunge(OFFSET)
=========================  =====================================================    ==================

.. note:: unmunge: replicate LSB to bit positions [31:16], replace LSB with 0.

.. note:: The type of $rA is ignored.

Stack group
-----------

While stack operations (as in push/pull) are not supported by the ISA, special load/store instructions are provided with small offsets and $r12 ($fp) and $r13 ($sp) as the base register to support compact form of common stack-load and store- operations. The supported offset range us -256 to +252 bytes.

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "S",         "bits": 1, attr: "$rS" },
      { "name": "OFS",       "bits": 7, attr: "offset" },
      { "name": "FIELD_C",   "bits": 4, attr: "op kind" },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |            OFS            | S |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

==================  ============================    ==================
Instruction code    Assembly                        Operation
==================  ============================    ==================
0x.c**              MEM[$rS + tiny OFS*4] <- $rD    Store $rD in memory
0x.d**              $rD <- MEM[$rS + tiny OFS*4]    Load $rD from memory
==================  ============================    ==================

.. warning::
  The encoding of field S is special: A=0 denotes $r12, A=1 denotes $r13

.. note::
  the existence of these ops complicate memory op decode as well as operation size decode, but save a *huge* amount of code-space, allowing almost all register spills and fills to be done in two bytes.

Indirect type load/store group
------------------------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "offset" },
      { "name": "FIELD_B",   "bits": 4, attr: "op kind" },
      { "name": "e",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

==================  =======================================    ==================
Instruction code    Assembly                                   Operation
==================  =======================================    ==================
0x.e0.              type $r0...$r7  <- MEM[$rD + FIELD_A*4]
0x.e1.              type $r8...$r14 <- MEM[$rD + FIELD_A*4]
0x.e2.              MEM[$rD + FIELD_A*4] <- type $r0...$r7
0x.e3.              MEM[$rD + FIELD_A*4] <- type $r8...$r14
==================  =======================================    ==================

.. note::
  FIELD_A is ones-complement coded

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

==================  ============================    ==================
Instruction code    Assembly                        Operation
==================  ============================    ==================
0x.e4.              $rD <- MEM8[$rA]                8-bit unsigned load from MEM[$rA] into $rD
0x.e5.              $rD <- MEM16[$rA]               16-bit unsigned load from MEM[$rA] into $rD
0x.e6.              $rD <- MEM[32][$rA]             32-bit load from MEM[$rA] into $rD
0x.e7.              $rD <- MEMLL[32][$rA]           32-bit unsigned load-lock (exclusive load)
0x.e8.              MEM8[$rA] <- $rD                8-bit store to MEM[$rA] from $rD
0x.e9.              MEM16[$rA] <- $rD               16-bit store to MEM[$rA] from $rD
0x.ea.              MEM[32][$rA] <- $rD             32-bit store to MEM[$rA] from $rD
0x.eb.              MEMSC[32][$rA] <- $rD           32-bit store-conditional (exclusive store)
0x.ec.              $rD <- SMEM8[$rA]               8-bit signed load from MEM[$rA] into $rD
0x.ed.              $rD <- SMEM16[$rA]              16-bit signed load from MEM[$rA] into $rD
==================  ============================    ==================

.. note::
  Loads don't change the type of their destination register.


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

==================  ============================    ==================
Instruction code    Assembly                        Operation
==================  ============================    ==================
0x1ee.              INV[32][$rA]                    invalidate cache line for address $rA
0x2ee.              $pc <- MEM[32][$rA]             32-bit load from MEM[$rA] into $PC
0x3ee.              $tpc <- MEM[32][$rA]            32-bit load from MEM[$rA] into $TPC
==================  ============================    ==================

.. note::
  Cache invalidation applies to all caches and to all levels of caches: L1D L1I; L2, if exists. System-level caches (L3) are not invalidated. In a multi-processor system, only local caches (caches that are in the path-to-memory for the core executing the instruction) are invalidated.


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
      { "name": "FIELD_E", "bits": 16 },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       f       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

==================  ====================================    ==================
Instruction code    Assembly                                Operation
==================  ====================================    ==================
0x.f4. 0x****       $rD <- MEM8[$rA+FIELD_E]                8-bit unsigned load from MEM[$rA+FIELD_E] into $rD
0x.f5. 0x****       $rD <- MEM16[$rA+FIELD_E]               16-bit unsigned load from MEM[$rA+FIELD_E] into $rD
0x.f6. 0x****       $rD <- MEM[32][$rA+FIELD_E]             32-bit load from MEM[$rA+FIELD_E] into $rD
0x.f7. 0x****       $rD <- MEMLL[32][$rA+FIELD_E]           32-bit unsigned load-lock (exclusive load)
0x.f8. 0x****       MEM8[$rA+FIELD_E] <- $rD                8-bit store to MEM[$rA+FIELD_E] from $rD
0x.f9. 0x****       MEM16[$rA+FIELD_E] <- $rD               16-bit store to MEM[$rA+FIELD_E] from $rD
0x.fa. 0x****       MEM[32][$rA+FIELD_E] <- $rD             32-bit store to MEM[$rA+FIELD_E] from $rD
0x.fb. 0x****       MEMSC[32][$rA+FIELD_E] <- $rD           32-bit store-conditional (exclusive store)
0x.fc. 0x****       $rD <- SMEM8[$rA+FIELD_E]               8-bit signed load from MEM[$rA+FIELD_E] into $rD
0x.fd. 0x****       $rD <- SMEM16[$rA+FIELD_E]              16-bit signed load from MEM[$rA+FIELD_E] into $rD
==================  ====================================    ==================

.. note:: FIELD_E is sign-extended before addition
.. note:: Loads don't change the type of a register.

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
      { "name": "FIELD_E", "bits": 16 },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       f       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

==================  ====================================    ==================
Instruction code    Assembly                                Operation
==================  ====================================    ==================
0x1fe. 0x****       INV[32][$rA+FIELD_E]                    invalidate cache line for address $rA+FIELD_E
0x2fe. 0x****       $pc <- MEM[32][$rA+FIELD_E]             32-bit load from MEM[$rA+FIELD_E] into $PC
0x3fe. 0x****       $tpc <- MEM[32][$rA+FIELD_E]            32-bit load from MEM[$rA+FIELD_E] into $TPC
==================  ====================================    ==================

.. note::
  Cache invalidation applies to all caches and to all levels of caches: L1D L1I; L2, if exists. System-level caches (L3) are not invalidated. In a multi-processor system, only local caches (caches that are in the path-to-memory for the core executing the instruction) are invalidated.

.. note:: FIELD_E is sign-extended before addition

Load/store multiple
-------------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "offset" },
      { "name": "FIELD_B",   "bits": 4, attr: "op kind" },
      { "name": "f",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16 },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       f       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

==================  =======================================    ==================
Instruction code    Assembly                                   Operation
==================  =======================================    ==================
0x.f0. 0x****       $r0...$r14 <- MEM[$rD] @ $rA               load any combination of registers with FIELD_E as mask; skip-mask in $rA
0x.f1. 0x****       MEM[$rD] <- $r0...$r14 @ $rA               store any combination of registers with FIELD_E as mask; skip-mask in $rA
0x.f2. 0x****       $r0...$r14 <- POP[$rD] @ $rA               pop any combination of registers with FIELD_E as mask; skip-mask in $rA
0x.f3. 0x****       PUSH[$rD] <- $r0...$r14 @ $rA              push any combination of registers with FIELD_E as mask; skip-mask in $rA
0x.f0f 0x****       $r0...$r14 <- MEM[$rD]                     load any combination of registers with FIELD_E as mask
0x.f1f 0x****       MEM[$rD] <- $r0...$r14                     store any combination of registers with FIELD_E as mask
0x.f2f 0x****       $r0...$r14 <- POP[$rD]                     pop any combination of registers with FIELD_E as mask
0x.f3f 0x****       PUSH[$rD] <- $r0...$r14                    push any combination of registers with FIELD_E as mask
==================  =======================================    ==================

.. note::
  0x.f0f decodes to the wrong FIELD_E size. Otherwise, this is not a bad encoding.

.. note::
  $rA is used as a 'skip' mask. If FIELD_A is 0xf, no skip mask is used

**These are very complex instructions.**

This is a multi-cycle instruction. For store instructions, the memory address is incremented/decremented for every register that's marked for storage. After that, the type info is stored for every register that's marked for type storage. If no register is marked for type storage in the $r0...$r7 region, the first type WORD is not stored. If no register is marked for type storage in the $r8...$r14 region, the second type WORD is not stored. Otherwise, skipped types are replaced by 0xf.

For load instructions, the reverse happens: for every marked load, the address is (post) incremented/decremented after loading. Types are loaded as needed (skipping type WORDs if none of the corresponding types are marked for load). Individual types are not updated if their associated field is 0xf upon load.

For a load multiple where the base register is marked for load, the implementation must ensure that the new register value only takes effect after the operation fully completes.

*Exception behavior*: If a exception (due to access violation during memory access) is raised, $tpc points to the load/store multiple instruction. It however is generally not guaranteed that no loads or stores have been performed. Consequently, some of the side-effects might have already taken place and the exception handler is in no position to know which ones. It is however safe to assume that the operation can be retried, as long as the following conditions are met:

* Address translation after the retry generates the same physical addresses for store multiple operations
* The target address is in regular memory as opposed to I/O or CSR space

The requirement to be able to retry means that if the base register is part of the set of registers to be loaded, it's value can only change after it is determined that no more exceptions can fire. This can be achieved by loading the base register last (i.e. not loading registers in order), or load the value into a temporary storage and update the base register as the last step.

.. note::

  Implementing these instructions is complicated. It requires some sort of sequencer in the pipeline and breaks the basic construct of a RISC ISA. It also complicates exception handling.

.. todo::

  These instructions are not supported by the toolset, or Espresso.


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
      { "name": "FIELD_E lower 16 bits", "bits": 16 },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E upper 16 bits", "bits": 16 },
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

=========================  ==========================  ==================
Instruction code           Assembly                    Operation
=========================  ==========================  ==================
0x.f4f 0x**** 0x****       $rD <- MEM8[FIELD_E]        8-bit unsigned load from MEM[FIELD_E] into $rD
0x.f5f 0x**** 0x****       $rD <- MEM16[FIELD_E]       16-bit unsigned load from MEM[FIELD_E] into $rD
0x.f6f 0x**** 0x****       $rD <- MEM[32][FIELD_E]     32-bit load from MEM[FIELD_E] into $rD
0x.f7f 0x**** 0x****       $rD <- MEMLL[32][FIELD_E]   32-bit unsigned load-lock (exclusive load)
0x.f8f 0x**** 0x****       MEM8[FIELD_E] <- $rD        8-bit store to MEM[FIELD_E] from $rD
0x.f9f 0x**** 0x****       MEM16[FIELD_E] <- $rD       16-bit store to MEM[FIELD_E] from $rD
0x.faf 0x**** 0x****       MEM[32][FIELD_E] <- $rD     32-bit store to MEM[FIELD_E] from $rD
0x.fbf 0x**** 0x****       MEMSC[32][FIELD_E] <- $rD   32-bit store-conditional (exclusive store)
0x.fcf 0x**** 0x****       $rD <- SMEM8[FIELD_E]       8-bit signed load from MEM[FIELD_E] into $rD
0x.fdf 0x**** 0x****       $rD <- SMEM16[FIELD_E]      16-bit signed load from MEM[FIELD_E] into $rD
=========================  ==========================  ==================

.. note:: Loads don't change the type of a register.

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
      { "name": "FIELD_E lower 16 bits", "bits": 16 },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E upper 16 bits", "bits": 16 },
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

=========================  ========================    ==================
Instruction code           Assembly                    Operation
=========================  ========================    ==================
0x1fef 0x**** 0x****       INV[32][FIELD_E]            invalidate cache line for address FIELD_E
0x2fef 0x**** 0x****       $pc <- MEM[32][FIELD_E]     32-bit load from MEM[FIELD_E] into $PC
0x3fef 0x**** 0x****       $tpc <- MEM[32][FIELD_E]    32-bit load from MEM[FIELD_E] into $TPC
=========================  ========================    ==================

.. note::
  Cache invalidation applies to all caches and to all levels of caches: L1D L1I; L2, if exists. System-level caches (L3) are not invalidated. In a multi-processor system, only local caches (caches that are in the path-to-memory for the core executing the instruction) are invalidated.

Extension groups
----------------

Extension groups allow for extending the instruction set by utilizing otherwise unused portions of the 16-bit instruction code-space, followed by a second 16-bit instruction code. These extension groups allow for expressing seldom used or specialized instructions while not impacting the compactness of the base ISA.

Zero compare lane predication group
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "f",         "bits": 4 },
      { "name": "0",         "bits": 4 },
      { "name": "f",         "bits": 4 },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "$rD" },
      { "name": "0",         "bits": 4 },
      { "name": "FIELD_C",   "bits": 4, attr: "op kind" },
      { "name": "FIELD_D",   "bits": 4, attr: "$rA" },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |       f       |       0       |       f       |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=========================  ========================    ==================
Instruction code           Assembly                    Operation
=========================  ========================    ==================
0xf0ff 0x.00.              $rD <- $rA == 0
0xf0ff 0x.01.              $rD <- $rA != 0
0xf0ff 0x.02.              $rD <- $rA < 0                signed compare
0xf0ff 0x.03.              $rD <- $rA >= 0               signed compare
0xf0ff 0x.04.              $rD <- $rA > 0                signed compare
0xf0ff 0x.05.              $rD <- $rA <= 0               signed compare
=========================  ========================    ==================

These instructions perform lane-wise comparisons of the prescribed type. The result (0 for FALSE, 1 for TRUE) is replicated across the length of each lane (8- 16- or 32-times) and placed in the destination register.

.. todo:: Extension group encoding changed. Toolset needs updating.

Lane predication group
~~~~~~~~~~~~~~~~~~~~~~

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "f",         "bits": 4 },
      { "name": "0",         "bits": 4 },
      { "name": "f",         "bits": 4 },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "$rA" },
      { "name": "FIELD_B",   "bits": 4, attr: "$rB" },
      { "name": "FIELD_C",   "bits": 4, attr: "op kind" },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |       f       |       0       |       f       |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=========================  ========================    ==================
Instruction code           Assembly                    Operation
=========================  ========================    ==================
0xf0ff 0x.1..              $rD <- $rB == $rA
0xf0ff 0x.2..              $rD <- $rB != $rA
0xf0ff 0x.3..              $rD <- signed $rB < $rA       signed compare
0xf0ff 0x.4..              $rD <- signed $rB >= $rA      signed compare
0xf0ff 0x.5..              $rD <- $rB < $rA
0xf0ff 0x.6..              $rD <- $rB >= $rA
=========================  ========================    ==================

These instructions perform lane-wise comparisons of the prescribed type. The result (0 for FALSE, 1 for TRUE) is replicated across the length of each lane (8- 16- or 32-times) and placed in the destination register.

.. todo:: Extension group encoding changed. Toolset needs updating.

Special vector operation group
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "f",         "bits": 4 },
      { "name": "1",         "bits": 4 },
      { "name": "f",         "bits": 4 },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "op kind" },
      { "name": "FIELD_B",   "bits": 4, attr: "0" },
      { "name": "FIELD_C",   "bits": 4, attr: "0" },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |       f       |       1       |       f       |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       0       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=========================  ============================  ==================
Instruction code           Assembly                      Operation
=========================  ============================  ==================
0xf1ff 0x.001              $rD <- vstat                  Store vector status (vstart and vend) in $rD
0xf1ff 0x.002              vstat <- $rD                  Load vector status (vstart and vend) from $rD
=========================  ============================  ==================

Unary vector operation group
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "f",         "bits": 4 },
      { "name": "1",         "bits": 4 },
      { "name": "f",         "bits": 4 },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "$rA" },
      { "name": "FIELD_B",   "bits": 4, attr: "op kind" },
      { "name": "FIELD_C",   "bits": 4, attr: "0" },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |       f       |       1       |       f       |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=========================  ============================  ==================
Instruction code           Assembly                      Operation
=========================  ============================  ==================
0xf1ff 0x.01.              $rD <- sum $rA                Reduction sum
0xf1ff 0x.02.              $rD <- SET_VEND $rA           Load VEND register and return it's value based on $rA
0xf1ff 0x.03.              $rD <- (cast TYPE_B)$rA       Element-wise type-cast $rA to TYPE_B
0xf1ff 0x.04.              $rD <- compress $rA & $rB     Element-wise compressed selection of $rA, $rB being the selector
=========================  ============================  ==================

Binary vector operation group
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "f",         "bits": 4 },
      { "name": "1",         "bits": 4 },
      { "name": "f",         "bits": 4 },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "$rA" },
      { "name": "FIELD_B",   "bits": 4, attr: "$rB" },
      { "name": "FIELD_C",   "bits": 4, attr: "op kind" },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |       f       |       1       |       f       |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=========================  ============================  ==================
Instruction code           Assembly                      Operation
=========================  ============================  ==================
0xf1ff 0x.1..              $rD <- interpolate $rA, $rB   [#note_interpolation]_
0xf1ff 0x.2..              $rD(i) <- $rA($rB(i))         [#note_lane_swizzle]_
0xf1ff 0x.3..              $rD <- (cast TYPE_B)$rA       Element-wise type-cast $rA to TYPE_B
0xf1ff 0x.4..              $rD <- compress $rA & $rB     Element-wise compressed selection of $rA, $rB being the selector
0xf1ff 0x.5..              $rD <- $rB + sum $rA          Reduction sum-accumulate
=========================  ============================  ==================

.. [#note_interpolation]
  This instruction performs linear interpolation between adjacent lanes of $rA using the value of $rB as the interpolator.

  If $rB is of an integral type, it is assumed to be a fractional value between 0 and 1. If it's a floating-point type, its value must be between 0.0 and 1.0.

  If the value of $rB is not within the requisite range, the outcome of the operation is implementation-defined.

  If $rB is a scalar type, it's broadcast to all lanes. If $rB is a vector type, its value is used lane-wise::

    $rD(i*2+0) <- $rA(i*2+0) *    $rB(i*2+0)  + $rA(i*2+1) *    $rB(i*2+1)
    $rD(i*2+1) <- $rA(i*2+0) * (1-$rB(i*2+0)) + $rA(i*2+1) * (1-$rB(i*2+1))

  .. todo:: Extension group encoding changed. Toolset needs updating.

  .. todo:: Do we really want to support this for floating-point types? There are a boat-load of multiplies here!

.. [#note_lane_swizzle]
  Each lane of $rD is set to the lane of $rA referenced by the corresponding lane of $rB.

  .. todo:: Original lane-swizzle:
    0x.af. 0x****              $rD <- lane_swizzle $rA, VALUE
    got removed. Toolset needs updating.


.. todo:: reduction sum used to be in the unary group and, well, used to be unary. Need to update toolset.


Scaled multiply group
~~~~~~~~~~~~~~~~~~~~~


.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "f",         "bits": 4 },
      { "name": "FLD_F",     "bits": 2, attr: "shift" },
      { "name": "FLD_O",     "bits": 2, attr: "op kind" },
      { "name": "f",         "bits": 4 },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "$rA" },
      { "name": "FIELD_B",   "bits": 4, attr: "$rB" },
      { "name": "FIELD_C",   "bits": 4, attr: "shift" },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |       f       |  OP   | FLD_F |       f       |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=========================  =========================================  ==================
Instruction code           Assembly                                   Operation
=========================  =========================================  ==================
0xf4ff 0x.*..              $rD <- full $rA * $rB >>> FIELD_C + 0
0xf5ff 0x.*..              $rD <- full $rA * $rB >>> FIELD_C + 8
0xf6ff 0x.*..              $rD <- full $rA * $rB >>> FIELD_C + 16
0xf7ff 0x.*..              $rD <- full $rA * $rB >>> FIELD_C + 32
0xf8ff 0x.*..              $rD <- full $rA * $rB >> FIELD_C + 0
0xf9ff 0x.*..              $rD <- full $rA * $rB >> FIELD_C + 8
0xfaff 0x.*..              $rD <- full $rA * $rB >> FIELD_C + 16
0xfbff 0x.*..              $rD <- full $rA * $rB >> FIELD_C + 32
=========================  =========================================  ==================

.. todo::
  This is not how BINUTILS is coded up at the moment. We need to follow-up with the changes there.

.. todo:: Extension group encoding changed. Toolset needs updating.


Prefix instructions
-------------------

Prefix instructions can precede any other instruction to modify their behavior.

.. note::
  *Exception behavior*: If a prefixed instruction throws an exception, $tpc points to the (first) prefix instruction after entering SCHEDULER mode. This allows the recovery code to decode and potentially retry the excepted instruction.

.. note::
  *Interrupt behavior*: If an interrupt is handled during the execution of a prefixed instruction, $tpc points to the (first) prefix instruction after entering SCHEDULER mode. None of the side-effects of the prefixed instruction take effect. If any of the side-effects of the prefixed instruction have taken effect, the whole instruction must be carried to completion and $tpc points to the subsequent instruction after entering SCHEDULER mode. In other words, under no circumstances can $tpc point anywhere between the first prefix and it's corresponding instruction when entering SCHEDULER mode.

.. note::
  *Prefix concatenation*: Every processor implementation has a maximum instruction length it supports. In this version of the spec, it's 64 bits. If an instruction with all its prefixes exceeds this limit, the processor raises an invalid instruction exception, with $tpc pointing to the first prefix instruction. Without this provision it would be possible to create arbitrarily long instruction sequences in TASK mode. That in turn would prevent interrupts from being raised, effectively locking up the system (at least up to the point of exhausting the addressable RAM space). The ISA puts further restrictions on what prefix instructions can be cascaded. As a general rule, prefixes of the same kind can appear only once in a prefix cascade.

Type override
~~~~~~~~~~~~~

This prefix instruction allows for the changing the way the subsequent operation interprets source operand types. It doesn't actually change the source register types. It also allows for explicit control of whether the destination type is written or not.

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

=========================  ===========================================================
Instruction code           Operation
=========================  ===========================================================
0xff** ...                 Type override for $rA (TYPE_A) and $rB (TYPE_B).
=========================  ===========================================================

Type override for $rA (TYPE_A) and $rB (TYPE_B).

If either TYPE_A or TYPE_B is set to 0xf, the corresponding register type is not overridden: the type from the register file is used during the subsequent operation.
