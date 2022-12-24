Instruction Decode
==================

Instruction encoding is such that the following condition can be used to determine if the FIELD_E is needed::

  FIELD_D == 0xf ||
  (FIELD_C == 0xf && (FIELD_B != 0xf || FIELD_A == 0xf)) ||
  (FIELD_C == 0xe && FIELD_A == 0xf) ||
  (FIELD_C < 0xc && (FIELD_B == 0xf || FIELD_A == 0xf))

The size of FIELD_E == 16 if::

  FIELD_D == 0xf || FIELD_A != 0xf

.. _branch_id_expression:

Branch Identification
=====================

Branches can be identified by::

  (FIELD_D == 0xf         && FIELD_C != 0xf) ||
  ((FIELD_D & 0xe) == 0x2 && (FIELD_C & 0xe) == 0xe && FIELD_B == 0xe) ||
  ((FIELD_D & 0xe) == 0x2 && FIELD_C == 0x0         && (FIELD_B & 0xe) == 0xe && (FIELD_A & 0xe) == 0xe && (FIELD_B & 1 != FIELD_A & 1) ||
  (FIELD_C == 0x0         && FIELD_B == 0x0         && (FIELD_A & 0xe) == 0x2) ||
  ((FIELD_D & 0x8) == 0   && FIELD_C == 0x0         && FIELD_B == 0x0         && FIELD_A == 0x0) <-- SWI insn.
  (FIELD_D == 0x8         && FIELD_C == 0x0         && FIELD_B == 0x0         && FIELD_A == 0x0) <-- STM insn.

If timing closure is an issue, the expression can be simplified by removing the last two terms, in effect not predicting SWI and STM instructions.

Term 3 has an XOR at the very end (:code:`(FIELD_B & 1 != FIELD_A & 1)`). This term can be decomposed into 2 4-term AND expressions, for essentially 0x20ef and 0x20fe (plus the 0x3... variants).

In essense the critical path is a 16-bit compare followed by 6-way OR (for the full term) or a 15-bit compare followed by a 4-way OR (for the simplified term).

.. note::
  It seems that Cyclone V has 5-bit LUTs. That would mean that one can compare 15 bits in 3 LUTs. Then, another LUT could be used to generate a single output bit. That is to say a 15-bit comparator has a 2-LUT latency. The 4 comparators can be combined in a
  single LUT, so the total decode latency is 3 LUTs. Since we have unused inputs on some of the LUTs, it's possible that the full term can also be decoded in that much latency.

  After simulation, the data latency seems to be a little over 5.5ns in the fastest Cyclone V parts. That would put us just below 200MHz clock frequency. However, due to clock also getting delayed we actually get a significant boost and timing closes all the way up to 421MHz. Technology viewer confirms the use of 3 layers of LUTs, which wold give us roughly 1.83ns delay per LUT, or 545MHz f_max.
