Instruction Encodings
=====================

There are three encoding variants:

16-bit instructions::

    +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
    |    FIELD_D    |    FIELD_C    |    FIELD_B    |    FIELD_A    |
    +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

32-bit instructions::

    +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
    |    FIELD_D    |    FIELD_C    |    FIELD_B    |    FIELD_A    |
    +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

    +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
    |                         FIELD_E                               |
    +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

48-bit instructions::

    +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
    |    FIELD_D    |    FIELD_C    |    FIELD_B    |    FIELD_A    |
    +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

    +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...
    |                         FIELD_E  lower 16 bits              ...
    +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...

    ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
    ...                       FIELD_E   upper 16 bits               |
    ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

FIELD_D:
  normally contains the register index of the [D]estination
FIELD_C:
  normally contains the instruction op-[C]ode
FIELD_B:
  normally contains the register index of the second operand (operand [B])
FIELD_A:
  normally contains the register index of the first operand (operand [A])
FIELD_E:
  normally contains an immediate or a memory offset

There are slight variations of these encodings, which are called out with the individual instructions.

Each 4-bit field from FIELD_A through FIELD_D normally has special meaning if their value is 0xf.