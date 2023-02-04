#!/usr/bin/python3
from random import *
from typing import *
from copy import copy

try:
    from silicon import *
except ImportError:
    import sys
    from pathlib import Path
    sys.path.append(str((Path() / ".." / ".." / ".." / "silicon").absolute()))
    from silicon import *

try:
    from .brew_types import *
    from .brew_utils import *
except ImportError:
    from brew_types import *
    from brew_utils import *

"""
Decode logic

We have two read ports to the register-file, plus one to reserve our output register.

The score-board and forwarding logic is all in the register-file, so we don't have
to be bothered with that. All we cared are the 'response' signals that let us
know that our requests are honored.

We can repeat reads as many times as needed, there's no state-change associated with them.

We have to be a bit careful with the result reservation though: while it's fine to reserve
the same register in multiple cycles, it's response will only come once. So we'll have to
remember that...

In general we're ready to execute an instruction, when read1, read2 got a response (if we cared),
when we've seen or see a rsv_response (if we cared) and when exec is ready to accept the next instruction.

In case of a fetch AV, we pretend to get an instruction with no dependencies and no reservation and push it as
the next instruction. We can't use any of the instruction fields: they're invalid, or may contain some
sensitive info that could be gleaned from stall-counting or something.

HW interrupts are dealt with in the execute stage.
"""

# TODO: we can't easily support indirect jumps in the V1 pipeline:
#       we determine branch and branch target in execute, yet memory is after it.
#       So, loading $pc or $tpc directly from memory is not supported. It must be
#       done through a general-purpose register in two instructions. This makes
#       subroutine epilogs longer and method calls more painful. Also, GCC will
#       need to be aware of this...

class DecodeStage(Module):
    clk = ClkPort()
    rst = RstPort()

    fetch = Input(FetchDecodeIf)
    exec = Output(DecodeExecIf)

    # Interface to the register file
    reg_file_req = Output(RegFileReadRequestIf)
    reg_file_rsp = Input(RegFileReadResponseIf)

    def body(self):
        field_d = self.fetch.inst_0[15:12]
        field_c = self.fetch.inst_0[11:8]
        field_b = self.fetch.inst_0[7:4]
        field_a = self.fetch.inst_0[3:0]
        field_e = Select(
            self.fetch.inst_len == 2, # 48-bit instructions
            concat(
                self.fetch.inst_1[15], self.fetch.inst_1[15], self.fetch.inst_1[15], self.fetch.inst_1[15],
                self.fetch.inst_1[15], self.fetch.inst_1[15], self.fetch.inst_1[15], self.fetch.inst_1[15],
                self.fetch.inst_1[15], self.fetch.inst_1[15], self.fetch.inst_1[15], self.fetch.inst_1[15],
                self.fetch.inst_1[15], self.fetch.inst_1[15], self.fetch.inst_1[15], self.fetch.inst_1[15],
                self.fetch.inst_1
            ),
            concat(self.fetch.inst_2, self.fetch.inst_1)
        )

        field_a_is_f = field_a == 0xf
        field_b_is_f = field_b == 0xf
        field_c_is_f = field_c == 0xf
        field_d_is_f = field_d == 0xf

        tiny_ofs = concat(self.fetch.inst_0[7:1], "2'b0")
        tiny_field_a = self.fetch.inst_0[0]

        field_a_plus_one = Wire()
        field_a_plus_one <<= (field_a+1)[3:0]
        ones_field_a = Select(
            field_a[3],
            field_a,
            concat(
                field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3],
                field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3],
                field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3],
                field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3],
                field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3],
                field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3],
                field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3], field_a_plus_one[3],
                field_a_plus_one
            )
        )

        # Codes: 0123456789abcde <- exact match to that digit
        #        . <- anything but 0xf
        #        * <- anything, including 0xf
        #        < <- less then subsequent digit
        #        > <- greater than subsequent digit (but not 0xf)
        #        : <- anything after that is comment
        inst_table = (
            #  CODE                                  EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( "<8000: SWI",                          exec.misc,     op.misc_swi,    None,       None,           None,      field_a,         None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " 8000: STM",                          exec.misc,     op.misc_stm,    None,       None,           None,      None,            None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " 9000: WOI",                          exec.cbranch,  op.cb_eq,       field_a,    field_b,        None,      "REG",           "REG",        0,          None,   0,     0,     0,  0,  0,  0 ), # Decoded as 'if $0 == $0 $pc <- $pc'
            ( ">9000: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .001: FENCE",                        exec.bitwise,  op.b_or,        None,       None,           None,      None,            None,         None,       None,   0,     0,     0,  0,  0,  0 ), # Decoded as a kind of NOP
            ( " .002: $pc <- $rD",                   exec.misc,     op.misc_pc_w_r, field_d,    None,           None,      "REG",           None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .003: $tpc <- $rD",                  exec.misc,     op.misc_tpc_w_r,field_d,    None,           None,      "REG",           None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .004: $rD <- $pc",                   exec.misc,     op.misc_pc_r,   None,       None,           field_d,   None,            None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .005: $rD <- $tpc",                  exec.misc,     op.misc_tpc_r,  None,       None,           field_d,   None,            None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .00>5: SII",                         exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            # Unary group                            EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " .01.: $rD <- tiny FIELD_A",          exec.bitwise,  op.b_or,        None,       None,           field_d,   ones_field_a,    0,            None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .02.: $rD <- $pc + FIELD_A*2",       exec.adder,    op.pc_add,      None,       None,           field_d,   ones_field_a,    0,            None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .03.: $rD <- -$rA",                  exec.adder,    op.b_sub_a,     field_a,    None,           field_d,   "REG",           0,            None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .04.: $rD <- ~$rA",                  exec.bitwise,  op.b_xor,       field_a,    None,           field_d,   "REG",           0xffffffff,   None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .05.: $rD <- bse $rA",               exec.bitwise,  op.b_or,        field_a,    None,           field_d,   "REG",           0,            None,       None,   0,     0,     1,  0,  0,  0 ),
            ( " .06.: $rD <- wse $rA",               exec.bitwise,  op.b_or,        field_a,    None,           field_d,   "REG",           0,            None,       None,   0,     0,     0,  1,  0,  0 ),
            ( " .0>6.: SII",                         exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ## Binary ALU group                       EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " .1..: $rD <- $rA ^ $rB",             exec.bitwise,  op.b_xor,       field_a,    field_b,        field_d,   "REG",           "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .2..: $rD <- $rA | $rB",             exec.bitwise,  op.b_or,        field_a,    field_b,        field_d,   "REG",           "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .3..: $rD <- $rA & $rB",             exec.bitwise,  op.b_and,       field_a,    field_b,        field_d,   "REG",           "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .4..: $rD <- $rA + $rB",             exec.adder,    op.add,         field_a,    field_b,        field_d,   "REG",           "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .5..: $rD <- $rA - $rB",             exec.adder,    op.a_sub_b,     field_a,    field_b,        field_d,   "REG",           "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .6..: $rD <- $rA << $rB",            exec.shift,    op.shll,        field_a,    field_b,        field_d,   "REG",           "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .7..: $rD <- $rA >> $rB",            exec.shift,    op.shlr,        field_a,    field_b,        field_d,   "REG",           "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .8..: $rD <- $rA >>> $rB",           exec.shift,    op.shar,        field_a,    field_b,        field_d,   "REG",           "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .9..: $rD <- $rA * $rB",             exec.mult,     None,           field_a,    field_b,        field_d,   "REG",           "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .a..: $rD <- ~$rA & $rB",            exec.bitwise,  op.b_nand,      field_a,    field_b,        field_d,   "REG",           "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .b..: $rD <- tiny $rB + FIELD_A",    exec.adder,    op.add,         None,       field_b,        field_d,   ones_field_a,    "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            # Load immediate group                   EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " .00f: $rD <- VALUE",                 exec.bitwise,  op.b_or,        None,       None,           field_d,    field_e,        0,            None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " 20ef: $pc <- VALUE",                 exec.misc,     op.misc_pc_w,   None,       None,           None,       None,           None,         field_e,    None,   0,     0,     0,  0,  0,  0 ),
            ( " 30ef: $tpc <- VALUE",                exec.misc,     op.misc_tpc_w,  None,       None,           None,       None,           None,         field_e,    None,   0,     0,     0,  0,  0,  0 ),
            ( " 80ef.: SII",                         exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " 90ef.: SII",                         exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            # Constant ALU grou  p                   EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " .1.f: $rD <- FIELD_E ^ $rB",         exec.bitwise,  op.b_xor,       None,       field_b,        field_d,   field_e,         "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .2.f: $rD <- FIELD_E | $rB",         exec.bitwise,  op.b_or,        None,       field_b,        field_d,   field_e,         "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .3.f: $rD <- FIELD_E & $rB",         exec.bitwise,  op.b_and,       None,       field_b,        field_d,   field_e,         "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .4.f: $rD <- FIELD_E + $rB",         exec.adder,    op.add,         None,       field_b,        field_d,   field_e,         "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .5.f: $rD <- FIELD_E - $rB",         exec.adder,    op.a_sub_b,     None,       field_b,        field_d,   field_e,         "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .6.f: $rD <- FIELD_E << $rB",        exec.shift,    op.shll,        None,       field_b,        field_d,   field_e,         "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .7.f: $rD <- FIELD_E >> $rB",        exec.shift,    op.shlr,        None,       field_b,        field_d,   field_e,         "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .8.f: $rD <- FIELD_E >>> $rB",       exec.shift,    op.shar,        None,       field_b,        field_d,   field_e,         "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .9.f: $rD <- FIELD_E * $rB",         exec.mult,     None,           None,       field_b,        field_d,   field_e,         "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .a.f: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .b.f: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            # Short load immediate group             EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " .0f0: $rD <- short VALUE",           exec.bitwise,  op.b_or,        None,       None,           field_d,    field_e,        0,            None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " 20fe: $pc <- short VALUE",           exec.misc,     op.misc_pc_w,   None,       None,           None,       None,           None,         field_e,    None,   0,     0,     0,  0,  0,  0 ),
            ( " 30fe: $tpc <- short VALUE",          exec.misc,     op.misc_tpc_w,  None,       None,           None,       None,           None,         field_e,    None,   0,     0,     0,  0,  0,  0 ),
            # Short constant ALU group               EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " .1f.: $rD <- FIELD_E ^ $rA",         exec.bitwise,  op.b_xor,       None,       field_a,        field_d,   field_e,         "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .2f.: $rD <- FIELD_E | $rA",         exec.bitwise,  op.b_or,        None,       field_a,        field_d,   field_e,         "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .3f.: $rD <- FIELD_E & $rA",         exec.bitwise,  op.b_and,       None,       field_a,        field_d,   field_e,         "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .4f.: $rD <- FIELD_E + $rA",         exec.adder,    op.add,         None,       field_a,        field_d,   field_e,         "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .5f.: $rD <- FIELD_E - $rA",         exec.adder,    op.a_sub_b,     None,       field_a,        field_d,   field_e,         "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .6f.: $rD <- FIELD_E << $rA",        exec.shift,    op.shll,        None,       field_a,        field_d,   field_e,         "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .7f.: $rD <- FIELD_E >> $rA",        exec.shift,    op.shlr,        None,       field_a,        field_d,   field_e,         "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .8f.: $rD <- FIELD_E >>> $rA",       exec.shift,    op.shar,        None,       field_a,        field_d,   field_e,         "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .9f.: $rD <- FIELD_E * $rA",         exec.mult,     None,           None,       field_a,        field_d,   field_e,         "REG",        None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .af.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .bf.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            # Zero-compare conditional branch group  EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " f00.: if $rA == 0",                  exec.cbranch,  op.cb_eq,       field_a,    None,           None,      "REG",           0,            field_e,    None,   0,     0,     0,  0,  0,  0 ),
            ( " f01.: if $rA != 0",                  exec.cbranch,  op.cb_ne,       field_a,    None,           None,      "REG",           0,            field_e,    None,   0,     0,     0,  0,  0,  0 ),
            ( " f02.: if $rA < 0",                   exec.cbranch,  op.cb_lts,      field_a,    None,           None,      "REG",           0,            field_e,    None,   0,     0,     0,  0,  0,  0 ),
            ( " f03.: if $rA >= 0",                  exec.cbranch,  op.cb_ges,      field_a,    None,           None,      "REG",           0,            field_e,    None,   0,     0,     0,  0,  0,  0 ),
            ( " f04.: if $rA > 0",                   exec.cbranch,  op.cb_lts,      None,       field_a,        None,      0,               "REG",        field_e,    None,   0,     0,     0,  0,  0,  0 ),
            ( " f05.: if $rA <= 0",                  exec.cbranch,  op.cb_ges,      None,       field_a,        None,      0,               "REG",        field_e,    None,   0,     0,     0,  0,  0,  0 ),
            ( " f06.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " f07.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " f08.: if $rA == 0",                  exec.cbranch,  op.cb_eq,       field_a,    None,           None,      "REG",           0,            field_e,    None,   0,     0,     0,  0,  0,  0 ),
            ( " f09.: if $rA != 0",                  exec.cbranch,  op.cb_ne,       field_a,    None,           None,      "REG",           0,            field_e,    None,   0,     0,     0,  0,  0,  0 ),
            ( " f0a.: if $rA < 0",                   exec.cbranch,  op.cb_lts,      field_a,    None,           None,      "REG",           0,            field_e,    None,   0,     0,     0,  0,  0,  0 ),
            ( " f0b.: if $rA >= 0",                  exec.cbranch,  op.cb_ges,      field_a,    None,           None,      "REG",           0,            field_e,    None,   0,     0,     0,  0,  0,  0 ),
            ( " f0c.: if $rA > 0",                   exec.cbranch,  op.cb_lts,      None,       field_a,        None,      0,               "REG",        field_e,    None,   0,     0,     0,  0,  0,  0 ),
            ( " f0d.: if $rA <= 0",                  exec.cbranch,  op.cb_ges,      None,       field_a,        None,      0,               "REG",        field_e,    None,   0,     0,     0,  0,  0,  0 ),
            ( " f0e.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            # Conditional branch group               EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " f1..: if $rB == $rA",                exec.cbranch,  op.cb_eq,       field_b,    field_a,        None,      "REG",           "REG",        field_e,    None,   0,     0,     0,  0,  0,  0 ),
            ( " f2..: if $rB != $rA",                exec.cbranch,  op.cb_ne,       field_b,    field_a,        None,      "REG",           "REG",        field_e,    None,   0,     0,     0,  0,  0,  0 ),
            ( " f3..: if signed $rB < $rA",          exec.cbranch,  op.cb_lts,      field_b,    field_a,        None,      "REG",           "REG",        field_e,    None,   0,     0,     0,  0,  0,  0 ),
            ( " f4..: if signed $rB >= $rA",         exec.cbranch,  op.cb_ges,      field_b,    field_a,        None,      "REG",           "REG",        field_e,    None,   0,     0,     0,  0,  0,  0 ),
            ( " f5..: if $rB < $rA",                 exec.cbranch,  op.cb_lt,       field_b,    field_a,        None,      "REG",           "REG",        field_e,    None,   0,     0,     0,  0,  0,  0 ),
            ( " f6..: if $rB >= $rA",                exec.cbranch,  op.cb_ge,       field_b,    field_a,        None,      "REG",           "REG",        field_e,    None,   0,     0,     0,  0,  0,  0 ),
            ( " f7..: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " f8..: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " f9..: if $rB == $rA",                exec.cbranch,  op.cb_eq,       field_b,    field_a,        None,      "REG",           "REG",        field_e,    None,   0,     0,     0,  0,  0,  0 ),
            ( " fa..: if $rB != $rA",                exec.cbranch,  op.cb_ne,       field_b,    field_a,        None,      "REG",           "REG",        field_e,    None,   0,     0,     0,  0,  0,  0 ),
            ( " fb..: if signed $rB < $rA",          exec.cbranch,  op.cb_lts,      field_b,    field_a,        None,      "REG",           "REG",        field_e,    None,   0,     0,     0,  0,  0,  0 ),
            ( " fc..: if signed $rB >= $rA",         exec.cbranch,  op.cb_ges,      field_b,    field_a,        None,      "REG",           "REG",        field_e,    None,   0,     0,     0,  0,  0,  0 ),
            ( " fd..: if $rB < $rA",                 exec.cbranch,  op.cb_lt,       field_b,    field_a,        None,      "REG",           "REG",        field_e,    None,   0,     0,     0,  0,  0,  0 ),
            ( " fe..: if $rB >= $rA",                exec.cbranch,  op.cb_ge,       field_b,    field_a,        None,      "REG",           "REG",        field_e,    None,   0,     0,     0,  0,  0,  0),
            # Bit-set-test branch group              EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " f.f.: if $rA[.]  == 1",              exec.bbranch,  op.bb_one,      field_a,    None,           None,      "REG",           field_c,      field_e,    None,   0,     0,     0,  0,  0,  0 ),
            ( " f..f: if $rB[.]  == 0",              exec.bbranch,  op.bb_one,      None,       field_b,        None,      field_c,         "REG",        field_e,    None,   0,     0,     0,  0,  0,  0 ),
            # Stack group                            EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " .c**: MEM[$rA+tiny OFS*4] <- $rD",   exec.adder,    op.addr,        None,       tiny_field_a,   None,      None,            "REG",        tiny_ofs,   3,      0,     1,     0,  0,  0,  0 ),
            ( " .d**: $rD <- MEM[$rA+tiny OFS*4]",   exec.adder,    op.addr,        None,       tiny_field_a,   field_d,   None,            "REG",        tiny_ofs,   3,      1,     0,     0,  0,  0,  0 ),
            # Type operations                        EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " .e0.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .e1.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .e2.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .e3.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            # Indirect load/store group              EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " .e4.: $rD <- MEM8[$rA]",             exec.adder,    op.addr,        None,       field_a,        field_d,   None,            "REG",        0,          1,      1,     0,     0,  0,  1,  0 ),
            ( " .e5.: $rD <- MEM16[$rA]",            exec.adder,    op.addr,        None,       field_a,        field_d,   None,            "REG",        0,          2,      1,     0,     0,  0,  0,  1 ),
            ( " .e6.: $rD <- MEM32[$rA]",            exec.adder,    op.addr,        None,       field_a,        field_d,   None,            "REG",        0,          3,      1,     0,     0,  0,  0,  0 ),
            ( " .e7.: $rD <- MEMLL32[$rA]",          exec.adder,    op.addr,        None,       field_a,        field_d,   None,            "REG",        0,          3,      1,     0,     0,  0,  0,  0 ),
            ( " .e8.: MEM8[$rA] <- $rD",             exec.adder,    op.addr,        None,       field_a,        None,      None,            "REG",        0,          1,      0,     1,     0,  0,  0,  0 ),
            ( " .e9.: MEM16[$rA] <- $rD",            exec.adder,    op.addr,        None,       field_a,        None,      None,            "REG",        0,          2,      0,     1,     0,  0,  0,  0 ),
            ( " .ea.: MEM32[$rA] <- $rD",            exec.adder,    op.addr,        None,       field_a,        None,      None,            "REG",        0,          3,      0,     1,     0,  0,  0,  0 ),
            ( " .eb.: MEMSR32[$rA] <- $rD",          exec.adder,    op.addr,        None,       field_a,        None,      None,            "REG",        0,          3,      0,     1,     0,  0,  0,  0 ),
            ( " .ec.: $rD <- SMEM8[$rA]",            exec.adder,    op.addr,        None,       field_a,        field_d,   None,            "REG",        0,          1,      1,     0,     1,  0,  0,  0 ),
            ( " .ed.: $rD <- SMEM16[$rA]",           exec.adder,    op.addr,        None,       field_a,        field_d,   None,            "REG",        0,          2,      1,     0,     0,  1,  0,  0 ),
            # Indirect jump group                    EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " 1ee.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " 2ee.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " 3ee.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            # Offset-indirect type operations        EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " 1ee.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " 2ee.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " 3ee.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            # Offset-indirect load/store group       EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " .f4.: $rD <- MEM8[$rA+FIELD_E]",     exec.adder,    op.addr,        None,       field_a,        field_d,   None,            "REG",        field_e,    1,      1,     0,     0,  0,  1,  0 ),
            ( " .f5.: $rD <- MEM16[$rA+FIELD_E]",    exec.adder,    op.addr,        None,       field_a,        field_d,   None,            "REG",        field_e,    2,      1,     0,     0,  0,  0,  1 ),
            ( " .f6.: $rD <- MEM32[$rA+FIELD_E]",    exec.adder,    op.addr,        None,       field_a,        field_d,   None,            "REG",        field_e,    3,      1,     0,     0,  0,  0,  0 ),
            ( " .f7.: $rD <- MEMLL32[$rA+FIELD_E]",  exec.adder,    op.addr,        None,       field_a,        field_d,   None,            "REG",        field_e,    3,      1,     0,     0,  0,  0,  0 ),
            ( " .f8.: MEM8[$rA+FIELD_E] <- $rD",     exec.adder,    op.addr,        None,       field_a,        None,      None,            "REG",        field_e,    1,      0,     1,     0,  0,  0,  0 ),
            ( " .f9.: MEM16[$rA+FIELD_E] <- $rD",    exec.adder,    op.addr,        None,       field_a,        None,      None,            "REG",        field_e,    2,      0,     1,     0,  0,  0,  0 ),
            ( " .fa.: MEM32[$rA+FIELD_E] <- $rD",    exec.adder,    op.addr,        None,       field_a,        None,      None,            "REG",        field_e,    3,      0,     1,     0,  0,  0,  0 ),
            ( " .fb.: MEMSR32[$rA+FIELD_E] <- $rD",  exec.adder,    op.addr,        None,       field_a,        None,      None,            "REG",        field_e,    3,      0,     1,     0,  0,  0,  0 ),
            ( " .fc.: $rD <- SMEM8[$rA+FIELD_E]",    exec.adder,    op.addr,        None,       field_a,        field_d,   None,            "REG",        field_e,    1,      1,     0,     1,  0,  0,  0 ),
            ( " .fd.: $rD <- SMEM16[$rA+FIELD_E]",   exec.adder,    op.addr,        None,       field_a,        field_d,   None,            "REG",        field_e,    2,      1,     0,     0,  1,  0,  0 ),
            # Offset-indirect jump group             EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " 1fe.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " 2fe.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " 3fe.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            # Absolute load/store group              EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " .f4f: $rD <- MEM8[FIELD_E]",         exec.adder,    op.addr,        None,       None,           field_d,   None,            0,            field_e,    1,      1,     0,     0,  0,  1,  0 ),
            ( " .f5f: $rD <- MEM16[FIELD_E]",        exec.adder,    op.addr,        None,       None,           field_d,   None,            0,            field_e,    2,      1,     0,     0,  0,  0,  1 ),
            ( " .f6f: $rD <- MEM32[FIELD_E]",        exec.adder,    op.addr,        None,       None,           field_d,   None,            0,            field_e,    3,      1,     0,     0,  0,  0,  0 ),
            ( " .f7f: $rD <- MEMLL32[FIELD_E]",      exec.adder,    op.addr,        None,       None,           field_d,   None,            0,            field_e,    3,      1,     0,     0,  0,  0,  0 ),
            ( " .f8f: MEM8[FIELD_E] <- $rD",         exec.adder,    op.addr,        None,       None,           None,      None,            0,            field_e,    1,      0,     1,     0,  0,  0,  0 ),
            ( " .f9f: MEM16[FIELD_E] <- $rD",        exec.adder,    op.addr,        None,       None,           None,      None,            0,            field_e,    2,      0,     1,     0,  0,  0,  0 ),
            ( " .faf: MEM32[FIELD_E] <- $rD",        exec.adder,    op.addr,        None,       None,           None,      None,            0,            field_e,    3,      0,     1,     0,  0,  0,  0 ),
            ( " .fbf: MEMSR32[FIELD_E] <- $rD",      exec.adder,    op.addr,        None,       None,           None,      None,            0,            field_e,    3,      0,     1,     0,  0,  0,  0 ),
            ( " .fcf: $rD <- SMEM8[FIELD_E]",        exec.adder,    op.addr,        None,       None,           field_d,   None,            0,            field_e,    1,      1,     0,     1,  0,  0,  0 ),
            ( " .fdf: $rD <- SMEM16[FIELD_E]",       exec.adder,    op.addr,        None,       None,           field_d,   None,            0,            field_e,    2,      1,     0,     0,  1,  0,  0 ),
            # Absolute jump group                    EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " 1fef: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " 2fef: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " 3fef: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
        )

        def parse_bit_mask(full_mask: str) -> Tuple[Wire, str]:
            """Create an expression that checks for the provided pattern

            Args:
                full_mask (str): bit-mask string in the format of .-s, *-s and hex digits, as described in the decode table

            Returns:
                Wire: An expression that returns '1' if the instruction code matches that pattern, '0' otherwise.
            """
            mask = full_mask.split(':')[0].strip() # Remove comment and trailing/leading spaces
            ins_name = full_mask.split(':')[1].strip() # This is the comment part, which we'll use to make up the name for the wire
            ins_name = ins_name.replace('<-', 'eq')
            ins_name = ins_name.replace('-$', 'minus_')
            ins_name = ins_name.replace('$', '')
            ins_name = ins_name.replace('[.]', '_bit')
            ins_name = ins_name.replace('[', '_')
            ins_name = ins_name.replace(']', '')
            ins_name = ins_name.replace('>>>', 'asr')
            ins_name = ins_name.replace('>>', 'lsr')
            ins_name = ins_name.replace('<<', 'lsl')
            ins_name = ins_name.replace('&', 'and')
            ins_name = ins_name.replace('|', 'or')
            ins_name = ins_name.replace('^', 'xor')
            ins_name = ins_name.replace('+', 'plus')
            ins_name = ins_name.replace('-', 'minus')
            ins_name = ins_name.replace('*', 'times')
            ins_name = ins_name.replace('~', 'not')
            ins_name = ins_name.replace('<=', 'le')
            ins_name = ins_name.replace('>=', 'ge')
            ins_name = ins_name.replace('<', 'lt')
            ins_name = ins_name.replace('>', 'gt')
            ins_name = ins_name.replace('==', 'eq')
            ins_name = ins_name.replace('!=', 'ne')
            ins_name = ins_name.replace(' ', '_')
            ins_name = ins_name.lower()
            idx = 0
            ret_val = 1
            for field, field_is_f in zip((field_d, field_c, field_b, field_a), (field_d_is_f, field_c_is_f, field_b_is_f, field_a_is_f)):
                do_gt = mask[idx] == '>'
                do_lt = mask[idx] == '<'
                if do_gt or do_lt: idx += 1
                digit = mask[idx]

                if digit == '.':
                    ret_val = ret_val & ~field_is_f
                elif digit == '*':
                    pass
                elif digit in ('0123456789abcdef'):
                    value = int(digit, 16)
                    if do_gt:
                        ret_val = ret_val & (field > value)
                    elif do_lt:
                        ret_val = ret_val & (field < value)
                    else:
                        ret_val = ret_val & (field == value)
                else:
                    raise SyntaxErrorException(f"Unknown digit {digit} in decode mask {full_mask}")
                idx += 1
            return ret_val, ins_name

        CODE       = 0
        EXEC_UNIT  = 1
        OP_CODE    = 2
        RD1_ADDR   = 3
        RD2_ADDR   = 4
        RES_ADDR   = 5
        OP_A       = 6
        OP_B       = 7
        OP_IMM     = 8
        MEM_LEN    = 9
        IS_LD      = 10
        IS_ST      = 11
        BSE        = 12
        WSE        = 13
        BZE        = 14
        WZE        = 15

        mask_expressions = []
        for expr, name in (parse_bit_mask(line[CODE]) for line in inst_table):
            setattr(self, f"mask_for_{name}", expr)
            mask_expressions.append(expr)

        # At this point we have all the required selections for the various control lines in 'inst_table' and their selection expressions in 'mask_expressions'.
        # All we need to do is to create the appropriate 'SelectOne' expressions.

        select_list_exec_unit = []
        select_list_op_code   = []
        select_list_rd1_addr  = []
        select_list_rd2_addr  = []
        select_list_res_addr  = []
        select_list_op_a      = []
        select_list_op_b      = []
        select_list_use_reg_a = []
        select_list_use_reg_b = []
        select_list_op_imm    = []
        select_list_mem_len   = []
        select_list_is_ld     = []
        select_list_is_st     = []
        select_list_bse       = []
        select_list_wse       = []
        select_list_bze       = []
        select_list_wze       = []

        select_lists = (
            select_list_exec_unit,
            select_list_op_code,
            select_list_rd1_addr,
            select_list_rd2_addr,
            select_list_res_addr,
            select_list_op_a,
            select_list_op_b,
            select_list_op_imm,
            select_list_mem_len,
            select_list_is_ld,
            select_list_is_st,
            select_list_bse,
            select_list_wse,
            select_list_bze,
            select_list_wze,
        )

        for line, mask_expr in zip(inst_table, mask_expressions):
            for idx, (select_list, value) in enumerate(zip(select_lists, line[EXEC_UNIT:])):
                idx += 1 # We are skipping the first column, so we have to accomodate it here
                # OP_A and OP_B are somewhat special to hide the read-latency of the register file:
                # We do two-stage muxing: We mux all non-reg-file outputs, then register them, then
                # do a post-mux to swap in the register file outputs
                if idx in (OP_A, OP_B):
                    use_reg_list = {OP_A: select_list_use_reg_a, OP_B: select_list_use_reg_b}[idx]
                    if isinstance(value, str):
                        assert value == "REG"
                        # We rely on default_port for the post-muxes to select the non-reg-file outputs
                        use_reg_list += (mask_expr, 1)
                    else:
                        assert not isinstance(value, str)
                        # We don't care of the pre-selector behavior in the reg-file-output cases
                        if value is not None: select_list += (mask_expr, value)
                else:
                    # Remove all the 0-s from the selectors for these fields and rely on default_ports to restore them
                    if idx in (IS_LD, IS_ST, BSE, WSE, BZE, WZE,) and value == 0:
                        value = None
                    assert not isinstance(value, str)
                    if value is not None: select_list += (mask_expr, value)

        # ... actually a little more than that: we have to also generate the reservation logic. So let's start with that.
        select_list_read1_needed = []
        select_list_read2_needed = []
        select_list_rsv_needed = []

        for line, mask_expr in zip(inst_table, mask_expressions):
            for select_list, value in zip((select_list_read1_needed, select_list_read2_needed, select_list_rsv_needed), line[RD1_ADDR:RES_ADDR+1]):
                if value is not None: select_list += (mask_expr, 1)

        # Now that we have the selection lists, we can compose the muxes
        exec_unit = SelectOne(*select_list_exec_unit)                   if len(select_list_exec_unit) > 0 else None
        op_code   = SelectOne(*select_list_op_code)                     if len(select_list_op_code) > 0 else None
        rd1_addr  = SelectOne(*select_list_rd1_addr)                    if len(select_list_rd1_addr) > 0 else None
        rd2_addr  = SelectOne(*select_list_rd2_addr)                    if len(select_list_rd2_addr) > 0 else None
        res_addr  = SelectOne(*select_list_res_addr)                    if len(select_list_res_addr) > 0 else None
        use_reg_a = SelectOne(*select_list_use_reg_a, default_port = 0) if len(select_list_use_reg_a) > 0 else None
        use_reg_b = SelectOne(*select_list_use_reg_b, default_port = 0) if len(select_list_use_reg_b) > 0 else None
        op_a      = SelectOne(*select_list_op_a)                        if len(select_list_op_a) > 0 else None
        op_b      = SelectOne(*select_list_op_b)                        if len(select_list_op_b) > 0 else None
        op_imm    = SelectOne(*select_list_op_imm)                      if len(select_list_op_imm) > 0 else None
        mem_len   = SelectOne(*select_list_mem_len)                     if len(select_list_mem_len) > 0 else None
        is_ld     = SelectOne(*select_list_is_ld, default_port = 0)     if len(select_list_is_ld) > 0 else 0
        is_st     = SelectOne(*select_list_is_st, default_port = 0)     if len(select_list_is_st) > 0 else 0
        bse       = SelectOne(*select_list_bse, default_port = 0)       if len(select_list_bse) > 0 else 0
        wse       = SelectOne(*select_list_wse, default_port = 0)       if len(select_list_wse) > 0 else 0
        bze       = SelectOne(*select_list_bze, default_port = 0)       if len(select_list_bze) > 0 else 0
        wze       = SelectOne(*select_list_wze, default_port = 0)       if len(select_list_wze) > 0 else 0

        read1_needed = SelectOne(*select_list_read1_needed, default_port=0)
        read2_needed = SelectOne(*select_list_read2_needed, default_port=0)
        rsv_needed   = SelectOne(*select_list_rsv_needed, default_port=0)

        # We let the register file handle the hand-shaking for us. We just need to implement the output buffers
        self.fetch.ready <<= self.reg_file_req.ready
        self.reg_file_req.valid <<= self.fetch.valid

        self.exec.valid <<= self.reg_file_rsp.valid
        self.reg_file_rsp.ready <<= self.exec.ready

        self.reg_file_req.read1_addr  <<= BrewRegAddr(rd1_addr)
        self.reg_file_req.read1_valid <<= read1_needed
        self.reg_file_req.read2_addr  <<= BrewRegAddr(rd2_addr)
        self.reg_file_req.read2_valid <<= read2_needed
        self.reg_file_req.rsv_addr    <<= BrewRegAddr(res_addr)
        self.reg_file_req.rsv_valid   <<= rsv_needed

        register_outputs = self.reg_file_req.ready & self.reg_file_req.valid

        self.exec.opcode                <<= Reg(op_code, clock_en=register_outputs)
        self.exec.exec_unit             <<= Reg(exec_unit, clock_en=register_outputs)
        self.exec.op_a                  <<= Select(Reg(use_reg_a, clock_en=register_outputs), Reg(op_a, clock_en=register_outputs), self.reg_file_rsp.read1_data)
        self.exec.op_b                  <<= Select(Reg(use_reg_b, clock_en=register_outputs), Reg(op_b, clock_en=register_outputs), self.reg_file_rsp.read2_data)
        self.exec.op_imm                <<= Reg(op_imm, clock_en=register_outputs)
        self.exec.mem_access_len        <<= Reg(mem_len, clock_en=register_outputs)
        self.exec.inst_len              <<= Reg(self.fetch.inst_len, clock_en=register_outputs)
        self.exec.is_load               <<= Reg(is_ld, clock_en=register_outputs)
        self.exec.is_store              <<= Reg(is_st, clock_en=register_outputs)
        self.exec.do_bse                <<= Reg(bse, clock_en=register_outputs)
        self.exec.do_wse                <<= Reg(wse, clock_en=register_outputs)
        self.exec.do_bze                <<= Reg(bze, clock_en=register_outputs)
        self.exec.do_wze                <<= Reg(wze, clock_en=register_outputs)
        self.exec.result_reg_addr       <<= Reg(BrewRegAddr(res_addr), clock_en=register_outputs)
        self.exec.result_reg_addr_valid <<= Reg(rsv_needed, clock_en=register_outputs)
        self.exec.fetch_av              <<= Reg(self.fetch.av, clock_en=register_outputs)




def sim():
    class RegFileEmulator(Module):
        clk = ClkPort()
        rst = RstPort()

        # Interface to the register file
        reg_file_req = Input(RegFileReadRequestIf)
        reg_file_rsp = Output(RegFileReadResponseIf)

        def simulate(self) -> TSimEvent:
            self.out_buf_full = False

            def wait_clk():
                first = True
                while first or self.clk.get_sim_edge() != EdgeType.Positive:
                    first = False
                    yield (self.clk, self.reg_file_rsp.ready)
                    if not self.out_buf_full or self.reg_file_rsp.ready:
                        self.reg_file_req.ready <<= 1
                    else:
                        self.reg_file_req.ready <<= 0


            self.reg_file_rsp.valid <<= 0
            self.reg_file_req.ready <<= 1

            rd_addr1 = None
            rd_addr2 = None
            rsv_addr = None
            while True:
                yield from wait_clk()

                if not self.out_buf_full:
                    self.reg_file_rsp.read1_data <<= None
                    self.reg_file_rsp.read2_data <<= None
                    self.reg_file_rsp.valid <<= 0
                else:
                    if rd_addr1 is not None:
                        rd_data1 = 0x100+rd_addr1
                        print(f"RF reading $r{rd_addr1:x} with value {rd_data1}")
                    else:
                        rd_data1 = None
                    if rd_addr2 is not None:
                        rd_data2 = 0x100+rd_addr2
                        print(f"RF reading $r{rd_addr2:x} with value {rd_data2}")
                    else:
                        rd_data2 = None
                    if rsv_addr is not None:
                        print(f"RF reserving $r{rsv_addr:x}")
                    self.reg_file_rsp.read1_data <<= rd_data1
                    self.reg_file_rsp.read2_data <<= rd_data2
                    self.reg_file_rsp.valid <<= 1

                self.out_buf_full = (self.reg_file_req.valid & self.reg_file_req.ready) == 1
                if self.out_buf_full:
                    rd_addr1 = copy(self.reg_file_req.read1_addr.sim_value) if self.reg_file_req.read1_valid == 1 else None
                    rd_addr2 = copy(self.reg_file_req.read2_addr.sim_value) if self.reg_file_req.read2_valid == 1 else None
                    rsv_addr = copy(self.reg_file_req.rsv_addr.sim_value  ) if self.reg_file_req.rsv_valid == 1   else None


    class FetchEmulator(Module):
        clk = ClkPort()
        rst = RstPort()

        fetch = Output(FetchDecodeIf)

        def simulate(self) -> TSimEvent:
            def wait_clk():
                yield (self.clk, )
                while self.clk.get_sim_edge() != EdgeType.Positive:
                    yield (self.clk, )

            def wait_rst():
                yield from wait_clk()
                while self.rst == 1:
                    yield from wait_clk()

            def wait_transfer():
                self.fetch.valid <<= 1
                yield from wait_clk()
                while (self.fetch.valid & self.fetch.ready) != 1:
                    yield from wait_clk()
                self.fetch.valid <<= 0

            def issue(inst, av):
                if len(inst) == 1:
                    self.fetch.inst_0 <<= inst[0]
                    self.fetch.inst_1 <<= None
                    self.fetch.inst_2 <<= None
                elif len(inst) == 2:
                    self.fetch.inst_0 <<= inst[0]
                    self.fetch.inst_1 <<= inst[1]
                    self.fetch.inst_2 <<= None
                elif len(inst) == 3:
                    self.fetch.inst_0 <<= inst[0]
                    self.fetch.inst_1 <<= inst[1]
                    self.fetch.inst_2 <<= inst[2]
                else:
                    assert False
                self.fetch.inst_len <<= len(inst)
                self.fetch.av <<= av
                yield from wait_transfer()

            self.fetch.valid <<= 0
            yield from wait_rst()
            for i in range(4):
                yield from wait_clk()

            yield from issue((0x0123,), False) # $r0 <- $r2 ^ $r3
            for i in range(4):
                yield from wait_clk()

    class ExecEmulator(Module):
        clk = ClkPort()
        rst = RstPort()

        exec = Input(DecodeExecIf)

        def simulate(self) -> TSimEvent:
            def wait_clk():
                yield (self.clk, )
                while self.clk.get_sim_edge() != EdgeType.Positive:
                    yield (self.clk, )

            def wait_rst():
                yield from wait_clk()
                while self.rst == 1:
                    yield from wait_clk()

            def wait_transfer():
                self.exec.ready <<= 1
                yield from wait_clk()
                while (self.exec.valid & self.exec.ready) != 1:
                    yield from wait_clk()
                self.exec.ready <<= 0

            self.exec.ready <<= 1



    class top(Module):
        clk = ClkPort()
        rst = RstPort()

        def body(self):
            self.fetch_emulator = FetchEmulator()
            self.exec_emulator = ExecEmulator()
            self.reg_file_emulator = RegFileEmulator()

            self.dut = DecodeStage()

            self.dut.fetch <<= self.fetch_emulator.fetch

            self.exec_emulator.exec <<= self.dut.exec

            self.reg_file_emulator.reg_file_req <<= self.dut.reg_file_req
            self.dut.reg_file_rsp <<= self.reg_file_emulator.reg_file_rsp




        def simulate(self) -> TSimEvent:
            seed(0)

            def clk() -> int:
                yield 10
                self.clk <<= ~self.clk & self.clk
                yield 10
                self.clk <<= ~self.clk
                yield 0

            print("Simulation started")

            self.rst <<= 1
            self.clk <<= 1
            yield 10
            for i in range(5):
                yield from clk()
            self.rst <<= 0

            for i in range(50):
                yield from clk()
            now = yield 10
            print(f"Done at {now}")

    Build.simulation(top, "decode.vcd", add_unnamed_scopes=True)

def gen():
    Build.generate_rtl(DecodeStage)

if __name__ == "__main__":
    #gen()
    sim()
