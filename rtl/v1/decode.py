#!/usr/bin/python3
from random import *
from typing import *
from silicon import *
from .brew_types import *
from .brew_utils import *

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

class FetchStage(Module):
    clk = ClkPort()
    rst = RstPort()

    fetch = Input(FetchDecodeIf)
    exec = Output(DecodeExecIf)

    # Interface to the register file
    read1_addr = Output(BrewRegAddr)
    read1_data = Input(BrewData)
    read1_request = Output(logic)
    read1_response = Input(logic)

    read2_addr = Output(BrewRegAddr)
    read2_data = Input(BrewData)
    read2_request = Output(logic)
    read2_response = Input(logic)

    rsv_addr = Output(BrewRegAddr)
    rsv_request = Output(logic)
    rsv_response = Input(logic)

    def body(self):
        read1_needed = Wire(logic)
        read2_needed = Wire(logic)
        rsv_needed = Wire(logic)

        mem_access = Wire(logic)

        field_d = self.fetch.inst[15:12]
        field_c = self.fetch.inst[11:8]
        field_b = self.fetch.inst[7:4]
        field_a = self.fetch.inst[3:0]
        field_e = Select(
            self.fetch.inst_len == 2, # 48-bit instructions
            Unsigned(32)(Signed(32)(Signed(16)(self.fetch.inst[15:0]))), # Sign-extend to 32 bits
            self.fetch.inst
        )

        field_a_is_f = field_a == 0xf
        field_b_is_f = field_b == 0xf
        field_c_is_f = field_c == 0xf
        field_d_is_f = field_d == 0xf

        tiny_ofs = {self.fetch.inst[7:1], "2'b0"}
        tiny_field_a = self.fetch.inst[0]

        field_a_from_ones_complement = Select(
            field_a[3],
            field_a,
            Unsigned(32)(Signed(32)(Signed(4)((field_a+1)[3:0])))
        )

        # Codes: 0123456789abcde <- exact match to that digit
        #        . <- anything but 0xf
        #        * <- anything, including 0xf
        #        < <- less then subsequent digit
        #        > <- greater than subsequent digit (but not 0xf)
        #        : <- anything after that is comment
        inst_table = (
            #  CODE                                  EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( "<8000: SWI",                          exec.misc,     op.misc_swi,    None,       None,           None,      "field_a",       None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " 8000: STM",                          exec.misc,     op.misc_stm,    None,       None,           None,      None,            None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " 9000: WOI",                          exec.cbranch,  op.cb_eq,       "field_a",  "field_b",      None,      "reg_1",         "reg_2",      0,          None,   0,     0,     0,  0,  0,  0 ), # Decoded as 'if $0 == $0 $pc <- $pc'
            ( ">9000: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .001: FENCE",                        exec.logic,    op.b_or,        None,       None,           None,      None,            None,         None,       None,   0,     0,     0,  0,  0,  0 ), # Decoded as a kind of NOP
            ( " .002: $pc <- $rD",                   exec.misc,     op.misc_pc_w,   "field_d",  None,           None,      None,            None,         "reg_1",    None,   0,     0,     0,  0,  0,  0 ),
            ( " .003: $tpc <- $rD",                  exec.misc,     op.misc_tpc_w,  "field_d",  None,           None,      None,            None,         "reg_1",    None,   0,     0,     0,  0,  0,  0 ),
            ( " .004: $rD <- $pc",                   exec.misc,     op.misc_pc_r,   None,       None,           "field_d", None,            None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .005: $rD <- $tpc",                  exec.misc,     op.misc_tpc_r,  "field_d",  None,           None,      None,            None,         "reg_1",    None,   0,     0,     0,  0,  0,  0 ),
            ( " .00>5: SII",                         exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            # Unary group                            EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " .01.: $rD <- tiny FIELD_A",          exec.logic,    op.b_or,        None,       None,           "field_d", "ones_field_a",  0,            None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .02.: $rD <- $pc + FIELD_A*2",       exec.adder,    op.pc_add,      None,       None,           "field_d", "ones_field_a",  0,            None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .03.: $rD <- -$rA",                  exec.adder,    op.b_sub_a,     "field_a",  None,           "field_d", "reg_1",         0,            None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .04.: $rD <- ~$rA",                  exec.logic,    op.b_xor,       "field_a",  None,           "field_d", "reg_1",         0xffffffff,   None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .05.: $rD <- bse $rA",               exec.logic,    op.b_or,        "field_a",  None,           "field_d", "reg_1",         0,            None,       None,   0,     0,     1,  0,  0,  0 ),
            ( " .06.: $rD <- wse $rA",               exec.logic,    op.b_or,        "field_a",  None,           "field_d", "reg_1",         0,            None,       None,   0,     0,     0,  1,  0,  0 ),
            ( " .0>6.: SII",                         exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            # Binary ALU group                       EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " .1..: $rD <- $A ^ $rB",              exec.logic,    op.b_xor,       "field_a",  "field_b",      "field_d", "reg_1",         "reg_2",      None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .2..: $rD <- $A | $rB",              exec.logic,    op.b_or,        "field_a",  "field_b",      "field_d", "reg_1",         "reg_2",      None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .3..: $rD <- $A & $rB",              exec.logic,    op.b_and,       "field_a",  "field_b",      "field_d", "reg_1",         "reg_2",      None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .4..: $rD <- $A + $rB",              exec.adder,    op.add,         "field_a",  "field_b",      "field_d", "reg_1",         "reg_2",      None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .5..: $rD <- $A - $rB",              exec.adder,    op.a_sub_b,     "field_a",  "field_b",      "field_d", "reg_1",         "reg_2",      None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .6..: $rD <- $A << $rB",             exec.shift,    op.shll,        "field_a",  "field_b",      "field_d", "reg_1",         "reg_2",      None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .7..: $rD <- $A >> $rB",             exec.shift,    op.shlr,        "field_a",  "field_b",      "field_d", "reg_1",         "reg_2",      None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .8..: $rD <- $A >>> $rB",            exec.shift,    op.shar,        "field_a",  "field_b",      "field_d", "reg_1",         "reg_2",      None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .9..: $rD <- $A * $rB",              exec.mult,     None,           "field_a",  "field_b",      "field_d", "reg_1",         "reg_2",      None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .a..: $rD <- ~$A & $rB",             exec.logic,    op.b_nand,      "field_a",  "field_b",      "field_d", "reg_1",         "reg_2",      None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .b..: $rD <- tiny $rB + FIELD_A",    exec.adder,    op.add,         None,       "field_b",      "field_d", "ones_field_a",  "reg_2",      None,       None,   0,     0,     0,  0,  0,  0 ),
            # Load immediate group                   EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " .00f: $rD <- VALUE",                 exec.logic,    op.b_or,        None,       None,           "field_d",  "field_e",      0,            None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " 20ef: $pc <- VALUE",                 exec.misc,     op.misc_pc_w,   None,       None,           None,       None,           None,         "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            ( " 30ef: $tpc <- VALUE",                exec.misc,     op.misc_tpc_w,  None,       None,           None,       None,           None,         "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            ( " 80ef.: SII",                         exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " 90ef.: SII",                         exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            # Constant ALU grou  p                   EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " .1.f: $rD <- FIELD_E ^ $rB",         exec.logic,    op.b_xor,       None,       "field_b",      "field_d", "field_e",       "reg_2",      None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .2.f: $rD <- FIELD_E | $rB",         exec.logic,    op.b_or,        None,       "field_b",      "field_d", "field_e",       "reg_2",      None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .3.f: $rD <- FIELD_E & $rB",         exec.logic,    op.b_and,       None,       "field_b",      "field_d", "field_e",       "reg_2",      None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .4.f: $rD <- FIELD_E + $rB",         exec.adder,    op.add,         None,       "field_b",      "field_d", "field_e",       "reg_2",      None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .5.f: $rD <- FIELD_E - $rB",         exec.adder,    op.a_sub_b,     None,       "field_b",      "field_d", "field_e",       "reg_2",      None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .6.f: $rD <- FIELD_E << $rB",        exec.shift,    op.shll,        None,       "field_b",      "field_d", "field_e",       "reg_2",      None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .7.f: $rD <- FIELD_E >> $rB",        exec.shift,    op.shlr,        None,       "field_b",      "field_d", "field_e",       "reg_2",      None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .8.f: $rD <- FIELD_E >>> $rB",       exec.shift,    op.shar,        None,       "field_b",      "field_d", "field_e",       "reg_2",      None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .9.f: $rD <- FIELD_E * $rB",         exec.mult,     None,           None,       "field_b",      "field_d", "field_e",       "reg_2",      None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .a.f: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .b.f: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            # Short load immediate group             EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " .0f0: $rD <- short VALUE",           exec.logic,    op.b_or,        None,       None,           "field_d",  "field_e",      0,            None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " 20fe: $pc <- short VALUE",           exec.misc,     op.misc_pc_w,   None,       None,           None,       None,           None,         "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            ( " 30fe: $tpc <- short VALUE",          exec.misc,     op.misc_tpc_w,  None,       None,           None,       None,           None,         "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            # Short constant ALU group               EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " .1f.: $rD <- FIELD_E ^ $rA",         exec.logic,    op.b_xor,       "field_a",  None,           "field_d", "field_e",       "reg_1",      None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .2f.: $rD <- FIELD_E | $rA",         exec.logic,    op.b_or,        "field_a",  None,           "field_d", "field_e",       "reg_1",      None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .3f.: $rD <- FIELD_E & $rA",         exec.logic,    op.b_and,       "field_a",  None,           "field_d", "field_e",       "reg_1",      None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .4f.: $rD <- FIELD_E + $rA",         exec.adder,    op.add,         "field_a",  None,           "field_d", "field_e",       "reg_1",      None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .5f.: $rD <- FIELD_E - $rA",         exec.adder,    op.a_sub_b,     "field_a",  None,           "field_d", "field_e",       "reg_1",      None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .6f.: $rD <- FIELD_E << $rA",        exec.shift,    op.shll,        "field_a",  None,           "field_d", "field_e",       "reg_1",      None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .7f.: $rD <- FIELD_E >> $rA",        exec.shift,    op.shlr,        "field_a",  None,           "field_d", "field_e",       "reg_1",      None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .8f.: $rD <- FIELD_E >>> $rA",       exec.shift,    op.shar,        "field_a",  None,           "field_d", "field_e",       "reg_1",      None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .9f.: $rD <- FIELD_E * $rA",         exec.mult,     None,           "field_a",  None,           "field_d", "field_e",       "reg_1",      None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .af.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .bf.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            # Zero-compare conditional branch group  EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " f00.: if $rA == 0",                  exec.cbranch,  op.cb_eq,       "field_a",  None,           None,      "reg_1",         0,            "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            ( " f01.: if $rA != 0",                  exec.cbranch,  op.cb_ne,       "field_a",  None,           None,      "reg_1",         0,            "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            ( " f02.: if $rA < 0",                   exec.cbranch,  op.cb_lts,      "field_a",  None,           None,      "reg_1",         0,            "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            ( " f03.: if $rA >= 0",                  exec.cbranch,  op.cb_ges,      "field_a",  None,           None,      "reg_1",         0,            "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            ( " f04.: if $rA > 0",                   exec.cbranch,  op.cb_lts,      None,       "field_a",      None,      0,               "reg_1",      "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            ( " f05.: if $rA <= 0",                  exec.cbranch,  op.cb_ges,      None,       "field_a",      None,      0,               "reg_1",      "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            ( " f06.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " f07.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " f08.: if $rA == 0",                  exec.cbranch,  op.cb_eq,       "field_a",  None,           None,      "reg_1",         0,            "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            ( " f09.: if $rA != 0",                  exec.cbranch,  op.cb_ne,       "field_a",  None,           None,      "reg_1",         0,            "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            ( " f0a.: if $rA < 0",                   exec.cbranch,  op.cb_lts,      "field_a",  None,           None,      "reg_1",         0,            "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            ( " f0b.: if $rA >= 0",                  exec.cbranch,  op.cb_ges,      "field_a",  None,           None,      "reg_1",         0,            "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            ( " f0c.: if $rA > 0",                   exec.cbranch,  op.cb_lts,      None,       "field_a",      None,      0,               "reg_1",      "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            ( " f0d.: if $rA <= 0",                  exec.cbranch,  op.cb_ges,      None,       "field_a",      None,      0,               "reg_1",      "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            ( " f0e.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            # Conditional branch group               EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " f1..: if $rB == $rA",                exec.cbranch,  op.cb_eq,       "field_a",  "field_b",      None,      "reg_2",         "reg_1",      "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            ( " f2..: if $rB != $rA",                exec.cbranch,  op.cb_ne,       "field_a",  "field_b",      None,      "reg_2",         "reg_1",      "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            ( " f3..: if signed $rB < $rA",          exec.cbranch,  op.cb_lts,      "field_a",  "field_b",      None,      "reg_2",         "reg_1",      "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            ( " f4..: if signed $rB >= $rA",         exec.cbranch,  op.cb_ges,      "field_a",  "field_b",      None,      "reg_2",         "reg_1",      "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            ( " f5..: if $rB < $rA",                 exec.cbranch,  op.cb_lt,       "field_a",  "field_b",      None,      "reg_2",         "reg_1",      "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            ( " f6..: if $rB >= $rA",                exec.cbranch,  op.cb_ge,       "field_a",  "field_b",      None,      "reg_2",         "reg_1",      "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            ( " f7..: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " f8..: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " f9..: if $rB == $rA",                exec.cbranch,  op.cb_eq,       "field_a",  "field_b",      None,      "reg_2",         "reg_1",      "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            ( " fa..: if $rB != $rA",                exec.cbranch,  op.cb_ne,       "field_a",  "field_b",      None,      "reg_2",         "reg_1",      "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            ( " fb..: if signed $rB < $rA",          exec.cbranch,  op.cb_lts,      "field_a",  "field_b",      None,      "reg_2",         "reg_1",      "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            ( " fc..: if signed $rB >= $rA",         exec.cbranch,  op.cb_ges,      "field_a",  "field_b",      None,      "reg_2",         "reg_1",      "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            ( " fd..: if $rB < $rA",                 exec.cbranch,  op.cb_lt,       "field_a",  "field_b",      None,      "reg_2",         "reg_1",      "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            ( " fe..: if $rB >= $rA",                exec.cbranch,  op.cb_ge,       "field_a",  "field_b",      None,      "reg_2",         "reg_1",      "field_e",  None,   0,     0,     0,  0,  0,  0),
            # Bit-set-test branch group              EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " f.f.: if $rA[.]  == 1",              exec.bbranch,  op.bb_one,      "field_a",  None,           None,      "reg_1",         "field_c",    "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            ( " f..f: if $rB[.]  == 0",              exec.bbranch,  op.bb_one,      None,       "field_b",      None,      "field_c",       "reg_2",      "field_e",  None,   0,     0,     0,  0,  0,  0 ),
            # Stack group                            EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " .c**: MEM[$rA,tiny OFS*4] <- $rD",   exec.adder,    op.addr,        None,       "tiny_field_a", None,      None,            "reg_2",      "tiny_ofs", 3,      0,     1,     0,  0,  0,  0 ),
            ( " .d**: $rD <- MEM[$rA,tiny OFS*4]",   exec.adder,    op.addr,        None,       "tiny_field_a", "field_d", None,            "reg_2",      "tiny_ofs", 3,      1,     0,     0,  0,  0,  0 ),
            # Type operations                        EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " .e0.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .e1.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .e2.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " .e3.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            # Indirect load/store group              EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " .e4.: $rD <- MEM8[$rA]",             exec.adder,    op.addr,        None,       "field_a",      "field_d", None,            "reg_2",      0,          1,      1,     0,     0,  0,  1,  0 ),
            ( " .e5.: $rD <- MEM16[$rA]",            exec.adder,    op.addr,        None,       "field_a",      "field_d", None,            "reg_2",      0,          2,      1,     0,     0,  0,  0,  1 ),
            ( " .e6.: $rD <- MEM32[$rA]",            exec.adder,    op.addr,        None,       "field_a",      "field_d", None,            "reg_2",      0,          3,      1,     0,     0,  0,  0,  0 ),
            ( " .e7.: $rD <- MEMLL32[$rA]",          exec.adder,    op.addr,        None,       "field_a",      "field_d", None,            "reg_2",      0,          3,      1,     0,     0,  0,  0,  0 ),
            ( " .e8.: MEM8[$rA] <- $rD",             exec.adder,    op.addr,        None,       "field_a",      None,      None,            "reg_2",      0,          1,      0,     1,     0,  0,  0,  0 ),
            ( " .e9.: MEM16[$rA] <- $rD",            exec.adder,    op.addr,        None,       "field_a",      None,      None,            "reg_2",      0,          2,      0,     1,     0,  0,  0,  0 ),
            ( " .ea.: MEM32[$rA] <- $rD",            exec.adder,    op.addr,        None,       "field_a",      None,      None,            "reg_2",      0,          3,      0,     1,     0,  0,  0,  0 ),
            ( " .eb.: MEMSR32[$rA] <- $rD",          exec.adder,    op.addr,        None,       "field_a",      None,      None,            "reg_2",      0,          3,      0,     1,     0,  0,  0,  0 ),
            ( " .ec.: $rD <- SMEM8[$rA]",            exec.adder,    op.addr,        None,       "field_a",      "field_d", None,            "reg_2",      0,          1,      1,     0,     1,  0,  0,  0 ),
            ( " .ed.: $rD <- SMEM16[$rA]",           exec.adder,    op.addr,        None,       "field_a",      "field_d", None,            "reg_2",      0,          2,      1,     0,     0,  1,  0,  0 ),
            # Indirect jump group                    EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " 1ee.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " 2ee.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " 3ee.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            # Offset-indirect type operations        EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " 1ee.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " 2ee.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " 3ee.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            # Offset-indirect load/store group       EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " .f4.: $rD <- MEM8[$rA+FIELD_E]",     exec.adder,    op.addr,        None,       "field_a",      "field_d", None,            "reg_2",      "field_e",  1,      1,     0,     0,  0,  1,  0 ),
            ( " .f5.: $rD <- MEM16[$rA+FIELD_E]",    exec.adder,    op.addr,        None,       "field_a",      "field_d", None,            "reg_2",      "field_e",  2,      1,     0,     0,  0,  0,  1 ),
            ( " .f6.: $rD <- MEM32[$rA+FIELD_E]",    exec.adder,    op.addr,        None,       "field_a",      "field_d", None,            "reg_2",      "field_e",  3,      1,     0,     0,  0,  0,  0 ),
            ( " .f7.: $rD <- MEMLL32[$rA+FIELD_E]",  exec.adder,    op.addr,        None,       "field_a",      "field_d", None,            "reg_2",      "field_e",  3,      1,     0,     0,  0,  0,  0 ),
            ( " .f8.: MEM8[$rA+FIELD_E] <- $rD",     exec.adder,    op.addr,        None,       "field_a",      None,      None,            "reg_2",      "field_e",  1,      0,     1,     0,  0,  0,  0 ),
            ( " .f9.: MEM16[$rA+FIELD_E] <- $rD",    exec.adder,    op.addr,        None,       "field_a",      None,      None,            "reg_2",      "field_e",  2,      0,     1,     0,  0,  0,  0 ),
            ( " .fa.: MEM32[$rA+FIELD_E] <- $rD",    exec.adder,    op.addr,        None,       "field_a",      None,      None,            "reg_2",      "field_e",  3,      0,     1,     0,  0,  0,  0 ),
            ( " .fb.: MEMSR32[$rA+FIELD_E] <- $rD",  exec.adder,    op.addr,        None,       "field_a",      None,      None,            "reg_2",      "field_e",  3,      0,     1,     0,  0,  0,  0 ),
            ( " .fc.: $rD <- SMEM8[$rA+FIELD_E]",    exec.adder,    op.addr,        None,       "field_a",      "field_d", None,            "reg_2",      "field_e",  1,      1,     0,     1,  0,  0,  0 ),
            ( " .fd.: $rD <- SMEM16[$rA+FIELD_E]",   exec.adder,    op.addr,        None,       "field_a",      "field_d", None,            "reg_2",      "field_e",  2,      1,     0,     0,  1,  0,  0 ),
            # Offset-indirect jump group             EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " 1fe.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " 2fe.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " 3fe.: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            # Absolute load/store group              EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " .f4f: $rD <- MEM8[FIELD_E]",         exec.adder,    op.addr,        None,       None,           "field_d", None,            0,            "field_e",  1,      1,     0,     0,  0,  1,  0 ),
            ( " .f5f: $rD <- MEM16[FIELD_E]",        exec.adder,    op.addr,        None,       None,           "field_d", None,            0,            "field_e",  2,      1,     0,     0,  0,  0,  1 ),
            ( " .f6f: $rD <- MEM32[FIELD_E]",        exec.adder,    op.addr,        None,       None,           "field_d", None,            0,            "field_e",  3,      1,     0,     0,  0,  0,  0 ),
            ( " .f7f: $rD <- MEMLL32[FIELD_E]",      exec.adder,    op.addr,        None,       None,           "field_d", None,            0,            "field_e",  3,      1,     0,     0,  0,  0,  0 ),
            ( " .f8f: MEM8[FIELD_E] <- $rD",         exec.adder,    op.addr,        None,       None,           None,      None,            0,            "field_e",  1,      0,     1,     0,  0,  0,  0 ),
            ( " .f9f: MEM16[FIELD_E] <- $rD",        exec.adder,    op.addr,        None,       None,           None,      None,            0,            "field_e",  2,      0,     1,     0,  0,  0,  0 ),
            ( " .faf: MEM32[FIELD_E] <- $rD",        exec.adder,    op.addr,        None,       None,           None,      None,            0,            "field_e",  3,      0,     1,     0,  0,  0,  0 ),
            ( " .fbf: MEMSR32[FIELD_E] <- $rD",      exec.adder,    op.addr,        None,       None,           None,      None,            0,            "field_e",  3,      0,     1,     0,  0,  0,  0 ),
            ( " .fcf: $rD <- SMEM8[FIELD_E]",        exec.adder,    op.addr,        None,       None,           "field_d", None,            0,            "field_e",  1,      1,     0,     1,  0,  0,  0 ),
            ( " .fdf: $rD <- SMEM16[FIELD_E]",       exec.adder,    op.addr,        None,       None,           "field_d", None,            0,            "field_e",  2,      1,     0,     0,  1,  0,  0 ),
            # Absolute jump group                    EXEC_UNIT      OP_CODE         RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_IMM      MEM_LEN IS_LD  IS_ST  BSE WSE BZE WZE
            ( " 1fef: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " 2fef: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
            ( " 3fef: SII",                          exec.misc,     op.misc_swi,    None,       None,           None,      7,               None,         None,       None,   0,     0,     0,  0,  0,  0 ),
        )