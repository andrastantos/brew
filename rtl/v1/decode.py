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
    from .scan import ScanWrapper
    from .synth import *
    from .assembler import *
except ImportError:
    from brew_types import *
    from brew_utils import *
    from scan import ScanWrapper
    from synth import *
    from assembler import *

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
the next instruction. There's no harm in using the instruction fields: while they're invalid, or may even
contain some sensitive info, they don't produce side-effects: all they do is to generate some mux changes
that then will be ignored in execute. However, we *have* to make sure that the output register enable is
de-asserted, so no matter what, no write-back will occur. Execute assume assumes that.

HW interrupts are dealt with in the execute stage.
"""

"""
We need to include an extra pipeline stage, unfortunately. What we have is:

1. Instr. capture in ForwardBuf
2. Issue of register request
3. Receipt of register response; issue to execute.

Since all the fields (except for field_e) is ready at (2), we need to register them for (3)

"""

class DecodeStage(GenericModule):
    clk = ClkPort()
    rst = RstPort()

    fetch = Input(FetchDecodeIf)
    output_port = Output(DecodeExecIf)

    # Interface to the register file
    reg_file_req = Output(RegFileReadRequestIf)
    reg_file_rsp = Input(RegFileReadResponseIf)

    do_branch = Input(logic)

    break_fetch_burst = Output(logic)

    def construct(self, has_multiply: bool = True, has_shift: bool = True, use_mini_table: bool = False):
        self.has_multiply = has_multiply
        self.has_shift = has_shift
        self.use_mini_table = use_mini_table

    def body(self):
        # IMPORTANT!!!!! It is very important that the ISA is such that the reset state of inst_word (0x0000) decodes to a 16-bit instruction. This means that
        # coming out of reset, we think that we have all the fields and allow for the next instruction to come in.

        stage_2_full = Wire(logic)
        stage_2_has_all_fields = Wire(logic)
        register_outputs = Wire(logic)
        release_outputs = Wire(logic)

        ########### DEAL WITH ASSEMBLING THE INSTRUCTION

        inst_word_in = Wire(FetchDecodeIf)
        inst_word_out = Wire(FetchDecodeIf)

        inst_word = inst_word_out.data
        inst_word_av = inst_word_out.av

        field_d = Wire()
        field_c = Wire()
        field_b = Wire()
        field_a = Wire()
        field_e = Wire()

        have_all_fields = Wire()

        inst_word_in.valid <<= self.fetch.valid
        self.fetch.ready <<= inst_word_in.ready | (~have_all_fields & (~stage_2_full | ~stage_2_has_all_fields | release_outputs))
        inst_word_in.data <<= self.fetch.data
        inst_word_in.av <<= self.fetch.av

        inst_word_out <<= ForwardBuf(inst_word_in, clear=self.do_branch)

        multi_word_inst = \
            (field_d == 0xf) | \
            ((field_c == 0xf) & ((field_b != 0xf) | (field_a == 0xf))) | \
            ((field_c == 0xe) &                     (field_a == 0xf)) | \
            ((field_c  < 0xc) & ((field_b == 0xf) | (field_a == 0xf)))

        inst_32_bit = (field_d == 0xf) | (field_a != 0xf)

        # 0 -> 16 bits, 1 -> 32 bits, 2 -> 48 bits
        inst_len = concat(
            multi_word_inst & ~inst_32_bit,
            multi_word_inst &  inst_32_bit
        )

        have_field_e_low = Wire(logic)
        need_field_e_low = inst_len != inst_len_16
        capture_field_e_low = need_field_e_low  & ~have_field_e_low & inst_word_out.valid & self.fetch.valid & self.fetch.ready
        have_field_e_high = Wire(logic)
        need_field_e_high = inst_len == inst_len_48
        capture_field_e_high = need_field_e_high & ~have_field_e_high & have_field_e_low & inst_word_out.valid & self.fetch.valid & self.fetch.ready

        field_e_low   = Reg(self.fetch.data, clock_en = capture_field_e_low)
        field_e_high  = Reg(self.fetch.data, clock_en = capture_field_e_high)

        have_field_e_low <<= Reg(Select(
            self.do_branch,
            Select(
                capture_field_e_low,
                Select(
                    self.output_port.ready & self.output_port.valid,
                    have_field_e_low,
                    0
                ),
                1
            ),
            0
        ))

        have_field_e_high <<= Reg(Select(
            self.do_branch,
            Select(
                capture_field_e_high,
                Select(
                    self.output_port.ready & self.output_port.valid,
                    have_field_e_high,
                    0
                ),
                1
            ),
            0
        ))

        have_all_fields <<= SelectOne(
            inst_len == inst_len_16, 1,
            inst_len == inst_len_32, have_field_e_low,
            inst_len == inst_len_48, have_field_e_high
        ) | inst_word_av

        ############ DECODE INSTRUCTION

        field_d <<= inst_word[15:12]
        field_c <<= inst_word[11:8]
        field_b <<= inst_word[7:4]
        field_a <<= inst_word[3:0]
        field_e <<= Select(
            inst_len == inst_len_48,
            concat(
                field_e_low[15], field_e_low[15], field_e_low[15], field_e_low[15],
                field_e_low[15], field_e_low[15], field_e_low[15], field_e_low[15],
                field_e_low[15], field_e_low[15], field_e_low[15], field_e_low[15],
                field_e_low[15], field_e_low[15], field_e_low[15], field_e_low[15],
                field_e_low
            ),
            concat(field_e_high, field_e_low)
        )

        field_a_is_f = field_a == 0xf
        field_b_is_f = field_b == 0xf
        field_c_is_f = field_c == 0xf
        field_d_is_f = field_d == 0xf

        tiny_ofs = Wire(Unsigned(32))
        tiny_ofs <<= concat(*(inst_word[7], )*23, inst_word[7:1], "2'b0")
        tiny_field_a = 12 | inst_word[0]

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
        ones_field_a_2x = concat(ones_field_a[30:0], "1'b0")

        # Codes: 0123456789abcde <- exact match to that digit
        #        . <- anything but 0xf
        #        * <- anything, including 0xf
        #        < <- less then subsequent digit
        #        > <- greater than subsequent digit (but not 0xf)
        #        : <- anything after that is comment
        #        $ <- part of the mini set (for fast sims)
        # Fields:
        #    exec_unit = EnumNet(op_class)
        #    alu_op = EnumNet(alu_ops)
        #    shifter_op = EnumNet(shifter_ops)
        #    branch_op = EnumNet(branch_ops)
        #    ldst_op = EnumNet(ldst_ops)
        #    op_a = BrewData
        #    op_b = BrewData
        #    op_c = BrewData
        #    mem_access_len = Unsigned(2) # 0 for 8-bit, 1 for 16-bit, 2 for 32-bit
        #    inst_len = Unsigned(2)
        #    do_bse = logic
        #    do_wse = logic
        #    do_bze = logic
        #    do_wze = logic
        #    result_reg_addr = BrewRegAddr
        #    result_reg_addr_valid = logic
        #    fetch_av = logic
        bo = branch_ops
        ao = alu_ops
        so = shifter_ops
        lo = ldst_ops
        oc = op_class
        a32 = access_len_32
        a16 = access_len_16
        a8  = access_len_8
        #      CODE                                  EXEC_UNIT    ALU_OP        SHIFTER_OP   BRANCH_OP    LDST_OP    RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_C        MEM_LEN BSE WSE BZE WZE
        SII =                                       (oc.branch,   None,         None,        bo.swi,      None,      None,       None,           None,      7,               None,         None,       None,   0,  0,  0,  0 )
        if self.has_shift:
            shift_ops = (
                #  CODE                                  EXEC_UNIT    ALU_OP        SHIFTER_OP   BRANCH_OP    LDST_OP    RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_C        MEM_LEN BSE WSE BZE WZE
                ( "  .6..: $rD <- $rA << $rB",            oc.shift,    None,         so.shll,     None,        None,      field_a,    field_b,        field_d,   "REG",           "REG",        None,       None,   0,  0,  0,  0 ),
                ( "  .7..: $rD <- $rA >> $rB",            oc.shift,    None,         so.shlr,     None,        None,      field_a,    field_b,        field_d,   "REG",           "REG",        None,       None,   0,  0,  0,  0 ),
                ( "  .8..: $rD <- $rA >>> $rB",           oc.shift,    None,         so.shar,     None,        None,      field_a,    field_b,        field_d,   "REG",           "REG",        None,       None,   0,  0,  0,  0 ),
                ( "  .6.f: $rD <- FIELD_E << $rB",        oc.shift,    None,         so.shll,     None,        None,      None,       field_b,        field_d,   "FIELD_E",       "REG",        None,       None,   0,  0,  0,  0 ),
                ( "  .7.f: $rD <- FIELD_E >> $rB",        oc.shift,    None,         so.shlr,     None,        None,      None,       field_b,        field_d,   "FIELD_E",       "REG",        None,       None,   0,  0,  0,  0 ),
                ( "  .8.f: $rD <- FIELD_E >>> $rB",       oc.shift,    None,         so.shar,     None,        None,      None,       field_b,        field_d,   "FIELD_E",       "REG",        None,       None,   0,  0,  0,  0 ),
                ( "  .6f.: $rD <- $rA << FIELD_E",        oc.shift,    None,         so.shll,     None,        None,      field_a,    None,           field_d,   "REG",           "FIELD_E",    None,       None,   0,  0,  0,  0 ),
                ( "  .7f.: $rD <- $rA >> FIELD_E",        oc.shift,    None,         so.shlr,     None,        None,      field_a,    None,           field_d,   "REG",           "FIELD_E",    None,       None,   0,  0,  0,  0 ),
                ( "  .8f.: $rD <- $rA >>> FIELD_E",       oc.shift,    None,         so.shar,     None,        None,      field_a,    None,           field_d,   "REG",           "FIELD_E",    None,       None,   0,  0,  0,  0 ),
            )
        else:
            shift_ops = (
                ( "  .6..: $rD <- $rA << $rB",            *SII),
                ( "  .7..: $rD <- $rA >> $rB",            *SII),
                ( "  .8..: $rD <- $rA >>> $rB",           *SII),
                ( "  .6.f: $rD <- FIELD_E << $rB",        *SII),
                ( "  .7.f: $rD <- FIELD_E >> $rB",        *SII),
                ( "  .8.f: $rD <- FIELD_E >>> $rB",       *SII),
                ( "  .6f.: $rD <- FIELD_E << $rA",        *SII),
                ( "  .7f.: $rD <- FIELD_E >> $rA",        *SII),
                ( "  .8f.: $rD <- FIELD_E >>> $rA",       *SII),
            )
        if self.has_multiply:
            mult_ops = (
                #  CODE                                  EXEC_UNIT    ALU_OP        SHIFTER_OP   BRANCH_OP    LDST_OP    RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B          OP_C        MEM_LEN BSE WSE BZE WZE
                ( "  .9..: $rD <- $rA * $rB",             oc.mult,     None,         None,        None,        None,      field_a,    field_b,        field_d,   "REG",           "REG",        None,       None,   0,  0,  0,  0 ),
                ( "  .9.f: $rD <- FIELD_E * $rB",         oc.mult,     None,         None,        None,        None,      None,       field_b,        field_d,   "FIELD_E",       "REG",        None,       None,   0,  0,  0,  0 ),
                ( "  .9f.: $rD <- FIELD_E * $rA",         oc.mult,     None,         None,        None,        None,      None,       field_a,        field_d,   "FIELD_E",       "REG",        None,       None,   0,  0,  0,  0 ),
            )
        else:
            mult_ops = (
                ( "  .9..: $rD <- $rA * $rB",             *SII),
                ( "  .9.f: $rD <- FIELD_E * $rB",         *SII),
                ( "  .9f.: $rD <- FIELD_E * $rA",         *SII),
            )
        full_inst_table = (
            *shift_ops,
            *mult_ops,
            #  Exception group                       EXEC_UNIT    ALU_OP        SHIFTER_OP   BRANCH_OP    LDST_OP    RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B             OP_C        MEM_LEN BSE WSE BZE WZE
            ( "$<8000: SWI",                          oc.branch,   None,         None,        bo.swi,      None,      None,       None,           None,      field_d,         None,            None,       None,   0,  0,  0,  0 ),
            ( "  8000: STM",                          oc.branch,   None,         None,        bo.stm,      None,      None,       None,           None,      None,            None,            None,       None,   0,  0,  0,  0 ),
            ( "  9000: WOI",                          oc.branch,   ao.a_minus_b, None,        bo.cb_eq,    None,      field_a,    field_b,        None,      "REG",           "REG",           0,          None,   0,  0,  0,  0 ), # Decoded as 'if $0 == $0 $pc <- $pc'
            ( " >9000: SII",                          *SII),
            #  PC manipulation group                 EXEC_UNIT    ALU_OP        SHIFTER_OP   BRANCH_OP    LDST_OP    RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B             OP_C        MEM_LEN BSE WSE BZE WZE
            ( "  .001: FENCE",                        oc.alu,      None,         None,        None,        None,      None,       None,           None,      None,            None,            None,       None,   0,  0,  0,  0 ), # Decoded as a kind of NOP
            ( "$ .002: $pc <- $rD",                   oc.branch,   None,         None,        bo.pc_w,     None,      field_d,    None,           None,      "REG",           None,            None,       None,   0,  0,  0,  0 ),
            ( "  .003: $tpc <- $rD",                  oc.branch,   None,         None,        bo.tpc_w,    None,      field_d,    None,           None,      "REG",           None,            None,       None,   0,  0,  0,  0 ),
            ( "$ .004: $rD <- $pc",                   oc.alu,      ao.pc_plus_b, None,        None,        None,      None,       None,           field_d,   None,            0,               None,       None,   0,  0,  0,  0 ),
            ( "  .005: $rD <- $tpc",                  oc.alu,      ao.tpc,       None,        None,        None,      None,       None,           field_d,   None,            None,            None,       None,   0,  0,  0,  0 ),
            ( "  .00>5: SII",                         *SII),
            # Unary group                            EXEC_UNIT    ALU_OP        SHIFTER_OP   BRANCH_OP    LDST_OP    RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B             OP_C        MEM_LEN BSE WSE BZE WZE
            ( "$ .01.: $rD <- tiny FIELD_A",          oc.alu,      ao.a_or_b,    None,        None,        None,      None,       None,           field_d,   0,               ones_field_a,    None,       None,   0,  0,  0,  0 ),
            ( "  .02.: $rD <- $pc + FIELD_A*2",       oc.alu,      ao.pc_plus_b, None,        None,        None,      None,       None,           field_d,   None,            ones_field_a_2x, None,       None,   0,  0,  0,  0 ),
            ( "  .03.: $rD <- -$rA",                  oc.alu,      ao.a_minus_b, None,        None,        None,      None,       field_a,        field_d,   0,               "REG",           None,       None,   0,  0,  0,  0 ),
            ( "$ .04.: $rD <- ~$rA",                  oc.alu,      ao.a_xor_b,   None,        None,        None,      None,       field_a,        field_d,   0xffffffff,      "REG",           None,       None,   0,  0,  0,  0 ),
            ( "  .05.: $rD <- bse $rA",               oc.alu,      ao.a_or_b,    None,        None,        None,      None,       field_a,        field_d,   0,               "REG",           None,       None,   1,  0,  0,  0 ),
            ( "  .06.: $rD <- wse $rA",               oc.alu,      ao.a_or_b,    None,        None,        None,      None,       field_a,        field_d,   0,               "REG",           None,       None,   0,  1,  0,  0 ),
            ( "  .0>6.: SII",                         *SII),
            ## Binary ALU group                      EXEC_UNIT    ALU_OP        SHIFTER_OP   BRANCH_OP    LDST_OP    RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B             OP_C        MEM_LEN BSE WSE BZE WZE
            ( "  .1..: $rD <- $rA ^ $rB",             oc.alu,      ao.a_xor_b,   None,        None,        None,      field_a,    field_b,        field_d,   "REG",           "REG",           None,       None,   0,  0,  0,  0 ),
            ( "$ .2..: $rD <- $rA | $rB",             oc.alu,      ao.a_or_b,    None,        None,        None,      field_a,    field_b,        field_d,   "REG",           "REG",           None,       None,   0,  0,  0,  0 ),
            ( "  .3..: $rD <- $rA & $rB",             oc.alu,      ao.a_and_b,   None,        None,        None,      field_a,    field_b,        field_d,   "REG",           "REG",           None,       None,   0,  0,  0,  0 ),
            ( "  .4..: $rD <- $rA + $rB",             oc.alu,      ao.a_plus_b,  None,        None,        None,      field_a,    field_b,        field_d,   "REG",           "REG",           None,       None,   0,  0,  0,  0 ),
            ( "  .5..: $rD <- $rA - $rB",             oc.alu,      ao.a_minus_b, None,        None,        None,      field_a,    field_b,        field_d,   "REG",           "REG",           None,       None,   0,  0,  0,  0 ),
            ( "  .a..: $rD <- ~$rA & $rB",            oc.alu,      ao.n_b_and_a, None,        None,        None,      field_a,    field_b,        field_d,   "REG",           "REG",           None,       None,   0,  0,  0,  0 ),
            ( "  .b..: $rD <- tiny $rB + FIELD_A",    oc.alu,      ao.a_plus_b,  None,        None,        None,      field_b,    None,           field_d,   "REG",           ones_field_a,    None,       None,   0,  0,  0,  0 ),
            # Load immediate group                   EXEC_UNIT    ALU_OP        SHIFTER_OP   BRANCH_OP    LDST_OP    RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B             OP_C        MEM_LEN BSE WSE BZE WZE
            ( "$ .00f: $rD <- VALUE",                 oc.alu,      ao.a_or_b,    None,        None,        None,      None,       None,           field_d,   "FIELD_E",       0,               None,       None,   0,  0,  0,  0 ),
            ( "  20ef: $pc <- VALUE",                 oc.branch,   None,         None,        bo.pc_w,     None,      None,       None,           None,      "FIELD_E",       None,            None,       None,   0,  0,  0,  0 ),
            ( "  30ef: $tpc <- VALUE",                oc.branch,   None,         None,        bo.tpc_w,    None,      None,       None,           None,      "FIELD_E",       None,            None,       None,   0,  0,  0,  0 ),
            ( "  80ef.: SII",                         *SII),
            ( "  90ef.: SII",                         *SII),
            # Constant ALU group                     EXEC_UNIT    ALU_OP        SHIFTER_OP   BRANCH_OP    LDST_OP    RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B             OP_C        MEM_LEN BSE WSE BZE WZE
            ( "  .1.f: $rD <- FIELD_E ^ $rB",         oc.alu,      ao.a_xor_b,   None,        None,        None,      None,       field_b,        field_d,   "FIELD_E",       "REG",           None,       None,   0,  0,  0,  0 ),
            ( "  .2.f: $rD <- FIELD_E | $rB",         oc.alu,      ao.a_or_b,    None,        None,        None,      None,       field_b,        field_d,   "FIELD_E",       "REG",           None,       None,   0,  0,  0,  0 ),
            ( "$ .3.f: $rD <- FIELD_E & $rB",         oc.alu,      ao.a_and_b,   None,        None,        None,      None,       field_b,        field_d,   "FIELD_E",       "REG",           None,       None,   0,  0,  0,  0 ),
            ( "  .4.f: $rD <- FIELD_E + $rB",         oc.alu,      ao.a_plus_b,  None,        None,        None,      None,       field_b,        field_d,   "FIELD_E",       "REG",           None,       None,   0,  0,  0,  0 ),
            ( "  .5.f: $rD <- FIELD_E - $rB",         oc.alu,      ao.a_minus_b, None,        None,        None,      None,       field_b,        field_d,   "FIELD_E",       "REG",           None,       None,   0,  0,  0,  0 ),
            ( "  .a.f: SII",                          *SII),
            ( "  .b.f: SII",                          *SII),
            # Short load immediate group             EXEC_UNIT    ALU_OP        SHIFTER_OP   BRANCH_OP    LDST_OP    RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B             OP_C        MEM_LEN BSE WSE BZE WZE
            ( "$ .0f0: $rD <- short VALUE",           oc.alu,      ao.a_or_b,    None,        None,        None,      None,       None,           field_d,   "FIELD_E",       0,               None,       None,   0,  0,  0,  0 ),
            ( "  20fe: $pc <- short VALUE",           oc.branch,   None,         None,        bo.pc_w,     None,      None,       None,           None,      "FIELD_E",       None,            None,       None,   0,  0,  0,  0 ),
            ( "  30fe: $tpc <- short VALUE",          oc.branch,   None,         None,        bo.tpc_w,    None,      None,       None,           None,      "FIELD_E",       None,            None,       None,   0,  0,  0,  0 ),
            # Short constant ALU group               EXEC_UNIT    ALU_OP        SHIFTER_OP   BRANCH_OP    LDST_OP    RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B             OP_C        MEM_LEN BSE WSE BZE WZE
            ( "  .1f.: $rD <- FIELD_E ^ $rA",         oc.alu,      ao.a_xor_b,   None,        None,        None,      None,       field_a,        field_d,   "FIELD_E",       "REG",           None,       None,   0,  0,  0,  0 ),
            ( "  .2f.: $rD <- FIELD_E | $rA",         oc.alu,      ao.a_or_b,    None,        None,        None,      None,       field_a,        field_d,   "FIELD_E",       "REG",           None,       None,   0,  0,  0,  0 ),
            ( "  .3f.: $rD <- FIELD_E & $rA",         oc.alu,      ao.a_and_b,   None,        None,        None,      None,       field_a,        field_d,   "FIELD_E",       "REG",           None,       None,   0,  0,  0,  0 ),
            ( "$ .4f.: $rD <- FIELD_E + $rA",         oc.alu,      ao.a_plus_b,  None,        None,        None,      None,       field_a,        field_d,   "FIELD_E",       "REG",           None,       None,   0,  0,  0,  0 ),
            ( "  .5f.: $rD <- FIELD_E - $rA",         oc.alu,      ao.a_minus_b, None,        None,        None,      None,       field_a,        field_d,   "FIELD_E",       "REG",           None,       None,   0,  0,  0,  0 ),
            ( "  .af.: SII",                          *SII),
            ( "  .bf.: SII",                          *SII),
            # Zero-compare conditional branch group  EXEC_UNIT    ALU_OP        SHIFTER_OP   BRANCH_OP    LDST_OP    RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B             OP_C        MEM_LEN BSE WSE BZE WZE
            ( "  f00.: if $rA == 0",                  oc.branch,   ao.a_minus_b, None,        bo.cb_eq,    None,      field_a,    None,           None,      "REG",           0,               "FIELD_E",  None,   0,  0,  0,  0 ),
            ( "  f01.: if $rA != 0",                  oc.branch,   ao.a_minus_b, None,        bo.cb_ne,    None,      field_a,    None,           None,      "REG",           0,               "FIELD_E",  None,   0,  0,  0,  0 ),
            ( "  f02.: if $rA < 0",                   oc.branch,   ao.a_minus_b, None,        bo.cb_lts,   None,      field_a,    None,           None,      "REG",           0,               "FIELD_E",  None,   0,  0,  0,  0 ),
            ( "  f03.: if $rA >= 0",                  oc.branch,   ao.a_minus_b, None,        bo.cb_ges,   None,      field_a,    None,           None,      "REG",           0,               "FIELD_E",  None,   0,  0,  0,  0 ),
            ( "  f04.: if $rA > 0",                   oc.branch,   ao.a_minus_b, None,        bo.cb_lts,   None,      None,       field_a,        None,      0,               "REG",           "FIELD_E",  None,   0,  0,  0,  0 ),
            ( "  f05.: if $rA <= 0",                  oc.branch,   ao.a_minus_b, None,        bo.cb_ges,   None,      None,       field_a,        None,      0,               "REG",           "FIELD_E",  None,   0,  0,  0,  0 ),
            ( "  f06.: SII",                          *SII),
            ( "  f07.: SII",                          *SII),
            ( "  f08.: if $rA == 0",                  oc.branch,   ao.a_minus_b, None,        bo.cb_eq,    None,      field_a,    None,           None,      "REG",           0,               "FIELD_E",  None,   0,  0,  0,  0 ),
            ( "  f09.: if $rA != 0",                  oc.branch,   ao.a_minus_b, None,        bo.cb_ne,    None,      field_a,    None,           None,      "REG",           0,               "FIELD_E",  None,   0,  0,  0,  0 ),
            ( "  f0a.: if $rA < 0",                   oc.branch,   ao.a_minus_b, None,        bo.cb_lts,   None,      field_a,    None,           None,      "REG",           0,               "FIELD_E",  None,   0,  0,  0,  0 ),
            ( "  f0b.: if $rA >= 0",                  oc.branch,   ao.a_minus_b, None,        bo.cb_ges,   None,      field_a,    None,           None,      "REG",           0,               "FIELD_E",  None,   0,  0,  0,  0 ),
            ( "  f0c.: if $rA > 0",                   oc.branch,   ao.a_minus_b, None,        bo.cb_lts,   None,      None,       field_a,        None,      0,               "REG",           "FIELD_E",  None,   0,  0,  0,  0 ),
            ( "  f0d.: if $rA <= 0",                  oc.branch,   ao.a_minus_b, None,        bo.cb_ges,   None,      None,       field_a,        None,      0,               "REG",           "FIELD_E",  None,   0,  0,  0,  0 ),
            ( "  f0e.: SII",                          *SII),
            # Conditional branch group               EXEC_UNIT    ALU_OP        SHIFTER_OP   BRANCH_OP    LDST_OP    RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B             OP_C        MEM_LEN BSE WSE BZE WZE
            ( "  f1..: if $rB == $rA",                oc.branch,   ao.a_minus_b, None,        bo.cb_eq,    None,      field_b,    field_a,        None,      "REG",           "REG",           "FIELD_E",  None,   0,  0,  0,  0 ),
            ( "  f2..: if $rB != $rA",                oc.branch,   ao.a_minus_b, None,        bo.cb_ne,    None,      field_b,    field_a,        None,      "REG",           "REG",           "FIELD_E",  None,   0,  0,  0,  0 ),
            ( "  f3..: if signed $rB < $rA",          oc.branch,   ao.a_minus_b, None,        bo.cb_lts,   None,      field_b,    field_a,        None,      "REG",           "REG",           "FIELD_E",  None,   0,  0,  0,  0 ),
            ( "  f4..: if signed $rB >= $rA",         oc.branch,   ao.a_minus_b, None,        bo.cb_ges,   None,      field_b,    field_a,        None,      "REG",           "REG",           "FIELD_E",  None,   0,  0,  0,  0 ),
            ( "  f5..: if $rB < $rA",                 oc.branch,   ao.a_minus_b, None,        bo.cb_lt,    None,      field_b,    field_a,        None,      "REG",           "REG",           "FIELD_E",  None,   0,  0,  0,  0 ),
            ( "  f6..: if $rB >= $rA",                oc.branch,   ao.a_minus_b, None,        bo.cb_ge,    None,      field_b,    field_a,        None,      "REG",           "REG",           "FIELD_E",  None,   0,  0,  0,  0 ),
            ( "  f7..: SII",                          *SII),
            ( "  f8..: SII",                          *SII),
            ( "  f9..: if $rB == $rA",                oc.branch,   ao.a_minus_b, None,        bo.cb_eq,    None,      field_b,    field_a,        None,      "REG",           "REG",           "FIELD_E",  None,   0,  0,  0,  0 ),
            ( "  fa..: if $rB != $rA",                oc.branch,   ao.a_minus_b, None,        bo.cb_ne,    None,      field_b,    field_a,        None,      "REG",           "REG",           "FIELD_E",  None,   0,  0,  0,  0 ),
            ( "  fb..: if signed $rB < $rA",          oc.branch,   ao.a_minus_b, None,        bo.cb_lts,   None,      field_b,    field_a,        None,      "REG",           "REG",           "FIELD_E",  None,   0,  0,  0,  0 ),
            ( "  fc..: if signed $rB >= $rA",         oc.branch,   ao.a_minus_b, None,        bo.cb_ges,   None,      field_b,    field_a,        None,      "REG",           "REG",           "FIELD_E",  None,   0,  0,  0,  0 ),
            ( "  fd..: if $rB < $rA",                 oc.branch,   ao.a_minus_b, None,        bo.cb_lt,    None,      field_b,    field_a,        None,      "REG",           "REG",           "FIELD_E",  None,   0,  0,  0,  0 ),
            ( "  fe..: if $rB >= $rA",                oc.branch,   ao.a_minus_b, None,        bo.cb_ge,    None,      field_b,    field_a,        None,      "REG",           "REG",           "FIELD_E",  None,   0,  0,  0,  0 ),
            # Bit-set-test branch group              EXEC_UNIT    ALU_OP        SHIFTER_OP   BRANCH_OP    LDST_OP    RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B             OP_C        MEM_LEN BSE WSE BZE WZE
            ( "  f.f.: if $rA[.]  == 1",              oc.branch,   None,         None,        bo.bb_one,   None,      field_a,    None,           None,      "REG",           field_c,         "FIELD_E",  None,   0,  0,  0,  0 ),
            ( "  f..f: if $rB[.]  == 0",              oc.branch,   None,         None,        bo.bb_zero,  None,      field_b,    None,           None,      "REG",           field_c,         "FIELD_E",  None,   0,  0,  0,  0 ),
            # Stack group                            EXEC_UNIT    ALU_OP        SHIFTER_OP   BRANCH_OP    LDST_OP    RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B             OP_C        MEM_LEN BSE WSE BZE WZE
            ( "$ .c**: MEM[$rA+tiny OFS*4] <- $rD",   oc.ld_st,    None,         None,        None,        lo.store,  field_d,    tiny_field_a,   None,      "REG",           "REG",           tiny_ofs,   a32,    0,  0,  0,  0 ),
            ( "$ .d**: $rD <- MEM[$rA+tiny OFS*4]",   oc.ld_st,    None,         None,        None,        lo.load,   None,       tiny_field_a,   field_d,   None,            "REG",           tiny_ofs,   a32,    0,  0,  0,  0 ),
            # Type operations                        EXEC_UNIT    ALU_OP        SHIFTER_OP   BRANCH_OP    LDST_OP    RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B             OP_C        MEM_LEN BSE WSE BZE WZE
            ( "  .e0.: SII",                          *SII),
            ( "  .e1.: SII",                          *SII),
            ( "  .e2.: SII",                          *SII),
            ( "  .e3.: SII",                          *SII),
            # Indirect load/store group              EXEC_UNIT    ALU_OP        SHIFTER_OP   BRANCH_OP    LDST_OP    RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B             OP_C        MEM_LEN BSE WSE BZE WZE
            ( "$ .e4.: $rD <- MEM8[$rA]",             oc.ld_st,    None,         None,        None,        lo.load,   None,       field_a,        field_d,   None,            "REG",           0,          a8,     0,  0,  1,  0 ),
            ( "  .e5.: $rD <- MEM16[$rA]",            oc.ld_st,    None,         None,        None,        lo.load,   None,       field_a,        field_d,   None,            "REG",           0,          a16,    0,  0,  0,  1 ),
            ( "  .e6.: $rD <- MEM32[$rA]",            oc.ld_st,    None,         None,        None,        lo.load,   None,       field_a,        field_d,   None,            "REG",           0,          a32,    0,  0,  0,  0 ),
            ( "  .e7.: $rD <- MEMLL32[$rA]",          oc.ld_st,    None,         None,        None,        lo.load,   None,       field_a,        field_d,   None,            "REG",           0,          a32,    0,  0,  0,  0 ),
            ( "$ .e8.: MEM8[$rA] <- $rD",             oc.ld_st,    None,         None,        None,        lo.store,  field_d,    field_a,        None,      "REG",           "REG",           0,          a8,     0,  0,  0,  0 ),
            ( "  .e9.: MEM16[$rA] <- $rD",            oc.ld_st,    None,         None,        None,        lo.store,  field_d,    field_a,        None,      "REG",           "REG",           0,          a16,    0,  0,  0,  0 ),
            ( "  .ea.: MEM32[$rA] <- $rD",            oc.ld_st,    None,         None,        None,        lo.store,  field_d,    field_a,        None,      "REG",           "REG",           0,          a32,    0,  0,  0,  0 ),
            ( "  .eb.: MEMSR32[$rA] <- $rD",          oc.ld_st,    None,         None,        None,        lo.store,  field_d,    field_a,        None,      "REG",           "REG",           0,          a32,    0,  0,  0,  0 ),
            ( "  .ec.: $rD <- SMEM8[$rA]",            oc.ld_st,    None,         None,        None,        lo.load,   None,       field_a,        field_d,   None,            "REG",           0,          a8,     1,  0,  0,  0 ),
            ( "  .ed.: $rD <- SMEM16[$rA]",           oc.ld_st,    None,         None,        None,        lo.load,   None,       field_a,        field_d,   None,            "REG",           0,          a16,    0,  1,  0,  0 ),
            # Indirect jump group                    EXEC_UNIT    ALU_OP        SHIFTER_OP   BRANCH_OP    LDST_OP    RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B             OP_C        MEM_LEN BSE WSE BZE WZE
            ( "  1ee.: SII",                          *SII),
            ( "  2ee.: SII",                          *SII),
            ( "  3ee.: SII",                          *SII),
            # Offset-indirect type operations        EXEC_UNIT    ALU_OP        SHIFTER_OP   BRANCH_OP    LDST_OP    RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B             OP_C        MEM_LEN BSE WSE BZE WZE
            ( "  1ee.: SII",                          *SII),
            ( "  2ee.: SII",                          *SII),
            ( "  3ee.: SII",                          *SII),
            # Offset-indirect load/store group       EXEC_UNIT    ALU_OP        SHIFTER_OP   BRANCH_OP    LDST_OP    RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B             OP_C        MEM_LEN BSE WSE BZE WZE
            ( "  .f4.: $rD <- MEM8[$rA+FIELD_E]",     oc.ld_st,    None,         None,        None,        lo.load,   None,       field_a,        field_d,   None,            "REG",           "FIELD_E",  a8,     0,  0,  1,  0 ),
            ( "  .f5.: $rD <- MEM16[$rA+FIELD_E]",    oc.ld_st,    None,         None,        None,        lo.load,   None,       field_a,        field_d,   None,            "REG",           "FIELD_E",  a16,    0,  0,  0,  1 ),
            ( "  .f6.: $rD <- MEM32[$rA+FIELD_E]",    oc.ld_st,    None,         None,        None,        lo.load,   None,       field_a,        field_d,   None,            "REG",           "FIELD_E",  a32,    0,  0,  0,  0 ),
            ( "  .f7.: $rD <- MEMLL32[$rA+FIELD_E]",  oc.ld_st,    None,         None,        None,        lo.load,   None,       field_a,        field_d,   None,            "REG",           "FIELD_E",  a32,    0,  0,  0,  0 ),
            ( "  .f8.: MEM8[$rA+FIELD_E] <- $rD",     oc.ld_st,    None,         None,        None,        lo.store,  field_d,    field_a,        None,      "REG",           "REG",           "FIELD_E",  a8,     0,  0,  0,  0 ),
            ( "  .f9.: MEM16[$rA+FIELD_E] <- $rD",    oc.ld_st,    None,         None,        None,        lo.store,  field_d,    field_a,        None,      "REG",           "REG",           "FIELD_E",  a16,    0,  0,  0,  0 ),
            ( "  .fa.: MEM32[$rA+FIELD_E] <- $rD",    oc.ld_st,    None,         None,        None,        lo.store,  field_d,    field_a,        None,      "REG",           "REG",           "FIELD_E",  a32,    0,  0,  0,  0 ),
            ( "  .fb.: MEMSR32[$rA+FIELD_E] <- $rD",  oc.ld_st,    None,         None,        None,        lo.store,  field_d,    field_a,        None,      "REG",           "REG",           "FIELD_E",  a32,    0,  0,  0,  0 ),
            ( "  .fc.: $rD <- SMEM8[$rA+FIELD_E]",    oc.ld_st,    None,         None,        None,        lo.load,   None,       field_a,        field_d,   None,            "REG",           "FIELD_E",  a8,     1,  0,  0,  0 ),
            ( "  .fd.: $rD <- SMEM16[$rA+FIELD_E]",   oc.ld_st,    None,         None,        None,        lo.load,   None,       field_a,        field_d,   None,            "REG",           "FIELD_E",  a16,    0,  1,  0,  0 ),
            # Offset-indirect jump group             EXEC_UNIT    ALU_OP        SHIFTER_OP   BRANCH_OP    LDST_OP    RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B             OP_C        MEM_LEN BSE WSE BZE WZE
            ( "  1fe.: SII",                          *SII),
            ( "  2fe.: SII",                          *SII),
            ( "  3fe.: SII",                          *SII),
            # Absolute load/store group              EXEC_UNIT    ALU_OP        SHIFTER_OP   BRANCH_OP    LDST_OP    RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B             OP_C        MEM_LEN BSE WSE BZE WZE
            ( "  .f4f: $rD <- MEM8[FIELD_E]",         oc.ld_st,    None,         None,        None,        lo.load,   None,       None,           field_d,   None,            0,               "FIELD_E",  a8,     0,  0,  1,  0 ),
            ( "  .f5f: $rD <- MEM16[FIELD_E]",        oc.ld_st,    None,         None,        None,        lo.load,   None,       None,           field_d,   None,            0,               "FIELD_E",  a16,    0,  0,  0,  1 ),
            ( "  .f6f: $rD <- MEM32[FIELD_E]",        oc.ld_st,    None,         None,        None,        lo.load,   None,       None,           field_d,   None,            0,               "FIELD_E",  a32,    0,  0,  0,  0 ),
            ( "  .f7f: $rD <- MEMLL32[FIELD_E]",      oc.ld_st,    None,         None,        None,        lo.load,   None,       None,           field_d,   None,            0,               "FIELD_E",  a32,    0,  0,  0,  0 ),
            ( "  .f8f: MEM8[FIELD_E] <- $rD",         oc.ld_st,    None,         None,        None,        lo.store,  field_d,    None,           None,      "REG",           0,               "FIELD_E",  a8,     0,  0,  0,  0 ),
            ( "  .f9f: MEM16[FIELD_E] <- $rD",        oc.ld_st,    None,         None,        None,        lo.store,  field_d,    None,           None,      "REG",           0,               "FIELD_E",  a16,    0,  0,  0,  0 ),
            ( "  .faf: MEM32[FIELD_E] <- $rD",        oc.ld_st,    None,         None,        None,        lo.store,  field_d,    None,           None,      "REG",           0,               "FIELD_E",  a32,    0,  0,  0,  0 ),
            ( "  .fbf: MEMSR32[FIELD_E] <- $rD",      oc.ld_st,    None,         None,        None,        lo.store,  field_d,    None,           None,      "REG",           0,               "FIELD_E",  a32,    0,  0,  0,  0 ),
            ( "  .fcf: $rD <- SMEM8[FIELD_E]",        oc.ld_st,    None,         None,        None,        lo.load,   None,       None,           field_d,   None,            0,               "FIELD_E",  a8,     1,  0,  0,  0 ),
            ( "  .fdf: $rD <- SMEM16[FIELD_E]",       oc.ld_st,    None,         None,        None,        lo.load,   None,       None,           field_d,   None,            0,               "FIELD_E",  a16,    0,  1,  0,  0 ),
            # Absolute jump group                    EXEC_UNIT    ALU_OP        SHIFTER_OP   BRANCH_OP    LDST_OP    RD1_ADDR    RD2_ADDR        RES_ADDR   OP_A             OP_B             OP_C        MEM_LEN BSE WSE BZE WZE
            ( "  1fef: SII",                          *SII),
            ( "  2fef: SII",                          *SII),
            ( "  3fef: SII",                          *SII),
        )

        def is_mini_set(full_mask:str) -> bool:
            return full_mask.strip()[0] == "$"

        def parse_bit_mask(full_mask: str) -> Tuple[Wire, str]:
            """Create an expression that checks for the provided pattern

            Args:
                full_mask (str): bit-mask string in the format of .-s, *-s and hex digits, as described in the decode table

            Returns:
                Wire: An expression that returns '1' if the instruction code matches that pattern, '0' otherwise.
            """
            mask = full_mask.split(':')[0].strip() # Remove comment and trailing/leading spaces
            if mask[0] == "$": mask = mask[1:]
            mask = mask.strip()
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
                        ret_val = ret_val & (field > value) & ~field_is_f
                    elif do_lt:
                        ret_val = ret_val & (field < value)
                    else:
                        ret_val = ret_val & (field == value)
                else:
                    raise SyntaxErrorException(f"Unknown digit {digit} in decode mask {full_mask}")
                idx += 1
            return ret_val, ins_name

        # Field and their mapping to output signals:
        CODE       =  0    #
        EXEC_UNIT  =  1    #    exec_unit = EnumNet(op_class)
        ALU_OP     =  2    #    alu_op = EnumNet(alu_ops)
        SHIFTER_OP =  3    #    shifter_op = EnumNet(shifter_ops)
        BRANCH_OP  =  4    #    branch_op = EnumNet(branch_ops)
        LDST_OP    =  5    #    ldst_op = EnumNet(ldst_ops)
        RD1_ADDR   =  6    #
        RD2_ADDR   =  7    #
        RES_ADDR   =  8    #    result_reg_addr = BrewRegAddr
        OP_A       =  9    #    op_a = BrewData
        OP_B       = 10    #    op_b = BrewData
        OP_C       = 11    #    op_c = BrewData
        MEM_LEN    = 12    #    mem_access_len = Unsigned(2) # 0 for 8-bit, 1 for 16-bit, 2 for 32-bit
        BSE        = 13    #    do_bse = logic
        WSE        = 14    #    do_wse = logic
        BZE        = 15    #    do_bze = logic
        WZE        = 16    #    do_wze = logic

        if self.use_mini_table:
            inst_table = tuple(line for line in full_inst_table if is_mini_set(line[CODE]))
        else:
            inst_table = full_inst_table

        print("Available instructions:")
        for inst in inst_table:
            print(f"    {inst[0]}")
        mask_expressions = []
        mask_expression_names = set()
        for expr, name in (parse_bit_mask(line[CODE]) for line in inst_table):
            idx = 1
            base_name = name
            while name in mask_expression_names:
                name = f"{base_name}_{idx}"
                idx += 1
            mask_expression_names.add(name)
            setattr(self, f"mask_for_{name}", expr)
            mask_expressions.append(expr)

        # At this point we have all the required selections for the various control lines in 'inst_table' and their selection expressions in 'mask_expressions'.
        # All we need to do is to create the appropriate 'SelectOne' expressions.

        select_list_exec_unit      = []
        select_list_alu_op         = []
        select_list_shifter_op     = []
        select_list_branch_op      = []
        select_list_ldst_op        = []
        select_list_rd1_addr       = []
        select_list_rd2_addr       = []
        select_list_res_addr       = []
        select_list_op_a           = []
        select_list_op_b           = []
        select_list_use_reg_a      = []
        select_list_use_reg_b      = []
        select_list_op_c           = []
        select_list_use_field_e_a  = []
        select_list_use_field_e_b  = []
        select_list_use_field_e_c  = []
        select_list_mem_len        = []
        select_list_bse            = []
        select_list_wse            = []
        select_list_bze            = []
        select_list_wze            = []

        select_lists = (
            select_list_exec_unit,
            select_list_alu_op,
            select_list_shifter_op,
            select_list_branch_op,
            select_list_ldst_op,
            select_list_rd1_addr,
            select_list_rd2_addr,
            select_list_res_addr,
            select_list_op_a,
            select_list_op_b,
            select_list_op_c,
            select_list_mem_len,
            select_list_bse,
            select_list_wse,
            select_list_bze,
            select_list_wze,
        )

        for line, mask_expr in zip(inst_table, mask_expressions):
            for idx, (select_list, value) in enumerate(zip(select_lists, line[EXEC_UNIT:])):
                idx += 1 # We are skipping the first column, so we have to accommodate it here
                # OP_A and OP_B are somewhat special to hide the read-latency of the register file:
                # We do two-stage muxing: We mux all non-reg-file outputs, then register them, then
                # do a post-mux to swap in the register file outputs
                if idx in (OP_A, OP_B, OP_C):
                    use_reg_list = {OP_A: select_list_use_reg_a, OP_B: select_list_use_reg_b, OP_C: None}[idx]
                    use_field_e_list = {OP_A: select_list_use_field_e_a, OP_B: select_list_use_field_e_b, OP_C: select_list_use_field_e_c}[idx]
                    if isinstance(value, str):
                        if value == "REG":
                            # We rely on default_port for the post-muxes to select the non-reg-file outputs
                            use_reg_list += (mask_expr, 1)
                        elif value == "FIELD_E":
                            use_field_e_list += (mask_expr, 1)
                    else:
                        # We don't care of the pre-selector behavior in the reg-file-output cases
                        if value is not None: select_list += (mask_expr, value)
                else:
                    # Remove all the 0-s from the selectors for these fields and rely on default_ports to restore them
                    if idx in (BSE, WSE, BZE, WZE,) and value == 0:
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

        def optimize_selector(selectors, selector_name):
            # Here, we look through all the selected items and group the selectors into as few groups as possible.
            groups = dict()
            for (selector, selected) in zip(selectors[::2],selectors[1::2]):
                if selected not in groups:
                    groups[selected] = []
                groups[selected].append(selector)
            # Re-create a new list by combining all the selectors for a given group
            final_list = []
            for idx, (selected, selector_list) in enumerate(groups.items()):
                group_selector = or_gate(*selector_list)
                setattr(self, f"group_{idx+1}_for_{selector_name}", group_selector)
                final_list += (group_selector, selected)
            return final_list



        # Now that we have the selection lists, we can compose the muxes
        exec_unit      = SelectOne(*optimize_selector(select_list_exec_unit,      "exec_unit"))                       if len(select_list_exec_unit) > 0 else None
        alu_op         = SelectOne(*optimize_selector(select_list_alu_op,         "alu_op"))                          if len(select_list_alu_op) > 0 else None
        shifter_op     = SelectOne(*optimize_selector(select_list_shifter_op,     "shifter_op"))                      if len(select_list_shifter_op) > 0 else None
        branch_op      = SelectOne(*optimize_selector(select_list_branch_op,      "branch_op"))                       if len(select_list_branch_op) > 0 else None
        ldst_op        = SelectOne(*optimize_selector(select_list_ldst_op,        "ldst_op"))                         if len(select_list_ldst_op) > 0 else None
        rd1_addr       = SelectOne(*optimize_selector(select_list_rd1_addr,       "rd1_addr"))                        if len(select_list_rd1_addr) > 0 else None
        rd2_addr       = SelectOne(*optimize_selector(select_list_rd2_addr,       "rd2_addr"))                        if len(select_list_rd2_addr) > 0 else None
        rsv_addr       = SelectOne(*optimize_selector(select_list_res_addr,       "rsv_addr"))                        if len(select_list_res_addr) > 0 else None
        use_reg_a      = SelectOne(*optimize_selector(select_list_use_reg_a,      "use_reg_a"), default_port = 0)     if len(select_list_use_reg_a) > 0 else 0
        use_reg_b      = SelectOne(*optimize_selector(select_list_use_reg_b,      "use_reg_b"), default_port = 0)     if len(select_list_use_reg_b) > 0 else 0
        use_field_e_a  = SelectOne(*optimize_selector(select_list_use_field_e_a,  "use_field_e_a"), default_port = 0) if len(select_list_use_field_e_a) > 0 else 0
        use_field_e_b  = SelectOne(*optimize_selector(select_list_use_field_e_b,  "use_field_e_b"), default_port = 0) if len(select_list_use_field_e_b) > 0 else 0
        use_field_e_c  = SelectOne(*optimize_selector(select_list_use_field_e_c,  "use_field_e_c"), default_port = 0) if len(select_list_use_field_e_c) > 0 else 0
        op_a           = SelectOne(*optimize_selector(select_list_op_a,           "op_a"))                            if len(select_list_op_a) > 0 else None
        op_b           = SelectOne(*optimize_selector(select_list_op_b,           "op_b"))                            if len(select_list_op_b) > 0 else None
        op_c           = SelectOne(*optimize_selector(select_list_op_c,           "op_c"))                            if len(select_list_op_c) > 0 else None
        mem_len        = SelectOne(*optimize_selector(select_list_mem_len,        "mem_len"))                         if len(select_list_mem_len) > 0 else None
        bse            = SelectOne(*optimize_selector(select_list_bse,            "bse"), default_port = 0)           if len(select_list_bse) > 0 else 0
        wse            = SelectOne(*optimize_selector(select_list_wse,            "wse"), default_port = 0)           if len(select_list_wse) > 0 else 0
        bze            = SelectOne(*optimize_selector(select_list_bze,            "bze"), default_port = 0)           if len(select_list_bze) > 0 else 0
        wze            = SelectOne(*optimize_selector(select_list_wze,            "wze"), default_port = 0)           if len(select_list_wze) > 0 else 0

        read1_needed = Select(inst_word_av, SelectOne(*optimize_selector(select_list_read1_needed, "read1_needed"), default_port=0) if len(select_list_read1_needed) > 0 else 0, 0)
        read2_needed = Select(inst_word_av, SelectOne(*optimize_selector(select_list_read2_needed, "read2_needed"), default_port=0) if len(select_list_read2_needed) > 0 else 0, 0)
        rsv_needed   = Select(inst_word_av, SelectOne(*optimize_selector(select_list_rsv_needed,   "rsv_needed"),   default_port=0) if len(select_list_rsv_needed)   > 0 else 0, 0)

        ########## DEAL WITH RESERVATION AND HANDSHAKE TO EXECUTE

        # We let the register file handle the hand-shaking for us. We just need to implement the output buffers
        #inst_word_out.ready <<= self.reg_file_req.ready & have_all_fields
        #inst_word_out.ready <<= self.reg_file_req.ready
        inst_word_out.ready <<= self.output_port.ready & have_all_fields & self.reg_file_req.ready
        # We have to create a different valid register for the reg file request as we only clear inst_word_out.valid
        # when we deliver to execute.
        # We need to request from the reg_file only once.
        reg_file_req_valid = Wire(logic)
        reg_file_req_valid <<= Reg(
            Select(
                inst_word_in.ready & inst_word_in.valid & ~self.do_branch,
                # We don't take a new input
                Select(
                    self.reg_file_req.ready | self.do_branch,
                    reg_file_req_valid,
                    0
                ),
                # We do request a new input
                1
            )
        )
        def Stage2Reg(input): return Reg(input, clock_en=register_outputs)

        self.reg_file_req.valid <<= reg_file_req_valid

        self.output_port.valid <<= self.reg_file_rsp.valid & (stage_2_has_all_fields | have_all_fields)
        self.reg_file_rsp.ready <<= self.output_port.ready & (stage_2_has_all_fields | have_all_fields)

        self.reg_file_req.read1_addr  <<= BrewRegAddr(rd1_addr)
        self.reg_file_req.read1_valid <<= read1_needed & ~self.do_branch & ~inst_word_av
        self.reg_file_req.read2_addr  <<= BrewRegAddr(rd2_addr)
        self.reg_file_req.read2_valid <<= read2_needed & ~self.do_branch & ~inst_word_av
        self.reg_file_req.rsv_addr    <<= BrewRegAddr(rsv_addr)
        self.reg_file_req.rsv_valid   <<= rsv_needed & ~self.do_branch & ~inst_word_av

        register_outputs <<= self.reg_file_req.ready & self.reg_file_req.valid
        release_outputs <<= self.output_port.ready & self.output_port.valid

        stage_2_full <<= Reg(Select(self.do_branch, Select(register_outputs, Select(release_outputs, stage_2_full, 0), 1), 0))


        stage_2_has_all_fields                 <<= Stage2Reg(have_all_fields)

        self.output_port.exec_unit             <<= Stage2Reg(exec_unit)
        self.output_port.alu_op                <<= Stage2Reg(alu_op)
        self.output_port.shifter_op            <<= Stage2Reg(shifter_op)
        self.output_port.branch_op             <<= Stage2Reg(branch_op)
        self.output_port.ldst_op               <<= Stage2Reg(ldst_op)
        self.output_port.op_a                  <<= Select(Stage2Reg(use_reg_a), Select(Stage2Reg(use_field_e_a), Stage2Reg(op_a), field_e), self.reg_file_rsp.read1_data)
        self.output_port.op_b                  <<= Select(Stage2Reg(use_reg_b), Select(Stage2Reg(use_field_e_b), Stage2Reg(op_b), field_e), self.reg_file_rsp.read2_data)
        self.output_port.op_c                  <<= Select(Stage2Reg(use_field_e_c), Stage2Reg(op_c), field_e)
        self.output_port.mem_access_len        <<= Stage2Reg(mem_len)
        self.output_port.inst_len              <<= Stage2Reg(inst_len)
        self.output_port.do_bse                <<= Stage2Reg(bse)
        self.output_port.do_wse                <<= Stage2Reg(wse)
        self.output_port.do_bze                <<= Stage2Reg(bze)
        self.output_port.do_wze                <<= Stage2Reg(wze)
        self.output_port.result_reg_addr       <<= Stage2Reg(BrewRegAddr(rsv_addr))
        self.output_port.result_reg_addr_valid <<= Stage2Reg(rsv_needed)
        self.output_port.fetch_av              <<= Stage2Reg(inst_word_av)

        #self.break_fetch_burst <<= inst_word_out.ready & inst_word_out.valid & ((exec_unit == op_class.ld_st) | (exec_unit == op_class.branch))
        self.break_fetch_burst <<= inst_word_out.ready & inst_word_out.valid & ((exec_unit == op_class.branch))


def sim():
    class RegFileEmulator(Module):
        clk = ClkPort()
        rst = RstPort()

        # Interface to the register file
        reg_file_req = Input(RegFileReadRequestIf)
        reg_file_rsp = Output(RegFileReadResponseIf)

        def construct(self):
            self.wait_range = 0

        def set_wait_range(self, wait_range):
            self.wait_range = wait_range

        def simulate(self, simulator: Simulator) -> TSimEvent:
            self.out_buf_full = False

            def wait_clk():
                yield (self.clk, self.reg_file_rsp.ready, self.reg_file_rsp.valid)
                while self.clk.get_sim_edge() != EdgeType.Positive:
                    if not self.out_buf_full or (self.reg_file_rsp.ready and self.reg_file_rsp.valid):
                        self.reg_file_req.ready <<= 1
                    else:
                        self.reg_file_req.ready <<= 0
                    yield (self.clk, self.reg_file_rsp.ready, self.reg_file_rsp.valid)


            self.reg_file_rsp.valid <<= 0
            self.reg_file_req.ready <<= 1

            while True:
                yield from wait_clk()

                if (self.reg_file_rsp.valid & self.reg_file_rsp.ready) == 1:
                    self.out_buf_full = False
                    self.reg_file_rsp.read1_data <<= None
                    self.reg_file_rsp.read2_data <<= None
                    self.reg_file_rsp.valid <<= 0

                if (self.reg_file_req.valid & self.reg_file_req.ready) == 1:
                    self.reg_file_rsp.read1_data <<= None
                    self.reg_file_rsp.read2_data <<= None
                    self.reg_file_rsp.valid <<= 0

                    rd_addr1 = copy(self.reg_file_req.read1_addr.sim_value) if self.reg_file_req.read1_valid == 1 else None
                    rd_addr2 = copy(self.reg_file_req.read2_addr.sim_value) if self.reg_file_req.read2_valid == 1 else None
                    rsv_addr = copy(self.reg_file_req.rsv_addr.sim_value  ) if self.reg_file_req.rsv_valid == 1   else None

                    if rd_addr1 is not None:
                        rd_data1 = 0x100+rd_addr1
                        simulator.log(f"RF reading $r{rd_addr1:x} with value {rd_data1}")
                    else:
                        rd_data1 = None
                    if rd_addr2 is not None:
                        rd_data2 = 0x100+rd_addr2
                        simulator.log(f"RF reading $r{rd_addr2:x} with value {rd_data2}")
                    else:
                        rd_data2 = None
                    if rsv_addr is not None:
                        simulator.log(f"RF reserving $r{rsv_addr:x}")
                    self.out_buf_full = True
                    for _ in range(randint(0,self.wait_range)):
                        self.reg_file_req.ready <<= 0
                        self.reg_file_rsp.valid <<= 0
                        simulator.log("RF waiting")
                        yield from wait_clk()
                    self.reg_file_rsp.read1_data <<= rd_data1
                    self.reg_file_rsp.read2_data <<= rd_data2
                    self.reg_file_rsp.valid <<= 1
                #else:
                #    self.reg_file_rsp.read1_data <<= None
                #    self.reg_file_rsp.read2_data <<= None
                #    self.reg_file_rsp.valid <<= 0


    class FetchEmulator(GenericModule):
        clk = ClkPort()
        rst = RstPort()

        fetch = Output(FetchDecodeIf)

        def construct(self, exp_queue: List['DecodeExpectations'], set_exec_wait_range: Callable, set_reg_file_wait_range: Callable):
            self.exp_queue = exp_queue
            self.set_exec_wait_range = set_exec_wait_range
            self.set_reg_file_wait_range = set_reg_file_wait_range

        def simulate(self, simulator) -> TSimEvent:
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

            def issue(inst, exp: DecodeExpectations, av = False, ):
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
                exp.fetch_av = av
                self.exp_queue.append(exp)
                self.fetch.inst_len <<= len(inst) - 1
                self.fetch.av <<= av
                yield from wait_transfer()

            self.fetch.valid <<= 0
            yield from wait_rst()
            for i in range(4):
                yield from wait_clk()


            """
            $<8000: SWI
            $ .002: $pc <- $rD
            $ .004: $rD <- $pc
            $ .01.: $rD <- tiny FIELD_A
            $ .04.: $rD <- ~$rA
            $ .2..: $rD <- $rA | $rB
            $ .00f: $rD <- VALUE
            $ .3.f: $rD <- FIELD_E & $rB
            $ .0f0: $rD <- short VALUE
            $ .4f.: $rD <- FIELD_E + $rA
            $ .c**: MEM[$rA+tiny OFS*4] <- $rD
            $ .d**: $rD <- MEM[$rA+tiny OFS*4]
            $ .e4.: $rD <- MEM8[$rA]
            $ .e8.: MEM8[$rA] <- $rD
            """
            test_inst_table = [
                "swi",
                "pc_eq_r",
                "r_eq_pc",
                "r_eq_t",
                "r_eq_not_r",
                "r_eq_r_or_r",
                "r_eq_I",
                "r_eq_I_and_r",
                "r_eq_i",
                "r_eq_i_plus_r",
                "mem32_r_plus_t_eq_r",
                "r_eq_mem32_r_plus_t",
                "r_eq_mem8_r",
                "mem8_r_eq_r"
            ]
            a = BrewAssembler()
            de = DecodeExpectations()
            def _i(method, *args, **kwargs):
                nonlocal a, de, simulator
                asm_fn = getattr(a, method)
                decode_fn = getattr(de, method)
                words = asm_fn(*args, **kwargs)
                words_str = ":".join(f"{i:04x}" for i in words)
                simulator.log(f"ISSUING {method} {words_str}")
                return words, decode_fn(*args, **kwargs)

            yield from issue(*_i("r_eq_r_or_r", 0,2,3), av=True)
            #yield from issue(a.r_eq_I(4, 0xdeadbeef))
            #yield from issue(a.mem8_r_eq_r(4,5))
            for i in range(4):
                yield from wait_clk()
            pass

            def test_cycle(cycle_count, exec_wait, reg_file_wait):
                simulator.log(f"====== {'NO' if exec_wait == 0 else f'random {exec_wait}'} exec wait, {'NO' if reg_file_wait == 0 else f'random {reg_file_wait}'} reg file wait")
                self.set_exec_wait_range(exec_wait)
                self.set_reg_file_wait_range(reg_file_wait)
                for i in range(cycle_count):
                    inst = test_inst_table[randint(0,len(test_inst_table)-1)]
                    yield from issue(*_i(inst))

                for i in range(4):
                    yield from wait_clk()

            cycle_count = 50
            yield from test_cycle(cycle_count, 0, 0)
            yield from test_cycle(cycle_count, 0, 4)
            yield from test_cycle(cycle_count, 4, 0)
            yield from test_cycle(cycle_count, 4, 4)


    class ExecEmulator(GenericModule):
        clk = ClkPort()
        rst = RstPort()

        input_port = Input(DecodeExecIf)

        def set_wait_range(self, wait_range):
            self.wait_range = wait_range

        def construct(self, exp_queue: List[DecodeExpectations]):
            self.exp_queue = exp_queue
            self.wait_range = 0

        def simulate(self, simulator) -> TSimEvent:
            def wait_clk():
                yield (self.clk, )
                while self.clk.get_sim_edge() != EdgeType.Positive:
                    yield (self.clk, )

            def wait_rst():
                yield from wait_clk()
                while self.rst == 1:
                    yield from wait_clk()

            def wait_transfer():
                self.input_port.ready <<= 1
                yield from wait_clk()
                while (self.input_port.valid & self.input_port.ready) != 1:
                    yield from wait_clk()
                self.input_port.ready <<= 0


            self.input_port.ready <<= 0
            yield from wait_rst()
            while True:
                yield from wait_transfer()
                exp = self.exp_queue.pop(0)
                simulator.log(f"EXEC got something {exp.fn_name}")
                exp.check(self.input_port, simulator)
                for _ in range(randint(0,self.wait_range)):
                    simulator.log("EXEC waiting")
                    yield from wait_clk()



    class top(Module):
        clk = ClkPort()
        rst = RstPort()

        def body(self):
            exec_exp_queue = []
            self.reg_file_emulator = RegFileEmulator()
            self.exec_emulator = ExecEmulator(exec_exp_queue)
            self.fetch_emulator = FetchEmulator(exec_exp_queue, self.exec_emulator.set_wait_range, self.reg_file_emulator.set_wait_range)

            self.dut = DecodeStage(use_mini_table=True)

            self.dut.fetch <<= self.fetch_emulator.fetch

            self.exec_emulator.input_port <<= self.dut.output_port

            self.dut.do_branch <<= 0

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

            for i in range(1000):
                yield from clk()
            now = yield 10
            print(f"Done at {now}")

    Build.simulation(top, "decode.vcd", add_unnamed_scopes=True)

def gen():
    def top():
        return ScanWrapper(DecodeStage, {"clk", "rst"})

    netlist = Build.generate_rtl(top, "decode.sv")
    top_level_name = netlist.get_module_class_name(netlist.top_level)
    flow = QuartusFlow(target_dir="q_decode", top_level=top_level_name, source_files=("decode.sv",), clocks=(("clk", 10), ("top_clk", 100)), project_name="decode", no_timing_report_clocks="clk")
    flow.generate()
    flow.run()

if __name__ == "__main__":
    #gen()
    sim()
