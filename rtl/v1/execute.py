#!/usr/bin/python3
from random import *
from typing import *
from silicon import *
from .brew_types import *
from .brew_utils import *

"""
Execute stage of the V1 pipeline.

This stage is sandwiched between 'decode' and 'memory'.

It does the following:
- Computes the result from source operands
- Computes effective address for memory
- Checks for all exceptions
- Tests for branches

"""

class ExecuteStage(Module):
    clk = ClkPort()
    rst = RstPort()

    # Pipeline input
    decode = Input(DecodeExecIf)

    # Pipeline output
    mem = Output(ExecMemIf)

    # side-band interfaces
    mem_base = Input(BrewMemBase)
    mem_limit = Input(BrewMemBase)
    spc_in  = Input(BrewInstAddr)
    spc_out = Output(BrewInstAddr)
    tpc_in  = Input(BrewInstAddr)
    tpc_out = Output(BrewInstAddr)
    task_mode_in  = Input(logic)
    task_mode_out = Output(logic)
    ecause_in = Input(Unsigned(8))
    ecause_out = Output(Unsigned(8))
    rcause_in = Input(Unsigned(8))
    rcause_out = Output(Unsigned(8))
    do_branch = Output(logic)
    interrupt = Input(logic)

    def body(self):
        @module(1)
        def bb_bit_idx(bit_code):
            SelectOne(
                bit_code < 9, bit_code,
                bit_code == 10, 14,
                bit_code == 11, 15,
                bit_code == 12, 16,
                bit_code == 13, 30,
                bit_code == 14, 31,
            )

        pc = Select(self.task_mode_in, self.spc_in, self.tpc_in)

        adder_result = SelectOne(
            self.decode.opcode == op.add,      self.decode.op_a + self.decode.op_b,
            self.decode.opcode == op.a_sub_b,  self.decode.op_a + self.decode.op_b,
            self.decode.opcode == op.b_sub_a,  self.decode.op_b - self.decode.op_a,
            self.decode.opcode == op.addr,     self.decode.op_b + self.decode.op_imm + (self.mem_base << BrewMemShift),
            self.decode.opcode == op.pc_add,   pc + {self.decode.op_a, "1'b0"},
        )[31:0]
        shifter_result = SelectOne(
            self.decode.opcode == op.shll, self.decode.op_a << self.decode.op_b[5:0],
            self.decode.opcode == op.shlr, self.decode.op_a >> self.decode.op_b[5:0],
            self.decode.opcode == op.shar, Signed(32)(self.decode.op_a) >> self.decode.op_b[5:0],
        )[31:0]
        logic_result = SelectOne(
            self.decode.opcode == op.b_or,    self.decode.op_a | self.decode.op_b,
            self.decode.opcode == op.b_and,   self.decode.op_a & self.decode.op_b,
            self.decode.opcode == op.b_nand, ~self.decode.op_a & self.decode.op_b,
            self.decode.opcode == op.b_xor,   self.decode.op_a ^ self.decode.op_b,
        )
        cbranch_result = SelectOne(
            self.decode.opcode == op.cb_eq,   self.decode.op_a == self.decode.op_b,
            self.decode.opcode == op.cb_ne,   self.decode.op_a != self.decode.op_b,
            self.decode.opcode == op.cb_lts,  Signed(32)(self.decode.op_a) <  Signed(32)(self.decode.op_b),
            self.decode.opcode == op.cb_ges,  Signed(32)(self.decode.op_a) >= Signed(32)(self.decode.op_b),
            self.decode.opcode == op.cb_lt,   self.decode.op_a <  self.decode.op_b,
            self.decode.opcode == op.cb_ge,   self.decode.op_a >= self.decode.op_b,
        )
        bbranch_result = SelectOne(
            self.decode.opcode == op.bb_one,  self.decode.op_a[bb_bit_idx(self.decode.op_b)],
            self.decode.opcode == op.bb_zero, self.decode.op_b[bb_bit_idx(self.decode.op_a)],
        )

        mem_av = (self.decode.exec_unit == exec.adder) & (self.decode.opcode == op.addr) & (adder_result[31:BrewMemShift] > self.mem_limit)
        mem_unaligned = (self.decode.exec_unit == exec.adder) & (self.decode.opcode == op.addr) & \
            Select(self.decode.mem_access_len,
                0, # 8-bit access is always aligned
                adder_result[0], # 16-bit access is unaligned if LSB is non-0
                adder_result[0] | adder_result[1], # 32-bit access is unaligned if lower two bits are non-0
                1 # This is an invalid length
            )

        def unmunge_offset(offset):
            return {
                offset[0], offset[0], offset[0], offset[0], offset[0], offset[0], offset[0], offset[0],
                offset[0], offset[0], offset[0], offset[0], offset[0], offset[0], offset[0], offset[0],
                offset[15:1], 0
            }

        next_inst_addr = SelectOne(
            self.decode.exec_unit == exec.cbranch | self.decode.exec_unit == exec.bbranch, pc + unmunge_offset(self.decode.op_imm),
            self.decode.exec_unit == exec.misc & self.decode.opcode == op.misc_pc_w, self.decode.op_imm,
            self.decode.exec_unit == exec.misc & self.decode.opcode == op.misc_tpc_w & self.task_mode_in, self.decode.op_imm,
            default_port = Select(
                self.task_mode_in,
                self.spc_in + self.decode.inst_len + 1, # Inst len is 0=16-bit, 1=32-bit, 2=48-bit
                self.tpc_in + self.decode.inst_len + 1  # Inst len is 0=16-bit, 1=32-bit, 2=48-bit
            )
        )

        mult_result_large = self.decode.op_a * self.decode.op_b
        mult_result = mult_result_large[31:0]
        exec_result = SelectOne(
            self.decode.exec_unit == exec.adder, adder_result,
            self.decode.exec_unit == exec.shift, shifter_result,
            self.decode.exec_unit == exec.mult, mult_result,
            self.decode.exec_unit == exec.logic, logic_result,
            self.decode.exec_unit == exec.misc, SelectOne(
                self.decode.opcode == op.misc_swi,   None,
                self.decode.opcode == op.misc_stm,   None,
                self.decode.opcode == op.misc_pc_r,  pc,
                self.decode.opcode == op.misc_tpc_r, self.tpc_in,
                self.decode.opcode == op.misc_pc_w,  None,
                self.decode.opcode == op.misc_tpc_w, None,
            )
        )
        is_branch = SelectOne(
            self.decode.exec_unit == exec.adder, mem_av,
            self.decode.exec_unit == exec.shift, 0,
            self.decode.exec_unit == exec.mult,  0,
            self.decode.exec_unit == exec.logic, 0,
            self.decode.exec_unit == exec.cbranch, cbranch_result,
            self.decode.exec_unit == exec.bbranch, bbranch_result,
            self.decode.exec_unit == exec.misc, SelectOne(
                self.decode.opcode == op.misc_swi,   1,
                self.decode.opcode == op.misc_stm,   1,
                self.decode.opcode == op.misc_pc_r,  0,
                self.decode.opcode == op.misc_tpc_r, 0,
                self.decode.opcode == op.misc_pc_w,  1,
                self.decode.opcode == op.misc_tpc_w, self.task_mode_in,

            )
        )

        is_exception = self.decode.fetch_av | SelectOne(
            self.decode.exec_unit == exec.adder, mem_av | mem_unaligned,
            self.decode.exec_unit == exec.shift, 0,
            self.decode.exec_unit == exec.mult,  0,
            self.decode.exec_unit == exec.logic, 0,
            self.decode.exec_unit == exec.cbranch, 0,
            self.decode.exec_unit == exec.bbranch, 0,
            self.decode.exec_unit == exec.misc, SelectOne(
                self.decode.opcode == op.misc_swi,   1,
                self.decode.opcode == op.misc_stm,   0,
                self.decode.opcode == op.misc_pc_r,  0,
                self.decode.opcode == op.misc_tpc_r, 0,
                self.decode.opcode == op.misc_pc_w,  0,
                self.decode.opcode == op.misc_tpc_w, 0,
            )
        )

        # TODO:: This can probably be optimized as many of the options can't really happen at the same time.
        #        With some caution in fetch, it can even be the case that all of these are exclusive conditions.
        #        Because of that, a simple concatenation of the bit-fields could probably work. But for now,
        #        let's leave it this way and optimize later.
        ecause_mask = Select(
            self.decode.fetch_av,
            Select(
                mem_unaligned,
                Select(
                    mem_av,
                    Select(
                        (self.decode.exec_unit == exec.misc) & (self.decode.opcode == op.misc_swi),
                        0,
                        1 << (self.decode.op_a & 7)
                    ),
                    1 << exc_mdp
                ),
                1 << exc_cua
            ),
            1 << exc_mip
        )

        self.do_branch <<= is_branch & reg_en

        reg_en = Wire(logic)
        handshake_fsm = ForwardBufLogic(
            input_valid = self.decode.valid,
            input_ready = self.decode.ready,
            output_valid = self.mem.valid,
            output_ready = self.mem.ready,
            out_reg_en = reg_en
        )

        self.mem.mem_access_len  <<= RegEn(self.decode.mem_access_len, reg_en)
        self.mem.result          <<= RegEn(exec_result, reg_en)
        self.mem.result_reg_addr <<= RegEn(self.decode.result_reg_addr, reg_en)
        self.mem.mem_addr        <<= RegEn(adder_result, reg_en)
        self.mem.is_load         <<= RegEn(self.decode.is_load, reg_en)
        self.mem.is_store        <<= RegEn(self.decode.is_store, reg_en)
        self.mem.do_bse          <<= RegEn(self.decode.do_bse, reg_en)
        self.mem.do_wse          <<= RegEn(self.decode.do_wse, reg_en)
        self.mem.do_bze          <<= RegEn(self.decode.do_bze, reg_en)
        self.mem.do_wze          <<= RegEn(self.decode.do_wze, reg_en)

        # Side-band info output
        self.spc_out <<= Select(reg_en,
            self.spc_in,
            Select(
                self.task_mode_in,
                # In Scheduler mode
                Select(is_exception, next_inst_addr, 0), # Exception in scheduler mode is reset
                # In Task mode
                self.spc_in
            )
        )
        self.tpc_out <<= Select(reg_en,
            self.tpc_in,
            Select(
                self.task_mode_in,
                # In Scheduler mode: TPC can only change through TCP manipulation instructions
                Select(
                    self.decode.exec_unit == exec.misc & self.decode.opcode == op.misc_tpc_w,
                    self.tpc_in,
                    self.op_imm
                ),
                # In Task mode: TPC changes through branches, or through normal execution
                Select(is_exception, next_inst_addr, self.tpc_in) # Exception in task mode: no TPC update
            )
        )
        # Still needs assignment
        self.task_mode_out <<= Select(reg_en,
            self.task_mode_in,
            Select(
                self.task_mode_in,
                # In scheduler mode: exit to ask mode, if STM instruction is executed
                ~((self.decode.exec_unit == exec.misc) & (self.decode.opcode == op.misc_stm)),
                # In task mode: we enter scheduler mode in case of an exception
                ~is_exception
            )
        )

        # We set the HWI ECAUSE bit even in scheduler mode: this allows for interrupt polling at least.
        # The interrupt will not get serviced of course until we're in TASK mode.
        hwi_mask =  Select(self.interrupt, 0, 1 << exc_hwi)
        self.ecause_out <<= Select(reg_en,
            self.ecause_in,
            Select(
                is_exception,
                self.ecause_in | hwi_mask,
                self.ecause_in | ecause_mask | hwi_mask
            )
        )
        self.rcause_out <<= Select(reg_en,
            self.rcause_in,
            Select(
                is_exception & ~self.task_mode_in,
                self.rcause_in,
                self.rcause_in | ecause_mask
            )
        )

def gen():
    Build.generate_rtl(ExecuteStage)

gen()
#sim()

