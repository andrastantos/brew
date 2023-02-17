#!/usr/bin/python3
from random import *
from typing import *
from silicon import *
try:
    from .brew_types import *
    from .brew_utils import *
    from .memory import MemoryStage
    from .scan import ScanWrapper
    from .synth import *
except ImportError:
    from brew_types import *
    from brew_utils import *
    from memory import MemoryStage
    from scan import ScanWrapper
    from synth import *

"""
Execute stage of the V1 pipeline.

This stage is sandwiched between 'decode' and 'memory'.

It does the following:
- Computes the result from source operands
- Computes effective address for memory
- Checks for all exceptions
- Tests for branches

"""

"""

    <- cycle 1 -> <- cycle 2 ->

    +-----------+ +-----------+
    |    ALU    | |   Branch  |
    +-----------+ |           |
    +-----------+ |           |
    | Br. Trgt. | |           |
    +-----------+ +-----------+
    +-----------+ +-----------+
    |    LDST   | |   Memory  |
    +-----------+ +-----------+
    +-------------------------+
    |         Shifter         |
    +-------------------------+
    +-------------------------+
    |        Multilper        |
    +-------------------------+

"""

class ExecUnitResultIf(Interface):
    result = BrewData


TIMING_CLOSURE_TESTS = False

class AluInputIf(Interface):
    opcode = EnumNet(alu_ops)
    op_a = BrewData
    op_b = BrewData
    pc = BrewInstAddr
    tpc = BrewInstAddr

class AluOutputIf(Interface):
    result = BrewData
    f_zero = logic
    f_sign  = logic
    f_carry = logic
    f_overflow = logic

class AluUnit(Module):
    clk = ClkPort()
    rst = RstPort()

    input_port = Input(AluInputIf)
    output_port = Output(AluOutputIf)

    OPTIMIZED = True
    def body(self):
        if self.OPTIMIZED:
            c_in = Wire(logic)
            c_in <<= (self.input_port.opcode == alu_ops.a_minus_b)
            xor_b = Wire(BrewData)
            xor_b <<= Select((self.input_port.opcode == alu_ops.a_minus_b) | (self.input_port.opcode == alu_ops.not_b_and_a), 0, 0xffffffff)
            b = xor_b ^ self.input_port.op_b
            a = self.input_port.op_a
            sum = a + b + c_in
            and_result = a & b
            xor_result = self.input_port.op_a ^ self.input_port.op_b

            use_adder = (self.input_port.opcode == alu_ops.a_plus_b) | (self.input_port.opcode == alu_ops.a_minus_b)
            use_and = (self.input_port.opcode == alu_ops.a_and_b) | (self.input_port.opcode == alu_ops.not_b_and_a)
            adder_result = Wire(Unsigned(33))
            adder_result <<= SelectOne(
                use_adder,                                      sum,
                self.input_port.opcode == alu_ops.a_or_b,       self.input_port.op_a | self.input_port.op_b,
                use_and,                                        and_result,
                self.input_port.opcode == alu_ops.a_xor_b,      xor_result,
                self.input_port.opcode == alu_ops.tpc,          concat(self.input_port.tpc, "1'b0"),
                self.input_port.opcode == alu_ops.pc,           concat(self.input_port.pc, "1'b0"),
            )

            self.output_port.result <<= adder_result[31:0]
            #self.output_port.f_zero <<= adder_result[31:0] == 0
            self.output_port.f_zero <<= xor_result == 0
            self.output_port.f_sign <<= adder_result[31]
            self.output_port.f_carry <<= adder_result[32] ^ (self.input_port.opcode == alu_ops.a_minus_b)
            # overflow for now is only valid for a_minus_b
            # See https://en.wikipedia.org/wiki/Overflow_flag for details
            self.output_port.f_overflow <<= (self.input_port.op_a[31] != self.input_port.op_b[31]) & (self.input_port.op_a[31] != adder_result[31])
        else:
            adder_result = Wire(Unsigned(33))
            adder_result <<= SelectOne(
                self.input_port.opcode == alu_ops.a_plus_b,     (self.input_port.op_a + self.input_port.op_b),
                self.input_port.opcode == alu_ops.a_minus_b,    Unsigned(33)(self.input_port.op_a - self.input_port.op_b),
                self.input_port.opcode == alu_ops.a_or_b,       self.input_port.op_a | self.input_port.op_b,
                self.input_port.opcode == alu_ops.a_and_b,      self.input_port.op_a & self.input_port.op_b,
                self.input_port.opcode == alu_ops.not_b_and_a,  ~self.input_port.op_b & self.input_port.op_a,
                self.input_port.opcode == alu_ops.a_xor_b,      self.input_port.op_a ^ self.input_port.op_b,
                self.input_port.opcode == alu_ops.tpc,          concat(self.input_port.tpc, "1'b0"),
                self.input_port.opcode == alu_ops.pc,           concat(self.input_port.pc, "1'b0"),
            )

            self.output_port.result <<= adder_result[31:0]
            self.output_port.f_zero <<= adder_result[31:0] == 0
            self.output_port.f_sign <<= adder_result[31]
            self.output_port.f_carry <<= adder_result[32]
            # overflow for now is only valid for a_minus_b
            # See https://en.wikipedia.org/wiki/Overflow_flag for details
            self.output_port.f_overflow <<= (self.input_port.op_a[31] != self.input_port.op_b[31]) & (self.input_port.op_a[31] != adder_result[31])


class ShifterInputIf(Interface):
    opcode = EnumNet(shifter_ops)
    op_a = BrewData
    op_b = BrewData

class ShifterOutputIf(Interface):
    result = BrewData

class ShifterUnit(Module):
    clk = ClkPort()
    rst = RstPort()

    input_port = Input(ShifterInputIf)
    output_port = Output(ShifterOutputIf)

    def body(self):
        # TODO: this can be optimized quite a bit. As of now, we instantiate 3 barrel shifters
        self.signed_a = Signed(32)(self.input_port.op_a)
        shifter_result = SelectOne(
            self.input_port.opcode == shifter_ops.shll, self.input_port.op_a << self.input_port.op_b[4:0],
            self.input_port.opcode == shifter_ops.shlr, self.input_port.op_a >> self.input_port.op_b[4:0],
            self.input_port.opcode == shifter_ops.shar, self.signed_a >> self.input_port.op_b[4:0],
        )[31:0]

        self.output_port.result <<= shifter_result


class MultInputIf(Interface):
    valid = logic
    op_a = BrewData
    op_b = BrewData

class MultOutputIf(Interface):
    result = BrewData

class MultUnit(Module):
    clk = ClkPort()
    rst = RstPort()

    input_port = Input(MultInputIf)
    output_port = Output(MultOutputIf)

    def body(self):
        if TIMING_CLOSURE_TESTS:
            mult_result = 0
        else:
            op_a = Select(self.input_port.valid, Reg(self.input_port.op_a, clock_en = self.input_port.valid), self.input_port.op_a)
            op_b = Select(self.input_port.valid, Reg(self.input_port.op_b, clock_en = self.input_port.valid), self.input_port.op_b)
            mult_result_large = op_a * op_b
            mult_result = mult_result_large[31:0]

        self.output_port.result <<= mult_result


class BranchTargetUnitInputIf(Interface):
    op_c       = BrewData
    pc         = BrewInstAddr
    inst_len   = Unsigned(2) # 0: single-beat, 1: two-beat, 3: 4-beat

class BranchTargetUnitOutputIf(Interface):
    branch_addr = BrewInstAddr
    straight_addr = BrewInstAddr

class BranchTargetUnit(Module):

    input_port = Input(BranchTargetUnitInputIf)
    output_port = Output(BranchTargetUnitOutputIf)

    def body(self):
        def unmunge_offset(offset):
            return concat(
                offset[0], offset[0], offset[0], offset[0], offset[0], offset[0], offset[0], offset[0],
                offset[0], offset[0], offset[0], offset[0], offset[0], offset[0], offset[0], offset[0],
                offset[15:1]
            )

        self.output_port.branch_addr   <<= (self.input_port.pc + unmunge_offset(self.input_port.op_c))[30:0]
        self.output_port.straight_addr <<= (self.input_port.pc + self.input_port.inst_len + 1)[30:0]


class BranchUnitInputIf(Interface):
    opcode          = EnumNet(branch_ops)
    op_a            = BrewData
    op_b            = BrewData
    pc              = BrewInstAddr
    spc             = BrewInstAddr
    tpc             = BrewInstAddr
    op_c            = BrewData
    task_mode       = logic
    branch_addr     = BrewInstAddr
    interrupt       = logic
    fetch_av        = logic # Coming all the way from fetch: if the instruction gotten this far, we should raise the exception
    mem_av          = logic # Coming from the load-store unit if that figures out an exception
    mem_unaligned   = logic # Coming from the load-store unit if an unaligned access was attempted
    f_zero          = logic
    f_sign          = logic
    f_carry         = logic
    f_overflow      = logic
    is_branch_insn  = logic

class BranchUnitOutputIf(Interface):
    spc               = BrewInstAddr
    spc_changed       = logic
    tpc               = BrewInstAddr
    tpc_changed       = logic
    task_mode         = logic
    task_mode_changed = logic
    ecause            = Unsigned(12)
    do_branch         = logic

class BranchUnit(Module):

    input_port = Input(BranchUnitInputIf)
    output_port = Output(BranchUnitOutputIf)

    def body(self):
        @module(1)
        def bb_get_bit(word, bit_code):
            return SelectOne(
                bit_code == 0,  word[0],
                bit_code == 1,  word[1],
                bit_code == 2,  word[2],
                bit_code == 3,  word[3],
                bit_code == 4,  word[4],
                bit_code == 5,  word[5],
                bit_code == 6,  word[6],
                bit_code == 7,  word[7],
                bit_code == 8,  word[8],
                bit_code == 9,  word[9],
                bit_code == 10, word[14],
                bit_code == 11, word[15],
                bit_code == 12, word[16],
                bit_code == 13, word[30],
                bit_code == 14, word[31],
            )

        # Branch codes:
        #  eq: f_zero = 1
        #  ne: f_zero = 0
        #  lt: f_carry = 1
        #  ge: f_carry = 0
        #  lts: f_sign != f_overflow
        #  ges: f_sign == f_overflow
        condition_result = self.input_port.is_branch_insn & SelectOne(
            self.input_port.opcode == branch_ops.cb_eq,   self.input_port.f_zero,
            self.input_port.opcode == branch_ops.cb_ne,   ~self.input_port.f_zero,
            self.input_port.opcode == branch_ops.cb_lts,  self.input_port.f_sign != self.input_port.f_overflow,
            self.input_port.opcode == branch_ops.cb_ges,  self.input_port.f_sign == self.input_port.f_overflow,
            self.input_port.opcode == branch_ops.cb_lt,   self.input_port.f_carry,
            self.input_port.opcode == branch_ops.cb_ge,   ~self.input_port.f_carry,
            self.input_port.opcode == branch_ops.bb_one,  bb_get_bit(self.input_port.op_a, self.input_port.op_b),
            self.input_port.opcode == branch_ops.bb_zero, ~bb_get_bit(self.input_port.op_a, self.input_port.op_b),
        )

        # Set if we have an exception: in task mode this results in a switch to scheduler mode, in scheduler mode, it's a reset
        is_exception = (self.input_port.is_branch_insn & (self.input_port.opcode == branch_ops.swi)) | self.input_port.mem_av | self.input_port.mem_unaligned | self.input_port.fetch_av

        # Set whenever we branch without a mode change
        in_mode_branch = SelectOne(
            self.input_port.is_branch_insn & (self.input_port.opcode == branch_ops.pc_w),     1,
            self.input_port.is_branch_insn & (self.input_port.opcode == branch_ops.tpc_w),    self.input_port.task_mode,
            self.input_port.is_branch_insn & (self.input_port.opcode == branch_ops.pc_w_r),   1,
            self.input_port.is_branch_insn & (self.input_port.opcode == branch_ops.tpc_w_r),  self.input_port.task_mode,
            is_exception,                                                                     ~self.input_port.task_mode,
            self.input_port.is_branch_insn & (self.input_port.opcode == branch_ops.stm),      0,
            default_port =                                                                    condition_result,
        )

        branch_target = SelectOne(
            self.input_port.is_branch_insn & (self.input_port.opcode == branch_ops.pc_w),                                   self.input_port.op_c[31:1],
            self.input_port.is_branch_insn & (self.input_port.opcode == branch_ops.tpc_w),                                  self.input_port.op_c[31:1],
            self.input_port.is_branch_insn & (self.input_port.opcode == branch_ops.pc_w_r),                                 self.input_port.op_a[31:1],
            self.input_port.is_branch_insn & (self.input_port.opcode == branch_ops.tpc_w_r),                                self.input_port.op_a[31:1],
            self.input_port.is_branch_insn & (self.input_port.opcode == branch_ops.stm),                                    self.input_port.tpc,
            default_port =                                                                                                  Select(is_exception | self.input_port.interrupt, self.input_port.branch_addr, self.input_port.tpc),
        )

        self.output_port.spc            <<= Select(is_exception, branch_target, 0)
        self.output_port.spc_changed    <<= ~self.input_port.task_mode & (is_exception | in_mode_branch)
        self.output_port.tpc            <<= branch_target
        self.output_port.tpc_changed    <<= Select(
            self.input_port.task_mode,
            # In Scheduler mode: TPC can only change through TCP manipulation instructions. For those, the value comes through op_c
            self.input_port.is_branch_insn & ((self.input_port.opcode == branch_ops.tpc_w) | (self.input_port.opcode == branch_ops.tpc_w_r)),
            # In task mode, all branches count, but so do exceptions which, while don't change TPC, they don't update TPC either.
            in_mode_branch | is_exception | self.input_port.interrupt
        )
        self.output_port.task_mode_changed <<= Select(
            self.input_port.task_mode,
            # In scheduler mode: exit to ask mode, if STM instruction is executed
            (self.input_port.is_branch_insn & (self.input_port.opcode == branch_ops.stm)),
            # In task mode: we enter scheduler mode in case of an exception or interrupt
            is_exception | self.input_port.interrupt
        )
        self.output_port.task_mode  <<= self.input_port.task_mode ^ self.output_port.task_mode_changed

        self.output_port.do_branch  <<= in_mode_branch | self.output_port.task_mode_changed

        exception_mask = Select(
            self.input_port.fetch_av,
            (
                Select(self.input_port.is_branch_insn & (self.input_port.opcode == branch_ops.swi), 0, 1 << (self.input_port.op_a[2:0])) |
                Select(self.input_port.mem_av, 0, 1 << exc_mdp) |
                Select(self.input_port.mem_unaligned, 0, 1 << exc_cua)
            ),
            1 << exc_mip
        )

        # We set the ECAUSE bits even in scheduler mode: this allows for interrupt polling at and after a reset,
        # we can check it to determine the reason for the reset
        interrupt_mask =  Select(self.input_port.interrupt & self.input_port.task_mode, 0, 1 << exc_hwi)
        self.output_port.ecause <<= exception_mask | interrupt_mask








class LoadStoreInputIf(Interface):
    is_ldst = logic
    opcode = EnumNet(ldst_ops)
    op_b = BrewData
    op_c = BrewData
    mem_base = BrewMemBase
    mem_limit = BrewMemBase
    task_mode = logic
    mem_access_len = Unsigned(2) # 0: 8-bit, 1: 16-bit, 2: 32-bit

class LoadStoreOutputIf(Interface):
    phy_addr = BrewAddr
    mem_av = logic
    mem_unaligned = logic
class LoadStoreUnit(Module):
    input_port = Input(LoadStoreInputIf)
    output_port = Output(LoadStoreOutputIf)

    def body(self):
        eff_addr = (self.input_port.op_b + self.input_port.op_c)[31:0]
        phy_addr = (eff_addr + Select(self.input_port.task_mode, 0, (self.input_port.mem_base << BrewMemShift)))[31:0]

        mem_op = self.input_port.opcode != ldst_ops.ldst_none

        mem_av = self.input_port.is_ldst & (eff_addr[31:BrewMemShift] > self.input_port.mem_limit)
        mem_unaligned = self.input_port.is_ldst & Select(self.input_port.mem_access_len,
            0, # 8-bit access is always aligned
            eff_addr[0], # 16-bit access is unaligned if LSB is non-0
            eff_addr[0] | eff_addr[1], # 32-bit access is unaligned if lower two bits are non-0
            1 # This is an invalid length
        )

        self.output_port.phy_addr <<= phy_addr
        self.output_port.mem_av <<= mem_av & mem_op
        self.output_port.mem_unaligned <<= mem_unaligned & mem_op

class ExecuteStage(Module):
    clk = ClkPort()
    rst = RstPort()

    # Pipeline input
    input_port = Input(DecodeExecIf)

    # Pipeline output
    output_port = Output(ResultExtendIf)

    # Interface to the bus interface
    bus_req_if = Output(BusIfRequestIf)
    bus_rsp_if = Input(BusIfResponseIf)

    # Interface to the CSR registers
    csr_if = Output(CsrIf)

    # side-band interfaces
    mem_base = Input(BrewMemBase)
    mem_limit = Input(BrewMemBase)
    spc_in  = Input(BrewInstAddr)
    spc_out = Output(BrewInstAddr)
    tpc_in  = Input(BrewInstAddr)
    tpc_out = Output(BrewInstAddr)
    task_mode_in  = Input(logic)
    task_mode_out = Output(logic)
    ecause_in = Input(Unsigned(12))
    ecause_out = Output(Unsigned(12))
    do_branch = Output(logic)
    interrupt = Input(logic)

    complete = Output(logic) # goes high for 1 cycle when an instruction completes. Used for verification

    def body(self):
        # We have two stages in one, really here
        pc = Select(self.task_mode_in, self.spc_in, self.tpc_in)

        # Stege 1
        ########################################
        # Ready-valid FSM
        stage_1_valid = Wire(logic)
        stage_2_ready = Wire(logic)

        multi_cycle_exec_lockout = Reg(self.input_port.ready & self.input_port.valid & (self.input_port.exec_unit == op_class.mult))

        stage_1_fsm = ForwardBufLogic()
        stage_1_fsm.input_valid <<= ~multi_cycle_exec_lockout & self.input_port.valid
        # we 'bite out' a cycle for two-cycle units, such as multiply
        self.input_port.ready <<= ~multi_cycle_exec_lockout & stage_1_fsm.input_ready
        stage_1_valid <<= stage_1_fsm.output_valid
        stage_1_fsm.output_ready <<= stage_2_ready

        stage_1_reg_en = Wire(logic)
        stage_1_reg_en <<= ~multi_cycle_exec_lockout & stage_1_fsm.out_reg_en

        # ALU
        alu_output = Wire(AluOutputIf)
        alu_unit = AluUnit()
        alu_unit.input_port.opcode <<= self.input_port.alu_op
        alu_unit.input_port.op_a   <<= self.input_port.op_a
        alu_unit.input_port.op_b   <<= self.input_port.op_b
        alu_unit.input_port.pc     <<= pc
        alu_unit.input_port.tpc    <<= self.tpc_in
        alu_output <<= alu_unit.output_port
        s1_alu_output = Wire(AluOutputIf)
        s1_alu_output <<= Reg(alu_output, clock_en = stage_1_reg_en)
        #result
        #f_zero
        #f_sign
        #f_carry
        #f_overflow

        # Load-store
        ldst_output = Wire(LoadStoreOutputIf)
        ldst_unit = LoadStoreUnit()
        ldst_unit.input_port.is_ldst        <<= self.input_port.exec_unit == op_class.ld_st
        ldst_unit.input_port.opcode         <<= self.input_port.ldst_op
        ldst_unit.input_port.op_b           <<= self.input_port.op_b
        ldst_unit.input_port.op_c           <<= self.input_port.op_c
        ldst_unit.input_port.mem_base       <<= self.mem_base
        ldst_unit.input_port.mem_limit      <<= self.mem_limit
        ldst_unit.input_port.task_mode      <<= self.task_mode_in
        ldst_unit.input_port.mem_access_len <<= self.input_port.mem_access_len
        ldst_output <<= ldst_unit.output_port
        s1_ldst_output = Wire(LoadStoreOutputIf)
        s1_ldst_output <<= Reg(ldst_output, clock_en = stage_1_reg_en)
        #phy_addr
        #mem_av
        #mem_unaligned

        # Branch-target
        branch_target_output = Wire(BranchTargetUnitOutputIf)
        branch_target_unit = BranchTargetUnit()
        branch_target_unit.input_port.op_c       <<= self.input_port.op_c
        branch_target_unit.input_port.pc         <<= pc
        branch_target_unit.input_port.inst_len   <<= self.input_port.inst_len
        branch_target_output <<= branch_target_unit.output_port
        s1_branch_target_output = Wire(BranchTargetUnitOutputIf)
        s1_branch_target_output <<= Reg(branch_target_output, clock_en = stage_1_reg_en)
        #branch_addr
        #straight_addr


        # These are two-cycle (multi-cycle) units
        # Shifter
        shifter_output = Wire(ShifterOutputIf)
        shifter_unit = ShifterUnit()
        shifter_unit.input_port.opcode <<= self.input_port.shifter_op
        shifter_unit.input_port.op_a   <<= self.input_port.op_a
        shifter_unit.input_port.op_b   <<= self.input_port.op_b
        shifter_output <<= shifter_unit.output_port
        s1_shifter_output = Wire(ShifterOutputIf)
        s1_shifter_output <<= Reg(shifter_output, clock_en = stage_1_reg_en)
        #result

        # Multiplier
        s1_mult_output = Wire(MultOutputIf)
        mult_unit = MultUnit()
        mult_unit.input_port.valid  <<= stage_1_reg_en
        mult_unit.input_port.op_a   <<= self.input_port.op_a
        mult_unit.input_port.op_b   <<= self.input_port.op_b
        s1_mult_output <<= Reg(mult_unit.output_port, clock_en = Reg(stage_1_reg_en)) # Need to delay the capture by one clock cycle
        #result

        # Delay inputs that we will need later
        s1_exec_unit = Reg(self.input_port.exec_unit, clock_en = stage_1_reg_en)
        s1_branch_op = Reg(self.input_port.branch_op, clock_en = stage_1_reg_en)
        s1_ldst_op = Reg(self.input_port.ldst_op, clock_en = stage_1_reg_en)
        s1_op_a = Reg(self.input_port.op_a, clock_en = stage_1_reg_en)
        s1_op_b = Reg(self.input_port.op_b, clock_en = stage_1_reg_en)
        s1_op_c = Reg(self.input_port.op_c, clock_en = stage_1_reg_en)
        s1_do_bse = Reg(self.input_port.do_bse, clock_en = stage_1_reg_en)
        s1_do_wse = Reg(self.input_port.do_wse, clock_en = stage_1_reg_en)
        s1_do_bze = Reg(self.input_port.do_bze, clock_en = stage_1_reg_en)
        s1_do_wze = Reg(self.input_port.do_wze, clock_en = stage_1_reg_en)
        s1_result_reg_addr = Reg(self.input_port.result_reg_addr, clock_en = stage_1_reg_en)
        s1_result_reg_addr_valid = Reg(self.input_port.result_reg_addr_valid, clock_en = stage_1_reg_en)
        s1_fetch_av = Reg(self.input_port.fetch_av, clock_en = stage_1_reg_en)
        s1_tpc = Reg(self.tpc_in, clock_en = stage_1_reg_en)
        s1_spc = Reg(self.spc_in, clock_en = stage_1_reg_en)
        s1_task_mode = Reg(self.task_mode_in, clock_en = stage_1_reg_en)
        s1_mem_access_len = Reg(self.input_port.mem_access_len, clock_en = stage_1_reg_en)

        # Stage 2
        ########################################
        # Ready-valid FSM
        mem_input = Wire(MemInputIf)
        s2_mem_output = Wire(MemOuputIf)

        s2_pc = Select(s1_task_mode, s1_spc, s1_tpc)

        stage_2_valid = Wire(logic)

        # NOTE: The use of s1_exec_unit here is not exactly nice: we depend on it being static independent of stage_2_ready.
        # It's correct, but it's not nice.
        # NOTE: since we're going out to the RF write port, self.output_port.ready is constant '1'. That in turn means
        #       that stage_2_fsm.input_ready is also constant '1'. So, really the only reason we would apply back-pressure
        #       is if there's a pending bus operation.
        stage_2_fsm = ForwardBufLogic()
        stage_2_fsm.input_valid <<= stage_1_valid
        mem_input.valid <<= stage_1_valid
        stage_2_ready <<= Select(s1_exec_unit == op_class.ld_st, stage_2_fsm.input_ready, mem_input.ready)
        stage_2_valid <<= Select(s1_exec_unit == op_class.ld_st, stage_2_fsm.output_valid, s2_mem_output.valid)
        stage_2_fsm.output_ready <<= self.output_port.ready | ~s1_result_reg_addr_valid
        s2_mem_output.ready <<= self.output_port.ready | ~s1_result_reg_addr_valid

        stage_2_reg_en = Wire(logic)
        stage_2_reg_en <<= stage_2_fsm.out_reg_en

        # PC handling:
        # We are upgrading TPC/SPC in the first cycle of execute, as if for straight execution.
        # In the second stage, we use the pre-computed branch target and exception flags to handle branches.
        # These means that there are two possible sources for xPC updates in every cycle:
        # - Current instruction in 'execute 1'
        # - Previous instruction in case of a branch, in 'execute 2'
        # To make things right, 'execute 2' has priority updating xPC

        # Branch unit
        branch_input = Wire(BranchUnitInputIf)
        branch_output = Wire(BranchUnitOutputIf)
        branch_unit = BranchUnit()
        branch_input.opcode          <<= s1_branch_op
        branch_input.pc              <<= s2_pc
        branch_input.spc             <<= s1_spc
        branch_input.tpc             <<= s1_tpc
        branch_input.task_mode       <<= s1_task_mode
        branch_input.branch_addr     <<= s1_branch_target_output.branch_addr
        branch_input.interrupt       <<= self.interrupt
        branch_input.fetch_av        <<= s1_fetch_av
        branch_input.mem_av          <<= s1_ldst_output.mem_av
        branch_input.mem_unaligned   <<= s1_ldst_output.mem_unaligned
        branch_input.op_a            <<= s1_op_a
        branch_input.op_b            <<= s1_op_b
        branch_input.op_c            <<= s1_op_c
        branch_input.f_zero          <<= s1_alu_output.f_zero
        branch_input.f_sign          <<= s1_alu_output.f_sign
        branch_input.f_carry         <<= s1_alu_output.f_carry
        branch_input.f_overflow      <<= s1_alu_output.f_overflow
        branch_input.is_branch_insn  <<= s1_exec_unit == op_class.branch

        branch_unit.input_port <<= branch_input
        branch_output <<= branch_unit.output_port
        #spc
        #spc_changed
        #tpc
        #tpc_changed
        #task_mode
        #task_mode_changed
        #ecause
        #do_branch

        # Memory unit
        memory_unit = MemoryStage()


        mem_input.is_load <<= (s1_exec_unit == op_class.ld_st) & (s1_ldst_op == ldst_ops.ldst_load) & ~s1_ldst_output.mem_av & ~s1_ldst_output.mem_unaligned
        mem_input.is_store <<= (s1_exec_unit == op_class.ld_st) & (s1_ldst_op == ldst_ops.ldst_store) & ~s1_ldst_output.mem_av & ~s1_ldst_output.mem_unaligned
        mem_input.result_reg_addr <<= s1_result_reg_addr
        mem_input.result_reg_addr_valid <<= s1_result_reg_addr_valid
        mem_input.result <<= s1_op_a
        mem_input.mem_addr <<= s1_ldst_output.phy_addr
        mem_input.mem_access_len <<= s1_mem_access_len
        memory_unit.input_port <<= mem_input
        self.bus_req_if <<= memory_unit.bus_req_if
        memory_unit.bus_rsp_if <<= self.bus_rsp_if
        self.csr_if <<= memory_unit.csr_if
        s2_mem_output <<= memory_unit.output_port
        #data_l
        #data_h

        # Combine all outputs into a single output register, mux-in memory results
        result = SelectOne(
            s1_exec_unit == op_class.alu, s1_alu_output.result,
            s1_exec_unit == op_class.mult, s1_mult_output.result,
            s1_exec_unit == op_class.shift, s1_shifter_output.result
        )

        self.output_port.valid <<= stage_2_valid & s1_result_reg_addr_valid

        self.output_port.data_l <<= Select(s1_exec_unit == op_class.ld_st, Reg(result[15: 0], clock_en = stage_2_reg_en), s2_mem_output.data_l)
        self.output_port.data_h <<= Select(s1_exec_unit == op_class.ld_st, Reg(result[31:16], clock_en = stage_2_reg_en), s2_mem_output.data_h)
        self.output_port.data_en <<= Reg(~branch_output.do_branch, clock_en = stage_2_reg_en)
        self.output_port.addr <<= Reg(s1_result_reg_addr, clock_en = stage_2_reg_en)
        self.output_port.do_bse <<= Reg(s1_do_bse, clock_en = stage_2_reg_en)
        self.output_port.do_wse <<= Reg(s1_do_wse, clock_en = stage_2_reg_en)
        self.output_port.do_bze <<= Reg(s1_do_bze, clock_en = stage_2_reg_en)
        self.output_port.do_wze <<= Reg(s1_do_wze, clock_en = stage_2_reg_en)

        # Set sideband outputs as needed
        self.do_branch <<= Reg(branch_output.do_branch & stage_2_reg_en)

        s1_tpc_out = Reg(Select(stage_1_reg_en, self.tpc_in, Select(self.task_mode_in, self.tpc_in, branch_target_output.straight_addr)))
        self.tpc_out <<= Select(
            stage_2_reg_en,
            s1_tpc_out,
            Select(branch_output.tpc_changed, s1_tpc_out, branch_output.tpc),
        )
        s1_spc_out = Reg(Select(stage_1_reg_en, self.spc_in, Select(~self.task_mode_in, self.spc_in, branch_target_output.straight_addr)))
        self.spc_out <<= Select(
            stage_2_reg_en,
            s1_spc_out,
            Select(branch_output.spc_changed, s1_spc_out, branch_output.spc),
        )
        self.task_mode_out <<= Select(
            stage_2_reg_en,
            s1_task_mode,
            Select(branch_output.task_mode_changed, s1_task_mode, branch_output.task_mode),
        )
        self.ecause_out <<= Select(stage_2_reg_en, self.ecause_in, self.ecause_in | branch_output.ecause)

        #self.complete <<= stage_2_reg_en
        self.complete <<= stage_2_valid






def sim():
    class Result(object):
        def __init__(
            self,
            data_l,
            data_h,
            data_en,
            addr,
            result_valid = None,
            do_bse = None,
            do_wse = None,
            do_bze = None,
            do_wze = None,
            spc_out = None,
            tpc_out = None,
            task_mode_out = None,
            ecause_out = None,
            do_branch = None,
        ):
            self.data_l = data_l
            self.data_h = data_h
            self.data_en = data_en
            self.addr = addr
            self.result_valid = result_valid
            self.do_bse = do_bse
            self.do_wse = do_wse
            self.do_bze = do_bze
            self.do_wze = do_wze
            self.spc_out = spc_out
            self.tpc_out = tpc_out
            self.task_mode_out = task_mode_out
            self.ecause_out = ecause_out
            self.do_branch = do_branch

        def compare(
            self,
            result: Junction,
            spc_out: Junction,
            tpc_out: Junction,
            task_mode_out: Junction,
            ecause_out: Junction,
            do_branch: Junction,
        ):
            assert self.data_l is None or result.data_l == self.data_l
            assert self.data_h is None or result.data_h == self.data_h
            assert self.data_en is None or result.data_en == self.data_en
            assert self.addr is None or result.addr == self.addr
            assert self.result_valid is None or result.valid == self.result_valid
            assert self.do_bse is None or result.do_bse == self.do_bse
            assert self.do_wse is None or result.do_wse == self.do_wse
            assert self.do_bze is None or result.do_bze == self.do_bze
            assert self.do_wze is None or result.do_wze == self.do_wze
            assert self.spc_out is None or spc_out == self.spc_out
            assert self.tpc_out is None or tpc_out == self.tpc_out
            assert self.task_mode_out is None or task_mode_out == self.task_mode_out
            assert self.ecause_out is None or ecause_out == self.ecause_out
            assert self.do_branch is None or do_branch == self.do_branch

            return True


    class DecodeEmulator(GenericModule):
        clk = ClkPort()
        rst = RstPort()

        output_port = Output(DecodeExecIf)

        mem_base      = Output(BrewMemBase)
        mem_limit     = Output(BrewMemBase)
        spc_in        = Output(BrewInstAddr)
        tpc_in        = Output(BrewInstAddr)
        task_mode_in  = Output(logic)
        ecause_in     = Output(Unsigned(12))
        interrupt     = Output(logic)

        def construct(self, result_queue, sideband_state):
            self.result_queue = result_queue
            self.sideband_state = sideband_state

        def simulate(self) -> TSimEvent:
            def wait_clk():
                yield (self.clk, )
                while self.clk.get_sim_edge() != EdgeType.Positive:
                    yield (self.clk, )

            def wait_for_transfer():
                self.output_port.valid <<= 1
                yield from wait_clk()
                while self.output_port.ready != 1:
                    yield from wait_clk()
                self.output_port.valid <<= 0


            def set_side_band(*, tpc = None, spc = None, task_mode = True, mem_base = None, mem_limit = None, interrupt = False, ecause = 0):
                self.sideband_state.mem_base = randint(0,0x3fff) if mem_base is None else mem_base
                self.sideband_state.mem_limit = randint(0,0x3fff) if mem_limit is None else mem_limit
                self.sideband_state.tpc = randint(0,0x3fff) | 0x10000000 if tpc is None else tpc
                self.sideband_state.spc = randint(0,0x3fff) | 0x50000000 if spc is None else spc
                self.sideband_state.task_mode = 1 if task_mode else 0
                self.sideband_state.ecause = ecause
                self.sideband_state.interrupt = 1 if interrupt else 0

                self.mem_base <<= self.sideband_state.mem_base
                self.mem_limit <<= self.sideband_state.mem_limit
                self.tpc_in <<= self.sideband_state.tpc
                self.spc_in <<= self.sideband_state.spc
                self.task_mode_in <<= self.sideband_state.task_mode
                self.ecause_in <<= self.sideband_state.ecause
                self.interrupt <<= self.sideband_state.interrupt

            def send_rr_op(unit: op_class, op: alu_ops, op_a: int, op_b: int, op_c: int = None, *, result_reg = 0, result_reg_valid = True, fetch_av = False, inst_len = inst_len_16):
                self.output_port.exec_unit <<= unit
                self.output_port.alu_op <<= op if unit == op_class.alu else None
                self.output_port.shifter_op <<= op if unit == op_class.shift else None
                self.output_port.branch_op <<= None
                self.output_port.ldst_op <<= None
                self.output_port.op_a <<= op_a
                self.output_port.op_b <<= op_b
                self.output_port.op_c <<= op_c
                self.output_port.mem_access_len <<= None
                self.output_port.inst_len <<= inst_len
                self.output_port.do_bse <<= 0
                self.output_port.do_wse <<= 0
                self.output_port.do_bze <<= 0
                self.output_port.do_wze <<= 0
                self.output_port.result_reg_addr <<= result_reg
                self.output_port.result_reg_addr_valid <<= 1 if result_reg_valid else 0
                self.output_port.fetch_av <<= 1 if fetch_av else 0

                mask = 0xffffffff
                if unit == op_class.alu:
                    if op == alu_ops.a_plus_b:
                        result = (op_a + op_b) & mask
                    elif op == alu_ops.a_minus_b:
                        result = (op_a - op_b) & mask
                    elif op == alu_ops.a_and_b:
                        result = op_a & op_b
                    elif op == alu_ops.not_b_and_a:
                        result = op_a & ~op_b
                    elif op == alu_ops.a_or_b:
                        result = op_a | op_b
                    elif op == alu_ops.a_xor_b:
                        result = op_a ^ op_b
                    elif op == alu_ops.tpc:
                        result = self.sideband_state.tpc << 1
                    elif op == alu_ops.pc:
                        result = self.sideband_state.tpc << 1 if self.sideband_state.task_mode else self.sideband_state.spc << 1
                elif unit == op_class.shift:
                    if op == shifter_ops.shll:
                        result = (op_a << (op_b & 31)) & mask
                    elif op == shifter_ops.shlr:
                        result = (op_a >> (op_b & 31)) & mask
                    elif op == shifter_ops.shar:
                        msb = (op_a >> 31) & 1
                        upper = (msb << 32) - msb
                        extended_op_a = (upper << 32) | op_a
                        result = (extended_op_a >> (op_b & 31)) & mask
                elif unit == op_class.mult:
                    result = (op_a * op_b) & mask

                next_spc = self.sideband_state.spc
                next_tpc = self.sideband_state.tpc
                next_task_mode = self.sideband_state.task_mode
                ecause_mask = 0
                next_do_branch = 0
                if not self.sideband_state.task_mode:
                    if fetch_av:
                        next_spc = 0
                        ecause_mask |= 1 << exc_mip
                        next_do_branch = 1
                    else:
                        next_spc += inst_len + 1
                else:
                    if fetch_av:
                        next_task_mode = 0
                        ecause_mask |= 1 << exc_mip
                        next_do_branch = 1
                    else:
                        next_tpc += inst_len +1

                self.result_queue.append(Result(
                    data_l = result & 0xffff,
                    data_h = result >> 16,
                    data_en = 0 if fetch_av else 1,
                    addr = result_reg,
                    result_valid = True,
                    do_bse = 0,
                    do_wse = 0,
                    do_bze = 0,
                    do_wze = 0,
                    spc_out = next_spc,
                    tpc_out = next_tpc,
                    task_mode_out = next_task_mode,
                    ecause_out = self.sideband_state.ecause | ecause_mask,
                    do_branch = next_do_branch,
                ))
                yield from wait_for_transfer()

            def send_alu_op(op: alu_ops, op_a: int, op_b: int, op_c: int = None, *, result_reg = 0, result_reg_valid = True, fetch_av = False, inst_len = inst_len_16):
                yield from send_rr_op(op_class.alu, op, op_a, op_b, op_c, result_reg=result_reg, result_reg_valid=result_reg_valid, fetch_av=fetch_av, inst_len=inst_len)
            def send_shifter_op(op: shifter_ops, op_a: int, op_b: int, op_c: int = None, *, result_reg = 0, result_reg_valid = True, fetch_av = False, inst_len = inst_len_16):
                yield from send_rr_op(op_class.shift, op, op_a, op_b, op_c, result_reg=result_reg, result_reg_valid=result_reg_valid, fetch_av=fetch_av, inst_len=inst_len)
            def send_mult_op(op_a: int, op_b: int, op_c: int = None, *, result_reg = 0, result_reg_valid = True, fetch_av = False, inst_len = inst_len_16):
                yield from send_rr_op(op_class.mult, None, op_a, op_b, op_c, result_reg=result_reg, result_reg_valid=result_reg_valid, fetch_av=fetch_av, inst_len=inst_len)
            def send_bubble():
                self.output_port.exec_unit <<= None
                self.output_port.alu_op <<= None
                self.output_port.shifter_op <<= None
                self.output_port.branch_op <<= None
                self.output_port.ldst_op <<= None
                self.output_port.op_a <<= None
                self.output_port.op_b <<= None
                self.output_port.op_c <<= None
                self.output_port.mem_access_len <<= None
                self.output_port.inst_len <<= None
                self.output_port.do_bse <<= None
                self.output_port.do_wse <<= None
                self.output_port.do_bze <<= None
                self.output_port.do_wze <<= None
                self.output_port.result_reg_addr <<= None
                self.output_port.result_reg_addr_valid <<= None
                self.output_port.fetch_av <<= None
                self.output_port.valid <<= 0
                yield from wait_clk()

            def send_cbranch_op(op: branch_ops, op_a: int, op_b: int, op_c: int = None, *, fetch_av = False, inst_len = inst_len_16):
                self.output_port.exec_unit <<= op_class.branch
                self.output_port.alu_op <<= alu_ops.a_minus_b
                self.output_port.shifter_op <<= None
                self.output_port.branch_op <<= op
                self.output_port.ldst_op <<= None
                self.output_port.op_a <<= op_a
                self.output_port.op_b <<= op_b
                self.output_port.op_c <<= op_c
                self.output_port.mem_access_len <<= None
                self.output_port.inst_len <<= inst_len
                self.output_port.do_bse <<= 0
                self.output_port.do_wse <<= 0
                self.output_port.do_bze <<= 0
                self.output_port.do_wze <<= 0
                self.output_port.result_reg_addr <<= None
                self.output_port.result_reg_addr_valid <<= 0
                self.output_port.fetch_av <<= 1 if fetch_av else 0

                pc = self.sideband_state.tpc if self.sideband_state.task_mode else self.sideband_state.spc

                def to_signed(i: int, length: int = 32) -> int:
                    mask = (1 << length) - 1
                    msb = 1 << (length-1)
                    i = i & mask
                    if i & msb == 0:
                        return i
                    return i - mask - 1

                def bit_idx(i: int) -> int:
                    if i == 0x0: return 0
                    if i == 0x1: return 1
                    if i == 0x2: return 2
                    if i == 0x3: return 3
                    if i == 0x4: return 4
                    if i == 0x5: return 5
                    if i == 0x6: return 6
                    if i == 0x7: return 7
                    if i == 0x8: return 8
                    if i == 0x9: return 9
                    if i == 0xa: return 14
                    if i == 0xb: return 15
                    if i == 0xc: return 16
                    if i == 0xd: return 30
                    if i == 0xe: return 31
                    assert False, f"Invalid bit index value: {i}"

                ecause_mask = 0
                is_exception = fetch_av
                if fetch_av:
                    ecause_mask |= 1 << exc_mip

                if op_c is not None:
                    offset_msb = op_c & 1
                    if offset_msb != 0:
                        offset_msb = 0xffff0000
                    offset = offset_msb | ((op_c & 0xfffe) >> 1)
                if op == branch_ops.cb_eq:
                    branch = op_a == op_b
                    branch_target = (pc + offset) & 0x7fffffff
                elif op == branch_ops.cb_ne:
                    branch = op_a != op_b
                    branch_target = (pc + offset) & 0x7fffffff
                elif op == branch_ops.cb_lts:
                    branch = to_signed(op_a) < to_signed(op_b)
                    branch_target = (pc + offset) & 0x7fffffff
                elif op == branch_ops.cb_ges:
                    branch = to_signed(op_a) >= to_signed(op_b)
                    branch_target = (pc + offset) & 0x7fffffff
                elif op == branch_ops.cb_lt:
                    branch = op_a < op_b
                    branch_target = (pc + offset) & 0x7fffffff
                elif op == branch_ops.cb_ge:
                    branch = op_a >= op_b
                    branch_target = (pc + offset) & 0x7fffffff
                elif op == branch_ops.bb_one:
                    branch = (op_a & (1 << bit_idx(op_b))) != 0
                    branch_target = (pc + offset) & 0x7fffffff
                elif op == branch_ops.bb_zero:
                    branch = (op_a & (1 << bit_idx(op_b))) == 0
                    branch_target = (pc + offset) & 0x7fffffff
                elif op == branch_ops.swi:
                    if not fetch_av:
                        ecause_mask |= 1 << (op_a & 7)
                    is_exception = True
                    branch = True
                    branch_target = None
                elif op == branch_ops.stm:
                    if not self.sideband_state.task_mode:
                        branch_target = pc + inst_len + 1
                        branch = True
                    else:
                        branch = False
                elif op == branch_ops.pc_w:
                    branch = True
                    branch_target = op_c >> 1
                elif op == branch_ops.tpc_w:
                    if self.sideband_state.task_mode:
                        branch = True
                        branch_target = None
                    else:
                        branch = False
                elif op == branch_ops.pc_w_r:
                    branch = True
                    branch_target = op_a >> 1
                elif op == branch_ops.tpc_w_r:
                    if self.sideband_state.task_mode:
                        branch = True
                        branch_target = None
                    else:
                        branch = False

                next_pc = pc + inst_len + 1 if not branch else branch_target
                if not is_exception:
                    next_spc = self.sideband_state.spc if     self.sideband_state.task_mode else next_pc
                    next_tpc = self.sideband_state.tpc if not self.sideband_state.task_mode else next_pc
                    next_task_mode = self.sideband_state.task_mode
                    if op == branch_ops.stm: next_task_mode = 1
                    if op == branch_ops.tpc_w: next_tpc = op_c >> 1
                    if op == branch_ops.tpc_w_r: next_tpc = op_a >> 1
                else:
                    if self.sideband_state.task_mode:
                        next_tpc = self.sideband_state.tpc
                        next_spc = self.sideband_state.spc
                    else:
                        next_tpc = self.sideband_state.tpc
                        next_spc = 0
                    next_task_mode = 0

                self.result_queue.append(Result(
                    data_l = None,
                    data_h = None,
                    data_en = None, # Since we aren't supposed to get a valid, it doesn't matter what data_en is
                    addr = None,
                    result_valid = False,
                    do_bse = None,
                    do_wse = None,
                    do_bze = None,
                    do_wze = None,
                    spc_out = next_spc,
                    tpc_out = next_tpc,
                    task_mode_out = next_task_mode,
                    ecause_out = self.sideband_state.ecause | ecause_mask,
                    do_branch = branch,
                ))
                yield from wait_for_transfer()

            self.output_port.valid <<= 0
            yield from wait_clk()
            while self.rst:
                yield from wait_clk()
            for i in range(5): yield from wait_clk()
            set_side_band()
            yield from send_alu_op(alu_ops.a_plus_b, 4, 3)
            yield from send_alu_op(alu_ops.a_minus_b, 7, 9)
            yield from send_alu_op(alu_ops.a_and_b, 4, 3)
            yield from send_alu_op(alu_ops.a_or_b, 12, 43)
            yield from send_alu_op(alu_ops.a_xor_b, 23, 12)
            yield from send_alu_op(alu_ops.not_b_and_a, 4, 3)
            set_side_band()
            yield from send_alu_op(alu_ops.a_plus_b, 4, 3, fetch_av=True)
            yield from send_alu_op(alu_ops.not_b_and_a, 4, 3)
            yield from send_mult_op(41,43)
            yield from send_shifter_op(shifter_ops.shll,0xf0000001,2)
            yield from send_shifter_op(shifter_ops.shll,0xf0000001,31)
            yield from send_shifter_op(shifter_ops.shll,0xf0000001,32)
            yield from send_shifter_op(shifter_ops.shlr,0xff00ff00,0)
            yield from send_shifter_op(shifter_ops.shlr,0xff00ff00,1)
            yield from send_shifter_op(shifter_ops.shlr,0xff00ff00,8)
            yield from send_shifter_op(shifter_ops.shar,0xff00ff00,0)
            yield from send_shifter_op(shifter_ops.shar,0xff00ff00,1)
            yield from send_shifter_op(shifter_ops.shar,0xff00ff00,8)
            set_side_band(tpc=0xddccbba, spc=0x2233445, task_mode=True)
            yield from send_alu_op(alu_ops.tpc, 3, 2, 1)
            yield from send_alu_op(alu_ops.pc, 3, 2, 1)
            yield from send_bubble()
            yield from send_bubble()
            yield from send_bubble()
            set_side_band(tpc=0xddccbba, spc=0x2233445, task_mode=False)
            yield from send_alu_op(alu_ops.tpc, 3, 2, 1)
            yield from send_alu_op(alu_ops.pc, 3, 2, 1)
            ### branch tests
            for i in range(5):
                yield from send_bubble()
            yield from send_cbranch_op(branch_ops.cb_eq, 3, 4, 0x1000)
            yield from send_cbranch_op(branch_ops.cb_eq, 15, 15, 0x2000)
            yield from send_cbranch_op(branch_ops.cb_ne, 3, 4, 0x1000)
            yield from send_cbranch_op(branch_ops.cb_ne, 15, 15, 0x2000)
            yield from send_cbranch_op(branch_ops.cb_lt, 3, 4, 0x1000)
            yield from send_cbranch_op(branch_ops.cb_lt, 15, 15, 0x2000)
            yield from send_cbranch_op(branch_ops.cb_lt, 4, 3, 0x3000)
            yield from send_cbranch_op(branch_ops.cb_lts, 3, 4, 0x1000)
            yield from send_cbranch_op(branch_ops.cb_lts, 15, 15, 0x2000)
            yield from send_cbranch_op(branch_ops.cb_lts, 4, 3, 0x3000)
            yield from send_cbranch_op(branch_ops.cb_ge, 3, 4, 0x1000)
            yield from send_cbranch_op(branch_ops.cb_ge, 15, 15, 0x2000)
            yield from send_cbranch_op(branch_ops.cb_ge, 4, 3, 0x3000)
            yield from send_cbranch_op(branch_ops.cb_ges, 3, 4, 0x1000)
            yield from send_cbranch_op(branch_ops.cb_ges, 15, 15, 0x2000)
            yield from send_cbranch_op(branch_ops.cb_ges, 4, 3, 0x3000)
            yield from send_cbranch_op(branch_ops.bb_one, 15, 3, 0x1000)
            yield from send_cbranch_op(branch_ops.bb_one, 15, 6, 0x2000)
            yield from send_cbranch_op(branch_ops.bb_zero, 15, 3, 0x1000)
            yield from send_cbranch_op(branch_ops.bb_zero, 15, 6, 0x2000)
            yield from send_cbranch_op(branch_ops.swi, 1, None, None)
            set_side_band(tpc=0xddccbba, spc=0x2233445, task_mode=True)
            yield from send_cbranch_op(branch_ops.swi, 2, None, None)
            yield from send_cbranch_op(branch_ops.swi, 3, None, None, fetch_av=True)
            yield from send_cbranch_op(branch_ops.pc_w, None, None, 0x1111)
            yield from send_cbranch_op(branch_ops.pc_w_r, 0x2222, None, None)
            yield from send_cbranch_op(branch_ops.tpc_w, None, None, 0x3333)
            yield from send_cbranch_op(branch_ops.tpc_w_r, 0x4444, None, None)
            set_side_band(tpc=0xddccbba, spc=0x2233445, task_mode=False)
            yield from send_cbranch_op(branch_ops.pc_w, None, None, 0x1111)
            yield from send_cbranch_op(branch_ops.pc_w_r, 0x2222, None, None)
            yield from send_cbranch_op(branch_ops.tpc_w, None, None, 0x3333)
            yield from send_cbranch_op(branch_ops.tpc_w_r, 0x4444, None, None)
            yield from send_cbranch_op(branch_ops.stm, None, None, None)
            set_side_band(tpc=0xddccbba, spc=0x2233445, task_mode=True)
            yield from send_cbranch_op(branch_ops.stm, None, None, None)
            ### random ALU tests
            for i in range(5):
                yield from send_bubble()
            for i in range(100):
                op_a = randint(0, 0xffffffff)
                op_b = randint(0, 0xffffffff)
                yield from send_alu_op(alu_ops.a_plus_b,    op_a, op_b)
                yield from send_alu_op(alu_ops.a_minus_b,   op_a, op_b)
                yield from send_alu_op(alu_ops.a_and_b,     op_a, op_b)
                yield from send_alu_op(alu_ops.a_or_b,      op_a, op_b)
                yield from send_alu_op(alu_ops.a_xor_b,     op_a, op_b)
                yield from send_alu_op(alu_ops.not_b_and_a, op_a, op_b)

    class ResultChecker(GenericModule):
        clk = ClkPort()
        rst = RstPort()

        input_port = Input(ResultExtendIf)
        spc_out = Input(BrewInstAddr)
        tpc_out = Input(BrewInstAddr)
        task_mode_out = Input(logic)
        ecause_out = Input(Unsigned(12))
        do_branch = Input(logic)
        complete = Input(logic)

        def construct(self, result_queue: Array, sideband_state):
            self.result_queue = result_queue
            self.sideband_state = sideband_state

        def body(self):
            self.spc = Wire()
            self.spc <<= Reg(self.spc_out)
            self.tpc = Wire()
            self.tpc <<= Reg(self.tpc_out)
            self.task_mode = Wire()
            self.task_mode <<= Reg(self.task_mode_out)
            self.ecause = Wire()
            self.ecause <<= Reg(self.ecause_out)

        def simulate(self) -> TSimEvent:
            def wait_clk():
                yield (self.clk, )
                while self.clk.get_sim_edge() != EdgeType.Positive:
                    yield (self.clk, )

            while True:
                self.input_port.ready <<= 1
                yield from wait_clk()
                if self.complete == 1:
                    expected: Result = self.result_queue.pop(0)
                    if expected.result_valid:
                        print(f"Writing REG $r{self.input_port.addr:x} with value {self.input_port.data_h:04x}{self.input_port.data_l:04x} (expected: {expected.data_h:04x}{expected.data_l:04x}) enable: {self.input_port.data_en}")
                    else:
                        print(f"Result $tpc:{self.tpc:08x} (expected:{expected.tpc_out:08x}) $spc:{self.spc:08x} (expected:{expected.spc_out:08x}) task_mode:{self.task_mode} (expected:{expected.task_mode_out})")
                    assert expected.compare(self.input_port, self.spc, self.tpc, self.task_mode, self.ecause, self.do_branch)

    class CsrEmulator(Module):
        clk = ClkPort()
        rst = RstPort()

        input_port = Input(CsrIf)

        def simulate(self) -> TSimEvent:
            def wait_clk():
                yield (self.clk, )
                while self.clk.get_sim_edge() != EdgeType.Positive:
                    yield (self.clk, )

            self.input_port.rd_data <<= None
            while True:
                yield from wait_clk()

                if self.rst == 1:
                    self.input_port.rd_data <<= None
                else:
                    if self.input_port.request == 1:
                        if self.input_port.read_not_write:
                            # Read request
                            print(f"Reading CSR {self.input_port.addr:x}")
                            self.input_port.rd_data <<= None
                            yield from wait_clk()
                            self.input_port.rd_data <<= self.input_port.addr
                        else:
                            # Write request
                            print(f"Writing CSR {self.input_port.addr:x} with {self.input_port.wr_data:x}")
                            self.input_port.rd_data <<= None
                    else:
                        self.input_port.rd_data <<= None

    '''
    class BusEmulator(Module):
        clk = ClkPort()
        rst = RstPort()

        input_port = Input(BusIfPortIf)

        def simulate(self, simulator: 'Simulator') -> TSimEvent:
            def wait_clk():
                yield (self.clk, )
                while self.clk.get_sim_edge() != EdgeType.Positive:
                    yield (self.clk, )

            self.input_port.response <<= 0
            self.input_port.data_out <<= None
            self.input_port.last <<= None
            while True:
                yield from wait_clk()

                if self.rst == 1:
                    self.input_port.response <<= 0
                    self.input_port.data_out <<= None
                    self.input_port.last <<= None
                else:
                    if self.input_port.request == 1:
                        burst_cnt = self.input_port.burst_len.sim_value+1
                        if self.input_port.read_not_write:
                            # Read request
                            addr = self.input_port.addr
                            data = self.input_port.addr[15:0]
                            print(f"Reading BUS {addr:x} burst:{self.input_port.burst_len} byte_en:{self.input_port.byte_en}")
                            self.input_port.response <<= 0
                            self.input_port.last <<= 0
                            self.input_port.data_out <<= None
                            #yield from wait_clk()
                            self.input_port.response <<= 1
                            for i in range(burst_cnt):
                                self.input_port.last <<= 1 if i == burst_cnt-1 else 0
                                self.input_port.response <<= 1 if i <= burst_cnt-1 else 0
                                yield from wait_clk()
                                print(f"    data:{data:x}")
                                self.input_port.data_out <<= data
                                data = data + 1
                            self.input_port.response <<= 0
                            self.input_port.last <<= 0
                        else:
                            # Write request
                            print(f"Writing BUS {self.input_port.addr:x} burst:{self.input_port.burst_len} byte_en:{self.input_port.byte_en}")
                            self.input_port.response <<= 0
                            self.input_port.last <<= 0
                            self.input_port.data_out <<= None
                            #print(f"    data:{self.input_port.data_in:x} at {simulator.now}")
                            #yield from wait_clk()
                            self.input_port.response <<= 1
                            for i in range(burst_cnt):
                                print(f"    data:{self.input_port.data_in:x} at {simulator.now}")
                                self.input_port.last <<= 1 if i == burst_cnt-1 else 0
                                self.input_port.response <<= 1 if i <= burst_cnt-1 else 0
                                yield from wait_clk()
                            self.input_port.response <<= 0
                            self.input_port.last <<= 0
                    else:
                        self.input_port.response <<= 0
                        self.input_port.data_out <<= None
                        self.input_port.last <<= None
        '''
    class top(Module):
        clk = ClkPort()
        rst = RstPort()

        def body(self):
            seed(0)
            result_queue = []

            class SidebandState(object): pass
            sideband_state = SidebandState()

            decode_emulator = DecodeEmulator(result_queue, sideband_state)
            csr_emulator = CsrEmulator()
            #bus_emulator = BusEmulator()
            result_checker = ResultChecker(result_queue, sideband_state)

            dut = ExecuteStage()

            dut.input_port <<= decode_emulator.output_port
            result_checker.input_port <<= dut.output_port

            csr_emulator.input_port <<= dut.csr_if
            #bus_emulator.input_port

            # side-band interfaces
            dut.mem_base <<= decode_emulator.mem_base
            dut.mem_limit <<= decode_emulator.mem_limit
            dut.spc_in <<= decode_emulator.spc_in
            dut.tpc_in  <<= decode_emulator.tpc_in
            dut.task_mode_in <<= decode_emulator.task_mode_in
            dut.ecause_in <<= decode_emulator.ecause_in
            dut.interrupt <<= decode_emulator.interrupt

            result_checker.spc_out <<= dut.spc_out
            result_checker.tpc_out <<= dut.tpc_out
            result_checker.task_mode_out <<= dut.task_mode_out
            result_checker.ecause_out <<= dut.ecause_out
            result_checker.do_branch <<= dut.do_branch
            result_checker.complete <<= dut.complete

        def simulate(self) -> TSimEvent:
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

    Build.simulation(top, "execute.vcd", add_unnamed_scopes=True)


def gen():
    def top():
        return ScanWrapper(ExecuteStage, {"clk", "rst"})

    netlist = Build.generate_rtl(top, "execute.sv")
    top_level_name = netlist.get_module_class_name(netlist.top_level)
    flow = QuartusFlow(target_dir="q_execute", top_level=top_level_name, source_files=("execute.sv",), clocks=(("clk", 10), ("top_clk", 100)), project_name="execute")
    flow.generate()
    flow.run()

if __name__ == "__main__":
    gen()
    #sim()

