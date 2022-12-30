#!/usr/bin/python3
from random import *
from typing import *
from silicon import *
from brew_types import *
from brew_utils import *

"""
Memory stage of the V1 pipeline.

This stage is sandwiched between 'execute' and 'write-back'.

It does the following:
- Handles handshaking with the bus interface
- Issues loads/stores
- Stalls the pipeline until memory responses come back (and passes over results for non-memory operations)

"""

class Memory(Module):
    clk = ClkPort()
    rst = RstPort()


    # Pipeline input
    bubble_in = Input(logic)
    is_load = Input(logic)
    is_store = Input(logic)
    result_reg_addr = Input(BrewRegAddr)
    result_in = Input(BrewData)
    mem_addr_in = Input(BrewAddr)
    mem_access_len = input(Unsigned(2))

    # side-band interfaces
    stall_in = Input(logic)
    stall_out = Output(logic) # The expectation is that multiply might be multi-cycle. We shall see...

    # Pipeline output
    bubble_out = Output(logic)
    result_reg_addr = Output(BrewRegAddr)
    result_out = Output(BrewData)

    # Interface to the bus interface
    request = Output(logic)
    grant   = Input(logic)
    read_not_write = Output(logic)
    addr    = Output(BrewBusAddr)
    data_in = Input(BrewData)
    data_out = Output(BrewData)
    burst_len = Output(Unsigned(2))
    byte_en = Output(Unsigned(2))

    def body(self):
        self.request <<= Reg(
            Select(
                self.bubble_in,
                self.is_load | self.is_store,
                False
            )
        )
        self.data_out <<= Reg(self.result_in)
        self.read_not_write <<= Reg(self.is_load)
        self.addr <<= Reg(self.mem_access_in[31:1])


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

        adder_result = SelectOne(
            self.opcode == ExecOpCodes.op_add,      self.op_a + self.op_b,
            self.opcode == ExecOpCodes.op_a_sub_b,  self.op_a + self.op_b,
            self.opcode == ExecOpCodes.op_b_sub_a,  self.op_b - self.op_a,
            self.opcode == ExecOpCodes.op_addr,     self.op_b + self.op_imm + (self.mem_base << BrewMemShift),
        )[31:0]
        shifter_result = SelectOne(
            self.opcode == ExecOpCodes.op_shll, self.op_a << self.op_b[5:0],
            self.opcode == ExecOpCodes.op_shlr, self.op_a >> self.op_b[5:0],
            self.opcode == ExecOpCodes.op_shar, Signed(32)(self.op_a) >> self.op_b[5:0],
        )[31:0]
        logic_result = SelectOne(
            self.opcode == ExecOpCodes.op_or,   self.op_a | self.op_b,
            self.opcode == ExecOpCodes.op_and,  self.op_a & self.op_b,
            self.opcode == ExecOpCodes.op_xor,  self.op_a ^ self.op_b,
        )
        cbranch_result = SelectOne(
            self.opcode == ExecOpCodes.op_cb_eq,   self.op_a == self.op_b,
            self.opcode == ExecOpCodes.op_cb_ne,   self.op_a != self.op_b,
            self.opcode == ExecOpCodes.op_cb_lts,  Signed(32)(self.op_a) <  Signed(32)(self.op_b),
            self.opcode == ExecOpCodes.op_cb_ges,  Signed(32)(self.op_a) >= Signed(32)(self.op_b),
            self.opcode == ExecOpCodes.op_cb_lt,   self.op_a <  self.op_b,
            self.opcode == ExecOpCodes.op_cb_ge,   self.op_a >= self.op_b,
        )
        bbranch_result = SelectOne(
            self.opcode == ExecOpCodes.op_bb_one,  self.op_a[bb_bit_idx(self.op_b)],
            self.opcode == ExecOpCodes.op_bb_zero, self.op_b[bb_bit_idx(self.op_a)],
        )

        mem_av = self.exec_unit == ExecUnits.exec_adder & self.opcode == ExecOpCodes.op_addr & adder_result > self.mem_limit

        pc = Select(self.task_mode_in, self.spc_in, self.tpc_in)

        branch_target = SelectOne(
            self.exec_unit == ExecUnits.exec_cbranch | self.exec_unit == ExecUnits.exec_bbranch, pc + self.mem_base + self.op_imm,
            self.exec_unit == ExecUnits.exec_misc & self.opcode == ExecOpCodes.op_misc_pc_w, self.op_imm,
            self.exec_unit == ExecUnits.exec_misc & self.opcode == ExecOpCodes.op_misc_tpc_w & self.task_mode_in, self.op_imm,
        )

        mult_result_large = self.op_a * self.op_b
        mult_result = mult_result_large[31:0]
        exec_result = SelectOne(
            self.exec_unit == ExecUnits.exec_adder, adder_result,
            self.exec_unit == ExecUnits.exec_shift, shifter_result,
            self.exec_unit == ExecUnits.exec_mult, mult_result,
            self.exec_unit == ExecUnits.exec_logic, logic_result,
        )
        is_branch = SelectOne(
            self.exec_unit == ExecUnits.exec_adder, mem_av,
            self.exec_unit == ExecUnits.exec_shift, 0,
            self.exec_unit == ExecUnits.exec_mult,  0,
            self.exec_unit == ExecUnits.exec_logic, 0,
            self.exec_unit == ExecUnits.exec_cbranch, cbranch_result,
            self.exec_unit == ExecUnits.exec_bbranch, bbranch_result,
            self.exec_unit == ExecUnits.exec_misc, SelectOne(
                self.opcode == ExecOpCodes.op_misc_swi,   1,
                self.opcode == ExecOpCodes.op_misc_stm,   1,
                self.opcode == ExecOpCodes.op_misc_pc_r,  0,
                self.opcode == ExecOpCodes.op_misc_tpc_r, 0,
                self.opcode == ExecOpCodes.op_misc_pc_w,  1,
                self.opcode == ExecOpCodes.op_misc_tpc_w, self.task_mode_in,
                self.opcode == ExecOpCodes.op_misc_bsi,   0,
                self.opcode == ExecOpCodes.op_misc_wsi,   0,

            )
        )

        branch_av = branch_target > self.mem_limit & is_branch


        is_exception = SelectOne(
            self.exec_unit == ExecUnits.exec_adder, mem_av,
            self.exec_unit == ExecUnits.exec_shift, 0,
            self.exec_unit == ExecUnits.exec_mult,  0,
            self.exec_unit == ExecUnits.exec_logic, 0,
            self.exec_unit == ExecUnits.exec_cbranch, branch_av,
            self.exec_unit == ExecUnits.exec_bbranch, branch_av,
            self.exec_unit == ExecUnits.exec_misc, SelectOne(
                self.opcode == ExecOpCodes.op_misc_swi,   1,
                self.opcode == ExecOpCodes.op_misc_stm,   0,
                self.opcode == ExecOpCodes.op_misc_pc_r,  0,
                self.opcode == ExecOpCodes.op_misc_tpc_r, 0,
                self.opcode == ExecOpCodes.op_misc_pc_w,  branch_av,
                self.opcode == ExecOpCodes.op_misc_tpc_w, branch_av,
                self.opcode == ExecOpCodes.op_misc_bsi,   0,
                self.opcode == ExecOpCodes.op_misc_wsi,   0,

            )
        )

        self.do_branch <<= Reg(is_branch)
        self.do_except <<= Reg(is_branch)
        self.bubble_out <<= Reg(self.bubble_in)
        self.mem_access_len_out <<= Reg(self.mem_access_len_in)
        self.result <<= Reg(exec_result)

        # Side-band info output
        self.spc_out <<= Select(self.bubble_in | self.stall_in,
            Select(
                self.task_mode_in,
                # In Scheduler mode
                Select(
                    is_branch,
                    Select(is_exception,
                        self.spc_in + self.inst_len + 1, # Inst len is 0=16-bit, 1=32-bit, 2=48-bit
                        0 # Exception in scheduler mode is reset
                    )
                ),
                # In Task mode
                self.spc_in
            ),
            # We're in a bubble or are stalled
            self.spc_in
        )
        self.tpc_out <<= Select(self.bubble_in | self.stall_in,
            Select(
                self.task_mode_in,
                # In Scheduler mode
                Select(
                    self.exec_unit == ExecUnits.exec_misc & self.opcode == ExecOpCodes.op_misc_tpc_w, branch_target,
                    self.tpc_in,

                ),
                # In Task mode
                Select(
                    is_branch,
                    Select(is_exception,
                        self.spc_in + self.inst_len + 1, # Inst len is 0=16-bit, 1=32-bit, 2=48-bit
                        0 # Exception in scheduler mode is reset
                    )
                )
            ),
            # We're in a bubble or are stalled
            self.tpc_in
        )
        # Still needs assignment
        task_mode_out = Output(logic)
        ecause_out = Output(Unsigned(8))


def gen():
    Build.generate_rtl(Fetch)

def sim():

    inst_choices = (
        (0x1100,                        ), # $r1 <- $r0 ^ $r0
        (0x20f0, 0x2ddd,                ), # $r1 <- short b001
        (0x300f, 0x3dd0, 0x3dd1,        ), # $r1 <- 0xdeadbeef
    )
    inst_stream = []
    class Generator(RvSimSource):
        def construct(self, max_wait_state: int = 0):
            super().construct(MemToFetchStream, None, max_wait_state)
            self.addr = -1
            self.inst_fetch_stream = []
        def generator(self, is_reset):
            if is_reset:
                return 0,0
            self.addr += 1
            while len(self.inst_fetch_stream) < 2:
                inst = inst_choices[randint(0,len(inst_choices)-1)]
                inst_stream.append(inst)
                self.inst_fetch_stream += inst
            # Don't combine the two instructions, because I don't want to rely on expression evaluation order. That sounds dangerous...
            data = self.inst_fetch_stream.pop(0)
            return self.addr, data

    class Checker(RvSimSink):
        def construct(self, max_wait_state: int = 0):
            super().construct(None, max_wait_state)
            self.cnt = 0
        def checker(self, value):
            def get_next_inst():
                inst = inst_stream.pop(0)
                print(f"  --- inst:", end="")
                for i in inst:
                    print(f" {i:04x}", end="")
                print("")
                has_prefix = inst[0] & 0x0ff0 == 0x0ff0
                if has_prefix:
                    prefix = inst[0]
                    inst = inst[1:]
                else:
                    prefix = None
                inst_len = len(inst)-1
                inst_code = 0
                for idx, word in enumerate(inst):
                    inst_code |= word << (16*idx)
                return prefix, has_prefix, inst_code, inst_len

            expected_prefix, expected_has_prefix, expected_inst_code, expected_inst_len = get_next_inst()
            print(f"Received: ", end="")
            if value.inst_bottom.has_prefix:
                print(f" [{value.inst_bottom.prefix:04x}]", end="")
            for i in range(value.inst_bottom.inst_len+1):
                print(f" {(value.inst_bottom.inst >> (16*i)) & 0xffff:04x}", end="")
            if value.has_top:
                print(f" top: {value.inst_top:04x}", end="")
            print("")

            assert expected_has_prefix == value.inst_bottom.has_prefix
            assert not expected_has_prefix or expected_prefix == value.inst_bottom.prefix
            assert expected_inst_len == value.inst_bottom.inst_len
            inst_mask = (1 << (16*(expected_inst_len+1))) - 1
            assert (expected_inst_code & inst_mask) == (value.inst_bottom.inst & inst_mask)
            if value.has_top == 1:
                expected_prefix, expected_has_prefix, expected_inst_code, expected_inst_len = get_next_inst()
                assert not expected_has_prefix
                assert expected_inst_len == 0
                assert expected_inst_code == value.inst_top

    class top(Module):
        clk = ClkPort()
        rst = RstPort()

        def body(self):
            seed(0)
            self.input_stream = Wire(MemToFetchStream)
            self.checker = Checker()
            self.generator = Generator()
            self.input_stream <<= self.generator.output_port
            dut = Fetch()
            dut.rst <<= self.rst
            dut.clk <<= self.clk
            self.checker.input_port <<= dut(self.input_stream)

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

            self.generator.max_wait_state = 2
            self.checker.max_wait_state = 5
            for i in range(500):
                yield from clk()
            self.generator.max_wait_state = 0
            self.checker.max_wait_state = 0
            for i in range(500):
                yield from clk()
            now = yield 10
            self.generator.max_wait_state = 5
            self.checker.max_wait_state = 2
            for i in range(500):
                yield from clk()
            now = yield 10
            print(f"Done at {now}")

    Build.simulation(top, "fetch.vcd", add_unnamed_scopes=True)

#gen()
sim()

