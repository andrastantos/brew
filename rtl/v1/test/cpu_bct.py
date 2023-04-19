#!/usr/bin/python3
# Basic confidence tests for the Brew V1 CPU

import sys
from pathlib import Path
import itertools
from dataclasses import dataclass

sys.path.append(str(Path(__file__).parent / ".." / ".." / ".." / ".." / "silicon"))
sys.path.append(str(Path(__file__).parent / ".." / ".." / ".." / ".." / "silicon" / "unit_tests"))
sys.path.append(str(Path(__file__).parent / ".." / ".." / ".." / ".." ))
#sys.path.append(str(Path(__file__).parent / ".." ))

from brew.rtl.v1.brew_v1 import BrewV1Top
from brew.rtl.v1.brew_types import *
from brew.rtl.v1.assembler import *
from silicon import *

con_base = 0x0001_0000

class RegFileLeech(Module):
    clk = ClkPort()
    rst = RstPort()

    def set_reg_file(self, reg_file: 'RegFile'):
        self.reg_file = reg_file
        self.wb_if = reg_file.write
    def simulate(self, simulator: Simulator):
        def wait_clk():
            yield self.clk
            while self.clk.get_sim_edge() != EdgeType.Positive:
                yield self.clk

        while True:
            yield from wait_clk()
            if self.wb_if.valid == 1 and self.wb_if.data_en == 1:
                reg_name = f"$r{self.wb_if.addr}"
                reg_value = f"{self.wb_if.data:08x} ({self.wb_if.data})"
                simulator.log(f"                          <<<<<<<<<< {reg_name} <= {reg_value}")

class ExecLeech(Module):
    clk = ClkPort()
    rst = RstPort()

    def set_execute(self, execute: 'ExecuteStage'):
        self.execute = execute
        self.do_branch = execute.do_branch
        self.tpc = execute.tpc_in
        self.spc = execute.spc_in
        self.task_mode = execute.task_mode_in
        self.exec_input = execute.input_port
    def simulate(self, simulator: Simulator):
        def wait_clk():
            yield self.clk
            while self.clk.get_sim_edge() != EdgeType.Positive:
                yield self.clk

        while True:
            yield from wait_clk()
            if (self.exec_input.valid & self.exec_input.ready) == 1:
                b = "SKIP" if self.do_branch == 1 else "Exec"
                if self.task_mode == 0:
                    prefix = "S"
                    pc = self.spc
                else:
                    prefix = "T"
                    pc = self.tpc
                if self.do_branch == 0:
                    simulator.log(f"{b}: {prefix}{(pc << 1):08x}")

class LdStLeech(Module):
    clk = ClkPort()
    rst = RstPort()

    def set_execute(self, execute: 'ExecuteStage'):
        self.execute = execute
        self.bus_req_if = execute.bus_req_if
        self.bus_rsp_if = execute.bus_rsp_if
    def simulate(self, simulator: Simulator):
        def wait_clk():
            yield self.clk
            while self.clk.get_sim_edge() != EdgeType.Positive:
                yield self.clk

        while True:
            yield from wait_clk()
            if (self.bus_req_if.valid & self.bus_req_if.ready) == 1:
                simulator.sim_assert(self.bus_req_if.byte_en != 0)
                if self.bus_req_if.byte_en == 1:
                    addr = self.bus_req_if.addr*2
                    access_size = 8
                if self.bus_req_if.byte_en == 2:
                    addr = self.bus_req_if.addr*2 + 1
                    access_size = 8
                if self.bus_req_if.byte_en == 3:
                    addr = self.bus_req_if.addr*2
                    access_size = 16
                if self.bus_req_if.read_not_write == 1:
                    mode = "reading"
                    value = ""
                elif self.bus_req_if.read_not_write == 0:
                    mode = "writing"
                    value = f" with value {self.bus_req_if.data:04x} ({self.bus_req_if.data})"
                else:
                    mode = "NONE"
                simulator.log(f"                          ---------- {mode} {access_size}-bit memory addr {addr:08x}{value}")

from copy import copy
class Dram(GenericModule):
    nRAS          = Input(logic)
    nCAS          = Input(logic)
    addr          = Input(Unsigned(11))
    nWE           = Input(logic)
    data_out      = Output(BrewByte)
    data_out_en   = Output(logic)
    data_in       = Input(BrewByte)
    data_in_en    = Input(logic)

    def construct(self, name:str, latency: int = 30, hold_time: int = 20):
        self.latency = latency
        self.hold_time = hold_time
        self.content = {}
        self.name = name

    def set_mem(self, addr: int, data: ByteString):
        for ofs, byte in enumerate(data):
            print(f"-------- programming DRAM {self.name} at address {addr+ofs:08x} with {byte:02x}")
            self.content[ofs+addr] = byte

    def get_mem(self, addr: int) -> Optional[int]:
        return self.content.get(addr, None)

    def simulate(self, simulator: Simulator):
        self.data_out <<= None
        self.data_out_en <<= 0
        while True:
            yield self.nRAS
            if self.nRAS.get_sim_edge() == EdgeType.Positive:
                # Get got deselected
                self.data_out <<= None
                self.data_out_en <<= 0
            elif self.nRAS.get_sim_edge() == EdgeType.Negative:
                simulator.sim_assert(self.nCAS == 1, f"DRAM {self.name}: nRAS should not assert while nCAS is low")
                row_addr = copy(self.addr.sim_value)
                while True:
                    yield self.nCAS, self.nRAS
                    if self.nRAS.get_sim_edge() == EdgeType.Positive:
                        # End of burst
                        break
                    simulator.sim_assert(self.nRAS.get_sim_edge() == EdgeType.NoEdge)
                    if self.nCAS.get_sim_edge() == EdgeType.Negative:
                        yield 0
                        yield 0
                        col_addr = copy(self.addr.sim_value)
                        addr = row_addr << self.addr.get_num_bits() | col_addr
                        if self.nWE == 1:
                            try:
                                value = self.content[addr]
                            except KeyError:
                                value = None
                            val_str = "--" if value is None else f"{value:02x}"
                            #simulator.log(f"                                              DRAM {self.name} Reading address {addr:08x}, returning {val_str}")
                            yield self.latency
                            self.data_out <<= value
                            self.data_out_en <<= 1
                        elif self.nWE == 0:
                            value = None if self.data_in_en != 1 else self.data_in
                            self.content[addr] = int(value)
                            val_str = "--" if value is None else f"{value:02x}"
                            #simulator.log(f"                                              DRAM {self.name} Writing address {addr:08x} with value {val_str}")
                            self.data_out <<= None
                            self.data_out_en <<= 0
                    elif self.nCAS.get_sim_edge() == EdgeType.Positive:
                        if self.nWE == 1:
                            yield self.hold_time
                            self.data_out <<= None
                            self.data_out_en <<= 0
                    else:
                        simulator.sim_assert(f"Unexpected nCAS edge: {self.nCAS.get_sim_edge()}")

class AddressDecode(Module):
    nNREN         = Input(logic)
    nCAS_0        = Input(logic)
    nCAS_1        = Input(logic)
    addr          = Input(Unsigned(11))
    full_addr     = Output(Unsigned(23))
    rom_en        = Output(logic)
    con_en        = Output(logic)

    def body(self):
        self.nCAS = Wire(logic)
        self.nCAS <<= self.nCAS_0 & self.nCAS_1

    def simulate(self, simulator: Simulator):
        global con_base
        self.rom_base = 0x0000_0000
        self.con_base = con_base
        self.decode_mask = 0xffff_0000
        self.rom_en <<= 0
        self.con_en <<= 0
        while True:
            yield self.nNREN
            if self.nNREN.get_sim_edge() == EdgeType.Negative:
                simulator.sim_assert(self.nCAS == 1, "nNREN should not assert while nCAS is low")
                row_addr = copy(self.addr.sim_value)
                while True:
                    yield self.nCAS, self.nNREN
                    if self.nNREN.get_sim_edge() == EdgeType.Positive:
                        # End of burst
                        self.rom_en <<= 0
                        self.con_en <<= 0
                        break
                    simulator.sim_assert(self.nNREN.get_sim_edge() == EdgeType.NoEdge)
                    if self.nCAS.get_sim_edge() == EdgeType.Negative:
                        col_addr = copy(self.addr.sim_value)
                        addr = (row_addr << self.addr.get_num_bits() | col_addr) << 1 | self.nCAS_0
                        self.full_addr <<= addr
                        if (addr & self.decode_mask) == self.con_base:
                            self.con_en <<= 1
                            self.rom_en <<= 0
                        elif (addr & self.decode_mask) == self.rom_base:
                            self.con_en <<= 0
                            self.rom_en <<= 1
                        else:
                            self.con_en <<= 0
                            self.rom_en <<= 0
                    elif self.nCAS.get_sim_edge() == EdgeType.Positive:
                        self.rom_en <<= 0
                        self.con_en <<= 0
                    else:
                        simulator.sim_assert(f"Unexpected nCAS edge: {self.nCAS.get_sim_edge()}")

class Rom(GenericModule):
    enable        = Input(logic)
    addr          = Input(Unsigned(23))
    data_out      = Output(BrewByte)
    data_out_en   = Output(logic)

    def append(self, words):
        for word in words:
            self.content.append((word >> 0) & 0xff)
            self.content.append((word >> 8) & 0xff)

    def set_mem(self, addr: int, data: ByteString):
        while self.get_size() < addr:
            self.content.append(None)
        for byte in data:
            self.content.append(byte)

    def get_mem(self, addr: int) -> Optional[int]:
        try:
            return self.content[addr]
        except IndexError:
            return None

    def get_size(self):
        return len(self.content)

    def construct(self, latency: int = 80, hold_time: int = 20):
        self.latency = latency
        self.hold_time = hold_time
        self.content = []

    def simulate(self, simulator: Simulator) -> TSimEvent:
        self.data_out <<= None
        self.data_out_en <<= 0
        while True:
            yield self.enable
            if self.enable.get_sim_edge() == EdgeType.Positive:
                # Got selected
                try:
                    value = self.content[int(self.addr)]
                except IndexError:
                    value = None
                val_str = "--" if value is None else f"{value:02x}"
                #simulator.log(f"ROM Reading address {self.addr:08x}, returning {val_str}")
                yield self.latency
                self.data_out <<= value
                self.data_out_en <<= 1
            elif self.enable.get_sim_edge() == EdgeType.Negative:
                yield self.hold_time
                self.data_out <<= None
                self.data_out_en <<= 0
            else:
                simulator.sim_assert(f"Unexpected ROM enable edge: {self.enable.get_sim_edge()}")

class Console(Module):
    enable        = Input(logic)
    addr          = Input(Unsigned(23))
    nWE           = Input(logic)
    data_in       = Input(BrewByte)
    data_in_en    = Input(logic)
    terminate     = Output(logic)

    def simulate(self, simulator: Simulator) -> TSimEvent:
        self.terminate <<= 0
        while True:
            yield self.enable
            if self.enable.get_sim_edge() == EdgeType.Positive:
                if self.nWE == 0:
                    value = None if self.data_in_en != 1 else self.data_in
                    val_str = "--" if value is None else f"{value:02x}"
                    addr = self.addr & 0xff
                    if addr == 0:
                        simulator.log(f"CONSOLE got value {val_str}")
                    if addr == 4:
                        simulator.log(f"SUCCESS")
                        self.terminate <<= 1
                    if addr == 8:
                        simulator.log(f"FAIL")
                        assert(False)
                        self.terminate <<= 1
            elif self.enable.get_sim_edge() == EdgeType.Negative:
                pass
            else:
                simulator.sim_assert(f"Unexpected CONSOLE enable edge: {self.enable.get_sim_edge()}")


"""
class TriStateConnect(Module):
    output_port: Output()

    def construct(self):
        self.input_ports = Dict()

    def create_named_port_callback(self, name: str, net_type: Optional['NetType'] = None) -> Optional[Port]:
        ret_val = Input(net_type)
        self.input_ports[name] = ret_val
        return ret_val
    def create_positional_port_callback(self, idx: int, net_type: Optional['NetType'] = None) -> Tuple[str, Port]:
        name = f"value_{idx}"
        return (name, self.create_named_port_callback(name, net_type))
    def generate_output_type(self) -> Optional['NumberMeta']:
        input_ports = self.input_ports.values()
        common_net_type = get_common_net_type(input_ports)
        if common_net_type is None:
            raise SyntaxErrorException(f"Can't figure out output port type for TriStateConnect")
        output_type = common_net_type.result_type(tuple(port.get_net_type() for port in input_ports), "SELECT") # Select has the same semantics as far as output type creation goes
        return output_type
    def simulate(self) -> TSimEvent:
        input_ports = self.input_ports.values()
        while True:
            yield input_ports
            for input_port in input_ports:
                if input_port.sim_value is not None:
                    self.output_port <<= input_port
                    break
"""

class top(Module):
    clk               = ClkPort()
    rst               = RstPort()

    nram_base = 0x0000_0000
    csr_base  = 0x4000_0000
    dram_base = 0x8000_0000

    def construct(self):
        self.pc = 0
        self.asm = BrewAssembler()
        self.timeout = 1500

    def body(self):
        self.cpu = BrewV1Top(csr_base=self.csr_base >> 30, nram_base=self.nram_base >> 30, has_multiply=True, has_shift=True, page_bits=7)
        self.dram_l = Dram(name="l")
        self.dram_h = Dram(name="h")
        self.addr_decode = AddressDecode()
        self.rom = Rom()
        self.con = Console()
        self.rf_leech = RegFileLeech()
        self.exec_leech = ExecLeech()
        self.ldst_leech = LdStLeech()

        self.dram_l.nRAS          <<= self.cpu.dram.nRAS_A
        self.dram_l.nCAS          <<= self.cpu.dram.nCAS_0
        self.dram_l.addr          <<= self.cpu.dram.addr
        self.dram_l.nWE           <<= self.cpu.dram.nWE
        self.dram_l.data_in       <<= self.cpu.dram.data_out
        self.dram_l.data_in_en    <<= self.cpu.dram.data_out_en

        self.dram_h.nRAS          <<= self.cpu.dram.nRAS_A
        self.dram_h.nCAS          <<= self.cpu.dram.nCAS_1
        self.dram_h.addr          <<= self.cpu.dram.addr
        self.dram_h.nWE           <<= self.cpu.dram.nWE
        self.dram_h.data_in       <<= self.cpu.dram.data_out
        self.dram_h.data_in_en    <<= self.cpu.dram.data_out_en

        self.cpu.dram.data_in     <<= SelectOne(
            self.dram_l.data_out_en, self.dram_l.data_out,
            self.dram_h.data_out_en, self.dram_h.data_out,
            self.rom.data_out_en,    self.rom.data_out,
        )

        self.addr_decode.nNREN    <<= self.cpu.dram.nNREN
        self.addr_decode.nCAS_0   <<= self.cpu.dram.nCAS_0
        self.addr_decode.nCAS_1   <<= self.cpu.dram.nCAS_1
        self.addr_decode.addr     <<= self.cpu.dram.addr

        self.rom.enable           <<= self.addr_decode.rom_en
        self.rom.addr             <<= self.addr_decode.full_addr
        self.con.enable           <<= self.addr_decode.con_en
        self.con.addr             <<= self.addr_decode.full_addr
        self.con.nWE              <<= self.cpu.dram.nWE

        self.cpu.dram.nWAIT       <<= 1
        self.cpu.DRQ              <<= 0
        self.cpu.nINT             <<= 1

    def set_timeout(self, timeout):
        self.timeout = timeout

    def simulate(self, simulator: Simulator) -> TSimEvent:
        def get_reg_file():
            reg_file = first(first(self.cpu.get_inner_objects("pipeline")).get_inner_objects("reg_file"))
            return reg_file

        def get_exec():
            exec = first(first(self.cpu.get_inner_objects("pipeline")).get_inner_objects("execute_stage"))
            return exec

        self.rf_leech.set_reg_file(get_reg_file())
        self.exec_leech.set_execute(get_exec())
        self.ldst_leech.set_execute(get_exec())

        def clk() -> int:
            yield 50
            self.clk <<= ~self.clk & self.clk
            yield 50
            self.clk <<= ~self.clk
            yield 0

        #self.program()
        simulator.log("Simulation started")

        self.rst <<= 1
        self.clk <<= 1
        yield 10
        for i in range(5):
            yield from clk()
        self.rst <<= 0

        for i in range(self.timeout):
            if self.con.terminate == 1:
                break
            yield from clk()
        yield 10
        assert(self.con.terminate == 1)
        simulator.log("Done")

    def set_mem(self, addr: int, data: ByteString):
        section = addr & 0xc000_0000
        if section == self.nram_base:
            self.rom.set_mem(addr & 0x03ff_ffff, data)
        elif section == self.dram_base:
            for idx, byte in enumerate(data):
                dram_addr = (addr & 0x03ff_ffff) + idx
                if (dram_addr & 1) == 0:
                    self.dram_l.set_mem(dram_addr >> 1, byte.to_bytes(1,"little"))
                else:
                    self.dram_h.set_mem(dram_addr >> 1, byte.to_bytes(1,"little"))
        else:
            raise SimulationException(f"Address {addr:08x} doesn't fall into any mapped memory region")

    def program(self, segments):
        for segment in segments:
            self.set_mem(segment.base_addr, segment.content)

def fail():
    global con_base
    mem32_I_eq_r(con_base+8, "$r0")

def check_reg(reg, value):
    global con_base
    r_eq_I_xor_r(reg, value, reg)
    if_r_eq_z(reg, get_dot()+10) # 10 bytes equals a 4-byte test and a 6-byte write instructions
    fail()
    r_eq_I_xor_r(reg, value, reg)

def terminate():
    global con_base
    mem32_I_eq_r(con_base+4, "$r0")

#def con_wr(reg, tmp_reg=14):
#    nonlocal con_base
#    prog(a.r_eq_r_or_r(tmp_reg, reg, reg))
#    prog(a.r_eq_r_shl_i(tmp_reg, tmp_reg, 28))
#    prog(a.r_eq_r_and_i(tmp_reg, tmp_reg, 16))
#    prog(a.if_r_)
#def con_wr(self, reg):
#    nonlocal con_base
#    self.prog(self.asm.mem32_I_eq_r(con_base, reg))

def test_1(top):
    """
    This test loads a few registers, then jumps to a zero-wait-state ROM address, but continues executing from the same physical location.
    It then loads a few more registers before terminating. This test is not self-checking, the log should be diffed for catching regressions.
    """

    create_segment("code", 0)
    create_segment("code0", 0x0400_0000)
    set_active_segment("code0")
    place_symbol("code0_start")
    set_active_segment("code")

    r_eq_t("$r0",0)
    r_eq_r_plus_t("$r1","$r0",1)
    r_eq_r_plus_t("$r2","$r0",2)
    r_eq_r_plus_t("$r3","$r0",3)
    r_eq_r_plus_t("$r4","$r0",4)
    r_eq_r_plus_t("$r5","$r0",5)
    r_eq_r_plus_r("$r6","$r5","$r1")
    r_eq_r_plus_r("$r7","$r5","$r2")
    pc_eq_I(get_dot().offset+0x0400_0000+6) # Setting internal wait-states to 0, but otherwise continue execution
    r_eq_r_plus_r("$r8","$r5","$r3")
    r_eq_r_plus_r("$r9","$r5","$r4")
    r_eq_r_plus_r("$r10","$r5","$r5")
    r_eq_r_plus_r("$r11","$r6","$r5")
    r_eq_I("$r12",12)
    r_eq_r_plus_r("$r13","$r7","$r6")
    r_eq_r_plus_r("$r14","$r7","$r7")
    terminate()


def test_2(top):
    """
    This test jumps to DRAM right out of reset, loads the registers, tests them and terminates
    """

    top.set_timeout(3000)

    create_segment("code", 0)
    create_segment("code_dram", 0x8000_0000)
    set_active_segment("code_dram")
    place_symbol("_start")
    set_active_segment("code")

    pc_eq_I("_start")

    set_active_segment("code_dram")
    r_eq_t("$r0",0)
    r_eq_r_plus_t("$r1","$r0",1)
    r_eq_r_plus_t("$r2","$r0",2)
    r_eq_r_plus_t("$r3","$r0",3)
    r_eq_r_plus_t("$r4","$r0",4)
    r_eq_r_plus_t("$r5","$r0",5)
    r_eq_r_plus_r("$r6","$r5","$r1")
    r_eq_r_plus_r("$r7","$r5","$r2")
    r_eq_r_plus_r("$r8","$r5","$r3")
    r_eq_r_plus_r("$r9","$r5","$r4")
    r_eq_r_plus_r("$r10","$r5","$r5")
    r_eq_r_plus_r("$r11","$r6","$r5")
    r_eq_I("$r12",12)
    r_eq_r_plus_r("$r13","$r7","$r6")
    r_eq_r_plus_r("$r14","$r7","$r7")
    for i in range(14):
        check_reg(f"$r{i}", i)
    terminate()


def test_3(top):
    """
    This test jumps to DRAM right out of reset, then loads a few registers, and a few loads and stores before entering an endless loop.
    """

    create_segment("code", 0)
    create_segment("code_dram", 0x8000_0000)
    set_active_segment("code_dram")
    place_symbol("_start")
    set_active_segment("code")

    pc_eq_I("_start")

    set_active_segment("code_dram")
    r_eq_I("$r13",0xdeadbeef)
    r_eq_I("$r14",0x12345678)
    mem32_I_eq_r(0x8000_1000,"$r14")
    r_eq_r_plus_t("$r14","$r14",5)
    r_eq_mem32_I("$r13",0x8000_1000)

    check_reg("$r13", 0x12345678)
    check_reg("$r14", 0x12345678+5)
    terminate()


def test_4(top):
    """
    Test jumping to task mode, then back to scheduler mode due to a fetch AV
    """

    top.set_timeout(3000)

    create_segment("code", 0)
    create_segment("code_dram", 0x8000_0000)
    create_segment("code_task", 0x8000_1000)
    set_active_segment("code_dram")
    place_symbol("_start")
    set_active_segment("code_task")
    place_symbol("_task_start")
    set_active_segment("code")

    pc_eq_I("_start")

    set_active_segment("code_dram")
    r_eq_t("$r0",0)
    r_eq_r_plus_t("$r1","$r0",1)
    r_eq_r_plus_t("$r2","$r0",2)
    r_eq_r_plus_t("$r3","$r0",3)
    r_eq_r_plus_t("$r4","$r0",4)
    r_eq_r_plus_t("$r5","$r0",5)
    r_eq_r_plus_r("$r6","$r5","$r1")
    r_eq_r_plus_r("$r7","$r5","$r2")
    r_eq_r_plus_r("$r8","$r5","$r3")
    r_eq_r_plus_r("$r9","$r5","$r4")
    r_eq_r_plus_r("$r10","$r5","$r5")
    r_eq_r_plus_r("$r11","$r6","$r5")
    r_eq_I("$r12",12)
    r_eq_r_plus_r("$r13","$r7","$r6")
    r_eq_r_plus_r("$r14","$r7","$r7")

    place_symbol("spc_loop")
    tpc_eq_I("_task_start")
    r_eq_r_plus_t("$r5","$r5",-1)
    stm()
    if_r_ne_z("$r5", "spc_loop")
    r_eq_r_plus_t("$r0","$r0",1)

    check_reg("$r0",   1)
    check_reg("$r1",   1)
    check_reg("$r2",   2)
    check_reg("$r3",   3)
    check_reg("$r4",   4)
    check_reg("$r5",   0)
    check_reg("$r6",   6)
    check_reg("$r7",   7)
    check_reg("$r8",   8)
    check_reg("$r9",   9)
    check_reg("$r10", 10)
    check_reg("$r11", 11)
    check_reg("$r12", 12)
    check_reg("$r13", 13)
    check_reg("$r14", 14)
    terminate()

    set_active_segment("code_task")
    r_eq_r_plus_t("$r2","$r2",1)
    pc_eq_I("_task_start")


def test_5(top):
    """
    Test jumping back and forth between task and system mode, no exceptions thrown
    """

    top.set_timeout(3000)

    create_segment("code", 0)
    create_segment("code_dram", 0x8000_0000)
    create_segment("code_task", 0x8000_1000)
    set_active_segment("code_dram")
    place_symbol("_start")
    set_active_segment("code_task")
    place_symbol("_task_start")
    set_active_segment("code")

    pc_eq_I("_start")

    set_active_segment("code_dram")
    r_eq_t("$r0",0)
    r_eq_r_plus_t("$r1","$r0",1)
    r_eq_r_plus_t("$r2","$r0",2)
    r_eq_r_plus_t("$r3","$r0",3)
    r_eq_r_plus_t("$r4","$r0",4)
    r_eq_r_plus_t("$r5","$r0",5)
    r_eq_r_plus_r("$r6","$r5","$r1")
    r_eq_r_plus_r("$r7","$r5","$r2")
    r_eq_r_plus_r("$r8","$r5","$r3")
    r_eq_r_plus_r("$r9","$r5","$r4")
    r_eq_r_plus_r("$r10","$r5","$r5")
    r_eq_r_plus_r("$r11","$r6","$r5")
    r_eq_I("$r12",12)
    r_eq_r_plus_r("$r13","$r7","$r6")
    r_eq_r_plus_r("$r14","$r7","$r7")

    r_eq_I("$r0",0xffffffff)
    mem32_I_eq_r(top.cpu.csr_mem_limit_reg,"$r0")
    r_eq_t("$r0",0)
    mem32_I_eq_r(top.cpu.csr_mem_base_reg,"$r0")
    tpc_eq_I("_task_start")

    # Scheduler mode loop: decrementing $r5
    place_symbol("spc_loop")
    r_eq_r_plus_t("$r5","$r5",-1)
    stm()
    r_eq_tpc("$r10") # Adjust $tpc to be over the SWI instruction
    r_eq_r_plus_t("$r10","$r10",2)
    tpc_eq_r("$r10")
    if_r_ne_z("$r5", "spc_loop")
    r_eq_r_plus_t("$r0","$r0",1)

    check_reg("$r0",   1)
    check_reg("$r1",   1)
    check_reg("$r2",   2)
    check_reg("$r3",   3)
    check_reg("$r4",   4)
    check_reg("$r5",   0)
    check_reg("$r6",   1)
    check_reg("$r7",   3)
    check_reg("$r8",   8)
    check_reg("$r9",   9)
    #check_reg("$r10", 10)
    check_reg("$r11", 11)
    check_reg("$r12", 12)
    check_reg("$r13", 13)
    check_reg("$r14", 14)
    terminate()


    # Task mode loop: decrementing $r6 and $r7
    set_active_segment("code_task")
    r_eq_r_plus_t("$r6","$r6",-1)
    swi(3)
    r_eq_r_plus_t("$r7","$r7",-1)
    pc_eq_I("_task_start")
    r_eq_r_plus_t("$r1","$r1",1)

def pc_rel(location):
    # Return a pc-relative address, based on munging for conditional branch instruction rules:
    #    replicate LSB to bit positions [31:16], replace LSB with 0.
    # To munge an address, we'll take the sign bit (bit 16) and stuff it in the location of bit-0.
    # We assert that bit-0 is 0 and that the relative location is within 64kWords
    #nonlocal pc
    linear = location-pc
    assert(abs(linear) < 0xffff)
    assert(linear & 1 == 0)
    bit_16 = (linear >> 16) & 1
    return (linear & 0xffff) | bit_16

def test_6(top):
    """
    Test conditional branches.
    """
    #nonlocal pc
    #nonlocal top_inst
    pc = 0
    task_start = 0x8000_1000
    prog(a.pc_eq_I(0x8000_0000)) # Jumping to DRAM
    pc = 0x8000_0000
    prog(a.r_eq_t(0,0))
    prog(a.r_eq_r_plus_t(1,0,1))
    prog(a.r_eq_r_plus_t(2,0,2))
    prog(a.r_eq_r_plus_t(3,0,3))
    prog(a.r_eq_r_plus_t(4,0,4))
    prog(a.r_eq_r_plus_t(5,0,5))
    prog(a.r_eq_r_plus_r(6,5,1))
    prog(a.r_eq_r_plus_r(7,5,2))
    prog(a.r_eq_r_plus_r(8,5,3))
    prog(a.r_eq_r_plus_r(9,5,4))
    prog(a.r_eq_r_plus_r(10,5,5))
    prog(a.r_eq_r_plus_r(11,6,5))
    prog(a.r_eq_I(12,12))
    prog(a.r_eq_r_plus_r(11,6,5))
    prog(a.r_eq_r_plus_r(12,6,6))
    prog(a.r_eq_r_plus_r(13,7,6))
    prog(a.r_eq_r_plus_r(14,7,7))
    check_reg(10,10)
    prog(a.r_eq_mem32_I(0,0x8000_0000))
    prog(a.mem32_I_eq_r(0x8000_0800,14))
    loop = pc
    prog(a.r_eq_r_minus_r(4,4,1))
    prog(a.if_r_ne_z(4,pc_rel(loop)))
    loop = pc
    prog(a.r_eq_r_plus_t(4,4,1))
    prog(a.if_r_ne_r(4,5,pc_rel(loop)))
    terminate()
    loop = pc
    prog(a.pc_eq_I(loop))



r = [None]*15

def startup():
    """
    Setting up initial segments, jump to DRAM and load all registers
    """
    global r

    create_segment("code", 0)
    create_segment("code_dram", 0x8000_0000)
    set_active_segment("code_dram")
    place_symbol("_start")
    set_active_segment("code")

    pc_eq_I("_start")

    set_active_segment("code_dram")
    for i in range(15):
        if i <= 7:
            r_eq_t(f"$r{i}",i)
        else:
            r_eq_r_plus_t(f"$r{i}",f"$r{i-7}",7)
        r[i] = i

def check(start=0, stop=14):
    """
    Test that all HW registers match the expectations
    """
    global r
    for idx in range(start, stop+1):
        check_reg(f"$r{idx}",   r[idx])


def test_framework(top):
    """
    Test register-to-register ALU operations
    """

    top.set_timeout(3000)

    startup()
    check()
    terminate()

def load_reg(reg, value):
    idx = int(reg[-1])
    r[idx] = value
    if value < 0xffff and value > 0:
        r_eq_i(reg, value)
    elif value < 0x7fff and value > -0x8000:
        r_eq_i(reg, value & 0xffff)
    else:
        r_eq_I(reg, value)

def shl(a,b):
    return (a << (b & 31)) & 0xffffffff

def shr(a,b):
    return ((a & 0xffffffff) >> (b & 31)) & 0xffffffff

def sar(a,b):
    lsb = (a >> 31) & 1
    if lsb != 0:
        top_bits = (lsb << (b & 31)) - 1
    else:
        top_bits = 0
    top_bits <<= (32-(b & 31))
    return shr(a,b) | (top_bits & 0xffffffff)

def test_alu_rr(top):
    """
    Test register-to-register ALU operations
    """

    top.set_timeout(6000)

    startup()
    load_reg("$r3", 0xff00f0f0)
    load_reg("$r4", 0x0f0f00ff)
    r[5] = (r[3] ^ r[4]) & 0xffffffff
    r[6] = (r[3] | r[4]) & 0xffffffff
    r[7] = (r[3] & r[4]) & 0xffffffff
    r[8] = (r[3] + r[4]) & 0xffffffff
    r[9] = (r[3] - r[4]) & 0xffffffff
    r[10] = shl(r[3], r[2])
    r[11] = shr(r[3], r[2])
    #r[12] = (r[3] >>> (r[2] & 63)) & 0xffffffff
    r[13] = (r[3] * r[4]) & 0xffffffff
    r[14] = (r[3] & ~r[4]) & 0xffffffff
    r_eq_r_xor_r("$r5", "$r3", "$r4")
    r_eq_r_or_r("$r6", "$r3", "$r4")
    r_eq_r_and_r("$r7", "$r3", "$r4")
    r_eq_r_plus_r("$r8", "$r3", "$r4")
    r_eq_r_minus_r("$r9", "$r3", "$r4")
    r_eq_r_shl_r("$r10", "$r3", "$r2")
    r_eq_r_shr_r("$r11", "$r3", "$r2")
    #r_eq_r_sar_r("$r12", "$r3", "$r4")
    r_eq_r_mul_r("$r13", "$r3", "$r4")
    r_eq_not_r_and_r("$r14", "$r3", "$r4")

    check()
    terminate()

def test_alu_Ir(top):
    """
    Test long immediate-to-register ALU operations
    """

    top.set_timeout(6000)

    startup()
    load_reg("$r4", 0xff00f0f0)
    i1 = 0x0f0f00ff
    r[5] = (r[4] ^ i1) & 0xffffffff
    r[6] = (r[4] | i1) & 0xffffffff
    r[7] = (r[4] & i1) & 0xffffffff
    r[8] = (r[4] + i1) & 0xffffffff
    r[9] = (i1 - r[4]) & 0xffffffff
    r[10] = shl(i1, r[4])
    r[11] = shr(i1, r[4])
    r[12] = sar(i1, r[4])
    r[13] = (r[4] * i1) & 0xffffffff

    r_eq_I_xor_r("$r5", i1, "$r4")
    r_eq_I_or_r("$r6", i1, "$r4")
    r_eq_I_and_r("$r7", i1, "$r4")
    r_eq_I_plus_r("$r8", i1, "$r4")
    r_eq_I_minus_r("$r9", i1, "$r4")
    r_eq_I_shl_r("$r10", i1, "$r4")
    r_eq_I_shr_r("$r11", i1, "$r4")
    r_eq_I_sar_r("$r12", i1, "$r4")
    r_eq_I_mul_r("$r13", i1, "$r4")

    check()
    terminate()

def test_alu_ir(top):
    """
    Test short immediate-to-register ALU operations
    """

    top.set_timeout(6000)

    startup()
    load_reg("$r4", 0xff00f0f0)
    i1 = 0x0f0f
    r[5] = (r[4] ^ i1) & 0xffffffff
    r[6] = (r[4] | i1) & 0xffffffff
    r[7] = (r[4] & i1) & 0xffffffff
    r[8] = (r[4] + i1) & 0xffffffff
    r[9] = (i1 - r[4]) & 0xffffffff
    r[10] = shl(r[4], i1)
    r[11] = shr(r[4], i1)
    r[12] = sar(r[4], i1)
    r[13] = (r[4] * i1) & 0xffffffff
    r_eq_i_xor_r("$r5", i1, "$r4")
    r_eq_i_or_r("$r6", i1, "$r4")
    r_eq_i_and_r("$r7", i1, "$r4")
    r_eq_i_plus_r("$r8", i1, "$r4")
    r_eq_i_minus_r("$r9", i1, "$r4")
    r_eq_r_shl_i("$r10", "$r4", i1)
    r_eq_r_shr_i("$r11", "$r4", i1)
    r_eq_r_sar_i("$r12", "$r4", i1)
    r_eq_i_mul_r("$r13", i1, "$r4")

    check()
    terminate()




def run_test(programmer: callable, test_name: str = None):
    if test_name is None:
        test_name = programmer.__name__

    vcd_filename = f"brew_v1_{test_name}.vcd"
    with Netlist().elaborate() as netlist:
        top_inst = top()
    programmer(top_inst)
    reloc()
    top_inst.program(get_all_segments())
    netlist.simulate(vcd_filename, add_unnamed_scopes=False)

if __name__ == "__main__":
    run_test(test_alu_ir)


