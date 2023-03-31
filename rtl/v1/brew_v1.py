#!/usr/bin/python3

# This is the top-level BREW V1 CPU

import sys
from pathlib import Path
import itertools

sys.path.append(str(Path(__file__).parent / ".." / ".." / ".." / "silicon"))
sys.path.append(str(Path(__file__).parent / ".." / ".." / ".." / "silicon" / "unit_tests"))

try:
    from .brew_types import *
    from .brew_utils import *

    from .pipeline import Pipeline
    from .bus_if import BusIf
    from .cpu_dma import CpuDma
    from .synth import *
    from .assembler import *
except ImportError:
    from brew_types import *
    from brew_utils import *

    from pipeline import Pipeline
    from bus_if import BusIf
    from cpu_dma import CpuDma
    from synth import *
    from assembler import *

class BrewV1Top(GenericModule):
    clk               = ClkPort()
    rst               = RstPort()

    # DRAM interface
    dram              = Output(ExternalBusIf)

    # External dma-request
    DRQ               = Input(Unsigned(4))

    nINT              = Input(logic)

    def construct(self, csr_base: int, nram_base: int, has_multiply: bool = True, has_shift: bool = True, page_bits: int = 7):
        self.csr_base = csr_base
        self.nram_base = nram_base
        self.has_multiply = has_multiply
        self.has_shift = has_shift
        self.page_bits = page_bits

        self.csr_top_level_ofs = 0
        self.csr_cpu_ver_reg    = (self.csr_base << 30) + self.csr_top_level_ofs + 0*4
        self.csr_mem_base_reg   = (self.csr_base << 30) + self.csr_top_level_ofs + 1*4
        self.csr_mem_limit_reg  = (self.csr_base << 30) + self.csr_top_level_ofs + 2*4
        self.csr_ecause_reg     = (self.csr_base << 30) + self.csr_top_level_ofs + 3*4
        self.csr_eaddr_reg      = (self.csr_base << 30) + self.csr_top_level_ofs + 4*4
        self.csr_event_sel_reg  = (self.csr_base << 30) + self.csr_top_level_ofs + 5*4
        self.csr_event_cnt0_reg = (self.csr_base << 30) + self.csr_top_level_ofs + 6*4
        self.csr_event_cnt1_reg = (self.csr_base << 30) + self.csr_top_level_ofs + 7*4
        self.csr_event_cnt2_reg = (self.csr_base << 30) + self.csr_top_level_ofs + 8*4
        self.csr_event_cnt3_reg = (self.csr_base << 30) + self.csr_top_level_ofs + 9*4

    def body(self):
        bus_if = BusIf(nram_base=self.nram_base)
        pipeline = Pipeline(csr_base=self.csr_base, has_multiply=self.has_multiply, has_shift=self.has_shift, page_bits=self.page_bits)
        dma = CpuDma()

        # Things that need CSR access
        ecause    = Wire(Unsigned(12))
        eaddr     = Wire(BrewAddr)
        mem_base  = Wire(BrewMemBase)
        mem_limit = Wire(BrewMemBase)

        fetch_to_bus = Wire(BusIfRequestIf)
        bus_to_fetch = Wire(BusIfResponseIf)
        mem_to_bus = Wire(BusIfRequestIf)
        bus_to_mem = Wire(BusIfResponseIf)
        dma_to_bus = Wire(BusIfDmaRequestIf)
        bus_to_dma = Wire(BusIfDmaResponseIf)
        csr_if = Wire(ApbIf)
        bus_if_reg_if = Wire(ApbIf)
        dma_reg_if = Wire(ApbIf)

        # BUS INTERFACE
        ###########################
        bus_if.fetch_request <<= fetch_to_bus
        bus_to_fetch <<= bus_if.fetch_response
        bus_if.mem_request <<= mem_to_bus
        bus_to_mem <<= bus_if.mem_response
        bus_if.dma_request <<= dma_to_bus
        bus_to_dma <<= bus_if.dma_response

        self.dram <<= bus_if.dram

        bus_if.reg_if <<= bus_if_reg_if

        # DMA
        ###########################
        dma_to_bus <<= dma.bus_req_if
        dma.bus_rsp_if <<= bus_to_dma

        dma.drq <<= self.DRQ

        dma.reg_if <<= dma_reg_if

        # PIPELINE
        ############################
        fetch_to_bus <<= pipeline.fetch_to_bus
        pipeline.bus_to_fetch <<= bus_to_fetch
        mem_to_bus <<= pipeline.mem_to_bus
        pipeline.bus_to_mem <<= bus_to_mem
        csr_if <<= pipeline.csr_if

        ecause <<= pipeline.ecause
        eaddr  <<= pipeline.eaddr
        pipeline.mem_base <<= mem_base
        pipeline.mem_limit <<= mem_limit

        pipeline.interrupt <<= ~self.nINT

        event_fetch_wait_on_bus = pipeline.fetch_wait_on_bus
        event_decode_wait_on_rf = pipeline.decode_wait_on_rf
        event_mem_wait_on_bus   = pipeline.mem_wait_on_bus
        event_branch_taken      = pipeline.branch_taken
        event_branch            = pipeline.branch
        event_load              = pipeline.load
        event_store             = pipeline.store
        event_execute           = pipeline.execute

        # CSR address decode
        #############################
        csr_top_level_psel = csr_if.psel & (csr_if.paddr[5:4] == 0)
        csr_bus_if_psel    = csr_if.psel & (csr_if.paddr[5:4] == 1)
        csr_dma_psel       = csr_if.psel & (csr_if.paddr[5:4] == 2)

        top_level_prdata = Wire(Unsigned(32))
        top_level_pready = Wire(logic)

        # CSR bus routing
        #############################
        dma_reg_if.pwrite  <<= csr_if.pwrite
        dma_reg_if.psel    <<= csr_dma_psel
        dma_reg_if.penable <<= csr_if.penable
        dma_reg_if.paddr   <<= csr_if.paddr[3:0]
        dma_reg_if.pwdata  <<= csr_if.pwdata

        bus_if_reg_if.pwrite  <<= csr_if.pwrite
        bus_if_reg_if.psel    <<= csr_dma_psel
        bus_if_reg_if.penable <<= csr_if.penable
        bus_if_reg_if.paddr   <<= csr_if.paddr[3:0]
        bus_if_reg_if.pwdata  <<= csr_if.pwdata

        csr_if.prdata <<= SelectOne(
            csr_dma_psel, dma_reg_if.prdata,
            csr_bus_if_psel, bus_if_reg_if.prdata,
            default_port = top_level_prdata
        )
        csr_if.pready <<= SelectOne(
            csr_dma_psel, dma_reg_if.pready,
            csr_bus_if_psel, bus_if_reg_if.pready,
            default_port = top_level_pready
        )

        # EVENT COUNTERS
        #############################

        event_cnts = []
        event_selects = []
        for i in range(4):
            event_cnt = Wire(Unsigned(20))
            event_select = Wire(Unsigned(4))
            event = Select(event_select, 1, event_fetch_wait_on_bus, event_decode_wait_on_rf, event_mem_wait_on_bus, event_branch_taken, event_branch, event_load, event_store, event_execute)
            event_cnt <<= Reg((event_cnt + 1)[event_cnt.get_num_bits()-1:0], clock_en=event)
            setattr(self, f"event_cnt_{i}", event_cnt)
            setattr(self, f"event_select_{i}", event_select)
            event_cnts.append(event_cnt)
            event_selects.append(event_select)
            del event
            del event_cnt
            del event_select

        # CSRs
        #####################

        '''
        APB signalling

                        <-- read -->      <-- write ->
            CLK     \__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/
            psel    ___/^^^^^^^^^^^\_____/^^^^^^^^^^^\______
            penable _________/^^^^^\___________/^^^^^\______
            pready  ---------/^^^^^\-----------/^^^^^\------
            pwrite  ---/^^^^^^^^^^^\-----\___________/------
            paddr   ---<===========>-----<===========>------
            prdata  ---------<=====>------------------------
            pwdata  ---------------------<===========>------
            csr_rs  ___/^^^^^^^^^^^\________________________
            csr_wr  ___________________________/^^^^^\______
        '''

        #csr_read_strobe = csr_top_level_psel & ~csr_if.pwrite # we don't care about qualification: perform a ready every clock...
        csr_write_strobe = csr_top_level_psel &  csr_if.pwrite & csr_if.penable
        top_level_pready <<= 1

        csr_addr = Wire(Unsigned(4))
        csr_addr <<= csr_if.paddr[3:0]
        top_level_prdata <<= Reg(Select(
            csr_addr,
            ## CSR0: version and capabilities
            0x00000000,
            ## CSR1: mem_base
            concat(mem_base, "10'b0"),
            ## CSR2: mem_limit
            concat(mem_limit, "10'b0"),
            ## CSR3: ecause
            ecause,
            ## CSR4: eaddr
            eaddr,
            ## CSR5: event select
            concat(*reversed(event_selects)),
            ## CSR6...9: event counters
            *event_cnts
        ))
        mem_base  <<= Reg(csr_if.pwdata[31:10], clock_en=(csr_addr == 1) & csr_write_strobe)
        mem_limit <<= Reg(csr_if.pwdata[31:10], clock_en=(csr_addr == 2) & csr_write_strobe)
        for idx, event_select in enumerate(event_selects):
            event_select <<= Reg(csr_if.pwdata[idx*4+3:idx*4], clock_en=(csr_addr == 4) & csr_write_strobe)

def sim():
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
                        addr = self.bus_req_if.addr
                        access_size = 8
                    if self.bus_req_if.byte_en == 2:
                        addr = self.bus_req_if.addr + 1
                        access_size = 8
                    if self.bus_req_if.byte_en == 3:
                        addr = self.bus_req_if.addr
                        access_size = 16
                    if self.bus_req_if.read_not_write == 1:
                        mode = "reading"
                        value = ""
                    elif self.bus_req_if.read_not_write == 0:
                        mode = "writing"
                        value = f" with value {self.bus_req_if.data:04x} ({self.bus_req_if.data})"
                    else:
                        mode = "NONE"
                    simulator.log(f"                          ---------- {mode} memory addr {addr:08x}{value}")

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
                                #simulator.log(f"DRAM {self.name} Reading address {addr:08x}, returning {val_str}")
                                yield self.latency
                                self.data_out <<= value
                                self.data_out_en <<= 1
                            elif self.nWE == 0:
                                value = None if self.data_in_en != 1 else self.data_in
                                self.content[addr] = value
                                val_str = "--" if value is None else f"{value:02x}"
                                #simulator.log(f"DRAM {self.name} Writing address {addr:08x} with value {val_str}")
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
            self.rom_base = 0x0000_0000
            self.con_base = 0x0001_0000
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

        def simulate(self, simulator: Simulator) -> TSimEvent:
            while True:
                yield self.enable
                if self.enable.get_sim_edge() == EdgeType.Positive:
                    if self.nWE == 1:
                        value = None if self.data_in_en != 1 else self.data_in
                        val_str = "--" if value is None else f"{value:02x}"
                        simulator.log(f"CONSOLE got value {val_str}")
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

            self.cpu.dram.nWAIT       <<= 1
            self.cpu.DRQ              <<= 0
            self.cpu.nINT             <<= 1

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

            for i in range(1500):
                yield from clk()
            yield 10
            simulator.log("Done")

        def set_mem(self, addr: int, data: ByteString):
            section = addr & 0xc000_0000
            if section == self.nram_base:
                self.rom.set_mem(addr & 0x03ff_ffff, data)
            elif section == self.dram_base:
                for idx, byte in enumerate(data):
                    lin_addr = (addr & 0x03ff_ffff) + idx
                    # re-arrange address bits to the way DRAM sees it.
                    dram_addr = (
                        # col addr
                        (((lin_addr >>  0) & 0xff) <<  0) |
                        (((lin_addr >> 16) & 0x01) <<  8) |
                        (((lin_addr >> 18) & 0x01) <<  9) |
                        (((lin_addr >> 20) & 0x01) << 10) |
                        # row addr
                        (((lin_addr >>  8) & 0xff) << 11) |
                        (((lin_addr >> 17) & 0x01) << 19) |
                        (((lin_addr >> 19) & 0x01) << 20) |
                        (((lin_addr >> 21) & 0x01) << 21)
                    )
                    if (dram_addr & 1) == 0:
                        self.dram_l.set_mem(dram_addr >> 1, byte.to_bytes())
                    else:
                        self.dram_h.set_mem(dram_addr >> 1, byte.to_bytes())
            else:
                raise SimulationException(f"Address {addr:08x} doesn't fall into any mapped memory region")

    def program():
        nonlocal top_inst

        pc = 0
        a = BrewAssembler()

        def i2b(inst: Sequence[int]) -> ByteString:
            return bytes(itertools.chain.from_iterable((w & 0xff, (w>>8)&0xff) for w in inst))

        def prog(inst):
            nonlocal pc
            top_inst.set_mem(pc, i2b(inst))
            pc += len(inst)*2

        def test_1():
            """
            This test loads a few registers, then jumps to a zero-wait-state ROM address, but continues executing from the same physical location.
            It then loads a few more registers and enters an endless loop.
            """
            nonlocal pc
            pc = 0
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
            prog(a.pc_eq_I(pc+0x0400_0000+6)) # Setting internal wait-states to 0, but otherwise continue execution
            pc = pc | 0x0400_0000
            prog(a.r_eq_r_plus_r(11,6,5))
            prog(a.r_eq_r_plus_r(12,6,6))
            prog(a.r_eq_r_plus_r(13,7,6))
            prog(a.r_eq_r_plus_r(14,7,7))
            prog(a.r_eq_r_plus_t(1,1,1))
            prog(a.pc_eq_I(pc-2)) # Endless loop.
            prog(a.r_eq_r_plus_t(14,14,14))


        def test_2():
            """
            This test jumps to DRAM right out of reset, then loads a few registers, and enters an endless loop.
            """
            nonlocal pc
            pc = 0
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
            prog(a.r_eq_r_plus_t(1,1,1))
            prog(a.pc_eq_I(pc-2)) # Endless loop.
            prog(a.r_eq_r_plus_t(14,14,14))


        def test_3():
            """
            This test jumps to DRAM right out of reset, then loads a few registers, and a few loads and stores before entering an endless loop.
            """
            nonlocal pc
            pc = 0
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
            prog(a.r_eq_mem32_I(0,0x8000_0000))
            prog(a.mem32_I_eq_r(0x8000_1000,14))
            prog(a.r_eq_r_plus_t(1,1,1))
            prog(a.pc_eq_I(pc-2)) # Endless loop.
            prog(a.r_eq_r_plus_t(14,14,14))

        test_3()

        def test_4():
            """
            Test jumping to task mode, then back to scheduler mode due to a fetch AV
            """
            nonlocal pc
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
            prog(a.r_eq_mem32_I(0,0x8000_0000))
            prog(a.mem32_I_eq_r(0x8000_1000,14))
            loop = pc
            prog(a.tpc_eq_I(task_start))
            prog(a.r_eq_r_plus_t(1,1,1))
            prog(a.stm())
            prog(a.pc_eq_I(loop)) # Endless loop.
            prog(a.r_eq_r_plus_t(14,14,14))
            pc = task_start
            loop = pc
            prog(a.r_eq_r_plus_t(2,2,1))
            prog(a.pc_eq_I(loop))


        def test_5():
            """
            Test jumping back and forth between task and system mode, no exceptions thrown
            """
            nonlocal pc
            nonlocal top_inst
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
            prog(a.r_eq_mem32_I(0,0x8000_0000))
            prog(a.mem32_I_eq_r(0x8000_0800,14))
            prog(a.r_eq_I(0,0xffffffff))
            prog(a.mem32_I_eq_r(top_inst.cpu.csr_mem_limit_reg,0))
            prog(a.r_eq_t(0,0))
            prog(a.mem32_I_eq_r(top_inst.cpu.csr_mem_base_reg,0))

            # Scheduler mode loop: incrementing $r1
            prog(a.tpc_eq_I(task_start))
            loop = pc
            prog(a.r_eq_r_plus_t(1,1,1))
            print(f"******* STM $pc: {pc:08x} ({pc//2:08x})")
            prog(a.stm())

            prog(a.r_eq_tpc(10)) # Adjust $tpc to be over the SWI instruction
            prog(a.r_eq_r_plus_t(10,10,2))
            prog(a.tpc_eq_r(10))

            prog(a.pc_eq_I(loop)) # Endless loop.
            prog(a.r_eq_r_plus_t(14,14,14))

            # Task mode loop: incrementing $r2
            pc = task_start
            loop = pc
            prog(a.r_eq_r_plus_t(2,2,1))
            prog(a.swi(3))
            prog(a.pc_eq_I(loop))
            prog(a.r_eq_r_plus_t(13,13,13))

        test_5()
    top_class = top
    vcd_filename = "brew_v1.vcd"
    if vcd_filename is None:
        vcd_filename = top_class.__name__.lower()
    with Netlist().elaborate() as netlist:
        top_inst = top_class()
    program()
    netlist.simulate(vcd_filename, add_unnamed_scopes=False)

def gen():
    def top():
        return BrewV1Top(csr_base=0x1, nram_base=0x0, has_multiply=True, has_shift=True, page_bits=7)

    back_end = SystemVerilog()
    back_end.yosys_fix = True
    netlist = Build.generate_rtl(top, "brew_v1_top.sv", back_end)
    top_level_name = netlist.get_module_class_name(netlist.top_level)
    flow = QuartusFlow(target_dir="q_brew_v1_top", top_level=top_level_name, source_files=("brew_v1.sv", "brew_v1_top.sv"), clocks=(("clk", 100),), project_name="BREW_V1")
    flow.generate()
    flow.run()

if __name__ == "__main__":
    #gen()
    sim()


