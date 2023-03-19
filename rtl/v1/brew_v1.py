#!/usr/bin/python3

# This is the top-level BREW V1 CPU

import sys
from pathlib import Path

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

        def construct(self, latency: int = 3, hold_time: int = 2):
            self.latency = latency
            self.hold_time = hold_time
        def simulate(self, simulator: Simulator):
            content = {}
            self.data_out <<= None
            self.data_out_en <<= 0
            while True:
                yield self.nRAS
                if self.nRAS.get_sim_edge() == EdgeType.Positive:
                    # Get got deselected
                    self.data_out <<= None
                    self.data_out_en <<= 0
                elif self.nRAS.get_sim_edge() == EdgeType.Negative:
                    simulator.sim_assert(self.nCAS == 1, "nRAS should not assert while nCAS is low")
                    row_addr = copy(self.addr.sim_value)
                    while True:
                        yield self.nCAS, self.nRAS
                        if self.nRAS.get_sim_edge() == EdgeType.Positive:
                            # End of burst
                            break
                        simulator.sim_assert(self.nRAS.get_sim_edge() == EdgeType.NoEdge)
                        if self.nCAS.get_sim_edge() == EdgeType.Negative:
                            col_addr = copy(self.addr.sim_value)
                            addr = row_addr << self.addr.get_num_bits() | col_addr
                            if self.nWE == 0:
                                try:
                                    value = content[addr]
                                except KeyError:
                                    value = None
                                val_str = "--" if value is None else f"{value:02x}"
                                simulator.log(f"DRAM Reading address {addr:08x}, returning {val_str}")
                                yield self.latency
                                self.data_out <<= value
                                self.data_out_en <<= 1
                            elif self.nWE == 1:
                                value = None if self.data_in_en != 1 else self.data_in
                                content[addr] = value
                                val_str = "--" if value is None else f"{value:02x}"
                                simulator.log(f"DRAM Writing address {addr:08x} with value {val_str}")
                                self.data_out <<= None
                                self.data_out_en <<= 0
                        elif self.nCAS.get_sim_edge() == EdgeType.Positive:
                            if self.nWE == 0:
                                yield self.hold_time
                                self.data_out <<= None
                                self.data_out_en <<= 0
                        else:
                            simulator.sim_assert(f"Unexpected nCAS edge: {self.nCAS.get_sim_edge()}")

    class AddressDecode(Module):
        nNREN         = Input(logic)
        nCAS_l        = Input(logic)
        nCAS_h        = Input(logic)
        addr          = Input(Unsigned(11))
        full_addr     = Output(Unsigned(23))
        rom_en        = Output(logic)
        con_en        = Output(logic)

        def body(self):
            self.nCAS = Wire(logic)
            self.nCAS <<= self.nCAS_l & self.nCAS_h

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
                            addr = (row_addr << self.addr.get_num_bits() | col_addr) << 1 | self.nCAS_l
                            self.full_addr <<= addr
                            if (addr & self.decode_mask) == self.con_base:
                                self.con_en <<= 1
                                self.rom_en <<= 0
                            elif (addr & self.decode_mask) == self.rom_base:
                                self.con_en <<= 0
                                self.rom_en <<= 1
                            else:
                                self.con_en <<= 0
                                self.rom_end <<= 0
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
        def get_size(self):
            return len(self.content)

        def construct(self, latency: int = 3, hold_time: int = 2):
            self.latency = latency
            self.hold_time = hold_time
            self.content = []

            a = BrewAssembler()
            self.append(a.r_eq_t(0,0))
            self.append(a.r_eq_r_plus_t(1,0,1))
            self.append(a.r_eq_r_plus_t(2,0,2))
            self.append(a.r_eq_r_plus_t(3,0,3))
            self.append(a.r_eq_r_plus_t(4,0,4))
            self.append(a.r_eq_r_plus_t(5,0,5))
            self.append(a.r_eq_r_plus_r(6,5,1))
            self.append(a.r_eq_r_plus_r(7,5,2))
            self.append(a.r_eq_r_plus_r(8,5,3))
            self.append(a.r_eq_r_plus_r(9,5,4))
            self.append(a.r_eq_r_plus_r(10,5,5))
            self.append(a.r_eq_r_plus_r(11,6,5))
            self.append(a.r_eq_I(12,12))
            self.append(a.pc_eq_I(self.get_size()+0x0400_0000+6)) # Setting internal wait-states to 0, but otherwise continue execution
            self.append(a.r_eq_r_plus_r(11,6,5))
            self.append(a.r_eq_r_plus_r(12,6,6))
            self.append(a.r_eq_r_plus_r(13,7,6))
            self.append(a.r_eq_r_plus_r(14,7,7))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))
            self.append(a.r_eq_r_plus_t(1,1,1))

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
                    simulator.log(f"ROM Reading address {self.addr:08x}, returning {val_str}")
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

        def body(self):
            cpu = BrewV1Top(csr_base=0x1, nram_base=0x0, has_multiply=True, has_shift=True, page_bits=7)
            dram_l = Dram()
            dram_h = Dram()
            addr_decode = AddressDecode()
            rom = Rom()
            con = Console()

            dram_l.nRAS          <<= cpu.dram.nRAS
            dram_l.nCAS          <<= cpu.dram.nCAS_a
            dram_l.addr          <<= cpu.dram.addr
            dram_l.nWE           <<= cpu.dram.nWE
            dram_l.data_in       <<= cpu.dram.data_out
            dram_l.data_in_en    <<= cpu.dram.data_out_en

            dram_h.nRAS          <<= cpu.dram.nRAS
            dram_h.nCAS          <<= cpu.dram.nCAS_b
            dram_h.addr          <<= cpu.dram.addr
            dram_h.nWE           <<= cpu.dram.nWE
            dram_h.data_in       <<= cpu.dram.data_out
            dram_h.data_in_en    <<= cpu.dram.data_out_en

            cpu.dram.data_in     <<= SelectOne(
                dram_l.data_out_en, dram_l.data_out,
                dram_h.data_out_en, dram_h.data_out,
                rom.data_out_en, rom.data_out,
            )

            addr_decode.nNREN    <<= cpu.dram.nNREN
            addr_decode.nCAS_l   <<= cpu.dram.nCAS_a
            addr_decode.nCAS_h   <<= cpu.dram.nCAS_b
            addr_decode.addr     <<= cpu.dram.addr

            rom.enable           <<= addr_decode.rom_en
            rom.addr             <<= addr_decode.full_addr
            con.enable           <<= addr_decode.con_en
            con.addr             <<= addr_decode.full_addr

            cpu.dram.nWAIT       <<= 1
            cpu.DRQ              <<= 0
            cpu.nINT             <<= 1

        def simulate(self, simulator: Simulator) -> TSimEvent:
            def clk() -> int:
                yield 10
                self.clk <<= ~self.clk & self.clk
                yield 10
                self.clk <<= ~self.clk
                yield 0

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

    Build.simulation(top, "brew_v1.vcd", add_unnamed_scopes=False)

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


