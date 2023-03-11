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
    from .synth import *
except ImportError:
    from brew_types import *
    from brew_utils import *

    from pipeline import Pipeline
    from bus_if import BusIf
    from synth import *

class BrewV1Top(GenericModule):
    clk               = ClkPort()
    rst               = RstPort()

    # DRAM interface
    dram              = Output(ExternalBusIf)

    # External bus-request
    nEXTRQ            = Input(logic)
    nEXTGRNT          = Output(logic)

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

        # Things that need CSR access
        ecause    = Wire(Unsigned(12))
        eaddr     = Wire(BrewAddr)
        mem_base  = Wire(BrewMemBase)
        mem_limit = Wire(BrewMemBase)

        fetch_to_bus = Wire(BusIfRequestIf)
        bus_to_fetch = Wire(BusIfResponseIf)
        mem_to_bus = Wire(BusIfRequestIf)
        bus_to_mem = Wire(BusIfResponseIf)
        csr_if = Wire(CsrIf)

        # BUS INTERFACE
        ###########################
        bus_if.fetch_request <<= fetch_to_bus
        bus_to_fetch <<= bus_if.fetch_response
        bus_if.mem_request <<= mem_to_bus
        bus_to_mem <<= bus_if.mem_response

        self.dram <<= bus_if.dram

        bus_if.ext_req         <<= ~self.nEXTRQ
        self.nEXTGRNT          <<= ~bus_if.ext_grnt

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

        #csr_read_strobe = csr_if.psel & ~csr_if.pwrite # we don't care about qualification: perform a ready every clock...
        csr_write_strobe = csr_if.psel &  csr_if.pwrite & csr_if.penable
        csr_if.pready <<= 1

        csr_addr = Wire(Unsigned(4))
        csr_addr <<= csr_if.paddr[3:0]
        csr_if.prdata <<= Reg(Select(
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

def gen():
    def top():
        return BrewV1Top(csr_base=0x1, nram_base=0x0, has_multiply=True, has_shift=True, page_bits=7)

    back_end = SystemVerilog()
    back_end.yosys_fix = True
    netlist = Build.generate_rtl(top, "brew_v1_top.sv", back_end)
    top_level_name = netlist.get_module_class_name(netlist.top_level)
    flow = QuartusFlow(target_dir="q_brew_v1_top", top_level=top_level_name, source_files=("brew_v1_top.sv",), clocks=(("clk", 100),), project_name="brew_v1_top")
    flow.generate()
    flow.run()

if __name__ == "__main__":
    gen()
    #sim()


