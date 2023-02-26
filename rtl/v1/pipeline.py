#!/usr/bin/python3
import sys
from pathlib import Path

sys.path.append(str(Path(__file__).parent / ".." / ".." / ".." / "silicon"))
sys.path.append(str(Path(__file__).parent / ".." / ".." / ".." / "silicon" / "unit_tests"))

try:
    from .brew_types import *
    from .brew_utils import *

    from .bus_if import BusIf
    from .fetch import FetchStage
    from .decode import DecodeStage
    from .execute import ExecuteStage
    from .memory import MemoryStage
    from .sign_zero_extend import ResultExtendStage
    from .reg_file import RegFile
    from .synth import *
except ImportError:
    from brew_types import *
    from brew_utils import *

    from bus_if import BusIf
    from fetch import FetchStage
    from decode import DecodeStage
    from execute import ExecuteStage
    from memory import MemoryStage
    from sign_zero_extend import ResultExtendStage
    from reg_file import RegFile
    from synth import *

"""
TODO: Should we change mem_limit to something that's measured in logical addresses instead of physical ones?

Area notes: pretty much all the area is spent on Execute and quite a bit on Decode. All the rest is more or less rounding error.
            Execute has a *lot* of fluff in it still, decode... maybe, but not as much, I don't think.
"""

class Pipeline(GenericModule):
    clk = ClkPort()
    rst = RstPort()

    # DRAM interface
    dram = Output(ExternalBusIf)

    # External bus-request
    ext_req           = Input(logic)
    ext_grnt          = Output(logic)

    interrupt         = Input(logic)

    def construct(self, csr_base: int, nram_base: int, has_multiply: bool = True, has_shift: bool = True):
        self.csr_base = csr_base
        self.nram_base = nram_base
        self.has_multiply = has_multiply
        self.has_shift = has_shift

    def body(self):
        # base and limit
        mem_base  = Wire(BrewMemBase)
        mem_limit = Wire(BrewMemBase)

        # Instruction pointers
        spc  = Wire(BrewInstAddr)
        tpc  = Wire(BrewInstAddr)
        task_mode  = Wire(logic)

        # ECause and RCause
        ecause = Wire(Unsigned(12))

        # Connecting tissue
        fetch_to_bus = Wire(BusIfRequestIf)
        bus_to_fetch = Wire(BusIfResponseIf)
        mem_to_bus = Wire(BusIfRequestIf)
        bus_to_mem = Wire(BusIfResponseIf)

        fetch_to_decode = Wire(FetchDecodeIf)
        decode_to_exec = Wire(DecodeExecIf)
        csr_if = Wire(CsrIf)

        do_branch = Wire(logic)

        exec_to_extend = Wire(ResultExtendIf)

        rf_req = Wire(RegFileReadRequestIf)
        rf_rsp = Wire(RegFileReadResponseIf)
        rf_write = Wire(RegFileWriteBackIf)


        bus_if = BusIf()
        fetch_stage = FetchStage()
        decode_stage = DecodeStage(has_multiply=self.has_multiply, has_shift=self.has_multiply)
        execute_stage = ExecuteStage(csr_base=self.csr_base, nram_base=self.nram_base, has_multiply=self.has_multiply, has_shift=self.has_multiply)
        result_extend_stage = ResultExtendStage()
        reg_file = RegFile()


        # BUS INTERFACE
        ###########################
        bus_if.fetch_request <<= fetch_to_bus
        bus_to_fetch <<= bus_if.fetch_response
        bus_if.mem_request <<= mem_to_bus
        bus_to_mem <<= bus_if.mem_response

        self.dram <<= bus_if.dram

        bus_if.ext_req         <<= self.ext_req
        self.ext_grnt          <<= bus_if.ext_grnt

        # FETCH STAGE
        ############################
        fetch_to_bus <<= fetch_stage.bus_if_request
        fetch_stage.bus_if_response <<= bus_to_fetch

        fetch_to_decode <<= fetch_stage.decode

        fetch_stage.mem_base <<= mem_base
        fetch_stage.mem_limit <<= mem_limit
        fetch_stage.spc <<= spc
        fetch_stage.tpc <<= tpc
        fetch_stage.task_mode <<= task_mode
        fetch_stage.do_branch <<= do_branch

        # DECODE STAGE
        ############################
        decode_stage.fetch           <<= fetch_to_decode
        decode_to_exec               <<= decode_stage.output_port

        rf_req <<= decode_stage.reg_file_req
        decode_stage.reg_file_rsp <<= rf_rsp

        decode_stage.do_branch <<= do_branch

        # EXECUTE STAGE
        #############################
        execute_stage.input_port <<= decode_to_exec
        exec_to_extend           <<= execute_stage.output_port

        mem_to_bus <<= execute_stage.bus_req_if
        execute_stage.bus_rsp_if <<= bus_to_mem

        csr_if <<= execute_stage.csr_if

        execute_stage.mem_base      <<= mem_base
        execute_stage.mem_limit     <<= mem_limit
        execute_stage.spc_in        <<= spc
        execute_stage.tpc_in        <<= tpc
        execute_stage.task_mode_in  <<= task_mode
        execute_stage.ecause_in     <<= ecause
        execute_stage.interrupt     <<= self.interrupt

        spc          <<= Reg(execute_stage.spc_out)
        tpc          <<= Reg(execute_stage.tpc_out)
        task_mode    <<= Reg(execute_stage.task_mode_out)
        ecause       <<= Reg(execute_stage.ecause_out)
        do_branch    <<= Reg(execute_stage.do_branch)

        # RESULT EXTEND STAGE
        ######################
        result_extend_stage.input_port <<= exec_to_extend
        rf_write <<= result_extend_stage.output_port

        # REG FILE
        ####################
        reg_file.read_req <<= rf_req
        rf_rsp <<= reg_file.read_rsp
        reg_file.write <<= rf_write

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

        paddr = BrewCsrAddr
        pwdata = BrewCsrData
        prdata = Reverse(BrewCsrData)


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
        ))
        mem_base  <<= Reg(csr_if.pwdata[31:10], clock_en=(csr_addr == 1) & csr_write_strobe)
        mem_limit <<= Reg(csr_if.pwdata[31:10], clock_en=(csr_addr == 2) & csr_write_strobe)

def gen():
    def top():
        return Pipeline(csr_base=0xc, nram_base=0xf, has_multiply=False, has_shift=False)

    netlist = Build.generate_rtl(top, "pipeline.sv")
    top_level_name = netlist.get_module_class_name(netlist.top_level)
    flow = QuartusFlow(target_dir="q_pipeline", top_level=top_level_name, source_files=("pipeline.sv",), clocks=(("clk", 100),), project_name="pipeline")
    flow.generate()
    flow.run()

if __name__ == "__main__":
    gen()
    #sim()


