#!/usr/bin/python3
import sys
from pathlib import Path

sys.path.append(str(Path(__file__).parent / ".." / ".." / ".." / "silicon"))
sys.path.append(str(Path(__file__).parent / ".." / ".." / ".." / "silicon" / "unit_tests"))

try:
    from .brew_types import *
    from .brew_utils import *

    from .bus_if2 import BusIf
    from .fetch import FetchStage
    from .decode import DecodeStage
    from .execute import ExecuteStage
    from .memory import MemoryStage
    from .reg_file import RegFile
except ImportError:
    from brew_types import *
    from brew_utils import *

    from bus_if2 import BusIf
    from fetch import FetchStage
    from decode import DecodeStage
    from execute import ExecuteStage
    from memory import MemoryStage
    from reg_file import RegFile

"""
TODO: Should we change mem_limit to something that's measured in logical addresses instead of physical ones?

"""

class Pipeline(Module):
    clk = ClkPort()
    rst = RstPort()

    # DRAM interface
    DRAM_nRAS         = Output(logic)
    DRAM_nCAS_a       = Output(logic)
    DRAM_nCAS_b       = Output(logic)
    DRAM_ADDR         = Output(Unsigned(12))
    DRAM_nWE          = Output(logic)
    DRAM_DATA_rd      = Input(BrewByte)
    DRAM_DATA_wr      = Output(BrewByte)
    DRAM_nNREN        = Output(logic)
    DRAM_nWAIT        = Input(logic)

    # External bus-request
    ext_req           = Input(logic)
    ext_grnt          = Output(logic)

    interrupt         = Input(logic)

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
        rcause = Wire(Unsigned(12))

        # Connecting tissue
        fetch_to_bus = Wire(BusIfRequestIf)
        bus_to_fetch = Wire(BusIfResponseIf)
        mem_to_bus = Wire(BusIfRequestIf)
        bus_to_mem = Wire(BusIfResponseIf)

        fetch_to_decode = Wire(FetchDecodeIf)
        decode_to_exec = Wire(DecodeExecIf)
        exec_to_mem = Wire(ExecMemIf)
        csr_if = Wire(CsrIf)

        do_branch = Wire(logic)

        rf_req = Wire(RegFileReadRequestIf)
        rf_rsp = Wire(RegFileReadResponseIf)
        rf_write = Wire(RegFileWriteBackIf)

        bus_if = BusIf()
        fetch_stage = FetchStage()
        decode_stage = DecodeStage()
        execute_stage = ExecuteStage()
        memory_stage = MemoryStage()
        reg_file = RegFile()


        # BUS INTERFACE
        ###########################
        ext_bus_if = Wire(ExternalBusIf)

        bus_if.fetch_request <<= fetch_to_bus
        bus_to_fetch <<= bus_if.fetch_response
        bus_if.mem_request <<= mem_to_bus
        bus_to_mem <<= bus_if.mem_response

        ext_bus_if <<= bus_if.dram

        self.DRAM_nRAS         <<= ext_bus_if.nRAS
        self.DRAM_nCAS_a       <<= ext_bus_if.nCAS_a
        self.DRAM_nCAS_b       <<= ext_bus_if.nCAS_b
        self.DRAM_ADDR         <<= ext_bus_if.addr
        self.DRAM_nWE          <<= ext_bus_if.nWE
        ext_bus_if.data_in     <<= self.DRAM_DATA_rd
        self.DRAM_DATA_wr      <<= ext_bus_if.data_out
        self.DRAM_nNREN        <<= ext_bus_if.nNREN
        ext_bus_if.nWAIT       <<= self.DRAM_nWAIT

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
        decode_to_exec               <<= decode_stage.exec

        rf_req <<= decode_stage.reg_file_req
        decode_stage.reg_file_rsp <<= rf_rsp

        decode_stage.do_branch <<= do_branch

        # EXECUTE STAGE
        #############################
        execute_stage.decode <<= decode_to_exec
        exec_to_mem          <<= execute_stage.mem

        execute_stage.mem_base  <<= mem_base
        execute_stage.mem_limit <<= mem_limit
        execute_stage.spc_in  <<= spc
        spc <<= Reg(execute_stage.spc_out)
        execute_stage.tpc_in  <<= tpc
        tpc <<= Reg(execute_stage.tpc_out)
        execute_stage.task_mode_in  <<= task_mode
        task_mode <<= Reg(execute_stage.task_mode_out)
        execute_stage.ecause_in <<= ecause
        ecause <<= Reg(execute_stage.ecause_out)
        execute_stage.rcause_in <<= rcause
        rcause <<= Reg(execute_stage.rcause_out)
        do_branch <<= execute_stage.do_branch
        execute_stage.interrupt <<= self.interrupt

        # MEMORY STAGE
        ###################
        memory_stage.exec <<= exec_to_mem

        rf_write <<= memory_stage.reg_file

        mem_to_bus <<= memory_stage.bus_req_if
        memory_stage.bus_rsp_if <<= bus_to_mem

        csr_if <<= memory_stage.csr_if

        # REG FILE
        ####################
        reg_file.read_req <<= rf_req
        rf_rsp <<= reg_file.read_rsp
        reg_file.write <<= rf_write

        # CSRs
        #####################
        #csr_addr = Select(csr_if.request, Reg(csr_if.addr, clock_en=csr_if.request), csr_if.addr)
        #csr_wr_data = Select(csr_if.request, Reg(csr_if.wr_data, clock_en=csr_if.request), csr_if.wr_data)
        #csr_read_not_write = Select(csr_if.request, Reg(csr_if.read_not_write, clock_en=csr_if.request), csr_if.read_not_write)
        csr_addr = Wire(Unsigned(4))
        csr_addr <<= csr_if.addr[3:0]
        csr_if.rd_data <<= Reg(Select(
            csr_addr,
            ## CSR0: version and capabilities
            0x00000000,
            ## CSR1: mem_base
            concat(mem_base, "10'b0"),
            ## CSR2: mem_limit
            concat(mem_limit, "10'b0"),
            ## CSR3: ecause
            ecause,
            ## CSR4: rcause
            rcause,
        ))
        mem_base  <<= Reg(csr_if.rd_data[31:10], clock_en=(csr_addr == 1) & ~csr_if.read_not_write & csr_if.request)
        mem_limit <<= Reg(csr_if.rd_data[31:10], clock_en=(csr_addr == 2) & ~csr_if.read_not_write & csr_if.request)

#from test_utils import *
#
#def test_verilog():
#    #test.rtl_generation(BusIf, "bus_if")
#    #test.rtl_generation(FetchStage, "fetch_stage")
#    #test.rtl_generation(DecodeStage, "decode_stage")
#    #test.rtl_generation(ExecuteStage, "execute_stage")
#    #test.rtl_generation(MemoryStage, "memory_stage")
#    #test.rtl_generation(RegFile, "reg_file")
#    test.rtl_generation(Pipeline, "pipeline")
#
#test_verilog()
#

def gen():
    Build.generate_rtl(Pipeline)

if __name__ == "__main__":
    gen()
    #sim()


