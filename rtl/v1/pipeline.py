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
    from .reg_file import RegFile
except ImportError:
    from brew_types import *
    from brew_utils import *

    from bus_if import BusIf
    from fetch import FetchStage
    from decode import DecodeStage
    from execute import ExecuteStage
    from memory import MemoryStage
    from reg_file import RegFile


class Pipeline(Module):
    clk = ClkPort()
    rst = RstPort()

    # DRAM interface
    DRAM_nRAS         = Output(logic)
    DRAM_nCAS_l       = Output(logic)
    DRAM_nCAS_h       = Output(logic)
    DRAM_ADDR         = Output(Unsigned(12))
    DRAM_nWE          = Output(logic)
    DRAM_DATA_rd      = Input(BrewBusData)
    DRAM_DATA_wr      = Output(BrewBusData)

    # External bus-request
    ext_req           = Input(logic)
    ext_grnt          = Output(logic)

    interrupt         = Input(logic)

    def body(self):
        # wait-state registers
        wait_states_0 = Wire(Unsigned(4))
        wait_states_1 = Wire(Unsigned(4))
        wait_states_2 = Wire(Unsigned(4))
        wait_states_3 = Wire(Unsigned(4))

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
        fetch_to_bus = Wire(BusIfPortIf)
        mem_to_bus = Wire(BusIfPortIf)
        fetch_to_decode = Wire(FetchDecodeIf)
        decode_to_exec = Wire(DecodeExecIf)
        exec_to_mem = Wire(ExecMemIf)
        csr_if = Wire(CsrIf)

        do_branch = Wire(logic)

        rf_request  = Wire(logic)
        rf_response = Wire(logic)

        rf_read1_addr  = Wire(BrewRegAddr)
        rf_read1_data  = Wire(BrewData)
        rf_read1_valid = Wire(logic)

        rf_read2_addr  = Wire(BrewRegAddr)
        rf_read2_data  = Wire(BrewData)
        rf_read2_valid = Wire(logic)

        rf_rsv_addr    = Wire(BrewRegAddr)
        rf_rsv_valid   = Wire(logic)

        wb_result_reg_addr = Wire(BrewRegAddr)
        wb_result          = Wire(BrewData)
        wb_request         = Wire(logic)

        bus_if = BusIf()
        fetch_stage = FetchStage()
        decode_stage = DecodeStage()
        execute_stage = ExecuteStage()
        memory_stage = MemoryStage()
        reg_file = RegFile()


        # BUS INTERFACE
        ###########################
        bus_if.fetch <<= fetch_to_bus
        bus_if.mem <<= mem_to_bus

        self.DRAM_nRAS       <<= bus_if.DRAM_nRAS
        self.DRAM_nCAS_l     <<= bus_if.DRAM_nCAS_l
        self.DRAM_nCAS_h     <<= bus_if.DRAM_nCAS_h
        self.DRAM_ADDR       <<= bus_if.DRAM_ADDR
        self.DRAM_nWE        <<= bus_if.DRAM_nWE
        bus_if.DRAM_DATA_rd  <<= self.DRAM_DATA_rd
        self.DRAM_DATA_wr    <<= bus_if.DRAM_DATA_wr
        bus_if.ext_req       <<= self.ext_req
        self.ext_grnt        <<= bus_if.ext_grnt

        bus_if.wait_states_0 <<= wait_states_0
        bus_if.wait_states_1 <<= wait_states_1
        bus_if.wait_states_2 <<= wait_states_2
        bus_if.wait_states_3 <<= wait_states_3

        # FETCH STAGE
        ############################
        fetch_to_bus <<= fetch_stage.bus_if
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

        rf_request                   <<= decode_stage.rf_request
        decode_stage.rf_response     <<= rf_response

        rf_read1_addr                <<= decode_stage.rf_read1_addr
        decode_stage.rf_read1_data   <<= rf_read1_data
        rf_read1_valid               <<= decode_stage.rf_read1_valid

        rf_read2_addr                <<= decode_stage.rf_read2_addr
        decode_stage.rf_read2_data   <<= rf_read2_data
        rf_read2_valid               <<= decode_stage.rf_read2_valid

        rf_rsv_addr                  <<= decode_stage.rf_rsv_addr
        rf_rsv_valid                 <<= decode_stage.rf_rsv_valid

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

        wb_result_reg_addr <<= memory_stage.w_result_reg_addr
        wb_result <<= memory_stage.w_result
        wb_request <<= memory_stage.w_request

        mem_to_bus <<= memory_stage.bus_if
        csr_if <<= memory_stage.csr_if

        # REG FILE
        ####################
        reg_file.request         <<= rf_request
        rf_response              <<= reg_file.response

        reg_file.read1_addr      <<= rf_read1_addr
        rf_read1_data            <<= reg_file.read1_data
        reg_file.read1_valid     <<= rf_read1_valid

        reg_file.read2_addr      <<= rf_read2_addr
        rf_read2_data            <<= reg_file.read2_data
        reg_file.read2_valid     <<= rf_read2_valid

        reg_file.rsv_addr        <<= rf_rsv_addr
        reg_file.rsv_valid       <<= rf_rsv_valid

        reg_file.write_data      <<= wb_result
        reg_file.write_addr      <<= wb_result_reg_addr
        reg_file.write_request   <<= wb_request


        # CSRs
        #####################
        #csr_addr = Select(csr_if.request, Reg(csr_if.addr, clock_en=csr_if.request), csr_if.addr)
        #csr_wr_data = Select(csr_if.request, Reg(csr_if.wr_data, clock_en=csr_if.request), csr_if.wr_data)
        #csr_read_not_write = Select(csr_if.request, Reg(csr_if.read_not_write, clock_en=csr_if.request), csr_if.read_not_write)
        csr_addr = Wire(Unsigned(4))
        csr_addr <<= csr_if.addr[3:0]
        csr_if.response <<= csr_if.request & (~csr_if.read_not_write | Reg(csr_if.request)) # We have 1 wait-state to give a response for reads, none for writes
        csr_if.rd_data <<= Select(
            Reg(csr_addr),
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
            ## CSR5: wait states
            concat(wait_states_3, wait_states_2, wait_states_1, wait_states_0),
        )
        mem_base  <<= Reg(csr_if.rd_data[31:10], clock_en=(csr_addr == 1) & ~csr_if.read_not_write & csr_if.request)
        mem_limit <<= Reg(csr_if.rd_data[31:10], clock_en=(csr_addr == 2) & ~csr_if.read_not_write & csr_if.request)
        wait_states_3 <<= Reg(csr_if.rd_data[15:12], clock_en=(csr_addr == 5) & ~csr_if.read_not_write & csr_if.request)
        wait_states_2 <<= Reg(csr_if.rd_data[11: 8], clock_en=(csr_addr == 5) & ~csr_if.read_not_write & csr_if.request)
        wait_states_1 <<= Reg(csr_if.rd_data[ 7: 4], clock_en=(csr_addr == 5) & ~csr_if.read_not_write & csr_if.request)
        wait_states_0 <<= Reg(csr_if.rd_data[ 3: 0], clock_en=(csr_addr == 5) & ~csr_if.read_not_write & csr_if.request)

from test_utils import *

def test_verilog():
    #test.rtl_generation(BusIf, "bus_if")
    #test.rtl_generation(FetchStage, "fetch_stage")
    #test.rtl_generation(DecodeStage, "decode_stage")
    #test.rtl_generation(ExecuteStage, "execute_stage")
    #test.rtl_generation(MemoryStage, "memory_stage")
    #test.rtl_generation(RegFile, "reg_file")
    test.rtl_generation(Pipeline, "pipeline")

test_verilog()
