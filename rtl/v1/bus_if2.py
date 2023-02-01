#!/usr/bin/python3
from random import *
from typing import *
from silicon import *
try:
    from .brew_types import *
    from .brew_utils import *
except ImportError:
    from brew_types import *
    from brew_utils import *

"""
Bus interface of the V1 pipeline.

This module is not part of the main pipeline, it sits on the side.

It communicates with 'fetch' and 'memory' to serve memory requests.

It does the following:
- Handles arbitration (internal and external)
- Generates appropriately timed signals for (NMOS) DRAM chips
- Sends data (in case of reads) back to requestors


                        <------- 4-beat burst -------------><---- single ----><---- single ----><---------- 4-beat burst ---------->
    CLK             \__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/
    DRAM_nRAS_A     ^^^^^^^^^\_____________________________/^^^^^\___________/^^^^^\___________/^^^^^\_____________________________/
    DRAM_nCAS_A     ^^^^^^^^^^^^\__/^^\__/^^\__/^^\__/^^^^^^^^^^^^^^\__/^^^^^^^^^^^^^^\__/^^^^^^^^^^^^^^\__/^^\__/^^\__/^^\__/^^^^^^
    DRAM_nRAS_B     ^^^^^^^^^\_____________________________/^^^^^\___________/^^^^^\___________/^^^^^\_____________________________/
    DRAM_nCAS_B     ^^^^^^^^^^^^^^^\__/^^\__/^^\__/^^\__/^^^^^^^^^^^^^^\__/^^^^^^^^^^^^^^\__/^^^^^^^^^^^^^^\__/^^\__/^^\__/^^\__/^^^
    DRAM_ADDR       ---------<==X=====X=====X=====X=====>--------<==X=====>--------<==X=====>--------<==X=====X=====X=====X=====>---
    DRAM_nWE        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    DRAM_DATA       --------------<>-<>-<>-<>-<>-<>-<>-<>-------------<>-<>--------------<>-<>-------------<>-<>-<>-<>-<>-<>-<>-<>--
    DRAM_nWE        ^^^^^^^^^\_____________________________/^^^^^\___________/^^^^^\___________/^^^^^\_____________________________/
    DRAM_DATA       ------------<==X==X==X==X==X==X==X==>-----------<==X==>-----------<==X==>-----------<==X==X==X==X==X==X==X==>---
    CLK             \__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/
    req_valid       ___/^^^^^^^^^^^^^^^^^^^^^^^\___________/^^^^^\___________/^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\____________
    req_ready       ^^^^^^^^^^^^^^^^^^^^^^^^^^^\___________/^^^^^\___________/^^^^^\___________/^^^^^^^^^^^^^^^^^^^^^^^\___________/
    req_wr          _________________________________________________________/^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\____________
    req_addr        ---<=====X=====X=====X=====>-----------<=====>-----------<=====X=================X=====X=====X=====\____________
    req_data        ---------------------------------------------------------------<=================X=====X=====X=====>------------
                       |----------------->                 |---------------->|----------------->
    rsp_valid       _____________________/^^^^^^^^^^^^^^^^^^^^^^^\___________/^^^^^\___________/^^^^^\______________________________
    rsp_ready       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    rsp_data        ---------------------<=====X=====X=====X=====>-----------<=====>-----------<=====>------------------------------

Notes:
1. Burst length is not communicated over the interface: only the de-assertion of req_valid/req_ready signals the end of a burst.
2. write data is captured with the address on every transaction.
3. rsp_ready is not monitored. It is expected to stay asserted at all times when there are outstanding reads.
4. Writes don't have any response
5. Client arbitration happens only during the idle state: i.e. we don't support clients taking over bursts from each other

Contract details:
1. If requestor lowers req_valid, it means the end of a burst: the bus interface will immediately lower req_ready and go through
   pre-charge and arbitration cycles.
2. Bus interface is allowed to de-assert req_ready independent of req_valid. This is the case for non-burst targets, such as
   ROMs or I/O.
3. Addresses must be consecutive and must not cross page-boundary within a burst. The bus_if doesn't check for this
   (maybe it should assert???) and blindly puts the address on the DRAM bus. Address incrementing is the responsibility
   of the requestor (it probably does it anyway). Bursts don't have to be from/to contiguous addresses, as long as they
   stay within one page (only lower 8 address bits change).
4. Reads and writes are not allowed to be mixed within a burst. This is - again - not checked by the bus_if.


Non-DRAM accesses:

                             <-- even read ---><--- odd write ---><- even read w. wait -->
    CLK             \__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/
    nNREN           ^^^^^^^^^\___________/^^^^^\___________/^^^^^\_________________/^^^^^^
    DRAM_nCAS_A     ^^^^^^^^^^^^\________/^^^^^^^^^^^^^^^^^^^^^^^^^^\______________/^^^^^^
    DRAM_nCAS_B     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\_______/^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    DRAM_ADDR       ---------<==X========>-----<==X========>-----<==X==============>------
    DRAM_nWE        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    DRAM_DATA       ---------------------<>----------------<>----------------------<>-----
    DRAM_nWE        ^^^^^^^^^\___________/^^^^^\___________/^^^^^\_________________/^^^^^^
    DRAM_DATA       ------------<========>-----------<=====>--------<==============>------
    nWAIT           ---------------/^^^^^\-----------/^^^^^\-----------\_____/^^^^^\------
    CLK             \__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/
    req_valid       ___/^^^^^\_____/^^^^^^^^^^^\___________/^^^^^\________________________
    req_ready       ^^^^^^^^^\___________/^^^^^\___________/^^^^^\_________________/^^^^^^
    req_wr          _______________/^^^^^^^^^^^\__________________________________________
    req_addr        ---<=====>-----<===========>-----------<=====>------------------------
    req_data        ---------------<===========>------------------------------------------
                       |----------------->                 |----------------------->
    rsp_valid       _____________________/^^^^^\___________________________________/^^^^^\
    rsp_ready       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    rsp_data        ---------------------<=====>-----------------------------------<=====>

1. Bursts are not supported: req_ready goes low after the request is accepted
2. Only 8-bit transfers are allowed; LSB address can be recovered from DRAM_nCAS_A.
4. nWAIT is sampled on the rising edge of every cycle, after internal wait-states are accounted for
5. There is at least one internal wait-state
6. For writes, the relevant byte of 'req_data' should be valid.
"""

class BusIf(Module):
    clk = ClkPort()
    rst = RstPort()

    # Interface to fetch and memory
    fetch_request  = Input(BusIfRequestIf)
    fetch_response = Output(BusIfResponseIf)
    mem_request  = Input(BusIfRequestIf)
    mem_response = Output(BusIfResponseIf)

    # DRAM interface
    dram = Output(ExternalBusIf)

    # External bus-request
    ext_req           = Input(logic)
    ext_grnt          = Output(logic)

    # Side-band interface (CSRs)
    wait_states_0     = Input(Unsigned(4))
    wait_states_1     = Input(Unsigned(4))
    wait_states_2     = Input(Unsigned(4))
    wait_states_3     = Input(Unsigned(4))

    def construct(self):
        pass

    def body(self):
        class BusIfStates(Enum):
            idle           = 0
            first          = 1
            middle         = 2
            external       = 3
            precharge      = 4
            pre_external   = 5
            non_dram_first = 6
            non_dram_wait  = 7

        self.fsm = FSM()

        self.fsm.reset_value   <<= BusIfStates.idle
        self.fsm.default_state <<= BusIfStates.idle

        state = Wire()
        next_state = Wire()
        state <<= self.fsm.state
        next_state <<= self.fsm.next_state

        arb_mem_not_fetch = Wire(logic)

        req_ready = Wire()
        req_ready <<= (state == BusIfStates.idle) | (state == BusIfStates.first) | (state == BusIfStates.middle)
        self.mem_request.ready <<= req_ready & arb_mem_not_fetch
        self.fetch_request.ready <<= req_ready & ~arb_mem_not_fetch
        # We create wires for these things in anticipation for the arbitrator between several inputs
        req_valid = Wire()
        req_valid <<= Select(arb_mem_not_fetch, self.fetch_request.valid, self.mem_request.valid)
        start = Wire()
        start <<= (state == BusIfStates.idle) & Select(arb_mem_not_fetch, self.fetch_request.valid, self.mem_request.valid)
        req_addr = Wire()
        req_addr <<= Select(arb_mem_not_fetch, self.fetch_request.addr, self.mem_request.addr)
        req_data = Wire()
        req_data <<= Select(arb_mem_not_fetch, self.fetch_request.data, self.mem_request.data)
        req_read_not_write = Wire()
        req_read_not_write <<= Select(arb_mem_not_fetch, self.fetch_request.read_not_write, self.mem_request.read_not_write)
        req_byte_en = Wire()
        req_byte_en <<= Select(arb_mem_not_fetch, self.fetch_request.byte_en, self.mem_request.byte_en)
        req_dram_not_ext = Wire()
        req_dram_not_ext <<= Select(arb_mem_not_fetch, self.fetch_request.dram_not_ext, self.mem_request.dram_not_ext)

        arb_mem_not_fetch <<= Select(
            state == BusIfStates.idle,
            Reg(self.mem_request.valid, clock_en=state == BusIfStates.idle),
            self.mem_request.valid
        )
        self.fsm.add_transition(BusIfStates.idle,         self.ext_req & ~req_valid,                                            BusIfStates.external)
        self.fsm.add_transition(BusIfStates.idle,                         req_valid & ~req_dram_not_ext,                        BusIfStates.non_dram_first)
        self.fsm.add_transition(BusIfStates.idle,                         req_valid &  req_dram_not_ext,                        BusIfStates.first)
        self.fsm.add_transition(BusIfStates.external,    ~self.ext_req,                                                         BusIfStates.idle)
        self.fsm.add_transition(BusIfStates.first,                       ~req_valid,                                            BusIfStates.precharge)
        self.fsm.add_transition(BusIfStates.first,                        req_valid,                                            BusIfStates.middle)
        self.fsm.add_transition(BusIfStates.middle,                      ~req_valid,                                            BusIfStates.precharge)
        self.fsm.add_transition(BusIfStates.precharge,   ~self.ext_req,                                                         BusIfStates.idle)
        self.fsm.add_transition(BusIfStates.precharge,    self.ext_req,                                                         BusIfStates.pre_external)
        self.fsm.add_transition(BusIfStates.pre_external, self.ext_req,                                                         BusIfStates.external)
        self.fsm.add_transition(BusIfStates.pre_external,~self.ext_req,                                                         BusIfStates.idle)
        self.fsm.add_transition(BusIfStates.non_dram_first, 1,                                                                  BusIfStates.non_dram_wait)
        self.fsm.add_transition(BusIfStates.non_dram_wait, ~self.dram.nWAIT,                                                    BusIfStates.non_dram_wait)
        self.fsm.add_transition(BusIfStates.non_dram_wait,  self.dram.nWAIT,                                                    BusIfStates.idle)

        self.wait_states = Wire(Unsigned(4))

        self.wait_states <<= Reg(
            Select(
                start,
                Select(
                    self.wait_states == 0,
                    (self.wait_states - 1)[3:0],
                    0
                ),
                (Select(
                    req_addr[30:29],
                    self.wait_states_0,
                    self.wait_states_1,
                    self.wait_states_2,
                    self.wait_states_3
                ) - 1)[3:0]
            )
        )

        input_row_addr = Wire()
        input_row_addr <<= concat(
            req_addr[21],
            req_addr[19],
            req_addr[17],
            req_addr[15:8]
        )
        row_addr = Wire()
        row_addr <<= Reg(input_row_addr, clock_en=start)

        col_addr = Wire()
        col_addr <<= Reg(concat(
            req_addr[20],
            req_addr[18],
            req_addr[16],
            req_addr[7:0]
        ), clock_en=req_valid & req_ready)
        read_not_write = Wire()
        read_not_write <<= Reg(req_read_not_write, clock_en=req_valid)
        byte_en = Wire()
        byte_en <<= Reg(req_byte_en, clock_en=req_valid)
        data_out = Wire()
        data_out <<= Reg(req_data, clock_en=req_valid)
        dram_not_ext = Wire()
        dram_not_ext <<= Reg(req_dram_not_ext, clock_en=req_valid)


        '''
        CAS generation:

            CLK             \__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/
            CAS_nWINDOW_A   ^^^^^^^^^\_______________________/^^^^^\_____/^^^^^\_____/^^^^^\_______________________/^^^^^^^^^^^^^^^^^^^^^^^^
            CAS_nWINDOW_B   ^^^^^^^^^^^^\_______________________/^^^^^\_____/^^^^^\_____/^^^^^\_______________________/^^^^^^^^^^^^^^^^^^^^^
            CAS_nWINDOW_C   ^^^^^^^^^^^^^^^\_______________________/^^^^^\_____/^^^^^\_____/^^^^^\_______________________/^^^^^^^^^^^^^^^^^^
            CAS_nEN_A       ^^^^^^^^^^^^\____________________/^^^^^^^^\__/^^^^^^^^\__/^^^^^^^^\____________________/^^^^^^^^^^^^^^^^^^^^^^^^
            DRAM_nCAS_A     ^^^^^^^^^^^^\__/^^\__/^^\__/^^\__/^^^^^^^^\__/^^^^^^^^\__/^^^^^^^^\__/^^\__/^^\__/^^\__/^^^^^^^^^^^^^^^^^^^^^^^^
            CAS_nEN_A       ^^^^^^^^^^^^^^^\____________________/^^^^^^^^\__/^^^^^^^^\__/^^^^^^^^\____________________/^^^^^^^^^^^^^^^^^^^^^
            DRAM_nCAS_B     ^^^^^^^^^^^^^^^\__/^^\__/^^\__/^^\__/^^^^^^^^\__/^^^^^^^^\__/^^^^^^^^\__/^^\__/^^\__/^^\__/^^^^^^^^^^^^^^^^^^^^^

        We need to avoid changing the enable signal on opposite edges of the clock.
        That is, CAS_nEN falls with ~CLK falling and rises with ~CLK rising.

        This way timing is not that critical, provided the LUT is just as glitch-free
        as logic gates would be. That's actually up to debate. Apparently Xilinx only
        guarantees glitch-free output for single input toggling, but in practice it
        appears to be true that the output doesn't glitch if normal logic wouldn't.

        From what I've gathered, the glitch-free nature of the output comes from
        depending on the output capacitance of the read-wire and careful timing of
        the switching of the pass-gates that make up the LUT read mux. So, fingers
        crossed, this is a safe circuit...
        '''

        DRAM_nRAS = Wire()
        DRAM_nRAS <<= Reg(
            (next_state == BusIfStates.idle) |
            (next_state == BusIfStates.external) |
            (next_state == BusIfStates.non_dram_first) |
            (next_state == BusIfStates.non_dram_wait) |
            (next_state == BusIfStates.pre_external),
            reset_value_port = 1
        ) # We re-register the state to remove all glitches
        nNREN = Wire()
        nNREN <<= Reg((next_state != BusIfStates.non_dram_first) & (next_state != BusIfStates.non_dram_wait), reset_value_port = 1) # We re-register the state to remove all glitches
        NR_CAS_nEN = Wire()
        NR_CAS_nEN <<= Reg(state != BusIfStates.non_dram_first, reset_value_port = 1) # We re-register the state to remove all glitches
        CAS_nWINDOW_A = Wire()
        CAS_nWINDOW_A <<= Reg(
            (next_state == BusIfStates.idle) |
            (next_state == BusIfStates.precharge) |
            (next_state == BusIfStates.pre_external) |
            (next_state == BusIfStates.non_dram_first) |
            (next_state == BusIfStates.non_dram_wait),
            reset_value_port = 1
        ) # We re-register the state to remove all glitches
        CAS_nWINDOW_C = Wire()
        CAS_nWINDOW_C <<= Reg(CAS_nWINDOW_A, reset_value_port = 1)
        CAS_nWINDOW_B = Wire()
        CAS_nWINDOW_B <<= NegReg(CAS_nWINDOW_A, reset_value_port = 1)
        CAS_nEN_A = Wire()
        CAS_nEN_A <<= (CAS_nWINDOW_A | CAS_nWINDOW_B)
        CAS_nEN_B = Wire()
        CAS_nEN_B <<= (CAS_nWINDOW_B | CAS_nWINDOW_C)


        self.dram.nRAS       <<= DRAM_nRAS
        self.dram.nCAS_a     <<= ~byte_en[0] | ((CAS_nEN_A |  self.clk) & NR_CAS_nEN)
        self.dram.nCAS_b     <<= ~byte_en[1] | ((CAS_nEN_B | ~self.clk) & NR_CAS_nEN)
        self.dram.addr       <<= Select(
            ((state == BusIfStates.first) | (state == BusIfStates.non_dram_first)) & self.clk,
            NegReg(col_addr),
            row_addr
        )
        self.dram.nWE        <<= read_not_write
        data_out_low = Wire()
        data_out_low <<= NegReg(data_out[7:0])
        data_out_high = Wire()
        data_out_high <<= Reg(data_out[15:8])
        self.dram.data_out   <<= Select(
            self.clk,
            data_out_low,
            data_out_high
        )

        self.dram.nNREN      <<= nNREN

        read_active = Wire()
        read_active <<= (state != BusIfStates.idle) & (state != BusIfStates.precharge) & (state != BusIfStates.pre_external) & (state != BusIfStates.external) & read_not_write
        data_in_low = Wire()
        data_in_low <<= Reg(self.dram.data_in)
        data_in_high = Wire()
        data_in_high <<= NegReg(self.dram.data_in)

        resp_data = Wire()
        resp_data <<= Reg(concat(data_in_high, data_in_low))

        self.mem_response.valid <<= Reg(Reg(read_active & arb_mem_not_fetch))
        self.fetch_response.valid <<= Reg(Reg(read_active & ~arb_mem_not_fetch))
        self.mem_response.data <<= resp_data
        self.fetch_response.data <<= resp_data

def sim():
    inst_stream = []


    class DRAM_sim(Module):
        addr_bus_len = 12
        addr_bus_mask = (1 << addr_bus_len) - 1

        bus_if = Input(ExternalBusIf)

        def simulate(self) -> TSimEvent:
            full_addr_a = 0
            full_addr_b = 0
            self.bus_if.data_in <<= None
            self.bus_if.nWAIT <<= 1
            while True:
                when = yield (self.bus_if.nRAS, self.bus_if.nCAS_a, self.bus_if.nCAS_b)
                data_assigned = False
                for (byte, ras, cas, full_addr) in (("low", self.bus_if.nRAS, self.bus_if.nCAS_a, full_addr_a), ("high", self.bus_if.nRAS, self.bus_if.nCAS_b, full_addr_b)):
                    if ras.get_sim_edge() == EdgeType.Negative:
                        #assert self.DRAM_nCAS_l.get_sim_edge() == EdgeType.NoEdge
                        #assert self.DRAM_nCAS_h.get_sim_edge() == EdgeType.NoEdge
                        #assert self.DRAM_nCAS_l == 1
                        #assert self.DRAM_nCAS_h == 1
                        # Falling edge or nRAS: capture row address
                        if full_addr is None:
                            full_addr = 0
                        full_addr = full_addr & self.addr_bus_mask | (self.bus_if.addr << self.addr_bus_len)
                    else:
                        if cas.get_sim_edge() == EdgeType.Negative:
                            #assert self.DRAM_nRAS.get_sim_edge() == EdgeType.NoEdge
                            #assert self.DRAM_nRAS == 0
                            # Falling edge of nCAS
                            full_addr = full_addr & (self.addr_bus_mask << self.addr_bus_len) | self.bus_if.addr
                            if self.bus_if.nWE == 0:
                                # Write to the address
                                data = f"{self.bus_if.data_out:x}"
                                print(f"Writing byte {byte} to address {full_addr:x} {data}")
                            else:
                                shift = 8 if byte == "high" else 0
                                data = (full_addr >> shift) & 0xff
                                if data_assigned:
                                    print(f"Driving both bytes at the same time at {when}")
                                print(f"Reading byte {byte} from address {full_addr:x} {data:x} at {when}")
                                self.bus_if.data_in <<= data
                                data_assigned = True
                if not data_assigned:
                    self.bus_if.data_in <<= None

    # These two queues will contain the expected read-back values
    read_data_l = []
    read_data_h = []
    class Generator(Module):
        clk = ClkPort()
        rst = RstPort()

        request_port = Output(BusIfRequestIf)

        def construct(self) -> None:
            self.mode = None

        def set_mode(self, mode):
            self.mode = mode

        #read_not_write  = logic
        #byte_en         = Unsigned(2)
        #addr            = BrewBusAddr
        #data            = BrewBusData
        #last            = logic

        def simulate(self) -> TSimEvent:
            self.burst_cnt = None
            self.burst_addr = None
            self.is_dram = None

            def reset():
                self.request_port.valid <<= 0
                self.request_port.read_not_write <<= None
                self.request_port.byte_en <<= None
                self.request_port.addr <<= None
                self.request_port.data <<= None
                self.request_port.dram_not_ext <<= None

            def read_or_write(addr, is_dram, burst_len, byte_en, data, do_write):
                if burst_len is not None:
                    assert addr is not None
                    assert is_dram is not None
                    self.burst_cnt = burst_len
                    self.burst_addr = addr
                    self.is_dram = is_dram
                else:
                    assert addr is None
                    assert is_dram is None
                    self.burst_addr += 1
                    self.burst_cnt -= 1
                assert self.burst_cnt >= 0

                self.request_port.valid <<= 1
                self.request_port.read_not_write <<= not do_write
                self.request_port.byte_en <<= byte_en
                self.request_port.addr <<= self.burst_addr
                self.request_port.data <<= data
                self.request_port.dram_not_ext <<= not self.is_dram

            def start_read(addr, is_dram, burst_len, byte_en):
                if burst_len > 0:
                    byte_en = 3
                read_or_write(addr, is_dram, burst_len, byte_en, None, do_write=False)

            def cont_read():
                read_or_write(None, None, None, 3, None, False)

            def start_write(addr, is_dram, burst_len, byte_en, data):
                if burst_len > 0:
                    byte_en = 3
                read_or_write(addr, is_dram, burst_len, byte_en, data, do_write=True)

            def cont_write(data):
                read_or_write(None, None, None, 3, data, False)

            def wait_clk():
                yield (self.clk, )
                while self.clk.get_sim_edge() != EdgeType.Positive:
                    yield (self.clk, )

            def wait_for_advance():
                yield from wait_clk()
                while not (self.request_port.ready & self.request_port.valid):
                    yield from wait_clk()

            def write(addr, is_dram, burst_len, byte_en, data):
                idx = 0
                start_write(addr, is_dram, burst_len, byte_en, data[idx])
                yield from wait_for_advance()
                while idx < burst_len:
                    idx += 1
                    cont_write(data[idx])
                    yield from wait_for_advance()
                reset()

            def read(addr, is_dram, burst_len, byte_en):
                idx = 0
                start_read(addr, is_dram, burst_len, byte_en)
                yield from wait_for_advance()
                while idx < burst_len:
                    idx += 1
                    cont_read()
                    yield from wait_for_advance()
                reset()

            reset()
            if self.mode == "fetch":
                yield from wait_clk()
                while self.rst == 1:
                    yield from wait_clk()
                yield from read(0xe,False,0,3)
                yield from wait_clk()
                yield from read(0x12,False,1,3)
                yield from wait_clk()
                yield from read(0x24,False,3,3)
                yield from wait_clk()
                yield from read(0x3,False,0,1)
                yield from wait_clk()
                yield from read(0x4,False,0,2)
                yield from wait_clk()
                yield from wait_clk()
                yield from wait_clk()
                yield from wait_clk()
                yield from read(0x34,True,0,2)
                yield from read(0x4,False,0,3)
            elif self.mode == "mem":
                yield from wait_clk()
                while self.rst == 1:
                    yield from wait_clk()
                yield from read(0x100e,False,0,3)


    '''
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
    '''

    class top(Module):
        clk = ClkPort()
        rst = RstPort()

        def body(self):
            seed(0)
            fetch_req = Wire(BusIfRequestIf)
            fetch_rsp = Wire(BusIfResponseIf)
            fetch_generator = Generator()
            fetch_generator.set_mode("fetch")
            fetch_req <<= fetch_generator.request_port

            mem_req = Wire(BusIfRequestIf)
            mem_rsp = Wire(BusIfResponseIf)
            mem_generator = Generator()
            mem_generator.set_mode("mem")
            mem_req <<= mem_generator.request_port

            dram_if = Wire(ExternalBusIf)
            dram_sim = DRAM_sim()

            dut = BusIf()

            dut.fetch_request <<= fetch_req
            fetch_rsp <<= dut.fetch_response
            dut.mem_request <<= mem_req
            mem_rsp <<= dut.mem_response
            dram_if <<= dut.dram
            dram_sim.bus_if <<= dram_if

            dut.ext_req <<= 0

            dut.wait_states_0 <<= 1
            dut.wait_states_1 <<= 1
            dut.wait_states_2 <<= 1
            dut.wait_states_3 <<= 1


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

            for i in range(50):
                yield from clk()
            now = yield 10
            print(f"Done at {now}")

    Build.simulation(top, "bus_if2.vcd", add_unnamed_scopes=True)


def gen():
    Build.generate_rtl(BusIf)

if __name__ == "__main__":
    #gen()
    sim()

