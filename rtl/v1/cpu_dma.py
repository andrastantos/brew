
#!/usr/bin/python3
from random import *
from typing import *
from silicon import *
try:
    from .brew_types import *
    from .brew_utils import *
    from .scan import ScanWrapper
    from .synth import *
except ImportError:
    from brew_types import *
    from brew_utils import *
    from scan import ScanWrapper
    from synth import *

from dataclasses import dataclass
import itertools

"""
    DMA core integrated into the CPU

    For DMA accesses we generate I/O-style bus-cycles (which happen to be valid DRAM cycles as well).
    The only difference is that we assert DRAM_nRAS and nDACKx instead of nREN. This generates a memory cycle.

    DRAM_nWE is asserted to writes (from I/O to memory) and de-asserted for reads.
    THIS IS OPPOSITE OF nIOR/nIOW DECODING ON THE ISA BUS. EXTERNAL LOGIC IS NEEDED!!!

    In other words,

    CLK             \__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^
    DRAM_nRAS       ^^^^^^^^^\___________/^^^^^\___________/^^^^^^^^^^^\___________/^^^^^\_________________/^^
    DRAM_nCAS_A     ^^^^^^^^^^^^\________/^^^^^^^^\________/^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    DRAM_nCAS_B     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\________/^^^^^^^^\______________/^^
    DRAM_ADDR       ---------<==X========>-----<==X========>-----------<==X========>-----<==X==============>--
    DRAM_nWE        ^^^^^^^^^^^^^^^^^^^^^^^^^^^\___________/^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\_________________/^^
    DRAM_DATA       --------------<======>----------------------------------<======>--------------------------
    DRAM_nWE        ^^^^^^^^^^^^^^^^^^^^^^^^^^^\___________/^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\_________________/^^
    DRAM_DATA       ----------------------------------<====>------------------------------------------<====>--
    DRQ             ___/^^^^^\-----------/^^^^^\---------\_____/^^^^^^^\-----------/^^^^^\---------------\____
    nDACK           ^^^^^^^^^\___________/^^^^^\___________/^^^^^^^^^^^\___________/^^^^^\_________________/^^
    TC              ---------<==========>------<===========>-----------<===========>-----<=================>--
    nWAIT           ---------------/^^^^^\-----------/^^^^^\-----------------/^^^^^\-----------\_____/^^^^^\--
    CLK             \__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^

    DRQ is sampled every clock cycle when nDACK is high, but is ignored when nDACK is low. There is a one
    clock-cycle delay in assertion of nDACK from the detection of DRQ (in other words nDACK is registered).

    For ISA-compatible devices, the following logic can be used:

        nIOR = (nREN | ~DRAM_nWE) & (&nDACK |  DRAM_nWE)
        nIOW = (nREN |  DRAM_nWE) & (&nDACK | ~DRAM_nWE)

        nWAIT = IOCHRDY

        TC = TC

Since DRAM_nCAS_x is asserted for 1.5 cycles, it's minimum pulse length is 150ns. That's much much shorter
then the ISA bus spec of 700ns (http://www.bitsavers.org/pdf/intel/_busSpec/Intel_ISA_Spec2.01_Sep89.pdf).

That means that we have to insert many many wait-states, not just one as with I/O cycles. Since the bus_req_if
is generating a memory access, we can't use high-order bits to describe wait-states. These wait-states
should be programmable in the DMA controller per channel: we might not want to waste that many bus-cycles
for a single transfer if we know the peripheral is fast enough.

All in all that the DMA interface is somewhat special.
"""

class CpuDma(Module):
    clk = ClkPort()
    rst = RstPort()

    bus_req_if = Output(BusIfDmaRequestIf)
    bus_rsp_if = Input(BusIfDmaResponseIf)
    reg_if = Input(ApbIf)

    drq = Input()

    def body(self):
        self.ch_count = self.drq.get_num_bits()

        ### Register offsets
        ################################
        ch_addr_ofs = 0
        ch_limit_ofs = 1
        int_reg_ofs = self.ch_count*2+0
        #stat_reg_ofs = self.ch_count*2+1 # read-only, thus not directly used as a constant
        config_reg_ofs = self.ch_count*2+2
        ch_bit_shift = 4
        # status register bits (each channel is shifted by 4 bits)
        stat_active_bit = 0
        stat_req_pending_bit = 1
        # config register bits (each channel is shifted by 4 bits)
        cfg_single_bit = 0
        cfg_read_not_write_bit = 1
        cfg_is_master_bit = 2
        cfg_high_priority_bit = 3

        prev_drq = Reg(self.drq)
        next_addr = Wire(BrewAddr)
        tc = Wire(logic)

        class ChInfo():
            def __init__(self):
                # Basic registers
                self.addr = Wire(BrewAddr)
                self.limit = Wire(BrewAddr)
                # Configuration bits
                self.single = Wire(logic)
                self.read_not_write = Wire(logic)
                self.is_master = Wire(logic)
                self.high_priority = Wire(logic)
                # Status bits
                self.active = Wire(logic)
                self.int_pending = Wire(logic)
                self.req_pending = Wire(logic)


        ch_infos = tuple(ChInfo() for _ in range(self.ch_count))

        reg_write_strobe = self.reg_if.psel & self.reg_if.pwrite & self.reg_if.penable
        self.reg_if.pready <<= 1

        reg_addr = self.reg_if.paddr

        selected_dma_channel = Wire(Unsigned(self.ch_count)) # one-hot encoded channel selector, based on arbitration

        for idx, ch_info in enumerate(ch_infos):
            ch_base = idx*2
            ch_served = selected_dma_channel[idx] & self.bus_rsp_if.valid

            ch_info.addr <<= Reg(
                Select(
                    (reg_addr == ch_base + ch_addr_ofs) & reg_write_strobe,
                    Select(
                        ch_served,
                        ch_info.addr,
                        next_addr
                    ),
                    self.reg_if.pwdata
                )
            )
            ch_info.limit <<= Reg(self.reg_if.pwdata, clock_en = (reg_addr == ch_base + ch_limit_ofs) & reg_write_strobe)

            ch_info.single         <<= Reg(self.reg_if.pwdata[idx*ch_bit_shift+cfg_single_bit        ], clock_en = (reg_addr == config_reg_ofs) & reg_write_strobe)
            ch_info.read_not_write <<= Reg(self.reg_if.pwdata[idx*ch_bit_shift+cfg_read_not_write_bit], clock_en = (reg_addr == config_reg_ofs) & reg_write_strobe)
            ch_info.is_master      <<= Reg(self.reg_if.pwdata[idx*ch_bit_shift+cfg_is_master_bit     ], clock_en = (reg_addr == config_reg_ofs) & reg_write_strobe)
            ch_info.high_priority  <<= Reg(self.reg_if.pwdata[idx*ch_bit_shift+cfg_high_priority_bit ], clock_en = (reg_addr == config_reg_ofs) & reg_write_strobe)

            ch_info.active <<= Reg(
                Select(
                    (reg_addr == ch_base + ch_addr_ofs) & reg_write_strobe,
                    Select(
                        ch_served,
                        ch_info.active,
                        ~tc
                    ),
                    1
                )
            )
            ch_info.int_pending <<= Reg(
                Select(
                    tc & ch_served,
                    Select(
                        (reg_addr == int_reg_ofs) & reg_write_strobe & self.reg_if.pwdata[idx],
                        ch_info.int_pending,
                        0
                    ),
                    1
                )
            )
            ch_info.req_pending <<= Select(
                ch_info.single & ~ch_info.is_master,
                # Single mode: create edge-sensitive requests (with an extra cycle latency)
                Reg(
                    Select(self.drq[idx] & ~prev_drq[idx],
                        Select(
                            ch_served,
                            ch_info.req_pending,
                            0
                        ),
                        1
                    )
                ),
                # Burst mode: level-sensitive requests
                self.drq[idx]
            )

            # Export all ch_info members into a wire that can be dumped into a VCD file
            for name, member in vars(ch_info).items():
                if is_wire(member):
                    setattr(self, f"ch_{idx}_{name}", member)
            setattr(self, f"ch_{idx}_served", ch_served)
        del ch_served

        ch_read_not_writes = concat(*(ch_info.read_not_write for ch_info in reversed(ch_infos)))

        self.reg_if.prdata <<= Reg(Select(
            reg_addr,
            # offset 0 through n: channel address and limit registers
            *(itertools.chain.from_iterable((ch_info.addr, ch_info.limit) for ch_info in ch_infos)),
            # int_reg_ofs
            concat(*(ch_info.int_pending for ch_info in reversed(ch_infos))),
            # stat_reg_ofs
            concat(
                *(itertools.chain.from_iterable(("1'b0", "1'b0", ch_info.req_pending, ch_info.active) for ch_info in reversed(ch_infos)))
            ),
            # config_reg_ofs
            #    cfg_single_bit = 0
            #    cfg_read_not_write_bit = 1
            #    cfg_is_master_bit = 2
            #    cfg_high_priority_bit = 3
            concat(
                *(itertools.chain.from_iterable((ch_info.is_master, ch_info.high_priority, ch_info.read_not_write, ch_info.single) for ch_info in reversed(ch_infos)))
            ),
        ))

        # Arbitration logic (we have a high-priority and a low-priority arbiter, each implementing their own round-robin)
        # NOTE: We step both arbiters at the same time, independent of whether they were the ones providing the requestor.
        #       This might be slightly sub-optimal, but I doubt it would cause trouble in real life.
        priority_change = Wire(logic)
        high_pri_selected_dma_channel = Wire(Unsigned(self.ch_count))
        low_pri_selected_dma_channel = Wire(Unsigned(self.ch_count))
        high_pri_req_pending = Wire(Unsigned(self.ch_count))
        low_pri_req_pending  = Wire(Unsigned(self.ch_count))
        high_pri_selected = Wire()

        priority_change <<= self.bus_req_if.ready & self.bus_req_if.valid
        high_pri_req_pending <<= concat(*(ch_info.req_pending &  ch_info.high_priority for ch_info in ch_infos))
        low_pri_req_pending  <<= concat(*(ch_info.req_pending & ~ch_info.high_priority for ch_info in ch_infos))
        high_pri_selected_dma_channel <<= RoundRobinArbiter(high_pri_req_pending, advance=priority_change &  high_pri_selected)
        low_pri_selected_dma_channel  <<= RoundRobinArbiter(low_pri_req_pending,  advance=priority_change & ~high_pri_selected)
        high_pri_selected <<= high_pri_selected_dma_channel != 0
        selected_dma_channel <<= Select(
            high_pri_selected,
            low_pri_selected_dma_channel,
            high_pri_selected_dma_channel
        )

        def byte_en_from_lsb(addr):
            return concat(addr[0], ~addr[0])

        selected_addr = SelectOne(selected_dma_channel, *(ch_info.addr for ch_info in ch_infos))
        selected_limit = SelectOne(selected_dma_channel, *(ch_info.limit for ch_info in ch_infos))
        tc <<= ~(selected_addr < selected_limit)
        next_addr <<= (selected_addr + 1)[31:0]

        # Bus interface
        self.bus_req_if.valid           <<= selected_dma_channel != 0
        read_not_writes = Wire(Unsigned(self.ch_count))
        read_not_writes <<= ch_read_not_writes & selected_dma_channel
        self.bus_req_if.read_not_write  <<= or_gate(*read_not_writes) # reduction or; only a single DMA channel is enabled, so all other channels are masked out
        self.bus_req_if.one_hot_channel <<= selected_dma_channel
        self.bus_req_if.byte_en         <<= SelectOne(selected_dma_channel, *(byte_en_from_lsb(ch_info.addr) for ch_info in ch_infos))
        self.bus_req_if.addr            <<= selected_addr[31:1]
        self.bus_req_if.is_master       <<= SelectOne(selected_dma_channel, *(ch_info.is_master for ch_info in ch_infos))
        self.bus_req_if.terminal_count  <<= tc


def gen():
    def top():
        class CpuDmaWrapper(Module):
            clk = ClkPort()
            rst = RstPort()

            bus_req_if = Output(BusIfDmaRequestIf)
            bus_rsp_if = Input(BusIfDmaResponseIf)
            reg_if_pwrite = Input(logic)
            reg_if_psel = Input(logic)
            reg_if_penable = Input(logic)
            reg_if_pready = Output(logic)
            reg_if_paddr = Input(Unsigned(4))
            reg_if_pwdata = Input(BrewCsrData)
            reg_if_prdata = Output(BrewCsrData)
            drq = Input(Unsigned(4))

            def body(self) -> None:
                dma = CpuDma()
                self.bus_req_if <<= dma.bus_req_if
                dma.bus_rsp_if <<= self.bus_rsp_if
                self.reg_if_pready <<= dma.reg_if.pready
                self.reg_if_prdata <<= dma.reg_if.prdata
                dma.reg_if.pwrite <<= self.reg_if_pwrite
                dma.reg_if.psel <<= self.reg_if_psel
                dma.reg_if.penable <<= self.reg_if_penable
                dma.reg_if.paddr <<= self.reg_if_paddr
                dma.reg_if.pwdata <<= self.reg_if_pwdata
                dma.drq <<= self.drq

        return ScanWrapper(CpuDmaWrapper, {"clk", "rst"})

    netlist = Build.generate_rtl(top, "cpu_dma.sv")
    top_level_name = netlist.get_module_class_name(netlist.top_level)
    flow = QuartusFlow(target_dir="q_cpu_dma", top_level=top_level_name, source_files=("cpu_dma.sv",), clocks=(("clk", 10), ("top_clk", 100)), project_name="cpu_dma")
    flow.generate()
    flow.run()

if __name__ == "__main__":
    gen()
    #sim()
