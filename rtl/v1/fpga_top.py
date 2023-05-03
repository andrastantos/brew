import sys
from pathlib import Path
import itertools

sys.path.append(str(Path(__file__).parent / ".." / ".." / ".." / "silicon"))
sys.path.append(str(Path(__file__).parent / ".." / ".." / ".." / "silicon" / "unit_tests"))

try:
    from .brew_types import *
    from .scan import *
    from .synth import *
    from .fpga_system import FpgaSystem
    from .brew_v1 import BrewV1Top
except ImportError:
    from brew_types import *
    from scan import *
    from synth import *
    from fpga_system import FpgaSystem
    from brew_v1 import BrewV1Top

from silicon import *

class FpgaTop(Module):
    clk               = ClkPort()
    clk2              = ClkPort()
    rst               = RstPort()

    output_pins = Output(BrewByte)
    input_pins = Input(BrewByte)

    def body(self):
        brew = BrewV1Top()

        ext_bus = Wire(ExternalBusIf)

        brew.clk <<= self.clk
        brew.rst <<= self.rst
        ext_bus <<= brew.dram

        brew.drq <<= 0
        brew.n_int <<= 1



        system = FpgaSystem(rom_content=None, dram0_content=None, dram1_content=None)

        system.clk <<= self.clk
        system.clk2 <<= self.clk2
        system.rst <<= self.rst

        system.brew_if <<= ext_bus

        self.output_pins <<= system.output_pins
        system.input_pins <<= self.input_pins


def sim():
    class top(Module):
        clk               = ClkPort()
        clk2              = ClkPort()
        rst               = RstPort()

        output_pins = Output(BrewByte)
        input_pins = Input(BrewByte)

        def body(self):
            top = FpgaTop()
            top.clk <<= self.clk
            top.clk2 <<= self.clk2
            top.rst <<= self.rst
            top.input_pins <<= self.input_pins
            self.output_pins <<= top.output_pins

        def simulate(self, simulator: Simulator) -> TSimEvent:
            clk_period = 100
            clk_ratio = 5

            clk_wait = clk_period // 2
            clk2_wait = clk_wait // clk_ratio

            def clk() -> int:
                self.clk <<= 1
                self.clk2 <<= 1
                for _ in range(clk_ratio):
                    yield clk2_wait
                    self.clk2 <<= ~self.clk2
                self.clk <<= ~self.clk
                for _ in range(clk_ratio):
                    yield clk2_wait
                    self.clk2 <<= ~self.clk2

            #self.program()
            simulator.log("Simulation started")

            self.rst <<= 1
            self.clk <<= 1
            yield 10
            for i in range(5):
                yield from clk()
            self.rst <<= 0

            for i in range(150):
                yield from clk()
            yield 10
            simulator.log("Done")

    top_class = top
    vcd_filename = "fpga_top.vcd"
    if vcd_filename is None:
        vcd_filename = top_class.__name__.lower()
    with Netlist().elaborate() as netlist:
        top_inst = top_class()
    netlist.simulate(vcd_filename, add_unnamed_scopes=False)

def gen():
    def top():
        return FpgaTop()

    netlist = Build.generate_rtl(top, "fpga_top.sv")
    top_level_name = netlist.get_module_class_name(netlist.top_level)
    flow = QuartusFlow(
        target_dir="q_fpga_top",
        top_level=top_level_name,
        source_files=("fpga_top.sv",),
        clocks=(("clk", 10), ("clk2", 50)),
        project_name="fpga_top",
        device="10M50DAF484C6G" # Device on the DECA board
    )
    flow.generate()
    flow.run()


if __name__ == "__main__":
    gen()
    #sim()


