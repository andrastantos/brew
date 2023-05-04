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
    from .assembler import *
except ImportError:
    from brew_types import *
    from scan import *
    from synth import *
    from fpga_system import FpgaSystem
    from brew_v1 import BrewV1Top
    from assembler import *

from silicon import *

class FpgaTop(GenericModule):
    clk               = ClkPort()
    clk2              = ClkPort()
    rst               = RstPort()

    output_pins = Output(BrewByte)
    input_pins = Input(BrewByte)

    def construct(self, program_generator: Callable) -> None:
        self.program_generator = program_generator
        return super().construct()

    def body(self):
        brew = BrewV1Top()

        ext_bus = Wire(ExternalBusIf)

        brew.clk <<= self.clk
        brew.rst <<= self.rst
        ext_bus <<= brew.dram

        brew.drq <<= 0
        brew.n_int <<= 1

        def create_mif(file_name, content):
            with open(file_name, "wt") as f:
                for byte in content:
                    f.write(f"{byte:02x}\n")

        clear_asm()
        self.program_generator()
        reloc()
        rom_content = None
        dram0_content = None
        dram1_content = None
        for segment in get_all_segments():
            section = segment.base_addr & 0xc000_0000
            if section == FpgaSystem.rom_base:
                rom_content = copy(segment.content)
                #rom_content = "fpga_top_rom_content.mif"
                #create_mif(rom_content, segment.content)
            elif section == FpgaSystem.dram_base:
                dram0_content = segment.content[0::2]
                dram1_content = segment.content[1::2]
                #dram0_content = "fpga_top_dram0_content.mif"
                #dram1_content = "fpga_top_dram1_content.mif"
                #create_mif(dram0_content, segment.content[0::2])
                #create_mif(dram1_content, segment.content[1::2])

        system = FpgaSystem(rom_content=rom_content, dram0_content=dram0_content, dram1_content=dram1_content, dram_size=128*1024, rom_size=8*1024)

        system.clk <<= self.clk
        system.clk2 <<= self.clk2
        system.rst <<= self.rst

        system.brew_if <<= ext_bus

        self.output_pins <<= system.output_pins
        system.input_pins <<= self.input_pins

def counter_demo():
    """
    Setting up initial segments, jump to DRAM and output a counter on GPIO
    """

    create_segment("code", 0)
    create_segment("code_dram", 0x8000_0000)
    set_active_segment("code_dram")
    place_symbol("_start")
    set_active_segment("code")

    pc_eq_I("_start")

    set_active_segment("code_dram")

    r_eq_i("$r5", 0)
    place_symbol("loop")
    mem8_I_eq_r(FpgaSystem.gpio_base, "$r5")
    r_eq_r_plus_i("$r5", "$r5", 1)
    pc_eq_I("loop")

def sim():
    class top(Module):
        clk               = ClkPort()
        clk2              = ClkPort()
        rst               = RstPort()

        output_pins = Output(BrewByte)
        input_pins = Input(BrewByte)

        def body(self):
            top = FpgaTop(counter_demo)
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

            for i in range(1000):
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
        return FpgaTop(counter_demo)

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


