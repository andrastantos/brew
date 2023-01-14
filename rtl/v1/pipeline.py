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


from test_utils import *

def test_verilog():
    #test.rtl_generation(BusIf, "bus_if")
    #test.rtl_generation(FetchStage, "fetch_stage")
    #test.rtl_generation(DecodeStage, "decode_stage")
    test.rtl_generation(ExecuteStage, "execute_stage")

test_verilog()