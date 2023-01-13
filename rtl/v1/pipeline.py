#!/usr/bin/python3
import sys
from pathlib import Path

sys.path.append(str(Path(__file__).parent / ".." / ".." / ".." / "silicon"))
sys.path.append(str(Path(__file__).parent / ".." / ".." / ".." / "silicon" / "unit_tests"))

try:
    from .brew_types import *
    from .brew_utils import *

    from .decode import DecodeStage
except ImportError:
    from brew_types import *
    from brew_utils import *

    from decode import DecodeStage


from test_utils import *

def test_verilog():
    test.rtl_generation(DecodeStage, "decode_stage")

test_verilog()