#!/usr/bin/python3
from typing import *
from silicon import *

class BrewBeInst(Struct):
    inst = Unsigned(48)
    prefix = Unsigned(16)
    has_prefix = logic
    inst_len = Number(min_val=0, max_val=3) # NOTE: theoretically, this should be 0...2, but I'm lazy

BrewInstAddr = Unsigned(31)
BrewDWordAddr = Unsigned(30)
BrewRegType = Unsigned(4)
BrewData = Unsigned(32)
BrewRegCnt = 14
BrewRegAddr = Unsigned(BrewRegCnt.bit_length())

class FeBeQueue(ReadyValid):
    inst_top    = Unsigned(16)
    inst_bottom = BrewBeInst()
    addr        = BrewInstAddr
    has_top     = logic

class DecodeIn(ReadyValid):
    inst     = BrewBeInst()
    addr     = BrewInstAddr

class FeFetch(ReadyValid):
    addr     = BrewDWordAddr
    data     = BrewData
