#!/usr/bin/python3
from typing import *
from silicon import *

BrewInstAddr = Unsigned(31)
BrewDWordAddr = Unsigned(30)
BrewData = Unsigned(32)
BrewRegCnt = 15
BrewRegAddr = Unsigned(BrewRegCnt.bit_length())

BrewMemBase = Unsigned(22)
BrewMemShift = 10

class MemToFetchStream(ReadyValid):
    data = Unsigned(16)

class FetchToDecodeQueue(ReadyValid):
    inst = Unsigned(48)
    inst_len = Unsigned(2) # Len 3 indicates a bubble


BrewBusAddr = Unsigned(31)
BrewBusData = Unsigned(16)

inst_len_16 = 0
inst_len_32 = 1
inst_len_48 = 2
inst_len_bubble = 3
