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

'''
MMU descriptor and page access modes
+---+---+---+---#---+---+---+---#---+---+---+---#---+---+---+---#---+---+---+---#---+---+---+---#---+---+---+---#---+---+---+---+
|                                   P_PA_ADDR                                   | C |   MODE    |               .               |
+---+---+---+---#---+---+---+---#---+---+---+---#---+---+---+---#---+---+---+---#---+---+---+---#---+---+---+---#---+---+---+---+

MODE   MNEMONIC   EXPLANATION
-----------------------------
0      INV        entry is not valid (or no access). Any access generates an exception
1      R          entry is readable
2       W         entry is writable
3      RW         entry is readable and writeable
4        X        entry is executable
5      R X        entry is read/executable
6      LINK       entry is link to 2nd level page table, if appears in the 1st level page table
6       WX        entry is writable and executable, if appears in the 2nd level page table
7      RWX        entry has all access rights
'''
mmu_mode_inv = 0
mmu_mode_r = 1
mmu_mode_w = 2
mmu_mode_rw = 3
mmu_mode_x = 4
mmu_mode_rx = 5
mmu_mode_lnk = 6
mmu_mode_wx = 6
mmu_mode_rwx = 7

def mmu_readable(mode):
    return mode & mmu_mode_r

def mmu_writable(mode):
    return mode & mmu_mode_w

def mmu_executable(mode):
    return mode & mmu_mode_x
