#!/usr/bin/python3
from typing import *
from silicon import *

BrewLineAddrWidth = 29

BrewByte = Unsigned(8)
BrewAddr = Unsigned(32)
BrewInstAddr = Unsigned(31)
BrewDWordAddr = Unsigned(30)
BrewLineAddr = Unsigned(BrewLineAddrWidth)
BrewLineAddrBtm = 2 # This is in words
BrewBusAddr = Unsigned(31)
BrewBusData = Unsigned(16)
BrewData = Unsigned(32)
BrewRegCnt = 15
#BrewRegAddr = Unsigned(BrewRegCnt.bit_length())
BrewRegAddr = Number(min_val=0, max_val=BrewRegCnt-1)

BrewMemBase = Unsigned(22)
BrewMemShift = 10 # This is in bytes

BrewCsrAddr = Unsigned(10) # We have 4kB of CSR space, in 1024 32-bit registers
BrewCsrData = Unsigned(32)

inst_len_16 = 0
inst_len_32 = 1
inst_len_48 = 2
inst_len_bubble = 3

acc_len_8 = 0
acc_len_16 = 1
acc_len_32 = 2

class op(Enum):
    add      = 0
    a_sub_b  = 1
    b_sub_a  = 2
    addr     = 3
    pc_add   = 4

    b_and    = 0
    b_nand   = 1
    b_or     = 2
    b_xor    = 3

    shll     = 0
    shlr     = 1
    shar     = 2

    # Codes match FIELD_B[2:0]
    # These codes are mapped to the register compares and forcing the other input to 0.
    #op_cb_ez    = 0
    #op_cb_nz    = 1
    #op_cb_lz    = 2
    #op_cb_gez   = 3
    #op_cb_gz    = 4
    #op_cb_lez   = 5

    # Codes match FIELD_C[2:0]
    cb_eq    = 1
    cb_ne    = 2
    cb_lts   = 3
    cb_ges   = 4
    cb_lt    = 5
    cb_ge    = 6

    # Bit-selection coming in op_b
    bb_one   = 0
    # Bit-selection coming in op_a
    bb_zero  = 1

    misc_swi     = 0 # SWI index comes in op_a
    misc_stm     = 1
    misc_pc_r    = 2
    misc_tpc_r   = 3
    misc_pc_w    = 4
    misc_tpc_w   = 5
    misc_pc_w_r  = 6
    misc_tpc_w_r = 7

class exec(Enum):
    adder   = 0
    mult    = 1
    shift   = 2
    bitwise = 3
    misc    = 4
    cbranch = 5
    bbranch = 6

access_len_8 = 0
access_len_16 = 1
access_len_32 = 2

class BusIfRequestIf(ReadyValid):
    read_not_write  = logic
    byte_en         = Unsigned(2)
    addr            = Unsigned(26) # Top 4 bits are used to set wait-state. This way anything can be accessed by any wait-state we want.
    dram_not_ext    = logic
    data            = BrewBusData

class BusIfResponseIf(ReadyValid):
    data            = BrewBusData

class ExternalBusIf(Interface):
    nRAS          = logic
    nCAS_a        = logic
    nCAS_b        = logic
    addr          = Unsigned(11)
    nWE           = logic
    data_in       = Reverse(BrewByte)
    data_out      = BrewByte
    nNREN         = logic
    nWAIT         = Reverse(logic)

class BusIfPortIf(Interface):
    request         = logic
    read_not_write  = logic
    burst_len       = Unsigned(2) # 0: single-beat, 1: two-beat, 3: 4-beat
    byte_en         = Unsigned(2)
    addr            = BrewBusAddr
    data_in         = BrewBusData
    response        = Reverse(logic)
    data_out        = Reverse(BrewBusData)
    last            = Reverse(logic)
class FetchDecodeIf(ReadyValid):
    inst_0 = Unsigned(16)
    inst_1 = Unsigned(16)
    inst_2 = Unsigned(16)
    inst_len = Unsigned(2) # Len 3 is reserved
    av = logic

class DecodeExecIf(ReadyValid):
    opcode = EnumNet(op)
    exec_unit = EnumNet(exec)
    op_a = BrewData
    op_b = BrewData
    op_imm = BrewData
    mem_access_len = Unsigned(2) # 0 for 8-bit, 1 for 16-bit, 2 for 32-bit
    inst_len = Unsigned(2)
    is_load = logic
    is_store = logic
    do_bse = logic
    do_wse = logic
    do_bze = logic
    do_wze = logic
    result_reg_addr = BrewRegAddr
    result_reg_addr_valid = logic
    fetch_av = logic

class ExecMemIf(ReadyValid):
    is_load = logic
    is_store = logic
    do_bse = logic
    do_wse = logic
    do_bze = logic
    do_wze = logic
    result_reg_addr = BrewRegAddr
    result_reg_addr_valid = logic
    result = BrewData
    result_data_valid = logic
    mem_addr = BrewAddr
    mem_access_len = Unsigned(2) # 0 for 8-bit, 1 for 16-bit, 2 for 32-bit

class RegFileWriteBackIf(Interface):
    valid = logic
    data = BrewData
    data_en = logic
    addr = BrewRegAddr

class RegFileReadRequestIf(ReadyValid):
    read1_addr = BrewRegAddr
    read1_valid = logic
    read2_addr = BrewRegAddr
    read2_valid = logic
    rsv_addr = BrewRegAddr
    rsv_valid = logic

class RegFileReadResponseIf(ReadyValid):
    read1_data = BrewData
    read2_data = BrewData


class CsrIf(Interface):
    request = logic

    addr = BrewCsrAddr
    wr_data = BrewCsrData
    rd_data = Reverse(BrewCsrData)
    read_not_write = logic


# Exception types:
'''
The following exceptions are supported:

- MIP: MMU Exception on the instruction port (details are in EX_ADDR_I/EX_OP_I)
- MDP: MMU Exception on the data port (details are in EX_ADDR_D/EX_OP_D)
- SWI: SWI instruction (details are in the ECAUSE/RCAUSE registers)
- CUA: unaligned access

Since we do posted writes (or at least should supported it), we can't really do precise bus error exceptions. So, those are not precise:

- IAV: interconnect access violation
- IIA: interconnect invalid address (address decode failure)
- ITF: interconnect target fault (target signaled failure)

These - being imprecise - can't be retried, so if they occur in TASK mode, the only recourse is to terminate the app, and if they happen in SCHEDULER mode, they will reboot, after setting RCAUSE and, if possible, RADDR.

All these sources are mapped into the ECAUSE and RCAUSE registers:

+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|IAV|IIA|ITF|MIP|MDP|CUA|SW7|SW6|SW5|SW4|SW3|SW2|SW1|SW0|
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
'''

exc_swi_0 = 0
exc_swi_1 = 1
exc_swi_2 = 2
exc_swi_3 = 3
exc_swi_4 = 4
exc_swi_5 = 5
exc_swi_6 = 6
exc_swi_7 = 7
exc_cua   = 8
exc_mdp   = 9
exc_mip   = 10
exc_hwi   = 11
exc_itf   = 12
exc_iia   = 13
exc_iav   = 14

