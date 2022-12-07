#!/usr/bin/python3
from random import *
from typing import *
from xml.dom import SyntaxErr
from silicon import *
from brew_types import *
from brew_utils import *

"""
This is a large project on its own.

For now, let's assume the following:

1. Cache way size is not greater than 4k or whatever the TLB page size is.
2. There is a side-channel interface to the TLB: the cache is logically addressed but physically tagged.
   3. The TLB interface however allows for a trivial loopback which makes logical and physical the same
4. There is no versioning for now: invalidation is a loop.
5. We do support writes into the cache, but for now, we don't support any snoop protocol
6. Memory interface is explicitly not AXI4: it's a much much simpler, pipelined, but in-order interface
"""

def get_num_bits(value, err_msg):
    bits = value.bit_length()
    if (value != (1 << bits)):
        raise SyntaxErrorException(err_msg)
    return bits

class MemReqChannel(ReadyValid):
    def __init__(self, word_size, addr_size, *, support_bursts, support_invalidate):
        """
        word_size: number of bits in a word
        addr_size: number of bits in a (byte) address
        """
        super().__init__()
        get_num_bits(word_size, "word_size must be a power of two")
        get_num_bits(addr_size, "addr_size must be a power of two")
        bytes_per_word = word_size // 8
        if word_size % 8 != 0:
            raise SyntaxErrorException("word_size must be divisible by 8")
        byte_addr_bits = get_num_bits(bytes_per_word, "word_size must have a power of two bytes")

        self.add_member("addr", Unsigned(addr_size-byte_addr_bits))
        self.add_member("wr_en", logic)
        self.add_member("byte_en", Unsigned(bytes_per_word))
        self.add_member("wr_data", Unsigned(word_size))
        if support_bursts:
            self.add_member("burst", logic)
        if support_invalidate:
            self.add_member("invalidate", logic)

class MemRspChannel(ReadyValid):
    def __init__(self, word_size):
        """
        word_size: number of bits in a word
        """
        super().__init__()
        get_num_bits(word_size, "word_size must be a power of two")

        self.add_member("error", logic)
        self.add_member("rd_data", Unsigned(word_size))

class TlbReqChannel(ReadyValid):
    def __init__(self, addr_size, page_size):
        """
        addr_size: number of bits in (byte) address
        page_size: size of a physical page (in bytes)
        """
        get_num_bits(addr_size, "word_size must be a power of 2")
        page_bits = get_num_bits(page_size, "page_size must be a power of 2")
        self.add_member("logical_page", Unsigned(addr_size-page_bits))

class TlbRspChannel(ReadyValid):
    def __init__(self, addr_size, page_size):
        """
        addr_size: number of bits in (byte) address
        page_size: size of a physical page (in bytes)
        """
        get_num_bits(addr_size, "word_size must be a power of 2")
        page_bits = get_num_bits(page_size, "page_size must be a power of 2")
        self.add_member("physical_page", Unsigned(addr_size-page_bits))
        self.add_member("permissions", Unsigned(3))

class Cache(GenericModule):
    """
    Implements an N-way set-associative instruction or cache.

    The cache supports read-only (ICache) or read-write (DCache)
    with write-through or (eventually) write-back behavior.

    Cache fills are issued as one cache-line long, critical-word-first bursts.
    No snooping protocol is supported.
    An MMU interface is provided for logical-to-physical translation as well
    as for controlling cacheability. Non-cacheable accesses are issued on the
    fill interface as non-burst transactions and no cache line is allocated
    for the results.
    The following MMU page violations are handled:
    - Read from a non-readable page is ignored and an error-response is returned
    - Optionally a read from a non-executable page is ignored and an error-response is returned
    - Write to a non-writeable page is ignored and an error-response is returned

    If the fill interface returns a bus-error, the cache line is not allocated and an error-response is returned

    Parameters:
    -----------
    cache_size: size of the whole cache in bytes
    way_cnt: number of associative ways in the cache
    line_size: size of a cache line in bytes
    word_size: size of a word in bits
    mmu_page_size: size of an MMU page in bytes
    addr_size: size of a (byte) address in bits
    cache_type:
        set to "read-only" to support only read operations
        set to "write-through" to support write-through writes
        set to "write-back" to support write-back writes
        set to "inst-cache" to behave as an instruction-cache (read only and reads treated as 'execute')
    """
    clk = ClkPort()
    rst = RstPort()

    flush = Input(logic)
    flushing = Output(logic)

    cpu_req_channel = Input()
    cpu_rsp_channel = Output()

    fill_flush_req_channel = Output()
    fill_flush_rsp_channel = Input()

    tlb_req_channel = Output()
    tlb_rsp_channel = Input()

    def construct(self, cache_size, way_cnt, line_size, word_size, mmu_page_size, addr_size, cache_type, read_mmu_mask):
        # cache_size and line_size and mmu_page_size are all specified in bytes, word_size is in bits
        self.cache_size = cache_size
        self.line_size = line_size
        self.way_cnt = way_cnt
        self.way_size = cache_size / way_cnt
        self.word_size = word_size
        self.addr_size = addr_size
        self.mmu_page_size = mmu_page_size
        self.bytes_per_word = word_size / 8
        self.read_mmu_mask = read_mmu_mask
        self.cache_type = cache_type
        if self.cache_type not in ("ready-only", "write-through", "write-back", "inst-cache"):
            raise SyntaxErrorException(f"Invalid cache type ({self.cache_type}) is specified")

        # Address is composed in the following way:
        #  +-----------------+--------------+-----------+----------+
        #  |      TAG bits   |   WAY ofs    | LINE ofs  | BYTE ofs |
        #  +-----------------+--------------+-----------+----------+
        #                 <------------ MMU page size ------------->
        # We require that WAY ofs + LINE ofs be within
        # one MMU page (4kbyte normally) to ensure that
        # logical-to-physical mapping doesn't change the
        # cache location to be accessed.

        if self.way_size > mmu_page_size:
            raise SyntaxErrorException(f"Cache way_size ({self.way_size//1024}kB) must be smaller then mmu_page_size ({mmu_page_size//1024}kB)")
        get_num_bits(self.word_size, "word_size must be a power of two")
        bytes_per_word = word_size // 8
        if word_size % 8 != 0:
            raise SyntaxErrorException("word_size must be divisible by 8")
        self.byte_ofs = get_num_bits(bytes_per_word, "word_size must have a power of two bytes")
        self.line_ofs_bits = get_num_bits(line_size, "line_size must be a power of two") - self.byte_ofs
        self.way_ofs_bits = get_num_bits(self.way_size, "way_size must be a power of 2") - self.line_ofs_bits - self.byte_ofs

        # Set the net types on the interfaces
        self.cpu_req_channel.set_net_type(MemReqChannel(self.word_size, self.addr_size, support_bursts=False, support_invalidate=True))
        self.cpu_rsp_channel.set_net_type(MemRspChannel(self.word_size))

        self.fill_flush_req_channel.set_net_type(MemReqChannel(self.word_size, self.addr_size, support_bursts=True, support_invalidate=False))
        self.fill_flush_rsp_channel.set_net_type(MemRspChannel(self.word_size))

        self.tlb_req_channel.set_net_type(TlbReqChannel(self.addr_size, self.mmu_page_size))
        self.tlb_rsp_channel.set_net_type(TlbRspChannel(self.addr_size, self.mmu_page_size))


    def body(self):
        # High-level state-machine to maintain the operating mode of the cache
        self.main_fsm = FSM()

        class MainStates(Enum):
            idle           = 0
            read_ways      = 1
            waiting_on_mmu = 2
            fetching       = 3
            single_fetching = 4
            flushing       = 5

        self.main_fsm.reset_value <<= MainStates.idle
        self.main_fsm.default_state <<= MainStates.idle

        self.main_fsm.add_transition(MainStates.idle, self.flush, MainStates.flushing)
        self.main_fsm.add_transition(MainStates.idle, ~self.flush & self.cpu_req_channel.valid, MainStates.read_ways)

        self.main_fsm.add_transition(MainStates.read_ways, self.flush, MainStates.flushing)
        self.main_fsm.add_transition(MainStates.idle, ~self.flush & self.cpu_req_channel.valid & cache_hit, MainStates.read_ways)

        """
        Lookup Pipeline (LP):
            Stage 1: issue address to TAG and WAY memories; issue address to TLB
                - If operation is write or invalidate and there's a fill in progress --> stall LP
            Stage 2: wait for TAG and WAY memories (I think we want to register both input and output of the RAMs for max speed)
            Stage 3:
                - If TLB is not ready --> stall LP until it is
                - If operation is read; it is permitted, it is cacheable and is a hit --> issue response
                - If operation is read; it is permitted, it is cacheable and is a miss --> issue burst request on fill interface; stall LP
                        NOTE: It is possible that we get here while the previous fill response is still streaming in. This is a consequence of
                        the critical-word-first operation. Since the fill interface is pipelined, there's no harm in issuing the next request on
                        that interface, while the results are not fully in yet.
                - If operation is read; it is permitted, it is NOT cacheable --> issue single read on fill interface; stall LP
                        NOTE: Same as before, it's possible that a cache fill is still in progress.
                        - If hit and fill in progress --> wait until fill complete (will be before single read comes back due to transaction ordering)
                        - If hit and fill not (any more) in progress -> invalidate cache line through fill port
                - If operation is read; it is not permitted --> issue error response
                - If operation is write; it is permitted; it is cacheable and is a hit --> issue write to TAG and WAY on the fill port
                       This is the write-back behavior. Write-through would also involve issuing a single write on the fill interface
                - If operation is write; it is permitted; it is cacheable and is a miss --> issue single write on fill interface
                       This is the 'no-allocate-on-write' behavior.
                - If operation is write; it is permitted; it is not cacheable --> issue single write on the fill interface
                        - If hit -> invalidate cache line through fill port
                - If operation is write; not permitted --> issue error response
                - If operation is invalidate; it is permitted (any MMU bit allows invalidation) and is a hit --> invalidate cache line through fill port

            Control signals:
            - stall --> results in pipeline not advancing for that clock cycle. It also results in CPU request interface READY to be low.
                        There is a problem here: the TAG and WAY RAMs will return data for stage1. If stalled, we will potentially clobber
                        a previous request, that's in stage 2 (since the RAMs don't have the same stall signal). This seems to mean that we'll
                        need to have an extra set of registers to capture the RAM outputs in case of stalls. If feels like a skid-buffer (ReverseBuf)
                        instance.
            - invalidate --> when set, any operation in stage 1 and stage 2 gets its 'ignore_hit' bit set. Stage 3 hit-check is master-killed
                             by this bit. This is needed as the TAG RAM would still return a hit, potentially, which then gets captured and interpreted
                             at stage 3. In other words, in-flight requests will have to be forced to be a miss.
                             Invalidation also involves stalling the LP until invalidation is complete

            Global invalidations
            ====================

            Invalidations are issued through MMIO writes to the cache controller.

            While memory writes *leave* the CPU in-order, there's no guarantee that they *arrive* at their targets in-order. This is especially
            true for write-back DCache accesses vs. uncached I/O writes.

            Furthermore, ICache is accessed by the CPU FE and is asynchronous to the actual execution of the instruction stream.

            Because of all that, cache invalidation is a tricky business. The following sequence needs to be observed:

            1. A cache invalidation write is issued
            2. A W-before RW-after FENCE instruction issued
            3. A read to test if the invalidation is complete is issued
            4. Test for invalidation complete, if not, jump to (3)

            Steps 1 and 2 should be obvious from above. Step 3 is needed because the CPU FE might have read some instructions from the ICache prior
            the invalidation command reaching the cache controller. Since we don't want execution to continue until invalidation is complete, we
            don't want that. Due to the FENCE, (3) is guaranteed to return 'invalidating', so (4) will jump. This is true even if the invalidation
            happened after the FE fetch of (3) and (4). The FE might fetch more instructions after (4), but that would be on a mis-predicted path
            and get flushed.

            Now, here's the trick: the cache controller *must* return 'invalidating' on the first read after an invalidate command, even if that happens
            after true invalidation completed. In other words 'invalidating' must be a clear-after-read bit (with some hot-sauce to make sure it doesn't
            clear pre-maturely).

            Otherwise, it would be possible that (3), if slow enough, would return 'no invalidation' because it has already completed. In that case the FE
            might have predicted properly that (4) would not jump (if in fact that's what the prediction engine would do) and thus more instructions
            could be sitting in the FE/BE queue that would not get flushed.

            So, for the first go around (4) is guaranteed to jump. The predictor either predicted that, in which case no instructions after (4) would
            be sitting in the queue (with the assumption that predictor is not smart enough to change its mind during prefetch). Or, the predictor
            predicted (4) not taken, in which case all the speculatively fetched instructions would get flushed.

            Either way, the result is that after (4) not jumping anymore, the CPU FE will start anew, fetching whatever comes after, and from a now
            invalidated ICache.

            When should we invalidate the caches?
            - When we've loaded something from disk (such as process creation, loading of shared objects)
            - When we've generated code (such as trampolines, JIT or self-modifying code)

            NOTE: since caches are physically tagged, they don't need to be invalidated when the MMU setup changes, such as process-changes or
            entering/leaving scheduler mode.

        Fill FSM:
            On every clock cycle,
        """