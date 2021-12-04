#pragma once

#include <cstddef>

typedef uint64_t PAddr_t;
typedef uint32_t LAddr_t;

const size_t cLAddrSize = 32;
const size_t cPAddrSize = 40; // 40 bits of physical address space (for now)
const size_t cWorldSize = cPAddrSize - cLAddrSize;
const PAddr_t cPAddrMax = (PAddr_t(1) << cPAddrSize) - 1;
const PAddr_t cPAddrMin = 0;

const size_t cPageBits = 12;
const LAddr_t cPageSize = LAddr_t(1) << cPageBits;
const LAddr_t cPageMask = cPageSize-1;
const size_t cPageOfsBits = cPageBits-2;
const PAddr_t cPageOfsMask = (PAddr_t(1) << cPageOfsBits) - 1;
const size_t cPageTableEntrySize = 4;
const size_t cPageTableSize1stLevel = cPageSize/cPageTableEntrySize;
const size_t cPageTableSize2ndLevel = cPageSize/cPageTableEntrySize;

