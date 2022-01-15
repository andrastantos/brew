#include "kcpu_mmu.h"
#include "kcpu_core.h"

PAddr_t Mmu_c::Translate(LAddr_t aAddr, AccessKinds_e aAccessKind) {
    ExecContexts_e exec_context = mCore.GetExecContext();
    // Do the table walk first
    uint32_t page_table_entry = exec_context == SCH ? mSBase : mTBase;
    unsigned int acc_mode = 1 << int(aAccessKind);
    PAddr_t within_page_ofs = aAddr & cPageMask;
    for (int level=1;level<2;level++) {
        PAddr_t page_table_addr = GetPagePhysicalAddress_(page_table_entry);
        // This is complicated, but essentially we're grabbing the top 10 or the middle 10 bits
        // of the aAddr, and DWORD align it to make it a proper offset.
        PAddr_t page_entry_ofs = ((aAddr >> (3-level) * cPageBits) & cPageOfsMask) << 2;
        page_table_entry = mInterconnect.GetUInt32(page_table_addr + page_entry_ofs);
        unsigned int page_mode = GetPageMode_(page_table_entry);
        if (page_mode == unsigned int(PageModes_e::LINK) && level==1) {
            continue;
        }
        PAddr_t translated_target = page_table_addr + within_page_ofs;
        // Test access permissions
        if (page_mode & acc_mode == 0) {
            // The handling of this exception will involve testing if the exception happened in TASK context.
            // If not, it will be turned into a reset.
            mExAddr = aAddr;
            mExOp = acc_mode << 8;
            throw MmuAccessViolation_x(mCore.GetTPC(), aAddr, translated_target, aAccessKind);
        }
        return translated_target;
    }
    SIM_ASSERT(false); // We should never get here: either the 1st level or the 2nd level lookup should have resulted in a hit
}

