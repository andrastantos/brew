#pragma once

#include <vector>
#include <stdint.h>
#include <cstddef>
#include <algorithm>
#include "utils.h"
#include "interfaces.h"
#include "kcpu_exceptions.h"
#include "kcpu_core_types.h"
#include "kcpu_mmu.h"

enum class SchTransCause_e {
    FILL=0,
    BREAK=1,
    SYSCALL=2,
    SII=4,
    MISALIGNED=5,
};

class TransitionToScheduler_x: public KcpuException_x {
public:
    TransitionToScheduler_x(LAddr_t aPC, SchTransCause_e aCause, const char *aError = nullptr): ConvertedSysException_x(aPC, aError), mCause(aCause) {}
    TransitionToScheduler_x(LAddr_t aPC, SchTransCause_e aCause, const std::string &aError): ConvertedSysException_x(aPC, aError), mCause(aCause) {}
	TransitionToScheduler_x(const TransitionToScheduler_x &aEx): ConvertedSysException_x(aEx), mCause(eEx.mCause) {}
protected:
    SchTransCause_e mCause;
};


class KcpuCore_c: public Component_i {
public:
    explicit KcpuCore_c(): mMmu(*this) {
        mMemoryPort = mMmu.GetTranslatorSlavePort();
        Reset();
    }
    void BurstStep(size_t aMaxStepCnt) {
        LAddr_t &PC_ref = GetPc_();
        LAddr_t PC = PC_ref;
        SIM_ASSERT(PC & 1 == 0)
        try {
            // Fetch the 32 bits that contains the instruction (we'll see later if we need the next 32 bits for an immediate or offset)
            uint32_t lower = mMemoryPort.FetchUInt32(PC & ~3);
            uint16_t inst_code = PC & 2 != 0 ? lower >> 16 : lower;
            size_t inst_size = 2;
            uint32_t value = 0;
            if Is48BitInstruction(inst_code) {
                inst_size = 6;
                value = mMemoryPort.FetchUInt32((PC + 4) & ~3);
                // swizzle words around in case that's needed
                if (PC & 2 == 0) {
                    value = ((value << 16) & 0xffff0000) | ((lower >> 16) & 0x0000ffff);
                }
            }
            bool break_burst = Decode(inst_code, value, inst_size, break_burst, update_pc, PC_ref);
        }
        catch (KcpuException_x &Ex) {

        }
    }
    bool Decode(uint16_t aInstCode, uint32_t aValue, size_t aInstSize, uint32_t &aPC_ref);
    bool DecodeALU(uint16_t aInstCode, uint32_t aValue, size_t aInstSize, uint32_t &aPC_ref);
    bool DecodeBranch(uint16_t aInstCode, uint32_t aValue, size_t aInstSize, uint32_t &aPC_ref);
    bool DecodeLoadStore(uint16_t aInstCode, uint32_t aValue, size_t aInstSize, uint32_t &aPC_ref);
    virtual void Reset() override {
        mMmu.Reset();
        mExecContext = ExecContexts_e::SCH;
        mSPC = 0;
    }
protected:
    Mmu_c mMmu;
    CoreSlavePort_i &mMemoryPort;
    LAddr_t &GetPc_() {
        return (mExecContext == ExecContexts_e::SCH) ? mSPC : mTPC;
    }
    ExecContexts_e mExecContext;
    LAddr_t mTPC;
    LAddr_t mSPC;
    uint32_t mRegFile[13];

    static bool Is48BitInstruction(uint16_t aInstCode) {
        if ((aInstCode & 0xf800) == 0xf800) return true;
        if ((aInstCode & 0x0f00) == 0x0f00) return true;
        if ((aInstCode & 0x00f0) == 0x00f0) return true;
        if ((aInstCode & 0x000f) == 0x000f) return true;
        return false;
    }
};
