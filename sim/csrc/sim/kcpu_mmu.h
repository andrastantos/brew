#pragma once

#include <vector>
#include <stdint.h>
#include <cstddef>
#include <algorithm>
#include <boost/filesystem.hpp>
#include "utils.h"
#include "interfaces.h"
#include "kcpu_exceptions.h"
#include "kcpu_core_types.h"

// MMU registers laid out as follows:
// Top cLAddrSize - cPageBits bits: PA for page
// Bottom cWorldSize bits: WORLD for page
// bits 7654 are: CRWX
// if more bits are available, those are for SW to use (such as DA bits)
enum class AccessKinds_e {
	EXEC=2,
	WRITE=1,
	READ=0
};

inline std::ostream & operator << (std::ostream &aStream, const AccessKinds_e &aField) {
	if (!aStream.good()) return aStream;
	switch (aField) {
		case AccessKinds_e::EXEC: aStream << "EXEC"; break;
		case AccessKinds_e::WRITE: aStream << "WRITE"; break;
		case AccessKinds_e::READ: aStream << "READ"; break;
		default: SIM_ASSERT(false); break;
	}
	return aStream; 
}

enum class PageModes_e {
	NONE=0,
	READ=1,
	WRITE=2,
	READ_WRITE=3,
	EXEC=4,
	READ_EXEC=5,
	WRITE_EXEC=6,
	LINK=6,
	READ_WRITE_EXEC=7,
	ALL=7
};

enum class MmuCsrMap_e {
    SBASE = 0,
    TBASE = 1,
    TLB_LA1 = 2,
    TLB_DATA1 = 3,
    TLB_LA2 = 4,
    TLB_DATA2 = 5,
    TLB_INV = 6,
    EX_ADDR = 7,
    EX_OP = 8,
};

class MmuAccessViolation_x: KcpuException_x {
public:
    MmuAccessViolation_x(LAddr_t aPC, LAddr_t aLATarget, PAddr_t aTarget, AccessKinds_e aAccessKind, const char *aError = nullptr): KcpuException_x(aPC, aTarget, aError), mLATarget(aLATarget), mAccessKind(aAccessKind) { FormatMessage_(); }
    MmuAccessViolation_x(LAddr_t aPC, LAddr_t aLATarget, PAddr_t aTarget, AccessKinds_e aAccessKind, const std::string &aError): KcpuException_x(aPC, aTarget, aError), mLATarget(aLATarget), mAccessKind(aAccessKind) { FormatMessage_(); }
    MmuAccessViolation_x(LAddr_t aPC, LAddr_t aLATarget, AccessKinds_e aAccessKind, const char *aError = nullptr): KcpuException_x(aPC, aError), mLATarget(aLATarget), mAccessKind(aAccessKind) { FormatMessage_(); }
    MmuAccessViolation_x(LAddr_t aPC, LAddr_t aLATarget, AccessKinds_e aAccessKind, const std::string &aError): KcpuException_x(aPC, aError), mLATarget(aLATarget), mAccessKind(aAccessKind) { FormatMessage_(); }
	MmuAccessViolation_x(const MmuAccessViolation_x &aEx): KcpuException_x(aEx), mLATarget(aEx.mLATarget), mAccessKind(aEx.mAccessKind) {}
protected:
    AccessKinds_e mAccessKind;
	LAddr_t mLATarget;

    void FormatMessage_() {
        *this << " attempting " << mAccessKind;
		*this << " while translating from logical address " << HexPrinter(mLATarget);
    }
};

class KcpuCore_c;

class Mmu_c: public SlavePort_i, public Component_i {
public:
	explicit Mmu_c(KcpuCore_c &aCore, SlavePort_i &aInterconnect):
		mCore(aCore),
		mInterconnect(aInterconnect),
		mTranslatorSlave(*this)
	{}

	PAddr_t Translate(LAddr_t aAddr, AccessKinds_e aAccessKind);

	CoreSlavePort_i &GetTranslatorSlavePort() {
		return mTranslatorSlave;
	}
	SlavePort_i &GetCsrSlavePort() {
		return *this;
	}

	virtual void Reset() override {
		mTBase = 0;
		mSBase = 0;
	}

	virtual uint8_t GetUInt8(PAddr_t aAddr) override {
		throw SysAccessSizeError_x(aAddr, "MMU CSRs support only 32-bit access");
		return 0;
	}
	virtual uint16_t GetUInt16(PAddr_t aAddr) override {
		throw SysAccessSizeError_x(aAddr, "MMU CSRs support only 32-bit access");
		return 0;
	}
	virtual uint32_t GetUInt32(PAddr_t aAddr) override {
		switch (aAddr) {
			case PAddr_t(MmuCsrMap_e::SBASE): return mSBase;
			case PAddr_t(MmuCsrMap_e::TBASE): return mTBase;
			case PAddr_t(MmuCsrMap_e::TLB_LA1): return mTlbLA1;
			case PAddr_t(MmuCsrMap_e::TLB_DATA1): return 0;
			case PAddr_t(MmuCsrMap_e::TLB_LA2): return mTlbLA2;
			case PAddr_t(MmuCsrMap_e::TLB_DATA2): return 0;
			case PAddr_t(MmuCsrMap_e::TLB_INV): return 0;
			case PAddr_t(MmuCsrMap_e::EX_ADDR): return mExAddr;
			case PAddr_t(MmuCsrMap_e::EX_OP): return mExOp;
			default:
				throw SysAddrDecodeError_x(aAddr, "Invalid CSR address for MMU");
		}
	}

	virtual void SetUInt8(PAddr_t aAddr, uint8_t aData) override {
		throw SysAccessSizeError_x(aAddr, "MMU CSRs support only 32-bit access");
	}
	virtual void SetUInt16(PAddr_t aAddr, uint16_t aData) override {
		throw SysAccessSizeError_x(aAddr, "MMU CSRs support only 32-bit access");
	}
	virtual void SetUInt32(PAddr_t aAddr, uint32_t aData) override {
		const auto tlb_la1_mask = (cPageOfsMask << (cPageOfsBits + cPageBits));
		const auto tlb_la2_mask = (((cPageOfsMask << cPageOfsBits) | cPageOfsMask) << cPageBits);
		switch (aAddr) {
			case PAddr_t(MmuCsrMap_e::SBASE): mSBase = aData & ~cPageMask; break;
			case PAddr_t(MmuCsrMap_e::TBASE): mTBase = aData & ~cPageMask; break;
			case PAddr_t(MmuCsrMap_e::TLB_LA1): mTlbLA1 = aData & tlb_la1_mask; break;
			case PAddr_t(MmuCsrMap_e::TLB_DATA1): break;
			case PAddr_t(MmuCsrMap_e::TLB_LA2): mTlbLA2 = aData & tlb_la2_mask; break;
			case PAddr_t(MmuCsrMap_e::TLB_DATA2): break;
			case PAddr_t(MmuCsrMap_e::TLB_INV): TlbInvalidate_(); break;
			case PAddr_t(MmuCsrMap_e::EX_ADDR): break;
			case PAddr_t(MmuCsrMap_e::EX_OP): break;
			default:
				throw SysAddrDecodeError_x(aAddr, "Invalid CSR address for MMU");
		}
	}

protected:
	uint32_t mTBase;
	uint32_t mSBase;
	uint32_t mTlbLA1;
	uint32_t mTlbLA2;
	uint32_t mExAddr;
	uint32_t mExOp;

	SlavePort_i &mInterconnect;
	KcpuCore_c &mCore;

    class TranslatorSlave_c: public CoreSlavePort_i {
	public:
		explicit TranslatorSlave_c(Mmu_c &aMmu): mMmu(aMmu) {}

        virtual uint8_t GetUInt8(LAddr_t aAddr) override { return mMmu.mInterconnect.GetUInt8(mMmu.Translate(aAddr, AccessKinds_e::READ)); }
        virtual uint16_t GetUInt16(LAddr_t aAddr) override { return mMmu.mInterconnect.GetUInt16(mMmu.Translate(aAddr, AccessKinds_e::READ)); }
        virtual uint32_t GetUInt32(LAddr_t aAddr) override { return mMmu.mInterconnect.GetUInt32(mMmu.Translate(aAddr, AccessKinds_e::READ)); }
        virtual uint32_t FetchUInt32(LAddr_t aAddr) override { return mMmu.mInterconnect.GetUInt32(mMmu.Translate(aAddr, AccessKinds_e::EXEC)); }
        virtual void SetUInt8(LAddr_t aAddr, uint8_t aData) override { mMmu.mInterconnect.SetUInt8(mMmu.Translate(aAddr, AccessKinds_e::WRITE), aData); }
        virtual void SetUInt16(LAddr_t aAddr, uint16_t aData) override { mMmu.mInterconnect.SetUInt16(mMmu.Translate(aAddr, AccessKinds_e::WRITE), aData); }
        virtual void SetUInt32(LAddr_t aAddr, uint32_t aData) override { mMmu.mInterconnect.SetUInt32(mMmu.Translate(aAddr, AccessKinds_e::WRITE), aData); }
	protected:
		Mmu_c &mMmu;
    };

	TranslatorSlave_c mTranslatorSlave;

	static PAddr_t GetPagePhysicalAddress_(uint32_t data) {
		// Retrieve the page portion
		return PAddr_t(data & ~cPageMask) + PAddr_t(data & ((1 << cWorldSize)-1)) << cLAddrSize;
	}
	static unsigned int GetPageMode_(uint32_t data) {
		return (data >> 8) & 15;
	}

	void TlbInvalidate_() {
		SIM_ASSERT(false); // This is not implemented yet
	}
};