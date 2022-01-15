#pragma once

#include <vector>
#include <stdint.h>
#include "utils.h"
#include "interfaces.h"
#include <exceptions.h>

// These exception classes represent the actual HW exceptions that the system can generate

class ConvertedSysException_x;

// An exception class for system level errors (such as bus-errors). These exceptions don't know about the PC in the CPU, only the target of the operation
// They can later be converted to a CPU exception (once the PC is known) using the 'Convert' method.
class KsysException_x: public Generic_x {
public:
    KsysException_x(PAddr_t aTarget, const char *aError = nullptr): Generic_x(aError), mTaget(aTarget) { FormatMessage_(); }
    KsysException_x(PAddr_t aTarget, const std::string &aError): Generic_x(aError), mTaget(aTarget) { FormatMessage_(); }
	KsysException_x(const KsysException_x &aEx) : Generic_x(aEx), mTarget(aEx.mTarget) {}

    ConvertedSysException_x Convert(LAddr_t aPC) const;
protected:
    PAddr_t mTarget;
    std::string mBaseMessage;

    void FormatMessage()_ {
        mBaseMessage = what(); // Save off the original base message
        if (!mSpacePrinted) {
            mErrorStrm << " ";
            mSpacePrinted = true;
        }
        mErrorStrm << "while accessing " << HexPrinter(mTarget);
        }
    }
};

class SysAddrDecodeError_x: public KsysException_x {
public:
    SysAddrDecodeError_x(PAddr_t aTarget, const char *aError = nullptr): KsysException_x(aTarget, aError) {}
    SysAddrDecodeError_x(PAddr_t aTarget, const std::string &aError): KsysException_x(aTarget, aError) {}
};

class SysAccessSizeError_x: public KsysException_x {
public:
    SysAccessSizeError_x(PAddr_t aTarget, const char *aError = nullptr): KsysException_x(aTarget, aError) {}
    SysAccessSizeError_x(PAddr_t aTarget, const std::string &aError): KsysException_x(aTarget, aError) {}
};

// CPU exceptions: these are either generated internally to the CPU (such as MMU protection violations) or converted from system exceptions.
class KcpuException_x: public Generic_x {
public:
    KcpuException_x(LAddr_t aPC, const char *aError = nullptr): Generic_x(aError), mPC(aPC), mTargetValid(false) { FormatMessage_(); }
    KcpuException_x(LAddr_t aPC, const std::string &aError): Generic_x(aError), mPC(aPC), mTargetValid(false) { FormatMessage_(); }
    KcpuException_x(LAddr_t aPC, PAddr_t aTarget, const char *aError = nullptr): Generic_x(aError), mPC(aPC), mTaget(aTarget), mTargetValid(true) { FormatMessage_(); }
    KcpuException_x(LAddr_t aPC, PAddr_t aTarget, const std::string &aError): Generic_x(aError), mPC(aPC), mTaget(aTarget), mTargetValid(true) { FormatMessage_(); }
	KcpuException_x(const KcpuException_x &aEx): Generic_x(aEx), mPC(aEx.mPC), mTarget(aEx.mTarget), mTargetValid(aEx.mTargetValid) {}
protected:
    LAddr_t mPC;
    PAddr_t mTarget;
    bool mTargetValid;

    void FormatMessage_() {
        if (!mSpacePrinted) {
            *this << " ";
            mSpacePrinted = true;
        }
        this << "at instruction address " << HexPrinter(aPC);
        if (mTargetValid) {
            *this << " accessing " << HexPrinter(mTarget);
        }
    }
};

class ConvertedSysException_x: public KcpuException_x {
public:
    ConvertedSysException_x(LAddr_t aPC, PAddr_t aTarget, const char *aError = nullptr): KcpuException_x(aPC, aTarget, aError) {}
    ConvertedSysException_x(LAddr_t aPC, PAddr_t aTarget, const std::string &aError): KcpuException_x(aPC, aTarget, aError) {}
	ConvertedSysException_x(const ConvertedSysException_x &aEx): KcpuException_x(aEx) {}
};