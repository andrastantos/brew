#include "kcpu_exceptions.h"

ConvertedSysException_x KsysException_x::Convert(LAddr_t aPC) const {
    return ConvertedSysException_x(aPC, mTarget, mBaseMessage);
}
