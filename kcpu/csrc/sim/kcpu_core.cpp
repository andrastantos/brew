#include "kcpu_core.h"

#define TO_SCH()

#ifdef EXCEPTION_NEXT
#error EXCEPTION_NEXT is defined
#endif
#ifdef EXCEPTION
#error EXCEPTION is defined
#endif
#ifdef BREAK_BURST
#error BREAK_BURST is defined
#endif
#ifdef CONTINUE_BURST
#error CONTINUE_BURST is defined
#endif

#define EXCEPTION_NEXT(aaExKind) {aPC_ref = new_pc; throw TransitionToScheduler_x(aPC_ref, SchTransCause_e::aaExKind);}
#define EXCEPTION(aaExKind) {throw TransitionToScheduler_x(aPC_ref, SchTransCause_e::aaExKind);}
#define BREAK_BURST {aPC_ref = new_pc; return true;}
#define CONTINUE_BURST {aPC_ref = new_pc; return false;}

bool KcpuCore_c::Decode(uint16_t aInstCode, uint32_t aValue, size_t aInstSize, uint32_t &aPC_ref) {
    unsigned int field_c = aInstCode & 0xf000 >> 12;
    unsigned int field_b = aInstCode & 0x0f00 >> 8;
    unsigned int field_a = aInstCode & 0x00f0 >> 4;
    unsigned int field_d = aInstCode & 0x000f >> 0;

    if (field_d != 0xf && field_c != 0xf) {
        return KcpuCore_c::DecodeALU(aInstCode, aValue, aInstSize, aPC_ref);
    } else if (field_d != 0xf && field_d == 0xf) {
        return KcpuCore_c::DecodeBranch(aInstCode, aValue, aInstSize, aPC_ref);
    } else if (field_c == 0xf) {
        return KcpuCore_c::DecodeLoadStore(aInstCode, aValue, aInstSize, aPC_ref);
    }
}

bool KcpuCore_c::DecodeALU(uint16_t aInstCode, uint32_t aValue, size_t aInstSize, uint32_t &aPC_ref) {
    unsigned int field_c = aInstCode & 0xf000 >> 12;
    unsigned int field_b = aInstCode & 0x0f00 >> 8;
    unsigned int field_a = aInstCode & 0x00f0 >> 4;
    unsigned int field_d = aInstCode & 0x000f >> 0;

    SIM_ASSERT(field_d != 0xf);

    uint32_t new_pc = aPC_ref + aInstSize;

    uint32_t *reg_a = field_a == 0xf ? &aValue : field_a == 0x0 ? &aPC_ref : &mRegFile[field_a-1];
    uint32_t *reg_b = field_b == 0xf ? &aValue : field_b == 0x0 ? &aPC_ref : &mRegFile[field_b-1];
    uint32_t *reg_d =                            field_d == 0x0 ? &new_pc  : &mRegFile[field_d-1]; // Targetting a PC is special

    switch (field_c) {
        case 0x0:
            if (field_d == 0 && field_b == field_a && field_b != 0xf) {
                switch (field_a) {
                    case 0x0: // FILL
                        EXCEPTION_NEXT(FILL);
                    case 0x1: // BREAK
                        EXCEPTION_NEXT(BREAK);
                    case 0x2: // SYSCALL
                        EXCEPTION_NEXT(SYSCALL);
                    break;
                    case 0x0330: // STU
                        mExecContext = ExecContexts_e::TSK;
                    break;
                    case 0xd: // FENCE
                    case 0xe: // WFENCE
                    break;
                    default:
                        EXCEPTION(SII);
                }
                CONTINUE_BURST;
            } else if (field_b == 0xf) {
                EXCEPTION(SII);
            } else {
                // XOR
                *reg_d = *reg_a ^ *reg_b;
                CONTINUE_BURST;
            }
            SIM_ASSERT(false);
        case 0x1:
            if (field_d == 0 && field_b == 0 && field_b == 0) {
                // WOI
                *reg_d = *reg_a | *reg_b; // Operationally, this is the same thing as the rest, so PC=PC|PC. The only difference is that we do break the burst.
                BREAK_BURST;
            } else if (field_b == 0xf) {
                EXCEPTION(SII);
            } else {
                // OR
                *reg_d = *reg_a | *reg_b;
                CONTINUE_BURST;
            }
            SIM_ASSERT(false);
        case 0x2:
            if (field_d == 0 && field_b == field_a && field_b != 0xf) {
                // BSWAP
                *reg_d = SwapBytes(*reg_a);
                CONTINUE_BURST;
            } else if (field_b == 0xf) {
                EXCEPTION(SII);
            } else {
                // AND
                *reg_d = *reg_a & *reg_b;
                CONTINUE_BURST;
            }
            SIM_ASSERT(false);
        case 0x3:
            if (field_d == 0 && field_b == field_a && field_b != 0xf) {
                // WSWAP
                *reg_d = ((*reg_a & 0xffff0000) >> 16) | ((*reg_a & 0x0000ffff) >> 0);
                CONTINUE_BURST;
            } else if (field_b == 0xf) {
                EXCEPTION(SII);
            } else {
                // SUB
                *reg_d = *reg_a - *reg_b;
                CONTINUE_BURST;
            }
            SIM_ASSERT(false);
        case 0x4:
            if (field_b == 0xf) {
                EXCEPTION(SII);
            } else {
                // ADD
                *reg_d = *reg_a + *reg_b;
                CONTINUE_BURST;
            }
            SIM_ASSERT(false);
        case 0x5:
            // SHL
            *reg_d = *reg_a << *reg_b;
            CONTINUE_BURST;
            SIM_ASSERT(false);
        case 0x6:
            // SHR
            *reg_d = *reg_a >> *reg_b;
            CONTINUE_BURST;
            SIM_ASSERT(false);
        case 0x7:
            // SAR
            *reg_d = int32_t(*reg_a) >> *reg_b;
            CONTINUE_BURST;
            SIM_ASSERT(false);
        case 0x8:
            if (field_b == 0) {
                // TMOV
                if (field_d == 0 & mExecContext == ExecContexts_e::SCH) reg_d = &mTPC;
                if (field_a == 0 & mExecContext == ExecContexts_e::SCH) reg_a = &mTPC;
                *reg_d = *reg_a;
                CONTINUE_BURST;
            } else if (field_b == 0xf) {
                EXCEPTION(SII);
            } else {
                // MULS
                *reg_d = int32_t(*reg_a) * int32_t(*reg_b);
                CONTINUE_BURST;
            }
            SIM_ASSERT(false);
        case 0x9:
            if (field_b == 0 && field_a == 0) {
                EXCEPTION(SII);
            } else if (field_b == 0) {
                // INC
                *reg_d = *reg_a + 1;
                CONTINUE_BURST;
            } else if (field_a == 0) {
                // DEC
                *reg_d = *reg_a - 1;
                CONTINUE_BURST;
            } else if (field_b == 0xf) {
                EXCEPTION(SII);
            } else {
                // MUL
                *reg_d = *reg_a * *reg_b;
                CONTINUE_BURST;
            }
            SIM_ASSERT(false);
        case 0xa:
            if (field_b == 0 && field_a == 0) {
                EXCEPTION(SII);
            } else if (field_b == 0) {
                // NEG
                *reg_d = -*reg_a;
                CONTINUE_BURST;
            } else if (field_a == 0) {
                // NOT
                *reg_d = ~*reg_a;
                CONTINUE_BURST;
            } else if (field_b == 0xf) {
                EXCEPTION(SII);
            } else {
                // XMULS
                *reg_d = int32_t((int64_t(*reg_a) * int64_t(*reg_b)) >> 32);
                CONTINUE_BURST;
            }
            SIM_ASSERT(false);
        case 0xb:
            if (field_b == 0 && field_a == 0) {
                EXCEPTION(SII);
            } else if (field_b == 0) {
                EXCEPTION(SII);
            } else if (field_a == 0) {
                EXCEPTION(SII);
            } else if (field_b == 0xf) {
                EXCEPTION(SII);
            } else {
                // XMUL
                *reg_d = int32_t((uint64_t(*reg_a) * uint64_t(*reg_b)) >> 32);
                CONTINUE_BURST;
            }
            SIM_ASSERT(false);
        case 0xc:
            EXCEPTION(SII);
            SIM_ASSERT(false);
        case 0xd:
            EXCEPTION(SII);
            SIM_ASSERT(false);
        case 0xe:
            EXCEPTION(SII);
            SIM_ASSERT(false);
        default:
            SIM_ASSERT(false);
    }
    SIM_ASSERT(false);
}

#ifdef BRANCH
#error BRANCH should not be defined
#endif
#define BRANCH {new_pc = relative ? aPC_ref + Offset : Offset;}
bool KcpuCore_c::DecodeBranch(uint16_t aInstCode, uint32_t aValue, size_t aInstSize, uint32_t &aPC_ref) {
    unsigned int field_c = aInstCode & 0xf000 >> 12;
    unsigned int field_b = aInstCode & 0x0f00 >> 8;
    unsigned int field_a = aInstCode & 0x00f0 >> 4;
    unsigned int field_d = aInstCode & 0x000f >> 0;
    LAddr_t Offset = aValue & ~3;
    bool relative = (aValue & 1) != 0;

    uint32_t new_pc = aPC_ref + aInstSize;

    uint32_t *reg_a = field_a == 0xf ? &aValue : field_a == 0x0 ? &aPC_ref : &mRegFile[field_a-1];
    uint32_t *reg_b = field_b == 0xf ? &aValue : field_b == 0x0 ? &aPC_ref : &mRegFile[field_b-1];

    SIM_ASSERT(field_d == 0xf);
    SIM_ASSERT(field_c != 0xf);
    if (field_b != 0xf && field_a != 0xf) {
        switch (field_c) {
            case 0x0: // BEQ
                if (*reg_a == *reg_b) BRANCH;
                BREAK_BURST;
                SIM_ASSERT(false);
            case 0x1: // BNE
                if (*reg_a != *reg_b) BRANCH;
                BREAK_BURST;
                SIM_ASSERT(false);
            case 0x2: // BGTS
                if (int32_t(*reg_a) > int32_t(*reg_b)) BRANCH;
                BREAK_BURST;
                SIM_ASSERT(false);
            case 0x3: // BLES
                if (int32_t(*reg_a) <= int32_t(*reg_b)) BRANCH;
                BREAK_BURST;
                SIM_ASSERT(false);
            case 0x4: // BGT
                if (*reg_a > *reg_b) BRANCH;
                BREAK_BURST;
                SIM_ASSERT(false);
            case 0x5: // BLE
                if (*reg_a <= *reg_b) BRANCH;
                BREAK_BURST;
                SIM_ASSERT(false);
            case 0x6:
                switch (field_a) {
                    case 0x0: // BEQZ
                        if (0 == *reg_b) BRANCH;
                        BREAK_BURST;
                        SIM_ASSERT(false);
                    case 0x1: // BNEZ
                        if (0 != *reg_b) BRANCH;
                        BREAK_BURST;
                        SIM_ASSERT(false);
                    case 0x2: // BLTZS
                        if (0 > int32_t(*reg_b)) BRANCH;
                        BREAK_BURST;
                        SIM_ASSERT(false);
                    case 0x3: // BGEZS
                        if (0 <= int32_t(*reg_b)) BRANCH;
                        BREAK_BURST;
                        SIM_ASSERT(false);
                    case 0x4: // BGTZS
                        if (0 < int32_t(*reg_b)) BRANCH;
                        BREAK_BURST;
                        SIM_ASSERT(false);
                    case 0x5: // BLEZS
                        if (0 >= int32_t(*reg_b)) BRANCH;
                        BREAK_BURST;
                        SIM_ASSERT(false);
                    default:
                        EXCEPTION(SII);
                        SIM_ASSERT(false);
                }
                SIM_ASSERT(false);
            case 0x7:
            case 0x8:
            case 0x9:
            case 0xa:
            case 0xb:
            case 0xc:
            case 0xd:
            case 0xe:
                EXCEPTION(SII);
                SIM_ASSERT(false);
            default:
                SIM_ASSERT(false);
        }
    } else if (field_b == 0xf && field_a != 0xf) {
        switch (field_c) {
            case 0x0:
            case 0x1:
            case 0x2:
            case 0x3:
            case 0x4:
            case 0x5:
            case 0x6:
            case 0x7:
            case 0x8:
            case 0x9:
            case 0xa:
            case 0xb:
                if ((*reg_a & (1 << field_c)) != 0) BRANCH;
                BREAK_BURST;
                SIM_ASSERT(false);
            case 0xc:
            case 0xd:
            case 0xe:
                EXCEPTION(SII);
                SIM_ASSERT(false);
            default:
                SIM_ASSERT(false);
        }
    } else if (field_b != 0xf && field_a == 0xf) {
        switch (field_c) {
            case 0x0:
            case 0x1:
            case 0x2:
            case 0x3:
            case 0x4:
            case 0x5:
            case 0x6:
            case 0x7:
            case 0x8:
            case 0x9:
            case 0xa:
            case 0xb:
                if ((*reg_b & (1 << field_c)) == 0) BRANCH;
                BREAK_BURST;
                SIM_ASSERT(false);
            case 0xc:
            case 0xd:
            case 0xe:
                EXCEPTION(SII);
                SIM_ASSERT(false);
            default:
                SIM_ASSERT(false);
        }
    } else {
        EXCEPTION(SII);
    }
    SIM_ASSERT(false);
}
#undef BRANCH

bool KcpuCore_c::DecodeLoadStore(uint16_t aInstCode, uint32_t aValue, size_t aInstSize, uint32_t &aPC_ref) {
    unsigned int field_c = aInstCode & 0xf000 >> 12;
    unsigned int field_b = aInstCode & 0x0f00 >> 8;
    unsigned int field_a = aInstCode & 0x00f0 >> 4;
    unsigned int field_d = aInstCode & 0x000f >> 0;

    SIM_ASSERT(field_c == 0xf);

    uint32_t new_pc = aPC_ref + aInstSize;

    uint32_t *reg_a = field_a == 0xf ? &aValue : field_a == 0x0 ? &aPC_ref : &mRegFile[field_a-1];
    uint32_t *reg_b = field_b == 0xf ? &aValue : field_b == 0x0 ? &aPC_ref : &mRegFile[field_b-1];
    uint32_t *reg_d_dst = field_d == 0xf ? &mTPC   : field_d == 0x0 ? &new_pc  : &mRegFile[field_d-1]; // Targetting a PC is special
    uint32_t *reg_d_src = field_d == 0xf ? &mTPC   : field_d == 0x0 ? &aPC_ref  : &mRegFile[field_d-1]; // Targetting a PC is special

    if (field_b & 0x8 == 0 && field_a != 0xf) {
        // load/store R_D from [R_B]
        if (field_d == 0xf) {
            EXCEPTION(SII);
            SIM_ASSERT(false);
        } else {
            LAddr_t offs = *reg_a;
            switch (field_b & 0x7) {
                case 0x0: // LDSB
                    *reg_d_dst = int32_t(int8_t(mMemoryPort.GetUInt8(offs)));
                    CONTINUE_BURST;
                    SIM_ASSERT(false);
                case 0x1: // LDB
                    *reg_d_dst = mMemoryPort.GetUInt8(offs);
                    CONTINUE_BURST;
                    SIM_ASSERT(false);
                case 0x2: // LDSW
                    if (offs & 1 != 0) EXCEPTION(MISALIGNED);
                    *reg_d_dst = int32_t(int16_t(mMemoryPort.GetUInt16(offs)));
                    CONTINUE_BURST;
                    SIM_ASSERT(false);
                case 0x3: // LDW
                    if (offs & 1 != 0) EXCEPTION(MISALIGNED);
                    *reg_d_dst = mMemoryPort.GetUInt16(offs);
                    CONTINUE_BURST;
                    SIM_ASSERT(false);
                case 0x4: // LD
                    if (offs & 3 != 0) EXCEPTION(MISALIGNED);
                    *reg_d_dst = mMemoryPort.GetUInt32(offs);
                    CONTINUE_BURST;
                    SIM_ASSERT(false);
                case 0x5: // STB
                    mMemoryPort.SetUInt8(offs, *reg_d_src);
                    CONTINUE_BURST;
                    SIM_ASSERT(false);
                case 0x6: // STW
                    if (offs & 1 != 0) EXCEPTION(MISALIGNED);
                    mMemoryPort.SetUInt16(offs, *reg_d_src);
                    CONTINUE_BURST;
                    SIM_ASSERT(false);
                case 0x7: // ST
                    if (offs & 3 != 0) EXCEPTION(MISALIGNED);
                    mMemoryPort.SetUInt32(offs, *reg_d_src);
                    CONTINUE_BURST;
                    SIM_ASSERT(false);
                default:
                    SIM_ASSERT(false);
            }
            SIM_ASSERT(false);
        }
    } else if (field_b & 0x8 == 0 && field_a == 0xf) {
        switch (field_b & 0x7) {
            case 0x0:
            case 0x1: // LDB
            case 0x2: // LDSW
            case 0x3: // LDW
                EXCEPTION(SII);
                SIM_ASSERT(false);
            case 0x4: // LD
                *reg_d_dst = *reg_a;
                CONTINUE_BURST;
                SIM_ASSERT(false);
            case 0x5: // STB
            case 0x6: // STW
            case 0x7: // ST
                EXCEPTION(SII);
                SIM_ASSERT(false);
            default:
                SIM_ASSERT(false);
        }
        SIM_ASSERT(false);
    } else if (field_b & 0x8 != 0) {
        LAddr_t offs = field_a == 0x0f ? aValue : aValue + *reg_a;
        switch (field_b & 0x7) {
            case 0x0: // LDSB
                *reg_d_dst = int32_t(int8_t(mMemoryPort.GetUInt8(offs)));
                CONTINUE_BURST;
                SIM_ASSERT(false);
            case 0x1: // LDB
                *reg_d_dst = mMemoryPort.GetUInt8(offs);
                CONTINUE_BURST;
                SIM_ASSERT(false);
            case 0x2: // LDSW
                if (offs & 1 != 0) EXCEPTION(MISALIGNED);
                *reg_d_dst = int32_t(int16_t(mMemoryPort.GetUInt16(offs)));
                CONTINUE_BURST;
                SIM_ASSERT(false);
            case 0x3: // LDW
                if (offs & 1 != 0) EXCEPTION(MISALIGNED);
                *reg_d_dst = mMemoryPort.GetUInt16(offs);
                CONTINUE_BURST;
                SIM_ASSERT(false);
            case 0x4: // LD
                if (offs & 3 != 0) EXCEPTION(MISALIGNED);
                *reg_d_dst = mMemoryPort.GetUInt32(offs);
                CONTINUE_BURST;
                SIM_ASSERT(false);
            case 0x5: // STB
                mMemoryPort.SetUInt8(offs, *reg_d_src);
                CONTINUE_BURST;
                SIM_ASSERT(false);
            case 0x6: // STW
                if (offs & 1 != 0) EXCEPTION(MISALIGNED);
                mMemoryPort.SetUInt16(offs, *reg_d_src);
                CONTINUE_BURST;
                SIM_ASSERT(false);
            case 0x7: // ST
                if (offs & 3 != 0) EXCEPTION(MISALIGNED);
                mMemoryPort.SetUInt32(offs, *reg_d_src);
                CONTINUE_BURST;
                SIM_ASSERT(false);
            default:
                SIM_ASSERT(false);
        }
        SIM_ASSERT(false);        
    }
    SIM_ASSERT(false);
}

#undef EXCEPTION_NEXT
#undef EXCEPTION
#undef BREAK_BURST
#undef CONTINUE_BURST
