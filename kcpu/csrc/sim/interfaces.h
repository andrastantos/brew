#pragma once

#include <cstddef>
#include "base_types.h"

class Component_i {
public:
	virtual void Reset() = 0;
};

class SlavePort_i {
public:
	virtual uint8_t GetUInt8(PAddr_t aOffs) = 0;
	virtual uint16_t GetUInt16(PAddr_t aOffs) = 0;
	virtual uint32_t GetUInt32(PAddr_t aOffs) = 0;
	virtual void SetUInt8(PAddr_t aOffs, uint8_t aData) = 0;
	virtual void SetUInt16(PAddr_t aOffs, uint16_t aData) = 0;
	virtual void SetUInt32(PAddr_t aOffs, uint32_t aData) = 0;
};

class CoreSlavePort_i {
public:
	virtual uint8_t GetUInt8(LAddr_t aOffs) = 0;
	virtual uint16_t GetUInt16(LAddr_t aOffs) = 0;
	virtual uint32_t GetUInt32(LAddr_t aOffs) = 0;
    virtual uint32_t FetchUInt32(LAddr_t aOffs) = 0;
	virtual void SetUInt8(LAddr_t aOffs, uint8_t aData) = 0;
	virtual void SetUInt16(LAddr_t aOffs, uint16_t aData) = 0;
	virtual void SetUInt32(LAddr_t aOffs, uint32_t aData) = 0;
};

