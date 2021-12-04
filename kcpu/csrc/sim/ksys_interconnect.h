#pragma once

#include <vector>
#include <stdint.h>
#include <cstddef>
#include <algorithm>
#include <boost/filesystem.hpp>
#include "utils.h"
#include "interfaces.h"
#include "kcpu_exceptions.h"

class Interconnect_c: public SlavePort_i, public Component_i {
public:
	Interconnect_c() {}

	void ConnectTarget(SlavePort_i &aPort, PAddr_t aOfs, PAddr_t aWindowSize) {
		Target_s s;
		s.port = &aPort;
		s.ofs = aOfs;
		s.window_size = aWindowSize;
		mTargets.push_back(s);
		// We could do a sorted insert, but I'm lazy. Plus, this part of the code is not
		// time critical, the one that tries to find the match for an offset is.
		std::sort(mTargets.begin(), mTargets.end(), Interconnect_c::TargetCompare_);
	}

	SlavePort_i &GetSlavePort() {
		return *this;
	}

	virtual void Reset() override {}
	virtual uint8_t GetUInt8(PAddr_t aAddr) override {
		auto target = FindTarget_(aAddr);
		return target.port->GetUInt8(target.ofs);
	}
	virtual uint16_t GetUInt16(PAddr_t aAddr) override {
		auto target = FindTarget_(aAddr);
		return target.port->GetUInt16(target.ofs);
	}
	virtual uint32_t GetUInt32(PAddr_t aAddr) override {
		auto target = FindTarget_(aAddr);
		return target.port->GetUInt32(target.ofs);
	}

	virtual void SetUInt8(PAddr_t aAddr, uint8_t aData) override {
		auto target = FindTarget_(aAddr);
		target.port->SetUInt8(target.ofs, aData);
	}
	virtual void SetUInt16(PAddr_t aAddr, uint16_t aData) override {
		auto target = FindTarget_(aAddr);
		target.port->SetUInt16(target.ofs, aData);
	}
	virtual void SetUInt32(PAddr_t aAddr, uint32_t aData) override {
		auto target = FindTarget_(aAddr);
		target.port->SetUInt32(target.ofs, aData);
	}

protected:
	struct TargetOffs_s {
		SlavePort_i *port;
		PAddr_t ofs;
	};

	TargetOffs_s FindTarget_(PAddr_t aAddr) const {
		auto target = std::lower_bound(mTargets.begin(), mTargets.end(), aAddr, TargetCompare_);
		if (target == mTargets.end()) {
			// Nothing maps to this address
            throw SysAddrDecodeError_x(aAddr, "No slave at address");
		}
		TargetOffs_s ret_val;
		ret_val.port = target->port;
		ret_val.ofs = aAddr - target->ofs;
		if (ret_val.ofs >= target->window_size) {
			// Outside of decode window
            throw SysAddrDecodeError_x(aAddr, "Outside of slave decode window");
		}
		return ret_val;
	}

	struct Target_s {
		SlavePort_i *port;
		PAddr_t ofs;
		PAddr_t window_size;
	};

	static bool TargetCompare_(const Target_s &a, const Target_s &b) {
		return a.ofs < b.ofs;
	}
	
	std::vector<Target_s> mTargets;
};
