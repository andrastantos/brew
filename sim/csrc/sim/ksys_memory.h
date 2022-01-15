#pragma once

#include <iostream>
#include <fstream>
#include <vector>
#include <stdint.h>
#include <array>
#include <memory>
#include <cstddef>
#include <boost/filesystem.hpp>
#include "utils.h"
#include "interfaces.h"
#include "kcpu_exceptions.h"

class Memory_c: public SlavePort_i, public Component_i {
	typedef std::array<uint8_t, cPageSize> Page_t;
public:
	Memory_c(PAddr_t mem_size):
		mPages(new std::array<std::unique_ptr<Page_t>, cPageTableSize1stLevel*cPageTableSize2ndLevel>),
		mMemSize(mem_size)
	{
		
	}

	SlavePort_i &GetSlavePort() {
		return *this;
	}

	virtual uint8_t GetUInt8(PAddr_t aAddr) override {
		return GetPage_(aAddr)[aAddr & cPageMask];
	}
	virtual uint16_t GetUInt16(PAddr_t aAddr) override {
		return *reinterpret_cast<uint16_t*>(&GetPage_(aAddr)[aAddr & cPageMask]);
	}
	virtual uint32_t GetUInt32(PAddr_t aAddr) override {
		return *reinterpret_cast<uint32_t*>(&GetPage_(aAddr)[aAddr & cPageMask]);
	}

	virtual void SetUInt8(PAddr_t aAddr, uint8_t aData) override {
		GetPage_(aAddr)[aAddr & cPageMask] = aData;
	}
	virtual void SetUInt16(PAddr_t aAddr, uint16_t aData) override {
		*reinterpret_cast<uint16_t*>(&GetPage_(aAddr)[aAddr & cPageMask]) = aData;
	}
	virtual void SetUInt32(PAddr_t aAddr, uint32_t aData) override {
		*reinterpret_cast<uint32_t*>(&GetPage_(aAddr)[aAddr & cPageMask]) = aData;
	}

	virtual void Reset() override {}

	void Dump(const char *aFileName) const {
		std::ofstream file(aFileName, std::ios::binary | std::ios::out);
		size_t size = 0;
		Page_t empty_page;
		for(auto &page: *mPages) {
			Page_t &page_to_write = (page == nullptr) ? empty_page : *page;
			file.write(reinterpret_cast<char*>(&page_to_write[0]),page->size());
			size += cPageSize;
			if (size >= mAllocatedSize) break;
		}
	}

	void Load(const char *aFileName, PAddr_t aBaseAddr, PAddr_t aLength) {
		SIM_ASSERT(aBaseAddr % cPageSize == 0);

		PAddr_t bytes_left = boost::filesystem::file_size(aFileName);
		PAddr_t page_addr = aBaseAddr;
		
		std::ifstream file(aFileName, std::ios::binary | std::ios::in);

		while (bytes_left > 0) {
			Page_t &page = GetPage_(page_addr);
			file.read(reinterpret_cast<char*>(page[0]),page.size()); // Might try to read past EOF, but that's fine
			if (file.eof()) break;
			page_addr += page.size();
			bytes_left -= page.size();
		}
	}
protected:
	mutable std::unique_ptr<std::array<std::unique_ptr<Page_t>, cPageTableSize1stLevel*cPageTableSize2ndLevel>> mPages;
	mutable PAddr_t mAllocatedSize = 0;
	PAddr_t mMemSize;

	Page_t &GetPage_(PAddr_t aAddr) const {
        if (aAddr >= mMemSize) {
            throw KsysException_x(aAddr, "Indexing beyond memory size");
        }
		size_t page_idx = aAddr >> cPageBits;
		mAllocatedSize = std::max(mAllocatedSize, page_idx * cPageSize);
		if ((*mPages)[page_idx] == nullptr) {
			(*mPages)[page_idx] = std::unique_ptr<Page_t>(new Page_t);
		}
		return *(*mPages)[page_idx];
	}

};