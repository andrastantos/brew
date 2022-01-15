#include <iostream>
#include <fstream>
#include <vector>
#include <stdint.h>
#include <array>
#include <memory>
#include <cstddef>
#include <algorithm>
#include <boost/filesystem.hpp>
#include "utils.h"
#include "interfaces.h"

#include "ksys_memory.h"
#include "ksys_interconnect.h"

#include "kcpu_mmu.h"




int main(void) {
	std::cout << "Hello world!\n" << std::endl;
	Memory_c mem(0x100000);
	std::cout << "First byte: " << int(mem.GetUInt8(0)) << std::endl;
	mem.SetUInt32(0, 0x01020304);
	std::cout << "First byte: " << int(mem.GetUInt8(0)) << int(mem.GetUInt8(1)) << int(mem.GetUInt8(2)) << int(mem.GetUInt8(3)) << std::endl;
	return 0;
}

