
#ifndef SIM_DRAM_HPP
#define SIM_DRAM_HPP

#include "sim_axi4.hpp"

#include <fstream>
#include <iostream>
#include <math.h>

template <unsigned int wAddr = 64, unsigned int wData = 64, unsigned int wID = 4>
class SimDRAM : public SimAXI4<wAddr,wData,wID> {
    const int wbData;
    off_t bit_mask;
public:
    SimDRAM(size_t size, AXI4Ptr<wAddr,wData,wID> ptr, int delay = 5): 
        SimAXI4<wAddr,wData,wID>(ptr, delay), wbData(wData/8)
    {
        if (size % wbData) size += 8 - (size % wbData);
        mem = new unsigned char[size];
        mem_size = size;
        bit_mask = (off_t(1) << int(ceil(log2(size)))) - 1;
    }

    ~SimDRAM() {
        delete []mem;
    }

    bool read(off_t start_addr, size_t size, uint8_t* buffer) {
        start_addr = start_addr & bit_mask;

        if (start_addr + size <= mem_size) {
            memcpy(buffer,&mem[start_addr],size);
            return true;
        }
        else return false;
    }

    bool write(off_t start_addr, size_t size, const uint8_t* buffer) {
        start_addr = start_addr & bit_mask;

        if (start_addr + size <= mem_size) {
            memcpy(&mem[start_addr], buffer, size);
            return true;
        }
        else return false;
    }

    void load_binary(const char *init_file, uint64_t start_addr = 0) {
        start_addr = start_addr & bit_mask;

        std::ifstream file(init_file, std::ios::in | std::ios::binary | std::ios::ate);
        size_t file_size = file.tellg();
        file.seekg(std::ios_base::beg);
        if (start_addr >= mem_size || file_size > mem_size - start_addr) {
            std::cerr << "memory size is not big enough for init file." << std::endl;
            file_size = mem_size;
        }
        file.read((char*)mem+start_addr, file_size);
    }
protected:
    AXI4Resp do_read(uint64_t start_addr, uint64_t size, uint8_t* buffer) {
        start_addr = start_addr & bit_mask;

        if (start_addr + size <= mem_size) {
            memcpy(buffer, &mem[start_addr], size);
            return RESP_OKEY;
        }
        else return RESP_DECERR;
    }

    AXI4Resp do_write(uint64_t start_addr, uint64_t size, const uint8_t* buffer) {
        start_addr = start_addr & bit_mask;
        if (start_addr + size <= mem_size) {
            memcpy(&mem[start_addr], buffer, size);
            return RESP_OKEY;
        }
        else return RESP_DECERR;
    }
private:
    uint8_t *mem;
    size_t mem_size;
};

#endif
