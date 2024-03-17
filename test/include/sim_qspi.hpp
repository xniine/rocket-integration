
#ifndef SIM_QSPI_HPP
#define SIM_QSPI_HPP

class SimQSPI {
public:
    VL_IN8 (*ie0, 0, 0); VL_OUT8(*oe0, 0, 0);
    VL_IN8 (*ie1, 0, 0); VL_OUT8(*oe1, 0, 0);
    VL_IN8 (*ie2, 0, 0); VL_OUT8(*oe2, 0, 0);
    VL_IN8 (*ie3, 0, 0); VL_OUT8(*oe3, 0, 0);
    VL_IN8 (*i0 , 0, 0); VL_OUT8(*o0 , 0, 0);
    VL_IN8 (*i1 , 0, 0); VL_OUT8(*o1 , 0, 0);
    VL_IN8 (*i2 , 0, 0); VL_OUT8(*o2 , 0, 0);
    VL_IN8 (*i3 , 0, 0); VL_OUT8(*o3 , 0, 0);

    VL_OUT8(*cs , 0, 0);
    VL_OUT8(*ck , 0, 0);
private:
    bool clk_act;
    bool spi_act;
    int  spi_out;
    int  spi_cmd;
    int  spi_adr;
    int  spi_idx;
    int  spi_sts;
public:
    SimQSPI(int size, unsigned char fill = 0xff) {
        mem = new unsigned char[size];
        mem_size = size;
        memset(mem, fill, mem_size);
        spi_cmd  = 0;
        spi_adr  = 0;
        spi_out  = 0;
        spi_idx  = 0;
        spi_act  = false;
        spi_sts  = 0;
    }

    ~SimQSPI() {
        delete []mem;
    }

    void load_binary(const char *init_file, uint64_t start_addr = 0) {
        start_addr = start_addr % mem_size;

        std::ifstream file(init_file, std::ios::in | std::ios::binary | std::ios::ate);
        size_t file_size = file.tellg();
        file.seekg(std::ios_base::beg);
        if (start_addr >= mem_size || file_size > mem_size - start_addr) {
            std::cerr << "memory size is not big enough for init file." << std::endl;
            file_size = mem_size;
        }
        file.read((char*)mem+start_addr, file_size);
    }

    void output() {
        if (*this->ie0) *this->i0 = (spi_out     ) & 0x1;
        if (*this->ie1) *this->i1 = (spi_out >> 1) & 0x1;
        if (*this->ie2) *this->i2 = (spi_out >> 2) & 0x1;
        if (*this->ie3) *this->i3 = (spi_out >> 3) & 0x1;
    }

    void update() {
        bool posedge = *this->ck && !clk_act;
        clk_act =  *this->ck;
        spi_act = !*this->cs;
        if (!spi_act) {
            spi_idx = 0;
            spi_cmd = 0;
            spi_adr = 0;
            return;
        }

        if (!posedge) {
            return;
        }

        if (spi_idx <= 7) {
            // Read Command
            spi_cmd = spi_cmd << 1;
            if (*this->oe0 && *this->o0) {
                spi_cmd = spi_cmd | 0x1;
            }
        }

        if (spi_idx == 7) {
            fprintf(stdout, "[%4.9f] spi cmd %02x\n",
                    Verilated::time()/1e12, spi_cmd);
        }

        if (spi_idx >= 7) {
            switch (spi_cmd) {
                case 0x03: read_addr(1, 3); send_data(1, 3, 32); break; // Normal
                case 0x0B: read_addr(1, 3); send_data(1, 3, 40); break; // Fast
                case 0x3B: read_addr(1, 3); send_data(2, 3, 40); break; // Dual Out Fast
                case 0xBB: read_addr(2, 3); send_data(2, 3, 24); break; // Dual I/O Fast
                case 0x6B: read_addr(1, 3); send_data(4, 3, 40); break; // Quad Out Fast
                case 0xEB: read_addr(4, 3); send_data(4, 3, 20); break; // Quad I/O Fast

                case 0x13: read_addr(1, 4); send_data(1, 4, 40); break;
                case 0x0C: read_addr(1, 4); send_data(1, 4, 48); break;
                case 0x3C: read_addr(1, 4); send_data(2, 4, 48); break;
                case 0xBC: read_addr(2, 4); send_data(2, 4, 28); break;
                case 0x6C: read_addr(1, 4); send_data(4, 4, 48); break;
                case 0xEC: read_addr(4, 4); send_data(4, 4, 22); break;

                case 0x9F: send_id(); break;
                case 0x01: read_st(); break;
                case 0x05: send_st(); break;
                case 0x06: break;
            }
        }
        spi_idx = spi_idx + 1;
    }

    void read_st() {
        int idx = spi_idx - 7;
        if (idx && idx <= 8) {
            spi_sts = spi_sts << 1;
            if (*this->oe0 && *this->o0) {
                spi_sts = spi_sts | 0x01;
            } 
            fprintf(stdout, "[%4.9f] spi idx %2d, din %02x\n",
                    Verilated::time()/1e12, idx, spi_sts & 0x1);
        }
    }

    void send_st() {
        int idx = spi_idx - 7;
        int val = spi_sts >> (7 - idx);
        spi_out = 0;
        if (idx < 8) {
            spi_out = 0x2 & (val << 1);
            fprintf(stdout, "[%4.9f] spi idx %2d, out %02x\n",
                    Verilated::time()/1e12, idx, spi_out);
        }
    }

    void send_id() {
        int idx = spi_idx - 7;
        int pid = 0x9D7019;
        int val = 0;
        if (idx <  8) {
            val = (pid >> (23 - idx % 8));
        } else
        if (idx < 16) {
            val = (pid >> (15 - idx % 8));
        } else
        if (idx < 24) {
            val = (pid >> ( 7 - idx % 8));
        }
        spi_out = 0;
        if (idx < 24) {
            spi_out = 0x2 & (val << 1);
            fprintf(stdout, "[%4.9f] spi idx %2d, out %02x\n",
                    Verilated::time()/1e12, idx, spi_out);
        }
    }

    void send_data(int ndq, int nb, int hdr) {
        int idx = spi_idx - hdr + 1;
        int ptr = spi_adr + idx * ndq / 8;
        int bat = idx % (8 / ndq);
        int val = mem[ptr % mem_size];
        if (idx < 0) {
            val = 0;
            return;
        }

        int out = val >> (8 - ndq - bat * ndq);
        spi_out = out & 0xF;
        if (ndq == 1) {
            spi_out = (out << 1) & 0x2;
        } else
        if (ndq == 2) {
            spi_out = (out << 1) & 0x6;
        }
        static int n = 0;
        if (n == 256) {
            fprintf(stdout, "[%4.9f] spi send_data idx %3d, ptr %08x, val %02x, out %02x\n",
                    Verilated::time()/1e12, idx, ptr, val, spi_out);
            n = 0;
        }
        n = n + 1;
    }

    void read_addr(int ndq, int nb) {
        if (spi_idx > 8 * nb + 7) return;
        if (spi_idx < 8) return;

        spi_adr = (spi_adr << ndq);
        if (ndq == 1) {
            if (*this->oe0 && *this->o0) spi_adr = spi_adr | 0x1;
        } else
        if (ndq == 2) {
            if (*this->oe0 && *this->o0) spi_adr = spi_adr | 0x1;
            if (*this->oe1 && *this->o1) spi_adr = spi_adr | 0x2;
        } else
        if (ndq == 4) {
            if (*this->oe0 && *this->o0) spi_adr = spi_adr | 0x1;
            if (*this->oe1 && *this->o1) spi_adr = spi_adr | 0x2;
            if (*this->oe2 && *this->o2) spi_adr = spi_adr | 0x4;
            if (*this->oe3 && *this->o3) spi_adr = spi_adr | 0x8;
        }
        if (spi_idx == 8 * nb + 7) {
            fprintf(stdout, "[%4.9f] spi read_addr idx %3d, adr %08x\n", 
                    Verilated::time()/1e12, spi_idx, spi_adr);
        }
    }
private:
    uint8_t *mem;
    size_t mem_size;
};

#endif

