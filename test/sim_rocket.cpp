
#include "VRocketSystem.h"
#include <sim_dram.hpp>
#include <sim_uart.hpp>
#include <sim_qspi.hpp>

#include <verilated_vcd_c.h>
#include <verilated.h>
#include <iostream>
#include <iomanip>

template <class T, class P>
void connect_ptr(T top, P &dram_ptr) {
    dram_ptr.awaddr  = &(top->mem_axi4_0_aw_bits_addr  );
    dram_ptr.awburst = &(top->mem_axi4_0_aw_bits_burst );
    dram_ptr.awid    = &(top->mem_axi4_0_aw_bits_id    );
    dram_ptr.awlen   = &(top->mem_axi4_0_aw_bits_len   );
    dram_ptr.awready = &(top->mem_axi4_0_aw_ready      );
    dram_ptr.awsize  = &(top->mem_axi4_0_aw_bits_size  );
    dram_ptr.awvalid = &(top->mem_axi4_0_aw_valid      );
    dram_ptr.wdata   = &(top->mem_axi4_0_w_bits_data   );
    dram_ptr.wlast   = &(top->mem_axi4_0_w_bits_last   );
    dram_ptr.wready  = &(top->mem_axi4_0_w_ready       );
    dram_ptr.wstrb   = &(top->mem_axi4_0_w_bits_strb   );
    dram_ptr.wvalid  = &(top->mem_axi4_0_w_valid       );
    dram_ptr.bid     = &(top->mem_axi4_0_b_bits_id     );
    dram_ptr.bready  = &(top->mem_axi4_0_b_ready       );
    dram_ptr.bresp   = &(top->mem_axi4_0_b_bits_resp   );
    dram_ptr.bvalid  = &(top->mem_axi4_0_b_valid       );
    dram_ptr.araddr  = &(top->mem_axi4_0_ar_bits_addr  );
    dram_ptr.arburst = &(top->mem_axi4_0_ar_bits_burst );
    dram_ptr.arid    = &(top->mem_axi4_0_ar_bits_id    );
    dram_ptr.arlen   = &(top->mem_axi4_0_ar_bits_len   );
    dram_ptr.arready = &(top->mem_axi4_0_ar_ready      );
    dram_ptr.arsize  = &(top->mem_axi4_0_ar_bits_size  );
    dram_ptr.arvalid = &(top->mem_axi4_0_ar_valid      );
    dram_ptr.rdata   = &(top->mem_axi4_0_r_bits_data   );
    dram_ptr.rid     = &(top->mem_axi4_0_r_bits_id     );
    dram_ptr.rlast   = &(top->mem_axi4_0_r_bits_last   );
    dram_ptr.rready  = &(top->mem_axi4_0_r_ready       );
    dram_ptr.rresp   = &(top->mem_axi4_0_r_bits_resp   );
    dram_ptr.rvalid  = &(top->mem_axi4_0_r_valid       );
}

int main(int argc, char** argv, char** env) {
    Verilated::commandArgs(argc, argv);
    Verilated::traceEverOn(true);
    VRocketSystem *top = new VRocketSystem;

    AXI4Ptr<32,64,4> dram_ptr;
    connect_ptr(top, dram_ptr);

    SimDRAM<32,64,4> dram_sim(1024*1024*2048L, dram_ptr);
    dram_sim.load_binary("./u-boot.itb", 0x90000000);

    SimUART uart_sim(0x364); // 0x364 <= 100M / 115200
    uart_sim.txd = &top->uart_0_txd;
    uart_sim.rxd = &top->uart_0_rxd;

    SimQSPI qspi_sim(0x2000000);
    qspi_sim.ie0 = &top->qspi_0_dq_0_ie;
    qspi_sim.ie1 = &top->qspi_0_dq_1_ie;
    qspi_sim.ie2 = &top->qspi_0_dq_2_ie;
    qspi_sim.ie3 = &top->qspi_0_dq_3_ie;
    qspi_sim.i0  = &top->qspi_0_dq_0_i ;
    qspi_sim.i1  = &top->qspi_0_dq_1_i ;
    qspi_sim.i2  = &top->qspi_0_dq_2_i ;
    qspi_sim.i3  = &top->qspi_0_dq_3_i ;
    qspi_sim.oe0 = &top->qspi_0_dq_0_oe;
    qspi_sim.oe1 = &top->qspi_0_dq_1_oe;
    qspi_sim.oe2 = &top->qspi_0_dq_2_oe;
    qspi_sim.oe3 = &top->qspi_0_dq_3_oe;
    qspi_sim.o0  = &top->qspi_0_dq_0_o ;
    qspi_sim.o1  = &top->qspi_0_dq_1_o ;
    qspi_sim.o2  = &top->qspi_0_dq_2_o ;
    qspi_sim.o3  = &top->qspi_0_dq_3_o ;
    qspi_sim.cs  = &top->qspi_0_cs_0   ;
    qspi_sim.ck  = &top->qspi_0_sck    ;
    qspi_sim.load_binary("./u-boot.itb", 0x1000000);
    qspi_sim.load_binary("./env.bin", 0xFC0000);

    // Trace
    VerilatedVcdC *tfp = new VerilatedVcdC;
    top->trace(tfp, 99);
    tfp->open("vlt_dump.vcd");
    
    // Execution
    top->reset = 1;
    top->clock = 0;
    for (uint64_t ticks = 0; !Verilated::gotFinish(); ticks++) {
        if (Verilated::time() % (10*1000000000L) == 0) {
            printf("\e[s\e[100Gtick-time [%4.9f]\e[u",
                   Verilated::time()/1e12);
        }
        /* if (Verilated::time() > 1000000000*240UL) {
            tfp->dump(Verilated::time());
        } */
        //----------------------------------------------------------------------
        Verilated::timeInc(5000);
        top->clock = 0;
        top->eval(); // Evaluate

        //----------------------------------------------------------------------
        // Receive from DUT
        //----------------------------------------------------------------------
        if (!top->reset) {
            qspi_sim.update();
            uart_sim.update();
            dram_sim.update();
        }
        
        //----------------------------------------------------------------------
        /* if (Verilated::time() > 1000000000*240UL) {
            tfp->dump(Verilated::time());
        }
        if (Verilated::time() > 1000000000*280UL) {
            break;
        } */
        //----------------------------------------------------------------------
        // Posedge and reset
        //----------------------------------------------------------------------
        if (ticks == 10) { top->reset = 0; }
        
        Verilated::timeInc(5000);
        top->clock = 1;
        top->eval(); // Evaluate
        
        //----------------------------------------------------------------------
        // Drive to DUT
        //----------------------------------------------------------------------
        if (!top->reset) {
            qspi_sim.output();
            uart_sim.output();
            dram_sim.output();
        }

        ////////////////////////////////////////////////////////////////////////
        // DEBUG
        ////////////////////////////////////////////////////////////////////////
        if (*dram_ptr.awvalid) {
            std::cerr << "dram_sim awvalid: awaddr ["
                      << std::setfill('0') << std::setw(8)
                      << std::hex << *dram_ptr.awaddr
                      << "], id ["
                      << std::hex << int(*dram_ptr.awid)
                      << "], size ["
                      << std::dec << int(0x01 << *dram_ptr.awsize)
                      << "], len ["
                      << std::dec << int(*dram_ptr.awlen + 1)
                      << "], burst ["
                      << std::dec << int(*dram_ptr.awburst)
                      << "]" << std::endl;
        }
        if (*dram_ptr.wvalid) {
            std::cerr << "dram_sim wvalid: ["
                      << std::setfill('0') << std::setw(16)
                      << std::hex << *dram_ptr.wdata
                      << "]" << std::endl;
        }
        if (*dram_ptr.arvalid) {
            std::cerr << "dram_sim arvalid: araddr [" 
                      << std::setfill('0') << std::setw(8)
                      << std::hex << *dram_ptr.araddr
                      << "], id ["
                      << std::dec << int(*dram_ptr.arid)
                      << "], size ["
                      << std::dec << int(0x01 << *dram_ptr.arsize)
                      << "], len ["
                      << std::dec << int(*dram_ptr.arlen + 1)
                      << "], burst ["
                      << std::dec << int(*dram_ptr.arburst)
                      << "]" << std::endl;
        }
        if (*dram_ptr.rvalid) {
            std::cerr << "dram_sim rvalid: ["
                      << std::setfill('0') << std::setw(16)
                      << std::hex << *dram_ptr.rdata
                      << "], id ["
                      << std::dec << int(*dram_ptr.rid)
                      << "]" << std::endl;
        }
        if (*dram_ptr.bvalid) {
            std::cerr << "dram_sim bvalid: id ["
                      << std::dec << int(*dram_ptr.bid)
                      << "]" << std::endl;
        }
    }
    tfp->close();
    top->final();
    delete top;
    return 0;
}
