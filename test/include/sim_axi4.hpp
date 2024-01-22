#ifndef SIM_AXI4_HPP
#define SIM_AXI4_HPP

#include "axi4_ptr.hpp"

#include <memory>
#include <algorithm>
#include <utility>
#include <queue>

template <unsigned int wAddr = 64, unsigned int wData = 64, unsigned int wID = 4>
class SimAXI4 {
    static_assert(wData <= 64, "wData should be <= 64.");
    static_assert(wAddr <= 64, "wAddr should be <= 64.");

    struct AXI4Req {
        AUTO_SIG(id   , wID-1  , 0);
        AUTO_SIG(addr , wAddr-1, 0);
        AUTO_SIG(len  , 7, 0);
        AUTO_SIG(size , 2, 0);
        AUTO_SIG(burst, 1, 0);
        int wait;
    };

    std::queue<std::unique_ptr<AXI4Req> > rd_req;
    std::queue<std::unique_ptr<AXI4Req> > wr_req;
    std::queue<int> bk_rsp;
    AXI4Ptr<wAddr,wData,wID> ptr;
    AXI4Val<wAddr,wData,wID> pin;

public:
    SimAXI4(const AXI4Ptr<wAddr,wData,wID> &ptr, int delay = 0): delay(delay) {
        this->ptr = ptr;

        pin.reset();
        pin.arready = 1;
        pin.awready = 1;
        pin. wready = 1;
        pin. bvalid = 0;

        rd_busy = false;
        wr_busy = false;
        wr_wait = 0; rd_wait = 0;
        wr_size = 0; rd_size = 0;
        wr_next = 0; rd_next = 0;
        wr_tail = 0; rd_tail = 0;
        wr_id   = 0;
    }

    void update() {
        pin.update(ptr);
        update_ar();
        update_aw();
        update_rd();
        update_wr();
        update_bk();
    }
    
    void output() {
        pin.output(ptr);
    }

    void reset() {
        pin.reset();
        pin. wready = 1;
        pin.awready = 1;
        pin.arready = 1;

        rd_busy = false;
        wr_busy = false;
        wr_wait = 0; rd_wait = 0;
        wr_size = 0; rd_size = 0;
        wr_next = 0; rd_next = 0;
        wr_tail = 0; rd_tail = 0;
        wr_id   = 0;
    }

private:
    unsigned int wbData = wData / 8;
    int delay;

    int  wr_wait, rd_wait;
    int  wr_size, rd_size;
    int  wr_next, rd_next;
    int  wr_tail, rd_tail;
    int  wr_id  ;
    bool wr_busy;
    bool rd_busy;

    void update_ar() {
        pin.arready = 1;
        if (pin.arvalid && pin.arready) {
            rd_req.push(std::unique_ptr<AXI4Req>(new AXI4Req {
                pin.arid   ,
                pin.araddr ,
                pin.arlen  ,
                pin.arsize ,
                pin.arburst,
                delay
            }));
        }
    }

    void update_aw() {
        pin.awready = 1;
        if (pin.awvalid && pin.awready) {
            wr_req.push(std::unique_ptr<AXI4Req>(new AXI4Req {
                pin.awid   ,
                pin.awaddr ,
                pin.awlen  ,
                pin.awsize ,
                pin.awburst,
                delay
            }));
        }
    }

    void update_rd() {
        if (rd_wait > 0) rd_wait = rd_wait - 1;
        // Peer-not-Ready
        if (pin.rvalid && !pin.rready) {
            return;
        }
        // End-of-Transmission
        if (!rd_busy) {
            if (pin.rvalid && pin.rlast) {
                rd_wait = delay;
            }
            pin.rvalid = 0;
            pin.rlast  = 0;
        }
        // Start Transmission 
        if (!rd_busy && rd_req.size() && rd_wait < 1) {
            auto ar = std::move(rd_req.front());
            rd_req.pop();

            rd_busy = true;
            rd_size = 1 << ar->size;
            rd_next = ar->addr - ar->addr % rd_size;
            rd_tail = rd_next + rd_size + rd_size * ar->len;

            pin.rid = ar->id;
            pin.rvalid = 1;
            pin.rlast  = 0;
        }

        if (!pin.rvalid) return;
        // Trigger Data Transmission
        do_read(rd_next - rd_next % wbData, wbData, (uint8_t*)&pin.rdata);
        rd_next = rd_next + rd_size;
        // Signals for the Last Cycle
        if (rd_next >= rd_tail) {
            pin.rlast = 1;
            rd_busy   = false;
            rd_size   = 0;
            rd_next   = 0;
            rd_tail   = 0;
        }
    }

    void update_wr() {
        if (wr_wait > 0) wr_wait = wr_wait - 1;
        if (wr_wait < 1) pin.wready = 1;
        if (!pin.wvalid) return;

        // Receiving Ongoing
        if (wr_busy) {
            do_write(wr_next, wr_size, reinterpret_cast<uint8_t*>(&pin.wdata + wr_next % wbData)); 
            wr_next = wr_next + wr_size;
            if (wr_next >= wr_tail) {
                wr_wait = delay;
                wr_busy = false;
                if (bk_rsp.empty()) {
                    bk_rsp.push(wr_id);
                }
            }
            return;
        }
        // Start Receiving
        if (wr_req.size() && wr_wait < 1) {
            auto wa = std::move(wr_req.front());
            wr_req.pop();

            wr_busy = true;
            wr_size = 1 << wa->size;
            wr_next = wa->addr;
            wr_tail = wr_next + wr_size + wr_size * wa->len;
            wr_id   = wa->id;

            do_write(wr_next, wr_size, reinterpret_cast<uint8_t*>(&pin.wdata + wr_next % wbData)); 
            wr_next = wr_next + wr_size;
            if (wr_next >= wr_tail) {
                wr_size = 0;
                wr_next = 0;
                wr_tail = 0;

                wr_wait = delay;
                wr_busy = false;
                if (wr_wait) pin.wready = 0;
                bk_rsp.push(wr_id);
            }
        }
    }

    void update_bk() {
        if (pin.bvalid && !pin.bready) return;
        if (bk_rsp.size()) {
            pin.bvalid = 1;
            pin.bresp = 0;
            pin.bid = bk_rsp.front();
            bk_rsp.pop();
        } else {
            pin.bvalid = 0;
            pin.bresp = 0;
            pin.bid = 0;
        }
    }

protected:
    virtual AXI4Resp do_write(uint64_t start_addr, uint64_t size, const uint8_t* buffer) = 0;
    virtual AXI4Resp do_read (uint64_t start_addr, uint64_t size, uint8_t* buffer) = 0;
};

#endif
