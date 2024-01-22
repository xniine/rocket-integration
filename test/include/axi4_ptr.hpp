#ifndef AXI4_HPP
#define AXI4_HPP

#include <verilated.h>
#include <condition_variable>
#include <cstdint>
#include <set>
#include <cstring>

#define AUTO_SIG(name, msb, lsb) \
    typename std::conditional <(msb-lsb+1) <=  8, CData, \
    typename std::conditional <(msb-lsb+1) <= 16, SData, \
    typename std::conditional <(msb-lsb+1) <= 32, IData, \
        QData>::type>::type>::type name

#define AUTO_OUT(name, msb, lsb) AUTO_SIG(name, msb, lsb)
#define  AUTO_IN(name, msb, lsb) AUTO_SIG(name, msb, lsb)

template<unsigned int wAddr = 64, unsigned int wData = 64, unsigned int wID = 4>
struct AXI4Ptr {
    static_assert(__builtin_popcount(wData) == 1,"wData should be the power of 2.");
    static_assert(wData >= 8,"wData should be larger or equal to 8.");
    // aw
    AUTO_IN (*awid   , wID-1  , 0)   = NULL;
    AUTO_IN (*awaddr , wAddr-1, 0)   = NULL;
    AUTO_IN (*awlen  , 7, 0)         = NULL;
    AUTO_IN (*awsize , 2, 0)         = NULL;
    AUTO_IN (*awburst, 1, 0)         = NULL;
    AUTO_IN (*awvalid, 0, 0)         = NULL;
    AUTO_OUT(*awready, 0, 0)         = NULL;
    // w
    AUTO_IN (*wdata  , wData-1  , 0) = NULL;
    AUTO_IN (*wstrb  , wData/8-1, 0) = NULL;
    AUTO_IN (*wlast  , 0, 0)         = NULL;
    AUTO_IN (*wvalid , 0, 0)         = NULL;
    AUTO_OUT(*wready , 0, 0)         = NULL;
    // b
    AUTO_OUT(*bid    , wID-1, 0)     = NULL;
    AUTO_OUT(*bresp  , 1, 0)         = NULL;
    AUTO_OUT(*bvalid , 0, 0)         = NULL;
    AUTO_IN (*bready , 0, 0)         = NULL;
    // ar
    AUTO_IN (*arid   , wID-1  , 0)   = NULL;
    AUTO_IN (*araddr , wAddr-1, 0)   = NULL;
    AUTO_IN (*arlen  , 7, 0)         = NULL;
    AUTO_IN (*arsize , 2, 0)         = NULL;
    AUTO_IN (*arburst, 1, 0)         = NULL;
    AUTO_IN (*arvalid, 0, 0)         = NULL;
    AUTO_OUT(*arready, 0, 0)         = NULL;
    // r
    AUTO_OUT(*rid    , wID-1  , 0)   = NULL;
    AUTO_OUT(*rdata  , wData-1, 0)   = NULL;
    AUTO_OUT(*rresp  , 1, 0)         = NULL;
    AUTO_OUT(*rlast  , 0, 0)         = NULL;
    AUTO_OUT(*rvalid , 0, 0)         = NULL;
    AUTO_IN (*rready , 0, 0)         = NULL;
};

template<unsigned int wAddr = 64, unsigned int wData = 64, unsigned int wID = 4>
struct AXI4Val {
    AUTO_IN (awid   , wID-1  , 0);
    AUTO_IN (awaddr , wAddr-1, 0);
    AUTO_IN (awlen  , 7, 0);
    AUTO_IN (awsize , 2, 0);
    AUTO_IN (awburst, 1, 0);
    AUTO_IN (awvalid, 0, 0);
    AUTO_OUT(awready, 0, 0);
    // w
    AUTO_IN (wdata  , wData-1  , 0);
    AUTO_IN (wstrb  , wData/8-1, 0);
    AUTO_IN (wlast  , 0, 0);
    AUTO_IN (wvalid , 0, 0);
    AUTO_OUT(wready , 0, 0);
    // b
    AUTO_OUT(bid    , wID-1, 0);
    AUTO_OUT(bresp  , 1, 0);
    AUTO_OUT(bvalid , 0, 0);
    AUTO_IN (bready , 0, 0);
    // ar
    AUTO_IN (arid   , wID-1  , 0);
    AUTO_IN (araddr , wAddr-1, 0);
    AUTO_IN (arlen  , 7, 0);
    AUTO_IN (arsize , 2, 0);
    AUTO_IN (arburst, 1, 0);
    AUTO_IN (arvalid, 0, 0);
    AUTO_OUT(arready, 0, 0);
    // r
    AUTO_OUT(rid    , wID-1  , 0);
    AUTO_OUT(rdata  , wData-1, 0);
    AUTO_OUT(rresp  , 1, 0);
    AUTO_OUT(rlast  , 0, 0);
    AUTO_OUT(rvalid , 0, 0);
    AUTO_IN (rready , 0, 0);

    AXI4Val() {
        arid    = 0; awid    = 0;
        araddr  = 0; awaddr  = 0;
        arlen   = 0; awlen   = 0;
        arsize  = 0; awsize  = 0;
        arburst = 0; awburst = 0;
        arvalid = 0; awvalid = 0;
        arready = 0; awready = 0;

        rid     = 0;
        rdata   = 0; wdata   = 0;
        rresp   = 0; wstrb   = 0;
        rlast   = 0; wlast   = 0;
        rvalid  = 0; wvalid  = 0;
        rready  = 0; wready  = 0;

        bid     = 0;
        bresp   = 0;
        bvalid  = 0;
        bready  = 0;
    }

    void reset() {
        arid    = 0; awid    = 0;
        araddr  = 0; awaddr  = 0;
        arlen   = 0; awlen   = 0;
        arsize  = 0; awsize  = 0;
        arburst = 0; awburst = 0;
        arvalid = 0; awvalid = 0;
        arready = 0; awready = 0;

        rid     = 0;
        rdata   = 0; wdata   = 0;
        rresp   = 0; wstrb   = 0;
        rlast   = 0; wlast   = 0;
        rvalid  = 0; wvalid  = 0;
        rready  = 0; wready  = 0;

        bid     = 0;
        bresp   = 0;
        bvalid  = 0;
        bready  = 0;
    }

    void update(const AXI4Ptr<wAddr,wData,wID> &ptr) {
        // aw
        awid    = *ptr.awid   ;
        awaddr  = *ptr.awaddr ;
        awlen   = *ptr.awlen  ;// & 0xFF;
        awsize  = *ptr.awsize ;// & 0x07;
        awburst = *ptr.awburst;// & 0x03;
        awvalid = *ptr.awvalid;
        // w
        wdata   = *ptr.wdata  ;
        wstrb   = *ptr.wstrb  ;
        wlast   = *ptr.wlast  ;
        wvalid  = *ptr.wvalid ;
        // b
        bready  = *ptr.bready ;
        // arid
        arid    = *ptr.arid   ;
        araddr  = *ptr.araddr ;
        arlen   = *ptr.arlen  ;// & 0xFF;
        arsize  = *ptr.arsize ;// & 0x07;
        arburst = *ptr.arburst;// & 0x03;
        arvalid = *ptr.arvalid;
        // r
        rready  = *ptr.rready ;
    }

    void output(AXI4Ptr<wAddr,wData,wID> &ptr) {
        *ptr.awready = awready;
        *ptr.wready  = wready ;
        *ptr.bid     = bid    ;
        *ptr.bresp   = bresp  ;
        *ptr.bvalid  = bvalid ;
        *ptr.arready = arready;
        *ptr.rid     = rid    ;
        *ptr.rdata   = rdata  ;
        *ptr.rresp   = rresp  ;
        *ptr.rlast   = rlast  ;
        *ptr.rvalid  = rvalid ;
    }
};

enum AXI4Resp {
    RESP_OKEY   = 0,
    RESP_EXOKEY = 1,
    RESP_SLVERR = 2,
    RESP_DECERR = 3
};

enum AXI4Burst {
    BURST_FIXED = 0,
    BURST_INCR  = 1,
    BURST_WRAP  = 2,
    BURST_RESERVED = 3
};

#endif
