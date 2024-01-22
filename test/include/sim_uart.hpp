
#ifndef SIM_UART_H
#define SIM_UART_H

#include "verilated.h"
#include <iostream>

#include <sys/ioctl.h>
#include <stdio.h>
#include <termios.h>
#include <unistd.h>

int _kbhit() {
    static const int STDIN = 0;
    static bool init = false;
    if (!init) {
        termios term;
        tcgetattr(STDIN, &term);
        term.c_lflag &= ~ICANON;
        tcsetattr(STDIN, TCSANOW, &term);
        setbuf(stdin, NULL);
        init = true;
    }
    int retn;
    ioctl(STDIN, FIONREAD, &retn);
    return retn;
}

class SimUART {
    int  clk_div;
    int  txd_cnt;
    int  txd_idx;
    char txd_val;
    bool txd_act;
    bool txd_out;

    int  rxd_cnt;
    int  rxd_idx;
    char rxd_val;
    bool rxd_act;
    bool rxd_out;
    bool rxd_pry;

public:
    VL_OUT8(*txd, 0, 0);
    VL_IN8 (*rxd, 0, 0);

    SimUART(int div) {
        this->clk_div = div;
        this->txd_act = false;
        this->txd_val = 0;
        this->txd_cnt = 0;
        this->txd_idx = 0;
        this->txd_out = false;
    }

    void output() {
        if (!rxd_act) {
            *this->rxd = 1;
            if (_kbhit()) { 
                rxd_val = getchar();
                rxd_cnt = clk_div;
                rxd_idx = 0;
            }
        } else 
        if (rxd_act && !rxd_cnt) {
            rxd_cnt = clk_div;
            rxd_idx = rxd_idx + 1;
            switch (rxd_idx) {
                case 0: {
                    *this->rxd = 0;
                    rxd_pry = false;
                }
                case 1:
                case 2:
                case 3:
                case 4:
                case 5:
                case 6:
                case 7:
                case 8: {
                    *this->rxd = rxd_val & 0x1;
                    rxd_pry = rxd_pry ^ (rxd_val & 0x1);
                    rxd_val = rxd_val >> 1;
                }
                case 9: {
                    *this->rxd = rxd_pry;
                }
                default: {
                    *this->rxd = 1;
                    rxd_act = false;
                }
            }
        }
        rxd_out = *this->rxd;
    }

    void update() {
        if (txd_act) {
            if (txd_cnt) {
                // 2) Clock Counting
                txd_cnt = txd_cnt - 1;
            } else
            if (txd_idx < 8) {
                // 3) Pick Bit 0-7, Save to "txd_val"
                txd_val = txd_val | (*this->txd << txd_idx);
                txd_cnt = clk_div;
                txd_idx = txd_idx + 1;
                txd_out = txd;
            } else {
                // 4) Whole Byte Received, mark "tx_act" to false
                if (txd_val == 0x0a) std::cout << std::endl;
                if (txd_val != 0x0d && txd_val != 0x0a) {
                    std::cout << txd_val;
                }
                txd_act = false;
                txd_val = 0;
                txd_cnt = clk_div;
                txd_idx = 0;
                txd_out = *this->txd;
            }
        } else {
            if (txd_out && !*this->txd) {
                // 1) UART Start
                txd_act = true;
                txd_val = 0;
                txd_cnt = clk_div + (clk_div / 2);
                txd_idx = 0;
                txd_out = false;
            } else
            if (txd_cnt) {
                // 5) Clock Counting for IDLE State
                txd_cnt = txd_cnt - 1;
            } else {
                // 6) IDLE Waiting, "txd" -> "txd_out" for start condition
                txd_act = false;
                txd_val = 0;
                txd_cnt = clk_div;
                txd_idx = 0;
                txd_out = *this->txd;
            }
        }
    }
};

#endif

