#define DRAM_BASE 0x80000000
#define TEXT_BASE 0x850F0F00 // TXT = 0x85100000 (DTB) - 0xF100 (60K + 256)

.section .text.start, "ax", @progbits
.globl _start
_start:
  csrwi 0x7c1, 0 // disable chicken bits
  li s0, DRAM_BASE
  csrr a0, mhartid
  la a1, _dtb
  j 2f // jr s0

.section .text.hang, "ax", @progbits
.globl _hang
_hang:
  csrwi 0x7c1, 0 // disable chicken bits
  csrr a0, mhartid
  la a1, _dtb
  j 2f // csrwi mie, 0
1:
  wfi
  j 1b
  //////////////////////////////////////////////////////////////////////////////
  // Load SPL
  //////////////////////////////////////////////////////////////////////////////
2:
  li t0, TEXT_BASE
  la t1, _spl
  la t2, _start + 0x10000
3:
  lwu t3, (t1)
  sw t3, (t0)
  addi t0, t0, 4
  addi t1, t1, 4
  bltu t1, t2, 3b
4:
  mv t2, t0
  li t1, TEXT_BASE
5:
  lwu t3, (t1)
  addi t1, t1, 4
  bltu t1, t2, 5b
6:
  li a0, TEXT_BASE
  la a1, _dtb
  la t0, _spl
  sub a1, a1, t0
  add a1, a1, a0
  jr a0

.section .text.spl, "ax", @progbits
.globl _spl
_spl:
  j _hang

.section .rodata.dtb, "a", @progbits
.globl _dtb
.align 5, 0
_dtb:
.ascii "DTB goes here"

