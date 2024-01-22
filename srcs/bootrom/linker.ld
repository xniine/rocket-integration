SECTIONS
{
    ROM_BASE = 0x80000000; /* ... but actually position independent */

    . = ROM_BASE;
    .text.start : { *(.text.start) }
    . = ROM_BASE + 0x40;
    .text.hang : { *(.text.hang) }
    . = ROM_BASE + 0x100; /* 256B */
    .text.spl : { *(.text.spl) }
    . = ROM_BASE + 0xF200; /* 60K + 512B */
    .rodata.dtb : { *(.rodata.dtb) }
}
