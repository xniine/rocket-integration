GCC=riscv64-unknown-elf-gcc
OBJCOPY=riscv64-unknown-elf-objcopy

.PHONY: clean all

all: bootrom.img bootrom.elf bootrom.bin
	cp bootrom.img bootrom-spl.img
	cat u-boot-spl.bin /dev/zero | dd bs=256 count=241 >> bootrom-spl.img

clean:
	rm -Rf bootrom.elf bootrom.bin bootrom.img bootrom-spl.img

%.img: %.bin
	dd if=$< of=$@ bs=256 count=1

%.bin: %.elf
	$(OBJCOPY) -O binary $< $@

%.elf: %.S linker.ld
	$(GCC) -Tlinker.ld $< -nostdlib -static -Wl,--no-gc-sections -o $@
