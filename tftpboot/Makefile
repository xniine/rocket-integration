
.PHONY: all clean

SYSROOT = ../toolchain/riscv/sysroot

all: ramdisk
	cp ../u-boot/fdt-1.dtb ./
	cp ../linux/arch/riscv/boot/Image ./
	mkimage -f kernel.its kernel.itb
	mkimage -f boot.fit boot.scr.uimg
ramdisk:
	rsync -avh ../busybox/_install ./
	bash ramdisk.txt
	rsync -avh $(SYSROOT)/lib64/lp64/libc.so.6 ./_install/lib
	mv ./ramdisk.image.gz initrd.gz
clean:
	rm -Rf fdt-1.dtb
	rm -Rf Image
	rm -Rf _install initrd.gz
	rm -Rf kernel.itb
	rm -Rf boot.scr.uimg

