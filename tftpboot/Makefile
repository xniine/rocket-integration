
.PHONY: all clean

all:
	cp ../u-boot/fdt-1.dtb ./
	cp ../linux/arch/riscv/boot/Image ./
	cp ../busybox/initramfs.cpio.gz ./
	mkimage -f kernel.its kernel.itb
	mkimage -f boot.fit boot.scr.uimg
clean:
	rm -Rf fdt-1.dtb
	rm -Rf initramfs.cpio.gz
	rm -Rf Image
	rm -Rf kernel.itb
	rm -Rf boot.scr.uimg

