
.PHONY: config u-boot rocket build clean

#-------------------------------------------------------------------------------
all: build
build: opensbi u-boot rocket
clean:
	rm -Rf *.json *.dts *.dtb *.sv
	cd ../opensbi && make clean
	cd ../u-boot && make clean
	cd bootrom && make clean

#-------------------------------------------------------------------------------
config:
	touch bootrom/bootrom-spl.img
	dtc RocketSystem.its > build.dtb
	sbt run

opensbi: config
	ls *.dtb | grep -m1 -v build.dtb | xargs -i ln -sf ../$(notdir $(PWD))/{} ../opensbi/fdt-1.dtb
	cd ../opensbi && make O=build CROSS_COMPILE=riscv64-unknown-linux-gnu- PLATFORM=generic FW_FDT_PATH=./fdt-1.dtb

u-boot: config
	ls *.dtb | grep -m1 -v build.dtb | xargs -i ln -sf ../$(notdir $(PWD))/{} ../u-boot/fdt-1.dtb
	ln -sf ../$(notdir $(PWD))/build.dtb ../u-boot/
	ln -sf ../opensbi/build/platform/generic/firmware/fw_dynamic.bin ../u-boot/
	cd ../u-boot && make CROSS_COMPILE=riscv64-unknown-linux-gnu- EXT_DTB=./build.dtb EXTRA_CFLAGS="-march=rv64gc -fPIC"

rocket:
	cd bootrom && make
	sbt 'run RocketSystem'
	sbt 'run RocketMed'

vl-sim:
	head -n-1 ./RocketSystem.sv > ../test/RocketSystem.sv
	cd ../test && make

vl-run:
	cd ../test && ./sim-run.sh

