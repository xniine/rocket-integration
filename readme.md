### Integration work for rocket-chip and linux on xilinx fpga

Running in ubuntu 20.04

- rocket - scala modules to compile the rocket-chip into verilog file
- rocket-chip - rocket-chip main code
- rocket-chip-blocks - rocket-chip peripheral devices developed by sifive
- test - source code to run rocket chip with verilator and simulated ddr and qspi
- u-boot - boot loader to embeded in bootROM and (later) start the linux
- opensbi - firmware required to boot linux
- busybox - source code for busybox ramdisk as rootfs
- tftpboot - tftp root folder to transfer the linux fit image (for u-boot netowrk boot)

### To Build:
  ```
  git submodule update --init --recursive
  
  #### install system dependencies 
  
  apt-get install build-essential 
  apt-get install device-tree-compiler
  apt-get install gawk texiinfo libgmp-dev
  apt-get install swig libssl-dev
  apt-get install help2man
  apt-get install u-boot-tools
  
  #### Prepare the toolchain for corss build
  cd toolchain; make all

  #### Prepare linux kernel
  wget https://cdn.kernel.org/pub/linux/kernel/v6.x/linux-6.2.tar.gz
  tar -xzf linux-6.2.tar.gz ; mv linux-6.2 linux

  #### Prepare ENV variables & patch the source code
  source toolchain/env.txt
  bash patch.txt

  #### Build rocket-chip, rocket-chip-blocks into JAR file
  cd rocket-chip; make verilog; cd -
  cd rocket-chip-blocks; sbt package; cd -
  cd rocket-chip-inclusive-cache; sbt package; cd -

  #### generate rocket-chip verilog
  cd rocket; make clean all; cd -

  #### Compile linux kernel & busybox
  cd linux
  make ARCH=riscv CROSS_COMPILE=riscv64-unknown-linux-gnu- menuconfig
  make ARCH=riscv CROSS_COMPILE=riscv64-unknown-linux-gnu- EXTRA_CFLAGS="-march=rv64imac_zicsr_zifencei -mabi=lp64"
  cd -

  cd busybox
  make menuconfig
  make install
  cd -

  #### Generate the FIT image to network boot the linux
  cd tftpboot; make clean all
  
  #### Start dnsmasq
  dnsmasq -d -C dhcp.conf -i enxa0cec8f6bbb8 # <- replace with the ifname of your own nic 
  ```
  The verilog file RocketMed.sv along with the device-tree will be generated in rocket folder.

  Copy and import these file into your vivado project.

