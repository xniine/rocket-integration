### Integration work for rocket-chip (riscv-64) and linux on xilinx fpga

Running in ubuntu 20.04

> - rocket - scala modules to compile the rocket-chip into verilog file
> - rocket-chip - rocket-chip main code
> - rocket-chip-blocks - rocket-chip peripheral devices developed by sifive
> - test - source code to run rocket chip with verilator and simulated ddr and qspi
> - u-boot - boot loader to embeded in bootROM and (in later phase) start the linux
> - opensbi - firmware required to boot linux
> - busybox - source code for busybox ramdisk as rootfs
> - tftpboot - tftp root folder to transfer the linux fit image (for u-boot netowrk boot)
> - mku3pa - vivado project for rocket-chip running in fpga

### To Build:
#### 1) Prepare the depdencies and repository
  ```
  git submodule update --init --recursive
  
  #### install system dependencies 
  
  apt-get install build-essential 
  apt-get install device-tree-compiler
  apt-get install gawk texiinfo libgmp-dev
  apt-get install swig libssl-dev
  apt-get install help2man
  apt-get install u-boot-tools
  apt-get install genext2fs
  ```
#### 2) Build with `bash ./build.txt` or step-by-step as below
  ```
  #### Prepare the toolchain for corss build
  cd toolchain; make all

  #### Prepare linux kernel
  test -e ./linux-6.5.tar.gz || wget https://cdn.kernel.org/pub/linux/kernel/v6.x/linux-6.5.tar.gz
  test -e ./linux || (tar -xzf linux-6.5.tar.gz && mv linux-6.5 linux)

  #### Prepare ENV variables & patch the source code
  test -e ./linux/.config || bash patch.txt
  source toolchain/env.txt

  #### Build rocket-chip, rocket-chip-blocks into JAR file
  cd rocket-chip; make verilog; cd -
  cd rocket-chip-blocks; sbt package; cd -
  cd rocket-chip-inclusive-cache; sbt package; cd -

  #### generate rocket-chip verilog
  cd rocket; make clean build vl-sim; cd -

  #### Compile linux kernel, busybox & FIT Image
  cd linux
  make ARCH=riscv CROSS_COMPILE=riscv64-unknown-linux-gnu- EXTRA_CFLAGS="-march=rv64imac_zicsr_zifencei -mabi=lp64"
  cd -

  cd busybox
  make install
  cd -

  cd tftpboot
  make clean all
  cd -
  ```

#### 3) Copy the outputs and copmile the code in vivado
  ```
  #### Copy output verilog file into vivado project
  head -n-1 ./rocket/RocketMed.sv > mku3pa/imports/RocketSystem.sv
  # <use vivado to generate bitstream, write into on-board qspi-flash>
  ```

  The verilog file RocketMed.sv along with the device-tree will be generated in rocket folder.  
  Copy and import these file into your vivado project.
  ![](/doc/rocket-utilization.png?raw=true)

#### 4) To test with qemu or verilator use the commands below
  ```
  #### qemu-system-riscv64
  cd tftpboot; bash ./qemu.txt

  #### verilator
  cd test; ./sym-out.sh; ./sim-run.sh
  ```
#### 5) To boot rocket-chip from fpga board
  the vivado project packed in the repo reuses Xilinx Jtag-UART from Xilinx MDM IP

  ##### a) You can start the Terminal console in xsdb as below
  ```
  xsdb% conn
  tcfchan#0
  xsdb% targets
    1  xcku3p
       2* MicroBlaze Debug Module at USER2
       3  Legacy Debug Hub
  xsdb% target 2
  xsdb% jtagterminal -start
  ```
  ![](/doc/rocket-jtag-uart.png?raw=true)

  ##### b) Start vivado generate and flash the bootrom into qspi-flash
  Write the generate uboot.fit (fit image for u-boot) at 0x1000000
  ![](/doc/rocket-gen-mcs.png?raw=true)

  - To network boot the linux, a mac address should be set as below
  ```
  env set ethaddr 02:00:11:22:33:49
  saveenv
  ```

  ##### c) Start dnsmasq and boot from tftp server: (in my case, I have only 32M nor flash, no space for linux fit image)
  ```
  dnsmasq -d -C dhcp.conf --tftp-root=$PWD -i eth0 # <- replace eth0 with the ifname of your own nic 
  ```
  ![](/doc/rocket-boot.png?raw=true)

  Also, you can check the doc/rocket-boot.log for the boot log in my system


