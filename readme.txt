
#-------------------------------------------------------------------------------
# Chipyard

mkdir -p miniconda3
wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O miniconda.sh
bash miniconda.sh -b -u -p miniconda3
 
./miniconda3/bin/conda --version
./miniconda3/bin/conda update conda
./miniconda3/bin/conda config --add channels conda-forge
./miniconda3/bin/conda config --set channel_priority strict
./miniconda3/bin/conda config --add channels https://mirrors.tuna.tsinghua.edu.cn/anaconda/pkgs/free/
./miniconda3/bin/conda config --add channels https://mirrors.tuna.tsinghua.edu.cn/anaconda/pkgs/main/
./miniconda3/bin/conda config --set show_channel_urls yes

./miniconda3/bin/conda install -n base conda-lock=1.4
source ./miniconda3/bin/activate base

pushd chipyard
./scripts/init-submodules-no-riscv-tools-nolog.sh 
./build-setup.sh -s 6 -s 7 -s 8 -s 9 riscv-tools
source ./env.sh 
popd sims/verilator/
make
popd
popd

#-------------------------------------------------------------------------------
# Mill

wget https://github.com/com-lihaoyi/mill/releases/download/0.11.6/0.11.6-assembly -O mill

#-------------------------------------------------------------------------------
# Sbt

wget https://github.com/sbt/sbt/releases/download/v1.9.7/sbt-1.9.7.tgz
tar -xzf sbt-1.9.7.tgz 

#-------------------------------------------------------------------------------
# Firtool

wget https://github.com/llvm/circt/releases/download/firtool-1.30.0/circt-bin-ubuntu-20.04.tar.gz
tar -xzf circt-bin-ubuntu-20.04.tar.gz
mkdir -p firtool
tar -C firtool -xzf circt-bin-ubuntu-20.04.tar.gz

#-------------------------------------------------------------------------------
# Rocket-chip

pushd rocket-chip
apt-get install device-tree-compiler
make verilog
popd

#-------------------------------------------------------------------------------
# SiFive (sifive-blocks/sifive-cache)

git clone https://github.com/chipsalliance/rocket-chip-blocks.git
git clone https://github.com/chipsalliance/rocket-chip-inclusive-cache.git 

pushd rocket-chip-blocks

cat << EOF > build.sbt
scalaVersion := "2.13.10"
val chiselVersion = "5.1.0"
addCompilerPlugin("org.chipsalliance" % "chisel-plugin" % chiselVersion cross CrossVersion.full)
libraryDependencies += "org.chipsalliance" %% "chisel" % chiselVersion

val rocketChipOut = Glob("../rocket-chip/out/rocketchip/*/assembly.dest/out.jar")
Compile / unmanagedJars += file(fileTreeView.value.list(rocketChipOut).map(_._1.toString).head)
EOF

sbt package
popd

#-------------------------------------------------------------------------------
pushd rocket-chip-inclusive-cache

cat << EOF > build.sbt
scalaVersion := "2.13.10"
val chiselVersion = "5.1.0"
addCompilerPlugin("org.chipsalliance" % "chisel-plugin" % chiselVersion cross CrossVersion.full)
libraryDependencies += "org.chipsalliance" %% "chisel" % chiselVersion

val rocketChipOut = Glob("../rocket-chip/out/rocketchip/*/assembly.dest/out.jar")
Compile / unmanagedJars += file(fileTreeView.value.list(rocketChipOut).map(_._1.toString).head)

Compile / scalaSource := baseDirectory.value / "design/craft/inclusivecache/src"
EOF

sbt package
popd 

#-------------------------------------------------------------------------------
# Risc-V GNU Toolchain

apt-get install gawk texiinfo libgmp-dev

pushd riscv-gnu-toolchain
./configure --prefix=$PWD/../riscv
make
make linux
popd

#-------------------------------------------------------------------------------
# Risc-V Tests

pushd riscv-tests
./configure --prefix=$PWD/../riscv/riscv64-unknown-elf
make
make install
popd


#-------------------------------------------------------------------------------
# OpenSBI

pushd opensbi
git checkout v1.3.1
ls ../srcs/*.dtb | grep -v build.dtb -m1 | xargs -i ln -sf {} ./fdt-1.dtb
make clean
make O=build CROSS_COMPILE=riscv64-unknown-linux-gnu- PLATFORM=generic FW_FDT_PATH=fdt-1.dtb
popd

#-------------------------------------------------------------------------------
# U-Boot

pushd u-boot
apt-get install swig libssl-dev
git checkout v2023.10

head -n-1 ../srcs/*.dts > ./rocket.dts
echo -e '\tbinman: binman {\n\t};\n};' >> ./rocket.dts
dtc ./rocket.dts -o rocket.dtb

cp ../u-boot.config .config
# export CROSS_COMPILE=riscv64-unknown-linux-gnu- 
# make sifive_unleashed_defconfig
# make menuconfig

make CROSS_COMPILE=riscv64-unknown-linux-gnu- EXT_DTB=./build.dtb \
EXTRA_CFLAGS="-march=rv64gczicsr_zifencei -fPIC -DLOG_DEBUG"

popd

#-------------------------------------------------------------------------------
# Linux

pushd linux
make ARCH=riscv CROSS_COMPILE=riscv64-unknown-linux-gnu- menuconfig
make ARCH=riscv CROSS_COMPILE=riscv64-unknown-linux-gnu- all
popd

#-------------------------------------------------------------------------------
# Verilator

pushd verilator
apt-get install help2man
autoconf
./configure --prefix=$PWD
make
make install
popd

#-------------------------------------------------------------------------------
# tftpboot
apt-get install u-boot-tools

nmcli d set enxa0cec8f6bbb8 managed off
ip link set enxa0cec8f6bbb8 up
ip addr add enxa0cec8f6bbb8 10.0.0.1/24

dnsmasq -d -C dhcp.conf -i enxa0cec8f6bbb8

