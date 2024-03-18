
.PHONY: clean riscv-gnu-toolchain verilator firtool mill sbt

all: sbt mill firtool riscv-gnu-toolchain 

riscv-gnu-toolchain:
	cd riscv-gnu-toolchain; ./configure --prefix=$(PWD)/riscv --enable-multilib && make && make linux

verilator:
	cd verilator; autoconf; ./configure --prefix=$(PWD); make

firtool:
	test -e circt-bin-ubuntu-20.04.tar.gz || wget https://github.com/llvm/circt/releases/download/firtool-1.30.0/circt-bin-ubuntu-20.04.tar.gz
	mkdir -p firtool
	tar -C firtool -xzf circt-bin-ubuntu-20.04.tar.gz

mill:
	test -e mill || wget https://github.com/com-lihaoyi/mill/releases/download/0.11.6/0.11.6-assembly -O mill
	chmod 755 ./mill

sbt:
	test -e sbt-1.9.7.tgz || wget https://github.com/sbt/sbt/releases/download/v1.9.7/sbt-1.9.7.tgz
	tar -xzf sbt-1.9.7.tgz 

clean:
	cd riscv-gnu-toolchain; make clean || exit 0
	rm -Rf firtool sbt

