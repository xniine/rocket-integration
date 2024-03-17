mkenvimage -r -s 0x20000 -p 0 -o env.bin env.txt
truncate -s 128k env.bin
