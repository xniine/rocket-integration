readelf --symbols ../opensbi/build/platform/generic/firmware/fw_dynamic.elf | sed 's/.*: //g' > sym.txt
cat ../u-boot/spl/u-boot-spl.sym ../u-boot/u-boot.sym >> ./sym.txt

