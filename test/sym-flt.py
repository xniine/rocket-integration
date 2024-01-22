#!/usr/bin/python3

import sys
import re

def sym_search(ptr, ptr_list):
    beg = 0
    mid = 0
    end = len(ptr_list)
    fin = len(ptr_list) - 1
    while beg + 1 < end:
        mid = int((beg + end) / 2);
        val = ptr_list[mid]
        if   ptr > val: beg = mid
        elif ptr < val: end = mid
        else: break
    mid = int((beg + end) / 2);
    return mid
    
def sym_filter(ptr, ptr_list, txt_list):
    idx = sym_search(ptr, ptr_list)
    if idx < 0: return ""
    return txt_list[idx]

def main():
    sym_file = sys.argv[1]

    sym_list = list()
    with open(sym_file, "r") as fd:
        for line in fd:
            mat = re.match("^ *([0-9a-fA-F]+) .* ([^ ]+)$", line)
            if not mat: continue
            sym_list.append((int(mat.group(1),16), mat.group(2).strip()))

    sym_list = sorted(sym_list, key=lambda x: x[0])
    ptr_list = [ x[0] for x in sym_list ]
    txt_list = [ x[1] for x in sym_list ]

    for line in sys.stdin:
        txt = line.strip()
        mat = re.match("^[0-9]+ (0x[0-9a-fA-F]+) .*", line)
        if mat:
            ptr = int(mat.group(1), 16)
            txt = txt + " " * max(0, 60 - len(txt))
            txt = txt + " # " + sym_filter(ptr, ptr_list, txt_list)
        mat = re.match("^.* pc=\[([0-9a-fA-F]+)\] .*", line)
        if mat:
            ptr = int(mat.group(1), 16)
            txt = txt + " # " + sym_filter(ptr, ptr_list, txt_list)
        print(txt)

if __name__ == "__main__":
    try: sys.exit(main())
    except: pass

