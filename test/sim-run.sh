#!/bin/bash

./obj_dir/VRocketSystem 2> >(./sym-flt.py ./sym.txt > sim.log)

