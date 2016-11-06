#!/bin/bash
rm -f scripts2016/patch.txt
cd src_patched


for d in $(ls -d */); do
cd $d 
for a in $(ls -1   *.f *.F *.for); do
diff -u $a ../../src/$d/$a >> ../../scripts2016/patch.txt 
done
cd ..
done
