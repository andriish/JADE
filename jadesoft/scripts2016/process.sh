#!/bin/bash
cd main2016


for d in $(ls -d */); do
cd $d 
cp ../../main/$d/*.f ./
cp ../../main/$d/*.F ./
cp ../../main/$d/*.for ./
cp ../../main_patched/$d/*.f ./
cp ../../main_patched/$d/*.F ./
cp ../../main_patched/$d/*.for ./
cd ..
done


cd src2016


for d in $(ls -d */); do
cd $d 
cp ../../src/$d/*.f ./
cp ../../src/$d/*.F ./
cp ../../src/$d/*.for ./
cp ../../src_patched/$d/*.f ./
cp ../../src_patched/$d/*.F ./
cp ../../src_patched/$d/*.for ./


#sed -i "s/^     \*/     +/"  *.f
#sed -i "s/^     \*/     +/"  *.F
#sed -i "s/^     \*/     +/"  *.for
#sed -i "s/^     \&/     +/"  *.f
#sed -i "s/^     \&/     +/"  *.F
#sed -i "s/^     \&/     +/"  *.for
sed -i "s/^     #/     +/"  *.f *.F *.for
sed -i "s/&1/*1/g"   *.f *.F *.for
sed -i "s/&2/*2/g"   *.f *.F *.for
sed -i "s/&3/*3/g"   *.f *.F *.for
sed -i "s/&4/*4/g"   *.f *.F *.for
sed -i "s/&5/*5/g"   *.f *.F *.for
sed -i "s/&6/*6/g"   *.f *.F *.for
sed -i "s/&7/*7/g"   *.f *.F *.for
sed -i "s/&8/*8/g"   *.f *.F *.for
sed -i "s/&9/*9/g"   *.f *.F *.for



cd ..
done
