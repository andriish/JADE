#!/bin/bash
for d in $(ls -d */); do
cd $d 
sed -i "s/^     \*/     +/"  *.f
sed -i "s/^     \*/     +/"  *.F
sed -i "s/^     \*/     +/"  *.for
sed -i "s/^     \&/     +/"  *.f
sed -i "s/^     \&/     +/"  *.F
sed -i "s/^     \&/     +/"  *.for
sed -i "s/^     #/     +/"  *.f
sed -i "s/^     #/     +/"  *.F
sed -i "s/^     #/     +/"  *.for

sed -i "s/&/*/g" *.f
sed -i "s/&/*/g" *.F
sed -i "s/&/*/g" *.for


cd ..
done
