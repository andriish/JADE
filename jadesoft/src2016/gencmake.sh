#!/bin/bash
for d in $(ls -d */); do
cd $d 
dir=$(basename $(pwd))
cat ../../GNUmake-objects | grep -A 50 "OBJS_"$dir | grep -B 50   "  "  | tr -d '\' | tr -s ' ' | tr -d '\t' | tr ' ' '\n' | grep o | sort >1
rm -f CMakeLists.txt
LB=$(echo $dir | tr  '[:lower:]' '[:upper:]')
echo "SET("$LB"_src" >> CMakeLists.txt
for a in $(cat 1); do
a=$(echo $a | sed 's@\.o@@g')
if [ -f $a".F" ]; then 
echo '${SRC'$LB"}/"$a".F" >> CMakeLists.txt
fi
if [ -f $a".f" ]; then 
echo '${SRC'$LB"}/"$a".f" >> CMakeLists.txt
fi
if [ -f $a".for" ]; then 
echo '${SRC'$LB"}/"$a".for" >> CMakeLists.txt
fi
done

echo ")" >> CMakeLists.txt
echo 'ADD_LIBRARY(${'$LB'LIB} STATIC ${'$LB'_src})'>> CMakeLists.txt
cd ..
done
