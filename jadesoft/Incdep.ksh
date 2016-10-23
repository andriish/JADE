#!/bin/ksh
#
# Shell script tool for extracting "#include-file" dependencies
# of FORTRAN files within the current directory.
# This routine is called by the Makefile for the JADE library.
#
# Nov/1999  P.A. Movilla Fernandez
#
# Arguments:
# ${1}     : The name of the library corresponding to the examined directory
# $(2),... : List of *.F files in the source library directory
#
log=Incdep.log
# check arguments
if [ ${#*} -lt 2 ]; then
 echo "GNUmake-Incdep: too few arguments" > $log
 exit
fi
#
Lib=${1}
echo "Current directory is"`pwd` > $log

# remove first argument
shift 1

# assign include files with the corresponding FORTRAN files
set -A FFiles `grep "#include" ${1} ${*}| cut -d : -f1`
set -A IFiles `grep "#include" ${1} ${*}| cut -d \" -f2`

# list of include files (alphabetically order)
set -A InclFiles `grep "#include" ${*}| cut -d \" -f2| sort|uniq`

# dependencies
echo ""              >> $log
echo "Dependencies:" >> $log
echo "-------------" >> $log
let i=0
while [ $i -lt ${#IFiles[@]} ]
do
    echo ${FFiles[$i]} '...' ${IFiles[$i]} >> $log
    let i=i+1
done

# FORTRAN files sorted by the include dependencies
echo ""                                  >> $log
echo ".F files sorted by include-files:" >> $log
echo "-- ----- ------ -- --------------" >> $log

let i=0
for ifile in ${InclFiles[@]}
do
    let k=0
    savefile="x"
    while [ $k -lt ${#FFiles[@]} ]
    do
	if [ ${IFiles[$k]} = $ifile ] 
	then
	    if [ $savefile !=  ${FFiles[$k]} ]
	    then
		cmd="Dep$i=\$Dep$i' '\${FFiles[$k]}"; eval $cmd
		oFiles=`echo ${FFiles[$k]}|sed 's/\.F/\.o/g'`
		cmd="LibDep$i=\$LibDep$i' '\${oFiles}"; eval $cmd
		savefile=${FFiles[$k]}
	    fi
	fi
	let k=k+1
    done
    cmd="echo $ifile: \${Dep$i}"; eval $cmd >> $log
    let i=i+1
done

# Compose `make' rules for #include dependencies
echo ""                                        >> $log
echo "Library members sorted by include-files:" >> $log
echo "------- ------- ------ -- -------------" >> $log

let i=0
for ifile in ${InclFiles[@]}
do
    cmd="echo ${Lib}'(' \${LibDep$i}' ) :\t '$ifile"; eval $cmd >> $log
    cmd="echo ${Lib}'(' \${LibDep$i}' ) :\t '$ifile"; eval $cmd
    let i=i+1
done
#
