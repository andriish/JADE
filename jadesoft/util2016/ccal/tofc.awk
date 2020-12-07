#!/bin/awk -f
# 
# 28/04/00 Pedro Movilla Fernandez
#
# Utility to restore the original TOF counter numbers in the
# TOF counter calibration data banks "TOFC". In the present 
# ASCII files aupdat1 and bupdat0, these data were erroneously
# written out as floating point numbers with exponent E-78.
# The original integer number can be easily reconstructed 
# from the mantissa of the floating point number.
#
# E.g.: 0.000026E-78  0.392330E+02   means: 26  0.392330E+02
#
# usage: awk -f tofc.awk <old file> > <new file>
#

###
BEGIN{}

### Process lines containing the string "E-78"
/E-78/ {
       iline++
       #print iline " --- Line#"NR " #Fields="NF " Content:" $0
       for ( i=1; i<=NF; i++ ) 
	 {
	   if ($i ~ /E-78/) { icount++; gsub(/E-78/,"",$i); gsub(/0\.0*/,"",$i) }
	   $i = sprintf("%13s",$i)
	 }
       #printf "%36-s %s \n" ," ---> changed to:", $0
       $1 = ( " " $1 )
  } 

### Let the rest unchanged
!/E-78/

### Statistics
END{ 
  #printf "%+30s\n", "------------------------------"
  #printf "%+30s %s\n", "Processed calibration file:", FILENAME
  #printf "%+30s %d\n", "Number of total lines:", NR
  #printf "%+30s %d\n", "Number of processed lines:", iline
  #printf "%+30s %d\n", "Number of processed numbers:", icount
    }
