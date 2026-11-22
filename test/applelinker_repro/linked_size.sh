#!/bin/sh
# Print the size of the linked symbol xx (C) or xx_ (Fortran) of the executable $1.
# macOS: size of the __common/__bss section of the executable; Linux: nm -S.
exe=$1
if [ "$(uname)" = "Darwin" ]; then
  nm -n -m "$exe" | grep -E ' _xx_?$' ; 
  size -m "$exe" | grep -E 'Section __(common|bss):'
else
  nm -S "$exe" | grep -E ' [BbCc] xx_?$'
fi
