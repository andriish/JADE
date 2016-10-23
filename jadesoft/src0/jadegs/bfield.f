C   18/12/78 C9032101   MEMBER NAME  BFIELD   (JADEGS)      FORTRAN
      SUBROUTINE BFIELD
C---
C---     SETTING MAGNETIC FIELD EQUAL TO 0
C---
#include "cjdrch.for"
#include "cgeo1.for"
      DRIDEV = 0.
      DRICOS = 1.
      DRISIN = 0.
      BKGAUS = 0.
      RETURN
      END
