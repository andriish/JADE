C   20/10/81 110201206  MEMBER NAME  MUFFLR   (JADEMUS)     FORTRAN
C   29/03/81 106171607  MEMBER NAME  MUFFLR   (WORKS)       FORTRAN
C LAST CHANGE 16.10 17/06/81 JOHN ALLISON. (1ST ISSUED THIS DAY.)
      SUBROUTINE MUFFLR(HAMB)
C
      IMPLICIT INTEGER*2 (H)
C
      DIMENSION HAMB(1)
C
C RE-FITS MUON HITS.
C
C-----------------------------------------------------------------------
C
C HAMB SUPPLIES AMBIGUITY SOLUTIONS FROM MUFFLX.
C
C-----------------------------------------------------------------------
C
#include "cmufwork.for"
C
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
C
      RETURN
      END
