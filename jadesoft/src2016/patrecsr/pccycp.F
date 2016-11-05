C   10/09/79 102191201  MEMBER NAME  PCCYCP   (PATRECSR)    FORTRAN
      SUBROUTINE PCCYCP
      IMPLICIT INTEGER*2(H)
C
C
C----------------------------------------------------------------------
C
C         --------------  SUBROUTINE PCCYCP  -----------------
C         --- G.F.PEARCE .. LAST UPDATE : 1200 ON 10/09/79 ---
C
C   SUBROUTINE TO DUMP THE PATTERN RECOGNITION COMMON /CCYCP/
C
C  CONTENTS OF /CCYCP/
C  -------------------
C
C  HPTSEC(I)   ..  POINTER TO FIRST I*2 WORD OF DATA FOR THE FIRST HIT
C              ..  OF CELL I IN THE /BCS/ COMMON.
C
C----------------------------------------------------------------------
C
C
#include "ccycp.for"
      PRINT11
 11   FORMAT(
     + 1X,47('-')/
     + ' DUMP OF PATTERN RECOGNITION POINTER ARRAY HPTSEC'/
     + 1X,47('-'))
      PRINT12,(HPTSEC(I),I=1,98)
 12   FORMAT( 4(1X,20I6/),1X,18I6 )
      RETURN
      END
