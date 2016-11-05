C   19/02/84 807251712  MEMBER NAME  EVBKPR   (JADEGS)      FORTRAN
C
C----------------------------------------------------------------------
      SUBROUTINE EVBKPR(IRET)
C----------------------------------------------------------------------
C
C   AUTHOR:   L. O'NEILL     ?     :  PRINT LIST OF BOS BANK NAMES/NOS.
C
C        MOD: C. BOWDERY  16/02/84 :  CREATE THIS AS SEPARATE S/R
C   LAST MOD: J. HAGEMANN 04/03/88 :  PRINT ALSO POINTER OF BANK
C
C----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
C
      DIMENSION NALIST(200)
C
      DATA NLMAX / 200 /
C
C------------------  C O D E  -----------------------------------------
C
      IRET = 1
      CALL BSLT
      CALL BMRT(NL,NALIST,NLMAX)
      IF(NL.LE.0) RETURN
C
      IRET = 0
      DO  1  INL = 1,NL
        LNAM   = NALIST(INL)
        IPLNAM = IDATA(IBLN(LNAM))
C
C                            BANK NAME IN LIST BUT NO BANKS EXIST NOW
C
        IF( IPLNAM .LE. 0 ) GO TO 1
C
  4     WRITE(6,3) LNAM, IDATA( IPLNAM - 2), IDATA(IPLNAM), IPLNAM
  3     FORMAT('  NAME:  ',A4,'    BOS NR: ',I3,'    LENGTH: ',I6,
     &         '   POINTER: ',I6)
C
        IPLNAM = IDATA(IPLNAM-1)
        IF( IPLNAM .GT. 0 ) GO TO 4
  1   CONTINUE
C
      RETURN
      END
