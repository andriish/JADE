C   03/02/81 206081403  MEMBER NAME  ATOFPR   (S)           FORTRAN
      SUBROUTINE ATOFPR(IPTOF)
      IMPLICIT INTEGER*2 (H)
      COMMON/TFPED/ HADC(2,42),HTDC(2,42),HSTAT(42),HON(42)
      COMMON/FWPED/ HADCF(2,16),HTDCF(2,16),HFSTAT(16),HFON(16)
      COMMON/TFPED1/HTDC1(2,42),HON1(42),HTSPAR(16)
#include "chead.for"
      DATA IBUG /0/
C
      NRUN = HEADR(18)
      IF(NRUN.GT.10000)  CALL ATFPR2(IPTOF)
      IF(NRUN.GT.10000)  RETURN
C
      IBUG = IBUG + 1
      PRINT 300,IBUG
  300 FORMAT(//' NEW EVENT',I5/)
      CALL ATOFRW(IPTOF)
      PRINT 305,HON
      PRINT 305,HON1
      PRINT 305,HFON
  305 FORMAT(' HON',42I3)
      DO   1   I=1,42
      IF(I.LE.16.AND.
     *   HON(I).LE.0.AND.HFON(I).LE.0.AND.HON1(I).LE.0)  GOTO  1
      IF(I.GT.16.AND.
     *   HON(I).LE.0.AND.HON1(I).LE.0)  GOTO  1
      PRINT301,I,HADC(1,I),HADC(2,I),HTDC(1,I),HTDC(2,I)
  301 FORMAT(I5,4I6)
      IF(I.GT.16)  GOTO  2
      IF(HFON(I).LE.0)  GOTO 2
      PRINT302,I,HADCF(1,I),HADCF(2,I),HTDCF(1,I),HTDCF(2,I)
  302 FORMAT('+',40X,I5,4I6)
    2 IF(HON1(I).LE.0)  GOTO  1
      PRINT304,I,HTDC1(1,I),HTDC1(2,I)
  304 FORMAT('+',80X,I5,2I6)
  303 FORMAT(/)
    1 CONTINUE
C
      RETURN
      END
