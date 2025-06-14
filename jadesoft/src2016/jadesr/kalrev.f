C   20/02/80 007100850  MEMBER NAME  KALREV   (JADESR)      FORTRAN
      SUBROUTINE KALREV
      IMPLICIT INTEGER*2 (H)
C---
C---     COPIES BACK FROM WORKING FILE TO AUPDAT...
C---     ELIMINATES DUPLICATE LGST BANKS.
C---                                   L.H. O'NEILL 20.02.80
C---
      DIMENSION JBUF(10000),IDAT(9991),HBUF(20000),ITIME(6),NAME( 7)
      EQUIVALENCE(JBUF(1),HBUF(1))
      EQUIVALENCE(JBUF(10),IDAT(1))
      DATA NLGST/4HLGST/
C---
      ISECLF=10
      ITOK=1
      IF(IUHR(ISECLF).NE.1) ITOK=0
      IF(ITOK.EQ.1) WRITE(6,401)
      IF(ITOK.NE.1) WRITE(6,402)
  401 FORMAT(1X,' TIME OK GOING INTO KALREV.')
  402 FORMAT(1X,' TIME ALMOST OUT GOING INTO KALREV.')
    3 CONTINUE
      IF(ITOK.GT.-20) WRITE(6,403)
  403 FORMAT(1X,' THREE LOOP.')
      ITOK=ITOK-1
      IF(ITOK.LT.0) GO TO 3
      REWIND 2
      REWIND 3
      NREC=0
      LGTIME=-1
    1 CONTINUE
      CALL EVREA1(3,NWORD,JBUF,IRET)
      IF(IRET.EQ.1) GO TO 1000
      IF(IRET.EQ.2) GO TO 2000
      NREC=NREC+1
C     IOUT=NWORD
C     IF(IOUT.GT.10) IOUT=10
C     WRITE(6,100) NREC,NWORD,(JBUF(I),I=1,IOUT)
  100 FORMAT(1X,I4,I12,10I10)
      IF(JBUF(1).NE.NLGST) GO TO 2
      IF(JBUF(6).EQ.LGTIME) GO TO 1
      LGTIME=JBUF(6)
    2 CONTINUE
      CALL KALBNK(2,NWORD,JBUF)
      GO TO 1
 1000 CONTINUE
      WRITE(6,101) NREC
  101 FORMAT(' *** READ ERROR ENCOUNTERED AFTER ',I4,' RECORDS.')
      RETURN
 2000 CONTINUE
      WRITE(6,102) NREC
  102 FORMAT('0NORMAL END OF FILE AFTER',I4,' RECORDS.')
      RETURN
      END
