C   31/03/80 004142105  MEMBER NAME  RESHUF   (JADESR)      FORTRAN
      SUBROUTINE RESHUF
      IMPLICIT INTEGER*2 (H)
C---
C---     MOVES N50S BANK AROUND FROM END OF FIRST PIECE OF OVERFLOW
C---     EVENT TO END OF LAST PIECE.
C---                                   L.H. O'NEILL 31.03.80
C---
      COMMON/CWORK/NWORK,IWORK(10000)
      DIMENSION HWORK(20000)
      EQUIVALENCE(IWORK(1),HWORK(1))
      DIMENSION IN50(200)
      DATA N50N/4HN50S/
      DATA LN50/0/
C---
         DATA ICALL/0/
         ICALL=ICALL+1
C        WRITE(6,5000) ICALL,HWORK(18),HWORK(19),HWORK(23)
 5000    FORMAT(1H0,' RESHUF, ICALL, ETC. = ',4I6)
      IOVERF=HWORK(23)
      IF(IOVERF.EQ.16) GO TO 10
      IF(LN50.NE.0) RETURN
      LPNT=4
    1 CONTINUE
      NBN=IWORK(LPNT-3)
      LENGB=IWORK(LPNT)
      IF(LENGB.LT.1) RETURN
C        WRITE(6,5001) NBN,LPNT,LENGB,NWORK
 5001    FORMAT(1X,A4,3I6)
      IF(NBN.EQ.N50N) GO TO 2
      LPNT=LPNT+LENGB+4
      IF(LPNT.LT.NWORK) GO TO 1
      RETURN
    2 CONTINUE
      IF((LPNT+LENGB).NE.NWORK) RETURN
      LN50=4*(LENGB+4)
C        WRITE(6,5002) LN50
 5002    FORMAT(1X,' LOAD IN50. LN50 = ',I6)
      CALL MVCL(IN50(1),0,IWORK(LPNT-3),0,LN50)
      NWORK=NWORK-LENGB-4
      RETURN
   10 CONTINUE
      IF(LN50.LE.16) LN50=0
      IF(LN50.EQ.0) RETURN
C        WRITE(6,5003) LN50
 5003    FORMAT(1X,' LOAD IWORK. LN50 = ',I6)
      CALL MVCL(IWORK(NWORK+1),0,IN50(1),0,LN50)
      NWORK=NWORK+LN50/4
      LN50=0
      RETURN
      END
