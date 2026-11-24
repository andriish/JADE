C   10/06/80 006140539  MEMBER NAME  LGESMR   (SOURCE)      FORTRAN
      SUBROUTINE LGESMR(ADATA)
C       PUT IN ENERGY SMEARING FOR EACH CLUSTER FOUND
C       WRITTEN BY Y.WATANABE   JUNE 10. 1980......
      DIMENSION ADATA(15)
      REAL*8 FNORM
C     EQUIVALENCE (IDATA(1),ADATA(1)),(ESH,ADATA(2)),(COST,ADATA(7))
C     The equivalence of the dummy argument was commented out, so that the
C     array IDATA was never set (zero) and every cluster was smeared like
C     one of the barrel: the first word of the cluster, the part of the
C     calorimeter (0 barrel, +1 and -1 end caps), is taken as an integer.
      INTEGER JBC
      JBC=TRANSFER(ADATA(1),JBC)
C
      DATA IC/0/
      IC=IC+1
      IF(IC.LT.5) WRITE(6,600)
600   FORMAT(2X,5(1H%),' E SMEAR IS CALLED',5(1H%))
C
      ESH=ADATA(2)
C     a cluster without energy is not smeared (SQRT(0.0036/ESH) is infinite)
      IF(.NOT.(ESH.GT.0.)) GO TO 90
      IF(IABS(JBC)-1) 10,30,90
10    CONTINUE
C     BARREL PART
      IF(ESH.GT.6.) GO TO 20
      SIG=SQRT(0.0036/ESH+0.001225)
      IF(SIG.LT.0.055) SIG=0.055
      GO TO 40
20    SIG=0.055-(ABS(ADATA(7))-0.4)*(ESH-6.)/144.
      IF(SIG.LT.0.04) SIG=0.04
      GO TO 40
C
C     END CAP
30    CONTINUE
      SIG=0.28/SQRT(ESH)
      IF(SIG.LT.0.12) SIG=0.12
C
40    ESH=ESH*(1.+FNORM(0.)*SIG)
      IF(ESH.LT.0.) ESH=0.
      ADATA(2)=ESH
90    RETURN
      END
