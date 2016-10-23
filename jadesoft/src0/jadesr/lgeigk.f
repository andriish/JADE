C   17/03/79 202121639  MEMBER NAME  LGEIGK   (SOURCE)      FORTRAN
      SUBROUTINE LGEIGK(PARA)
C
C     R.EICHLER   7-11-78 17:00
C     LAST MODIFICATION 18-03-79 11:35   BY S.YAMADA,/CLGDMS/ IS ADDED.
C     LAST IMPROVEMENT  13-11-79 11:15   BY S.KOMAMIYA
C                                           COPIED FROM LGEIGN
C   CALCULATE AVPH,SIGPH,AVZ,SIGZ,SIGZPH
C
      IMPLICIT INTEGER *2 (H)
C
      COMMON /CLGDMS/ X0,RADIUS(6),RADSX0(6),THX0(4),
     $                ZEND(2),ZENDX0(2),ZWID(2),ZGAP(2),PHWID(2),
     $                ZECAP(4),ZECAPX(4),THECPX(2)
      COMMON/LGSHP/WW(50),ZZ(50),XX(50),EW(2),AVPH,AVZ,SIGPH,SIGZ,EV,KK
C-----------------------------------------------------------------------
      COMMON /EIGVEC/EIGV11,EIGV12,EIGV21,EIGV22
C-----------------------------------------------------------------------
      EW(1)=0.
      EW(2)=0.
      ZSUM=0.
      PHSUM=0.
      Z2SUM=0.
      PH2SUM=0.
      ZPHSUM=0.
      IMAX = KK
      IF(IMAX.GT.50) IMAX = 50
C-----------------------------------------------------------------------
      DO 10 I=1,IMAX
      ZSUM=ZSUM+ZZ(I)
      PHSUM=PHSUM+XX(I)
      Z2SUM=Z2SUM+ZZ(I)**2
      PH2SUM=PH2SUM+XX(I)**2
      ZPHSUM=ZPHSUM+ZZ(I)*XX(I)
   10 CONTINUE
C-----------------------------------------------------------------------
      AI=FLOAT(IMAX)
      AVZ=ZSUM/AI
      AVPH=PHSUM/AI
      AVZ2=Z2SUM/AI
      AVPH2=PH2SUM/AI
      AVZPH=ZPHSUM/AI
      VARZ=AVZ2-AVZ**2
      VARPH=AVPH2-AVPH**2
      CORZPH=AVZPH-AVZ*AVPH
      SIGZ=SQRT(ABS(VARZ))
      SIGPH=SQRT(ABS(VARPH))
      SIGZPH=SQRT(ABS(CORZPH))
C-----------------------------------------------------------------------
C---- CALCULATE EIGENVALUES OF MATRIX    VARZ   CORZPH
C                                        CORZPH VARPH
C
      F=(VARZ-VARPH)**2+4.*CORZPH**2
      B=VARZ+VARPH
      EW(1)=(B+SQRT(F))/2.
      EW(2)=(B-SQRT(F))/2.
C-----------------------------------------------------------------------
      IF(EW(1) .GT. EW(2)) GO TO 50
      C=EW(2)
      EW(2)=EW(1)
      EW(1)=C
   50 CONTINUE
C------------------------------------------ CALCULATE EIGEN VECTORS ----
      EIGV11=EW(1)-VARZ
      EIGV12=CORZPH
      EIGV21=EW(2)-VARZ
      EIGV22=CORZPH
      RETURN
      END
