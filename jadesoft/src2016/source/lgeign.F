C   17/03/79 003120036  MEMBER NAME  LGEIGNX  (SOURCE)      FORTRAN
      SUBROUTINE LGEIGN(PARA)
C
C     R.EICHLER   7-11-78 17:00
C     LAST MODIFICATION 15-02-80 13:35   BY S.YAMADA, AVZ CORR IS ADDED.
C
C   CALCULATE AVPH,SIGPH,AVZ,SIGZ,SIGZPH
C
      IMPLICIT INTEGER *2 (H)
C
#include "clgwork1.for"
      COMMON /CLGDMS/ X0,RADIUS(6),RADSX0(6),THX0(4),
     $                ZEND(2),ZENDX0(2),ZWID(2),ZGAP(2),PHWID(2),
     $                ZECAP(4),ZECAPX(4),THECPX(2)
      COMMON/LGSHP/WW(50),ZZ(50),XX(50),EW(2),AVPH,AVZ,SIGPH,SIGZ,EV,KK
C
      DATA EZ,EPH/0.33,0.33/, NVCODE/180021615/, MESS/0/
C
      IF(MESS) 1000,1000,1001
 1000 IDENT(1) = IDENT(1)+NVCODE/100000000*100
      MESS = 1
C
 1001   WTSUM=0.
        WTZSUM=0.
        ZSUM=0.
        ZPH=0.
        Z2SUM=0.
        PHSUM = 0.
        PH2SUM = 0.
        IMAX = KK
        IF(IMAX.GT.50) IMAX = 50
          DO 10 I=1,IMAX
          W=WW(I)**EPH
          ZW=W
C           USE THE SAME WEIGHT  15-02-80
          WZ=ZW*ZZ(I)
          ZSUM=ZSUM+WZ
          ZPH=ZPH+WZ*XX(I)
          Z2SUM=Z2SUM+WZ*ZZ(I)
          WTSUM=WTSUM+W
          WTZSUM=WTZSUM+ZW
          WPH=W*XX(I)
          PHSUM=PHSUM+WPH
          PH2SUM=PH2SUM+WPH*XX(I)
   10     CONTINUE
C      WRITE(6,900) WTSUM,WTZSUM,AVZ,PH2SUM
C900  FORMAT(' WTSUM,WTZSUM,AVZ,PH2SUM',5F12.3)
      AVPH=PHSUM/WTSUM
      SIGPH=(PH2SUM/WTSUM-AVPH*AVPH)
      AVZ=ZSUM/WTZSUM
      COSTH=1.
      IF(PARA .LE.0.1) COSTH=AVZ/SQRT(AVZ*AVZ+RADIUS(3)**2)
      SIGZ=(Z2SUM/WTZSUM-AVZ*AVZ)*COSTH*COSTH
      SIGZPH=(ZPH/WTZSUM-AVZ*AVPH)*COSTH
C
C---- CALCULATE EIGENVALUES OF MATRIX  ( SIGPH  SIGPHZ )
C                                      ( SIGPHZ SIGZ   )
      D=SIGPH+SIGZ
      F=D*D+4.*SIGZPH*SIGZPH-4.*SIGPH*SIGZ
      IF (F .LT. 0.) GO TO 60
      C=SQRT(F)*0.5
      EW(1)=D*0.5+C
      EW(2)=D*0.5-C
      F=SIGPH-EW(1)
      IF(ABS(F) .LT. 1.E-20) F=1.E-20
C---- EV=ANGLE OF EIGENVECTORS TO X-Y RESP. Z-PH COORD.SYSTEM
      EV=ATAN(-SIGZPH/F)
      IF(EW(1) .GT. EW(2)) GO TO 50
      C=EW(2)
      EW(2)=EW(1)
      EW(1)=C
   50 CONTINUE
      IF (EW(1))  60,60,70
C
C---- IF MATRIX IS SINGULAR OR DUE TO ROUNDING ERRORS BOTH EIGEN
C     VALUES ARE NEGATIV,SET EIGENVALUES SO THAT EW1/EW2 AND EW1+EW2
C---- ARE RIDICULOUS
   60 EW(1)=1.
      EW(2)=-10.
C
C---- AVZ IS CORRECTED FOR BARREL HITS,IF ABS(Z) .LE.500MM.
   70 IF(PARA.GT.0.1) GO TO 100
      IF( AVZ.LE.-50. .OR. AVZ.GE.50. ) GO TO 100
      AVZ =  AVZ*AVZ*AVZ/2500.
  100 RETURN
      END
