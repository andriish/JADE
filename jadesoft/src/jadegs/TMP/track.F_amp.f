C   28/11/79 001251642  MEMBER NAME  TRACK    (JADESR)      FORTRAN
      SUBROUTINE TRACK(IP,ITP,RMIN,NHT,FI,THETA,PTOT,PT,PL,PX,PY,PZ)
C---
C---     RETURNS VARIOUS TRACK PARAMETERS FOR A SINGLE TRACK
C---
C---     IP IS ADRESS IN PATR OR LGCL BANKS
C---     ITP = 0  ---> GAMMA
C---     ITP = 1  ---> CHARGED TRACK
C---     ALL UNITS IN MM,RAD,GEV
C---                                    J.OLSSON  28.11.79
C---            LATEST CHANGE  18.12.79
C---
      IMPLICIT INTEGER*2 (H)
#include "cdata.for"
      COMMON /CJTRIG/ PI,TWOPI
      COMMON /CHEADR/ HEAD(108)
      DATA HERR/0/
C--
      IF(ITP.EQ.0) GO TO 1000
C                                             >>>>> CHARGED TRACK <<<<<<
      NHT = IDATA(IP+24)
      IF(IDATA(IP+33).GT.NHT) NHT = IDATA(IP+33)
      CALL MOMENT(IP,PX,PY,PZ,PT,PTOT,FI,THETA)
      CALL PARMIN(ADATA(IP+19),ADATA(IP+20),ADATA(IP+21),
     $ ADATA(IP+22),RMIN,IDATA(IP+18))
999   IF(ABS(PTOT).LT.ABS(PT)) HERR = HERR + 1
      IF(ABS(PTOT).LT.ABS(PT).AND.HERR.LT.10)
     $ WRITE(6,7575) HEAD(18),HEAD(19),
     $ IP,NHT,PX,PY,PZ,PT,PTOT,FI,THETA
7575  FORMAT(' R * EV.',2I6,' IP NHT ',2I6,' PX-Z,PT PTOT',5E11.3,' FI *
     $TH ',2F8.3)
      PL = PTOT*PTOT - PT*PT + .000001
      IF(PL.LE.0.000001) GO TO 9977
      PL = SQRT(PL)
      IF(THETA.GT.PI*.5) PL = - PL
      GO TO 9999
9977  PL = 0.
      GO TO 9999
C                                                  >>>>> PHOTON <<<<<
1000  RMIN = 0.
      NHT = 0
      CALL MOMGAM(IP,PX,PY,PZ,PT,PTOT,FI,THETA)
      GO TO 999
9999  RETURN
      END
