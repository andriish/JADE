C   22/02/79 705211106  MEMBER NAME  PREVTX   (JADEGS)      FORTRAN
      SUBROUTINE PREVTX
C
      INTEGER*2 HDATA
#include "cdata.for"
      COMMON /CWORK1/ NT,T(500)
      DIMENSION IT(2)
      EQUIVALENCE (T(1),IT(1))
C        CONSTANTS
      DATA SIGX0/.14/, SIGZ0/20./, H03/.15/, X0INN/.153/
C
      NT = 0
      IP = IDATA(IBLN('PATR'))
      IF(IP.EQ.0) RETURN
      NT = IDATA(IP+2)
      IF(NT.LE.0) RETURN
      L0 = IDATA(IP+1)
      LT = IDATA(IP+3)
C
      KT = IP + L0
      JT = 0
      DO 19 I=1,NT
      IT(JT+1) = 0
C        MOST OF THE FOLLOWING TRACK REJECT CRITERIA ARE DUE
C        TO FAULTS IN PATTERN RECOGNITION
      IF(IDATA(KT+48).NE.0) GOTO 18
      IF(ADATA(KT+25).EQ.0.) GOTO 18
      PT = SQRT(ADATA(KT+8)**2+ADATA(KT+9)**2)
      IF(PT.EQ.0.) GOTO 18
      IF(ADATA(KT+10).EQ.0.) GOTO 18
      T(JT+2) = -1. / ADATA(KT+25)
      T(JT+3) = ATAN2(ADATA(KT+9),ADATA(KT+8))
      TTH = ADATA(KT+10) / PT
      CTH = 1. / SQRT(1.+TTH**2)
      T(JT+4) = ATAN(TTH)
      T(JT+5) = ADATA(KT+5)
      T(JT+6) = ADATA(KT+6)
      T(JT+7) = ADATA(KT+7)
      SIGX = ADATA(KT+23)
      SIGZ = ADATA(KT+32)
      XNPT = IDATA(KT+24)
      ZNPT = IDATA(KT+33)
      IF(SIGX.EQ.0..OR.SIGZ.EQ.0..OR.XNPT.EQ.0..OR.ZPNT.EQ.0.) GOTO 18
      PROJL = ABS((ADATA(KT+14)-ADATA(KT+7))/TTH)
      IF(PROJL.LT.200.) GOTO 18
      T(JT+8) = SIGX * 14./(PROJL*SQRT(XNPT))
      T(JT+9) = SIGZ * 4.*CTH**2/(PROJL*SQRT(ZNPT))
      T(JT+10) = SIGX * 3./(SQRT(XNPT)*SIN(T(JT+3)))
      T(JT+11) = SIGX * 3./(SQRT(XNPT)*COS(T(JT+3)))
      T(JT+12) = SIGZ * SQRT(6./ZNPT)
      IT(JT+13) = IDATA(KT+24)
      T(JT+14) = 0.
      T(JT+15) = 0.
C     PRINT 118, I,(ADATA(KT+II),II=5,10),ADATA(KT+25),XNPT,ZNPT,SIGX,
C    *           SIGZ
C     PRINT 118, I,(T(JT+II),II=3,12)
C 118 FORMAT(I5,11E11.3)
C
      IF(SIGX.GT.10.*SIGX0) GOTO 18
      IF(SIGZ.GT.10.*SIGZ0) GOTO 18
      IT(JT+1) = 1
   18 JT = JT + 15
   19 KT = KT + LT
C
      JT = 0
      DO 29 I=1,NT
      IF(IT(JT+1).EQ.0) GOTO 29
      PT = H03*T(JT+2)
      BETA = 1.
      TH2 = (15./(PT*BETA))**2 * X0INN * COS(T(JT+4))
      T(JT+8) = SQRT(T(JT+8)**2+TH2)
      T(JT+9) = SQRT(T(JT+9)**2+TH2)
   29 JT = JT + 15
C
      RETURN
      END
