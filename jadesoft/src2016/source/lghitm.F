C   14/03/79 208201407  MEMBER NAME  LGHITM   (SOURCE)      FORTRAN
      SUBROUTINE LGHITM(JDMP)
C
C     S.YAMADA   14-03-79  10:00
C     LAST MODIFICATION  15-03-79  13:50
C
C---- TEST HISTOGRAMS FOR LG-HITS
C
      IMPLICIT INTEGER *2 (H)
C
C
#include "clgwork1.for"
      INTEGER *2 HIST(2880)/2880*0/
C////   //////////////////////////////////////////
C----   TEST HISTOGRAMS OF THE ALWAYS-ON ADCS (FOR TAPE 133)
CCC     DATA NBAD/ 44/
CCC     INTEGER *2 NBLIST(44)/ 1044,1088,1184,1198,1224,1318,1503,1535,
CCC  1                         1546,1549,1554,1567,1573,1578,1680,1695,
CCC  2                         1737,1750,1833,1847,1888,1897,1907,1983,
CCC  3                         2015,2125,2162,2191,2194,2199,2202,2314,
CCC  4                         2323,2337,2354,2364,2366,2384,2393,2492,
CCC  5                         2494,2559,2564,2604/
C//////////////////////////////////////////////////
C                                             (FOR TAPE 137,138)
        DATA NBAD/8/
        INTEGER *2 NBLIST(8)/ 171,803,804,866,1334,1807,2238,2384/
C
C//////////////////////////////////////////////////
C
      JDMP = 0
      HADO = 0
      NHIT = LNG-3
      AHIT = NHIT
      CALL HFILL( 10, AHIT, DUM, 1.0)
      IF(NHIT) 100,100,10
   10   DO 20 N=1,NHIT
        HAD = HLGADC(1,N)
        IF(NBAD.LE.0) GO TO 21
          DO 23 NN=1,NBAD
          IF(HAD.EQ.NBLIST(NN)) GO TO 22
   23     CONTINUE
C
        GO TO 21
C
   22   CONTINUE
        IF(HAD.LT.400.OR.HAD.GT.2300.OR.(HAD.GT.1000.AND.HAD.LT.1700))
     +                                            GO TO 24
        NPLOT = HAD
        ADC = HLGADC(2,N)
        CALL HFILL( NPLOT, ADC, DUM, 1.0)
C----   ERASE ALWAYS-ON ADCS FROM FURTHER ANALYSIS
   24   HLGADC(2,N) = 0
C////   ///////////////////////////////////////////
C
   21   HAD = HAD+1
        IF(HAD.GE.1 .AND. HAD.LE.2880) HIST(HAD) = HIST(HAD)+1
        IF(HAD.LE.HADO) JDMP = 1
        HADO = HAD
C
C----   PLOT EACH CRATE ADC'S SEPARATELY
        NPL = (HAD-1)/960+11
        PH = HLGADC(2,N)
        CALL HFILL(NPL, PH,DUM, 1.0)
   20   CONTINUE
  100 RETURN
C
C*********************************************************
C
      ENTRY LGHITL
C
C---- PRINT OUT THE ABOVE HISTOGRAM
        DO 50 N=1,180
        KE = 16*N
        KS = KE-15
        KSL = MOD(N-1,60)/3+1
        KS1 = KS-1
        WRITE(6,600) KSL,KS1, (HIST(KK),KK=KS,KE)
  600   FORMAT(' ',I3,' (',I4,') :',8I4,2X,8I4)
   50   CONTINUE
C
C---- WRITE FREQUENTLY HIT CHANNEL NUMBERS.
        DO 60 N=1,2880
        IF( HIST(N).LT.100 ) GO TO 60
        NC = N-1
        ICR = NC/960
        IW = NC-ICR*960
        ISL = IW/48
        ICH = IW-48*ISL
        ISL = ISL+1
        WRITE(6,601) NC,ICR,ISL,ICH,HIST(N)
  601   FORMAT(' NC=',I5,'  ICR,ISL,ICH=',3I5,5X,'COUNT=',I5)
   60   CONTINUE
      RETURN
      END
