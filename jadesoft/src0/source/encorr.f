C   10/02/88 808121015  MEMBER NAME  ENCORR   (SOURCE)      FORTRAN


C   LG ENERGY CORRECTION FOR PHOTONS, TAKING INTO ACCOUNT
C     ENERGY LOSS IN COIL
C     LEAKAGE OF PHOTON SHOWER
C     ANGULAR DEPENDENCE OF CERENKOV LIGHT COLLECTION EFFICIENCY
C     READOUT THRESHOLD
C                                                    D.PITZL 02.08.88

C    DATA FROM GEANT3

C***********************************************************************
      SUBROUTINE ENCORR ( ENERGY, COSTET, FESEE )
C***********************************************************************

C   INPUT  : COSTET = COS THETA OF PHOTON CLUSTER
C            ENERGY = PHOTON ENERGY IN GEV
C   OUTPUT : FESEE  = SEEN ENERGY / INCIDENT ENERGY

      IMPLICIT INTEGER*2 (H)
#include "'f11god.patrecsr.for"

C--                                DATA FOR 15 ANGLES, 11 ENERGIES,
C                                       AND 2 PERIODS
      DIMENSION NRSF5 ( 15, 11 )
      DIMENSION NRSF6 ( 15, 11 )
      DIMENSION F     ( 15, 11, 2 )
      DIMENSION  CT (15)
      DIMENSION  E (11)

      DATA  NE  / 11 /
      DATA  NCT / 15 /

      DATA E /  0.15 ,  0.25 ,  0.35 ,  0.45 ,  0.60 ,  0.85 , 1.25  ,
     +          2.0  ,  3.75 ,  7.5  , 12.0  /

      DATA  CT /
     + 0.00 , 0.045, 0.135, 0.20 , 0.25 , 0.32 , 0.43 ,
     + 0.545, 0.62 , 0.675, 0.72 , 0.755, 0.785, 0.82 , 0.86 /

C      LINE N HAS ESEEN / E INC FOR ENERGY (N) FOR 15 ANGLES

C      0.0         0.2         .43     .62     .72         .82
      DATA NRSF5 /
     -  780,780,775,775,765,760,735,710,690,675,660,645,630,620,620,
     -  800,800,795,785,785,785,755,725,710,685,670,660,650,640,640,
     -  825,825,820,810,805,805,770,745,720,700,685,675,665,655,655,
     -  845,845,840,825,820,810,785,750,725,705,695,685,675,665,665,
     -  865,865,855,840,840,825,795,765,740,720,710,700,690,680,680,
     -  885,885,875,855,855,855,820,785,765,745,735,730,720,710,710,
     -  895,895,890,880,880,875,835,805,785,770,762,755,740,730,730,
     -  910,910,900,890,895,900,865,835,815,800,785,775,770,760,760,
     -  915,915,900,890,900,920,895,865,840,820,805,795,785,775,775,
     -  900,900,880,870,890,915,888,857,842,822,800,799,800,800,800,
     -  880,880,855,840,845,870,860,840,830,820,800,800,800,800,800/

      DATA NRSF6 /
     -  770,770,765,760,745,730,700,670,645,630,615,600,590,580,580,
     -  800,800,795,785,775,760,725,695,670,650,630,620,610,600,600,
     -  830,830,825,815,800,780,745,715,690,670,650,635,625,615,615,
     -  860,860,855,840,820,795,765,730,705,685,665,655,645,640,640,
     -  885,885,875,860,845,815,780,750,720,700,690,680,670,660,660,
     -  910,910,900,880,865,845,810,775,750,730,720,710,700,690,690,
     -  930,930,925,910,895,870,830,795,775,760,750,740,730,720,720,
     -  960,960,950,935,920,895,860,825,805,790,775,765,760,750,750,
     -  980,980,960,945,930,915,890,860,835,815,800,790,780,770,770,
     -  980,980,960,940,930,915,888,857,842,822,800,799,800,800,800,
     -  960,960,930,910,880,870,860,840,830,820,800,800,800,800,800/

      DATA NCALLS / 0 /

C---                 INITIALISATION
C                    NECESSARY: ONLY 19 CONTINUATION LINES FOR DATA
C                               STATEMENT ALLOWED.

      IF ( NCALLS .GT. 0 ) GOTO 5
         K = 1
         DO 2  J=1,NE
            DO 1  I=1,NCT
               F ( I, J, K ) = FLOAT ( NRSF5 ( I, J ) ) / 1000.0
  1         CONTINUE
  2      CONTINUE
         K = 2
         DO 4  J=1,NE
            DO 3  I=1,NCT
               F ( I, J, K ) = FLOAT ( NRSF6 ( I, J ) ) / 1000.0
  3         CONTINUE
  4      CONTINUE
  5   CONTINUE

      IHEAD  = IDATA ( IBLN('HEAD') )
      NYEAR  = HDATA ( 2 * IHEAD +  8 )

      K = 1
      IF ( NYEAR .GT. 1982 ) K = 2

      IF ( NCALLS .EQ. 0 ) PRINT 7777

 7777 FORMAT (  T2, 'JADELG.LOAD (ENCORR) CALLED FROM LGECOR',
     +   ' FOR PHOTONS' )

      NCALLS = 1


      FESEE = 1.0
      ACT = ABS ( COSTET )
C                         LIMIT COS THETA TO BARREL:
      IF ( ACT .GT. 0.86 ) RETURN

C        LINEAR INTERPOLATION IN ENERGY AND COS THETA

C---                  FIND COS THETA BIN I
      IEND = NCT - 1
      DO 10  I=1, IEND
         IF ( ACT .GE. CT(I) .AND. ACT .LE. CT(I+1) ) GOTO 15
  10  CONTINUE
  15  CONTINUE
C---                                        NO EXTRAPOLATION
      IF ( ENERGY .GT. E (1) ) GOTO 20
         FACT1 = F ( I, 1, K)
         FACT2 = F ( I+1, 1, K )
         GOTO 111
  20  IF ( ENERGY .LT. E (NE) ) GOTO 25
         FACT1 = F ( I, NE, K )
         FACT2 = F ( I+1, NE, K )
         GOTO 111
  25  CONTINUE

C---                  FIND ENERGY-BIN J
      JEND = NE - 1
      DO 40  J=1, JEND
         IF ( ENERGY .GE. E(J) .AND. ENERGY .LE. E(J+1) ) GOTO 50
  40  CONTINUE
  50  CONTINUE
C---                                LINEAR INTERPOLATION
      EDIFF  = ENERGY - E(J)
      ESTEP  = E(J+1) - E(J)
      EFAC   = EDIFF / ESTEP
C                            ENERGY INTERPOLATION AT LOWER AND HIGHER
C                            COS THETA POINT:
C
      FACT1  = (1.0-EFAC) * F (I, J, K) + EFAC * F (I, J+1, K)
      FACT2  = (1.0-EFAC) * F (I+1, J, K) + EFAC * F (I+1, J+1, K)

 111  CONTINUE
C                             COS THETA INTERPOLATION:
      CTDIFF = ACT - CT(I)
      CTSTEP = CT(I+1) - CT(I)
      CTFAC  = CTDIFF / CTSTEP
      FESEE = FACT1 + CTFAC * ( FACT2 - FACT1 )

      RETURN
      END
