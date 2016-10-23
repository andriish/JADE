C   22/04/87 712211546  MEMBER NAME  THCORR   (S)           FORTRAN
C
C
C    CORRECTION OF LG-CLUSTER ENERGIES FOR LG-READOUT THRESHOLD
C                                                      D.PITZL 13.12.87
C
C    TWO PERIODS: 1979-82  READOUT THRESHOLD = 25 MEV ( 5 ADC-COUNTS )
C                 1983-86  READOUT THRESHOLD = 36 MEV ( 6 ADC-COUNTS )
C
C    DATA FROM EGS4
C
C-------------------------------------------------------------------
      SUBROUTINE THCORR ( EINC, NRING, RTH )
C-------------------------------------------------------------------
C
C       INPUT: EINC  = INCIDENT PHOTON ENERGY IN MEV
C              NRING = NUMBER OF RING OF MOST ENERGETIC BLOCK IN CLUSTER
C
C       OUTPUT: RTH  = LOST TRACKLENGTH OF CHARGED PARTICLES /
C                      SEEN TRACKLENGTH OF CHARGED PARTICLES IN THE
C                         SHOWER
C
C       GET RTH BY INTERPOLATION IN EINC FOR EVERY BARREL-LG-RING
C
C       VARIABLES  E : LIST OF ENERGY POINTS IN MEV
C                  R25: MATRIX OF THRESHOLD LOSSES FOR RINGS 0-15 AND
C                       EACH ENERGY 1979-82
C                  R36: MATRIX OF THRESHOLD LOSSES FOR RINGS 0-15 AND
C                       EACH ENERGY 1983-86
C
C
      IMPLICIT INTEGER*2 (H)
#include "'f11god.patrecsr.for"
C
C
      DIMENSION  R25 (8,16), R36 (8,16), E(8), RW (8)
      DATA NENERG / 8 /
C
C   LINE N HAS THE CORRECTION IN % FOR RING N-1 FOR EACH ENERGY
C
      DATA E /  200. , 300. , 500. , 700. ,1000. ,2000. ,5000. ,10000. /
C
      DATA R36/ 21.  , 12.  ,  9.1 ,  8.2 ,  5.6 ,  0.0 ,  0.0 ,  0.0 ,
     1          21.  , 12.  ,  9.1 ,  8.2 ,  5.6 ,  0.0 ,  0.0 ,  0.0 ,
     2          22.  , 12.  ,  8.9 ,  7.4 ,  6.2 ,  0.0 ,  0.0 ,  0.0 ,
     3          22.  , 15.  ,  9.2 ,  7.0 ,  5.7 ,  0.0 ,  0.0 ,  0.0 ,
     4          21.  , 13.  ,  8.6 ,  7.5 ,  5.6 ,  0.0 ,  0.0 ,  0.0 ,
     5          17.  , 14.  ,  8.6 ,  6.8 ,  5.6 ,  0.0 ,  0.0 ,  0.0 ,
     6          19.  , 14.  ,  8.4 ,  7.3 ,  5.5 ,  0.0 ,  0.0 ,  0.0 ,
     7          16.  , 14.  ,  8.7 ,  6.6 ,  5.3 ,  0.0 ,  0.0 ,  0.0 ,
     8          15.  , 14.  ,  8.2 ,  6.6 ,  5.3 ,  0.0 ,  0.0 ,  0.0 ,
     9          15.  , 14.  ,  7.8 ,  6.6 ,  5.4 ,  0.0 ,  0.0 ,  0.0 ,
     *          15.  , 13.  ,  7.4 ,  6.1 ,  4.8 ,  0.0 ,  0.0 ,  0.0 ,
     1          13.  , 12.  ,  7.1 ,  5.8 ,  4.8 ,  0.0 ,  0.0 ,  0.0 ,
     2          12.  , 12.  ,  6.8 ,  5.4 ,  4.5 ,  0.0 ,  0.0 ,  0.0 ,
     3          11.  , 10.  ,  6.6 ,  5.  ,  4.  ,  0.0 ,  0.0 ,  0.0 ,
     4          11.  ,  9.  ,  6.4 ,  4.  ,  3.5 ,  0.0 ,  0.0 ,  0.0 ,
     5          10.  ,  8.  ,  6.0 ,  3.5 ,  3.0 ,  0.0 ,  0.0 ,  0.0 /
C
      DATA R25/ 21.  , 12.  ,  9.1 ,  8.2 ,  5.6 ,  0.0 ,  0.0 ,  0.0 ,
     1          21.  , 12.  ,  9.1 ,  8.2 ,  5.6 ,  0.0 ,  0.0 ,  0.0 ,
     2          22.  , 12.  ,  8.9 ,  7.4 ,  6.2 ,  0.0 ,  0.0 ,  0.0 ,
     3          22.  , 15.  ,  9.2 ,  7.0 ,  5.7 ,  0.0 ,  0.0 ,  0.0 ,
     4          21.  , 13.  ,  8.6 ,  7.5 ,  5.6 ,  0.0 ,  0.0 ,  0.0 ,
     5          17.  , 14.  ,  8.6 ,  6.8 ,  5.6 ,  0.0 ,  0.0 ,  0.0 ,
     6          19.  , 14.  ,  8.4 ,  7.3 ,  5.5 ,  0.0 ,  0.0 ,  0.0 ,
     7          16.  , 14.  ,  8.7 ,  6.6 ,  5.3 ,  0.0 ,  0.0 ,  0.0 ,
     8          15.  , 14.  ,  7.3 ,  6.6 ,  5.3 ,  0.0 ,  0.0 ,  0.0 ,
     9          15.  , 14.  ,  6.8 ,  6.6 ,  5.4 ,  0.0 ,  0.0 ,  0.0 ,
     *          15.  , 13.  ,  7.4 ,  6.1 ,  4.8 ,  0.0 ,  0.0 ,  0.0 ,
     1          13.  , 12.  ,  6.8 ,  5.8 ,  4.8 ,  0.0 ,  0.0 ,  0.0 ,
     2          13.  , 12.  ,  5.8 ,  5.4 ,  4.5 ,  0.0 ,  0.0 ,  0.0 ,
     3          10.  ,  8.  ,  6.  ,  5.  ,  4.  ,  0.0 ,  0.0 ,  0.0 ,
     4           9.  ,  7.  ,  5.  ,  4.  ,  3.5 ,  0.0 ,  0.0 ,  0.0 ,
     5           8.  ,  6.  ,  4.5 ,  3.5 ,  3.0 ,  0.0 ,  0.0 ,  0.0 /
C
C
C  RINGS ARE SYMMETRIC AROUND Z=0, SO 0=31, 1=30, ..., 15=16
C  RING NUMBERS 0-15 USED. THEIR CORRECTION ARE ON COLUMNS 1-16 OF RL
C
      NRINGW = NRING
      IF ( NRINGW .GT. 15 ) NRINGW = 31 - NRINGW
      NRINGW = NRINGW + 1

      IHEAD  = IDATA( IBLN('HEAD') )
      NRUN   = HDATA( 2*IHEAD + 10 )
      NREC   = HDATA( 2*IHEAD + 11 )
      NYEAR  = HDATA( IHEAD*2 +  8 )

C   COPY THRESHOLD CORRECTION DATA FOR RING NRING INTO WORK-ARRAY

      IF ( NYEAR .GT. 1982 ) GOTO 2
         DO 1 I = 1, NENERG
 1          RW (I) = R25 (I,NRINGW)
         GOTO 4
 2    CONTINUE
         DO 3 I=1, NENERG
 3          RW (I) = R36 (I,NRINGW)
 4    CONTINUE

C---     LINEAR INTERPOLATION IN ENERGY

      IF ( EINC .GT. E (1) ) GOTO 20
C---                                NO EXTRAPOLATION
         RTH   = RW (1)
         GOTO 999
  20  IF ( EINC .LT. E (NENERG) ) GOTO 30
         RTH   = RW (NENERG)
         GOTO 999
  30  CONTINUE
C---                  FIND ENERGY-BIN
      IEND = NENERG - 1
      DO 40  I = 1, IEND
         IF ( EINC .GE. E(I) .AND. EINC .LE. E(I+1) ) GOTO 50
  40  CONTINUE
  50  CONTINUE
      EDIFF = EINC - E(I)
      ESTEP = E(I+1) - E(I)
      EFAC  = EDIFF / ESTEP
      RTH   = RW (I) + EFAC * ( RW (I+1) - RW (I) )
 999  CONTINUE
      RTH   = RTH   / 100.
      RETURN
      END
