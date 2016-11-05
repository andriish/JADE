C   10/12/87 808021751  MEMBER NAME  LKCORR   (SOURCE)      FORTRAN

C   CORRECTION FOR LEAKAGE OF PHOTON-SHOWERS IN BARREL-LG
C                                                    D.PITZL 13.12.87

C   MATERIAL BETWEEN ID AND LG IS INCLUDED AS 1.04 X0 ALU
C
C   DONE FOR EVERY LG-RING SEPERATELY
C   DATA FOR SF5 AND SF6
C   INTERPOLATION IN PHOTON-ENERGY

C    DATA FROM EGS4

C***********************************************************************
      SUBROUTINE LKCORR ( ENERGY, NRING, RLEAK )
C***********************************************************************

C   INPUT  : NRING = NUMBER OF BARREL-LG-RING  ( 0-31 )
C            ENERGY = PHOTON ENERGY IN MEV
C   OUTPUT : RLEAK = RATIO LEAKING ENERGY OVER INCIDENT ENERGY

      IMPLICIT INTEGER*2 (H)
*** include "'f11god.patrecsr.for" PMF 27.10.98: does not exist; replaced by 'cdata.f'
#include "cdata.for"
      DIMENSION RSF5 (9,16), RSF6 (9,16), E (9), RW (9)
      DATA  NENERG / 9 /

      DATA E /
     +   100. , 300. , 500. , 700. ,1000. ,2000. ,5000. ,10000.,15000. /

C      LINE N OF DATA-TABLE HAS LEAKAGE IN % FOR RING N FOR 9 ENERGIES
C      SO RSF (I,J) HAS THE LEAKAGE-CORRECTION FOR ENERGY E(I)
C      AND RING NUMBER J.

C   THERE IS NO CORRECTION FOR THE OUTER TWO RINGS ON BOTH SIDES OF THE
C   BARREL SINCE THEY SUFFER FROM SIDEWARD LEAKAGE WITH LARGE
C   FLUCTUATIONS.


      DATA RSF5 /
     +   0.0  ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,
     1   0.0  ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,
     2   0.94 , 1.39 , 1.81 , 2.20 , 2.82 , 3.92 , 5.01 , 10.3 , 11.7 ,
     3   0.13 , 0.21 , 0.30 , 0.44 , 0.52 , 0.88 , 1.04 , 1.87 , 2.10 ,
     4   0.16 , 0.26 , 0.36 , 0.52 , 0.62 , 0.98 , 1.21 , 2.36 , 2.71 ,
     5   0.24 , 0.38 , 0.50 , 0.71 , 0.86 , 1.25 , 1.64 , 3.20 , 3.51 ,
     6   0.34 , 0.53 , 0.68 , 0.94 , 1.17 , 1.67 , 2.18 , 4.05 , 4.64 ,
     7   0.45 , 0.72 , 0.92 , 1.22 , 1.51 , 2.20 , 2.83 , 5.24 , 6.00 ,
     8   0.60 , 0.97 , 1.23 , 1.56 , 1.98 , 2.81 , 3.63 , 6.49 , 7.42 ,
     9   0.82 , 1.25 , 1.62 , 1.98 , 2.53 , 3.54 , 4.54 , 8.07 , 9.05 ,
     +   1.06 , 1.54 , 2.02 , 2.44 , 3.11 , 4.31 , 5.51 , 9.69 , 11.0 ,
     1   1.30 , 1.88 , 2.46 , 2.99 , 3.74 , 5.14 , 6.56 , 11.3 , 12.8 ,
     2   1.50 , 2.19 , 2.88 , 3.49 , 4.32 , 5.90 , 7.57 , 12.8 , 14.6 ,
     3   1.68 , 2.50 , 3.28 , 3.96 , 4.87 , 6.61 , 8.55 , 13.8 , 15.9 ,
     4   1.79 , 2.71 , 3.56 , 4.28 , 5.25 , 7.10 , 9.22 , 14.6 , 16.9 ,
     5   1.85 , 2.81 , 3.70 , 4.45 , 5.44 , 7.34 , 9.56 , 15.5 , 17.8 /

      DATA RSF6 /
     +   0.0  ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,
     1   0.0  ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,
     2   0.94 , 1.39 , 1.81 , 2.20 , 2.82 , 3.92 , 5.01 , 10.3 , 11.7 ,
     3   0.13 , 0.21 , 0.30 , 0.44 , 0.52 , 0.88 , 1.04 , 1.87 , 2.10 ,
     4   0.16 , 0.26 , 0.36 , 0.52 , 0.62 , 0.98 , 1.21 , 2.36 , 2.71 ,
     5   0.24 , 0.38 , 0.50 , 0.71 , 0.86 , 1.25 , 1.64 , 3.20 , 3.51 ,
     6   0.34 , 0.53 , 0.68 , 0.94 , 1.17 , 1.67 , 2.18 , 4.05 , 4.64 ,
     7   0.45 , 0.72 , 0.92 , 1.22 , 1.51 , 2.20 , 2.83 , 5.24 , 6.00 ,
     8   0.60 , 0.97 , 1.23 , 1.56 , 1.98 , 2.81 , 3.63 , 6.49 , 7.42 ,
     9   0.82 , 1.25 , 1.62 , 1.98 , 2.53 , 3.54 , 4.54 , 8.07 , 9.05 ,
     +   1.06 , 1.54 , 2.02 , 2.44 , 3.11 , 4.31 , 5.51 , 9.69 , 11.0 ,
     1   1.30 , 1.88 , 2.46 , 2.99 , 3.74 , 5.14 , 6.56 , 11.3 , 12.8 ,
     2   1.50 , 2.19 , 2.88 , 3.49 , 4.32 , 5.90 , 7.57 , 12.8 , 14.6 ,
     3   0.23 , 0.39 , 0.44 , 0.68 , 0.84 , 1.03 , 1.44 , 2.21 , 2.88 ,
     4   0.26 , 0.45 , 0.49 , 0.77 , 0.93 , 1.15 , 1.61 , 2.50 , 3.22 ,
     5   0.28 , 0.47 , 0.52 , 0.81 , 0.97 , 1.22 , 1.69 , 2.60 , 3.34 /

      DATA  NCALLS / 0 /

      IF ( NCALLS .EQ. 0 ) PRINT 9123
 9123 FORMAT ( T2,'JADELG.LOAD (LKCORR) CALLED FROM LGECOR' )
      NCALLS = 1

C                         FIND COLUMN IN RSF CORRESPONDING TO NRING:
      NRINGW = NRING
C---                                             SYMMETRY AROUND Z=0
      IF ( NRINGW .GT. 15 ) NRINGW = 31 - NRINGW
      NRINGW = NRINGW + 1

      IHEAD  = IDATA( IBLN('HEAD') )
      NRUN   = HDATA( 2*IHEAD + 10 )
      NREC   = HDATA( 2*IHEAD + 11 )
      NYEAR  = HDATA( IHEAD*2 +  8 )

C   COPY LEAKAGE CORRECTION DATA FOR RING NRING INTO WORK-ARRAY

      IF ( NYEAR .GT. 1982 ) GOTO 2
         DO 1 I = 1, NENERG
 1          RW (I) = RSF5 (I,NRINGW)
         GOTO 4
 2    CONTINUE
         DO 3 I=1, NENERG
 3          RW (I) = RSF6 (I,NRINGW)
 4    CONTINUE

C        LINEAR INTERPOLATION IN ENERGY

      IF ( ENERGY .GT. E (1) ) GOTO 20
C---                                NO EXTRAPOLATION
         RLEAK = RW (1)
         GOTO 999
  20  IF ( ENERGY .LT. E (NENERG) ) GOTO 30
         RLEAK = RW (NENERG)
         GOTO 999
  30  CONTINUE
C---                  FIND ENERGY-BIN
      IEND = NENERG - 1
      DO 40  I=1, IEND
         IF ( ENERGY .GE. E(I) .AND. ENERGY .LE. E(I+1) ) GOTO 50
  40  CONTINUE
  50  CONTINUE
C---                                LINEAR INTERPOLATION
      EDIFF = ENERGY - E(I)
      ESTEP = E(I+1) - E(I)
      EFAC  = EDIFF / ESTEP
      RLEAK = RW (I) + EFAC * ( RW (I+1) - RW (I) )
 999  CONTINUE
      RLEAK = RLEAK / 100.
      RETURN
      END
