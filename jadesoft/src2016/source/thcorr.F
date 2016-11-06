C   22/04/87 808021707  MEMBER NAME  THCORR   (SOURCE)      FORTRAN
C
C
C    CORRECTION OF LG-CLUSTER ENERGIES FOR LG-READOUT THRESHOLD
C                                                      D.PITZL 22.6.88
C
C    TWO PERIODS: 1979-82  READOUT THRESHOLD = 25 MEV ( 5 ADC-COUNTS )
C                 1983-86  READOUT THRESHOLD = 36 MEV ( 6 ADC-COUNTS )
C
C    DATA FROM GEANT3
C
C-------------------------------------------------------------------
      SUBROUTINE THCORR ( EINC, NRING, RTH )
C-------------------------------------------------------------------
C
C       INPUT: EINC  = INCIDENT PHOTON ENERGY IN MEV
C              NRING = NUMBER OF RING OF MOST ENERGETIC BLOCK IN CLUSTER
C
C       OUTPUT: RTH  = LOST LG SIGNAL / INCIDENT ENERGY
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
*** include "'f11god.patrecsr.for"  PMF 27.10.98: does not exist; replaced by 'cdata.f'
#include "cdata.for"
C
C
      DIMENSION  R25 (11,16), R36 (11,16), E(11), RW (11)
      DATA NENERG / 11 /
C
C   LINE N HAS THE CORRECTION IN % FOR RING N FOR EACH ENERGY
C
      DATA  E /
     1 150., 250., 350., 450., 600., 850.,1250.,2000.,3750.,7500.,15000.
     2 /
C
      DATA R36 /
     *  0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0,
     1  0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0,
     2 23.5 ,21.7 ,19.7 ,14.9 ,11.6 , 8.7 , 6.3 , 4.2 , 2.9 , 1.5 , 0.0,
     3 23.8 ,22.8 ,19.2 ,14.8 ,11.9 , 9.0 , 6.5 , 4.6 , 3.0 , 1.8 , 0.0,
     4 23.9 ,22.7 ,18.2 ,14.5 ,11.9 , 9.0 , 6.6 , 4.9 , 3.1 , 2.0 , 0.0,
     5 23.6 ,20.6 ,17.4 ,13.8 ,11.2 , 8.4 , 6.4 , 4.7 , 3.0 , 2.0 , 0.0,
     6 22.8 ,19.2 ,16.2 ,12.4 ,10.2 , 7.8 , 5.9 , 4.3 , 2.9 , 1.7 , 0.0,
     7 22.0 ,18.5 ,15.4 ,11.7 , 9.8 , 7.4 , 5.5 , 4.1 , 2.7 , 1.7 , 0.0,
     8 20.8 ,17.2 ,14.1 ,11.0 , 9.1 , 6.9 , 5.1 , 3.9 , 2.6 , 1.7 , 0.0,
     9 19.3 ,15.8 ,12.6 ,10.4 , 8.3 , 6.4 , 4.8 , 3.7 , 2.5 , 1.7 , 0.0,
     * 18.2 ,15.2 ,11.6 , 9.7 , 7.7 , 6.0 , 4.5 , 3.4 , 2.4 , 1.6 , 0.0,
     1 17.1 ,14.3 ,10.8 , 8.9 , 7.1 , 5.6 , 4.3 , 3.3 , 2.3 , 1.5 , 0.0,
     2 16.0 ,12.9 ,10.2 , 8.1 , 6.6 , 5.4 , 4.2 , 3.3 , 2.2 , 1.5 , 0.0,
     3 14.3 ,11.3 , 8.8 , 7.2 , 6.2 , 4.9 , 4.0 , 3.1 , 2.2 , 1.4 , 0.0,
     4 13.5 ,10.4 , 8.0 , 6.7 , 6.0 , 4.7 , 3.8 , 3.0 , 2.2 , 1.4 , 0.0,
     5 13.2 ,10.4 , 8.1 , 6.9 , 5.9 , 4.8 , 3.7 , 2.9 , 2.0 , 1.3 , 0.0/
C
      DATA R25 /
     *  0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0,
     1  0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0,
     2 18.0 ,16.8 ,14.7 ,11.3 , 8.8 , 6.2 , 4.7 , 3.0 , 2.0 , 1.2 , 0.0,
     3 18.3 ,17.2 ,14.4 ,11.0 , 9.0 , 6.4 , 4.8 , 3.4 , 2.2 , 1.4 , 0.0,
     4 18.5 ,17.0 ,14.0 ,10.7 , 9.0 , 6.5 , 4.9 , 3.7 , 2.3 , 1.5 , 0.0,
     5 18.1 ,15.8 ,13.4 ,10.4 , 8.6 , 6.4 , 4.9 , 3.5 , 2.3 , 1.5 , 0.0,
     6 17.5 ,14.5 ,12.5 , 9.8 , 7.6 , 5.9 , 4.6 , 3.3 , 2.2 , 1.4 , 0.0,
     7 16.8 ,13.8 ,11.5 , 9.1 , 7.1 , 5.5 , 4.3 , 3.1 , 2.1 , 1.3 , 0.0,
     8 15.8 ,13.0 ,10.3 , 8.4 , 6.6 , 5.2 , 4.0 , 2.9 , 2.0 , 1.3 , 0.0,
     9 14.8 ,12.2 , 9.3 , 7.9 , 6.2 , 4.9 , 3.8 , 2.8 , 1.9 , 1.3 , 0.0,
     * 13.9 ,11.5 , 8.7 , 7.3 , 5.8 , 4.5 , 3.5 , 2.7 , 1.9 , 1.2 , 0.0,
     1 13.1 ,10.7 , 8.0 , 6.7 , 5.5 , 4.2 , 3.4 , 2.7 , 1.8 , 1.2 , 0.0,
     2 12.4 , 9.6 , 7.3 , 6.3 , 5.2 , 4.1 , 3.3 , 2.6 , 1.7 , 1.1 , 0.0,
     3 11.3 , 8.8 , 6.9 , 5.6 , 4.9 , 3.9 , 3.2 , 2.3 , 1.7 , 1.1 , 0.0,
     4 10.7 , 8.3 , 6.3 , 5.4 , 4.8 , 3.8 , 3.0 , 2.3 , 1.6 , 1.1 , 0.0,
     5 10.6 , 8.1 , 6.4 , 5.5 , 4.6 , 3.8 , 2.9 , 2.2 , 1.5 , 1.0 , 0.0/
C

      DATA NCALLS / 0 /

      IF ( NCALLS .EQ. 0 ) PRINT 9955
 9955 FORMAT ( T2,'JADELG.LOAD (THCORR) CALLED FROM LGECOR' )
      NCALLS = NCALLS + 1
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
