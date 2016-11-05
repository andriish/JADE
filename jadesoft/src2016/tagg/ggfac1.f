C   14/03/84            MEMBER NAME  GGFAC1   (S)           FORTRAN
C   13/09/79 C9091301   MEMBER NAME  GGFAC1   (S)           FORTRAN
      SUBROUTINE GGFAC1
C---  COPY CALIBRATION CONSTANTS OF LEAD-GLASS INTO COMMON /CALICO/
C---  RUNS 540 - 1056
C
C     H.WRIEDT    13.09.79     19:00
C     LAST MODIFICATION    13.09.79   19:00
C
      COMMON /CFACT1/ FACT1(191)
      COMMON /CALICO/ FAKTOR(191)
C
        DO 1 I = 1,191
    1   FAKTOR(I) = FACT1(I)
      WRITE(6,600)
  600 FORMAT(/' *** FIRST SET OF LEAD-GLASS CALIBRATION CONSTANTS',
     +        ' INCLUDED ***')
      RETURN
      END
      BLOCK DATA
C
      COMMON /CFACT1/ FACT1
C     THIS DATA APPLICABLE FROM RUN 540 TO RUN 1056
C     THE GAIN ASSUMES ENERGY(MEV)=CHANNEL*5.0
      REAL*4 FACT1(191)/
     +1.000,1.000,1.000,1.000,1.000,0.672,1.000,1.406,1.023,0.750,1.000,
     10.900,0.776,1.023,0.957,1.500,0.937,1.285,0.750,1.000,0.600,3.749,
     11.000,0.865,0.900,1.023,1.184,0.726,1.250,1.000,1.323,1.071,0.849,
     11.607,1.406,0.803,0.776,1.000,1.000,0.900,0.918,1.184,0.918,1.097,
     10.865,1.000,1.000,1.000,1.000,1.000,1.323,1.250,1.666,0.937,1.000,
     11.000,0.937,0.978,1.250,1.500,1.000,0.918,1.023,1.000,1.000,1.000,
     10.703,1.184,0.818,0.865,1.250,0.900,1.000,0.978,1.000,1.323,0.937,
     11.097,1.250,0.625,1.323,1.285,0.978,1.323,1.000,1.071,1.406,1.250,
     11.097,1.323,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,
     11.000,0.937,0.900,1.216,0.608,1.071,1.000,1.000,0.849,0.789,0.978,
     10.608,1.184,1.250,0.776,1.000,1.000,1.046,0.592,0.643,0.608,0.803,
     11.023,0.714,1.406,0.833,1.000,1.000,0.592,0.789,0.978,0.625,0.803,
     10.608,1.000,1.000,0.776,0.937,0.682,0.738,0.714,0.726,1.000,1.000,
     11.000,1.000,0.726,0.803,0.652,0.865,0.608,1.000,1.000,1.323,1.046,
     11.071,1.071,1.023,0.608,0.738,1.000,1.000,0.937,0.625,1.250,0.833,
     10.625,1.216,0.672,1.216,1.023,1.000,1.000,0.643,1.406,0.818,0.750,
     10.882,1.023,0.672,1.000,1.000,0.672,0.849,0.865,0.672,1.023,1.000,
     21.000,1.000,1.000,1.000/
      END