C   12/02/88 808021742  MEMBER NAME  BBCORR   (SOURCE)      FORTRAN


C    LG CALIBRATION CORRECTION: PRODUCT OF BBLEAK, BBCLCE, BBTHLD
C        TAKING INTO ACCOUNT JAPANESE CORRECTION 'BRLGN'

C           CALLED BY CALCOR

C           D.PITZL  15.2.88

C=======================================================================
      SUBROUTINE BBCORR ( NRING, FCORR )
C=======================================================================

C    INPUT  : NRING = NUMBER OF LG-RING IN BARREL ( 0-31 )
C    OUTPUT : FCORR = CORRECTION FACTOR

      IMPLICIT INTEGER*2 (H)
C PMF 26/11/98 'f11god.patrecsr.for' replaced by 'cdata.for'
C               (see lganal.F)
C include "'f11god.patrecsr.for"
#include "cdata.for"

      DATA ICALLS / 0 /

      DIMENSION IRN (16)

      DIMENSION F35SF6 (16)
      DIMENSION F44SF6 (16)
      DIMENSION F35SF5 (16)


      DATA IRN /         0  ,   1  ,   2  ,   3  ,   4  ,   5  ,    6 ,
     +      7  ,   8 ,   9 ,   10 ,   11  ,  12  ,  13  ,  14  ,   15 /

C---        ECM = 35 GEV, LG WITH SF6 + SF6 ( 1986    ) :

      DATA F35SF6 /    0.860, 0.860, 0.860, 0.885, 0.895, 0.890, 0.890,
     +   0.890, 0.890, 0.885, 0.880, 0.895, 0.925, 0.960, 0.975, 0.980/

C---        ECM = 44 GEV, LG WITH SF6 + SF6 ( 1983-85 ) :

      DATA F44SF6 /    0.855, 0.855, 0.855, 0.880, 0.895, 0.890, 0.890,
     +   0.890, 0.885, 0.883, 0.875, 0.885, 0.915, 0.940, 0.965, 0.970/

C---        ECM = 35 GEV, LG ONLY SF5 ( 1979-82 ) :

      DATA F35SF5 /    0.865, 0.865, 0.865, 0.885, 0.905, 0.900, 0.900,
     +   0.900, 0.900, 0.905, 0.915, 0.925, 0.925, 0.925, 0.925, 0.930/
C
      IHEAD  = IDATA ( IBLN('HEAD') )
      NRUN   = HDATA ( 2 * IHEAD + 10 )
      NYEAR  = HDATA ( 2 * IHEAD +  8 )
      EBM    = EBEAM ( HDATA ( 2*IHEAD + 10 ) ) / 1000.0
C
      NRINGW = NRING
C---                                              SYMMETRY AROUND Z=0
      IF ( NRINGW .GT. 15 ) NRINGW = 31 - NRINGW
      NRINGW = NRINGW + 1
C
C---              BRANCH ACCORDING TO YEAR AND ENERGY
C
      IF ( NYEAR .GT. 1982 ) GOTO 20
         FCORR = F35SF5 ( NRINGW )
         IF ( ICALLS .EQ. 0 ) PRINT 1000, NYEAR, EBM
         ICALLS = 1
         GOTO 99
 20   CONTINUE
      IF ( EBM .LT. 19.75 ) GOTO 30
         FCORR = F44SF6 ( NRINGW )
         IF ( ICALLS .EQ. 0 ) PRINT 1000, NYEAR, EBM
         ICALLS = 1
         GOTO 99
 30   CONTINUE
         FCORR = F35SF6 ( NRINGW )
         IF ( ICALLS .EQ. 0 ) PRINT 1000, NYEAR, EBM
         ICALLS = 1
 99   CONTINUE

 1000 FORMAT ( T2,'JADELG.LOAD (BBCORR) CALLED FROM CALCOR',
     +       ' FOR DATA FROM ',I4,', EBEAM = ',F7.3, ' GEV' )

      RETURN
      END
