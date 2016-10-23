C   20/11/84            MEMBER NAME  PRTPVX   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE  PRTPVX( IBANK, JUNIT )
C-----------------------------------------------------------------------
C
C   AUTHOR:    C. BOWDERY    20/11/84 : PRINTS OUT TPVX BANK 'IBANK'
C
C LAST MOD:    C. BOWDERY     2/10/86 : UNIT NUMBER IS A PARAMETER
C
C
C      INPUT:     JUNIT  = LOGICAL UNIT NUMBER FOR OUTPUT
C                 IBANK  = NUMBER OF TPVX BANK TO BE PRINTED.
C                          IF IBANK = 0, IT PRINTS ALL TPVX BANKS
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2  (H)
C
#include "cdata.for"
C
      INTEGER  IARR(100)
C
C------------------  C O D E  ------------------------------------------
C
      IUNIT = JUNIT
      IF( JUNIT .LT. 1  .OR.  JUNIT .GT. 99 ) IUNIT = 6
C
C                            IBANK = 0  MEANS PRINT ALL BANKS
C
      IF( IBANK .GT. 0 ) GO TO 4
        CALL BDAR( 'TPVX', NBKS, IARR, 100 )
        IF( NBKS .GT. 0 ) GO TO 2
C
          WRITE(IUNIT,1)
   1      FORMAT(/' ***  NO TPVX BANKS EXIST TO BE PRINTED '/)
          RETURN
C
   2    IBK = 1
        ITPVXN = IARR( IBK )
        WRITE(IUNIT,3) NBKS
   3    FORMAT(//' ***  ',I3,' TPVX  BANKS FOLLOW:'//)
        GO TO 8
C
C                            IBANK > 0 MEANS PRINT TPVX/IBANK
C
   4  IBK = IBANK
      CALL CLOC( ITPVXN, 'TPVX', IBK )
C
      IF( ITPVXN .GT. 0 ) GO TO 6
        WRITE(IUNIT,5) IBK
   5    FORMAT(/' ***  WARNING  ***  PRTPVX FOUND NO TPVX/',I2,' BANK'/)
        RETURN
C
   6  WRITE(IUNIT,7) IBK
   7  FORMAT(//' ***  TPVX  BANK ',I3,' FOLLOWS:'//)
C
C                           PRINT A TPVX BANK
C
   8  ITPVX2 = ITPVXN * 2
C
      NUMBK  = IDATA( ITPVXN -  2 )
      LENGTH = IDATA( ITPVXN      )
C
      ITRKN  = HDATA( ITPVX2 +  1 )
      IFLAG  = HDATA( ITPVX2 +  2 )
C
      XVTX   = ADATA( ITPVXN +  2 )
      YVTX   = ADATA( ITPVXN +  3 )
      ZVTX   = ADATA( ITPVXN +  4 )
      SXVTX  = ADATA( ITPVXN +  5 )
      SYVTX  = ADATA( ITPVXN +  6 )
      SZVTX  = ADATA( ITPVXN +  7 )
      CHISQ  = ADATA( ITPVXN +  8 )
C                                           2 POSSIBLE INTERPRETATIONS
      IDOF   = IDATA( ITPVXN +  9 )
      ADOF   = ADATA( ITPVXN +  9 )
C
      COST   = ADATA( ITPVXN + 10 )
C
      ICHARG = HDATA( ITPVX2 + 21 )
      NSEC   = HDATA( ITPVX2 + 22 )
      NPTRK  = HDATA( ITPVX2 + 23 )
      NMTRK  = HDATA( ITPVX2 + 24 )
      NNTRK  = HDATA( ITPVX2 + 25 )
      NATRK  = HDATA( ITPVX2 + 26 )
      NGAMS  = HDATA( ITPVX2 + 27 )
      NEP    = HDATA( ITPVX2 + 28 )
      NMU    = HDATA( ITPVX2 + 29 )
      NHADRN = HDATA( ITPVX2 + 30 )
C
      K1 = ITPVX2 + 31
      K2 = ITPVX2 + 30 + NSEC
C
      MODE = MOD( IFLAG, 10 )
C
      IF( MODE .EQ. 2  .OR.  MODE .EQ. 3 ) GO TO 20
        WRITE(IUNIT,15) NUMBK, LENGTH, ITRKN, IFLAG,
     +                  XVTX,  YVTX,  ZVTX,
     +                  SXVTX, SYVTX, SZVTX, CHISQ, IDOF,
     +                  COST,  ICHARG,NSEC,  NPTRK, NMTRK,
     +                  NNTRK, NATRK, NGAMS, NEP,   NMU,
     +                  NHADRN, ( HDATA(K),K=K1,K2)
  15    FORMAT(1X,I2,2X,I2,2X,I2,I4,7F8.2,I8,F8.4,I3,9I3,10(4I3/117X))
        GO TO 30
C
C                           ADOF   INSTEAD OF IDOF
C
  20    WRITE(IUNIT,25) NUMBK, LENGTH, ITRKN, IFLAG,
     +                  XVTX,  YVTX,  ZVTX,
     +                  SXVTX, SYVTX, SZVTX, CHISQ, ADOF,
     +                  COST,  ICHARG,NSEC,  NPTRK, NMTRK,
     +                  NNTRK, NATRK, NGAMS, NEP,   NMU,
     +                  NHADRN, ( HDATA(K),K=K1,K2)
  25    FORMAT(1X,I2,2X,I2,2X,I2,I4,7F8.2,F8.2,F8.4,I3,9I3,10(4I3/117X))
C
  30  IF( IBANK .GT.    0 ) RETURN
      IF( IBK   .GE. NBKS ) RETURN
      IBK = IBK + 1
      ITPVXN = IARR( IBK )
      GO TO 8
C
      END
