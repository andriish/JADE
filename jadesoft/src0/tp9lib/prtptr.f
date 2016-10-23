C   19/11/84 707261808  MEMBER NAME  PRTPTR   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE  PRTPTR( IBANK, JUN1, JUN2, JUN3, JUN4, JUN5 )
C-----------------------------------------------------------------------
C
C   AUTHOR:    C. BOWDERY    20/11/84 : PRINTS OUT TPTR BANK 'IBANK'
C
C LAST MOD:    C. BOWDERY     2/10/86 : UNIT NUMBERS ARE PARAMETERS
C
C      INPUT:     IBANK  = BOS NUMBER OF TPTR BANK TO BE PRINTED
C                          IF IBANK = 0, IT PRINTS ALL TPTR BANKS
C                 JUN1   = LOGICAL UNIT NUMBER FOR OUTPUT STREAM 1
C                 JUN2   = LOGICAL UNIT NUMBER FOR OUTPUT STREAM 2
C                 JUN3   = LOGICAL UNIT NUMBER FOR OUTPUT STREAM 3
C                 JUN4   = LOGICAL UNIT NUMBER FOR OUTPUT STREAM 4
C                 JUN5   = LOGICAL UNIT NUMBER FOR OUTPUT STREAM 5
C
C     I M P O R T A N T :   ROUTINE USES  5  OUTPUT STREAMS
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
      LUN1 = JUN1
      LUN2 = JUN2
      LUN3 = JUN3
      LUN4 = JUN4
      LUN5 = JUN5
C
      IF( JUN1 .LT. 1  .OR.  JUN1 .GT. 99 ) LUN1 = 6
      IF( JUN2 .LT. 1  .OR.  JUN2 .GT. 99 ) LUN2 = 6
      IF( JUN3 .LT. 1  .OR.  JUN3 .GT. 99 ) LUN3 = 6
      IF( JUN4 .LT. 1  .OR.  JUN4 .GT. 99 ) LUN4 = 6
      IF( JUN5 .LT. 1  .OR.  JUN5 .GT. 99 ) LUN5 = 6
C
C                            IBANK = 0  MEANS PRINT ALL BANKS
C
      IF( IBANK .GT. 0 ) GO TO 4
        CALL BDAR( 'TPTR', NBKS, IARR, 100 )
        IF( NBKS .GT. 0 ) GO TO 2
C
          WRITE(LUN1,1)
          WRITE(LUN2,1)
          WRITE(LUN3,1)
          WRITE(LUN4,1)
          WRITE(LUN5,1)
   1      FORMAT(/' ***  NO TPTR BANKS EXIST TO BE PRINTED '/)
          RETURN
C
   2    IBK = 1
        ITPTRN = IARR( IBK )
        WRITE(LUN1,3) NBKS
        WRITE(LUN2,3) NBKS
        WRITE(LUN3,3) NBKS
        WRITE(LUN4,3) NBKS
        WRITE(LUN5,3) NBKS
   3    FORMAT(//' ***  ',I3,' TPTR  BANKS FOLLOW:'//)
        GO TO 8
C
C                            IBANK > 0 MEANS PRINT TPTR/IBANK
C
   4  IBK = IBANK
      CALL CLOC( ITPTRN, 'TPTR', IBK )
C
      IF( ITPTRN .GT. 0 ) GO TO 6
        WRITE(LUN1,5) IBK
        WRITE(LUN2,5) IBK
        WRITE(LUN3,5) IBK
        WRITE(LUN4,5) IBK
        WRITE(LUN5,5) IBK
   5    FORMAT(/' ***  WARNING  ***  PRTPTR FOUND NO TPTR/',I2,' BANK'/)
        RETURN
C
   6  WRITE(LUN1,7) IBK
      WRITE(LUN2,7) IBK
      WRITE(LUN3,7) IBK
      WRITE(LUN4,7) IBK
      WRITE(LUN5,7) IBK
   7  FORMAT(//' ***  TPTR  BANK ',I3,' FOLLOWS:'//)
C
C                           PRINT A TPTR BANK
C
   8  ITPTR2 = ITPTRN * 2
C
      NUMBK  = IDATA( ITPTRN -  2 )
      LENGTH = IDATA( ITPTRN      )
C
      IVTX   = HDATA( ITPTR2 +  1 )
      IVTX2  = HDATA( ITPTR2 +  2 )
      IFLAG  = HDATA( ITPTR2 +  3 )
      ITRKN  = HDATA( ITPTR2 +  4 )
C
      NLGCL  = HDATA( ITPTR2 +  5 )
      LGCL1  = HDATA( ITPTR2 +  6 )
      LGCL2  = HDATA( ITPTR2 +  7 )
C
      NMUCL  = HDATA( ITPTR2 +  8 )
      MUCL1  = HDATA( ITPTR2 +  9 )
      MUCL2  = HDATA( ITPTR2 + 10 )
      MUCL3  = HDATA( ITPTR2 + 11 )
      MUCL4  = HDATA( ITPTR2 + 12 )
C
      IFWCL  = HDATA( ITPTR2 + 13 )
      ISPR1  = HDATA( ITPTR2 + 14 )
      ISPR2  = HDATA( ITPTR2 + 15 )
      ISPR3  = HDATA( ITPTR2 + 16 )
C
      ITSTO  = HDATA( ITPTR2 + 17 )
      IOTFL  = HDATA( ITPTR2 + 18 )
C
      X0     = ADATA( ITPTRN + 10 )
      Y0     = ADATA( ITPTRN + 11 )
      Z0     = ADATA( ITPTRN + 12 )
      SIGX0  = ADATA( ITPTRN + 13 )
      SIGY0  = ADATA( ITPTRN + 14 )
      SIGZ0  = ADATA( ITPTRN + 15 )
C
      DIS    = ADATA( ITPTRN + 16 )
      DRSR   = ADATA( ITPTRN + 17 )
      DZSZ   = ADATA( ITPTRN + 18 )
      CHISR  = ADATA( ITPTRN + 19 )
      IDOFR  = IDATA( ITPTRN + 20 )
      CHISZ  = ADATA( ITPTRN + 21 )
      IDOFZ  = IDATA( ITPTRN + 22 )
C
      CHARG  = ADATA( ITPTRN + 23 )
      P      = ADATA( ITPTRN + 24 )
      SIGP   = ADATA( ITPTRN + 25 )
      ITSTD  = IDATA( ITPTRN + 26 )
C
      DX     = ADATA( ITPTRN + 27 )
      DY     = ADATA( ITPTRN + 28 )
      DZ     = ADATA( ITPTRN + 29 )
      SIGDX  = ADATA( ITPTRN + 30 )
      SIGDY  = ADATA( ITPTRN + 31 )
      SIGDZ  = ADATA( ITPTRN + 32 )
      SPARE1 = ADATA( ITPTRN + 33 )
C
      INMAS  = HDATA( ITPTR2 + 67 )
      ITYPE  = HDATA( ITPTR2 + 68 )
C
      AMASS  = ADATA( ITPTRN + 35 )
      ETOT   = ADATA( ITPTRN + 36 )
      ESH    = ADATA( ITPTRN + 37 )
      SIGESH = ADATA( ITPTRN + 38 )
C
      IQSH   = HDATA( ITPTR2 + 77 )
      ICLAS  = HDATA( ITPTR2 + 78 )
C
      CHISD1 = ADATA( ITPTRN + 40 )
C                                               SHORT BANK, LENGTH = 40
      IF( LENGTH .EQ. 40 ) GO TO 10
C
      CHISD2 = ADATA( ITPTRN + 41 )
      SPARE2 = ADATA( ITPTRN + 42 )
      NMUHIT = HDATA( ITPTR2 + 85 )
      IACCFL = HDATA( ITPTR2 + 86 )
      MUQUAL = HDATA( ITPTR2 + 87 )
C
      MUSP1  = HDATA( ITPTR2 + 88 )
      MUSP2  = HDATA( ITPTR2 + 89 )
      MUSP3  = HDATA( ITPTR2 + 90 )
      MUSP4  = HDATA( ITPTR2 + 91 )
C
      MUSP5  = HDATA( ITPTR2 + 92 )
      MUSP6  = HDATA( ITPTR2 + 93 )
      MUSP7  = HDATA( ITPTR2 + 94 )
      MUSP8  = HDATA( ITPTR2 + 95 )
      MUCUTS = HDATA( ITPTR2 + 96 )
C
      AMULEN = ADATA( ITPTRN + 49 )
      ABSMM  = ADATA( ITPTRN + 50 )
C
      IF( LENGTH .EQ. 50 ) GO TO 10
C
      ABSG   = ADATA( ITPTRN + 51 )
      ELOSS  = ADATA( ITPTRN + 52 )
      ABSPI  = ADATA( ITPTRN + 53 )
C
      ELOSSP = ADATA( ITPTRN + 54 )
      PROBMU = ADATA( ITPTRN + 55 )
      PROBPI = ADATA( ITPTRN + 56 )
      SPARE3 = ADATA( ITPTRN + 57 )
C
      IQTOF  = IDATA( ITPTRN + 58 )
      TOF    = ADATA( ITPTRN + 59 )
      PLEN   = ADATA( ITPTRN + 60 )
      BETA   = ADATA( ITPTRN + 61 )
      SIGB   = ADATA( ITPTRN + 62 )
C
      TOFMAS = ADATA( ITPTRN + 63 )
      SIGMAS = ADATA( ITPTRN + 64 )
      CHISP1 = ADATA( ITPTRN + 65 )
      CHISK1 = ADATA( ITPTRN + 66 )
      CHISM1 = ADATA( ITPTRN + 67 )
      CHISE1 = ADATA( ITPTRN + 68 )
C
      DEDXTF = ADATA( ITPTRN + 69 )
      SPARE4 = ADATA( ITPTRN + 70 )
      SPARE5 = ADATA( ITPTRN + 71 )
C
      IQDEDX = IDATA( ITPTRN + 72 )
      DEDX   = ADATA( ITPTRN + 73 )
      SIGDE  = ADATA( ITPTRN + 74 )
      CHISP2 = ADATA( ITPTRN + 75 )
      CHISK2 = ADATA( ITPTRN + 76 )
      CHISM2 = ADATA( ITPTRN + 77 )
      CHISE2 = ADATA( ITPTRN + 78 )
C
      NTYPE2 = IDATA( ITPTRN + 79 )
      ISPR4  = IDATA( ITPTRN + 80 )
C
  10  WRITE(LUN1,17) NUMBK, LENGTH, IVTX, IVTX2, IFLAG,
     +               ITRKN, NLGCL, LGCL1, LGCL2, NMUCL,
     +               MUCL1, MUCL2, MUCL3, MUCL4, IFWCL,
     +               ISPR1, ISPR2, ISPR3, ITSTO, IOTFL,
     +               X0,    Y0,    Z0,    SIGX0, SIGY0,
     +               SIGZ0, DIS,   DRSR
C
      WRITE(LUN2,19) DZSZ , CHISR, IDOFR, CHISZ, IDOFZ,
     +               CHARG, P,     SIGP,  ITSTD, DX,
     +               DY,    DZ,    SIGDX, SIGDY, SIGDZ,
     +               SPARE1,INMAS, ITYPE, AMASS, ETOT
C
C                            PRINTOUT DEPENDS ON LENGTH OF BANK
C
      IF( LENGTH .GT. 40 ) GO TO 12
        WRITE(LUN3,21) ESH, SIGESH, IQSH, ICLAS, CHISD1
        WRITE(LUN4,23)
        WRITE(LUN5,25)
        GO TO 30
C
  12  IF( LENGTH .GT. 50 ) GO TO 14
        WRITE(LUN3,21) ESH,   SIGESH,IQSH,  ICLAS, CHISD1,
     +                 CHISD2,SPARE2,NMUHIT,IACCFL,MUQUAL,
     +                 MUSP1, MUSP2, MUSP3, MUSP4, MUSP5,
     +                 MUSP6, MUSP7, MUSP8, MUCUTS,AMULEN,
     +                 ABSMM
        WRITE(LUN4,23)
        WRITE(LUN5,25)
        GO TO 30
C
  14  WRITE(LUN3,21) ESH,   SIGESH, IQSH, ICLAS, CHISD1,
     +               CHISD2,SPARE2,NMUHIT,IACCFL,MUQUAL,
     +               MUSP1, MUSP2, MUSP3, MUSP4, MUSP5,
     +               MUSP6, MUSP7, MUSP8, MUCUTS,AMULEN,
     +               ABSMM, ABSG,  ELOSS, ABSPI, ELOSSP,
     +               PROBMU
C
      WRITE(LUN4,23) PROBPI,SPARE3,IQTOF, TOF,   PLEN,
     +               BETA,  SIGB,  TOFMAS,SIGMAS,CHISP1,
     +               CHISK1,CHISM1,CHISE1,DEDXTF,SPARE4,
     +               SPARE5
      WRITE(LUN5,25) IQDEDX,DEDX,  SIGDE, CHISP2,CHISK2,
     +               CHISM2,CHISE2,NTYPE2,ISPR4
C
  17  FORMAT(1X,I3,2X,I2,2X,2I3,I5,4I3,2X,5I2,2X,I3,3I2,2I3,8F8.2)
  19  FORMAT(1X,2F8.2,I3,F8.2,I3,F6.1,2F8.2,I3,6F8.2,F4.1,2I3,2F8.2)
  21  FORMAT(1X,2F8.2,2I3,2F8.2,F4.1,I4,I2,I4,2X,8I2,1X,I3,7F8.2)
  23  FORMAT(1X,F8.2,F4.1,I3,F6.1,F8.2,F7.3,8F8.2,2F4.1)
  25  FORMAT(1X,I4,6F8.2,2I3)
C
  30  IF( IBANK .GT.    0 ) RETURN
      IF( IBK   .GE. NBKS ) RETURN
      IBK = IBK + 1
      ITPTRN = IARR( IBK )
      GO TO 8
C
      END
