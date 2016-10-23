C   13/03/84 410282057  MEMBER NAME  FAMPDS   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE FAMPDS
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. OLSSON   20/07/83 :  DISPLAY FAMP RESULTS
C
C        MOD  J. OLSSON    9/12/83 :
C   LAST MOD  C. BOWDERY  14/03/84 :  RECOMMENTING. USES FAMPCO
C   LAST MOD  J. OLSSON   26/10/84 :  NAME 'FAMP' NOW ACCEPTED
C
C        DISPLAY ROUTINE FOR RESULTS OF ONLINE FAMP ANALYSIS
C        FAMP:  FAST AMSTERDAM MICRO PROCESSOR
C
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cgraph.for"
#include "cdata.for"
#include "cgeo1.for"
C
      COMMON / CWORK2 / HWORK(80)
      COMMON / CJTRIG / PI,TWOPI
C
      DATA VELH /.2998E-4/
      DATA INAME /'BK17'/
      DATA JNAME /'FAMP'/
      DATA HPLUS/'+ '/,HMINUS/'- '/
*** PMF 17/11/99: add variables needed for emulation of DESYLIB routine 'CORE'  
      CHARACTER cHWORK*160
      EQUIVALENCE (cHWORK,HWORK(1))
*** PMF(end)
C
C------------------  C O D E  ------------------------------------------
C
C                            DISPLAY PATREC FITS FROM FAMP BANK 'BK17'
C
      IFAMP = IDATA( IBLN( 'FAMP' ) )
      IF( IFAMP .GT. 0 ) GO TO 2
      IFAMP = IDATA( IBLN( 'BK17' ) )
      IF( IFAMP .GT. 0 ) GO TO 2
        WRITE(6,1)
   1    FORMAT(' NO FAMP (''BK17'') BANK AVAILABLE')
        RETURN
C
   2  IF( LASTVW .LT. 4 ) GO TO 4
        WRITE(6,3)
   3    FORMAT(' FAMP RESULTS DISPLAY NOT POSSIBLE IN THIS VIEW')
        RETURN
C
   4  LH2 = IFAMP * 2
      ND  = HDATA(LH2+7)
      IF( ND .GT. 0 ) GO TO 6
        WRITE(6,5)
   5    FORMAT(' NO INNER DETECTOR INFORMATION IN FAMP BANK (''BK17'')')
        RETURN
C
   6  CONTINUE
      NTR  = HDATA( LH2 + ND + 4 )
      NTRT = HDATA( LH2 + ND + 5 )
      IF( NTR .GT. 0 ) GO TO 20
        IF( NTR .LT. 0 ) WRITE(6,10)
  10      FORMAT(' NO FAMP PATTERN RECOGNITION WAS PERFORMED')
        IF( NTR .EQ. 0 ) WRITE(6,14)
  14      FORMAT(' NO TRACKS WERE FOUND BY FAMP PATTERN RECOGNITION')
        RETURN
C
C                            THERE ARE FAMP PATREC DATA TO BE DISPLAYED
C
  20  LORT = HDATA( LH2 + ND + 6 )
      LTRT = HDATA( LH2 + ND + 7 )
C
      IPLT = LH2 + ND + LORT - LTRT
C
C                            LOOP OVER ALL THE TRACKS AND DRAW THEM
C
      VELHB = ABS( VELH * BKGAUS )
C
      DO  100  ITR = 1,NTR
        IPLT  = IPLT + LTRT
        IF( HDATA(IPLT) .LT. 0 ) GO TO 100
C
C                            FAMPCO EXTRACTS COORDINATES IN MM
C
        CALL FAMPCO( IPLT, X1, Y1, Z1, X2, Y2, Z2, RR )
C
        DIST2 = (X1 - X2)**2 + (Y1 - Y2)**2
C
        XM    = X1 + (X2 - X1) / 2.0
        YM    = Y1 + (Y2 - Y1) / 2.0
C
        SAGIT = DIST2 / (8.0 * RR)
        REST  = RR - SAGIT
        DIST  = SQRT( DIST2 )
        COSD  = (X2 - X1) / DIST
        SIND  = (Y2 - Y1) / DIST
        X0    = XM + (SIND * REST)
        Y0    = YM - (COSD * REST)
        PHI0  = ATAN2( Y0,X0 )
        RMIN  = SQRT( X0**2 + Y0**2 ) - ABS( RR )
C+++
C       WRITE(6,21) ITR,X1,Y1,X2,Y2,XM,YM
  21    FORMAT(' ITR ',I3,' XY1 XY2 XYM ',3(2E13.5,2X))
C       WRITE(6,22) SAGIT,DIST,REST,X0,Y0,PHI0,RMIN
  22    FORMAT(' SG D REST XY0 FI0 RM ',5E13.5,2X,F6.3,E13.5)
C+++
C
C                            FAMPFT IS A VERSION OF CIRDIS (CIRCLE DISP)
C
        CALL FAMPFT( 25, RR, RMIN, PHI0, X1, Y1, X2, Y2, 0.0, 0.0 )
C
C                            WRITE TRACK NUMBER AT (X2,Y2)
C
        CALL TRNUMB( ITR, 0, X2, Y2, 0.0 )
 100  CONTINUE
C
C                            PRINT OUT TRACK INFORMATION ON SCREEN
C
C                            HEADER OF TRACK INFO LISTING
C
      XXX = XMIN + 0.82 * (XMAX - XMIN)
      YYY = YMIN + 0.76 * (YMAX - YMIN)
      CALL XXXYYY( XXX, YYY, SSS, 0 )
      YYY = YYY - 12.0 * SSS
      IF( LASTVW .EQ. 2 ) YYY = YYY - 2.0 * SSS
C
      CALL CORE(HWORK,80)
      WRITE(cHWORK,200) INAME,NTR   ! PMF 17/11/99: UNIT=10 changed to cHWORK
 200  FORMAT(' BANK ',A4,' NR OF TRACKS ',I3)
      CALL SYSSYM( XXX, YYY, SSS, HWORK, 27, 0.0 )
C
      YYY = YYY - 2.0 * SSS
      CALL CORE(HWORK,80)
      WRITE(cHWORK,205)             ! PMF 17/11/99: UNIT=10 changed to cHWORK
 205  FORMAT('NR +-  PTRANS NFI NZ')
      CALL SYSSYM( XXX, YYY, SSS, HWORK, 22, 0.0 )
C
      YYY = YYY - 1.5 * SSS
      CALL CORE(HWORK,80)
      WRITE(cHWORK,210)             ! PMF 17/11/99: UNIT=10 changed to cHWORK
 210  FORMAT(' X1 Y1 Z1')
      CALL SYSSYM( XXX, YYY, SSS, HWORK, 9, 0.0 )
C
      YYY = YYY - 1.5 * SSS
      CALL CORE(HWORK,80)
      WRITE(cHWORK,215)             ! PMF 17/11/99: UNIT=10 changed to cHWORK
 215  FORMAT(' X2 Y2 Z2')
      CALL SYSSYM( XXX, YYY, SSS, HWORK, 9, 0.0 )
      YYY = YYY - 4.5 * SSS
C
C                            LOOP OVER THE TRACK PARAMETERS AGAIN
C
      IPLT = LH2 + ND + LORT - LTRT
      DO  300  ITR = 1,NTR
        IPLT    = IPLT + LTRT
        IF( HDATA(IPLT) .LT. 0 ) GO TO 300
C
C                            FAMPCO EXTRACTS COORDINATES IN MM
C
        CALL FAMPCO( IPLT, X1, Y1, Z1, X2, Y2, Z2, RR )
C
        NFI     = HDATA(IPLT + 11)
        NZ      = HDATA(IPLT + 15)
        PTRANS  = VELHB * ABS( RR )
C
C                            DETERMINE SIGN
C
        HSIGN   = HPLUS
        IF( RR * BKGAUS .LT. 0.0 ) HSIGN = HMINUS
C
C                            WRITE TRACK PARAMETERS ON SCREEN
C
        YYY = YYY - 1.5 * SSS
        CALL CORE(HWORK,80)
        WRITE(cHWORK,310) ITR,HSIGN,PTRANS,NFI,NZ ! PMF 17/11/99: UNIT=10 changed to cHWORK
 310    FORMAT(I2,1X,A2,1X,F7.2,1X,I2,1X,I2)
        CALL SYSSYM( XXX, YYY, SSS, HWORK, 19, 0.0 )
C
        YYY = YYY - 1.5 * SSS
        CALL CORE(HWORK,80)
        WRITE(cHWORK,320) X1,Y1,Z1  ! PMF 17/11/99: UNIT=10 changed to cHWORK
 320    FORMAT(3(F7.1,1X))
        CALL SYSSYM( XXX, YYY, SSS, HWORK, 24, 0.0 )
C
        YYY = YYY - 1.5 * SSS
        CALL CORE(HWORK,80)
        WRITE(cHWORK,320) X2,Y2,Z2  ! PMF 17/11/99: UNIT=10 changed to cHWORK
        CALL SYSSYM( XXX, YYY, SSS, HWORK, 24, 0.0 )
 300  CONTINUE
C
      RETURN
      END
