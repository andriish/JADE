C   14/05/80 403210033  MEMBER NAME  TOFVW    (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE xTOFVW ! PMF 11/04/00 TOFVW->xTOFVW
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. OLSSON   14/05/80 :  TOF V MOMENTUM DIAGRAM FOR 1 EVENT
C
C        MOD: J. OLSSON   28/05/82 :
C   LAST MOD: C. BOWDERY  19/03/84 :  IMPROVED LAYOUT
C
C        SHOW TOF VS MOMENTUM DIAGRAM FOR ONE EVENT
C
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C-----------------------------------------------------------------------
C                            MACRO CGRAPH .... GRAPHICS COMMON
C-----------------------------------------------------------------------
C
      LOGICAL DSPDTL,SSTPS,PSTPS,FREEZE
C
      COMMON / CGRAPH / JUSCRN,NDDINN,NDDOUT,IDATSV(11),ICREC,MAXREC,
     +                  LSTCMD,ACMD,LASTVW,ISTANV,
     +                  SXIN,SXAX,SYIN,SYAX,XMIN,XMAX,YMIN,YMAX,
     +                  DSPDTL(30),SSTPS(10),PSTPS(10),FREEZE(30),
     +                  IREADM,LABEL,LSTPS(10),IPSVAR
C
C------- END OF MACRO CGRAPH -------------------------------------------
C
C----------------------------------------------------------------------
C             MACRO CDATA .... BOS COMMON.
C
C             THIS MACRO ONLY DEFINES THE IDATA/HDATA/ADATA NAMES.
C             THE ACTUAL SIZE OF /BCS/ IS FIXED ON MACRO CBCSMX
C             OR BY OTHER MEANS. A DEFAULT SIZE OF 40000 IS GIVEN HERE.
C
C----------------------------------------------------------------------
C
      COMMON /BCS/ IDATA(40000)
      DIMENSION HDATA(80000),ADATA(40000),IPNT(50)
      EQUIVALENCE (HDATA(1),IDATA(1),ADATA(1)),(IPNT(1),IDATA(55))
      EQUIVALENCE (NWORD,IPNT(50))
C
C------------------------ END OF MACRO CDATA --------------------------
C---------  MACRO CHSYM    SYMBOLS USED IN GRAPHICS WRITING -----
      COMMON /CHSYM/ HSYM(36)
C------ END MACRO CHSYM
C
      COMMON / CHEADR / HEAD(108)
      COMMON / CWORK1 / HMW(132)
      COMMON / CWORK  / NR,RAW(5,42),NC,ICRT1(5,42),NTRK,ICRT2(50),
     +                  TRK(5,50),ITRC(50),NTC,ITRK(5,42),
     +                  INFM(4),IR(14,50)
C
      DIMENSION    RE(14,50) , ARMAS(4)
      EQUIVALENCE  (RE(1,1),IR(1,1))
C
      DATA ARMAS / 0.000511, 0.139567, 0.49367, 0.93828 /
*** PMF 17/11/99: add variables needed for emulation of DESYLIB routine 'CORE'  
      CHARACTER cHMW*264
      EQUIVALENCE (cHMW,HMW(1))
*** PMF(end)
C
C-------------------  C O D E  -----------------------------------------
C
C                            OPTION 2 DRAWS A FULL SIZE PICTURE
C                            OPTION 4 DRAWS A SMALL PICTURE
C
      NN = ACMD
      IF( NN .NE. -4 ) GO TO 1
        CALL TWINDO(3265,4095,-30,800)
        GO TO 2
C
   1    CALL ERASE
        CALL TWINDO(0,4095,0,4095)
C
   2  XMINRE = XMIN
      XMAXRE = XMAX
      YMINRE = YMIN
      YMAXRE = YMAX
      XMIN   = -0.3
      YMIN   = -0.45
      XMAX   =  1.7
      YMAX   =  2.15
C
C                            DEFINE THE COORDINATE SYSTEM
C
      CALL DWINDO(XMIN,XMAX,YMIN,YMAX)
C
C                            DRAW DIAGRAM FRAME
C
      FRAC  = ALOG10( 5.0 )
      XVMAX = 2.0 - FRAC * 0.5
      CALL MOVEA(   0.0, 0.0 )
      CALL DRAWA( XVMAX, 0.0 )
      CALL DRAWA( XVMAX, 1.2 )
      CALL DRAWA(   0.0, 1.2 )
      CALL DRAWA(   0.0, 0.0 )
C
C                            DRAW SCALE MARKERS EXCEPT FOR SMALL VIEW
C
      IF( NN .EQ. -4 ) GO TO 6
      CALL CHRSIZ(2)
      CALL MOVEA( -0.08, -0.08 )
      CALL CORE(HMW,5)
      WRITE(cHMW,4) ! PMF 17/11/99: JUSCRN changed to cHMW
   4  FORMAT(' 0.05')
      CALL EOUTST(5,HMW)
C
   6  FRAC1 = (1.0 - FRAC) * 0.5
      CALL MOVEA( FRAC1, 0.0  )
      CALL DRAWA( FRAC1, 0.03 )
C
      IF( NN .EQ. -4 ) GO TO 10
      CALL MOVEA( FRAC1 - 0.06, -0.08 )
      CALL CORE(HMW,4)
      WRITE(cHMW,8) ! PMF 17/11/99: JUSCRN changed to cHMW
   8  FORMAT(' 0.1')
      CALL EOUTST(4,HMW)
C
  10  CALL MOVEA( FRAC1 + FRAC * 0.5, 0.0   )
      CALL DRAWA( FRAC1 + FRAC * 0.5, 0.02  )
      CALL MOVEA( FRAC1 + 0.5       , 0.0   )
      CALL DRAWA( FRAC1 + 0.5       , 0.03  )
C
      IF( NN .EQ. -4 ) GO TO 14
      CALL MOVEA( FRAC1 + 0.47      , -0.08 )
      CALL CORE(HMW,2)
      WRITE(cHMW,12) ! PMF 17/11/99: JUSCRN changed to cHMW
  12  FORMAT(' 1')
      CALL EOUTST(2,HMW)
C
  14  CALL MOVEA( FRAC1 + (1.0 + FRAC) * 0.5, 0.0  )
      CALL DRAWA( FRAC1 + (1.0 + FRAC) * 0.5, 0.02 )
      CALL MOVEA( FRAC1 + 1.0, 0.0  )
      CALL DRAWA( FRAC1 + 1.0, 0.03 )
C
      IF( NN .EQ. -4 ) GO TO 18
      CALL MOVEA( FRAC1 + 0.96, -0.08 )
      CALL CORE(HMW,3)
      WRITE(cHMW,16) ! PMF 17/11/99: JUSCRN changed to cHMW
  16  FORMAT(' 10')
      CALL EOUTST(3,HMW)
C
  18  CALL MOVEA( FRAC1 + (2.0 + FRAC) * 0.5, 0.   )
      CALL DRAWA( FRAC1 + (2.0 + FRAC) * 0.5, 0.02 )
C
      IF( NN .EQ. -4 ) GO TO 22
      CALL MOVEA( FRAC1 + 1.45, -0.08 )
      CALL CORE(HMW,4)
      WRITE(cHMW,20) ! PMF 17/11/99: JUSCRN changed to cHMW
  20  FORMAT(' 100')
      CALL EOUTST(4,HMW)
C
      CALL MOVEA( -0.09, 0.18 )
      CALL CORE(HMW,3)
      WRITE(cHMW,21) ! PMF 17/11/99: JUSCRN changed to cHMW
  21  FORMAT(' .2')
      CALL EOUTST(3,HMW)
C
  22  CALL MOVEA( 0.0,  0.2 )
      CALL DRAWA( 0.03, 0.2 )
C
      IF( NN .EQ. -4 ) GO TO 26
      CALL MOVEA( -0.09, 0.38 )
      CALL CORE(HMW,3)
      WRITE(cHMW,24) ! PMF 17/11/99: JUSCRN changed to cHMW
  24  FORMAT(' .4')
      CALL EOUTST(3,HMW)
C
  26  CALL MOVEA( 0.0,  0.4 )
      CALL DRAWA( 0.03, 0.4 )
C
      IF( NN .EQ. -4 ) GO TO 30
      CALL MOVEA( -0.09, 0.58)
      CALL CORE(HMW,3)
      WRITE(cHMW,28) ! PMF 17/11/99: JUSCRN changed to cHMW
  28  FORMAT(' .6')
      CALL EOUTST(3,HMW)
C
  30  CALL MOVEA( 0.0,  0.6 )
      CALL DRAWA( 0.03, 0.6 )
C
      IF( NN .EQ. -4 ) GO TO 34
      CALL MOVEA( -0.09, 0.78 )
      CALL CORE(HMW,3)
      WRITE(cHMW,32) ! PMF 17/11/99: JUSCRN changed to cHMW
  32  FORMAT(' .8')
      CALL EOUTST(3,HMW)
C
  34  CALL MOVEA( 0.0,  0.8 )
      CALL DRAWA( 0.03, 0.8 )
      IF( NN .EQ. -4 ) GO TO 38
      CALL MOVEA( -0.09, 0.98 )
      CALL CORE(HMW,3)
      WRITE(cHMW,36) ! PMF 17/11/99: JUSCRN changed to cHMW
  36  FORMAT(' 1.')
      CALL EOUTST(3,HMW)
C
  38  IF( NTRK .NE. 0 ) GO TO 40
        CALL TRMOUT(80,'NO TRACKS TO DISPLAY^')
        GO TO 998
C
  40  DO  100  ITR = 1,NTRK
        IF( IR(2,ITR) .NE. 1  .AND.  IR(2,ITR) .NE. 2 ) GO TO 100
        BETA  = RE(6,ITR)
        DBETA = RE(7,ITR)
        PBETA = ABS(TRK(4,ITR))
        IF( BETA  .LT.   0.0  .OR.   BETA .GT. 1.3  ) GO TO 100
        IF( PBETA .GT. 100.0  .OR.  PBETA .LT. 0.05 ) GO TO 100
        PBETA = ALOG10(PBETA)
        PBETA = FRAC1 + (1.0 + PBETA) * 0.5
C
        CALL PLYGON( 10, 0.012, PBETA, BETA, 0 )
C
        CALL MOVEA( PBETA, BETA + 0.012 )
        CALL DRAWA( PBETA, BETA + DBETA )
        CALL MOVEA( PBETA, BETA - 0.012 )
        CALL DRAWA( PBETA, BETA - DBETA )
C
        DESIZE = 0.032
        IF( NN .EQ. -4 ) DESIZE = 0.11
        CALL DNUM( ITR, PBETA + 0.02, BETA + 0.02, DESIZE, 0.0 )
C
 100  CONTINUE
C
C                            DRAW "THEORETICAL" CURVES IN PLOT
C
      CALL MOVEA( 0.0, 1.0 )
      CALL DASHA( XVMAX, 1.0, 14 )
C
      DO  200  IMS = 2,4
        FMASS  = ARMAS(IMS)
        FMASS2 = FMASS**2
        PXX    = 0.0
        IMMM   = 0
        PADD   = 0.002
        DO  150  IMM = 1,1500
          IF( IMM .EQ.  500 ) PADD = 0.02
          IF( IMM .EQ. 1000 ) PADD = 0.2
          PXX = PXX + PADD
          PX  = PXX
          IF( PX .GT. 100.0  .OR.  PX .LT. 0.05 )  GO TO 150
          BETA = PX / SQRT( PX * PX + FMASS2 )
          IF( BETA .LT. 0.0  .OR.  BETA .GT. 1.0 ) GO TO 150
          PX   = ALOG10(PX)
          PX   = FRAC1 + (1.0 + PX) * 0.5
          IMMM = IMMM  + 1
          IF( IMMM .EQ. 1 ) CALL MOVEA( PX, BETA )
          IF( IMMM .NE. 1 ) CALL DRAWA( PX, BETA )
 150    CONTINUE
 200  CONTINUE
C
      IF( NN .EQ. -4 ) GO TO 998
      CALL MOVEA( 1.5, -0.12 )
      CALL CHRSIZ(2)
      CALL CORE(HMW,6)
      WRITE(cHMW,209) ! PMF 17/11/99: JUSCRN changed to cHMW
 209  FORMAT(' GeV/c')
      CALL EOUTST(6,HMW)
      CALL MOVEA( -0.24, 0.9 )
      CALL CORE(HMW,6)
      WRITE(cHMW,211) ! PMF 17/11/99: JUSCRN changed to cHMW
 211  FORMAT(' Beta ')
      CALL EOUTST(6,HMW)
C
C                            WRITE CAPTION
C
      INDES = -2
      CALL CAPMRK(INDES,IESUM)
998   CALL TWINDO(0,4095,0,4095)
      XMIN = XMINRE
      XMAX = XMAXRE
      YMIN = YMINRE
      YMAX = YMAXRE
C
      CALL SETSCL(LASTVW)
C
      RETURN
      END
