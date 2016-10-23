C   01/11/84 807241257  MEMBER NAME  DRAWID   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE DRAWID( IVIEW )
C-----------------------------------------------------------------------
C
C   AUTHOR:    C. BOWDERY    26/04/84 :  DRAW INNER DETECTOR
C
C       MOD:   J. HAGEMANN   09/10/84 :  HATCH TANK WALL ON SCREEN
C  LAST MOD:   J. HAGEMANN   11/04/88 :  BIG DOTS FOR SIGNAL WIRES
C                                        WHEN REQUESTED
C
C     DRAW THE INNER DETECTOR FOR THE VIEW IVIEW.
C       IVIEW = 1   : R PHI VIEW
C       IVIEW = 2   : RZ VIEWS
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL  DSPDTM, FLVCDO
C
      COMMON / CJTRIG / PI,TWOPI
      COMMON / CJCELL / NCELL(3),NWIRES(3)
      COMMON / CGRAP2 / BCMD,DSPDTM(30),ISTVW,JTVW
      COMMON / CGVCDO / FLVCDO(20)
C
C-----------------------------------------------------------------------
C                            MACRO CJDRCH .... JET CHAMBER CONSTANTS.
C-----------------------------------------------------------------------
C
      COMMON / CJDRCH / RDEC(4),PSIIN(3),RINCR(3),FIRSTW(3),FSENSW(3),
     +                  RDEPTH,SWDEPL,YSUSPN,TIMDEL(2,3),ZMAX,ZOFFS,
     +                  ZRESOL,ZNORM,ZAL,ZSCAL,DRIDEV,DRICOS,DRISIN,
     +                  PEDES,TZERO(3),DRIROT(96,2),SINDRI(96,2),
     +                  COSDRI(96,2),DRIVEL(96,2),T0FIX(3),
     +                  ABERR(8), DUMJDC(20)
C
C      BLOCK DATA SET TO MC VALUES, KALIBR WILL SET REAL DATA VALUES
C--->  A CHANGE OF THIS COMMON MUST BE DONE SIMULTANEOUSLY WITH  <----
C--->  A CHANGE OF THE BLOCK DATA                                <----
C
C--------------------------- END OF MACRO CJDRCH -----------------------
C
C-----------------------------------------------------------------------
C                            MACRO CGEO1 .... JADE GEOMETRY
C-----------------------------------------------------------------------
C
      COMMON / CGEO1 / BKGAUS,
     +                 RPIP,DRPIP,XRLPIP,   RBPC,DRBPC,XRLBPC,
     +                 RITNK,DRITNK,XRLTKI, R0ROH,DR0ROH,XR0ROH,
     +                 R1ROH,DR1ROH,XR1ROH, R2ROH,DR2ROH,XR2ROH,
     +                 R3ROH,DR3ROH,XR3ROH, ROTNK,DROTNK,XRLTKO,
     +                 RTOF,DRTOF,XRTOF,    RCOIL,DRCOIL,XRCOIL,
     +                 ZJM,DZJM,XRZJM,ZJP,DZJP,XRZJP,ZTKM,DZTKM,XRZTKM,
     +                 ZTKP,DZTKP,XRZTKP,ZBPPL,ZBPMI,ZTOFPL,ZTOFMI,
     +                 XRJETC,RLG,ZLGPL,ZLGMI,OUTR2,CTLIMP,
     +                 CTLIMM,DELFI,BLXY,BLZ,BLDEP,ZENDPL,ZENDMI,DEPEND,
     +                 XHOL1,XHOL2,YHOL1,YHOL2,BLFI
C
C------------------------- END OF MACRO CGEO1 --------------------------
C
C-----------------------------------------------------------------------
C                            MACRO CGEO2 .... JADE TAGGING GEOMETRY
C-----------------------------------------------------------------------
C
      COMMON / CGEO2 / FENDC,XYHOL1,XYHOL2,BLDPFW,ZMINBL,ZPLUBL,
     +                 XSC(2),YSC(2),RSC(2),ZMISC(2),ZPLSC(2),DZSC,
     +                 CHX(3,4),CHY(3,4),CHZ(3,4),WLEN,PITCH,WZDIS
C
C--------------------------- END OF MACRO CGEO2 ------------------------
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
C
C-----------------------------------------------------------------------
C                            MACRO CGRSCL .... GRAPHICS VIEW SCALES
C-----------------------------------------------------------------------
C
      COMMON  / CGRSCL / XMINST(30), XMAXST(30), YMINST(30)
C
C------- END OF MACRO CGRSCL -------------------------------------------
C
C
C------------------  C O D E  ------------------------------------------
C
C                            CDTL 4   DRAW JET CHAMBER WALLS
C                            CDTL 5   DRAW JET CHAMBER WIRES
C                            CDTL 6   DRAW CROSSES FOR JET CHAMBER HITS
C
      GO TO ( 10 , 20 ) , IVIEW
C
C                            R PHI VIEW
C
  10  IF( ( .NOT. DSPDTL(4) )  .AND.  ( .NOT. DSPDTL(5) ) ) GO TO 9
C
      RDT = 0.5/ABS(XMAX-XMIN)*ABS(XMAXST(20)-XMINST(20))
      NRING = 3
      IF( LASTVW .EQ. 20 ) NRING = 1
      DO  2  J = 1,NRING
        RADD   = RINCR(J)
        RR1    = FSENSW(J)
        NN     = NCELL(J)
        NWE    = NWIRES(J)
        RR2    = RR1 + RADD*(NWE - 1)
        DELPHI = TWOPI/NN
C
        DO  4  K = 1,NN
          DO  6  L = 4,5
            IF( .NOT. DSPDTL(L) ) GO TO 6
            RR11 = RR1
            IF( L .EQ. 4 ) RR11 = RR11 - 15.
            RR22 = RR2
            IF( L .EQ. 4 ) RR22 = RR22 + 15.
C
            PHI  = DELPHI * (K - 1) + 0.5 * DELPHI * (L - 4)
            CSN  = COS(PHI)
            SSN  = SIN(PHI)
            XL1  = RR11 * CSN
            YL1  = RR11 * SSN
            IF( L .EQ. 5 ) GO TO 7
C
            CALL DRAMOV(XL1,YL1,RR22*CSN,RR22*SSN,0)
            GO TO 6
    7       CONTINUE
            STEPX = RADD * CSN
            STEPY = RADD * SSN
            DO  8  M = 1,NWE
              CALL POINTA(XL1,YL1)
              IF( LASTVW .NE. 20 .OR. .NOT. FLVCDO(19) )  GO TO 1008
                 CALL PLYGON( 11, RDT, XL1, YL1, 0 )
1008          XL1 = XL1 + STEPX
              YL1 = YL1 + STEPY
   8        CONTINUE
   6      CONTINUE
   4    CONTINUE
   2  CONTINUE
C
   9  DDD   = 10.24 * 25.4 / ( XMAX - XMIN )
      NN = 525.0 * DDD
      IF( NN .LT. 40 ) NN = 40
      CALL PLYGON(NN,RITNK,0.,0.,0)
      CALL PLYGON(NN,RITNK+DRITNK,0.,0.,0)
      IF( DSPDTM(15) )
     +              CALL DRHATC(IVIEW, 200, RITNK, RITNK+DRITNK, 0., 0.)
C
      NN = 750.0 * DDD
      IF( NN .LT. 40 ) NN = 40
      CALL PLYGON(NN,ROTNK,0.,0.,0)
      CALL PLYGON(NN,ROTNK+DROTNK,0.,0.,0)
      IF( DSPDTM(15) )
     +              CALL DRHATC(IVIEW, 500, ROTNK, ROTNK+DROTNK, 0., 0.)
C
      RETURN
C
C                            Z VIEWS
C
C                            FIRST DRAW BOX
C
  20  CALL DRAMOV(ZTKM, RITNK,ZTKM, ROTNK,0)
      CALL DRAWA( ZTKP, ROTNK)
      CALL DRAWA( ZTKP, RITNK)
      CALL DRAWA( ZTKM, RITNK)
      CALL DRAMOV(ZTKM,-RITNK,ZTKM,-ROTNK,0)
      CALL DRAWA( ZTKP,-ROTNK)
      CALL DRAWA( ZTKP,-RITNK)
      CALL DRAWA( ZTKM,-RITNK)
      X1 = -ZMAX + ZOFFS
      X2 =  ZMAX + ZOFFS
C
C                            DRAW WIRES OF INNER DETECTOR
C
      IF( .NOT. DSPDTL(5) ) RETURN
C
      DO  21  J = 1,3
        RADD = RINCR(J)
        R    = FSENSW(J)
        NWE  = NWIRES(J) - 1
        DO  22  K = 1,2
          R = R + (K-1)*NWE*RADD
          CALL DRAMOV(X1, R,X2, R,0)
          CALL DRAMOV(X1,-R,X2,-R,0)
  22    CONTINUE
  21  CONTINUE
C
      RETURN
      END
