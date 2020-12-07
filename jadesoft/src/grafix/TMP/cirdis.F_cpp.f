C   01/11/84 807241309  MEMBER NAME  CIRDIS   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE CIRDIS(NN,CURV,RMIN,PHM,XB,YB,XE,YE,A,B,ITYP,XCE,YCE)
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. OLSSON    1/03/79 :  DRAW A CIRCLE SEGMENT
C
C       MOD:   J. OLSSON   10/02/84 :
C       MOD:   C. BOWDERY   8/06/84 :  NEW COMMAND NUMBERS
C       MOD:   J. HAGEMANN 22/10/84 :  FOR COMMAND VC
C       MOD:   C. BOWDERY   9/08/85 :  FIX CDTL 23 (LG EXTRAPOLATION)
C       MOD:   J. HAGEMANN 10/12/85 :  CORRECT TRACK NUMBER DISPLAY
C       MOD:   J. HAGEMANN 14/04/86 :  CORRECT DISPLAY OF TRACKS IN
C                                      MAGNIFIED VRX VIEW
C       MOD:   J. HAGEMANN 24/11/86 :  DASHED DISPLAY OF COMFIT-TRACKS
C       MOD:   J. HAGEMANN 05/06/87 :  DUE TO UPDATE OF STANDARD CIRDIS
C  LAST MOD:   J. HAGEMANN 27/10/87 :  FOR VRX VIEW
C
C        DRAWS A CIRCLE SEGMENT FROM XB,YB TO XE,YE OF THE CIRCLE WITH
C        PARAMETERS CURV,RMIN,PHM.    NN IS THE SEGMENTATION OF THE
C        POLYGON APPROXIMATION.
C        XCE,YCE IS THE POINT TO WHICH CLOSEST APPROACH IS FOUND
C        THE CIRCLE IS "PROJECTED" ACCORDING TO THE Z-COORDINATE, TO
C        AGREE WITH THE PERSPECTIVE "CYLINDER" VIEW.
C        THE Z-COORDINATE IS GOTTEN FROM THE R-Z FIT:  Z = A + B*R
C                        OR FROM THE Z-FI(HELIX) FIT:  Z = A + B*FI
C
C        IF DSPDTL(19) = TRUE, TRACKS ARE PROLONGED TO CLOSEST APPROACH
C                              TO ORIGIN.
C        IF DSPDTL(23) = TRUE, TRACKS ARE PROLONGED TO TOF OR LEAD GLASS
C                              RADIUS LIMIT.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL FLVCDO
      LOGICAL TBIT
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
C
      COMMON / CJTRIG / PI,TWOPI
      COMMON / CGVCDO / FLVCDO(20)
      COMMON/CWORK2/HWORK(40),JNDEX,NTR,LTR,ITR,IPO,ICNT,NBK,NCLST,NWPCL
      COMMON / CVCPV  / ICD, DFX, DFY, IRC
      COMMON / CHEADR / HEAD(108)
C
      DATA  ZDEEP / 5800.0 /
C     DATA  MUMAX,MUMAYP,MUMAYN / 3320.,3320.,-3020./
C
C------------------  C O D E  ------------------------------------------
C
C                            IS THE CURVATURE TOO SMALL?
C
      IF( ABS(CURV) .GT. 1.0E-08 ) GO TO 10
        CALL TRMOUT(44,'Curvature zero encountered in circle display')
        RETURN
C
  10  RAD  = ABS( 1.0/CURV )
      YC   = RAD + RMIN
      XC   = YC*COS(PHM)
      YC   = YC*SIN(PHM)
      RTS  = RLG
      ZZPL = ZLGPL
      ZZMI = ZLGMI
      IF( LASTVW .EQ.  1 ) RTS  = RTOF
      IF( LASTVW .EQ. 17 ) RTS  = 0.33*(XMAX-XMIN)
      IF( LASTVW .EQ. 17 ) ZZPL = RTS
      IF( LASTVW .EQ. 17 ) ZZMI = RTS
      RDT = ABS(XMAX-XMIN)/ABS(XMAXST(20)-XMINST(20))
      IF( RDT .LT. 0.5 ) RDT = 1.0
      IF( LASTVW .EQ. 20 ) RTS = R1ROH*RDT*1.1
      XRV = 0.0
      YRV = 0.0
      IF( .NOT.FLVCDO(11) .OR. LASTVW.NE.17 ) GO TO 5
         CALL VTXCRV( INT(HEAD(18)), XRV, YRV, DXR, DYR ) !PMF 08/11/99: add run argument HEAD(18)
    5 CONTINUE
C--
      PHMIT = ATAN2(YC-YCE,XC-XCE)
      FI1   = PHMIT - PI
      IF( .NOT.DSPDTL(19) .AND. LSTCMD.NE.52 ) FI1 = ATAN2(YB-YC,XB-XC)
      FI2   = ATAN2(YE-YC,XE-XC)
      IF( FI1 .LT. -PI) FI1 = FI1 + TWOPI
      FIB = FI1
      ARC = ARCMIN(FI2-FI1)
      IF( ARC*CURV .GT. 0.0 ) ARC = SIGN(TWOPI-ABS(ARC),-ARC)
      DEFI = ARC/NN
      IF( LASTVW.GE.17 .AND. LASTVW.LE.19 ) DEFI = DEFI * .01
      XP = - XC - RAD*COS(FI1)
      YP = YC + RAD*SIN(FI1)
      RP = SQRT(XP*XP + YP*YP)
      IF( ITYP .EQ. 1 ) GO TO 233
C        FIHEL = FI1
C        IF( FIHEL .LT. 0.0 ) FIHEL = FIHEL + TWOPI
C        FIHEL = ABS(FIHEL - FIHEL1) - FISTRT
C        ZP    = B*FIHEL + A
C        ZDEL  = B*ABS(DEFI)
C        GO TO 235
233   ZP = A + B*RP
235   IF( LASTVW .NE. 14 ) GO TO 499
         FP = (ZDEEP - ZP)/(ZDEEP + ZLGPL)
         XP = FP*XP
         YP = FP*YP
499   CALL MOVEA(XP,YP)
      DO 501  I = 1,NN
         FI1 = FI1 + DEFI
         XP  = - XC - RAD*COS(FI1)
         YP  =   YC + RAD*SIN(FI1)
         RP  = SQRT(XP*XP + YP*YP)
C                          /  (-XP-XRV)**2 = (XP+XRV)**2
         RP1 = SQRT((XP+XRV)**2 + (YP-YRV)**2)
         ZP = A + B*RP
237      IF( LASTVW .NE. 14 ) GO TO 507
            FP = (ZDEEP - ZP)/(ZDEEP + ZLGPL)
            XP = FP*XP
            YP = FP*YP
507      CONTINUE
         IF( LASTVW .NE. 20 ) GO TO 502
             IF( RP1 .LT. 35.0 .OR. RP1 .GT. 75.0 ) GO TO 502
                XTN = XP
                YTN = YP
502      CONTINUE
         IF( RP1 .GT. RTS ) GO TO 508
         IF(.NOT.TBIT(ICD,15) .OR. (TBIT(ICD,15).AND..NOT.FLVCDO(10)) )
     *                                        CALL DRAWA(XP,YP)
         IF( TBIT(ICD,15) .AND. FLVCDO(10) .AND. IRC .LT. 0 )
     *                                        CALL DRAWA(XP,YP)
         IF( TBIT(ICD,15) .AND. FLVCDO(10) .AND. IRC .GE. 0 )
     *                                        CALL DASHA(XP,YP,34)
501   CONTINUE
508   CONTINUE
      IF( LASTVW.NE.20 ) CALL TRNUMB(ITR,0,-XP,YP,ZP)
      IF(LASTVW.EQ.20.AND.TBIT(ICD,14)) CALL TRNUMB(ITR,0,-XTN,YTN,ZP)
      IF(LASTVW.EQ.20.AND..NOT.TBIT(ICD,14)) CALL TRNUMB(ITR,0,XB,YB,ZP)
C
      CALL MOVEA(XP,YP)

      IF(.NOT.DSPDTL(23) .OR. LASTVW.GT.16)  RETURN
C
C                            EXTENSION TO TOF OR PBG LIMIT
C
      IFLGX = 0
      NNN   = 0
      R2    = RP
      Z2    = ZP
551   FI1   = FI1 + DEFI
      NNN   = NNN + 1
      IF( NNN.GT.200 .OR. ABS(ARCMIN(FI1-PHM)).LT.ABS(DEFI) ) RETURN
555   XP = - XC - RAD*COS(FI1)
      YP =   YC + RAD*SIN(FI1)
      RP = SQRT(XP*XP + YP*YP)
      IF( RP .LT. RITNK ) RETURN
C     IF( ITYP .EQ. 1 ) GO TO 238
C        ZP = ZP + ZDEL
C        GO TO 239
238   ZP = A + B*RP
239   IF( IFLGX .NE. 0 ) GO TO 553
CHECK LIMITS
         IF( ABS(ZP) .LT. ZZPL ) GO TO 554
            IFLGX = 1
            FAT   = ABS((ABS(ZP) - ZLGPL)/(ZP - Z2))
552         FI1   = FI1 - FAT*DEFI
C           IF( ITYP .EQ. 2 ) ZDEL = -FAT*ZDEL
            GO TO 555
554      IF( RP .LT. RTS ) GO TO 553
            IFLGX = 2
            FAT   = (RP-RTS)/(RP-R2)
            GO TO 552
553   IF( LASTVW .NE. 14 ) GO TO 557
            FP = (ZDEEP - ZP)/(ZDEEP + ZLGPL)
            XP = FP*XP
            YP = FP*YP
557   R2 = RP
      Z2 = ZP
      IF(.NOT.TBIT(ICD,15) .OR. (TBIT(ICD,15).AND..NOT.FLVCDO(10)) )
     *                                        CALL DRAWA(XP,YP)
      IF( TBIT(ICD,15) .AND. FLVCDO(10) .AND. IRC .LT. 0 )
     *                                        CALL DRAWA(XP,YP)
      IF( TBIT(ICD,15) .AND. FLVCDO(10) .AND. IRC .GE. 0 )
     *                                        CALL DASHA(XP,YP,34)
      IF(IFLGX.EQ.0) GO TO 551
      IF(IFLGX.EQ.1) CALL PLYGON(6,10.,XP,YP,0)
      RETURN
      END
C*****************************
      FUNCTION ARCMIN(A)
C
C THIS IS A ROUTINE USED IN PLUTO PATREC. IT FINDS THE SHORTEST ARC
C     BETWEEN TWO ANGLES.
C
      COMMON/CJTRIG/ PI,TWOPI
C
      ARCMIN=A
      IF (ABS(A).LE.PI) GOTO 2
      ARCMIN=AMOD (A+5.*PI,TWOPI)-PI
    2 RETURN
      END
      DOUBLE PRECISION FUNCTION DRCMIN(AA)
      COMMON/CJTRIG/ PI,ZWEIPI
      REAL*8 AA
      DRCMIN=AA
      IF (DABS(AA).LE.PI) GOTO 3
      ARGUM = AA + 2.*ZWEIPI + PI
      DRCMIN=AMOD (ARGUM,ZWEIPI)-PI
    3 RETURN
      END
