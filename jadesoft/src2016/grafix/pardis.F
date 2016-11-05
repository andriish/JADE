C   08/08/85 807251617  MEMBER NAME  PARDIS   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE PARDIS(NN,A,B,C,D,XB,YB,XE,YE,AZ,BZ,XCE,YCE)
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. OLSSON    1/03/79 :  DRAW A PARABOLA SEGMENT
C
C       MOD:   J. OLSSON   10/02/84 :
C       MOD:   C. BOWDERY   8/06/84 :  NEW COMMAND NUMBERS
C       MOD:   J. HAGEMANN 22/10/84 :  FOR COMMAND VC
C       MOD:   C. BOWDERY   9/08/85 :  FIX CDTL 23 (LG EXTRAPOLATION)
C       MOD:   J. HAGEMANN 20/09/85 :  FOR MAGNIFIED VIEW VC
C       MOD:   J. HAGEMANN 10/12/85 :  FOR SIMFIT AND COMFIT
C       MOD:   J. HAGEMANN 14/04/86 :  CORRECT DISPLAY OF TRACKS IN
C                                      MAGNIFIED VRX VIEW
C       MOD:   J. HAGEMANN 11/07/86 :  CONVERT SOME CODE TO DOUBLE
C                                      PRECISION WERE NECESSARY
C  LAST MOD:   J. HAGEMANN 27/10/87 :  FOR VRX VIEW
C
C       DRAWS A SEGMENT FROM XB,YB TO XE,YE OF THE PARABOLA WITH PARA-
C       METERS A-D. NN IS THE SEGMENTATION OF THE POLYGON APPROXIMATION.
C       THE PARAMETERS ARE: A = ANGLE BETWEEN LOCAL X-AXIS AND DETECTOR
C                           B = X COORDINATE OF LOCAL MINIMUM POINT.
C                           C = Y COORDINATE OF LOCAL MINIMUM POINT.
C                           D = PARABOLA PARAMETER IN  Y = D * X*X
C        XCE,YCE IS THE POINT TO WHICH CLOSEST APPROACH IS FOUND.
C        THE PARABOLA IS "PROJECTED" ACCORDING TO THE Z-COORDINATE, TO
C        AGREE WITH THE PERSPECTIVE "CYLINDER" VIEW.
C        THE Z-COORDINATE IS GOTTEN FROM THE R-Z FIT:  Z = AZ + BZ*R
C
C        IF DSPDTL(23) = TRUE, TRACKS ARE PROLONGED TO LEADGLASS LIMITS.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL DSPDTM, FL18, FL22, FL24
      LOGICAL FLVCDO
      LOGICAL TBIT
C
      REAL*8 P,Q,DET,BIGR,COSFI,FIHELP,Y1,Y2,Y3,DXPB,SIGN,U,V
C
#include "cgraph.for"
#include "cgrscl.for"
#include "cgeo1.for"
C
      COMMON / CGRAP2 / BCMD,DSPDTM(30)
      COMMON / CGVCDO / FLVCDO(20)
      COMMON / CPROJ  / XMINR,XMAXR,YMINR,YMAXR,IPRJC,FL18,FL22,FL24
      COMMON/CWORK2/HWORK(40),JNDEX,NTR,LTR,ITR,IPO,ICNT,NBK,NCLST,NWPCL
      COMMON / CVCPV  / ICD, DFX, DFY, IRC
      COMMON / CHEADR / HEAD(108)
C
      REAL*8 DTWOPI / 3.141592653589793D0 /
      DATA ZDEEP / 5800.0 /
C
C------------------  C O D E  ------------------------------------------
C
      COSA = COS(A)
      SINA = SIN(A)
C
C                            DEFINE MAX RADIUS FOR DRAWING LINES
C
      RTS  = RLG
      IF( LASTVW .EQ.  1 ) RTS = RTOF
      IF( LASTVW .EQ. 17 ) RTS = 0.33*(XMAX-XMIN)
      RDT = ABS(XMAX-XMIN)/ABS(XMAXST(20)-XMINST(20))
      IF( RDT .LT. 0.5 ) RDT = 1.0
      IF( LASTVW .EQ. 20 ) RTS = R1ROH*RDT*1.1
      XRV = 0.0
      YRV = 0.0
      IF( .NOT.FLVCDO(11) .OR. LASTVW.NE.17 ) GO TO 5
         CALL VTXCRV( INT(HEAD(18)) , XRV, YRV, DXR, DYR ) !PMF 08/11/99: add run argument HEAD(18)
    5 CONTINUE
C
C                            IF NOT VRES AND NOT DRAWING RES BACK TO 0,0
C
      IF( .NOT. DSPDTL(19)  .AND.  LSTCMD .NE. 52 ) GO TO 100
C--
C-- FIND POINT OF CLOSEST APPROACH TO XCE,YCE,BY SOLVING 3D DEGREE EQUA-
C-- TION ARISING FROM THE CONDITION D/DX (DIST)  =  0
C-- USE CARDAN'S FORMULA
C--
      Q   = DBLE( (XCE-B)*COSA + (YCE-C)*SINA)
      P   = DBLE(-(XCE-B)*SINA + (YCE-C)*COSA)
      P   = (1.0D0 - 2.0D0*DBLE(D)*P)/(6.0D0*DBLE(D)*DBLE(D))
      Q   = - Q/(4.*D*D)
      DET = Q*Q + P*P*P
      IF( DET .GT. 0.D0 ) GO TO 50
C                                     DET < 0,  3 REAL SOLUTIONS
         BIGR   = DSQRT(DABS(P))
         IF( Q .LT. 0.0D0 ) BIGR = -BIGR
         COSFI  = Q/(BIGR**3)
         FIHELP = DARCOS(COSFI)
         IF(FIHELP .LT. 0.0D0 ) FIHELP = FIHELP + DTWOPI
         FIHELP = FIHELP/3.0D0
         BIGR   = -2.0D0*BIGR
         Y1     = BIGR*DCOS(FIHELP)
         Y2     = BIGR*DCOS(FIHELP+DTWOPI/3.0D0)
         Y3     = BIGR*DCOS(FIHELP+2.0D0*DTWOPI/3.0D0)
         DXPB   = DMIN1(Y1,Y2,Y3)
         XPB    = SNGL(DXPB)
         GO TO 200
   50 CONTINUE
      DET  = DSQRT(DET)
      SIGN = 1.0D0
      IF( -Q+DET .LT. 0.0D0 ) SIGN = -1.0D0
      U    = (((-Q+DET)*SIGN)**0.3333333333333333D0)*SIGN
      SIGN = 1.0D0
      IF( -Q-DET .LT. 0.0D0 ) SIGN = -1.0D0
      V    = (((-Q-DET)*SIGN)**0.3333333333333333D0)*SIGN
      DXPB = U + V
      XPB  = SNGL(DXPB)
      GO TO 200
  100 CONTINUE
      XPB  = (XB-B)*COSA + (YB-C)*SINA
  200 CONTINUE
      XPE  = (XE-B)*COSA + (YE-C)*SINA
      YPB  = D*XPB*XPB
      DELX = ABS(XPE - XPB)/FLOAT(NN)
      IF( LASTVW .GE. 17 .AND. LASTVW .LE. 19 ) DELX = DELX * .01
      XP   = XPB*COSA - YPB*SINA + B
      YP   = XPB*SINA + YPB*COSA + C
      RP   = SQRT(XP*XP + YP*YP)
      ZP   = AZ + BZ*RP
      IF( LASTVW .NE. 14 ) GO TO 300
         FP = (ZDEEP - ZP)/(ZDEEP + ZLGPL)
         XP = FP*XP
         YP = FP*YP
  300 CONTINUE
      CALL MOVEA(-XP,YP)
      DO 500  I = 1,NN
         XPB = XPB + DELX
         YPB = D*XPB*XPB
         XP  = XPB*COSA - YPB*SINA + B
         YP  = XPB*SINA + YPB*COSA + C
         RP  = SQRT(XP*XP + YP*YP)
         RP1 = SQRT((XP-XRV)**2 + (YP-YRV)**2)
         ZP  = AZ + BZ*RP
         IF( LASTVW .NE. 14) GO TO 400
            FP = (ZDEEP - ZP)/(ZDEEP + ZLGPL)
            XP = FP*XP
            YP = FP*YP
  400    CONTINUE
         IF( LASTVW .NE. 20 ) GO TO 450
             IF( RP1 .LT. 35.0 .OR. RP1 .GT. 75.0 ) GO TO 450
                XTN = XP
                YTN = YP
  450    CONTINUE
         IF( RP1 .GT. RTS ) GO TO 600
         IF(.NOT.TBIT(ICD,15) .OR. (TBIT(ICD,15).AND..NOT.FLVCDO(10)) )
     +                                        CALL DRAWA(-XP,YP)
         IF( TBIT(ICD,15) .AND. FLVCDO(10) .AND. IRC .LT. 0 )
     +                                        CALL DRAWA(-XP,YP)
         IF( TBIT(ICD,15) .AND. FLVCDO(10) .AND. IRC .GE. 0 )
     +                                        CALL DASHA(-XP,YP,34)
  500 CONTINUE
  600 CONTINUE
      IF( LASTVW.NE.20 ) CALL TRNUMB(ITR,0,XP,YP,ZP)
      IF(LASTVW.EQ.20.AND.TBIT(ICD,14)) CALL TRNUMB(ITR,0,XTN,YTN,ZP)
      IF(LASTVW.EQ.20.AND..NOT.TBIT(ICD,14)) CALL TRNUMB(ITR,0,XB,YB,ZP)
C
      CALL MOVEA(-XP,YP)
C
C                             EXTEND TO TOF OR LG LIMIT
C
      IF(.NOT.DSPDTL(23) .OR. LASTVW.GT.16)  RETURN
      IFLGX = 0
      R2    = RP
      Z2    = ZP
  650 CONTINUE
      XPB   = XPB + DELX
  700 CONTINUE
      YPB   = D*XPB*XPB
      XP    = XPB*COSA - YPB*SINA + B
      YP    = XPB*SINA + YPB*COSA + C
      RP    = SQRT(XP*XP + YP*YP)
      ZP    = AZ + BZ*RP
      IF( IFLGX .NE. 0 ) GO TO 850
C                                   CHECK LIMITS
      IF( ABS(ZP) .LT. ZLGPL ) GO TO 800
         IFLGX = 1
         FAT   = ABS((ABS(ZP) - ZLGPL)/(ZP - Z2))
  750    CONTINUE
         XPB   = XPB - FAT*DELX
         GO TO 700
  800 CONTINUE
      IF( RP .LT. RTS ) GO TO 850
         IFLGX = 2
         FAT   = (RP-RTS)/(RP-R2)
         GO TO 750
  850 CONTINUE
      IF( LASTVW .NE. 14 ) GO TO 900
         FP = (ZDEEP - ZP)/(ZDEEP + ZLGPL)
         XP = FP*XP
         YP = FP*YP
  900 CONTINUE
      R2 = RP
      Z2 = ZP
      IF(.NOT.TBIT(ICD,15) .OR. (TBIT(ICD,15).AND..NOT.FLVCDO(10)) )
     +                                        CALL DRAWA(-XP,YP)
      IF( TBIT(ICD,15) .AND. FLVCDO(10) .AND. IRC .LT. 0 )
     +                                        CALL DRAWA(-XP,YP)
      IF( TBIT(ICD,15) .AND. FLVCDO(10) .AND. IRC .GE. 0 )
     +                                        CALL DASHA(-XP,YP,34)
      IF( IFLGX .EQ. 0 ) GO TO 650
      IF( IFLGX .EQ. 1 ) CALL PLYGON(6,10.,-XP,YP,0)
      RETURN
      END
