C   04/08/81 807241320  MEMBER NAME  DRAWER   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE DRAWER
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. OLSSON        ?     :  ROUTINE FOR DIAGNOSTICS
C                                        DRAWS POINTS CIRCLES LINES ETC
C
C       MOD:   J. HAGEMANN   19/09/85 :  SMALL BUG CORRECTED FOR VIEW
C                                        VRX (INDEX=17)
C       MOD:   J. HAGEMANN   30/01/86 :  INCLUDING PARABOLA AND FIT-
C                                        POINTS
C  LAST MOD:   J. HAGEMANN   07/03/86 :  OPTION 7: LINE GIVEN BY A
C                                        POINT, SLOPE AND LENGTH
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cgraph.for"
C
      COMMON / CGAMOD / MODEGA, GAHEX
      COMMON / CJTRIG / PI,TWOPI
C
      character*2 HYES,  HBLK, HCD
      DATA HYES / 'Y ' /
      DATA HBLK / '  ' /
C
C-----------------  C O D E  -------------------------------------------
C
      NN = ACMD
      NN = NN + 1
      IF( NN .NE. 1 ) GO TO 100
      GO TO 101
  102 CALL TRMOUT(80,'CODE 0:  RETURN^')
      CALL TRMOUT(80,'CODE 1:  LIST^')
      CALL TRMOUT(80,'CODE 2:  DRAW POINT X,Y ON SCREEN^')
      CALL TRMOUT(80,'CODE 3:  DRAW CIRCLE WITH RADIUS RAD AND CENTER X,
     $Y ^')
      CALL TRMOUT(80,'CODE 4:  DRAW LINE BETWEEN X1 Y1  AND  X2 Y2^')
      CALL TRMOUT(80,'CODE 5:  DRAW PARABOLA (X0,Y0,C,ALPHA) FROM XPB TO
     $ XPE^')
      CALL TRMOUT(80,'CODE 6:  DRAW HIT IN DETECTOR SYSTEM WITH COORDINA
     $TES GIVEN IN FIT SYSTEM^')
      CALL TRMOUT(80,'CODE 7:  DRAW LINE GIVEN BY A POINT, SLOPE AND LEN
     $GTH^')
  101 CALL TRMOUT(80,'ENTER OPTION.  1=LIST^')
      NN = TERNUM(DUM) + 1
  100 IF( NN .GE. 1 .AND. NN .LE. 8 ) GO TO 200
      GO TO 101
  200 GO TO (9999,102,300,400,500,600,700,800),NN
  300 BIG  = 40.0
      IF( LASTVW .EQ. 17 ) BIG = 0.4
      IF( LASTVW .EQ. 20 ) BIG = 16.0
      CALL TRMOUT(80,' ENTER X AND Y COOORDINATES OF POINT^')
      GO TO 9000
  400 CALL TRMOUT(80,' ENTER X,Y OF CENTER, AND CIRCLE RADIUS ^')
      GO TO 9000
  500 BIG  = 40.0
      IF( LASTVW .EQ. 17 ) BIG = 0.4
      IF( LASTVW .EQ. 20 ) BIG = 16.0
      CALL TRMOUT(80,' ENTER X1 Y1   X2 Y2  AS ENDPOINTS OF LINE^')
      GO TO 9000
  600 IF( MODEGA .EQ. 1 ) CALL CLEAR
      CALL TRMOUT(80,' ENTER X0,Y0 OF PARABOLA, C-PARAMETER, ANGLE (RADI
     $AN), XPB AND XPE^')
      READ(5,*) XS,YS,RAD,RD2,XPB,XPE
      GO TO 9001
  700 IF( MODEGA .EQ. 1 ) CALL CLEAR
      BIG  = 12.0
      IF( LASTVW .EQ. 17 ) BIG = 0.2
      IF( LASTVW .EQ. 20 ) BIG = 8.0
      CALL TRMOUT(80,' FIRST ENTER FIT SYSTEM PARAMETER (X0, Y0 , CSROT,
     $ SNROT) :^')
      CALL REALNM(X0,Y0,CSROT,SNROT)
      GO TO 9001
  800 CONTINUE
      CALL TRMOUT(80,' ENTER X,Y , SLOPE  AND LENGTH OF LINE TO
     $ BE DRAWN^')
      READ(5,*) X0,Y0,TAN,RLEN
      GO TO 9001
C
 9000 CALL REALNM(XS,YS,RAD,RD2)
      IF( LASTVW.LT.4 .OR. LASTVW.EQ.17 .OR. LASTVW.EQ.20 ) XS = -XS
 9001 GO TO (9999,102,301,401,501,601,701,801),NN
C
  301 CALL MOVEA( XS-BIG,     YS )
      CALL DRAWA( XS+BIG,     YS )
      CALL MOVEA(     XS, YS-BIG )
      CALL DRAWA(     XS, YS+BIG )
      GO TO 101
  401 CALL PLYGON(300,RAD,XS,YS,0)
      GO TO 101
  501 IF( LASTVW.LT.4 .OR. LASTVW.EQ.17 .OR. LASTVW.EQ.20 ) RAD = -RAD
      CALL DRAMOV(XS,YS,RAD,RD2,0)
      CALL MOVEA(  XS-BIG,      YS )
      CALL DRAWA(  XS+BIG,      YS )
      CALL MOVEA(      XS,  YS-BIG )
      CALL DRAWA(      XS,  YS+BIG )
      CALL MOVEA( RAD-BIG,     RD2 )
      CALL DRAWA( RAD+BIG,     RD2 )
      CALL MOVEA(     RAD, RD2-BIG )
      CALL DRAWA(     RAD, RD2+BIG )
      GO TO 101
  601 COSA = COS(RD2)
      SINA = SIN(RD2)
      YPB  = RAD*XPB*XPB
      DELX = (XPE - XPB)/100.0
      XP   = XPB*COSA - YPB*SINA + XS
      YP   = XPB*SINA + YPB*COSA + YS
      CALL MOVEA( -XP, YP )
      DO 650 I = 1, 100
         XPB  = XPB + DELX
         YPB  = RAD*XPB*XPB
         XP   = XPB*COSA - YPB*SINA + XS
         YP   = XPB*SINA + YPB*COSA + YS
         CALL DRAWA( -XP, YP )
  650 CONTINUE
      GO TO 101
C
  701 CALL TRMOUT(80,' ENTER HIT COORDINATES IN FIT SYSTEM :^')
      CALL REALNM(XH,YH,DUM1,DUM2)
      XDUM = XH
      XH   = XDUM*CSROT - YH*SNROT + X0
      YH   = XDUM*SNROT + YH*CSROT + Y0
      CALL MOVEA( -XH-BIG,     YH )
      CALL DRAWA( -XH+BIG,     YH )
      CALL MOVEA(     -XH, YH-BIG )
      CALL DRAWA(     -XH, YH+BIG )
      CALL TRMOUT(80,' OTHER HITS ?  <Y>, N^')
      CALL TRMIN( 2, HCD )
      IF( HCD .EQ. HBLK ) HCD = HYES
      IF( HCD .EQ. HYES ) GO TO 701
      GO TO 101
C
  801 ALF = ATAN(TAN)
      SN  = SIN(ALF)
      CS  = COS(ALF)
      XA  = X0 - RLEN*CS
      XE  = X0 + RLEN*CS
      YA  = Y0 - RLEN*SN
      YE  = Y0 + RLEN*SN
      IF( LASTVW .GT. 4 .AND. LASTVW .NE. 17 .AND. LASTVW .NE. 20 )
     &                                        GO TO 802
         XA = -XA
         XE = -XE
  802 CALL DRAMOV( XA, YA, XE, YE, 0 )
      GO TO 101
C
 9999 RETURN
      END
