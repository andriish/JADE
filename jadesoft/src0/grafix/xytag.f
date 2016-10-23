C   24/04/79 406231924  MEMBER NAME  XYTAG    (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE XYTAG( NO, X, Y, SLOPE )
C-----------------------------------------------------------------------
C
C    AUTHOR:   L. O'NEILL   24/04/79 :  COORDINATE CALCULATIONS FOR FW
C
C       MOD:   J. OLSSON    15/06/81 :  M.C. ERROR CORRECTED
C       MOD:   J. OLSSON     7/10/83 :
C       MOD:   J. NYE       24/05/84 :  USES TAGMRK
C  LAST MOD:   C. BOWDERY   23/06/84 :  FOR JOHN NYE'S CHANGE
C
C
C        THIS ROUTINE PROVIDES THE X AND Y COORDINATES (MM)
C        IN THE STANDARD DETECTOR COORDINATE SYSTEM,
C        OF THE CENTRE OF TAGGING SYSTEM COUNTER NUMBER "NB",
C        WHERE "NB" AND THE X AND Y COORDINATE VALUES APPEAR IN
C        ORDER IN THE ARGUMENT FIELD.
C
C        OBS: 1979-80 HAS BLOCK NRS 1-190
C        OBS: 1981-82 HAS BLOCK NRS 0-63
C        OBS: 1983-.. HAS BLOCK NRS 4-91. THIS IS PACKED INTO ARRAY 1-4800001500
C
C        THE COORDINATES REFER TO THE CORNER WHICH IS SUITABLE FOR
C        WRITING, I.E. LOWER LEFT CORNER
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
#include "cgraph.for"
#include "cwktag.for"
#include "cgeo2.for"
#include "cgeo3.for"
C
      COMMON / CJTRIG / PI,TWOPI,PIHALF
      COMMON / CWORK1 / R,FI,R1,FI1,X1,Y1,R2,FI2,X2,Y2,ZET,X3,Y3,X4,Y4
C
      DIMENSION  NCOLMN(6),AMID(6),WFACT(4)
C
      DATA  NCOLMN / 2,9, 18, 29, 37, 45 /
      DATA  AMID   / 1.0, 6.0, 14.0, 24.0, 33.5, 41.5 /
      DATA  WFACT  / -0.25, -0.75, 0.75, 0.25 /
      DATA  ICAL   / 0 /
C
C------------------  C O D E  ------------------------------------------
C
      IF( ICAL .NE. 0 ) GO TO 2121
      DFI  = PI*.25
      DDFI = DFI*.05
      ICAL = 1
C
C                            CALL TAGMRK TO CHOOSE 'MARK'
C                            IF IFLMRK = -1  ALREADY DONE
C
 2121 IF( IFLMRK .NE. -1 ) CALL TAGMRK(&2000)
      IF( (MARK .LT. -1) .OR. (MARK .GT. 2) ) CALL TAGMRK(&2000)
C
      IF( MARK .EQ. 1 ) GO TO 1000
      IF( MARK .EQ. 2 ) GO TO 2000
C
C                           1979-80 TAGGING APPARATUS
C
      NB = NO - 1
      IF((NB.GE.  0).AND.(NB.LE. 45)) GO TO 1
      IF((NB.GE. 48).AND.(NB.LE. 93)) GO TO 1
      IF((NB.GE. 96).AND.(NB.LE.141)) GO TO 1
      IF((NB.GE.144).AND.(NB.LE.189)) GO TO 1
101   WRITE(JUSCRN,100) NO
  100 FORMAT(' XYTAG CALLED WITH ILLEGAL BLOCK NUMBER',I10)
      X = 0.
      Y = 0.
      SLOPE = 0.
      RETURN
    1 CONTINUE
      IZ=-1
      IF(NB.GE.96) IZ=1
      IF(IZ.EQ.1) NB=NB-96
      IX=-1
      IF(NB.GE.48) IX=1
      IF(IX.EQ.1) NB=93-NB
      DO 202 I=1,6
      ICOL=I
      IF(NB.LE.NCOLMN(I)) GO TO 203
  202 CONTINUE
  203 CONTINUE
      X=(-6.5+ICOL)*FENDC
      Y=(AMID(ICOL)-NB)*FENDC
      SIGNY=1.
      IF(Y.LT.0.) SIGNY=-1.
      IF(ICOL.EQ.5) Y=Y+SIGNY*1.5*FENDC
      IF(ICOL.EQ.6) Y=Y+SIGNY*XYHOL2
      IF(ABS(Y).LT.(1.5*FENDC)) X=X+2.*FENDC-XYHOL1
      X=IX*IZ*X
      Y=IX*IZ*Y
      SLOPE = 0.
      RETURN
 1000 CONTINUE
C
C                             1981-82  TAGGING APPARATUS
C
      NB=NO
      IF((NB.GE.  0).AND.(NB.LE. 63)) GO TO 2
      GO TO 101
    2 CONTINUE
      IZ=-1
      IF(NB.GT.31) IZ=1
      IF(IZ.EQ.1) NB=NB-32
      IY=1
      IF(NB.GT.15) IY=-1
      IF(IY.EQ.-1) NB=31-NB
      IX=1
      IF(NB.GT.7) IX=-1
      IF(IX.EQ.-1) NB=15-NB
      MB=NB
      IF(NB.GT.1) MB=NB+1
      JY=MB/3
      JX=MB-3*JY
      YQ=(JY+0.5)*FENDC
      XQ=(2.5-JX)*FENDC
      IF(JX.EQ.2) YQ=YQ+0.25*FENDC
      IF(JY.EQ.0) XQ=XQ+0.25*FENDC
      X=-IZ*IX*XQ
      Y=-IZ*IY*YQ
      SLOPE = 0.
      RETURN
2000  CONTINUE
C
C                            1983-.. TAGGING APPARATUS
C
      NB=NO
      IZ=-1
      IF(NB.GT.24) IZ=1
      IF(NB.LT.1.OR.NB.GT.48) GO TO 101
C
C                            K GIVES LAYER NUMBER, L SERIAL 1 TO 8
C
      KNB = NB
      IF(IZ.GT.0) KNB = KNB - 24
      K = (KNB-1)/8 + 1
      L = KNB - (K-1)*8
      FISTRT = - FLOAT(IZ)*PIHALF
      IF(IZ.GT.0) GO TO 8021
C  MINUS Z  ENDCAP
      IF(L.GT.2.AND.L.LT.7) GO TO 8022
C UPPER HALF
      FI = FISTRT + (L-1)*DFI + DDFI
      RFI = PBSCR(K)
      IF(K.EQ.1) RFI = RFI -.02*FENDC
      GO TO 8030
8022  CONTINUE
C LOWER HALF
      FI = FISTRT + L*DFI - DDFI
      RFI = PBSCR(K+1) - 0.1*(PBSCR(K+1)-PBSCR(K))
      GO TO 8030
8021  CONTINUE
C  PLUS Z  ENDCAP
      IF(L.GT.2.AND.L.LT.7) GO TO 8024
C LOWER HALF
      FI = FISTRT + L*DFI - DDFI
      RFI = PBSCR(K+1) - 0.1*(PBSCR(K+1)-PBSCR(K))
      GO TO 8030
8024  CONTINUE
C UPPER HALF
      FI = FISTRT + (L-1)*DFI + DDFI
      RFI = PBSCR(K)
      IF(K.EQ.1) RFI = RFI -.02*FENDC
8030  CONTINUE
      LL = L - ((L-1)/4) * 4
      SLOPE = WFACT(LL)*PIHALF
      X = RFI*COS(FI)
      Y = RFI*SIN(FI)
C  GET IN ADDITION THE CORNER POINTS, FOR CRICRO DISPLAY
      FI1 = FISTRT + (L-1)*DFI
      FI2 = FI1 + DFI
      RFI1 = PBSCR(K)
      RFI2 = PBSCR(K+1)
C
      X1 = RFI1*COS(FI1)
      X2 = RFI2*COS(FI1)
      X3 = RFI2*COS(FI2)
      X4 = RFI1*COS(FI2)
      Y1 = RFI1*SIN(FI1)
      Y2 = RFI2*SIN(FI1)
      Y3 = RFI2*SIN(FI2)
      Y4 = RFI1*SIN(FI2)
C
      RETURN
      END
