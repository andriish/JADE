C   23/08/85 711181952  MEMBER NAME  MCTAGB   (S)           FORTRAN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   11/03/84 403171903  MEMBER NAME  MCTAGB   (S)           FORTRAN
C
C SUBROUTINE TO RETURN BLOCK NUMBER OF HIT BLOCK GIVEN
C X,Y,Z
C
C HANDLES MARKMC 1 2 3 AND 4
C  = 1979/80, 1981/2 ,1983...
C
      SUBROUTINE MCTAGB(MARKMC,X,Y,Z,IBLNO,*)
C
C
C A.J.FINCH 11/3/84
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C    CONSTANTS:
C
C  ------------------------------------------------------------------
C
C 81/2 STUFF
C __________
C
      INTEGER*2 XYMAP(8,8)
C
      DATA XYMAP/2,31,15,18,50,47,63,34,1,32,16,17,49,48,64,33,
     15,28,12,21,53,44,60,37,8,25,9,24,56,41,57,40,
     14,29,13,20,52,45,61,36,3,30,14,19,51,46,62,35,
     17,26,10,23,55,42,58,39,6,27,11,22,54,43,59,38/
C
      DATA X1/83.0/,X2/95.0/,X3/164.0/,X4/176.0/,X5/245.0/,
     1X6/257.0/,Y1/81.0/,Y2/95.5/,Y3/162.0/,Y4/176.5/,Y5/243.0/,
     1Y6/258.0/
C
C ----------------------------------------------------------------------
C
C  79 80 STUFF-
C _____________
C
      DIMENSION NBLIST(11,6)
      DATA NBLIST /4*0,1,2,3,4*0,
     1             2*0,4,5,6,7,8,9,10,2*0,
     2             0,11,12,13,14,15,16,17,18,19,0,
     3             20,21,22,23,24,25,26,27,28,29,30,
     4             31,32,33,34,3*0,35,36,37,38,
     5             39,40,41,42,3*0,43,44,45,46/
C
C ---------------------------------------------------------------------
C
C  83 STUFF
C  ________
C
C     DATA R1/96.0/,R2/110.0/,R3/140.0/,R4/225.0/
C  CHANGE 18.11.87   INNER RADIUS SET TO 86 MM TO ALLOW FOR HITTING
C  FROM BENEATH. THE OUTER RADIUS, 225 MM, IS KEPT TO ALLOW FOR LEAVING
C  THE TAGGER ON THE OUTSIDE BEFORE THE END (CORRECT VALUE IS 214.4 MM).
C
      DATA R1/86.0/,R2/110.0/,R3/140.0/,R4/225.0/
      DATA PIBY4/0.785398163/
      DATA PIBY8/0.392699081/
      DATA TWOPI/6.283185307/
      DATA PI/3.141592654/
C     DATA HSHIFT/6.0/
C
C
C=============== CODE =======
C
C CHOOSE WHICH TAGGER,AND SAVE X,Y
C
C
      IBLOCK = -1
C
      IF(MARKMC.EQ.1)GOTO 1000
      IF(MARKMC.EQ.2)GOTO 1001
      IF(MARKMC.EQ.3)GOTO 1001
      IF(MARKMC.GE.4)GOTO 1002
C
C ----------------------------------------------------------------------
C
C HERE FOR 1983 ....TAGGER
C
C ----------------------------------------------------------------------
C
C
1002  CONTINUE
C
       FI = ATAN(ABS(Y)/ABS(X))
        IF(X.LT.0.AND.Y.GT.0)FI = PI - FI
        IF(X.LT.0.AND.Y.LT.0)FI = FI + PI
        IF(X.GT.0.AND.Y.LT.0)FI = TWOPI - FI
      R = SQRT(X**2+Y**2)
      AFI = FI
C
      DO 10 I = 1,8
       J = I
       AFI = AFI - PIBY4
       IF(AFI.LE.0)GOTO 20
10    CONTINUE
C
20    AFI = AFI + PIBY4
      IF(AFI.GE.PIBY8)BFI =  AFI - PIBY8
      IF(AFI.LT.PIBY8)BFI = PIBY8  - AFI
      RMID = R* COS(BFI)
      IF(RMID.LT.R1)GOTO 90
       IF(RMID.GE.R1.AND.RMID.LT.R2)SHIFT2=0
       IF(RMID.GE.R2.AND.RMID.LT.R3)SHIFT2=8
       IF(RMID.GE.R3.AND.RMID.LT.R4)SHIFT2=16
       IF(RMID.GE.R4)GOTO 90
C
       IF(Z.LT.0.AND.J.LT.3)IBLNO=J+6+SHIFT2
       IF(Z.LT.0.AND.J.GE.3)IBLNO=J-2+SHIFT2
       IF(Z.GT.0.AND.J.LT.7)IBLNO=J+2+SHIFT2+24
       IF(Z.GT.0.AND.J.GE.7)IBLNO=J-6+SHIFT2+24
      RETURN
 90   CONTINUE
      RETURN 1
C
C -----END OF SECTION FOR 1983 TAGGER
C
C---------------------------------------------------------------------
C
C HERE FOR 1981/2 TAGGER
C
C----------------------------------------------------------------------
C
1001  CONTINUE
C( USED TO BE:  SUBROUTINE BLNO81(X,Y,IBLOCK) )
C
      ISHIFT=0
      IF(Z.GT.0)ISIGNZ=1
      IF(Z.LT.0)ISIGNZ=-1
      IF(ISIGNZ.GT.0)ISHIFT=4
C
C   NOW CHOOSE QUADRANT USING X/Y SIGN
C
      IF((X.GT.0).AND.(Y.GT.0))I=1
      IF((X.GT.0).AND.(Y.LT.0))I=2
      IF((X.LT.0).AND.(Y.GT.0))I=3
      IF((X.LT.0).AND.(Y.LT.0))I=4
      I=I+ISHIFT
C
C NOW FIND WHICH BLOCK WITHIN THE QUADRANT USING THE
C  ABSOLUTE MAGNITUDE OF X/Y
C
      AX=ABS(X)
      AY=ABS(Y)
C
C( DO A  RETURN 1 FOR ILLEGAL VALUES OF X/Y OUTSIDE BLOCK EDGES )
C
C DEAL WITH THE TWO BLOCKS CLOSE TO THE X AXIS
C
      IF(AY.GT.Y1)GOTO 105
       IF(AX.LT.X2)RETURN 1
       IF(AX.GT.X6)RETURN 1
       J=1
       IF(AX.GT.X4)J=2
       GOTO 100
C
C DEAL WITH THE TWO BLOCKS ALONG THE Y AXIS
C
105   IF(AX.GT.X1)GOTO110
       IF(AY.LT.Y2)RETURN 1
       IF(AY.GT.Y6)RETURN 1
       J=3
       IF(AY.GT.Y4)J=4
C
       GOTO100
C
C THIS LEAVES THE OUTER BLOCK OF FOUR
C SPLIT FIRST ON Y
C
 110   IF(AY.GT.Y5)RETURN 1
       IF(AX.GT.X5)RETURN 1
       IF(AY.GT.Y3) GOTO 115
C
C THEN SPLIT ON SIZE OF X
C
        J=5
        IF(AX.GT.X3)J=6
        GOTO 100
C
C SPLIT LAST TWO BLOCKS ON X
C
 115   J=7
       IF(AX.GT.X3)J=8
C
C I AND J ARE NOW DETERMINED CAN USE THEM TO DETERMINE ACTUAL
C  HARDWARE ADDRESS /BLOCK NUMBER
C
 100      CONTINUE
          IBLNO=XYMAP(I,J)
C
C   NOW RETURN WITH THE KNOWLEDGE OF A JOB WELL DONE
C
          RETURN
C
C------------------------------------------------------------
C
C ------END OF SECTION FOR 1981/2 TAGGER
C
C----------------------------------------------------------
C
C HERE FOR 1979/80 TAGGER
C
C----------------------------------------------------------
C
 1000  CONTINUE
C
C---  OBTAIN PB-GLASS BLOCK NUMBER CORRESPONDING TO X-Y-POSITION
C---  INPUT:  X- AND Y-COORDINATES (IN MM)
C
C     H.WRIEDT      12.12.78      15:30
C     LAST MODIFICATION     18.01.79       15:00
C
      XSAVE = X
      YSAVE = Y
      IF(Z.GT.0)ISIGNZ=1
      IF(Z.LT.0)ISIGNZ=-1
      N= 0
C---  +Z-DIRECTION (TOWARDS MARKMC J)
      X0 = X
      Y0 = Y
      IF (ISIGNZ.GT.0) GOTO 205
C---  -Z-DIRECTION (TOWARDS PLUTO & CELLO)
      X0 = -X
      Y0 = -Y
C
205   X = X0
      Y = Y0
      IF (X0) 210,211,211
210   X = -X0
211   IF (ABS(Y0).LT.121.5) X = X+20.
      IF (Y0) 220,221,221
220   Y = -Y0
221   IF (X.LT.81.) Y = Y-30.
C
      NX = X/81.
      IF (NX.GE.6) GOTO 2100
      NX = 6-NX
      NY = (Y+40.5)/81.
      IF (NY.GE.6) GOTO 2100
      IF (X0) 240,230,230
C---  BLOCK ADDRESSES 1 TO 46
230   IF (Y0) 231,232,232
231   NY = 6-NY
      GOTO 235
232   NY = 6+NY
235   N = NBLIST(NY,NX)
      IF (N.EQ.0) GOTO 2100
C---  BLOCK ADDRESSES 97 TO 142
      IF (ISIGNZ.GT.0) N = N+96
      GOTO 2100
C---  BLOCK ADDRESSES 49 TO 94
240   IF (Y0) 241,242,242
241   NY = 6+NY
      GOTO 245
242   NY = 6-NY
245   N = NBLIST(NY,NX)
      IF (N.EQ.0) GOTO 2100
      N = 95-N
C---  BLOCK ADDRESSES 145 TO 190
      IF (ISIGNZ.GT.0) N = N+96
C
2100  CONTINUE
      X = XSAVE
      Y = YSAVE
      IBLNO = N
      IF(IBLNO.LE.0)RETURN 1
      RETURN
      END
