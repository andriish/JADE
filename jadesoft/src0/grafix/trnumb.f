C   01/11/84 807251625  MEMBER NAME  TRNUMB   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE TRNUMB(ITR,INDEX,X,Y,Z)
C-----------------------------------------------------------------------
C
C    AUTHOR:  J. OLSSON         ?    : TRACK NUMBER DISPLAY
C
C       MOD:  J. OLSSON     16/02/84 :
C       MOD:  C. BOWDERY    13/03/84 : REVERSE CDTL(12/31/32) MEANING
C       MOD:  C. BOWDERY     8/06/84 : NEW COMMAND NUMBERS
C       MOD:  J. HAGEMANN   22/10/84 : FOR NEW COMMAND VC
C  LAST MOD:  J. HAGEMANN   10/12/85 : IMPROVED DISPLAY FOR VC VIEW
C
C        WRITE THE NUMBER ITR AT POSITION X,Y
C        PROJECTED ACCORDING TO Z-COORDINATE, TO AGREE WITH PERSPECTIVE
C        CYLINDRICAL DISPLAY VIEW
C
C        INDEX = 0  :  NORMAL SIZE (SH5)
C        INDEX = 1  :  SMALL SIZE  (.6 * SH5)
C        INDEX = 2  :  LARGE SIZE  (2.*SH5)
C        INDEX = 3  :  EXTRA SMALL SIZE (.1*SH5)
C                             LATEST CHANGE 16.02.84   (J.OLSSON)
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL DSPDTM
      LOGICAL FL18,FL22,FL24
      LOGICAL TBIT
C
      COMMON / CPROJ  / XMINR,XMAXR,YMINR,YMAXR,IPRJC,FL18,FL22,FL24
      COMMON / CGRAP2 / BCMD,DSPDTM(30)
      COMMON / CVCPV  / ICD, DFX, DFY, IRC
#include "cgraph.for"
#include "cgeo1.for"
C
      DATA ADD / 20.0 /, ZDEEP / 5800.0 /
C
C------------------  C O D E  ------------------------------------------
C
C                            DRAW NO NUMBERS IF CDTL 12/31/32 OFF
C                            FOR COMMAND RES/TR/VRES
C
      IF( .NOT. DSPDTL(12) .AND.  LSTCMD .EQ. 45 ) RETURN
      IF( .NOT. DSPDTM(1)  .AND.  LSTCMD .EQ. 47 ) RETURN
      IF( .NOT. DSPDTM(2)  .AND.  LSTCMD .EQ. 52 ) RETURN
C
      FACT =  1.0
      SH5  = 30.0
      ADDD = ADD
C
      IF( LASTVW .EQ. 3 .OR.
     +    LASTVW .EQ. 6 .OR.
     +    LASTVW .EQ. 7                       ) SH5  = 95.0
C
      IF( LASTVW .EQ. 10 .OR.  LASTVW .EQ. 11 ) SH5  = 95.0
      IF( LASTVW .GE. 17 .AND. LASTVW .LE. 19 ) SH5  =  0.01*(XMAX-XMIN)
      IF( LASTVW .GE. 17 .AND. LASTVW .LE. 19 ) ADDD = SH5
      IF( LASTVW .EQ. 20 ) SH5  = 8.0
      IF( LASTVW .EQ. 20 ) ADDD = 6.0
C
      IF( INDEX .EQ. 1                        ) SH5  = 18.0
      IF( INDEX .EQ. 1  .AND.  SH5 .LT. 1.0   ) SH5  =  0.01*(XMAX-XMIN)
      IF( INDEX .EQ. 2                        ) SH5  = 60.0
      IF( INDEX .EQ. 3                        ) SH5  =  9.0
C
      IF( FL18 ) FACT = 4.0
      SH5  = SH5 * FACT
C
      IF( INDEX .EQ. 1 ) FACT = - FACT
      IF( INDEX .EQ. 3 ) FACT = 0.0
      XX   = X + ADDD * FACT
      IF( X .LT. 0.0   ) XX   = X - ADDD * FACT
C
      FACT = ABS(FACT)
      YY   = Y + ADDD * FACT
      IF( Y .LT. 0.0   ) YY   = Y - ADDD * FACT
      IF( LASTVW.NE.20.OR..NOT.TBIT(ICD,15).OR.TBIT(ICD,14)) GO TO 300
         XX = X - DFX*60.0
         YY = Y - DFY*60.0
  300 IF( LASTVW .NE. 14  .OR.  LSTCMD .EQ. 104 ) GO TO 499
C
      FP   = (ZDEEP - Z) / (ZDEEP + ZLGPL)
      XX   = FP * XX
      YY   = FP * YY
  499 IF( LASTVW .NE. 13 ) CALL DNUM(ITR,-XX,YY,SH5,0.)
      IF( LASTVW .EQ. 13 ) CALL DNUM(ITR, XX,YY,SH5,0.)
C
      RETURN
      END
