C   16/06/80 404301635  MEMBER NAME  FWMUCN   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE FWMUCN
C-----------------------------------------------------------------------
C
C   AUTHOR:   L. O'NEILL       ?     :  DRAW FORWARD MUON COUNTERS
C
C        MOD: J. OLSSON     28/05/81 :
C   LAST MOD: C. BOWDERY    30/04/84 :  COMMENTING IMPROVED
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cgraph.for"
#include "cdata.for"
C
      COMMON / CHEADR / HEAD(108)
      COMMON / CWORK2 / AGEO(6,16)
      COMMON / CFWMU  / HAXES(4,3),AFWMU(18)
C
C------------------  C O D E  ------------------------------------------
C
      IF( HEAD(18) .LT. 3730 ) RETURN
      MPROJ = 2
      IF( LASTVW .GT.  7 ) MPROJ = 1
      IF( LASTVW .EQ. 15 ) MPROJ = 3
      IXS = HAXES(1,MPROJ)
      IXA = HAXES(2,MPROJ)
      IYS = HAXES(3,MPROJ)
      IYA = HAXES(4,MPROJ)
C
      DO  1  I = 1,4
        AGEO(1,I) = AFWMU(I)
        AGEO(4,I) = AFWMU(I+4)
        AGEO(5,I) = AFWMU(I+8)
   1  CONTINUE
C
      DO  2  I = 1,4
        AGEO(1,9-I) = -AGEO(1,I) - 5.0
        AGEO(4,9-I) =  AGEO(4,I)
        AGEO(5,9-I) =  AGEO(5,I)
   2  CONTINUE
C
      DO  3  I = 1,8
        AGEO(1,(I+8)) = AGEO(1,I)
        AGEO(4,(I+8)) = AGEO(4,I)
        AGEO(5,(I+8)) = AGEO(5,I)
   3  CONTINUE
C
      DO  4  J = 1,16
        AGEO(6,J) = AFWMU(15)
   4  CONTINUE
C
      DO  5  J = 1,8
        AGEO(2,J+8) = AFWMU(17)
        AGEO(2,J)   = AFWMU(13)
   5  CONTINUE
C
      DO  6  J = 1,4
        AGEO(3,J+4)  =  AFWMU(16)
        AGEO(3,J+8)  = -AFWMU(16)
        AGEO(3,J+12) =  AFWMU(18)
        AGEO(3,J)    =  AFWMU(14)
   6  CONTINUE
C
      IF( LASTVW .NE. 15 ) GO TO 9
      X1   = 5250.0
      Y1   = 4000.0
C
      DO  7  I = 1,8
        AGEO(1,I) = AGEO(1,I) - X1
        AGEO(2,I) = AGEO(2,I) + Y1
   7  CONTINUE
C
      X1 = 1250.0
C
      DO  8  I = 9,16
        AGEO(1,I) = AGEO(1,I) - X1
        AGEO(2,I) = AGEO(2,I) + Y1
   8  CONTINUE
C
   9  DO  10  I = 1,16
        IF( MPROJ .EQ. 1  .AND.  I .GT.  4  .AND.  I .LT. 9 ) GO TO 10
        IF( MPROJ .EQ. 1  .AND.  I .GT. 12 ) GO TO 10
        X0 = IXS * AGEO(IXA,I)
        Y0 = IYS * AGEO(IYA,I)
        DX =       AGEO((IXA+3),I)
        DY =       AGEO((IYA+3),I)
        CALL RECTAN(X0-DX,Y0-DY,X0+DX,Y0+DY,0)
  10  CONTINUE
C
      RETURN
      END
