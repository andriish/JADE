C   17/12/83 807251630  MEMBER NAME  VRXCIR   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE VRXCIR
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. OLSSON   17/12/83 :  CONCENTRIC CIRCLES AROUND VERTEX
C
C       MOD:   J. OLSSON    8/02/84 :
C       MOD:   C. BOWDERY   8/06/84 :  NEW COMMAND NUMBERS
C       MOD:   J. HAGEMANN 11/06/87 :  SET CHARACTER SIZE
C  LAST MOD:   J. HAGEMANN 26/10/87 :  FOR CIRCLES AROUND RUN VERTEX
C
C DRAW CONCENTRIC CIRCLES AROUND VERTEX POINT, FOR VERTEX REGION DISPLAY
C DRAW CONCENTRIC RECTANGLES FOR ZX AND ZY VIEWS
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL FLVCDO
      LOGICAL TBIT
C
#include "cgraph.for"
C
      COMMON / CGVCDO / FLVCDO(20)
* PMF 08/11/99: COMMON added to get run info for subroutine VTXCRV
      COMMON / CHEADR / HEAD(108)
* PMF (end)
C
      DIMENSION HTX(6)
C
      DATA HTX /2H1 ,2H2 ,2H3 ,2H4 ,2H5 ,2HMM/
C
C------------------  C O D E  ------------------------------------------
C
      CALL CHRSIZ(4)
C
      DDD=6.*10.24*25.4/(XMAX-XMIN)
C                            SET CODE FOR DASH DRAWING
      LLX = 18
      R   = 0.0
      DR  = 0.5
      XRV = 0.0
      YRV = 0.0
      IF( .NOT.FLVCDO(11) .OR. LASTVW .NE. 17 ) GO TO 1
          CALL VTXCRV( INT(HEAD(18)), XRV, YRV, DXR, DYR ) ! PMF 08/11/99: add run argument HEAD(18)
    1 CONTINUE
C
C                  DRAW CIRCLES/RECTANGLES  AROUND INTERACTION POINT
C
      DO 10  I = 1,10
         R = R + DR
         IF( LASTVW .NE. 17 ) GO TO 2
            NN = DDD*FLOAT(I)
            CALL PLYGON(NN,R,-XRV,YRV,LLX)
            GO TO 3
C
C                  DRAW RECTANGLES IN ZX AND ZY VIEWS
C
    2       CALL MOVEA(-R,-R)
            CALL DASHA(R,-R,LLX)
            CALL DASHA(R,R,LLX)
            CALL DASHA(-R,R,LLX)
            CALL DASHA(-R,-R,LLX)
    3    IF(TBIT(I,31)) GO TO 10
C                  FOR EVEN I, WRITE THE RADIUS
         CALL MOVEA(-XRV,R+YRV)
         J = I/2
         IF(J.LT.5) CALL EOUTST(2,HTX(J))
         IF(J.EQ.5) CALL EOUTST(4,HTX(J))
   10 CONTINUE
C
      RETURN
      END
