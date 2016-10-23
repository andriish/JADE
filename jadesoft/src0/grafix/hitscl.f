C   01/11/84 807251601  MEMBER NAME  HITSCL   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE HITSCL( X0, Y0, SCRUNI )
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. HAGEMANN   30/01/86 :  DRAW SCALING ON HIT CROSS
C
C       ARGUMENTS :
C           X0     =  X - COORDINATE OF HIT
C           Y0     =  Y - COORDINATE OF HIT
C           SCRUNI =  SIZE OF HIT CROSSES
C
C-----------------------------------------------------------------------
C
      LOGICAL FL18,FL22,FL24
C
      COMMON / CPROJ  / XMINR,XMAXR,YMINR,YMAXR,IPRJC,FL18,FL22,FL24
C
      DATA WIDTH  / 0.15 /
      DATA SCALW  / 0.25 /
C
C-----------------  C O D E  -------------------------------------------
C
      IF( IPRJC .NE. 0 )  RETURN
C
       XL = X0 - WIDTH
       XR = X0 + WIDTH
       YY = Y0 - SCRUNI
       YD = Y0 - WIDTH
       YU = Y0 + WIDTH
       XX = X0 - SCRUNI
C
       DO 10 I = 1, 13
          CALL MOVEA(XL,YY)
          CALL DRAWA(XR,YY)
          YY = YY + SCALW
          CALL MOVEA(XX,YD)
          CALL DRAWA(XX,YU)
          XX = XX + SCALW
   10 CONTINUE
C
      RETURN
      END
