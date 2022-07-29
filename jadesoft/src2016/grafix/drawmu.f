C   15/02/84 605031744  MEMBER NAME  DRAWMU   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE DRAWMU( XPOS , YPOS )
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. OLSSON    3/05/84 :  DRAW MU SYMBOL
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C-------------------  C O D E  -----------------------------------------
C
      CALL MOVEA( XPOS + 0.2 , YPOS - 0.6  )
      CALL DRAWA( XPOS + 0.4 , YPOS + 0.65 )
      CALL MOVEA( XPOS + 0.35 , YPOS + 0.1 )
      CALL DRAWA( XPOS + 0.4 , YPOS        )
      CALL DRAWA( XPOS + 0.6 , YPOS        )
      CALL DRAWA( XPOS + 0.65, YPOS + 0.1  )
      CALL MOVEA( XPOS + 0.7 , YPOS + 0.65 )
      CALL DRAWA( XPOS + 0.65, YPOS + 0.1  )
      CALL DRAWA( XPOS + 0.7 , YPOS        )
C
      RETURN
      END
