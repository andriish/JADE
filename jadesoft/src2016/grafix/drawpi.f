C   15/02/84 605031743  MEMBER NAME  DRAWPI   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE DRAWPI( XPOS , YPOS )
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY   15/02/84 :  DRAW PI SYMBOL
C   MODIFIED  J. OLSSON     3/05/87
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C-------------------  C O D E  -----------------------------------------
C
      CALL MOVEA( XPOS + 0.3 , YPOS        )
      CALL DRAWA( XPOS + 0.3 , YPOS + 0.75 )
      CALL MOVEA( XPOS       , YPOS + 0.65 )
      CALL DRAWA( XPOS + 0.3 , YPOS + 0.75 )
      CALL DRAWA( XPOS + 0.6 , YPOS + 0.65 )
      CALL DRAWA( XPOS + 0.9 , YPOS + 0.75 )
      CALL MOVEA( XPOS + 0.6 , YPOS + 0.65 )
      CALL DRAWA( XPOS + 0.6 , YPOS        )
C
      RETURN
      END
