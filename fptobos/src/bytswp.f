      SUBROUTINE BYTSWP(IA,MODE)
*
*   Swap Bytes in word IA, according to value of MODE
*
*        MODE = 1    Bytes 1,2,3,4  swapped into  4,3,2,1
*
*        Uses Cernlib routine CBYT
*    Author  J.Olsson     13.12.2005
*
*
      INTEGER IA,MODE,IB
*
*------------  CODE  ------------------
*
      IF(MODE.NE.1) THEN
        WRITE(6,1001)  MODE 
 1001   FORMAT(' BYTSWP:  MODE=',I6,'  not implemented ')
        GO TO 9999
      ENDIF
*
      CALL CBYT(IA,1,IB,25,8)
      CALL CBYT(IA,9,IB,17,8)
      CALL CBYT(IA,17,IB,9,8)
      CALL CBYT(IA,25,IB,1,8)
*
      IA = IB
*
 9999 CONTINUE
      RETURN
      END
