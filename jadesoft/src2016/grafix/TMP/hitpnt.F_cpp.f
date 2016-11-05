C   01/11/84 807251600  MEMBER NAME  HITPNT   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE HITPNT( X01, Y01, X02, Y02, IPH )
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. HAGEMANN   04/02/86 :  WRITE VTXC-BANK HIT POINTER
C                                        CLOSE TO HIT AND MIRROR HIT
C
C       ARGUMENTS :
C           X01,2  =  X - COORDINATES OF HIT, MIRROR HIT
C           Y01,2  =  Y - COORDINATES OF HIT, MIRROR HIT
C           IPH    =  VTXC-BANK HIT POINTER
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C---------  MACRO CHSYM    SYMBOLS USED IN GRAPHICS WRITING -----
      COMMON /CHSYM/ HSYM(36)
C------ END MACRO CHSYM
C
      DIMENSION HBUF(4)
C
      DATA HBLANK   /'  '/
      DATA HMINUS   /'- '/
C
C-----------------  C O D E  -------------------------------------------
C
      IF( IPH .GT. 9 ) GO TO 50
         HBUF(2) = HSYM(IPH + 1)
         HBUF(1) = HBLANK
         CALL HEMSYM( X02, Y02, 1.7, HBUF, 2, 0.0 )
         HBUF(1) = HMINUS
         CALL HEMSYM( X01, Y01, 1.7, HBUF, 2, 0.0 )
         GO TO 1000
   50 IF( IPH .GT. 99 ) GO TO 100
         ITEN    = IPH/10
         HBUF(2) = HSYM(ITEN + 1)
         IDIG    = IPH - ITEN*10
         HBUF(3) = HSYM(IDIG + 1)
         HBUF(1) = HBLANK
         CALL HEMSYM( X02, Y02, 1.7, HBUF, 3, 0.0 )
         HBUF(1) = HMINUS
         CALL HEMSYM( X01, Y01, 1.7, HBUF, 3, 0.0 )
         GO TO 1000
  100 CONTINUE
         IHUN    = IPH/100
         HBUF(2) = HSYM(IHUN + 1)
         ITEN    = (IPH-IHUN*100)/10
         HBUF(3) = HSYM(ITEN+1)
         IDIG    = IPH - IHUN*100 - ITEN*10
         HBUF(4) = HSYM(IDIG + 1)
         HBUF(1) = HBLANK
         CALL HEMSYM( X02, Y02, 1.7, HBUF, 4, 0.0 )
         HBUF(1) = HMINUS
         CALL HEMSYM( X01, Y01, 1.7, HBUF, 4, 0.0 )
C
 1000 CONTINUE
C
      RETURN
      END
