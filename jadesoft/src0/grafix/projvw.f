C   08/08/85 508090428  MEMBER NAME  PROJVW   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE PROJVW( JVIEW, LFLAG )
C-----------------------------------------------------------------------
C
C    AUTHOR:   C. BOWDERY   8/08/85 :  DRAW ONE PROJECTION
C
C
C     PERFORMS EVENT, DETECTOR AND RESULTS DISPLAY FOR ONE PROJECTION
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL  LFLAG
C
#include "cgraph.for"
C
C------------------  C O D E  ------------------------------------------
C
      NACMD = ACMD
      CALL SETSCL( JVIEW )
C
      IF( LFLAG ) CALL EVDISP(JVIEW)
      IF( NACMD .NE. 99  .AND.  LFLAG ) CALL JADISP(JVIEW)
C
C                            LAST COMMAND WAS VRES?
C                            IF SO, DO RESULTS DISPLAY WITH VERTICES
C
      IF( LSTCMD .NE. 52 ) GO TO 10
        CALL VXDISP(JVIEW)
        GO TO 13
C
C                            IF MUPT OR MUONS, DRAW MUON TRACKS,ETC
C                            S/R MULDSP CALLS XYMUD AND BOTH USE LASTVW
C
  10  IF( LSTCMD .NE. 105  .AND.  LSTCMD .NE. 106 ) GO TO 12
        LVIEW  = LASTVW
        LASTVW = JVIEW
        CALL MULDSP
        LASTVW = LVIEW
        GO TO 13
C
C                            IF CDTL 16 OR RES OR MUR2
C                            THEN CALL RESULTS DISPLAY
C
  12    IF( LSTCMD .EQ. 45  .OR.  LSTCMD .EQ. 55
     +                      .OR.  DSPDTL(16)     ) CALL RSDISP(JVIEW)
C
  13  RETURN
C
      END
