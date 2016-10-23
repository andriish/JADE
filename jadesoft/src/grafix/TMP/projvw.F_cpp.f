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
C-----------------------------------------------------------------------
C                            MACRO CGRAPH .... GRAPHICS COMMON
C-----------------------------------------------------------------------
C
      LOGICAL DSPDTL,SSTPS,PSTPS,FREEZE
C
      COMMON / CGRAPH / JUSCRN,NDDINN,NDDOUT,IDATSV(11),ICREC,MAXREC,
     +                  LSTCMD,ACMD,LASTVW,ISTANV,
     +                  SXIN,SXAX,SYIN,SYAX,XMIN,XMAX,YMIN,YMAX,
     +                  DSPDTL(30),SSTPS(10),PSTPS(10),FREEZE(30),
     +                  IREADM,LABEL,LSTPS(10),IPSVAR
C
C------- END OF MACRO CGRAPH -------------------------------------------
C
C
C------------------  C O D E  ------------------------------------------
C
      NACMD = ACMD
      CALL SETSCL( JVIEW )
C
      IF( LFLAG ) CALL EVDISP(JVIEW)
      IF( NACMD .NE. 99  .AND.  LFLAG ) then
         call setcol('JADE')    ! PMF 23/11/99: set colour
         CALL JADISP(JVIEW)
         call setcol(' ')    ! PMF 23/11/99: reset colour
      endif
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
