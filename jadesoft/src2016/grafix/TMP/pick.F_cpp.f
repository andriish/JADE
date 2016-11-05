C   09/03/84 602121755  MEMBER NAME  PICK     (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE PICK
C-----------------------------------------------------------------------
C
C   AUTHOR:  J. HAGEMANN   12/02/86 : PICKS COORDINATES FROM SCREEN
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
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
      COMMON / CGAMOD / MODEGA, GAHEX
      COMMON / CWORK1 / HWORK(40),SIZEWN,RANGE,XCENT,YCENT
C
C------------------  C O D E  ------------------------------------------
C
C                            CLEAR TSO TERMINAL IF RUNNING IN GA-MODE
C
      IF( MODEGA .EQ. 1 ) CALL CLEAR
C
C                            CHECK IF PICK IS POSSIBLE FOR THIS VIEW
      IF( LASTVW .GT. 11 .AND. LASTVW .LT. 17 ) GO TO 4000
C
C                            FLASH CURSOR ON SCREEN AND AWAIT RESULT
C
      CALL VCURSR(HWORK(1),XGET,YGET)
C
      IF( LASTVW.LT.4 .OR. LASTVW.EQ.17 .OR. LASTVW.EQ.20 ) GO TO 900
C
         IF( (LASTVW .GT. 3 .AND. LASTVW .LT. 8) .OR. LASTVW .EQ. 18 )
     *      GO TO 800
C
            WRITE(6,9700) XGET, YGET
 9700       FORMAT(' Z : ',F10.3,'  Y : ',F10.3)
            GO TO 5000
C
  800 WRITE(6,9800) XGET, YGET
 9800 FORMAT(' Z : ',F10.3,'  X : ',F10.3)
      GO TO 5000
C
  900 XGET = -XGET
      WRITE(6,9900) XGET, YGET
 9900 FORMAT(' X : ',F10.3,'  Y : ',F10.3)
C
      GO TO 5000
C
 4000 CALL TRMOUT(80,' Sorry, PICK not available for this view!^')
C
 5000 CONTINUE
C
      RETURN
      END
