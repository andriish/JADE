C   20/10/81 110201218  MEMBER NAME  XYMUD    (JADEMUS)     FORTRAN
C   25/09/81 109292308  MEMBER NAME  XYMUD    (S)           FORTRAN
C   22/01/79 C9030601   MEMBER NAME  XYMUD    (JADEGS)      FORTRAN
C
C LAST CHANGE 15.08  25/09/81 CHRIS BOWDERY - REMOVE ROTATE MODE CODE
C
      SUBROUTINE XYMUD(XCOR,YCOR,ZCOR,X,Y)
C
C        THIS ROUTINE CONVERTS THE DETECTOR FRAME COORDINATES OF
C        MUON HITS (XCOR,YCOR,ZCOR) TO THE DISPLAY COORDINATES
C        FOR THE CURRENT VIEW
C
C                                      L.H. O'NEILL
C                                      MONDAY, DECEMBER 11, 1978.
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
      IPROJ=1
      IF(LASTVW.GT.3) IPROJ=2
      IF(LASTVW.GT.7) IPROJ=3
      IF(IPROJ.EQ.1) X=-XCOR
      IF(IPROJ.EQ.1) Y= YCOR
      IF(IPROJ.EQ.2) X= ZCOR
      IF(IPROJ.EQ.2) Y= XCOR
      IF(IPROJ.EQ.3) X= ZCOR
      IF(IPROJ.EQ.3) Y= YCOR
      RETURN
      END
