C   02/06/80 104011142  MEMBER NAME  MUR2DS   (JADEGS)      FORTRAN
      SUBROUTINE MUR2DS
      IMPLICIT INTEGER*2 (H)
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
      CALL RSDISP(LASTVW)
      IF(LASTVW.EQ.13) CALL YAMADA(0)
      IF(DSPDTL(17).AND.(LASTVW.LT.4.OR.LASTVW.EQ.14))
     $ CALL PROJEC(LASTVW)
      RETURN
      END
