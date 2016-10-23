C   20/12/85 512202018  MEMBER NAME  XXXYYY   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE XXXYYY(XXX,YYY,SSS,INTOX)
C-----------------------------------------------------------------------
C
C     COMPUTE POSITION ON SCREEN, GIVEN INPUT IN DETECTOR COORDINATES
C
C-----------------------------------------------------------------------
C
      IMPLICIT  INTEGER*2 (H)
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
      IF(INTOX.EQ.1) GO TO 1
      CALL CHRSIZ(4)
      CALL CSIZE(IHO,IVE)
      SSS = IVE*(YMAX-YMIN)/6240.
      YYY = YYY - 15.*SSS
      SSS = IVE*.55
1     CALL MOVEA(XXX,YYY)
      CALL SEELOC(IHO,IVE)
      XXX = IHO
      YYY = IVE
      RETURN
      END
