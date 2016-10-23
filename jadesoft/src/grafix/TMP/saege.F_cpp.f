C   24/11/78 402191820  MEMBER NAME  SAEGE    (JADEGS)      FORTRAN
      SUBROUTINE SAEGE(X0,Y0,X1,Y1)
C---
C---    DRAWS SAWTOOTH FUNCTION. THIS IS SUPPOSED TO LOOK LIKE A PHOTON.
C---    LAST UPDATE  10.2.1984  J.O.
C---
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
C---
      RLEN=SQRT((X0-X1)**2+(Y0-Y1)**2)
      IF(RLEN.LT..1) GO TO 2
      DIV = (XMAX-XMIN)/30./6.! PMF 29/11/99: add factor 1/6.
C     NSTEPS=RLEN/100.
      NSTEPS=RLEN/DIV
      IF(NSTEPS.LT.2) NSTEPS=2
      STEPX=(X1-X0)/NSTEPS
      STEPY=(Y1-Y0)/NSTEPS
      CALL MOVEA(X0,Y0)
      ISIGN=1
      DX=-0.5*STEPY
      DY= 0.5*STEPX
      XB=X0+0.5*STEPX
      YB=Y0+0.5*STEPY
      XP=XB+ISIGN*DX/2.
      YP=YB+ISIGN*DY/2.
      CALL DRAWA(XP,YP)
      DO 1 ISTEP=2,NSTEPS
      ISIGN=-ISIGN
      XB=X0+STEPX*(ISTEP-0.5)
      YB=Y0+STEPY*(ISTEP-0.5)
      XP=XB+ISIGN*DX/2.
      YP=YB+ISIGN*DY/2.
      CALL DRAWA(XP,YP)
    1 CONTINUE
      CALL DRAWA(X1,Y1)
    2 CONTINUE
      RETURN
      END
