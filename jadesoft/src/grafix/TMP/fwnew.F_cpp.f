C   10/04/84            MEMBER NAME  FWNEW    (JADEGS)      FORTRAN
      SUBROUTINE FWNEW
      IMPLICIT INTEGER*2 (H)
C-----------------------------------------------------------------------
C                            MACRO CGEO2 .... JADE TAGGING GEOMETRY
C-----------------------------------------------------------------------
C
      COMMON / CGEO2 / FENDC,XYHOL1,XYHOL2,BLDPFW,ZMINBL,ZPLUBL,
     +                 XSC(2),YSC(2),RSC(2),ZMISC(2),ZPLSC(2),DZSC,
     +                 CHX(3,4),CHY(3,4),CHZ(3,4),WLEN,PITCH,WZDIS
C
C--------------------------- END OF MACRO CGEO2 ------------------------
C
C----------------------------------------------------------------------
C      MACRO CGEO3 .... JADE FORWARD DETECTOR GEOMETRY, 1981-3 VERSION
C----------------------------------------------------------------------
C
      COMMON / CGEO3 / ZPLUM2,ZMINM2,NRPBSC,PBSCR(4),PBSCZ(4)
C
C------------------------ END OF MACRO CGEO3 --------------------------
C
C
C  DRAW SIDEVIEW OF THE TAGGING APPARATUS IN 1981-82 VERSION
C
      DO 1 JZ=1,2
      IZ=-3+2*JZ
      Z0=ZMINM2
      IF(JZ.EQ.2) Z0=ZPLUM2
      Z1=Z0+IZ*BLDPFW
      X0=-3.25*FENDC
      X1=-X0
      CALL RECTAN(Z0,X0,Z1,X1,0)
      X=-3.*FENDC
      DO 1 JX=1,7
      CALL MOVEA(Z0,X)
      CALL DRAWA(Z1,X)
      X=X+FENDC
    1 CONTINUE
      RETURN
      END
