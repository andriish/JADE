C   10/04/84            MEMBER NAME  FW83     (JADEGS)      FORTRAN
      SUBROUTINE FW83
      IMPLICIT INTEGER*2 (H)
C
C DRAW TAGGING APPARATUS IN SIDEVIEW, 1983-.. VERSION
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
      DO 1 JZ=1,3,2
      Z0=PBSCZ(JZ)
      Z1=PBSCZ(JZ+1)
      DO 2  IZ = 1,2
      IIZ = -3 + 2*IZ
      X0 = IIZ*PBSCR(1)
      X1 = IIZ*PBSCR(4)
      CALL RECTAN(Z0,X0,Z1,X1,0)
      DO 3 JX=2,3
      X = PBSCR(JX)*IIZ
      CALL MOVEA(Z0,X)
      CALL DRAWA(Z1,X)
3     CONTINUE
2     CONTINUE
1     CONTINUE
      RETURN
      END
