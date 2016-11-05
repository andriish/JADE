C   23/11/87 711240952  MEMBER NAME  PRVECT   (S)        M  FORTRAN77
      SUBROUTINE PRVECT
C-----------------------------------------------------------
C  Version of 24/11/87       last mod 24/11/87    E Elsen
C  Print banks VECT, n
C-----------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
      COMMON / BCS / IW(1)
      DIMENSION HW(1),RW(1)
      EQUIVALENCE (HW(1),IW(1),RW(1))
      LOGICAL FIRST / .TRUE. /
      IF( FIRST ) THEN
        FIRST = .FALSE.
        IPVECT = IBLN('VECT')
      ENDIF
      NPVECT = IW(IPVECT)
      IF( NPVECT.GT.0 ) THEN
        WRITE(6,9101) (IW(NPVECT+I),I=-3,0)
 9101   FORMAT(/'  Bank',5X,A4,3I9)
        L0 = IW(NPVECT+1)
        WRITE(6,9102) (IW(NPVECT+I),I=1,L0)
 9102   FORMAT(6X,6I9,2F9.3,5I9)
        L1 = IW(NPVECT+2)
        NP = NPVECT + L0
        NP9 = NP + (IW(NPVECT+4)-1)*L1
        IF( IW(NPVECT+4).GT.0 ) THEN
          WRITE(6,9103) ((J-NP)/L1+1,
     *                   (RW(J+I),I=1,5),IW(J+6),IW(J+7),
     *                   (RW(J+I),I=8,10),J=NP,NP9,L1)
 9103     FORMAT('     #       px       py       pz        E        m',
     *           '   q  type        x        y        z'/
     *          (1X,I5,5F9.3,I4,I6,3F9.3))
        ENDIF
        NPVECT = IW(NPVECT-1)
        IF( NPVECT.GT.0 ) THEN
          WRITE(6,9101) (IW(NPVECT+I),I=-3,0)
          L0 = IW(NPVECT+1)
          WRITE(6,9102) (IW(NPVECT+I),I=1,L0)
          L1 = IW(NPVECT+2)
          NP = NPVECT + L0
          NP9 = NP + (IW(NPVECT+4)-1)*L1
          IF( IW(NPVECT+4).GT.0 ) THEN
          WRITE(6,9104) ((J-NP)/L1+1,
     *                     (RW(J+I),I=1,5),IW(J+6),IW(J+7),
     *                     (RW(J+I),I=8,10),(HW(J*2+I),I=21,22),
     *                      RW(J+12),J=NP,NP9,L1)
 9104     FORMAT('     #       px       py       pz        E        m',
     *           '   q  type        x        y        z',
     *           ' vect part s(parnt)'/
     *          (1X,I5,5F9.3,I4,I6,3F9.3),2I5,F9.3)
          ENDIF
        ENDIF
      ENDIF
      END
