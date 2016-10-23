C   17/05/88 805171201  MEMBER NAME  EBGENL   (S)           FORTRAN77
      FUNCTION EBGENL( DUMMY )
      IMPLICIT INTEGER*2 (H)
      COMMON / BCS / IW(1)
      DIMENSION HW(1)
      EQUIVALENCE (HW(1),IW(1))
      COMMON / CPROD / NEV, BEAM
      DATA ICALL / 0 /
C
      IF( ICALL .GT. 0 ) GO TO 1
      IBHEAD = IBLN('HEAD')
      ICALL = 1
    1 CONTINUE
      NPHEAD = IW(IBHEAD)
      IF( NPHEAD .LE. 0 ) GO TO 100
      HRUN = HW( NPHEAD*2+10)
      EBGENL= EBEAM( HRUN ) / 1000.
      RETURN
C                                           BEAM FROM CPROD
  100 EBGENL = BEAM
      RETURN
      END
