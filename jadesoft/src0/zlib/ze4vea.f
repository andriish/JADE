C   20/08/87 806081033  MEMBER NAME  ZE4VEA   (S)           FORTRAN77
      SUBROUTINE ZE4VEA
C-----------------------------------------------------------
C  VERSION OF 20/08/87     LAST MOD 08/06/88    E ELSEN
C  PERFORM ELECTRON ANALYSIS FOR ALL TRACKS SPECIFIED
C  IN ZE4V
C-----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / BCS / IW(1)
      DIMENSION HW(1),RW(1)
      EQUIVALENCE (HW(1),IW(1),RW(1))
      INTEGER LADD(-1:2)
      LOGICAL FIRST /.TRUE./
C
      IF( FIRST ) THEN
        FIRST = .FALSE.
        IPZE4V = IBLN('ZE4V')
      ENDIF
C
      NPZE4V = IW(IPZE4V)
      IF( NPZE4V.GT.0 ) THEN
        NT = HW(NPZE4V*2 + 6)
        LT = HW(NPZE4V*2 + 5)
        LADD(-1) = LT
        LADD(0) = HW(NPZE4V*2 + 9) + LT
        LADD(1) = HW(NPZE4V*2 + 7) + LT
        LADD(2) = HW(NPZE4V*2 +11) + LT
        NREL = HW(NPZE4V*2+1)
C
        DO 10 J=1,NT
          ITPF = HW((NPZE4V+NREL)*2+18)
          IF( ITPF.EQ.1 ) THEN
            CALL ZE4VET( NPZE4V, NREL+NPZE4V, JCAND, ESHM )
            NPZE4V = IW(IPZE4V)
            IF( JCAND .EQ. 0 ) THEN
C                                      CANDIDATE
              RW(NPZE4V+NREL+LT+3) = ESHM
              HW((NPZE4V+NREL)*2+8) = HW((NPZE4V+NREL)*2+8)/100*100 + 2
            ELSE
C                                      NO CANDIDATE
              RW(NPZE4V+NREL+LT+3) = 0.
              IF( MOD( HW((NPZE4V+NREL)*2+8), 100 ) .EQ. 2 )
     *          HW((NPZE4V+NREL)*2+8) = HW((NPZE4V+NREL)*2+8)/100*100
            ENDIF
            CALL EAFLST( JCAND, RW(NPZE4V+NREL+3) )
          ENDIF
          NREL = NREL + LADD(ITPF)
   10   CONTINUE
      ENDIF
      END
