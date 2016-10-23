C   27/08/87 708271321  MEMBER NAME  IRNEVT   (S)           FORTRAN77
      FUNCTION IRNEVT( DUMMY )
C-----------------------------------------------------------
C  Version of 27/08/87      Last mod 27/08/87  E Elsen
C  Provide run and event number in one I*4 word.
C  If no HEAD bank is there ZE4V is tried.
C-----------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
      COMMON / BCS / IW(1)
      DIMENSION HW(1), HELP(2)
      EQUIVALENCE (HW(1),IW(1)),(HELP(1), IHELP)
      LOGICAL FIRST / .TRUE. /
C
      IF( FIRST ) THEN
        FIRST = .FALSE.
        IPHEAD = IBLN('HEAD')
        IPZE4V = IBLN('ZE4V')
      ENDIF
C
      NPHEAD = IW(IPHEAD)
      IF( NPHEAD .GT. 0 ) THEN
        IRUN = HW( NPHEAD*2+10)
        IEVT = HW( NPHEAD*2+11)
        HELP(1) = IRUN
        HELP(2) = IEVT
        IRNEVT = IHELP
      ELSE
        NPZE4V = IW(IPZE4V)
        IF( NPZE4V.GT.0 ) THEN
          IRNEVT = IW(NPZE4V+7)
        ELSE
          IRNEVT = 0
        ENDIF
      ENDIF
      END
