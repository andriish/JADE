C   20/08/87 806091144  MEMBER NAME  ZE4VTP   (S)        M  FORTRAN77
      SUBROUTINE ZE4VTP
C-----------------------------------------------------------
C  Version of 08/06/88     E ELSEN
C  Last mod   08/06/88     E ELSEN
C  Fill information originating from TP into ZE4V bank.
C-----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / BCS / IW(1)
      DIMENSION HW(1),RW(1)
      EQUIVALENCE (HW(1),IW(1),RW(1))
      INTEGER LADD(-1:2)
      LOGICAL FIRST 
      DATA FIRST /.TRUE./
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
        NP = HW(NPZE4V*2+1) + NPZE4V
C
        DO 10 J=1,NT
          ITPF = HW(NP*2+18)
          IF( ITPF.EQ.1 ) THEN
            NTPTR = HW(NP*2+17)
            CALL CLOC( NPTPTR, 'TPTR', NTPTR )
            IF( NPTPTR.GT.0 ) THEN
C                                       n dE/dx
              HW((NP+LT)*2+25) = IW(NPTPTR+72)
C                                       dE/dx
              RW(NP+LT+ 9) = RW(NPTPTR+73)
C                                       s(dE/dx)
              RW(NP+LT+10) = RW(NPTPTR+74)
C                                       Muon Quality or unused (= 0 )
              MUONQU = HW( NPTPTR*2 + 96 )
              IW(NP+LT+12) = MUONQU
              IF ( MUONQU .EQ. 9 )
     *          HW( NP*2 + 8 ) = HW( NP*2 + 8 )/100*100 + 3
C                                      TOF RESULTS (BETA)
              IF ( ABS( IW( NPTPTR + 58 )) .EQ. 1 ) THEN
                HW( (NP+LT)*2 + 28 ) =
     *             INT( RW( NPTPTR+61 )*1000.) * IW( NPTPTR + 58 )
              ENDIF
            ELSE
              HW((NP+LT)*2+25) = 0
              RW(NP+LT+ 9) = 0.
              RW(NP+LT+10) = 0.
              IW(NP+LT+12) = 0
              HW( (NP+LT)*2 + 28 ) = 0
C                                           erase muon flag
*** PMF 09/12/99              IF( MOD( HW(NP*2+8), 100 ).EQ.3 ) THEN
* Shuffle first MOD argument into an integer*4 variable.
              ihelp=HW(NP*2+8)
              IF( MOD( ihelp, 100 ).EQ.3 ) THEN
*** PMF(end)
                  HW(NP*2+8) = HW( NP*2 + 8 )/100*100
              ENDIF
            ENDIF
          ENDIF
          NP = NP + LADD(ITPF)
   10   CONTINUE
      ENDIF
      END
