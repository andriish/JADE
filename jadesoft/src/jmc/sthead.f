C   19/04/79 712211435  MEMBER NAME  STHEAD   (S)           FORTRAN
      SUBROUTINE STHEAD( LENGTH, IHEAD )
C-----------------------------------------------------------
C
C  VERSION OF 18/04/79                       E.ELSEN
C     CHANGED TO SET WORD 16 IN HEAD BANK TO 1, TO MARK THE USAGE OF
C     NEW, CORRECTED VERSION OF TRKGAM    SEE JCN NR     .
C                       MOD. 15.2.1986       J.OLSON
C     CHANGED TO SET WORD 18 IN HEAD BANK TO -1, TO MARK IF TRACKING WAS
C     DONE WITHOUT MULTIPLE SCATTERING AND ENERGY LOSS (FLAGS= ELOSS,
C                                                              MULSC)
C     CHANGED TO SET WORD 17 IN HEAD BANK TO 1, IF LFLAG(4)=.TRUE.
C     THIS MARKS THE USE OF MEIER/MAGNUSSEN LEAD GLASS SIMULATION.
C
C                  LAST MOD. 10.06.1986       J.HAGEMANN
C                  LAST MOD. 11.06.1986       J.OLSSON
C
C     PRODUCTION DATE NOW SAVED IN HALF WORDS 93-95
C                         18/12/87    E ELSEN
C
C  SET FIRST 50 WORDS OF BANK 'HEAD', CONTAINING EVENT AND
C  RUN INFORMATION ( SEE JADE COMPUTER NOTE 23 )
C  EXTRA ENTRY LHHEAD RETURN LENGTH ONLY.
C----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
      COMMON / BCS / IW(1)
      DIMENSION RW(1), HW(1)
      EQUIVALENCE (HW(1),RW(1),IW(1))
      DIMENSION IHEAD(50)
C
      LOGICAL  ELOSS, MULSC
      COMMON / CJSWLO / ITIMOD, MULSC, ELOSS
C
      LOGICAL*1 LFLAG
      COMMON/CFLAG/LFLAG(10)
C
      COMMON / CGEO1 / BKGAUS
      DIMENSION HELP(2)
      EQUIVALENCE (IHELP,HELP(1))
      DATA HRCTYP / 1 /, HROPAT / Z'61CC' /
      DATA ICALL / 0 /
C
      COMMON/TODAY/IDATE(3)
      DIMENSION HPROD(6)
C
      IF( ICALL .GT. 0 ) GO TO 1
      ICALL = 1
      IPVECT = IBLN('VECT')
    1 CONTINUE
      IBEAM = 0
      NR = 0
      NPVECT = IW(IPVECT)
      IF( NPVECT .LE. 0 ) GO TO 2
      NR = IW(NPVECT+3)
      L0 = IW(NPVECT+1)
      IF( L0 .LE. 12 ) GO TO 2
      IBEAM = IW(NPVECT+13)
    2 CONTINUE
C
C
      IHEAD(2) = IDATE(1)
      IHEAD(3) = IDATE(2)
      IHEAD(4) = IDATE(3)
C
      HELP(1) = NR
      HELP(2) = HRCTYP
      IHEAD(6) = IHELP
C
      HELP(1) = HROPAT
      HELP(2) = 0
      IHEAD(7) = IHELP
C
C   NEW TRKGAM MARKER,   SEE JCN NR     .     15.2.1986
C
      HELP(1) = 0
      HELP(2) = 1
      IHEAD(8) = IHELP
C
C   NEW MARKER FOR USE OF MEIER/MAGNUSSEN LG TRACKING     11.06.86
C
      HELP(1) = 0
      IF(.NOT.LFLAG(4)) GO TO 647
      HELP(1) = 1
C
C   NEW MARKER FOR MULTIPLE SCATTERING AND ENERGY LOSS    10.06.86
C
647   HELP(2) = 0
      IF( .NOT.ELOSS .AND. .NOT.MULSC ) HELP(2) = -1
      IHEAD(9) = IHELP
C
      HELP(1) = IBEAM
      HELP(2) = BKGAUS * 1000.
      IHEAD(15) = IHELP
C
C                                           STORE PRODUCTION DATE
      CALL DATEMC( HPROD )
      HELP(1) = HPROD(4)
      HELP(2) = HPROD(5)
      IHEAD(47) = IHELP
      IHELP = IHEAD(48)
      HELP(1) = HPROD(6)
      IHEAD(48) = IHELP
C
C-----------------------------------------------------------
      ENTRY LHHEAD( LENGTH )
C-----------------------------------------------------------
      LENGTH = 100
      RETURN
      END
