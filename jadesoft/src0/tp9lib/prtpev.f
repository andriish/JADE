C   08/11/84            MEMBER NAME  PRTPEV   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE  PRTPEV( JUNIT )
C-----------------------------------------------------------------------
C
C   AUTHOR:    C. BOWDERY     8/11/84 : PRINTS OUT THE TPEV BANK
C
C      MOD:    C. BOWDERY    11/12/84 : CORRECT MISTAKE IN Q1,Q2,Q3
C LAST MOD:    C. BOWDERY     2/10/86 : UNIT NUMBER IS A PARAMETER
C
C
C      INPUT:     JUNIT  = LOGICAL UNIT NUMBER FOR OUTPUT
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2  (H)
C
#include "cdata.for"
C
C------------------  C O D E  ------------------------------------------
C
      IUNIT = JUNIT
      IF( JUNIT .LT. 1  .OR.  JUNIT .GT. 99 ) IUNIT = 6
C
      CALL CLOC( ITPEV, 'TPEV', 1 )
      IF( ITPEV .GT. 0 ) GO TO 5
        WRITE(IUNIT,1)
   1    FORMAT(' ***  WARNING  ***  PRTPEV FOUND NO TPEV/1 BANK'//)
        RETURN
C
C                           TPEV BANK/1 IS THERE
C
   5  ITPEV2 = ITPEV * 2
      WRITE(IUNIT,7) IDATA( ITPEV )
   7  FORMAT('1'//20X,
     +     '*****   T P E V   *****'//6X,'BANK:  1',8X,'LENGTH:',I5/)
C
C
C                            PARTICLES
C
C
      WRITE(IUNIT,9) IDATA( ITPEV + 1 ), IDATA( ITPEV + 2 )
   9  FORMAT(6X,'VERSION NUMBER:',I3,8X,'PRODUCTION DATE AND TIME:',I10)
C
      NTR  = HDATA( ITPEV2 +  5 )
      NTRP = HDATA( ITPEV2 +  6 )
      NTRM = HDATA( ITPEV2 +  7 )
      NTRX = HDATA( ITPEV2 +  8 )
      NTRN = HDATA( ITPEV2 +  9 )
      NTRB = HDATA( ITPEV2 + 10 )
      NTRF = HDATA( ITPEV2 + 11 )
C
      WRITE(IUNIT,12) NTR, NTRP, NTRM, NTRX, NTRN, NTRB, NTRF
  12  FORMAT(/'   PARTICLES     TOTAL   +    -    ?   Q=0   B    F'/
     +        '                 -----  ---  ---  ---  ---  ---  ---'//
     +        18X,I3,6(3X,I2) /)
C
C
C                            NUMBER OF VERTICES
C
C
      NVX  = HDATA( ITPEV2 + 12 )
      NVXN = HDATA( ITPEV2 + 13 )
      NVXC = HDATA( ITPEV2 + 14 )
C
      WRITE(IUNIT,15) NVX, NVXN, NVXC
  15  FORMAT(/'   VERTICES      TOTAL   N    C'/
     +        '                 -----  ---  ---'//
     +        18X,I2,1X,2(3X,I2) /)
C
C
C                            PARTICLE TYPES
C
C
      NPHOT = HDATA( ITPEV2 + 15 )
      NEP   = HDATA( ITPEV2 + 16 )
      NEM   = HDATA( ITPEV2 + 17 )
      NMUP  = HDATA( ITPEV2 + 18 )
      NMUM  = HDATA( ITPEV2 + 19 )
C
      NPIP  = HDATA( ITPEV2 + 20 )
      NPIM  = HDATA( ITPEV2 + 21 )
      NPI0  = HDATA( ITPEV2 + 22 )
      NKP   = HDATA( ITPEV2 + 23 )
      NKM   = HDATA( ITPEV2 + 24 )
      NK0S  = HDATA( ITPEV2 + 25 )
C
      NETA  = HDATA( ITPEV2 + 26 )
      NP    = HDATA( ITPEV2 + 27 )
      NPBAR = HDATA( ITPEV2 + 28 )
      NLAMB = HDATA( ITPEV2 + 29 )
      NX    = HDATA( ITPEV2 + 30 )
C
      NFLAG = HDATA( ITPEV2 + 31 )
      NSP1  = HDATA( ITPEV2 + 32 )
      NSP2  = HDATA( ITPEV2 + 33 )
      NSP3  = HDATA( ITPEV2 + 34 )
C
      WRITE(IUNIT,18) NPHOT, NEP, NEM, NMUP, NMUM, NPIP, NPIM, NPI0,
     +            NKP, NKM, NK0S, NETA, NP, NPBAR, NLAMB, NX,
     +            NFLAG, NSP1, NSP2, NSP3
  18  FORMAT(/'   TYPES       GAM   E+   E-  MU+  MU-  PI+  PI-  PI0',
     +                     '   K+   K-  K0S  ETA   P    P-  LAM   ? ',
     +                     '  FLAG   3 SPARE'/
     +                   13X,16('  ---'),'  ----   -------'//
     +        12X,16(3X,I2),3X,I4,1X,3(2X,I1) / )
C
C
C                            VISIBLE ENERGY, MISSING MOMENTUM
C
C
      EVISC  = ADATA( ITPEV + 18 )
      SIGEVC = ADATA( ITPEV + 19 )
      EVISN  = ADATA( ITPEV + 20 )
      SIGEVN = ADATA( ITPEV + 21 )
C
      PMISSX = ADATA( ITPEV + 22 )
      PMISSY = ADATA( ITPEV + 23 )
      PMISSZ = ADATA( ITPEV + 24 )
      SIGPMX = ADATA( ITPEV + 25 )
      SIGPMY = ADATA( ITPEV + 26 )
      SIGPMZ = ADATA( ITPEV + 27 )
C
      WRITE(IUNIT,22) EVISC, SIGEVC, EVISN, SIGEVN, PMISSX, PMISSY,
     +            PMISSZ,SIGPMX, SIGPMY, SIGPMZ
  22  FORMAT(/'   EVIS,PMISS               VISIBLE ENERGY           ',
     +        '                       MISSING MOMENTUM'
     +       /'                 CHARGED  SIGMA(C)  NEUTRAL  SIGMA(N)',
     +        '       X       Y      Z     SIGMA(X)  SIGMA(Y)  SIGMA(Z)'
     +       /'                 -------  --------  -------  --------',
     +        '    ------  ------  ------  --------  --------  --------'
     +      //'                 ',F6.3,4X,F6.3,3X,F6.3,4X,F6.3,
     +         4X,F7.3,1X,F7.3,1X,F7.3,2X,F7.3,3X,F7.3,3X,F7.3/)
C
C
C                            SPHERICITY
C
C
      ISPHFL = HDATA( ITPEV2 + 55 )
      ISPHTR = HDATA( ITPEV2 + 56 )
C
      ALPHA1 = ADATA( ITPEV + 29 )
      ALPHA2 = ADATA( ITPEV + 30 )
      ALPHA3 = ADATA( ITPEV + 31 )
C
      DCX1   = ADATA( ITPEV + 32 )
      DCY1   = ADATA( ITPEV + 33 )
      DCZ1   = ADATA( ITPEV + 34 )
      DCX2   = ADATA( ITPEV + 35 )
      DCY2   = ADATA( ITPEV + 36 )
      DCZ2   = ADATA( ITPEV + 37 )
      DCX3   = ADATA( ITPEV + 38 )
      DCY3   = ADATA( ITPEV + 39 )
      DCZ3   = ADATA( ITPEV + 40 )
C
      WRITE(IUNIT,25) ISPHFL, ISPHTR, ALPHA1, ALPHA2, ALPHA3,
     +            DCX1, DCY1, DCZ1,
     +            DCX2, DCY2, DCZ2, DCX3, DCY3, DCZ3
  25  FORMAT(/'   SPHERICITY      FLAG  TRACKS   ALPHA1 ALPHA2 ALPHA3 ',
     + ' DCX1   DCY1   DCZ1   DCX2   DCY2   DCZ2   DCX3   DCY3   DCZ3'/
     +        '                   ----  ------   ------ ------ ------ ',
     + ' ----   ----   ----   ----   ----   ----   ----   ----   ----'//
     +        '                     ',I1,5X,I2,5X,3(F5.3,2X),
     +     9(F6.3,1X)  /)
C
C
C                            THRUST
C
C
      ITHTR1 = HDATA( ITPEV2 + 81 )
      ITHTR2 = HDATA( ITPEV2 + 82 )
C
      THRUST = ADATA( ITPEV + 42 )
      DCTHX  = ADATA( ITPEV + 43 )
      DCTHY  = ADATA( ITPEV + 44 )
      DCTHZ  = ADATA( ITPEV + 45 )
C
      UNUSE1 = ADATA( ITPEV + 46 )
      UNUSE2 = ADATA( ITPEV + 47 )
      UNUSE3 = ADATA( ITPEV + 48 )
      UNUSE4 = ADATA( ITPEV + 49 )
      UNUSE5 = ADATA( ITPEV + 50 )
C
      WRITE(IUNIT,28) ITHTR1, ITHTR2, THRUST, DCTHX, DCTHY, DCTHZ,
     +            UNUSE1, UNUSE2, UNUSE3, UNUSE4, UNUSE5
  28  FORMAT(/'   THRUST          TRACKS  USED   THRUST    DCX     DCY',
     +        '     DCZ         5 UNUSED'/
     +        '                   ------  ----   ------    ---     ---',
     +        '     ---         --------'//
     +        '                     ',I2,5X,I2,5X,F5.3,2X,3(F7.3,1X),
     +             5(2X,F3.1) /)
C
C
C                            2 PRONG DATA
C
C
      BPTOF  = ADATA( ITPEV + 51 )
      DTOF   = ADATA( ITPEV + 52 )
      COLLIN = ADATA( ITPEV + 53 )
      ACOPL  = ADATA( ITPEV + 54 )
      UNUSE6 = ADATA( ITPEV + 55 )
C
      WRITE(IUNIT,32) BPTOF, DTOF, COLLIN, ACOPL, UNUSE6
  32  FORMAT(/'   2 PRONGS        BPTOF  DIFTOF  COLLIN  ACOPL  UNUSED'/
     +        '                   -----  ------  ------  -----  ------'/
     +       /18X,F7.3,1X,F6.3,2X,F6.3,1X,F6.3,4X,F3.1/)
C
C
C                            ERROR FLAGS
C
C
      IERRPT = HDATA( ITPEV2 + 111 )
      IERRVF = HDATA( ITPEV2 + 112 )
      IERRTF = HDATA( ITPEV2 + 113 )
      IERRDX = HDATA( ITPEV2 + 114 )
      IERRLG = HDATA( ITPEV2 + 115 )
      IERRMU = HDATA( ITPEV2 + 116 )
      IERRFW = HDATA( ITPEV2 + 117 )
      IERRV0 = HDATA( ITPEV2 + 118 )
      IERRJA = HDATA( ITPEV2 + 119 )
      IERRNU = HDATA( ITPEV2 + 120 )
C
      WRITE(IUNIT,35) IERRPT, IERRVF, IERRTF, IERRDX, IERRLG, IERRMU,
     +            IERRFW, IERRV0, IERRJA, IERRNU
  35  FORMAT(/'   ERROR FLAGS     PATR    VXFIT    TOF     DEDX',
     +        '     LG     MUON   FWDET   VEES    JETAN     -   '/
     +        '                   -----   -----   -----   -----',
     +        '   -----   -----   -----   -----   -----   -----'//
     +        '                ',10(3X,I5) /)
C
      RETURN
      END
