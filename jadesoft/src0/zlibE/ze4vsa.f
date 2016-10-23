C   07/11/86 702121551  MEMBER NAME  ZE4VTP   (S)           FORTRAN77
      SUBROUTINE ZE4VTP( IGEN )
C-----------------------------------------------------------
C VERSION OF 11/11/86       M.ZIMMER
C LAST MOD   27/11/86       M.ZIMMER
C LAST MOD   29/01/87       G.ECKERLIN
C NEW VERSION FOR NEW ZE4V-BANK
C PACK INFORMATION AS FAR AS POSSIBLE FROM TP-BANK
C-----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / BCS / IW(1)
      DIMENSION RW(1), HW(1)
      EQUIVALENCE (HW(1),RW(1),IW(1))
C
      COMMON P(10,400)
      DIMENSION IP(10,100),HP(20,1)
      EQUIVALENCE (P(1,1),IP(1,1),HP(1,1))
C
      DIMENSION R(3), VECTB(1000)
      INTEGER NWARN(2) / 2*0 /
      DATA IEV / 0 /, IEVLIM / 0 /
C
      EXTERNAL VECSUB
C
C                                           CUT PARAMETERS
C                                           RMIN CUT
      DATA RMINCT /  50. /
C                                           Z-CUT
      DATA ZCT    / 350. /
C                                           MIN R-PHI-HITS
      DATA NRPHCT / 20 /
C                                           MIN R-Z-HITS
      DATA NRZCT  / 12 /
C                                           MOMENTUM CUT
      DATA PCT   / .05 /
C                                           LG MIN ENERGY
      DATA ELGPHM / .150 /
C                                           LG SUBTRACTION ENERGY
      DATA ELGMIN /    .350 /
C                                           FORMAT OF ZE4V BANK
      DATA  LH / 31 /, LT / 9 /, LVX / 3 /, LMC / 0 /,
     & LTCH / 12 /, LTNE / 5 /, LTRE / 0 /
C                                      MIN/MAX OF PART. FOR THR-SPH-AKO
C                                      FOR TEST DATA ONLY
      DATA NPAMIN / 1 / , NPAMAX / 7 /

      LVECS = 0
      GO TO 1
      ENTRY ZE4VUP( NST, N, IGEN )
C--------------------------------------------------------
C  ENTRY FOR VECSUB OPTION
C  LEAVES PARTICLES IN BLANK COMMON
C--------------------------------------------------------
      LVECS = 1
    1 CONTINUE
      IEV = IEV + 1
C
C                                      LOOK FOR BANKS
      NPZE4V = IW( IBLN('ZE4V'))
      IF( NPZE4V .GT. 0 ) CALL BDLS( 'ZE4V',IGEN)
      NPTPEV = IW(IBLN('TPEV'))
      IF( NPTPEV .LE. 0 )   CALL FEHLER( 1, &8000 )
      NTR = HW( NPTPEV*2 + 5 )
      IF( NTR .LE. 0 )   CALL FEHLER( 3, &8000 )
      NVX = HW( NPTPEV*2 + 12 )
      IF ( NVX .LT. 1 )     CALL FEHLER( 5, &8000 )
      NPPATR = IW( IBLN('PATR'))
      IF( NPPATR .LE. 0)    CALL FEHLER( 7, &8000 )
      LTPATR = IW( NPPATR + 3 )
      LHPATR = IW( NPPATR + 1 )
C
C                                      LENGTH OF HEADER
      LHEAD = LH + NVX*LVX
C                                      IF MC : LHEAD + LMC
      IF (MCFLAG.NE.0) LHEAD = LHEAD + LMC

C                                           CREATE ZE4V HEAD
      CALL BCRE( NPZE4V, 'ZE4V', IGEN, LHEAD, &8100, IER )
      CALL BSAW( 1, 'ZE4V' )
C
      HW(NPZE4V*2+ 1) = LHEAD
      HW(NPZE4V*2+ 2) = LVX
      HW(NPZE4V*2+ 3) = NVX
C                                      MONTE-CARLO PART
      HW(NPZE4V*2+ 4) = LMC
C                                      GENARAL TRACK PART
      HW(NPZE4V*2+ 5) = LT
C                                      TOTAL NUMBER OF PARTICLES
      HW(NPZE4V*2+ 6) = 0
C                                      CHARGED TRACK PART
      HW(NPZE4V*2+ 7) = LTCH
      HW(NPZE4V*2+ 8) = 0
C                                      NEUTRAL TRACK PART
      HW(NPZE4V*2+ 9) = LTNE
      HW(NPZE4V*2+10) = 0
C                                      RECONSTRUCTED TRACK PART
      HW(NPZE4V*2+11) = LTRE
      HW(NPZE4V*2+12) = 0
C                                      RUN/EVENT #
      IW(NPZE4V  + 7) = IRNEVT( DUMMY )
C                                           EBEAM
      EBE = EBGENL( DUMMY )
      RW(NPZE4V  + 8) = EBE
C                                      E-VIS FROM TP (CH + NEUTR)
      RW(NPZE4V  + 29) = RW( NPTPEV + 18) + RW( NPTPEV + 20 )
C                                      E-VIS FROM TP (NEUTRAL)
      RW(NPZE4V  + 30) = RW( NPTPEV + 20)
C                                      ACCEPT EVENT BY MCREDU
      MRFLAG = -1
      CALL MCREDU( 0., &2040 )
C                                      EVENT ACCEPTED
      MRFLAG = 1
 2040 CONTINUE
      IW( NPZE4V + 31 ) = MRFLAG
C
C                                           SET VERTEX HERE
      KPTPVX = IW( IBLN( 'TPVX'))
      IF( KPTPVX .LE. 0 ) CALL FEHLER( 34, &3546 )
 3546 CONTINUE
      CALL BPOS( 'TPVX' )
C                                      FILL VERTEX
      DO 1020 I0 = 1,NVX
        CALL BNXT( NPTPVX, &8020 )
        DO 1020 I=1,3
          IF( NPTPVX .LE. 0 ) THEN
            R(I) = 0.
          ELSE
            R(I) = RW( NPTPVX + 1 + I )
          ENDIF
          RW(NPZE4V + LH + LVX*(I0-1) + I) = R(I)
 1020 CONTINUE
C                                           MC PART OF HEADER
C                                           FILLED FROM VECT BANK
C
      IF(MCFLAG.EQ.0.) GOTO 4213
C
      NPVECT = IW(IBLN('VECT'))
      IF(NPVECT.GT.0) GO TO 4119
       WRITE(6,9212) HW(NPZE4V*2+14),HW(NPZE4V*2+13)
9212   FORMAT(' NO VECT BANK IN EVENT ',I8,'  RUN ',I8)
       GOTO 4213
4119  CONTINUE
C
C                                        START VECT-ANALYSIS
C
C                                        POINTER TO MC PART IN ZE4V
      NPMC = LH + NVX*LVX

      HLENG = IW(NPVECT+1)
      IEVT = IW(NPVECT+3)
      NPARMC = IW(NPVECT+4)
      DO 4118 IMC=1,6
      VECTB(IMC) = FLOAT(IW(NPVECT+IMC))
4118  CONTINUE
      VECTB(7) = RW(NPVECT+7)
      VECTB(8) = RW(NPVECT+8)
      VECTB(9) = FLOAT(IW(NPVECT+9))
      NFLAV = IABS(IW(NPVECT+9))
      VECTB(10) = 0.
      DO 4320 IMC = 1,NPARMC
      JMC = 10 * IMC
      K = JMC + HLENG - 10
      VECTB(JMC+1) = RW(NPVECT+K+1)
      VECTB(JMC+2) = RW(NPVECT+K+2)
      VECTB(JMC+3) = RW(NPVECT+K+3)
      VECTB(JMC+4) = RW(NPVECT+K+4)
      VECTB(JMC+5) = RW(NPVECT+K+5)
      VECTB(JMC+8) = RW(NPVECT+K+8)
      VECTB(JMC+9) = RW(NPVECT+K+9)
      VECTB(JMC+10) = RW(NPVECT+K+10)
      VECTB(JMC+6) = FLOAT(IW(NPVECT+K+6))
      VECTB(JMC+7) = FLOAT(IW(NPVECT+K+7))
4320  CONTINUE
C
C                                        FIND NO OF JETS
C
      NJET = 0
      DO 4110 IMC=1,4
      IND = (NPARMC-IMC+1)*10
      IF(VECTB(IND+7).GE.-100.) GOTO 4313
      NJET = NJET + 1
      RW(NPMC + (IMC-1)*4 + 2) = VECTB(IND+1)
      RW(NPMC + (IMC-1)*4 + 3) = VECTB(IND+1)
      RW(NPMC + (IMC-1)*4 + 4) = VECTB(IND+1)
      RW(NPMC + (IMC-1)*4 + 5) = VECTB(IND+1)
      GOTO 4110
4313  CONTINUE
      RW(NPMC + (IMC-1)*4 + 2) = 0.
      RW(NPMC + (IMC-1)*4 + 3) = 0.
      RW(NPMC + (IMC-1)*4 + 4) = 0.
      RW(NPMC + (IMC-1)*4 + 5) = 0.
4110  CONTINUE
      HW(NPMC*2 + 1) = NJET
      HW(NPMC*2 + 2) = NFLAV
C
4213  CONTINUE
C
C                                           END OF MC PART
C
      NPPHOT = IW( IBLN('PHOT'))
C                                           LOOP OVER TPTR BANKS
      CALL BPOS( 'TPTR' )
C                                           LOOP OVER TRACKS
      DO 300 J = 1, NTR
        NRPHI = 0
        NRZ = 0
        CALL BNXT( NPTPTR, &300 )
        INFOVX = HW( NPTPTR*2 + 2 )
        IF( INFOVX .LT. 0 .OR. INFOVX .GT. 10 )
     &                                CALL FEHLER( 15, &300 )
        CHARGE = RW( NPTPTR + 23 )
C
        IF ( ABS( CHARGE ) .GT. .5 .AND. NPPATR .GT. 0 )     THEN
           NRPATR = HW( NPTPTR*2 + 4)
           NPTRAK = NPPATR + LHPATR + (NRPATR-1)*LTPATR
           CALL PRTOCI( NPTRAK, CAP, RMIN, PHIM, SIG )
           NRPHI = IW (NPTRAK + 24)
           NRZ   = IW (NPTRAK + 33)
           IF( NRPHI .LT. NRPHCT )   CALL FEHLER( 17, &300 )
           IF( NRZ .LT. NRZCT )         CALL FEHLER( 19, &300 )
C          RMIN = RW( NPTPTR + 16 )
           IF( ABS( RMIN ) .GT. RMINCT ) CALL FEHLER ( 11, &300 )
           AZV = ABS( RW( NPTRAK + 31) )
           IF( AZV .GT. ZCT )           CALL FEHLER( 21, &300 )
        ENDIF
        PTOT = RW( NPTPTR + 24 )
        IF( PTOT .LT. PCT )  THEN
                                      CALL FEHLER ( 13, &300 )
               WRITE(6,*) '!!!!  PTOT NOT PASSED !!!!!!!!!!!!!!!!!'
           ENDIF
C                                      ACCEPT THIS TRACK
C                                      FILL GLOBAL PART FOR EACH TRACK
C                                      EXTEND BANK
        CALL BCHM( NPZE4V, LT, IER )
C                                      POINTER TO NEXT ZE4V-WORD
        NP = NPZE4V + LHEAD + HW( NPZE4V*2 + 6) * LT
     &                      + HW( NPZE4V*2 + 8) * LTCH
     &                      + HW( NPZE4V*2 +10) * LTNE
     &                      + HW( NPZE4V*2 +12) * LTRE
        NP2 = NP * 2
C                                      EX,EY,EZ
        DO 1200 I = 1,3
          RW( NP + I ) = RW( NPTPTR + 26 + I )
 1200   CONTINUE
C                                      PARTICLE CODE (MC-CONV)
        HW(NP2 + 8) = HW(NPTPTR*2 + 68 )
C                                      # OF VERTEX OF PARTICLE ORIGIN
        HW(NP2 + 9) = HW( NPTPTR*2 + 1)
C                                      # OF SEC VERTEX
        HW(NP2 +10) = HW( NPTPTR*2 + 2)
C                                      MOMENTUM
        FAC = 1.
        IF( PTOT .GT. EBE ) FAC = .5*EBE /PTOT
        RW(NP + 6) = PTOT*FAC
        RW(NP + 7) =  CHARGE
C
        NPART = 0
        IF( NPPATR .GT. 0 .AND. NPPHOT  .GT. 0 ) THEN
          NPHOT = NPPHOT
  210     CONTINUE
          IF( NPHOT.LE.0 .OR. NPART.GT.0 ) GO TO 215
          IF(IW(NPHOT+24).EQ.IW(NPTRAK+1)) NPART = IW(NPHOT+25)
          IF(IW(NPHOT+25).EQ.IW(NPTRAK+1)) NPART = IW(NPHOT+24)
          NPHOT = IW( NPHOT - 1 )
          GO TO 210
  215   ENDIF
C                                      PATR NUMBER OF PHOT PARTNER
  220   HW( NP2 + 7 ) = NPART
C                                           MC PART
C                                      0 OR PART# IN PALL BANK (HW(+15))
C                                      0 OR ELECTRON ORIGIN    (HW(+16))
        IW( NP + 8 ) = 0
CCC     IF( HW(NPZE4V*2+5).LT.100 .AND. NPPATR .GT. 0)
        IF( MCFLAG.NE.0 .AND. NPPATR .GT. 0)
     *      CALL ZE4VMC( IW(NPTRAK+1), IW( NP+8 ) )
C                                        # OF TPTR BANK
        HW( NP2 + 17 ) = IW( NPTPTR - 2 )
C
C                                      LG-INFORMATION
        JCLUS1 = HW(NPTPTR*2 + 6)
        JCLUS2 = HW(NPTPTR*2 + 7)
        SIGECL =  0.
        ECL    =  0.
        JBAREC =  0
        NBLOCK = 0
        IF (JCLUS1.LE.0) GOTO 3123
        NPLGCL = IW(IBLN('LGCL'))
        IF ( NPLGCL .GT. 0 ) THEN
          IPENDE = IW( NPLGCL + 4 )
          IP3 = IW( NPLGCL + 3 )
          NWPCL = IW( NPLGCL + 25 )
          NPOINT = NPLGCL + IP3 + (JCLUS1-1) * NWPCL -1
          IF ( NPOINT .GT. ( NPLGCL + IPENDE )) CALL FEHLER( 23,&3123)
          ECL    = RW( NPOINT + 2 )
          SIGECL = RW( NPOINT + 3 )
          JBAREC = IW( NPOINT + 1 )
          ICON = IW(NPOINT + 8)
          ICL =    ( NPLGCL + 25 + JCLUS1 ) * 2
          NBLOCK = HW(ICL) - HW(ICL-1) + 1 - ICON/100
C                               CORRECT ENERGY FOR CONNECTED CLUSTER
          DZDS = 1.
          IF( JBAREC.EQ.0 ) DZDS = 1./SQRT(1-RW(NPOINT+11)**2)
          IF( ICON .NE. 0 .AND. FAC .EQ. 1.)
     &    RW(NP + 6) = RW(NP + 6) - (ICON/100) * ELGMIN * DZDS
        ELSE
          CALL FEHLER( 25, &3123)
        ENDIF
 3123   CONTINUE
C
C                                      FILL SPECIAL TRACK PART
C
C------------------------------------- CHARGED PRIMARY -----------
C
        IF( ABS(CHARGE) .GT. .5 .AND. INFOVX .EQ. 0 ) THEN
C
          HW(NPZE4V*2 + 8 ) = HW(NPZE4V*2 + 8) + 1
          HW(NPZE4V*2 + 6 ) = HW(NPZE4V*2 + 6) + 1
          HW(NP2 + 18 ) = 1
          CALL BCHM( NPZE4V, LTCH, IER )
C                                      CLUSTER-ENRGY IN GEV
          RW( NP  + LT + 1 ) = ECL
C                                      ERROR IN CLUSTER-ENRGY
          RW( NP  + LT + 2 ) = SIGECL
C                                      CORRECTED CLUSTER ENERGY(NOT YET)
          RW( NP  + LT + 3 ) = 0.
C                                      DETECTOR FLAG
          HW( NP2 + LT*2 + 7) = HW( NPTPTR*2 + 3 )
C                                      #OF 1. AND 2. CONN. CLUSTER
          HW( NP2 + LT*2 + 8) = JCLUS1 + 100*JCLUS2
C                                      # IN PATR-BANK
          HW( NP2 + LT*2 + 9) = HW( NPTPTR*2 + 4 )
C                                      NRPHI + 100*NRZ
          HW( NP2 + LT*2 +10) = NRPHI + 100*NRZ
C                                      (X,Y,Z) TRACK AT DOCA
          RW( NP + LT + 6 ) = RW( NPTPTR + 10 )
          RW( NP + LT + 7 ) = RW( NPTPTR + 11 )
          RW( NP + LT + 8 ) = RW( NPTPTR + 12 )
C                                      DEDX
          RW( NP + LT + 9 ) = RW( NPTPTR + 73 )
          RW( NP + LT +10 ) = RW( NPTPTR + 74 )
          RW( NP + LT +11 ) = RMIN
C                                      MUON QUALITY NUMBER
          RW( NP + LT +12 ) = HW( NPTPTR*2 + 96 )
C
C------------------------------------- NEUTRAL PRIMARY
C
        ELSEIF ( ABS(CHARGE) .LT. .5 .AND. INFOVX .EQ. 0 ) THEN
C
          HW(NPZE4V*2 + 10) = HW(NPZE4V*2 + 10) + 1
          HW(NPZE4V*2 + 6)  = HW(NPZE4V*2 + 6 ) + 1
          HW(NP2 + 18 ) = 0
          CALL BCHM( NPZE4V, LTNE, IER )
C                                      CLUSTER-ENRGY IN GEV
          RW( NP + LT + 1 ) = ECL
C                                      ERROR IN CLUSTER-ENRGY
          RW( NP + LT + 2 ) = SIGECL
C                                      CORRECTED CLUSTER ENERGY(NOT YET)
          RW( NP + LT + 3 ) = 0.
C                                      LG DETECTOR PART
          HW( NP2 + LT*2 + 7) = JBAREC
C                                      #OF 1. AND 2. CONN. CLUSTER
          HW( NP2 + LT*2 + 8) = JCLUS1 + 100*JCLUS2
C                                      NUMBER OF LG-BL. - CONN. TRACKS
          HW( NP2 + LT*2 + 9) = NBLOCK
          HW( NP2 + LT*2 +10) = 0
C
C------------------------------------- RECONSTRUCTED SECONDARY
C
        ELSEIF( INFOVX. GT. 0 ) THEN
C
          HW( NPZE4V*2 + 12) = HW( NPZE4V*2 + 12) + 1
          HW( NPZE4V*2 + 6 ) = HW( NPZE4V*2 + 6 ) + 1
          HW(NP2 + 18 ) = 2
          CALL BCHM( NPZE4V, LTRE, IER )
C
        ELSE
          CALL BCHM( NPZE4V, (-1)*LT, IER)
          CALL FEHLER( 27, &300 )
        ENDIF
  300 CONTINUE
C
 2050 CONTINUE
C
      IF( LVECS.NE.0 ) GO TO 2100
         N = HW( NPZE4V*2 + 6 )
         IF( N .LT. NPAMIN ) THEN
            CALL BDLS( 'ZE4V', IGEN )
                             CALL FEHLER( 31, &8000)
         ELSEIF ( N .GT. NPAMAX ) THEN
            CALL BDLS( 'ZE4V',IGEN )
                             CALL FEHLER( 33, &8000)
         ENDIF
C
         NST = 0
C                                           SAVE AWAY BLANK COMMON
         CALL BCRE( NPBUF, '--BU', 0 , 10*(N+5), &8200, IER )
         CALL UCOPY( P(1,NST+1), RW(NPBUF+1), 10*(N+5) )
 2100 CONTINUE
      IF( IEV.LT.IEVLIM) CALL BPRS( 'ZE4V', IGEN )
      CALL ZE4VUN( NST, N, 0 )
      IF( N.LE.0 ) GO TO 2150
         IF( IEV.LT.IEVLIM) CALL PWRT( NST+1, NST+N, 10000 )
         NPZE4V = IW(IBLN('ZE4V'))
         CALL SPTHAK( NST+1, NST+N, NST+N+4, THR,
     &                              NST+N+1, SPH,
     &                              NST+N+5, AKO, IER )
C                                      SPHERICITY
         CALL UCOPY( P(1,NST+N+1), RW(NPZE4V+ 9), 4 )
         CALL UCOPY( P(1,NST+N+2), RW(NPZE4V+13), 4 )
         CALL UCOPY( P(1,NST+N+3), RW(NPZE4V+17), 4 )
C                                      THRUST
         CALL UCOPY( P(1,NST+N+4), RW(NPZE4V+21), 4 )
C                                      AKOPLANARITY
         CALL UCOPY( P(1,NST+N+5), RW(NPZE4V+25), 4 )
         IF( IEV.LT.IEVLIM) CALL BPRS( 'ZE4V', IGEN )
         IF( IEV.LT.IEVLIM) CALL PWRT( NST+N+1, NST+N+5, 10000 )
C                                           RESTORE BLANK COMMON
 2150 CONTINUE
      IF( LVECS.NE.0 ) GO TO 2200
         CALL UCOPY( RW(NPBUF+1), P(1,NST+1), 10*(N+5) )
         CALL BDLS( '--BU', 0 )
 2200 CONTINUE
C
      GO TO 8000
C
C
C
C                                           BANK ZE4V
      ENTRY ZE4VST( NST, N, IGEN )
C-----------------------------------------------------------
C NEW VERSION  17-11-86    M.ZIMMER
C ONLY A VERY ELEMENTARY FORM OF ZE4V BANK IS FILLED
C ALL PARTICLES GET THE SAME NUMBER OF WORDS ( LT ) AND NO
C ADDITIONAL INFORMATION IS STORED
C STORE MOMENTA FROM VECSUB COMMON INTO ZE4V
C THE PARTICLES ARE STORED IN
C NST+1..NST+N, WHERE N IS INPUT
C-----------------------------------------------------------
C
      IF( N.LE.0 ) GO TO 5000
C                                           EBEAM
         EBE = EBGENL( DUMMY )
C
         CALL BCRE( NPZE4V, 'ZE4V', IGEN, LH+LT*N, &8100, IER )
         CALL BSAW( 1, 'ZE4V' )
         HW(NPZE4V*2+ 1) = LH
         HW(NPZE4V*2+ 2) = 0
         HW(NPZE4V*2+ 3) = 0
         HW(NPZE4V*2+ 4) = 0
         HW(NPZE4V*2+ 5) = LT
         HW(NPZE4V*2+ 6) = N
         HW(NPZE4V*2+ 7) = 0
         HW(NPZE4V*2+ 8) = 0
         HW(NPZE4V*2+ 9) = 0
         HW(NPZE4V*2+10) = 0
         HW(NPZE4V*2+11) = 0
         HW(NPZE4V*2+12) = 0
C                                      RUN/EVENT #
      IW(NPZE4V  + 7) = IRNEVT( DUMMY )
      RW(NPZE4V  + 8) = EBE
C
      CALL SPTHAK( NST+1, NST+N, NST+N+4, THR,
     &                           NST+N+1, SPH,
     &                           NST+N+5, AKO, IER )
C                                      SPHERICITY
      CALL UCOPY( P(1,NST+N+1), RW(NPZE4V+ 9), 4 )
      CALL UCOPY( P(1,NST+N+2), RW(NPZE4V+13), 4 )
      CALL UCOPY( P(1,NST+N+3), RW(NPZE4V+17), 4 )
C                                      THRUST
      CALL UCOPY( P(1,NST+N+4), RW(NPZE4V+21), 4 )
C                                      AKOPLANARITY
      CALL UCOPY( P(1,NST+N+5), RW(NPZE4V+25), 4 )
C
         NP = NPZE4V + LH
         N1 = NST + 1
         NN = NST + N
         DO 3000 J=N1,NN
            RW(NP  + 1) = P(1,J)/P(6,J)
            RW(NP  + 2) = P(2,J)/P(6,J)
            RW(NP  + 3) = P(3,J)/P(6,J)
            HW(NP*2+ 7) = HP(17,J)
            HW(NP*2+ 8) = 0
            HW(NP*2+ 9) = 0
            HW(NP*2+10) = 0
            RW(NP  + 6) = P(6,J)
            RW(NP  + 7) = P(7,J)
            HW(NP*2+15) = HP(19,J)
            HW(NP*2+16) = HP(20,J)
            HW(NP*2+17) = 0
            HW(NP*2+18) = -1
C
            NP = NP + LT
 3000    CONTINUE
C
 5000 CONTINUE
      GO TO 8000
C
C
      ENTRY ZE4VDP
C-----------------------------------------------------------
C  DUMP BANK IN READABLE FORMAT
C-----------------------------------------------------------
      NPZE4V = IW(IBLN('ZE4V'))
      IF( NPZE4V.LE.0 ) GO TO 6000
C
      NZ2 = NPZE4V*2
      LLH = HW( NZ2 + 1 ) - HW( NZ2 + 3 )*HW( NZ2 + 2 )
     &                       - HW( NZ2 + 4)
      WRITE(6,9103) IW(NPZE4V-2), IW(NPZE4V),(HW(NZ2+I),I=1,14),
     *              RW(NPZE4V+8),(RW( NPZE4V+ LLH + I ),I=1,3)
 9103 FORMAT(//' DUMP OF ZE4V  ',I4,' LENGTH=',I5,5X,/,
     *1X,' LHD  LVX  NVX  LMC   LT   NP  LCH  NCH  LNE  NNE  LRE  NRE',
     *'   # RUN   # EVT   E-BEAM   X-VX1   Y-VX1   Z-VX1',/,
     * 1X,12(I4,1X),1X,I6,2X,I6,2X,F7.3,3(1X,F7.2)/)
      J = NPZE4V+9
      K = NPZE4V+28
      WRITE(6,9104) (RW(I),I=J,K)
 9104 FORMAT( 5X,3F10.3,2X,F10.3/
     *        5X,3F10.3,2X,F10.3,'  SPHERICITY'/
     *        5X,3F10.3,2X,F10.3/
     *        5X,3F10.3,2X,F10.3,'  THRUST'/
     *        5X,3F10.3,2X,F10.3,'  AKOPLANARITY')
C
      N = HW( NZ2 + 6 )
      LTR = HW( NZ2 + 5 )
      LTRCH = HW( NZ2 + 7 )
      LTRNE = HW( NZ2 + 9 )
      LTRRE = HW( NZ2 + 11)
      J = 0
      NP = NPZE4V + HW(NZ2+1)
      IF( N.LE.0 ) GO TO 5100
      WRITE(6,9105)
 9105 FORMAT(1X,'NR    EX    EY    EZ   PTOT  Q',
     *       ' PH TY VO VS PL OR PT',
     *       '   ECL S_ECL M_ECL  DET CL12 PA RZRF',
     *       ' X-TRK Y-TRK Z-TRK  DEDX SDEDX  R-MIN')
 5100 IF( J.GE.N ) GO TO 6000
      J = J + 1
      NP2 = NP*2
         IF( HW(NP*2 +18 ) .EQ. 1 )  THEN
      WRITE(6,9108)J,RW(NP+1),RW(NP+2),RW(NP+3),RW(NP+6),INT(RW(NP+7)),
     *                    HW(NP2+7),HW(NP2+8),HW(NP2+9),HW(NP2+10),
     *                    HW(NP2+15),HW(NP2+16),HW(NP2+18),
     &                    (RW(NP+LTR+I),I=1,3),(HW(NP2+LTR*2+I),I=7,10),
     &                    (RW(NP+LTR+I),I=6,11)
 9108       FORMAT(1X,I2,1X,3(F5.2,1X),F6.3,1X,I2,1X,6(I2,1X),I2,
     &            3(1X,F5.2),2(1X,I4),1X,I2,1X,I4,3(1X,F5.1),
     &            2(1X,F5.2),1X,F6.2)
            NP = NP + LTR + LTRCH
C
          ELSEIF ( HW(NP*2 + 18) .EQ. 0 ) THEN
C
      WRITE(6,9109)J,RW(NP+1),RW(NP+2),RW(NP+3),RW(NP+6),INT(RW(NP+7)),
     *                    HW(NP2+7),HW(NP2+8),HW(NP2+9),HW(NP2+10),
     *                    HW(NP2+15),HW(NP2+16),HW(NP2+18),
     &                    (RW(NP+LTR+I),I=1,3),(HW(NP2+LTR*2+I),I=7,8)
 9109       FORMAT(1X,I2,1X,3(F5.2,1X),F6.3,1X,I2,1X,6(I2,1X),I2,
     &                 3(1X,F5.2),2(1X,I4))
            NP = NP + LTR + LTRNE
C
          ELSEIF( HW(NP*2 + 18) .EQ. 2 )  THEN
C
            NP = NP + LTR + LTRRE
C
          ELSEIF( HW(NP*2 + 18) .EQ.-1 )  THEN
C
            NP = NP + LTR
C
         ENDIF
C
         GO TO 5100
 6000 CONTINUE
C
 8000 CONTINUE
      RETURN
C
 8020 CALL FEHLER (35, &8000)
      RETURN
C
 8100 NWARN(1) = NWARN(1) + 1
      IF( NWARN(1).LT.10 ) WRITE(6,9101) IER
 9101 FORMAT(' IER=',I4,' WHEN CREATING ZE4V')
      RETURN
C
 8200 NWARN(2) = NWARN(2) + 1
      IF( NWARN(2).LT.10 ) WRITE(6,9102) IER
 9102 FORMAT(' IER=',I4,' WHEN CREATING --BU')
C
      RETURN
      END
      SUBROUTINE EA3RUN( NST, N, IMODE )
C-----------------------------------------------------------
C UNPACK DOCA POINTS FROM ZE4V INTO VECSUB COMMON
C IMODE = 0 ALL PARTICLES
C       = 3 ALL NEUTRALS
C       = 4 ALL CHARGED
C THE PARTICLES ARE STORED IN
C NST+1..NST+N, WHERE N IS SET ON EXIT
C    THE INDIVIDUAL POSITIONS ARE:
C  P(  1..3,*)   RX,RY,RZ
C-----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / BCS / IW(1)
      DIMENSION RW(1), HW(1)
      EQUIVALENCE (HW(1),RW(1),IW(1))
C
      COMMON P(10,400)
      DIMENSION IP(10,100),HP(20,1)
      EQUIVALENCE (P(1,1),IP(1,1),HP(1,1))
C
C
      N = 0
      NPZE4V = IW(IBLN('ZE4V'))
      IF( NPZE4V.LE.0 ) GO TO 7000
         NT = HW(NPZE4V*2 + 3 )
         NP = NPZE4V + HW(NPZE4V*2+1)
         DO 6200 J=1,NT
            CHARGE = RW(NP+7)
            IF( CHARGE.EQ.0. .AND. IMODE.EQ.4 ) GO TO 6100
            IF( CHARGE.NE.0. .AND. IMODE.EQ.3 ) GO TO 6100
               N = N + 1
               CALL UCOPY( RW(NP+8), P(1,NST+N), 3 )
 6100       CONTINUE
            NP = NP + HW(NPZE4V*2+2)
 6200    CONTINUE
 7000 CONTINUE
      RETURN
      ENTRY EA3RVX( NAST )
C-----------------------------------------------------------
C UNPACK EVENT VERTEX FROM ZE4V INTO VECSUB COMMON
C STORED IN NAST
C-----------------------------------------------------------
C
C
      NA = 0
      NPZE4V = IW(IBLN('ZE4V'))
      IF( NPZE4V.LE.0 ) GO TO 8000
         CALL UCOPY( RW(NPZE4V+ 5), P(1,NAST), 3 )
 8000 CONTINUE
      RETURN
      END
