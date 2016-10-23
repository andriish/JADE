C   14/02/87 805191458  MEMBER NAME  ZE4VPKO1 (S)           FORTRAN77
      SUBROUTINE ZE4VPK( IGEN )
C-----------------------------------------------------------
C VERSION OF 21/11/86       M.ZIMMER
C MODIFIED   23/01/87       M.ZIMMER
C MODIFIED   14/02/87       G.ECKERLIN  (ZE4INI INCLUDED)
C MODIFIED   23/02/87       G.ECKERLIN / M.ZIMMER
C MODIFIED   02/04/87       G.ECKERLIN  (PARTONS INCLUDED FOR MC)
C LAST MOD   02/04/87       G.ECKERLIN  (NO CALL TO ZE4VEK FOR MC)
C LAST MOD   09/07/87       M ZIMMER    (NEW TRACEBACK IMPLEMENTED)
C LAST MOD   21/07/87       M ZIMMER    (EXTERNAL REFERENCES ADDED)
C LAST MOD   27/08/87       E ELSEN     (ITYP HANDLING, DEDX MC)
C LAST MOD   28/08/87       E ELSEN     (ZE4VMC CALL, EAMCDE CALL)
C LAST MOD   25/11/87       E ELSEN     (EAMCDE ONLY IF NDEDX GE 0
C LAST MOD   26/01/88       E ELSEN     (COPE WITH MISSING PATR
C LAST MOD   13/04/88       E ELSEN     (MUON CODE)
C LAST MOD   13/04/88       M ZIMMER    (BANK EXTENDED ( DATE , NDEDX )
C LAST MOD   27/04/88       E ELSEN     COMFIT RESULT STORED
C NEW VERSION FOR NEW ZE4V-BANK
C PACK INFORMATION INTO ZE4V-BANK FROM PATR AND LGCL BANKS
C IMODE = 0 UNCONNECTED LG-CLUSTERS AND PATR
C       = 1 UNCONNECTED LG-CLUSTERS
C       = 2 ALL CLUSTERS AND PATR
C       = 3 ALL CLUSTERS
C       = 4 CHARGED ONLY ( ALL MASSES PI )
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
      DIMENSION VECTB(1000)
C
      DIMENSION PACT(3), R(3), RDOCA(3)
      INTEGER NWARN(2) / 2*0 /
      DATA IEV / 0 /, IEVLIM / 0 /
C                                            FORMAT OF ZEHD BANK
      DIMENSION HRUN(2)
      EQUIVALENCE (IRUN,HRUN(1))
C                                   MC INFO IN ZEHD NOT IMPLEMENTED
      DATA LMCH / 0 /
      DATA MCFLAG / 0 /
C
      EXTERNAL VECSUB
      EXTERNAL ZE4VMC
      EXTERNAL ZE4VUN
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
C
      DATA IMODE / 2 /
C                                           FORMAT OF ZE4V BANK
CHANGED :  LTCH (12 )  31.3.88 MZ
C
      DATA  LH / 31 /, LT / 9 /, LVX / 3 /, LMC / 18 /,
     & LTCH / 14 /, LTNE / 5 /, LTRE / 5 /
C                                            FORMAT OF ZEHD BANK
      DATA HVERS/ 1 /, LZHD / 3 /
      DATA LCUT / 12 /
C
      LVECS = 0
      GO TO 1

      ENTRY ZE4VIN( IGEN )
C--------------------------------------------------------
C  ENTRY FOR FIRST EVENT
C  CREATES ZEHD BANK FIRST
C  CHECKS IF MONTE CARLO OR DATA
C--------------------------------------------------------
      NPZEHD = IW(IBLN('ZEHD'))
C                                   DELETE OLD BANK
      IF (NPZEHD.GT.0) CALL BDLS('ZEHD',IGEN)
C                                      INIT ERROR MESSAGES
      CALL FEHLTX( 1,'ERROR CREATING ZEHD-BANK')
      CALL FEHLTX( 2,'NO PATR / LGCL BANK ')
      CALL FEHLTX( 3, 'NUMBER OF CHARGED TRACKS <= 0 IN PATR')
      CALL FEHLTX( 4, 'ERROR WHILE CREATING ZIM1-BANK')
      CALL FEHLTX( 5, 'NO TRACKS IN ZE4V-BANK')
      CALL FEHLTX( 11,'NUMBER OF HITS IN R-PHI LT CUT')
      CALL FEHLTX( 12,'NUMBER OF HITS IN R-Z LT CUT')
      CALL FEHLTX( 13,'Z- CUT NOT PASSED')
      CALL FEHLTX( 14,'RMIN CUT NOT PASSED')
      CALL FEHLTX( 15,'MOMENTUM CUT NOT PASSED')
      CALL FEHLTX( 17,'NO CONNECTED LG-CLUSTER')
      CALL FEHLTX( 18,'JCLUS > 0 BUT NO LGCL-BANK')
      CALL FEHLTX( 19,'CORR. ECL < 0 (NO CL. IN LGANAL)')
C                                   CHECK IF MONTE CARLO
      IRUN = IRNEVT(DUMMY)
      IF (HRUN(1).LT.100) MCFLAG = 1
C                                   CALC LENGTH OF BANK
      LZEHD = LZHD + LCUT + LMCH
C                                   CREATE NEW BANK
      CALL BCRE(NPZEHD,'ZEHD',IGEN,LZEHD,&30,IER)
      CALL BSAW(1,'ZEHD')
C                                   FILL HEADER PART
      HW(NPZEHD*2 + 1) = HVERS
C                                   FLAG MC>1/DATA=0
      HW(NPZEHD*2 + 2) = MCFLAG
C                                   # OF WORDS FOR HEADER PART
      HW(NPZEHD*2 + 3) = LZHD
C                                   # OF WORDS FOR CUT INFO
      HW(NPZEHD*2 + 4) = LCUT
C                                   # OF WORDS FOR MC INFO
      HW(NPZEHD*2 + 5) = LMCH
C                                   NOT USED
      HW(NPZEHD*2 + 6) = 0
C                                   CUT INFO PART
      NPZECT = NPZEHD + LZHD
      RW(NPZECT + 1) = 0.
C
      RW(NPZECT + 2) = 0.
C
      HW(NPZECT*2 + 5) = NRPHCT
C
      HW(NPZECT*2 + 6) = NRZCT
C
      RW(NPZECT + 4) = RMINCT
C
      RW(NPZECT + 5) = ZCT
C
      RW(NPZECT + 6) = PCT
C
      RW(NPZECT + 7) = 0.
C
      RW(NPZECT + 8) = 0.
C
      RW(NPZECT + 9) = 0.
C
      RW(NPZECT + 10) = ELGMIN
C
      RW(NPZECT + 11) = ELGPHM
C
      RW(NPZECT + 12) = .5
C                                   MC INFO PART
C     NPZEMC = NPZECT + LCUT
C     DO 20 IC = 1, LMCH
C       RW(NPZECT + IC) = 0.       NOT IMPLEMENTED !!!!!
C20   CONTINUE
      LVECS = 0
      GOTO 1
C                                  ERROR IN BCRE
 30   CONTINUE
      LVECS = 0
      CALL FEHLER( 1, &1 )

      ENTRY ZE4VUP( NST, N, IGEN )
C--------------------------------------------------------
C  ENTRY FOR VECSUB OPTION
C  LEAVES PARTICLES IN BLANK COMMON
C--------------------------------------------------------
      LVECS = 1
    1 CONTINUE
      IEV = IEV + 1
C
C
C                                      LOOK FOR BANKS
CC    NPZE4V = IW( IBLN('ZE4V'))
CC    IF( NPZE4V .GT. 0 ) CALL BDLS( 'ZE4V',IGEN)
      CALL BDLS( 'ZE4V',IGEN)
C                                      PATR - NBANK
      NPPATR = IW( IBLN('PATR'))
      IF( NPPATR .LE. 0 .AND. IMODE .EQ. 4 ) CALL FEHLER( 2, &8000 )
      NPLGCL = IW( IBLN('LGCL'))
      IF( NPPATR .LE. 0 .AND. NPLGCL .LE. 0 ) CALL FEHLER (2,&8000 )
      IF( NPLGCL .LE. 0 .AND. IMODE .EQ. 1
     &.OR.NPLGCL .LE. 0 .AND. IMODE .EQ. 3 ) CALL FEHLER( 2, &8000 )
C                                      IF NOT MC LMC = 0
      IF (MCFLAG.EQ.0) LMC = 0
C
      NVX = 1
      LHEAD = LH + NVX*LVX + LMC
C                                           CREATE ZE4V HEAD
      CALL BCRE( NPZE4V, 'ZE4V', IGEN, LHEAD, &8100, IER )
      CALL BSAW( 1, 'ZE4V' )
      HW(NPZE4V*2+ 1) = LHEAD
C                                      VERTEX PART
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
C                                           EBEAM IN GEV
      EBE = .001*EBEAM( HW(NPZE4V*2+13 ) )
      RW(NPZE4V  + 8) = EBE
C
CHANGED 31.3.88 MZ ( DATE ADDED )
C
      NPHEAD = IW( IBLN( 'HEAD' ))
      IF ( NPHEAD .GT. 0 ) THEN
        NPH2 = 2 * NPHEAD
        HW( 2*NPZE4V + 57 ) = HW( NPH2 + 6 )*100 + HW( NPH2 + 7 )
        HW( 2*NPZE4V + 58 ) = HW( NPH2 + 8 )
      ELSE
        IW( NPZE4V + 29 ) = 0
      ENDIF
C---END MOD
C                                      NOT USED
      RW(NPZE4V  +30) = 0.
C
C                                      ACCEPT EVENT BY MCREDU
      MRFLAG = -1
      CALL zMCREDU( 0., &2040 )! PMF 11/04/00 MCREDU->zMCREDU
C                                      EVENT ACCEPTED
      MRFLAG = 1
 2040 CONTINUE
      IW( NPZE4V + 31 ) = MRFLAG
      NTRVX = 0
      CALL ZE4VVX( R, NTRVX )
C                                           SET VERTEX HERE
      DO 20 I=1,3
         RW(NPZE4V  + LH + I) = R(I)
   20 CONTINUE
C
C$$$$$$---------------------------------    MC PART OF HEADER
C                                           FILLED FROM VECT BANK

      IF(MCFLAG.EQ.0) GOTO 4213
C
      NPVECT = IW(IBLN('VECT'))
      IF(NPVECT.GT.0) GO TO 4119
       WRITE(6,9212) HW(NPZE4V*2+14),HW(NPZE4V*2+13)
9212   FORMAT(' NO VECT BANK IN EVENT ',I8,'  RUN ',I8)
       GOTO 4213
4119  CONTINUE

C                                        START VECT-ANALYSIS
C
C                                        POINTER TO MC PART IN ZE4V
      NPMC = NPZE4V + LH + NVX*LVX
C
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
      RW(NPMC + (IMC-1)*4 + 3) = VECTB(IND+2)
      RW(NPMC + (IMC-1)*4 + 4) = VECTB(IND+3)
      RW(NPMC + (IMC-1)*4 + 5) = VECTB(IND+4)
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
C$$$$$$---------------------
C
      IF( IMODE .EQ. 1 .OR. IMODE.EQ.3 ) GO TO 1000
C
C                                           PATR TRACKS
      IPPHOT = IBLN('PHOT')
      NPPHOT = IW( IPPHOT )

      IF( NPPATR .LE. 0 )      CALL FEHLER( 3, &1000 )
      NTR = IW( NPPATR + 2 )
      IF( NTR .LE. 0 )      CALL FEHLER( 3, &1000 )
C
      IP0 = IW( NPPATR + 1 ) + NPPATR
      LP = IW( NPPATR + 3 )
      IP9 = IP0 + ( NTR - 1) * LP
C                                      LOCATE HWDS
      CALL CLOC( NPHWDS, 'HWDS', IW(NPPATR-2) )
C                                           LOOP OVER TRACKS
      DO 300 J = IP0, IP9, LP
        NUMTRK = IW( J + 1 )
        NRPHI = IW(J+24)
        IF( NRPHI .LT. NRPHCT ) CALL FEHLER ( 11, &300 )
        NRZ = IW(J+33)
        IF( NRZ .LT. NRZCT )  CALL FEHLER ( 12, &300 )
        AZV = ABS( RW(J+31) )
        IF( AZV .GT. ZCT )         CALL FEHLER ( 13, &300 )
        CALL PRTOCI( J, CAP, RMIN, PHIM, SIG )
        IF( ABS( RMIN ) .GT. RMINCT )    CALL FEHLER ( 14, &300 )
C                                      SET REFERNCE POINT FOR CRDOCA
        CALL CRDOCA( J, R, PACT, RDOCA, CHARGE )
        PTOT = SQRT( PACT(1)**2 + PACT(2)**2 + PACT(3)**2 )
        IF( PTOT .LT. PCT )      CALL FEHLER ( 15, &300 )
C                                      ACCEPT THIS TRACK

C                                      FILL GLOBAL PART FOR EACH TRACK
C                                      EXTEND BANK
        CALL BCHM( NPZE4V, LT+LTCH, IER )
C                                      POINTER TO NEXT ZE4V-WORD
        NP = NPZE4V + LHEAD + HW( NPZE4V*2 + 6) * LT
     &                      + HW( NPZE4V*2 + 8) * LTCH
     &                      + HW( NPZE4V*2 +10) * LTNE
     &                      + HW( NPZE4V*2 +12) * LTRE
        NP2 = NP * 2
C                                      SUM UP # OF TOTAL AND CH. TRKS
        HW(NPZE4V*2+8 ) = HW(NPZE4V*2+8) + 1
        HW(NPZE4V*2+6 ) = HW(NPZE4V*2+6) + 1
C                                      EX,EY,EZ
        DO 1200 I = 1,3
          RW( NP + I ) = PACT(I)/PTOT
 1200   CONTINUE
C                                      # OF VERTEX OF PARTICLE ORIGIN
        HW( NP2 + 9) = 1
C                                      # OF SEC VERTEX
        HW( NP2 +10) = 0
C                                      MOMENTUM
        FAC = 1.
        IF( PTOT .GT. EBE ) FAC = .5*EBE /PTOT
        RW(NP + 6) = PTOT*FAC
        RW(NP + 7) =  CHARGE
C                                      LG-INFORMATION
        JCLUS1 = IW( J + 40 )
        NPLGCL = IW(IBLN('LGCL'))
        SIGECL = 0.
        ECL    = 0.
        JBAREC = -1000
        IF ( NPLGCL .GT. 0 .AND. JCLUS1 .GT. 0 ) THEN
          IPENDE = IW( NPLGCL + 4 )
          IP3 = IW( NPLGCL + 3 )
          NWPCL = IW( NPLGCL + 25 )
          NPOI = NPLGCL + IP3 + (JCLUS1-1) * NWPCL -1
          ECL    = RW( NPOI + 2 )
          SIGECL = RW( NPOI + 3 )
          JBAREC = IW( NPOI + 1 )
        ELSEIF( NPLGCL .GT. 0 .AND. JCLUS1 .LE. 0 ) THEN
          CALL FEHLER( 17, &3123)
        ELSEIF( NPLGCL .LE. 0 .AND. JCLUS1 .GT. 0 ) THEN
          CALL FEHLER( 18, &3123)
        ENDIF
 3123   CONTINUE
C
        NPART = 0
        NPHOT = IW(IPPHOT)
        DO 210 WHILE( NPHOT.GT.0 .AND. NPART.EQ.0 )
          IF(IW(NPHOT+24).EQ.IW(J+1)) NPART = IW(NPHOT+25)
          IF(IW(NPHOT+25).EQ.IW(J+1)) NPART = IW(NPHOT+24)
          NPHOT = IW(NPHOT-1)
  210   CONTINUE
C                                      PATR NUMBER OF PHOT PARTNER
  220   HW( NP2 + 7 ) = NPART
C                                           MC PART
C                                      0 OR PART# IN PALL BANK (HW(+15))
C                                      0 OR ELECTRON ORIGIN    (HW(+16))
        IW( NP + 8 ) = 0
C                                      CHECK FOR MONTE CARLO
        IF( MCFLAG.GT.0 ) THEN
C                                      NEW ENTRY IN ZE4VMC
          CALL ZE4VMC( IW(J+1), NPALL, IORI, ITYPMC, IVECT )
          HW( NP2 + 15) = NPALL
          HW( NP2 + 16) = IORI
          HW( NP2 +  8) = ITYPMC*100
        ELSE
          HW( NP2 +  8) = 0
        ENDIF
C                                      NUMBER OF CORR. TPTR BANK
        NPTPTR = IW(IBLN('TPTR'))
        DO 230 WHILE( NPTPTR.GT.0 .AND. HW(NPTPTR*2+4).NE.NUMTRK )
          NPTPTR = IW(NPTPTR-1)
  230   CONTINUE
        IF( NPTPTR .GT. 0 ) THEN
          HW(NP2 + 17 ) = IW(NPTPTR-2)
        ELSE
          HW(NP2 + 17 ) = 0
        ENDIF
C                                      ADDITIONAL CH. TRACK PART
        HW(NP2 + 18 ) = 1
C                                      CLUSTER-ENRGY IN GEV
        RW(NP + LT + 1 ) = ECL
C                                      ERROR IN CLUSTER-ENRGY
        RW(NP + LT + 2 ) = SIGECL
C                                      INIT MEIER ENERGY
        RW(NP + LT + 3 ) = 0.
C                                      DETECTOR FLAG
        JDETEC = 1000
        IF ( JBAREC .EQ. 0 ) JDETEC = JDETEC + 100
        IF ( IABS(JBAREC) .EQ. 1 ) JDETEC = JDETEC + 1
        HW( NP2 + LT*2 + 7) = JDETEC
C                                      #OF 1. AND 2. CONN. CLUSTER
        HW( NP2 + LT*2 + 8) = JCLUS1
C                                      # IN PATR-BANK
        HW( NP2 + LT*2 + 9) = NUMTRK
C                                      NRPHI + 100*NRZ
        HW( NP2 + LT*2 +10) = NRPHI + 100*NRZ
C                                      (X,Y,Z) TRACK AT DOCA
        RW( NP + LT + 6) = RDOCA( 1 )
        RW( NP + LT + 7) = RDOCA( 2 )
        RW( NP + LT + 8) = RDOCA( 3 )
C
        DEDX = 0.
        SIDEDX = 0.
        MUONQU = 0
        HBETA = 0.
        IF( NPTPTR .GT. 0 ) THEN
C                                      DEDX
          NDEDX = IW( NPTPTR + 72 )
          DEDX = RW( NPTPTR + 73 )
          SIDEDX = RW( NPTPTR + 74 )
C                                      MUON QUALITY FLAG OR 0
          MUONQU = HW( NPTPTR*2 + 96 )
CHANGED  13.4.88
          IF ( MUONQU .EQ. 9 )
     *      HW( NP2 + 8 ) = HW( NP2 + 8 )/100*100 + 3
C                                      TOF RESULTS (BETA)
          IF ( ABS( IW( NPTPTR + 58 )) .EQ. 1 ) THEN
            HBETA = INT( RW( NPTPTR+61 ) * 1000.) * IW( NPTPTR + 58 )
          ENDIF
        ENDIF
C                                      IF NOT FILLED, GENERATE
C                                      FOR MC
        IF( MCFLAG.GT.0 .AND.
     *      NDEDX.GE.0 .AND.  DEDX.EQ.0. .AND. SIDEDX.EQ.0. ) THEN
          CALL EAMCDE( NP, IVECT, DEDX, SIDEDX, NDEDX )
        ENDIF
C
        RW( NP + LT + 9 ) = DEDX
        RW( NP + LT +10 ) = SIDEDX
        RW( NP + LT +11 ) = RMIN
        IW( NP + LT +12 ) = MUONQU
CHANGED 31.3.88 MZ  ( 2HW ADDED )
        HW( NP2 + 2*LT + 25 ) = NDEDX
C
C                                      VTXC RESULTS IF AVAILABLE
        IF( NPHWDS.GT.0 ) THEN
          JHWDS = (J-IP0)/LP*IW(NPHWDS+3) + IW(NPHWDS+1) + NPHWDS
C                                      VTXC HITS
          HW( NP2 + 2*LT + 27 ) = IW( JHWDS + 59 )
          IF( IW(JHWDS+62) .GT. -2 ) THEN
            HW( NP2 + 2*LT + 26 ) = 2
          ELSE
            HW( NP2 + 2*LT + 26 ) = 1
          ENDIF
        ELSE
          HW( NP2 + 2*LT + 26 ) = 0
          HW( NP2 + 2*LT + 27 ) = 0
        ENDIF
        HW( NP2 + 2*LT + 28 ) = HBETA
C
  300 CONTINUE
C                                      SAVE PATR BANK SINCE LG ANALYSIS
C                                      OVERWRITES CLUSTER INFORMATION
      IPPATR = IBLN('PATR')
      NPPATR = IW(IPPATR)
      IF( NPPATR.GT.0 ) THEN
        LPATR = IW(NPPATR)
        CALL BCRE( NPBUF1, '--BU', 1 , LPATR, &8200, IER )
        IF( IER.EQ.0 ) THEN
          CALL UCOPY( RW(NPPATR+1), RW(NPBUF1+1), LPATR )
C                                      PERFORM ELECTRON ANALYSIS
C                                      ON WHOLE EVENT
C                                      WILL SET PATRICLE TYPE AND E-MEIEr
          CALL ZE4VEA
C                                      RESTORE PATR
          NPPATR = IW(IPPATR)
          CALL CLOC( NPBUF1, '--BU', 1 )
          CALL UCOPY( RW(NPBUF1+1), RW(NPPATR+1), LPATR )
          NPZE4V = IW(IBLN('ZE4V'))
        ENDIF
        CALL BDLS( '--BU', 1 )
      ENDIF
C
C
C------------------------------------- NEUTRAL PRIMARY
C
C                                           LGCL BANKS
 1000 IF( IMODE .EQ. 4 ) GO TO 2050
C
      NPLGCL = IW(IBLN('LGCL'))
      IF( NPLGCL .EQ. 0 ) GO TO 2050
C
      NCLST = IW( NPLGCL + 7 )
      IF( NCLST .LE. 0 ) GO TO 2050
C                                           LOOP OF CLUSTERS
C                                           ALL CLUSTERS GET
C                                           CHARGE 0
      IP2 = IW( NPLGCL + 2 )
      IP3 = IW( NPLGCL + 3 )
      NWPCL = IW( NPLGCL + 25 )
      IP0 = NPLGCL + IP3 - 1
      IP9 = IP0 + (NCLST-1) * NWPCL
      JCL = 0
      DO 2000 J=IP0,IP9,NWPCL
        JCL = JCL + 1
C                                           CHECK IF CONNNECTED
        ICONN = IW( J + 8 )
        IF( ICONN.NE.0 .AND. IMODE.LT.2 ) GO TO 2000
        ECL  = RW( J + 2 )
        ETOT = ECL
        SIGECL = RW( J + 3 )
        JBAREC = IW( J + 1 )
C                                      # OF LG-BLOCKS IN  CLUSTER
C                                      MINUS NUMBER OF TRACKS CONN.
        NPCLMP = NPLGCL + IP2 + JCL - 1
        NBLOCK = HW( NPCLMP*2 ) - HW(NPCLMP*2 - 1) + 1 -ICONN/100
        IF (JBAREC .EQ. 0 ) JBAREC = 100
C                                           PATH LENGTH CORREC
        DZDS = 1.
        IF( IW(J+1).EQ.0 ) DZDS = 1./SQRT(1-RW(J+11)**2)
        IF( ICONN .NE. 0 ) ETOT = ETOT - ELGMIN*(ICONN/100)*DZDS
        IF( ETOT .LT. ELGPHM ) GO TO 2000
C
C                                           ACCEPT THIS TRACK
        CALL BCHM( NPZE4V, LT+LTNE, IER )
C                                      POINTER TO NEXT ZE4V-WORD
        NP = NPZE4V + LHEAD + HW( NPZE4V*2 + 6) * LT
     &                      + HW( NPZE4V*2 + 8) * LTCH
     &                      + HW( NPZE4V*2 +10) * LTNE
     &                      + HW( NPZE4V*2 +12) * LTRE
        NP2 = NP * 2
C                                      SUM UP # OF TOTAL AND NE. TRKS
        HW(NPZE4V*2+10) = HW(NPZE4V*2+10) + 1
        HW(NPZE4V*2+6 ) = HW(NPZE4V*2+6 ) + 1
C                                      EX,EY,EZ
        DO 1211 I = 1,3
           RW(NP+I) = RW(J+8+I)
 1211   CONTINUE
C                                      0 OR PATR # OF PHOT PARTNER
        HW( NP2 + 7) = 0
C                                      PART TYPE
        ITYP = 1
        HW( NP2 + 8) = ITYP
C                                      1./2. VERTEX
        HW( NP2 + 9) = 1
        HW( NP2 +10) = 0
C                                      PTOT
        RW(NP  + 6) = ETOT
C                                      CHARGE
        RW(NP  + 7) = 0.
C                                      0 OR PART# IN PALL BANK
        HW( NP2 + 15) = 0
C                                      0 OR ELECTRON ORIGIN
        HW( NP2 + 16) = 0
C                                      TPTR NUMBER
        HW( NP2 + 17) = 0
C                                      0 FOR NEUTRAL PARTICLES
        HW( NP2 + 18) = 0
C                                      CLUSTER-ENERGY IN GEV
        RW( NP + LT + 1 ) = ECL
C                                      ERROR IN CLUSTER-ENERGY
        RW( NP + LT + 2 ) = SIGECL
C                                      CORRECTED CLUSTER ENERGY(NOT YET)
        RW( NP + LT + 3 ) = 0.
C                                      LG DETECTOR PART
        HW( NP2 + LT*2 + 7) = JBAREC
C                                      #OF 1. AND 2. CONN. CLUSTER
        HW( NP2 + LT*2 + 8) = ( J - IP0 ) / NWPCL + 1
C                                      # OF (BLOCKS - CH TRACKS) (1.CL)
        HW( NP2 + LT*2 + 9) = NBLOCK
C                                      # OF (BLOCKS - CH TRACKS) (2.CL)
        HW( NP2 + LT*2 +10) = 0
 2000 CONTINUE
C
C
 2050 CONTINUE
C
      IF( LVECS.NE.0 ) GO TO 2100
         N = HW( NPZE4V*2 + 6 )
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
         CALL zSPTHAK( NST+1, NST+N, NST+N+4, THR, ! PMF 11/04/00 SPTHAK->zSPTHAK
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
C NST+1..NST+N, WHERE N IS SET ON EXIT
C-----------------------------------------------------------
C
      IF( N.LE.0 ) GO TO 5000
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
      RW(NPZE4V  + 8) = .001*EBEAM( HW(NPZE4V*2+13 ) )
C
      CALL zSPTHAK( NST+1, NST+N, NST+N+4, THR, ! PMF 11/04/00 SPTHAK->zSPTHAK
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
            HW(NP*2+ 7) = HP(18,J)
            HW(NP*2+ 8) = HP(16,J)
            HW(NP*2+ 9) = 0
            HW(NP*2+10) = 0
            RW(NP  + 6) = P(6,J)
            RW(NP  + 7) = P(7,J)
            HW(NP*2+15) = HP(19,J)
            HW(NP*2+16) = HP(20,J)
            HW(NP*2 + 17) = 0
            HW(NP*2 + 18) = -1
C
            NP = NP + LT
 3000    CONTINUE
C
 5000 CONTINUE
      GO TO 8000
C
C
 8000 CONTINUE
      NPZE4V = IW( IBLN('ZE4V'))
      IF ( HW(NPZE4V*2+6).LE.0) THEN
        CALL FEHLER( 5, &8001 )
 8001   CALL BDLS( 'ZE4V',IGEN)
      ENDIF
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
