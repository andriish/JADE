C   12/08/87 804271519  MEMBER NAME  EAUNIN   (S)           FORTRAN77
      SUBROUTINE EAUNIN( JTRK )
C-----------------------------------------------------------
C  VERSION OF 11/08/87     LAST MOD 19/08/87    E ELSEN
C  CHECK UNIQUENESS OF CLUSTER ASSIGNMENT FOR TRACK JTRK
C  ADAPTED FORM F11HEL.NEUFS(ELSE34, ENTRY UNINES IN EVENTO)
C
C  INPUT:
C     JTRK  TRACK NUMBER
C-----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C---------------------------------------- COMMONS FOR LEAD GLASS ENERGY
      COMMON /LINK/   IBLCHK,IREG,NBLK,NBLE,XI,YI,ZI,XF,YF,ZF,XSTART,
     *                YSTART,ZSTART,PSTART,TRKL(2,50),TRITER,EBITER,
     *                PMAG,NNEW,NENEW,NLIST(40),ENEW,ICHARG(40,20),
     +                NBLO,MEICL(50),NEICL(50),EBIT1,NBN1,EBLO1,NBL1
      COMMON /LISE/   IBLCSE,NBSK,NBSE,XSI,YSI,ZSI,XSF,YSF,ZSF,XSTAST,
     *                YSTAST,ZSTAST,PSTAST,TRSE(3,50),TRISE
      DIMENSION IRSE(3,50)
      EQUIVALENCE(IRSE(1,1),TRSE(1,1))
      COMMON /CLDAT/ NUMBLO,NBLIST(40),ENLIST(40),IPACL,ECLCO,ECLUN,
     *               PIMP1,PIMP2
      COMMON / CONNEC / ICON,SUMPI,CONC(3,10)
      DIMENSION ICONC(3,10)
      EQUIVALENCE(ICONC(1,1),CONC(1,1))
      COMMON / CONNEP / ICOP,CONP(5,30),SUPH(5)
      DIMENSION ICONP(5,30)
      EQUIVALENCE(ICONP(1,1),CONP(1,1))
C-------------------------------------------- GENERAL COMMONS
C
      COMMON / CRELC / SIGP,SIG1EP,CH1EP,CHI1W,CHI2N,PCHI2,ESHM,RFIR,
     +                 CHIRP,CHIRZ,ZINT,RDIS,JKAND
C
      COMMON / BCS / IW(1)
      DIMENSION HW(1),RW(1)
      EQUIVALENCE (HW(1),IW(1),RW(1))
      DIMENSION LADD(-1:2)
      INTEGER RUN, EVENT
      LOGICAL FIRST 
      DATA FIRST / .TRUE. /
C
C
      IF( FIRST ) THEN
        FIRST = .FALSE.
        IPHEAD = IBLN('HEAD')
        IPPATR = IBLN('PATR')
        IPZE4V = IBLN('ZE4V')
        IPALGN = IBLN('ALGN')
      ENDIF
      NPHEAD = IW(IPHEAD)
      IF( NPHEAD .GT. 0 ) THEN
        RUN = HW(NPHEAD*2+10)
        EVENT = HW(NPHEAD*2+11)
      ELSE
        RUN = 0
        EVENT = 0
      ENDIF
C
      ICON=0
      ICOP=0
      SUMPI=0.0
      DO 10 I=1,5
        SUPH(1)=0.
   10 CONTINUE
C
      CALL CLOC(NPALSA,'ALSA',JTRK )
      IF( NPALSA.GT.0 ) THEN
        DELZ=ABS(ZF-ZI)
        ZHIG=ZI+DELZ
        ZLOW=ZI-DELZ
C
        NPZE4V = IW(IPZE4V)
        NPPATR = IW(IPPATR)
        IF( NPZE4V .GT. 0 .AND. NPPATR.GT.0 ) THEN
          LT = HW( NPZE4V*2 + 5 )
          LADD(-1) = LT
          LADD(0)  = HW(NPZE4V*2 +  9) + LT
          LADD(1)  = HW(NPZE4V*2 +  7) + LT
          LADD(2)  = HW(NPZE4V*2 + 11) + LT
          NT = HW(NPZE4V*2 + 6)
          NP0 = NPZE4V + HW(NPZE4V*2 + 1)
          NP = NP0
          J = 0
          NPJ = 0
          DO 20 WHILE( J.LT.NT .AND. NPJ.LE.0 )
            J = J + 1
            ITPF =  HW(NP*2+18)
            IF( ITPF .EQ. 1 ) THEN
              IF( HW((NP+LT)*2+9) .EQ. JTRK ) NPJ = NP
            ENDIF
            ITPF =  HW(NP*2+18)
            NP = NP + LADD( ITPF )
   20     CONTINUE
C                                    TRACK IN ZE4V?
          IF( NPJ .GT. 0 ) THEN
            NP = NP0
            DO 1000 J=1,NT
              ITPF =  HW(NP*2+18)
              IPTRK = HW((NP+LT)*2+9)
              IF( ITPF .EQ. 1 .AND. IPTRK .NE. JTRK ) THEN
                COSOP = RW(NP+1)*RW(NPJ+1) + RW(NP+2)*RW(NPJ+2) +
     *                RW(NP+3)*RW(NPJ+3)
                PTOT = RW(NP+6)
                IF( COSOP .GT. 0.2 .AND. PTOT .GT. 0.02 ) THEN
                  EBISE = -1.
                  IP = NPPATR + (HW((NP+LT)*2+9)-1)*IW(NPPATR+3) +
     *                 IW(NPPATR+1)
                  CALL TRKBSE( IP, NPALSA, EBISE )
C                                           ENERGY LOSS OF MIN ION. TRK
C                                           IN COIL, 8.4 CM AL EQUIV,
C                                           AT 0.0044 GEV/CM
                  ELOS=0.0044*8.4/SQRT(1.-RW(NP+3)**2)
                  PTOT=AMAX1(0.0,PTOT-ELOS)
C
                  IF(EBISE.GT.0.) THEN
                    ICON=ICON+1
                    SUMPI=SUMPI+PTOT
C
                    IF(ICON.LE.10) THEN
                      ICONC(1,ICON)=IPTRK
                      CONC(2,ICON)=PTOT
                      CONC(3,ICON)=EBISE
                    ENDIF
                  ELSE
C                         EXAMINE TRACKS NEAR IN R-PHI
                    NPALGN=IW(IPALGN)
                    CALL TRKBSE(IP, NPALGN, EBISE )
                    IF( IBLCSE.LE.0 ) THEN
C                            IS IMPACT OF TRACK ISE WITHIN Z RANGE
C                            OF TRACK IPNR
                      IF(ZSTAST.GE.ZLOW.AND.ZSTAST.LE.ZHIG) THEN
                        DAPS=ABS(PSTART-PSTAST)
                        IF(DAPS.LE.0.5) THEN
                          ICOP=ICOP+1
                          IF(ICOP.LE.30) THEN
                            ICONP(1,ICOP)=IPTRK
                            CONP(2,ICOP)=PTOT
                            CONP(3,ICOP)=ZSTAST
                            CONP(4,ICOP)=PSTAST
                            CONP(5,ICOP)=EBISE
C                                      0.0748 = BLOCK WIDTH PHI
                            INSU=DAPS/0.0748 + 1
                            DO 575 JP=MIN(INSU+1,5),5
                              SUPH(JP)=SUPH(JP)+PTOT
  575                       CONTINUE
                          ENDIF
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
C
              ENDIF
              NP = NP + LADD( ITPF )
 1000       CONTINUE
          ENDIF
        ENDIF




C
C
C
        IF(ICON.GT.10) THEN
          WRITE(6,9101) ICON, SUMPI, RUN, EVENT, JTRK
 9101     FORMAT(' +++ EAUNIN: ICON=',I3,'>10.  SUMPI=',F10.3,
     +            ' IN EVENT',2I8,' TRACK',I4)
          ICON = 10
        ENDIF
        IF(ICOP.GT.30) THEN
          WRITE(6,9102) ICOP, RUN, EVENT, JTRK
 9102     FORMAT(' +++ EAUNIN: ICOP=',I3,'>30 IN EVENT',2I8,
     *           ' TRACK',I4)
          ICOP = 30
         ENDIF
      ELSE
C
        WRITE(6,9104) JTRK, RUN, EVENT
 9104   FORMAT(' +++ EAUNIN: ALSA,',I3,' MISSING IN EVENT ',2I8 )
C
      ENDIF
      END
