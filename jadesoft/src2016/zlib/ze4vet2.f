C   19/08/87 805090925  MEMBER NAME  ZE4VET2  (S)           FORTRAN77
      SUBROUTINE ZE4VET( NPZE4V, NPKTRK, JCAND, ESHM )
C-----------------------------------------------------------
C  VERSION OF 18/08/87                          E ELSEN
C  PERFORM ELECTRON ANALYSIS FOR SINGLE TRACK SPECIFIED
C  BY NPKTRK IN ZE4V
C  INPUT:
C     NPZE4V  BOS POINTER TO ZE4V BANK
C     NPKTRK  ABSOLUTE POINTER TO TRACK IN ZE4V BANK
C  OUTPUT:
C     JCAND = 0  IF ELECTRON CANDIDATE
C           =    FIRST NONZERO EKAND RETURN CODE
C                NOT ALL CONDITIONS MAY HAVE BEEN CHECKED
C     ESHM  = CORRECTED LG SHOWER ENERGY (MEIER)
C             ONLY FILLED FOR JCAND = 0
C
C  RECORD OF CHANGES
C  22/04/88  NDEDX NOW FROM ZE4V
C            FULL TRACK RANGE CHECKED          E E
C-----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C
      INTEGER ILIST(21)/68,  71, 100, 248, 254, 332, 360, 366, 449, 456,
     *                 548, 555, 569, 576, 615, 740, 744, 871, 906, 908,
     *                 946 /, ICUR / 1 /
C
      COMMON / BCS / IW(1)
      DIMENSION HW(1),RW(1)
      EQUIVALENCE (HW(1),IW(1),RW(1))
      COMMON /LINK/   IBLCHK,IREG,NBLK,NBLE,XI,YI,ZI,XF,YF,ZF,XSTART,
     *                YSTART,ZSTART,PSTART,TRKL(2,50),TRITER,EBITER,
     *                PMAG,NNEW,NENEW,NLIST(40),ENEW,ICHARG(40,20),
     +                NBLO,MEICL(50),NEICL(50),EBIT1,NBN1,EBLO1,NBL1
      DIMENSION IRKL(2,50)
      EQUIVALENCE(IRKL(1,1),TRKL(1,1))
      COMMON / CONNEC / ICON,SUMPI,CONC(3,10)
      COMMON / CONNEP / ICOP,CONP(5,30),SUPH(5)
C                                           SHORT FORM ONLY
      COMMON / CERECU / ICUT,
C  CUT VALUES
     + ISCUT,ICHCUT,COSCUT,PCUT,NDCUT,RSCUT,IDVERS,DLOCUT,DHICUT,DFLCUT,
     + DFHCUT,SUMCUT,ENECUT,PCHCUT,ESHCUT,JEVERS,EPCUT,EPHCUT,AMICUT
C
      REAL*4 RVTX(3), PACT(3), RDOCA(3)
      INTEGER ONE / 1 /
      LOGICAL LDEDX
      INTEGER LEVEL1 / 0 /, LEVEL2 / 0 /, LEVEL3 / 0 /
      LOGICAL FIRST / .TRUE. /
      EXTERNAL EKAND
C
      IF( FIRST ) THEN
        FIRST = .FALSE.
        IPPATR = IBLN('PATR')
        CALL LGINIT
        CALL FJHBIT( ICUT )
C                                           STATE ALL CUTS HERE AS
C                                           THEY ARE APPLIED AT THE
C                                           DIFFERENT LEVELS
C                                           CUTS BEFORE LEVEL 1
        LEVEL1 = LOR(LEVEL1,iSHFTL(ONE, 2))
        LEVEL1 = LOR(LEVEL1,iSHFTL(ONE, 3))
        LEVEL1 = LOR(LEVEL1,iSHFTL(ONE, 4))
        LEVEL1 = LOR(LEVEL1,iSHFTL(ONE, 5))
        LEVEL1 = LOR(LEVEL1,iSHFTL(ONE, 6))
        LEVEL1 = LOR(LEVEL1,iSHFTL(ONE, 7))
        LEVEL1 = LOR(LEVEL1,iSHFTL(ONE,12))
C                                           CUTS BEFORE LEVEL 2
        LEVEL2 = LOR(LEVEL2,iSHFTL(ONE,11))
        LEVEL2 = LOR(LEVEL2,iSHFTL(ONE,14))
C                                           CUTS BEFORE LEVEL 3
        LEVEL3 = LOR(LEVEL3,iSHFTL(ONE, 8))
        LEVEL3 = LOR(LEVEL3,iSHFTL(ONE, 9))
        LEVEL3 = LOR(LEVEL3,iSHFTL(ONE,19))
        LEVEL3 = LOR(LEVEL3,iSHFTL(ONE,20))
C
        LEVEL1 = LAND(ICUT,LCOMPL(LEVEL1))
        LEVEL2 = LAND(LEVEL1,LCOMPL(LEVEL2))
        LEVEL3 = LAND(LEVEL2,LCOMPL(LEVEL3))
      ENDIF
C
C
      JCAND = 0
C                         CUTS BASED ON ZE4V
      IF( NPZE4V.GT.0 ) THEN
        ITPF = HW(NPKTRK*2+18)
        IF( ITPF .EQ. 1 ) THEN
          LT = HW(NPZE4V*2 + 5)


          NUMEVT = HW(NPZE4V*2 + 14)


          PTOT = RW(NPKTRK+6)
          ABSCOS = ABS(RW(NPKTRK+3))
          DEDX = RW(NPKTRK+LT+9)
          SDEDX = RW(NPKTRK+LT+10)
          NDEDX = HW((NPKTRK+LT)*2+25)
          RSDEDX = SDEDX/AMAX1(0.001,DEDX)
          IF( NDEDX .GT. 0 ) THEN
            IF( IDVERS .EQ. 1 ) THEN
              LDEDX = DEDX.LT.10.-DLOCUT*SDEDX.OR.
     *              DEDX.GT.10.+DHICUT*SDEDX
            ELSEIF( IDVERS .EQ. 2 ) THEN
              LDEDX = DEDX.LT.DFLCUT.OR.DEDX.GT.DFHCUT
            ELSE
              LDEDX = .FALSE.
            ENDIF
          ELSE
            LDEDX = .FALSE.
          ENDIF
          IPAR = HW(NPKTRK*2+7)
          JTRK = HW((NPKTRK+LT)*2+9)
          DO 30 I=1,3
   30     RVTX(I) = RW(NPZE4V+31+I)
C
          IF( ABSCOS.GT.COSCUT )       JCAND = LOR(JCAND,iSHFTL(ONE, 2))
          IF( PTOT.LT.PCUT )           JCAND = LOR(JCAND,iSHFTL(ONE, 3))
          IF( NDEDX.LT.NDCUT )         JCAND = LOR(JCAND,iSHFTL(ONE, 5))
          IF( RSDEDX.GT.RSCUT )        JCAND = LOR(JCAND,iSHFTL(ONE, 6))
          IF( LDEDX )                  JCAND = LOR(JCAND,iSHFTL(ONE, 7))
          IF( IPAR.GT.0 )              JCAND = LOR(JCAND,iSHFTL(ONE,12))
          JCAND = LAND( ICUT, JCAND )
C
          IF( JCAND .EQ.0 .AND. LEVEL1.NE.0 ) THEN
            CALL EAZERO
            CALL EALGCL( JTRK, ESHM )
            IF( ESHM .GT. 0 ) THEN
              SHADFR = AMAX1(0.011, ENEW/EBITER )
              IF( LOG10(SHADFR).GT.ESHCUT )
     *                                 JCAND = LOR(JCAND,iSHFTL(ONE,11))
              JCAND = LAND( ICUT, JCAND )
              IF( JCAND .EQ. 0 .AND. LEVEL2.NE.0 ) THEN
                CALL EAUNIN( JTRK )
                SULPI = SUMPI/ESHM
                IF( SUMPI.GT. .3 ) THEN
                  ESHSUB = ESHM-0.08*SUMPI
                ELSE
                  ESHSUB = ESHM
                ENDIF
                IF( JEVERS .EQ. 1 ) THEN
                  VEREL = AMAX1(0.01,ESHSUB/PTOT)
                ELSE
                  VEREL = AMAX1(0.01,ESHM/PTOT)
                ENDIF
                SULH2 = SUPH(2)/ESHM
                IF( SULPI.GT.SUMCUT )  JCAND = LOR(JCAND,iSHFTL(ONE, 8))
                IF( SULH2.GT.ENECUT )  JCAND = LOR(JCAND,iSHFTL(ONE, 9))
                IF( VEREL.LT. EPCUT )  JCAND = LOR(JCAND,iSHFTL(ONE,19))
                IF( VEREL.GT. EPCUT )  JCAND = LOR(JCAND,iSHFTL(ONE,20))
                JCAND = LAND( ICUT, JCAND )
C
                NPPATR = IW(IPPATR)
                IF( JCAND.EQ.0 .AND. LEVEL3.NE.0 ) THEN
                  IF( NPPATR.GT.0 .AND.
     *                0.LT.JTRK .AND. JTRK.LE.IW(NPPATR+2) ) THEN
                    J = NPPATR + IW(NPPATR+1) + IW(NPPATR+3)*(JTRK-1)
                    CALL CRDOCA( J, RW(NPZE4V+32), PACT, RDOCA, CHARGE )
                    PTRANS = SQRT( PACT(1)**2+PACT(2)**2)
                    ZVER = RVTX(3)
                    ICLUR = 0
                    CALL CHIEN5( JTRK, PTRANS, ESHM, ZVER, CHARGE,
     *                           CHI2N, PCHI2, ICLUR )
                    IF( ICLUR.GT.0 ) THEN
                      IF( 2 .LE. ICHCUT )
     *                                 JCAND = LOR(JCAND,iSHFTL(ONE, 2))
                    ELSEIF( PCHI2.LT.PCHCUT ) THEN

                                       JCAND = LOR(JCAND,iSHFTL(ONE,10))
                    ENDIF
                  ELSE
                    JCAND =                    LOR(JCAND,iSHFTL(ONE, 4))
                  ENDIF
                ENDIF
              ENDIF
            ELSE
                                       JCAND = LOR(JCAND,iSHFTL(ONE,14))
              IF( 1 .LE. ICHCUT )      JCAND = LOR(JCAND,iSHFTL(ONE, 2))
            ENDIF
          ENDIF
        ELSE
          JCAND =                              LOR(JCAND,iSHFTL(ONE, 4))
        ENDIF
      ELSE
        JCAND =                                LOR(JCAND,iSHFTL(ONE,26))
      ENDIF
      JCAND = LAND( ICUT, JCAND )
      IF ( NUMEVT .EQ. ILIST(ICUR) ) THEN
        WRITE(6,9001) NUMEVT,JCAND,JCAND,ESHM
 9001   FORMAT(3X, 'EVENT,JCAND: ESHM',I5,2X,I20,2X,Z10,2X,F10.3)
      ELSEIF( NUMEVT .GT. ILIST(ICUR)) THEN
        ICUR = ICUR + 1
      IF ( NUMEVT .EQ. ILIST(ICUR) )
     *         WRITE(6,9001) NUMEVT,JCAND,JCAND,ESHM
      ENDIF
      END
