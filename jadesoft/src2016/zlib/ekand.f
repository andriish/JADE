C   13/08/87 806031220  MEMBER NAME  EKAND    (S)           FORTRAN77
      SUBROUTINE EKAND(JTRK,IKAND)
C-----------------------------------------------------------
C  VERSION OF 13/08/87                          E ELSEN
C  REWRITE OF F11HEL.NEUFS(EKAND)
C
C  INPUT:
C     JTRK  TRACK NUMBER
C  OUTPUT:
C     IKAND = 0 IF ELECTRON CANDIDATE
C           = FAILURE REASON ELSE
C
C  RECORD OF CHANGES:
C  22/04/88  LOG ESHM<0 OCCURENCES
C-----------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
      COMMON / BCS / IW(1)
      DIMENSION HW(1),RW(1)
      EQUIVALENCE (HW(1),IW(1),RW(1))
C
      COMMON / CERECU / ICUT,
C  CUT VALUES
     + ISCUT,ICHCUT,COSCUT,PCUT,NDCUT,RSCUT,IDVERS,DLOCUT,DHICUT,DFLCUT,
     + DFHCUT,SUMCUT,ENECUT,PCHCUT,ESHCUT,JEVERS,EPCUT,EPHCUT,AMICUT,
C  RETURN VALUES FOR CUT VARIABLES
     + ISCAN,ICHECK,CTH,PMOM6,JREM,NDEX,RELDE,DEDX,
     + SULPI,SULH2,PCHI2,ESHLO,VEREL,IPAR,IPAMI,AMINV,
C  RETURN VALUES FOR NON CUT VARIABLES
     + SIGDE,ESHM,ESHSUB,EBITER,ENEW,CHI2N,ICON,SUMPI,ICOP,DPHIM
C
      INTEGER ONE / 1 /
C
      CALL CINIT
      IKAND=0
C
      CALL BLOC(NPLGZU,'LGZU',JTRK,*1200)
C                                 NUMBER OF TRACK IN 'ZE4V' BANK
      JREM=HW(NPLGZU*2+3)
C
C------------------------------------------------- SCAN FLAG
      ISCAN=HW(NPLGZU*2+1)
      IF(ISCAN.GE.ISCUT )   IKAND = LOR(LAND(ICUT,iSHFTL(ONE, 0)),IKAND)
C
C------------------------------------------------- ICHECK
      ICHECK=HW(NPLGZU*2+2)
      IF(ICHECK.LT.ICHCUT)  IKAND = LOR(LAND(ICUT,iSHFTL(ONE, 1)),IKAND)
C
C------------------------------------------------- COS(THETA)
      PMOM6=RW(NPLGZU+25)
      IF(PMOM6.LT.0.001) PMOM6=0.001
      CTH=RW(NPLGZU+34)/PMOM6
      CTH=ABS(CTH)
      IF(CTH.GT.COSCUT )    IKAND = LOR(LAND(ICUT,iSHFTL(ONE, 2)),IKAND)
C
C------------------------------------------------- MOMENTUM CUT
      IF(PMOM6.LT.PCUT)     IKAND = LOR(LAND(ICUT,iSHFTL(ONE, 3)),IKAND)
C
C------------------------------------------------- JTRK IN 'ZE4V' ?
      IF(JREM.LE.0 )        IKAND = LOR(LAND(ICUT,iSHFTL(ONE, 4)),IKAND)
C
C----------DE/DX--------DE/DX--------DE/DX-------- DE/DX CUTS
C
      NDEX=HW(NPLGZU*2+4)
      DEDX=RW(NPLGZU+3)
      IF(DEDX.LT.0.001) DEDX=0.001
      SIGDE=RW(NPLGZU+4)
      IF( SIGDE.GT.0. .AND. NDEX.EQ.0 ) NDEX = 1000
      IF(SIGDE.LE.0.0) SIGDE=10.0
      RELDE=SIGDE/DEDX
C
C---------------------------------------------------------- DE/DX HITS
      IF(NDEX.LT.NDCUT)     IKAND = LOR(LAND(ICUT,iSHFTL(ONE, 5)),IKAND)
C
C------------------------------------------------------ RELATIVE SIGMA
      IF(RELDE.GT.RSCUT)    IKAND = LOR(LAND(ICUT,iSHFTL(ONE, 6)),IKAND)
C
C---------------------------------------------------------- DE/DX
      IDCUT=0
      GOTO (100,110), IDVERS
C                               SIGMA CUT
  100 DELO=10.0-DLOCUT*SIGDE
      DEHI=10.0+DHICUT*SIGDE
      GOTO 120
C                               FIXED DE/DX CUT
  110 DELO=DFLCUT
      DEHI=DFHCUT
  120 IF(DEDX.LT.DELO.OR.DEDX.GT.DEHI) IDCUT=1
      IF(IDCUT.EQ.1)        IKAND = LOR(LAND(ICUT,iSHFTL(ONE, 7)),IKAND)
C
C--------LEAD GLASS--------LEAD GLASS--------LEAD GLASS------ LG CUTS
C
      ESHM=RW(NPLGZU+16)
      ESHSUB=ESHM
        ESSU=RW(NPLGZU+16)
        IF(ESSU.LT.0.001) ESSU=0.001
      EBITER=RW(NPLGZU+17)
        IF(EBITER.LT.0.001) EBITER=0.001
      ENEW=RW(NPLGZU+18)
      CHI2N=RW(NPLGZU+29)
      PCHI2=RW(NPLGZU+30)
      ICON=HW(NPLGZU*2+92)
      SUMPI=RW(NPLGZU+47)
      ICOP=HW(NPLGZU*2+95)
      DPHIM=RW(NPLGZU+49)
      SUPH2=RW(NPLGZU+51)
C
C-------------------------------------------- UNIQUENESS
      SULPI=SUMPI/ESSU
      IF(SULPI.GT.SUMCUT) GOTO 200
      IF(SUMPI.GT.0.3) GOTO 210
      CFAC=1.0-0.08*SULPI
      ESHSUB=ESHSUB*CFAC
      GOTO 210
  200 IKAND =                       LOR(LAND(ICUT,iSHFTL(ONE, 8)),IKAND)
C
C-------------------------------------------- NEAR BY LG ENERGY
  210 SULH2=SUPH2/ESSU
      IF(SULH2.GT.ENECUT)   IKAND = LOR(LAND(ICUT,iSHFTL(ONE, 9)),IKAND)
C
C------------------------------------------------------------ P(CHI**2)
      IF(PCHI2.LT.PCHCUT)   IKAND = LOR(LAND(ICUT,iSHFTL(ONE,10)),IKAND)
C
C----------------------------------------------------SHADOW ENERGY
      ESHAD=ENEW/EBITER
      ESHAD=AMAX1(0.011,ESHAD)
      ESHLO=LOG10(ESHAD)
      IF(ESHLO.GT.ESHCUT)   IKAND = LOR(LAND(ICUT,iSHFTL(ONE,11)),IKAND)
C
C-------- GAMMA CONVERSIONS -------- GAMMA CONVERSIONS --------
C
      IPAR=HW(NPLGZU*2+111)
      IF(IPAR.GE.0 )        IKAND = LOR(LAND(ICUT,iSHFTL(ONE,12)),IKAND)
C
      IPAMI=HW(NPLGZU*2+73)
      AMINV=RW(NPLGZU+38)
      IF(AMINV.LT.AMICUT )  IKAND = LOR(LAND(ICUT,iSHFTL(ONE,13)),IKAND)
C
C
C
C
C---------------------------------------------------- E/P
      IF(ESHM.LT.0.      )  IKAND = LOR(LAND(ICUT,iSHFTL(ONE,14)),IKAND)
      GOTO (220,230), JEVERS
  220 VEREL=AMAX1(0.01,ESHSUB/PMOM6)
      GOTO 240
  230 VEREL=AMAX1(0.01,ESHM/PMOM6)
  240 IF(VEREL.LT.EPCUT)    IKAND = LOR(LAND(ICUT,iSHFTL(ONE,19)),IKAND)
      IF(VEREL.GT.EPHCUT)   IKAND = LOR(LAND(ICUT,iSHFTL(ONE,20)),IKAND)
C
      GO TO 1210
C
C---------------------- NO 'LGZU' BANK
 1200 IKAND = iSHFTL(ONE,26)
 1210 CALL EALZST( IKAND )
      RETURN
      END
C=======================================================================
C
      BLOCK DATA BLZ2 ! PMF 09/12/9: add name
C
C     STANDARD SELECTION CUTS FOR ELECTRON CANDIDATES
C
      COMMON  / CERECU / ICUT,
C  CUT VALUES
     + ISCUT,ICHCUT,COSCUT,PCUT,NDCUT,RSCUT,IDVERS,DLOCUT,DHICUT,DFLCUT,
     + DFHCUT,SUMCUT,ENECUT,PCHCUT,ESHCUT,JEVERS,EPCUT,EPHCUT,AMICUT,
C  RETURN VALUES FOR CUT VARIABLES
     + ISCAN,ICHECK,CTH,PMOM6,JREM,NDEX,RELDE,DEDX,
     + SULPI,SULH2,PCHI2,ESHLO,VEREL,IPAR,IPAMI,AMINV,
C  RETURN VALUES FOR NON CUT VARIABLES
     + SIGDE,ESHM,ESHSUB,EBITER,ENEW,CHI2N,ICON,SUMPI,ICOP,DPHIM
C
C
C          STANDARD CUT CRITERIA
      DATA ICUT/ 548350 /
C
C          SCAN FLAG,                 COS(THETA),      MOMENTUM
      DATA ISCUT/ 1 /,  ICHCUT/ 3 /,  COSCUT/ 0.76 /,  PCUT/ 1.0 /
C
C          DE/DX HITS,  RELATIVE SIGMA, DE/DX CUT VERSION
      DATA NDCUT/ 15 /, RSCUT/ 0.175 /, IDVERS/ 1 /
C
C          LOWER          UPPER SIGMA CUT
      DATA DLOCUT/ 1.0 /, DHICUT/ 2.0 /
C
C          LOWER          UPPER FIXED DE/DX CUT
      DATA DFLCUT/ 9.0 /, DFHCUT/ 12.5 /
C
C          UNIQUENESS,     NEAR BY LG ENERGY
      DATA SUMCUT/ 0.60 /, ENECUT/ 0.30 /
C
C          P(CHI**2),       SHADOW ENERGY
      DATA PCHCUT / 0.02 /, ESHCUT/ -0.3 /
C
C          E/P VERSION,  LOWER         UPPER E/P CUT
      DATA JEVERS/ 1 /,  EPCUT/ 0.8 /, EPHCUT/ 3.0 /
C
C          MINIMUM INVARIANT MASS
      DATA AMICUT/ 0.050 /
C
      END
C=======================================================================
C
      SUBROUTINE CINIT
C
C     INITIATE RETURN VALUES
C
      COMMON  / CERECU / ICUT,
C  CUT VALUES
     + ISCUT,ICHCUT,COSCUT,PCUT,NDCUT,RSCUT,IDVERS,DLOCUT,DHICUT,DFLCUT,
     + DFHCUT,SUMCUT,ENECUT,PCHCUT,ESHCUT,JEVERS,EPCUT,EPHCUT,AMICUT,
C  RETURN VALUES FOR CUT VARIABLES
     + ISCAN,ICHECK,CTH,PMOM6,JREM,NDEX,RELDE,DEDX,
     + SULPI,SULH2,PCHI2,ESHLO,VEREL,IPAR,IPAMI,AMINV,
C  RETURN VALUES FOR NON CUT VARIABLES
     + SIGDE,ESHM,ESHSUB,EBITER,ENEW,CHI2N,ICON,SUMPI,ICOP,DPHIM
C
      DATA ICALL/0/
C
      ICALL=ICALL+1
      IF(ICALL.GE.2) GOTO 100
C
      ICUW=ICUT
      WRITE(6,99)
   99 FORMAT(/,1X,' ELECTRON SELECTION CRITERIA',/)
      CALL FJHBIT(ICUW)
C
  100 ISCAN=1000
      ICHECK=-1
      CTH=2.0
      PMOM6=-1.0
      JREM=-1
      NDEX=-1
      RELDE=1000.0
      DEDX=-1.0
      SULPI=1000.0
      SULH2=1000.0
      PCHI2=-1.0
      VEREL=-1.0
      SIGDE=1000.0
      ESHM=-1.0
      ESHSUB=-1.0
      EBITER=-1.0
      ENEW=-1.0
      ICON=-1
      SUMPI=-1.0
      ICOP=-1
      DPHIM=-1.0
      IPAR=-1
      IPAMI=-1
      AMINV=1000.0
C
      RETURN
      END
C======================================================================
C
      SUBROUTINE FJHBIT(KA)
C
C     WRITE BIT PATTERN OF KA
C
      COMMON  / CERECU / ICUT,
C  CUT VALUES
     + ISCUT,ICHCUT,COSCUT,PCUT,NDCUT,RSCUT,IDVERS,DLOCUT,DHICUT,DFLCUT,
     + DFHCUT,SUMCUT,ENECUT,PCHCUT,ESHCUT,JEVERS,EPCUT,EPHCUT,AMICUT,
C  RETURN VALUES FOR CUT VARIABLES
     + ISCAN,ICHECK,CTH,PMOM6,JREM,NDEX,RELDE,DEDX,
     + SULPI,SULH2,PCHI2,ESHLO,VEREL,IPAR,IPAMI,AMINV,
C  RETURN VALUES FOR NON CUT VARIABLES
     + SIGDE,ESHM,ESHSUB,EBITER,ENEW,CHI2N,ICON,SUMPI,ICOP,DPHIM
      DIMENSION JHBIT(32)
      LOGICAL FIRST / .TRUE. /
      CHARACTER*50 CCOND(32) / 32*'0  NOT USED' /
      CHARACTER*3 CLEL(0:31)
      INTEGER ITRCNT / 0  /, ACCPTD / 0 /, NOFAIL / 0 /, ITRNOF / 0 /
      INTEGER REASON(0:31) / 32*0 /
      INTEGER FAILS(0:31) / 32*0 /, FAILSP(0:31) / 32*0 /,CHECKS(0:31) /
     *  0,  1,  3,  2,  4,  7,  5,  6, 12,  8,  9, 10, 11, 13, 14, 15,
     * 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31/
      REAL*4 REAMAT(0:31,0:31) / 1024*0./
      INTEGER ONE / 1 /
C
      DO 103 IB1=1,32
        IF( LAND(KA,iSHFTL(ONE,32-IB1)) .NE. 0 ) THEN
          JHBIT(IB1) = 1
        ELSE
          JHBIT(IB1) = 0
        ENDIF
  103 CONTINUE
C
C
      WRITE(6,1014) JHBIT,KA
 1014 FORMAT(1X,32I2,I12)
C
      IF( FIRST ) THEN
        FIRST = .FALSE.
C
        WRITE(CCOND(32),9232) JHBIT(32),ISCUT
        WRITE(CCOND(31),9231) JHBIT(31),ICHCUT
        WRITE(CCOND(30),9230) JHBIT(30),COSCUT
        WRITE(CCOND(29),9229) JHBIT(29),PCUT
        WRITE(CCOND(28),9228) JHBIT(28)
        WRITE(CCOND(27),9227) JHBIT(27),NDCUT
        WRITE(CCOND(26),9226) JHBIT(26),RSCUT
        WRITE(CCOND(25),9225) JHBIT(25),DLOCUT,DHICUT,IDVERS
        WRITE(CCOND(24),9224) JHBIT(24),SUMCUT
        WRITE(CCOND(23),9223) JHBIT(23),ENECUT
        WRITE(CCOND(22),9222) JHBIT(22),PCHCUT
        WRITE(CCOND(21),9221) JHBIT(21),ESHCUT
        WRITE(CCOND(20),9220) JHBIT(20)
        WRITE(CCOND(19),9219) JHBIT(19),AMICUT
        WRITE(CCOND(18),9218) JHBIT(18)
        WRITE(CCOND(17),9217) JHBIT(17)
        WRITE(CCOND(16),9216) JHBIT(16)
        WRITE(CCOND(15),9215) JHBIT(15)
        WRITE(CCOND(14),9214) JHBIT(14)
        WRITE(CCOND(13),9213) JHBIT(13),EPCUT,JEVERS
        WRITE(CCOND(12),9212) JHBIT(12),EPHCUT
 9232   FORMAT(I1,2X,'ISCAN      <',I3)
 9231   FORMAT(I1,2X,'ICHECK     >=',I3)
 9230   FORMAT(I1,2X,'COSTH      <=',F6.3)
 9229   FORMAT(I1,2X,'P          > ',F6.3)
 9228   FORMAT(I1,2X,'TRACK IN ZE4V?')
 9227   FORMAT(I1,2X,'NDEDX      > ',I3)
 9226   FORMAT(I1,2X,'RELDE      < ',F6.3)
 9225   FORMAT(I1,2X,F6.3,' < DELDEDX/SIGMA < ',F6.3,' IDVERS=',I3)
 9224   FORMAT(I1,2X,'UNIQUENESS < ',F6.3)
 9223   FORMAT(I1,2X,'NEAR BY E  < ',F6.3)
 9222   FORMAT(I1,2X,'P(CHI**2)  > ',F6.3)
 9221   FORMAT(I1,2X,'LG10(ESHAD)> ',F6.3)
 9220   FORMAT(I1,2X,'PHOT PARTNER?')
 9219   FORMAT(I1,2X,'MIN(MIJ)   < ',F6.3)
 9218   FORMAT(I1,2X,'No Meier LG Energy')
 9217   FORMAT(I1,2X,'OBSOLETE')
 9216   FORMAT(I1,2X,'OBSOLETE')
 9215   FORMAT(I1,2X,'OBSOLETE')
 9214   FORMAT(I1,2X,'OBSOLETE')
 9213   FORMAT(I1,2X,'E/P        > ',F6.3,' JEVERS=',I3)
 9212   FORMAT(I1,2X,'E/P        < ',F6.3)
        WRITE(6,9401) (CCOND(I),CCOND(I-16),I=32,17,-1)
 9401   FORMAT(' CUTS USED IN EKAND (0=CUT IS DISABLED)'/1X,100('-')/
     *        (1X,2A50))
      ENDIF
      RETURN
      ENTRY EALZFA( KA )
C-----------------------------------------------------------
C  DETAIL REASON FOR FAILURE
C-----------------------------------------------------------
      IF( KA .NE. 0 ) THEN
        IFL = 0
        I = LAND(ICUT,KA)
        DO 1000 J=32,1,-1
          IF( LAND(I,iSHFTL(ONE,32-J)) .NE. 0 ) THEN
            IF( IFL .EQ. 0 ) THEN
              IFL = 1
              WRITE(6,9301) CCOND(J)(4:50)
 9301         FORMAT(10X,' CANDIDATE FAILED AT CUTS: ',A46)
            ELSE
              WRITE(6,9302) CCOND(J)(4:50)
 9302         FORMAT(10X,'                           ',A46)
            ENDIF
          ENDIF
 1000   CONTINUE
      ENDIF
      RETURN
*PMF 09/12/99      ENTRY EAFLST( KA, / COSTH / )
      ENTRY EAFLST( KA, COSTH  )
C-----------------------------------------------------------
C  ACCUMULATE STATISTICS FOR FAILURES (SIMPLE VERSION)
C-----------------------------------------------------------
      CALL NOARG( NARGS )
      ITRNOF = ITRNOF + 1
      IF( KA .NE. 0 ) THEN
        DO 6010 J=0,31
          I = CHECKS(J)
          IF( LAND(KA,iSHFTL(ONE,I)) .EQ. 0 ) GO TO 6010
            FAILS(I) = FAILS(I) + 1
            IF( NARGS.GT.1 .AND. COSTH.GT.0. ) FAILSP(I) = FAILSP(I) + 1
            GO TO 6020
 6010   CONTINUE
 6020   CONTINUE
      ELSE
        NOFAIL = NOFAIL + 1
      ENDIF
      RETURN
      ENTRY EAFLDP
C-----------------------------------------------------------
C  PRINT SIMPLE STATISTICS FOR FAILURES AND CANDIDATES
C-----------------------------------------------------------
      WRITE(6,9601) ITRNOF
 9601 FORMAT(//' Summary of Electron Selection after ',I7,' tracks.',
     *   36X,' Asymmetry of'/
     *   ' cut     rejected     surviving  n/(n-1)       Name',
     *   37X,' rejected tracks'/
     *   1X,103('-'))
      IF( ITRNOF .GT.0 ) THEN
        IPREV = ITRNOF
        DO 6030 J=0,31
          I = CHECKS(J)
          IF( FAILS(I) .GT. 0 ) THEN
            IPREV = IPREV - FAILS(I)
            ASYM = FLOAT(2*FAILSP(I)-FAILS(I))/FAILS(I)
            DASYM = .5*SQRT((1.+ASYM)*(1.-ASYM)/FAILS(I))
            WRITE(6,9602) I, FAILS(I), FLOAT(FAILS(I))/ITRNOF*100,
     *                    IPREV, FLOAT(IPREV)/ITRNOF*100,
     *                    FLOAT(IPREV)/MAX(IPREV+FAILS(I),1)*100,
     *                    CCOND(32-I)(4:50), ASYM, DASYM
 9602       FORMAT(I3,I8,'=',F5.1,'%',I7,'=',F5.1,'%  ',F5.1,'% ',A46,
     *             3X,F6.3,'+-',F5.3)
          ENDIF
 6030   CONTINUE
        WRITE(6,9607) NOFAIL, FLOAT(NOFAIL)/ITRNOF*100
 9607   FORMAT(/3X,I8,'=',F6.2,'% Accepted')
      ENDIF
      RETURN
      ENTRY EALZST( KA )
C-----------------------------------------------------------
C  ACCUMULATE STATISTICS FOR FAILURES
C-----------------------------------------------------------
      ITRCNT = ITRCNT + 1
      IF( KA .NE. 0 ) THEN
        IREAS = 0
        DO 7010 I=0,31
          IF( LAND(KA,iSHFTL(ONE,I)) .NE. 0 ) THEN
            REASON(I) = REASON(I) + 1
            IREAS = IREAS + 1
          ENDIF
 7010   CONTINUE
C       WEIGHT = 1./FLOAT(IREAS)
        WEIGHT = 1.
        DO 7030 I=0,31
          IF( LAND(KA,iSHFTL(ONE,I)) .NE. 0 ) THEN
            IF( IREAS .EQ. 1 ) THEN
              REAMAT(I,I) = REAMAT(I,I) + WEIGHT
            ELSE
              DO 7020 J=I+1,31
                IF( LAND(KA,iSHFTL(ONE,J)) .NE. 0 ) THEN
                  REAMAT(I,J) = REAMAT(I,J) + WEIGHT
                ENDIF
 7020         CONTINUE
            ENDIF
          ENDIF
 7030   CONTINUE
      ELSE
        ACCPTD = ACCPTD + 1
      ENDIF
      RETURN
      ENTRY EALZFL
C-----------------------------------------------------------
C  PRINT STATISTICS FOR FAILURES AND CANDIDATES
C-----------------------------------------------------------
      WRITE(6,9501) ITRCNT
 9501 FORMAT(//40X,'SUMMARY OF '/
     *   20X,' FAILURE REASONS FOR E CANDIDATES AFTER',I7,' TRACKS'/
     *   1X,72('-'))
      IF( ITRCNT .GT.0 ) THEN
        DO 8010 I=0,31
          IF( REASON(I) .GT. 0 ) THEN
            WRITE(6,9502) I, REASON(I), FLOAT(REASON(I))/ITRCNT*100,
     *                    CCOND(32-I)(4:50)
 9502       FORMAT(1X,I3,I7,' = ',F5.1,'% AT CUT ',A46)
          ENDIF
 8010   CONTINUE
        WRITE(6,9507) ACCPTD, FLOAT(ACCPTD)/ITRCNT*100
 9507   FORMAT(/4X,I7,' = ',F5.1,'% ACCEPTED')
        WRITE(6,9503) (I,I=0,31)
 9503   FORMAT(/' REASON MATRIX '/3X,32I3)
C       WRITE(6,9504) (J,(INT(REAMAT(I,J)/ITRCNT*100.),I=0,31),J=0,31)
C9504   FORMAT(1X,I2,32I3)
C                                           SYMMETRIZE
C       DO 8012 J=1,31
C         DO 8011 I=0,J-1
C           REAMAT(J,I) = REAMAT(I,J)
C8011     CONTINUE
C8012   CONTINUE
        DO 8030 J=0,31
          DO 8020 I=0,31
            L = INT(REAMAT(I,J)/ITRCNT*100.+.5)
            IF( L .EQ. 0 ) THEN
              IF( I.NE. J ) THEN
                CLEL(I) = '  .'
              ELSE
                CLEL(I) = '   '
              ENDIF
            ELSE
              WRITE(CLEL(I),9505) L
 9505         FORMAT(I3)
            ENDIF
 8020     CONTINUE
          WRITE(6,9506) J, CLEL, CCOND(32-J)(4:33)
 9506     FORMAT(1X,I2,32A3,1X,A30)
 8030   CONTINUE
      ENDIF
      END
