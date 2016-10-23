C   20/01/87 804271520  MEMBER NAME  ZE4VEK   (S)           FORTRAN77
C
      SUBROUTINE ZE4VEK(NPSPUR,IKAND)
C-------------------------------------------------------------
C
C  LAST MOD 18/08/87 E ELSEN
C  TPTR POINTER BUG FIXED                           18/08/87 E ELSEN
C
C  RENAME OF COMMON TO CZRECU AND SUBR. TO ZJHBIT   12/08/87 E ELSEN
C ROUTINE TO DETERMINE WHETHER A TRACK IS AN ELECTRON CANDIDATE
C
C INPUT  NPSPUR = POINTER TO TRACK IN ZE4V BANK
C
C STANDARD CUTS SEE BLOCK DATA
C
C OUTPUT  IKAND=0 ELEKTRON CANDIDATE
C              .NE.0 NO CANDIDATE
C
C-------------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
      LOGICAL TBIT, LCONV
C
      COMMON /BCS/ IW(1)
      DIMENSION HW(1), RW(1)
      EQUIVALENCE (HW(1),IW(1),RW(1))
C
      LOGICAL LFIRST / .TRUE. /
C
      COMMON / CZRECU / ICUT,
C  CUT VALUES
     + ISCUT,ICHCUT,COSCUT,PCUT,NDCUT,RSCUT,IDVERS,DLOCUT,DHICUT,DFLCUT,
     + DFHCUT,SUMCUT,ENECUT,PCHCUT,ESHCUT,JEVERS,EPCUT,EPHCUT,AMICUT
C
      IF ( LFIRST ) THEN
        LFIRST = .FALSE.
        WRITE(6,9001)
 9001   FORMAT(/,1X,' ELECTRON SELECTION CRITERIA',/)
        CALL ZJHBIT(ICUT)
C
        CALL FEHLTX( 31,'ZE4VEK: NO ZE4V-BANK')
        CALL FEHLTX( 32,'ZE4VEK: NO CORESPONDING TPTR BANK')
        CALL FEHLTX( 33,'ZE4VEK: NO PHOT-BANK FILLED')
      ENDIF
C
      IKAND=0
C                                      CHECK IF BANKS ARE FILLED
      NPZE4V = IW( IBLN('ZE4V') )
      IF( NPZE4V .LE. 0 ) CALL FEHLER( 31, &7000 )
C
      NTPTR = HW( NPSPUR*2 + 17)
      CALL CLOC( NPTPTR, 'TPTR', NTPTR )
      IF( NPTPTR .LE. 0 ) THEN
        IF( TBIT( ICUT, 26))    CALL FEHLER( 32, &7000 )
        IF( TBIT( ICUT, 25))    CALL FEHLER( 32, &7000 )
        IF( TBIT( ICUT, 24))    CALL FEHLER( 32, &7000 )
      ENDIF
C
      NPPHOT = IW( IBLN('PHOT') )
      IF (NPPHOT .LE. 0 ) THEN
        IF( TBIT( ICUT, 19))    CALL FEHLER( 33, &7000 )
        IF( TBIT( ICUT, 18))    CALL FEHLER( 33, &7000 )
      ENDIF
C
      LT = HW(NPZE4V*2 + 5)
C                                      TRACK # IN PATR - BANK
      INR = HW( NPSPUR*2 + LT*2 + 9 )
C------------------------------------------------- COS(THETA)
      COSTH = ABS( RW( NPSPUR + 3 ))
      IF( COSTH .GT. COSCUT .AND. TBIT(ICUT,29)) IKAND=IKAND+4
C
C------------------------------------------------- MOMENTUM CUT
      PTOT = RW(NPSPUR + 6)
      IF(PTOT .LT. 0.001) PTOT = 0.001
      IF(PTOT .LT. PCUT .AND. TBIT(ICUT,28)) IKAND=IKAND+8
C
C----------DE/DX--------DE/DX--------DE/DX-------- DE/DX CUTS
C
C                                      LOOK INTO TPTR-BANK
      IF( NPTPTR .LE. 0 ) GOTO 2110
C
      NDEX = IW( NPTPTR + 72 )
      DEDX = RW( NPTPTR + 73 )
      IF(DEDX.LT.0.001) DEDX=0.001
      SIGDE = RW( NPTPTR + 74 )
      IF(SIGDE.LE.0.0) SIGDE=10.0
      RELDE = SIGDE / DEDX
C
C---------------------------------------------------------- DE/DX HITS
      IF(NDEX.LT.NDCUT.AND.TBIT(ICUT,26)) IKAND=IKAND+32
C
C------------------------------------------------------ RELATIVE SIGMA
      IF(RELDE.GT.RSCUT.AND.TBIT(ICUT,25)) IKAND=IKAND+64
C
C---------------------------------------------------------- DE/DX
      IDCUT=0
      GOTO (100,110), IDVERS
C                               SIGMA CUT
  100 DELO = 10.0 - DLOCUT*SIGDE
      DEHI = 10.0 + DHICUT*SIGDE
      GOTO 120
C                               FIXED DE/DX CUT
  110 DELO = DFLCUT
      DEHI = DFHCUT
  120 IF( DEDX .LT. DELO .OR. DEDX .GT. DEHI) IDCUT=1
      IF(IDCUT.EQ.1.AND.TBIT(ICUT,24)) IKAND=IKAND+128
C
C--------LEAD GLASS--------LEAD GLASS--------LEAD GLASS------ LG CUTS
C
C                                      LOOK FOR BANKS
 2110 CONTINUE
C
      ESHOWM = RW( NPSPUR + LT + 3 )
C
C-------- GAMMA CONVERSIONS -------- GAMMA CONVERSIONS --------
C
      IF( NPPHOT .LE. 0 ) GOTO 2130
C
      XMINV = 1000.
      IPPHOT = IBLN('PHOT')
        LCONV = .FALSE.
        NPHOT = IPPHOT + 1
  210   NPHOT = IW(NPHOT-1)
        IF( NPHOT.LE.0 ) GO TO 220
        IF( IW(NPHOT+24) .EQ. INR .OR. IW(NPHOT+25) .EQ. INR ) THEN
          LCONV = .TRUE.
          XMINV = RW( NPHOT + 5 )
          GOTO 220
        ENDIF
        GO TO 210
  220 CONTINUE
      IF(LCONV .AND. TBIT(ICUT,19)) IKAND=IKAND+4096
C
      IF(XMINV.LT.AMICUT.AND.TBIT(ICUT,18)) IKAND=IKAND+8192
C
C
 2130 CONTINUE
C
C
C---------------------------------------------------- E/P
      VEREL=AMAX1(0.01,ESHOWM / PTOT)
      IF(VEREL.LT.EPCUT.AND.TBIT(ICUT,12)) IKAND=IKAND+524288
      IF(VEREL.GT.EPHCUT.AND.TBIT(ICUT,11)) IKAND=IKAND+1048576
C
      RETURN
C
 7000 CONTINUE
C
      IKAND = 1000
      RETURN
C
      END
C=======================================================================
C
      BLOCK DATA
C
C     STANDARD SELECTION CUTS FOR ELECTRON CANDIDATES
C
      COMMON  / CZRECU / ICUT,
C  CUT VALUES
     + ISCUT,ICHCUT,COSCUT,PCUT,NDCUT,RSCUT,IDVERS,DLOCUT,DHICUT,DFLCUT,
     + DFHCUT,SUMCUT,ENECUT,PCHCUT,ESHCUT,JEVERS,EPCUT,EPHCUT,AMICUT
C
C
C          STANDARD CUT CRITERIA
      DATA ICUT/ Z001800EC /
C     DATA ICUT/ Z001830EC /
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
      DATA SUMCUT/ 0.15 /, ENECUT/ 0.30 /
C
C          P(CHI**2),       SHADOW ENERGY
      DATA PCHCUT / 0.05 /, ESHCUT/ -0.5 /
C
C          E/P VERSION,  LOWER         UPPER E/P CUT
      DATA JEVERS/ 1 /,  EPCUT/ 0.8 /, EPHCUT/ 2.0 /
C
C          MINIMUM INVARIANT MASS
      DATA AMICUT/ 0.050 /
C
      END
C======================================================================
C
      SUBROUTINE ZJHBIT(KA)
C
C     WRITE BIT PATTERN OF KA
C
      COMMON  / CZRECU / ICUT,
C  CUT VALUES
     + ISCUT,ICHCUT,COSCUT,PCUT,NDCUT,RSCUT,IDVERS,DLOCUT,DHICUT,DFLCUT,
     + DFHCUT,SUMCUT,ENECUT,PCHCUT,ESHCUT,JEVERS,EPCUT,EPHCUT,AMICUT
      DIMENSION JHBIT(32)
C
      INTEGER ONE / 1 /
      DO 103 IB1=1,32
        IF( LAND(KA,SHFTL(ONE,32-IB1)) ) THEN
          JHBIT(IB1) = 1
        ELSE
          JHBIT(IB1) = 0
        ENDIF
  103 CONTINUE
C
      WRITE(6,1014) JHBIT,KA
 1014 FORMAT(1X,32I2,I12)
C
      WRITE(6,9101) JHBIT(32),ISCUT,
     *              JHBIT(31),ICHCUT,
     *              JHBIT(30),COSCUT,
     *              JHBIT(29),PCUT,
     *              JHBIT(28),
     *              JHBIT(27),NDCUT,
     *              JHBIT(26),RSCUT,
     *              JHBIT(25),DLOCUT,DHICUT,IDVERS
 9101 FORMAT(' CUTS USED IN ZE4VEK (0=CUT IS DISABLED)'/
     *       ' --------------------------------------'/
     *       2X,I3,2X,'ISCAN      <',I3,T40,
     *       2X,I3,2X,'ICHECK     >=',I3/
     *       2X,I3,2X,'COSTH      <=',F6.3,T40,
     *       2X,I3,2X,'P          > ',F6.3/
     *       2X,I3,2X,'TRACK IN ZE4V?',T40,
     *       2X,I3,2X,'NDEDX      > ',I3/
     *       2X,I3,2X,'RELDE      < ',F6.3,T40,
     *       2X,I3,2X,F6.3,' < DELDEDX/SIGMA < ',F6.3,' IDVERS=',I3)
      WRITE(6,9102) JHBIT(24),SUMCUT,
     *              JHBIT(23),ENECUT,
     *              JHBIT(22),PCHCUT,
     *              JHBIT(21),ESHCUT,
     *              JHBIT(20),
     *              JHBIT(19),AMICUT
 9102 FORMAT(2X,I3,2X,'UNIQUENESS < ',F6.3,T40,
     *       2X,I3,2X,'NEAR BY E  < ',F6.3/
     *       2X,I3,2X,'P(CHI**2)  > ',F6.3,T40,
     *       2X,I3,2X,'LG10(ESHAD)> ',F6.3/
     *       2X,I3,2X,'PHOT PARTNER?',T40,
     *       2X,I3,2X,'MIN(MIJ)   < ',F6.3)
      WRITE(6,9103) JHBIT(18),
     *              JHBIT(17),
     *              JHBIT(16),
     *              JHBIT(15),
     *              JHBIT(14),
     *              JHBIT(13),EPCUT,JEVERS,
     *              JHBIT(12),EPHCUT
 9103 FORMAT(2X,I3,2X,'OBSOLETE',T40,
     *       2X,I3,2X,'OBSOLETE'/
     *       2X,I3,2X,'OBSOLETE',T40,
     *       2X,I3,2X,'OBSOLETE'/
     *       2X,I3,2X,'OBSOLETE',T40,
     *       2X,I3,2X,'E/P        > ',F6.3,' JEVERS=',I3/
     *       2X,I3,2X,'E/P        < ',F6.3)
      RETURN
      END
