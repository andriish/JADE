      SUBROUTINE AMPS2Z( IP, NPJETC, Z, W, IFLAG )
      IMPLICIT INTEGER*2 (H)
      COMMON /CZSCAL/ IPVERS, ZALPDI, RESFAC, SECH(5)
C
C  COMMON FOR Z-RESOLUTION PARAMETERS,   USED IN SUBR. AMPS2Z
C
      COMMON / CZSPRM / NZSPRD,
C                                           PARMS FOR RESOLUTIONS
     * AZSRS0(3), AZSRSA(3),
C                                           PARMS FOR CUTS
     * AZSCT1(3), AZSCT2(3), AZSCT3(3), AZSCT4(3),
C                                           SECOND HIT
     * AZSSHT(5,3),
C                                           SECOND HIT DISTANCE
     * AZSSHD(3),
C                                           AVE RESOLUTIONS
     * AZSSAV(3),
C                                           ZSPD BANK FILL FLAG
     * LZSPDF
      LOGICAL LZSPDF
C-----------------------------------------------------------------------
C                            MACRO CJDRCH .... JET CHAMBER CONSTANTS.
C-----------------------------------------------------------------------
C
      COMMON / CJDRCH / RDEC(4),PSIIN(3),RINCR(3),FIRSTW(3),FSENSW(3),
     +                  RDEPTH,SWDEPL,YSUSPN,TIMDEL(2,3),ZMAX,ZOFFS,
     +                  ZRESOL,ZNORM,ZAL,ZSCAL,DRIDEV,DRICOS,DRISIN,
     +                  PEDES,TZERO(3),DRIROT(96,2),SINDRI(96,2),
     +                  COSDRI(96,2),DRIVEL(96,2),T0FIX(3),
     +                  ABERR(8), DUMJDC(20)
C
C      BLOCK DATA SET TO MC VALUES, KALIBR WILL SET REAL DATA VALUES
C--->  A CHANGE OF THIS COMMON MUST BE DONE SIMULTANEOUSLY WITH  <----
C--->  A CHANGE OF THE BLOCK DATA                                <----
C
C--------------------------- END OF MACRO CJDRCH -----------------------
C
      COMMON / BCS / HW(1)
      REAL EXTRMZ / 1250. /, ZALDEF / 1400. /
      REAL DISMAX / 3.6 /
      LOGICAL FIRST / .TRUE. /
      IF(
     - FIRST
     -)THEN
         FIRST = .FALSE.
         WRITE(6,9101) NZSPRD, ZALPDI, ZALDEF
 9101    FORMAT(' +++ AMPS2Z    NZSPRD=',I3,' ZALPDI=',F10.3,
     *          ' ZALDEF=',F10.3)
      ENDIF
      Z = 0.
      W = 1.
      IFLAG = 16
      AL = HW(IP+2)/8.
      AR = HW(IP+1)/8.
      IF(
     -  AL.GT.0. .AND. AR.GT.0.
     -)THEN
      IF(
     -  ZALPDI .EQ. ZALDEF
     -)THEN
          Z = ZALPDI*(AL-AR)/(AL+AR)
      IF(
     - NZSPRD .LE. 2
     -)THEN
            W = ( AZSSAV(NZSPRD) /
     *              ( AZSRS0(NZSPRD)+
     *                AZSRSA(NZSPRD)*SQRT(AL**2+AR**2)/(AL+AR)**2
     *              )
     *          )**2
      ELSE
            W = (AZSSAV(NZSPRD)/
     *             (AZSRS0(NZSPRD)+AZSRSA(NZSPRD)/(AL+AR)))**2
      ENDIF
      ELSE
          Z = .5*ZAL*(AL-AR)/(AL+AR)
      ENDIF
      IF(
     -  ABS( Z ) .LT. EXTRMZ
     -)THEN
      IF(
     - NZSPRD.GT.2
     -)THEN
            ISEC = 0
            NP = IP - 4
      IF(
     -  NP.GT.NPJETC*2+100 .AND. HW(NP)/8 .EQ. HW(IP)/8
     -)THEN
               ICELL = HW(IP)/128 + 1
               DIS = (HW(NP+3)-HW(IP+3))*
     *               (DRIVEL(ICELL,1)+DRIVEL(ICELL,2))/2.
               IF( ABS(DIS).LT.DISMAX ) ISEC = ISEC + 1
      ENDIF
            NP = IP + 4
      IF(
     -  NP.LT.NPJETC*2+100+HW(NPJETC*2+99) .AND.
     *          HW(NP)/8 .EQ. HW(IP)/8
     -)THEN
               ICELL = HW(IP)/128 + 1
               DIS = (HW(NP+3)-HW(IP+3))*
     *               (DRIVEL(ICELL,1)+DRIVEL(ICELL,2))/2.
               IF( ABS(DIS).LT.DISMAX ) ISEC = ISEC + 1
      ENDIF
            IF( ISEC .EQ. 0 ) IFLAG = 0
      ELSE
             IFLAG = 0
      ENDIF
      ENDIF
        IF(IFLAG.EQ.0 .AND.
     *     ( HW(IP+1).EQ. 32760 .OR. HW(IP+2).EQ. 32760 ) ) IFLAG = 32
      ENDIF
      RETURN
      END
