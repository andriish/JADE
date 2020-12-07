C   29/04/87 705071933  MEMBER NAME  ZSLINE9  (JADEGS)      FORTRAN
      SUBROUTINE ZSLINE(N,Z0,TANL,SIGMAZ,ZMIN,ZMAX)
C*811215*DITTMANN****************************************************
C*                                                                  *
C*             Z - S   S T R A I G H T   L I N E   F I T            *
C*                                                                  *
C*       SELECT HIT TRIPLES ON ADJACENT WIRES,                      *
C*       IF NOT SUFFICIENT HIT TRIPLES FOUND, SELECT HIT DOUBLES    *
C*       IF NOT SUFFICIENT HIT DOUBLES, USE SINGLE HITS.            *
C*       FIT FIRST APPROXIMATION LINE THROUGH HIT MULTIPLES,        *
C*       REMOVE ITERATIVELY BAD FITTING MULTIPLES.                  *
C*       USE THIS LINE AS ROAD CENTER WITH WIDTH 2 SIGMA            *
C*       TO SELECT HITS FOR FINAL FIT.                              *
C*       MODIFIED 29/04/87    E ELSEN                               *
C*       INTRODUCED COMMON CZSPRM                                   *
C********************************************************************
      COMMON /CWORK/ NHIT,IZB(60),IJC(60),IJH(60),NW(60),LH(60),
     *                LRFL(60),ALU(60),ARU(60),X(60),Y(60),Z(60),S(60),
     *                G(60),DT(60),DL(60),DW(60),FCORR(60)
      DIMENSION JW(60),JTRIP(60),ZTRIP(60),STRIP(60)
      REAL*8 SN,SS,SZ,SS2,SZ2,SSZ,D,ZZ,TT,SSJ,SZJ,SGJ
C     DIMENSION ITR(60),ITRW(60),DZ1TR(60),DZ2TR(60)
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
C
      N = 0
      SMIN = 1000.
      SMAX = 0.
      DO 11 J=1,NHIT
      JW(J) = 0
      IF(IZB(J).EQ.0) GOTO 11
      N = N + 1
      JW(J) = LAND(NW(J),15)
      IF(S(J).LT.SMIN) SMIN=S(J)
      IF(S(J).GT.SMAX) SMAX=S(J)
   11 CONTINUE
      IF(N.LT.3) GOTO 100
      DS1 = SMAX - SMIN
C        SELECT HIT TRIPLES
      NMULT = 3
      NTRIP = 0
      SIGMAT = 0.
      SMIN = 1000.
      SMAX = 0.
      NH2 = NHIT - 2
      DO 15 J=1,NH2
      IF(IZB(J).EQ.0) GOTO 15
      IF(JW(J+1).NE.JW(J)+1 .OR. JW(J+2).NE.JW(J)+2) GOTO 15
      DZ1 = Z(J) + Z(J+2) - 2.*Z(J+1)
      DZ2 = Z(J) - Z(J+2)
      IF(ABS(DZ1).GT.AZSCT1(NZSPRD)) GOTO 15
      IF(ABS(DZ2).GT.AZSCT2(NZSPRD)) GOTO 15
      NTRIP = NTRIP + 1
      JTRIP(NTRIP) = J
      ZTRIP(NTRIP) = (Z(J) + Z(J+1) + Z(J+2)) / 3.
      STRIP(NTRIP) = S(J+1)
      DZ = (Z(J)**2+Z(J+1)**2+Z(J+2)**2 -3.*ZTRIP(NTRIP)**2)/2.
      IF(DZ.LT.0.) DZ=0.
      SIGMAT = SIGMAT + SQRT(DZ)
      IF(S(J).LT.SMIN) SMIN=S(J)
      IF(S(J+2).GT.SMAX) SMAX=S(J+2)
C     ITR(NTRIP) = J
C     ITRW(NTRIP) = NW(J)
C     DZ1TR(NTRIP) = DZ1
C     DZ2TR(NTRIP) = DZ2
   15 CONTINUE
C     PRINT 101,(ITR(I),ITRW(I),DZ1TR(I),DZ2TR(I),I=1,NTRIP)
C 101 FORMAT( 7(I4,X Z3,2F5.0))
      IF(NTRIP.LT.3) GOTO 16
      SIGMAT=SIGMAT/NTRIP
      DS3 = SMAX - SMIN
      IF(2.*DS3.GT.DS1) GOTO 20
C        < 3 HIT TRIPLES, TRY WITH HIT DOUBLES
   16 NMULT = 2
      NTRIP = 0
      SIGMAT = 0.
      NH2 = NHIT - 1
      DO 17 J=1,NH2
      IF(IZB(J).EQ.0) GOTO 17
      IF(JW(J+1).NE.JW(J)+1) GOTO 17
      DZ1 = Z(J) - Z(J+1)
      IF(ABS(DZ1).GT.AZSCT3(NZSPRD)) GOTO 17
      NTRIP = NTRIP + 1
      JTRIP(NTRIP) = J
      ZTRIP(NTRIP) = (Z(J) + Z(J+1)) / 2.
      STRIP(NTRIP) = (S(J) + S(J+1)) / 2.
      DZ = Z(J)**2+Z(J+1)**2 - 2.*ZTRIP(NTRIP)**2
      IF(DZ.LT.0.) DZ=0.
      SIGMAT = SIGMAT + SQRT(DZ)
C     ITR(NTRIP) = J
C     ITRW(NTRIP) = NW(J)
C     DZ1TR(NTRIP) = DZ1
C     DZ2TR(NTRIP) = 0.
   17 CONTINUE
C     PRINT 101,(ITR(I),ITRW(I),DZ1TR(I),DZ2TR(I),I=1,NTRIP)
      IF(NTRIP.LT.3) GOTO 18
      SIGMAT=SIGMAT/NTRIP
      GOTO 20
C        < 3 HIT DOUBLES, TRY WITH SINGLE HITS
   18 NMULT = 1
      NTRIP = 0
      DO 19 J=1,NHIT
      IF(IZB(J).EQ.0) GOTO 19
      NTRIP = NTRIP + 1
      JTRIP(NTRIP) = J
      ZTRIP(NTRIP) = Z(J)
      STRIP(NTRIP) = S(J)
   19 CONTINUE
      IF(NTRIP.LT.3) GOTO 100
C        FIRST APPROXIMATION BY FIT THRU TRIPLE COORDINATES
   20 N = 0
      SN = 0.
      SS = 0.
      SZ = 0.
      SS2 = 0.
      SZ2 = 0.
      SSZ = 0.
      DO 21 J=1,NTRIP
      IF(JTRIP(J).EQ.0) GOTO 21
      N = N + 1
      SSJ = STRIP(J)
      SZJ = ZTRIP(J)
      SN = SN + 1.D0
      SS = SS + SSJ
      SZ = SZ + SZJ
      SS2 = SS2 + SSJ**2
      SZ2 = SZ2 + SZJ**2
      SSZ = SSZ + SSJ*SZJ
   21 CONTINUE
      D = SN*SS2 - SS**2
      ZZ = (SZ*SS2-SS*SSZ) / D
      TT = (SN*SSZ-SS*SZ) / D
      SIGMAZ = (SZ2-ZZ*SZ-TT*SSZ) / (SN-2.)
      IF(SIGMAZ.LT.0.) SIGMAZ=0.
      SIGMAZ = SQRT(SIGMAZ)
      Z0 = ZZ
      TANL = TT
C     PRINT 102, NMULT,Z0,TANL,SIGMAZ,SIGMAT
C     PRINT 103, (JTRIP(I),I=1,NTRIP)
      IF(NMULT.EQ.3 .AND. SIGMAZ.LT.1.2*SIGMAT) GOTO 23
      IF(NMULT.EQ.2 .AND. SIGMAZ.LT.1.4*SIGMAT) GOTO 23
      IF(N.LE.3 .AND. NMULT.EQ.3) GOTO 16
      IF(N.LE.3 .AND. NMULT.EQ.2) GOTO 18
      IF(N.LE.3) GOTO 23
C        BAD FIT, REMOVE WORST TRIPLE
      SIGMAX = 0.
      DO 22 J=1,NTRIP
      IF(JTRIP(J).EQ.0) GOTO 22
      DZ = ABS(ZTRIP(J)-Z0-STRIP(J)*TANL)
      IF(DZ.LT.SIGMAX) GOTO 22
      SIGMAX = DZ
      JMAX = J
   22 CONTINUE
      IF(NMULT.EQ.1 .AND. SIGMAX.LT.AZSCT4(NZSPRD)) GOTO 23
      JTRIP(JMAX) = 0
      GOTO 20
C        SELECT HITS USING FIRST APPROXIMATION ROAD
   23 N = 0
      SIGMAZ = 0.
      I = 0
      DO 26 J=1,NTRIP
      IF(JTRIP(J).EQ.0) GOTO 26
      IF(JTRIP(J).LE.I) GOTO 24
      I = JTRIP(J)
      N = N + 1
      DZ = Z(I) - Z0 - S(I)*TANL
      SIGMAZ = SIGMAZ + DZ**2
      IF(NMULT.EQ.1) GOTO 26
   24 IF(JTRIP(J)+1.LE.I) GOTO 25
      I = JTRIP(J) + 1
      N = N + 1
      DZ = Z(I) - Z0 - S(I)*TANL
      SIGMAZ = SIGMAZ + DZ**2
      IF(NMULT.EQ.2) GOTO 26
   25 I = JTRIP(J) + 2
      N = N + 1
      DZ = Z(I) - Z0 - S(I)*TANL
      SIGMAZ = SIGMAZ + DZ**2
   26 CONTINUE
      SIGMAZ = SQRT(SIGMAZ/(N-2))
      DO 27 J=1,NHIT
      IF(IZB(J).EQ.0) GOTO 27
      DZ = ABS(Z(J)-Z0-S(J)*TANL)
      IF(DZ.GT.2.*SIGMAZ) IZB(J)=0
   27 CONTINUE
C        FINAL PARAMETERS FOR Z = Z0 + S*TANL
C        WEIGHTED WITH PULSEHEIGHT
   40 N = 0
      SN = 0.
      SS = 0.
      SZ = 0.
      SS2 = 0.
      SZ2 = 0.
      SSZ = 0.
      SMIN = 1000.
      SMAX = 0.
      DO 41 J=1,NHIT
      IF(IZB(J).EQ.0) GOTO 41
      IZB(J) = IABS(IZB(J))
      N = N + 1
      SSJ = S(J)
      SZJ = Z(J)
      SGJ = G(J)
      SN = SN + SGJ
      SS = SS + SSJ*SGJ
      SZ = SZ + SZJ*SGJ
      SS2 = SS2 + SSJ**2*SGJ
      SZ2 = SZ2 + SZJ**2*SGJ
      SSZ = SSZ + SSJ*SZJ*SGJ
      IF(S(J).LT.SMIN) SMIN=S(J)
      IF(S(J).GT.SMAX) SMAX=S(J)
C     ITR(N) = J
   41 CONTINUE
      IF(N.LT.3) GOTO 100
      D = SN*SS2 - SS**2
      ZZ = (SZ*SS2-SS*SSZ) / D
      TT = (SN*SSZ-SS*SZ) / D
      SIGMAZ = (SZ2-ZZ*SZ-TT*SSZ) / SN
      IF(SIGMAZ.LT.0.) SIGMAZ=0.
      SIGMAZ = SQRT(SIGMAZ)
      Z0 = ZZ
      TANL = TT
      ZMIN = Z0 + SMIN*TANL
      ZMAX = Z0 + SMAX*TANL
C     PRINT 102, N,Z0,TANL,SIGMAZ
C 102 FORMAT(I5,4F10.1)
C     PRINT 103, (ITR(I),I=1,N)
C 103 FORMAT(40I3)
C
  100 RETURN
      END
