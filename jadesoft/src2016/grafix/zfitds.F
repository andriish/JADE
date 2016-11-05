C   22/04/87 705201641  MEMBER NAME  ZFITDS   (S)        M  FORTRAN77
      SUBROUTINE ZFITDS( ITRK )
C-----------------------------------------------------------
C  Version of 22/04/87      last mod 20/05/87   E Elsen
C     (..+1) = IJC(I),       REL POINTER IN JETC
C     (..+2) = S(I),         TRACK LENGTH
C     (..+3) = Z(I),         Z
C     (..+4) = FLAG(I),      0 IF OK
C     (..+5) = 1/SIGMA(I)**2
C----------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
      COMMON / BCS / IW(1)
      DIMENSION HW(1),RW(1)
      EQUIVALENCE (HW(1),IW(1),RW(1))
#include "cgraph.for"
#include "czsprm.for"
C                                           MODEGA = 1 FOR GA
      COMMON / CGAMOD / MODEGA, GAHEX
C
      REAL SBTICK / 100. /, SSTICK / 20. /
      REAL ZBTICK / 200. /, ZSTICK / 20. /
      REAL ABTICK / 1000. /, ASTICK / 200. /
      INTEGER NSOFF / 2 / , NZOFF / 0 /, NAOFF / 0 /
      INTEGER IXTOFF/ 100 /, IYTOFF / 100 /
      INTEGER PLOTW(4) / 210, 4085, 10, 3000 /
      INTEGER GRAPHW(4), AMPSW(4)
      INTEGER FLAG
      LOGICAL OWNZ / .FALSE. /, LHELP
      CHARACTER*8 DEST / 'L2      ' /
      CHARACTER*1 ANSWER
      CHARACTER*132 CLINE
      CHARACTER*50 CNUMB
      CHARACTER*3  CTOG
      CHARACTER*4  CHELP
      CHARACTER*1  ZERO / '0' /, NINE / '9' /
      CHARACTER*8 DATTIM(2)
C
      IX(S) = GRAPHW(1) + SCALEX*(S-SL)
      IY(Z) = GRAPHW(3) + SCALEY*(Z-ZL)
      IA(IAM) = AMPSW(3) + SCALAY*IAM
C
      CALL BLOC( NPZSPD, 'ZSPD', ITRK, &8000, IER )
C                                           EXTREME VALUES
      NTRW = IW(NPZSPD+1)
      NHIT = (IW(NPZSPD)-1)/NTRW
      SMIN = 1.E10
      SMAX = -SMIN
      ZMIN = 1.E10
      ZMAX = -ZMIN
      DO 10 J=1,NHIT
        S = RW(NPZSPD+(J-1)*NTRW+3)
        Z = RW(NPZSPD+(J-1)*NTRW+4)
        IF( SMIN.GT.S ) SMIN = S
        IF( SMAX.LT.S ) SMAX = S
        IF( ZMIN.GT.Z ) ZMIN = Z
        IF( ZMAX.LT.Z ) ZMAX = Z
   10 CONTINUE
      IF( SMIN.GE.0. ) THEN
        SL = INT(SMIN/SBTICK)*SBTICK
      ELSE
        SL = INT(SMIN/SBTICK-1.)*SBTICK
      ENDIF
      SL = SL - NSOFF*SSTICK
      IF( SMAX.GE.0. ) THEN
        SH = INT(SMAX/SBTICK+1.)*SBTICK
      ELSE
        SH = INT(SMAX/SBTICK)*SBTICK
      ENDIF
      IF( ZMIN.GE.0. ) THEN
        ZL = INT(ZMIN/ZBTICK)*ZBTICK
      ELSE
        ZL = INT(ZMIN/ZBTICK-1.)*ZBTICK
      ENDIF
      ZL = ZL - NZOFF*ZSTICK
      IF( ZMAX.GE.0. ) THEN
        ZH = INT(ZMAX/ZBTICK+1.)*ZBTICK
      ELSE
        ZH = INT(ZMAX/ZBTICK)*ZBTICK
      ENDIF
C
      CALL ERASE
      CALL TWINDO( PLOTW(1), PLOTW(2), PLOTW(3), PLOTW(4) )
      GRAPHW(1) = PLOTW(1) + 300
      GRAPHW(2) = PLOTW(2) - 200
      GRAPHW(3) = PLOTW(3) + 200
      GRAPHW(4) = PLOTW(4) - 1024
      AMPSW(1) = GRAPHW(1)
      AMPSW(2) = GRAPHW(2)
      AMPSW(3) = GRAPHW(4) + 100
      AMPSW(4) = PLOTW(4) - 100
      CALL MOVABS( PLOTW(1), PLOTW(3) )
      CALL DRWABS( PLOTW(2), PLOTW(3) )
      CALL DRWABS( PLOTW(2), PLOTW(4) )
      CALL DRWABS( PLOTW(1), PLOTW(4) )
      CALL DRWABS( PLOTW(1), PLOTW(3) )
C                                           DRAW RUN AND EVENT NUMBER
      NPHEAD = IW(IBLN('HEAD'))
      IF( NPHEAD.GT.0 ) THEN
        WRITE(CLINE,9004)
     *         HW(NPHEAD*2+10), HW(NPHEAD*2+11), ICREC, ITRK, IDATSV
 9004   FORMAT(2I7,I4,' Track',I3,'  DSN=',11A4)
        CALL MOVABS( PLOTW(1)+400, PLOTW(4)-75  )
        CALL EOUTST( 77, CLINE )
        CALL DAY( DATTIM(1), DATTIM(2) )
        WRITE(CLINE,9005) DATTIM
 9005   FORMAT(' JADE z-s and Amplitude Display at ',A8,1X,A8)
        CALL MOVABS( AMPSW(1)+1000, AMPSW(3)-75  )
        CALL EOUTST( 52, CLINE )
      ENDIF
C                                           SET SCALES
      SCALEX = (GRAPHW(2)-GRAPHW(1))/(SH-SL)
      SCALEY = (GRAPHW(4)-GRAPHW(3))/(ZH-ZL)
C                                           DRAW AXES
      CALL MOVABS( IX(SL), IY(ZH) )
      CALL DRWABS( IX(SL), IY(ZL) )
      CALL DRWABS( IX(SH), IY(ZL) )
      NT = (SH-(SL+NSOFF*SSTICK))/SBTICK+1.5
      I0 = IY(ZL) - 20
      I9 = 40
      DO 110 J=1,NT
        S = SL + (J-1)*SBTICK + NSOFF*SSTICK
        CALL MOVABS( IX(S), I0 )
  110   CALL DRWREL( 0, I9 )
      NT = (SH-SL)/SSTICK+1.5
      I0 = IY(ZL) - 10
      I9 = 20
      DO 120 J=1,NT
        S = SL + (J-1)*SSTICK
        CALL MOVABS( IX(S), I0 )
  120   CALL DRWREL( 0, I9 )
      NT = (ZH-(ZL+NZOFF*ZSTICK))/ZBTICK+1.5
      I0 = IX(SL) - 20
      I9 = 40
      DO 130 J=1,NT
        Z = ZL + (J-1)*ZBTICK + NZOFF*ZSTICK
        CALL MOVABS( I0, IY(Z) )
  130   CALL DRWREL( I9, 0 )
      NT = (ZH-ZL)/ZSTICK+1.5
      I0 = IX(SL) - 10
      I9 = 20
      DO 140 J=1,NT
        Z = ZL + (J-1)*ZSTICK
        CALL MOVABS( I0, IY(Z) )
  140   CALL DRWREL( I9, 0 )
C                                           DRAW NUMBERS
      S = SL + NSOFF*SSTICK
      I = S
      WRITE(CNUMB,9121) I
 9121 FORMAT(I5)
      CALL MOVABS( IX(S)-100, IY(ZL)-IYTOFF )
      CALL EOUTST( 5, CNUMB )
      S = SH
      I = S
      WRITE(CNUMB,9121) I
      CALL MOVABS( IX(S)-100, IY(ZL)-IYTOFF )
      CALL EOUTST( 5, CNUMB )
      CLINE = 'S'
      CALL MOVABS( IX(S)-400, IY(ZL)-IYTOFF )
      CALL EOUTST( 1, CLINE )
      Z = ZL + NZOFF*ZSTICK
      I = Z
      WRITE(CNUMB,9121) I
      CALL MOVABS( IX(SL)-250, IY(Z)-20 )
      CALL EOUTST( 5, CNUMB )
      Z = ZH
      I = Z
      WRITE(CNUMB,9121) I
      CALL MOVABS( IX(SL)-250, IY(Z)-20 )
      CALL EOUTST( 5, CNUMB )
      CLINE = 'Z'
      CALL MOVABS( IX(SL)-250, IY(Z)-420 )
      CALL EOUTST( 1, CLINE )
C
C                                           DRAW HITS
      IDS = IX(5.) - IX(0.)
      DZ0 = AZSSAV(NZSPRD)
      NPJETC = IW(IBLN('JETC'))
      DO 1000 J=1,NHIT
        S = RW(NPZSPD+(J-1)*NTRW+3)
        Z = RW(NPZSPD+(J-1)*NTRW+4)
        IF( RW(NPZSPD+(J-1)*NTRW+6).GT.0. ) THEN
          DZ = DZ0/SQRT(RW(NPZSPD+(J-1)*NTRW+6))
        ELSE
          DZ = DZ0
        ENDIF
        FLAG = IW(NPZSPD+(J-1)*NTRW+5)
        IF( FLAG .EQ. 0 ) THEN
          CALL MOVABS( IX(S)-IDS, IY(Z) )
          CALL DRWREL( 2*IDS, 0 )
          CALL MOVABS( IX(S), IY(Z-DZ) )
          CALL DRWABS( IX(S), IY(Z+DZ) )
        ELSE
          CALL MOVABS( IX(S)-IDS, IY(Z-DZ) )
          CALL DRWABS( IX(S)-IDS, IY(Z+DZ) )
          CALL DRWABS( IX(S)+IDS, IY(Z+DZ) )
          CALL DRWABS( IX(S)+IDS, IY(Z-DZ) )
          CALL DRWABS( IX(S)-IDS, IY(Z-DZ) )
        ENDIF
        IF( NPJETC.GT.0 .AND. OWNZ ) THEN
          IP = NPJETC*2+IW(NPZSPD+(J-1)*NTRW+2)
          CALL AMPS2Z( IP, NPJETC, ZOWN, W, IFL )
          IF( ZOWN.GT.ZL .AND. ZOWN.LT.ZH ) THEN
            DZ = DZ0/SQRT(W)
            CALL MOVABS( IX(S)-IDS, IY(ZOWN   ) )
            CALL DRWABS( IX(S)    , IY(ZOWN+DZ) )
            CALL DRWABS( IX(S)+IDS, IY(ZOWN   ) )
            CALL DRWABS( IX(S)    , IY(ZOWN-DZ) )
            CALL DRWABS( IX(S)-IDS, IY(ZOWN   ) )
          ENDIF
        ENDIF
 1000 CONTINUE
C
      NPPATR = IW(IBLN('PATR'))
      IF( NPPATR.GT.0 ) THEN
        L0 = IW(NPPATR+1)
        L1 = IW(NPPATR+3)
        NTRK = IW(NPPATR+2)
        IF( 0.LT.ITRK .AND. ITRK .LE. NTRK ) THEN
          DZDS = RW(NPPATR+L0+(ITRK-1)*L1+30)
C         Z0   = RW(NPPATR+L0+(ITRK-1)*L1+31)
          Z0   = RW(NPPATR+L0+(ITRK-1)*L1+ 7)
          S1 = SMIN
          Z1 = Z0 + S1*DZDS
          IF( Z1.LT.ZL ) THEN
            S1 = (ZL-Z0)/DZDS
            Z1 = ZL
          ELSEIF( Z1.GT.ZH ) THEN
            S1 = (ZH-Z0)/DZDS
            Z1 = ZH
          ENDIF
          S9 = SMAX
          Z9 = Z0 + S9*DZDS
          IF( Z9.GT.ZH ) THEN
            S9 = (ZH-Z0)/DZDS
            Z9 = ZH
          ELSEIF( Z9.LT.ZL ) THEN
            S9 = (ZL-Z0)/DZDS
            Z9 = ZL
          ENDIF
          CALL MOVABS( IX(S1), IY(Z1) )
          CALL DRWABS( IX(S9), IY(Z9) )
        ENDIF
      ENDIF
C                                           DRAW AMPLITUDES
C                                           FIND MAXIMUM
      NPJETC = IW(IBLN('JETC'))
      IF( NPJETC.GT.0 ) THEN
C                                           FIND MAX AMPLITUDE
        IMAXA = 0
        DO 1700 J=1,NHIT
          IAR = HW(NPJETC*2+IW(NPZSPD+(J-1)*NTRW+2)+1)
          IAL = HW(NPJETC*2+IW(NPZSPD+(J-1)*NTRW+2)+2)
          IF( IAR .GT. IMAXA ) IMAXA = IAR
          IF( IAL .GT. IMAXA ) IMAXA = IAL
 1700   CONTINUE
        IAH = INT(IMAXA/ABTICK+1.)*ABTICK+.5
        SCALAY = FLOAT(AMPSW(4)-AMPSW(3))/AMAX0(IAH,1)
C                                           DRAW AMPLITUDE AXES
        CALL MOVABS( IX(SL), IA(IAH) )
        CALL DRWABS( IX(SL), IA(0) )
        CALL DRWABS( IX(SH), IA(0) )
        CLINE = 'Charge'
        CALL MOVABS( IX(SL)-250, IA(IAH)-IYTOFF )
        CALL EOUTST( 6, CLINE )
C                                           DRAW TICKS
        NT = ( IAH-NAOFF*ASTICK ) / ABTICK + 1.5
        I0 = IX(SL) - 20
        I9 = 40
        DO 1710 J=1,NT
          IAL = (J-1)*ABTICK + NAOFF*ASTICK + .5
          CALL MOVABS( I0, IA(IAL) )
 1710     CALL DRWREL( I9, 0 )
        NT = IAH / ASTICK + .5
        I0 = IX(SL) - 10
        I9 = 20
        DO 1720 J=1,NT
          IAL = (J-1)*ASTICK + .5
          CALL MOVABS( I0, IA(IAL) )
 1720     CALL DRWREL( I9, 0 )
        NT = (SH-(SL+NSOFF*SSTICK))/SBTICK+1.5
        I0 = IA(0) - 20
        I9 = 40
        DO 1730 J=1,NT
          S = SL + (J-1)*SBTICK + NSOFF*SSTICK
          CALL MOVABS( IX(S), I0 )
 1730     CALL DRWREL( 0, I9 )
        NT = (SH-SL)/SSTICK+1.5
        I0 = IA(0) - 10
        I9 = 20
        DO 1740 J=1,NT
          S = SL + (J-1)*SSTICK
          CALL MOVABS( IX(S), I0 )
 1740     CALL DRWREL( 0, I9 )
        WRITE(CNUMB,9121) IAH
        CALL MOVABS( IX(SL)-250, IA(IAH)-20 )
        CALL EOUTST( 5, CNUMB )
        IDS = IDS/2
        DO 1810 J=1,NHIT
          S = RW(NPZSPD+(J-1)*NTRW+3)
          IAR = HW(NPJETC*2+IW(NPZSPD+(J-1)*NTRW+2)+1)
          CALL MOVABS( IX(S)-IDS, IA( 0 ) )
          CALL DRWABS( IX(S)-IDS, IA(IAR) )
          CALL DRWABS( IX(S)    , IA(IAR) )
          CALL DRWABS( IX(S)    , IA( 0 ) )
          IAL = HW(NPJETC*2+IW(NPZSPD+(J-1)*NTRW+2)+2)
          CALL DRWABS( IX(S)    , IA(IAL) )
          CALL DRWABS( IX(S)+IDS, IA(IAL) )
          CALL DRWABS( IX(S)+IDS, IA( 0 ) )
 1810   CONTINUE
      ENDIF
C                                           ENTER EDITOR
      IF( MODEGA .EQ. 1 ) THEN
        CALL CLEAR
        LHELP = .TRUE.
      ELSE
        CALL TRMOUT( 80,
     +   ' Enter track number or Character or HELP^')
        LHELP = .FALSE.
      ENDIF
 2000 CONTINUE
      IF( LHELP ) THEN
        CTOG = 'ON '
        IF( .NOT. OWNZ ) CTOG = 'OFF'
        CLINE = 'Z-S FIT PACKAGE^'
        CALL TRMOUT( 80, CLINE )
        CLINE = '---------------^'
        CALL TRMOUT( 80, CLINE )
        CLINE = ' Track Level:^'
        CALL TRMOUT( 80, CLINE )
        CLINE = ' ^'
        CALL TRMOUT( 80, CLINE )
        CLINE = '   Select Option by entering Leading Character,^'
        CALL TRMOUT( 80, CLINE )
        CLINE = '   enter next track number or hit RETURN:^'
        CALL TRMOUT( 80, CLINE )
        CLINE = '     HardCopy.......................^'
        CALL TRMOUT( 80, CLINE )
        WRITE(CLINE,9201) DEST
 9201   FORMAT('     Set DEST (',A8,')............^')
        CALL TRMOUT( 80, CLINE )
        CLINE = '     Dump Data......................^'
        CALL TRMOUT( 80, CLINE )
        WRITE(CLINE,9205) CTOG
 9205   FORMAT('     with standard Z (AMPS2Z)?.(',A3,')^')
        CALL TRMOUT( 80, CLINE )
        CLINE = '     Cursor Hit.....................^'
        CALL TRMOUT( 80, CLINE )
        CLINE = '     Legend.........................^'
        CALL TRMOUT( 80, CLINE )
      ENDIF
 2001 CALL TRMIN(50,CNUMB)
      ANSWER = CNUMB(1:1)
      CHELP = CNUMB(1:4)
      IF( CHELP .EQ. 'HELP' ) THEN
        LHELP = .TRUE.
        GO TO 2000
      ENDIF
C
      LHELP = MODEGA .EQ. 1
      IF( ANSWER .EQ. 'h' .OR. ANSWER .EQ. 'H' ) THEN
        CALL HDCDST( DEST )
        LHELP = .FALSE.
        GO TO 2000
      ELSEIF( ANSWER .EQ. 'l' .OR. ANSWER .EQ. 'L' ) THEN
        CLINE =  'Position Cursor to add legend^'
        CALL TRMOUT( 80, CLINE )
        CALL SCURSR( ICHR, IXP0, IYP0 )
        IXP = IXP0
        IYP = IYP0
        IDS = 20
        CALL MOVABS( IXP-IDS, IYP )
        CALL DRWREL( 2*IDS, 0 )
        CALL MOVABS( IXP, IYP-IDS )
        CALL DRWREL( 0, 2*IDS )
        CLINE= 'Associated Hits'
        CALL MOVABS( IXP+40, IYP-15 )
        CALL EOUTST( 15, CLINE )
        IYP = IYP - 70
        CALL MOVABS( IXP-IDS, IYP-IDS )
        CALL DRWABS( IXP-IDS, IYP+IDS )
        CALL DRWABS( IXP+IDS, IYP+IDS )
        CALL DRWABS( IXP+IDS, IYP-IDS )
        CALL DRWABS( IXP-IDS, IYP-IDS )
        CLINE= 'Rejected Hits'
        CALL MOVABS( IXP+40, IYP-15 )
        CALL EOUTST( 13, CLINE )
        IF( OWNZ ) THEN
          IYP = IYP - 70
          CALL MOVABS( IXP-IDS, IYP     )
          CALL DRWABS( IXP    , IYP+IDS )
          CALL DRWABS( IXP+IDS, IYP     )
          CALL DRWABS( IXP    , IYP-IDS )
          CALL DRWABS( IXP-IDS, IYP     )
          CLINE= 'Standard z'
          CALL MOVABS( IXP+40, IYP-15 )
          CALL EOUTST( 10, CLINE )
        ENDIF
        IF( MODEGA.EQ.1 ) CALL CLEAR
        GO TO 2000
      ELSEIF( ANSWER .EQ. 'w' .OR. ANSWER .EQ. 'W' ) THEN
        OWNZ = .NOT. OWNZ
        IF( MODEGA.EQ.1 ) CALL CLEAR
        GO TO 2000
      ELSEIF( ANSWER .EQ. 's' .OR. ANSWER .EQ. 'S' ) THEN
        CLINE = ' Enter Destination^'
        CALL TRMOUT( 80, CLINE )
        DEST = ' '
        CALL TRMIN( 8, DEST )
        IF( .NOT. (
     *        DEST .EQ. 'L1      '   .OR.
     *        DEST .EQ. 'L2      '   .OR.
     *        DEST .EQ. 'L3      '   .OR.
     *        DEST .EQ. 'L4      '   .OR.
     *        DEST .EQ. 'EXT     '   ) ) THEN
          WRITE(CLINE,9204) DEST
 9204     FORMAT(' Unknown Destination:',A8,' L1 will be assumed^')
          CALL TRMOUT( 80, CLINE )
          DEST = 'L1      '
        ENDIF
        IF( MODEGA.EQ.1 ) CALL CLEAR
        GO TO 2000
      ELSEIF( ANSWER .EQ. 'c' .OR. ANSWER .EQ. 'C' ) THEN
        IF( LHELP ) THEN
          WRITE(6,9202)
 9202     FORMAT(' The Cursor ( Haircross ) is used to obtain'/
     *     ' additional information on specified hits:'/
     *     ' The horizontal coordinate of the cross is used to find',
     *     ' the closest hit,'/
     *     ' the position of the cross is used to place the text',
     *     ' in format:'/
     *   '   Hit Multiplicity      Wire Number    Quality Code(0=good)'/
     *     '        and then for each hit'/
     *     '   Amplitude left       Amplitude Right    Time'//
     *     '   Enter Cursor Position on Graphics Screen')
        ENDIF
        CALL SCURSR( ICHR, IXP, IYP )
        IXP0 = IXP
        IYP0 = IYP
        I = 0
        IDIST = 99999
        DO 2100 J=1,NHIT
          IDEL = IABS(IX(RW(NPZSPD+(J-1)*NTRW+3))-IXP)
          IF( IDEL .LT. IDIST ) THEN
            IDIST = IDEL
            I = J
          ENDIF
 2100   CONTINUE
        IF( I.GT.0 ) THEN
          NPJETC = IW(IBLN('JETC'))
          IF( NPJETC.GT. 0 ) THEN
            IP0 = NPJETC*2+IW(NPZSPD+(I-1)*NTRW+2)
            NPJET1 = IW(NPJETC-1)
            IF( NPJET1.GT. 0 ) THEN
              IP1 = NPJET1*2+IW(NPZSPD+(I-1)*NTRW+2)
              NW = HW(IP0)/8
              MULT = 1
              IP = IP0 - 4
 2110         IF( IP.GT.NPJETC*2+100 .AND. HW(IP)/8.EQ.NW ) THEN
                IP = IP - 4
                MULT = MULT + 1
                GO TO 2110
              ENDIF
              IP = IP0 + 4
 2120         IF( IP.LT.NPJETC*2+HW(NPJETC*2+99)+100 .AND.
     *                         HW(IP)/8.EQ.NW ) THEN
                IP = IP + 4
                MULT = MULT + 1
                GO TO 2120
              ENDIF
              WRITE(CLINE,9162) MULT, NW, IW(NPZSPD+(I-1)*NTRW+5)
              CALL MOVABS( IXP, IYP )
              CALL EOUTST( 18, CLINE )
              WRITE(CLINE,9162) HW(IP0+1), HW(IP0+2), HW(IP0+3)
 9162         FORMAT(3I6)
              IYP = IYP - 50
              CALL MOVABS( IXP, IYP )
              CALL EOUTST( 18, CLINE )
              IP = IP0 - 4
 2130         IF( IP.GT.NPJETC*2+100 .AND. HW(IP)/8.EQ.NW ) THEN
                IYP = IYP - 50
                WRITE(CLINE,9162) HW(IP+1), HW(IP+2), HW(IP+3)
                CALL MOVABS( IXP, IYP )
                CALL EOUTST( 18, CLINE )
                IP = IP - 4
                GO TO 2130
              ENDIF
              IP = IP0 + 4
 2140         IF( IP.LT.NPJETC*2+HW(NPJETC*2+99)+100 .AND.
     *                         HW(IP)/8.EQ.NW ) THEN
                IYP = IYP - 50
                WRITE(CLINE,9162) HW(IP+1), HW(IP+2), HW(IP+3)
                CALL MOVABS( IXP, IYP )
                CALL EOUTST( 18, CLINE )
                IP = IP + 4
                GO TO 2140
              ENDIF
              CALL MOVABS( IXP0, IYP0-50 )
              CALL DSHABS( IX(RW(NPZSPD+(I-1)*NTRW+3)),
     *                       IY(RW(NPZSPD+(I-1)*NTRW+4)), 1 )
            ENDIF
          ENDIF
        ENDIF
        IF( MODEGA.EQ.1 ) CALL CLEAR
        GO TO 2000
      ELSEIF( ANSWER .EQ. 'd' .OR. ANSWER .EQ. 'D' ) THEN
        NPJETC = IW(IBLN('JETC'))
        WRITE(CLINE,9152)
 9152   FORMAT(' HIT JETC POI    s       z     Flag      Weight',
     *           '    z      Flag   Weight')
        CALL TRMOUT( 80, CLINE )
        DO 7900 J=NPZSPD+1,NPZSPD+IW(NPZSPD)-1,NTRW
          I = (J-(NPZSPD+1))/NTRW+1
          IF( NPJETC.GT.0 ) THEN
            IP = NPJETC*2+IW(NPZSPD+(I-1)*NTRW+2)
            CALL AMPS2Z( IP, NPJETC, ZOWN, W, IFL )
          ELSE
            ZOWN = 0.
            W = 0.
            IFL = 0
          ENDIF
          WRITE(CLINE,9151)  I,
     *                   IW(J+1), RW(J+2), RW(J+3), IW(J+4), RW(J+5),
     *                   ZOWN, IFL, W
 9151     FORMAT(I6,I7,2F8.2,I6,E12.3,F8.2,I6,E12.3,'^')
          CALL TRMOUT( 80, CLINE )
 7900   CONTINUE
        IF( MODEGA.EQ.1 ) CALL CLEAR
        GO TO 2000
      ELSEIF( ICHAR(ANSWER) .GT. ICHAR(ZERO) .AND.
     *          ICHAR(ANSWER) .LE. ICHAR(NINE) ) THEN
        READ(CNUMB,*) ITRK
        ITRK = -ITRK
        RETURN
      ENDIF
 8000 CONTINUE
      END
