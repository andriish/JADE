C   29/04/87 803181325  MEMBER NAME  ZSFIT    (JADEGS)      FORTRAN
C   14/03/88 803141331  MEMBER NAME  ZSFIT    (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE ZSFIT(MDE,DUM1Y,DUM2Y,DUM3Y,MDEH,nargs) ! PMF 03/12/99: 6th argument added
C-----------------------------------------------------------------------
C
C------- METHOD: P.DITTMANN ------ LAST CHANGE: 09.11.87  E.ELSEN  -C
C                                 CHANGE 19.1.84 KILL PRINT IN JADEZC
C                    Z - S   F I T                        J.O.      C
C        MAIN STEERING ROUTINE FOR Z-S PACKAGE.                     C
C        COLLECTION OF ALL HITS OF A CERTAIN TRACK,                 C
C        CALIBRATION OF AMPLITUDES AND CHANGE OF JETC BANK (ZRECAL),C
C        REMOVAL OF HITS IN TRACK OVERLAP REGIONS IN R/PHI AND      C
C        CALCULATION OF X-Y COORDINATES (ZSPUR,ZSXY), CALCULATION   C
C        OF Z-S COORDINATES AND WEIGHT, STRAIT LINE FIT (ZSLINE),   C
C        UPDATE PATR BANK AND JHTL BANK.                            C
C                                                                   C
C        MODE = 0   DOES EVERYTHING                                 C
C             = 1   CALIBRATION ONLY                                C
C             = 2   CALIBRATION AND FIT ONLY                        C
C             = 3   = MODE 0 WITHOUT UPDATING THE 'PATR' BANK       C
C             = 4   = MODE 2 WITHOUT UPDATING THE 'PATR' BANK       C
C             + 8   AS ABOVE + CREATION OF JHTQ BANK                C
C    MODIFIED 09/11/87   E ELSEN
C      ROUTINE NOW ALSO CALLABLE FOR MC
C    MODIFIED 11/06/87   E ELSEN
C      OVERFLOW HITS REJECTED IN AMPS2Z WITH CODE 32
C      ADDED CREATION OF BANK JHTQ, WHICH TRANSFERS THE HIT
C      QUALITY FLAG TO THE DEDX PROGRAMS.
C      ADDED ZSPDBK CALL IF FLAG IS SET
C    MODIFIED 29/04/87   E ELSEN
C      ALL CONSTANTS PUT INTO BLOCKDATA ATTACHED TO AMPS2Z
C      NEW CONSTANTS PROVIDED FOR FADC DATA
C      AMPS2Z CALLED
C    MODIFIED 09/11/87   E ELSEN
C      FIT NOW ALSO PERFORMED FOR MC
C    MODIFIED 14/03/88   E ELSEN
C      SINCE JETC HIT COUNTING NOT IN EFFECT FOR MC
C      RECODE LH EXPRESSION
C-------------------------------------------------------------------C
#include "cdata.for"
#include "cgraph.for"
      INTEGER*2 HDATA
C-----------
      COMMON/CALIBR/JPOINT(1)
      DIMENSION ACALIB(1)
      EQUIVALENCE (JPOINT(1),ACALIB(1))
C-----------
      COMMON /CZSCAL/ IPVERS,ZAL,RESFAC,SECH(5)
C----*----------------TCORR(4,96),XTALK(2,96),PAR(7,1536)-------------
      COMMON /CWORK/ NHIT,IZB(60),IJC(60),IJH(60),NW(60),LH(60),
     *                LRFL(60),ALU(60),ARU(60),X(60),Y(60),Z(60),S(60),
     *                G(60),DT(60),DL(60),DW(60),FCORR(60)
C                                           DEDX-FLAGS
      LOGICAL LWDEDX, MC
      INTEGER DEDXFL(60)
      INTEGER JHTQBT / 2 /, ONE / 1 /
      INTEGER IPRJQ / 0 /, IPRJQM / 10 /
C
#include "czsprm.for"
      DIMENSION IC(6)
      LOGICAL TBIT
      DATA MTRNO1/Z FE0000/, MTRNO2/Z FE/
      DATA ICALL/0/
C------------------- 11.9.85
      MODE=MDE
      MODEH=MDEH
C---------------------------
      MODEHH=MODE
      ICALL =ICALL+1
      IJETC = IDATA( IBLN('JETC') )
      IJETU = 0
      IF( IJETC.GT.0 ) IJETU = IDATA( IJETC - 1 )
      IJHTL = IDATA( IBLN('JHTL') )
      IPATR = IDATA( IBLN('PATR') )
      IX    = 2*IDATA( IBLN('HEAD') )
      IRUN = 0
      IF( IX.GT.0 ) IRUN=HDATA(IX+10)
C                                           MONTE CARLO
      MC = IRUN.LT.100
C
      IF(IJETC.EQ.0
     +   .OR. (IJETU.EQ.0 .AND. .NOT. MC )
     +   .OR. IJHTL.EQ.0 .OR. IPATR.EQ.0) GOTO 80
C
***PMF03/12/99      CALL NOARG( NARGS )
* NOARG does not work properly on the present platform.
* Introduce 6th argument NARGS for subroutine ZSFIT instead.
      IF (MODE.GT.10.AND.NARGS.GT.4 ) MODE=MODEH
      LWDEDX = LAND( MODE, 8 )
***PMF03/12/99      IF( LWDEDX ) MODE = MODE - 8
* On some plattforms, the logical .TRUE. is represented
* by the FIRST bit set to 1, and the status of the
* other bits is ignored!!! In this case, e.g. the byte
* 00000110 represents the logical value .FALSE. , in contrast
* to what here is expected. Thus handle LWDEDX as INTEGER
* and test if any of the bits is set.
      IF( LWDEDX.NE.0 ) MODE = MODE - 8
      IF (MODE.LT.0 .OR. MODE.GT.4) GOTO 86
      GOTO 60
C
   70 IFLAG=0
      IF (MODE.EQ.3) GOTO 83
      IF (MODE.EQ.4) GOTO 84
C
   85 CONTINUE
      IF( MC ) GO TO 91
        IPN = JPOINT(11)
        IPVERS=JPOINT(IPN+1)
        IF(ICALL.EQ.1 .AND. NDDINN .EQ. 0 ) PRINT 901, IPVERS,MODE
  901 FORMAT(/'  ***********     Z - RECALIBRATION  VERSION',I8,'     ',
     +'     MODE',I5/)
        ZAL = 1400.
        NRUN=HDATA(IX+10)
        GO TO 92
   91 CONTINUE
C                                           SET TO ZERO TO SIGNAL AMPS2Z
        ZAL = 0.
C                                           FIND EQUIV RUN NUMBER
        NRUN = IZT2RN(DUMMY)
   92 CONTINUE
C                                           PERIOD
      NZSPRD = 1
      IF( NRUN .GT.  6000 ) NZSPRD = 2
      IF( NRUN .GT. 24200 ) NZSPRD = 3
      RESFAC = AZSRSA(NZSPRD)
      DO 34 I=1,5
   34 SECH(I) = AZSSHT(I,NZSPRD)
C
      IJETC2 = IJETC*2
      IJETU2 = IJETU*2
      NT = IDATA(IPATR+2)
      IF( NT.LE.0 .OR. NT.GT.60 ) GOTO 88
      IF( .NOT. LWDEDX ) GO TO 43
C                                           HITS/WORD
        NHPW = 32/JHTQBT
        LJHTQ = (IDATA(IJHTL)-1+NHPW-1)/NHPW + 1
        CALL BCRE( NPJHTQ, 'JHTQ', IDATA(IJHTL-2),
     *                             LJHTQ, *41, IERJHT )
        CALL BSAW( 1, 'JHTQ' )
        IDATA(NPJHTQ+1) = JHTQBT
C                                           CREATION OF BANK MAY HAVE
C                                           MOVED POINTERS
        IJETC = IDATA( IBLN('JETC') )
        IJETU = 0
        IF( IJETC.GT.0 ) IJETU = IDATA( IJETC - 1 )
        IJHTL = IDATA( IBLN('JHTL') )
        IPATR = IDATA( IBLN('PATR') )
        GO TO 43
   41 CONTINUE
        LWDEDX = .FALSE.
        IF( IPRJQ .GT. IPRJQM ) GO TO 43
          IPRJQ = IPRJQ + 1
          WRITE(6,9101) IERJHT
 9101     FORMAT ('  ***** ERROR IN ZSFIT: NOT ENOUGH SPACE FOR JHTQ',
     *            ' IER=',I5)
   43 CONTINUE
C                                           LOOP OVER ALL TRACKS
      L0 = IDATA(IPATR+1)
      LT = IDATA(IPATR+3)
      KT = IPATR + L0
      DO 59 I=1,NT
        NHIT = 0
        NHF = 0
        IF(ADATA(KT+25).EQ.0.) GOTO 59
        R = -1. / ADATA(KT+25)
        SR = SIGN(1.,R)
        PT = SQRT(ADATA(KT+8)**2+ADATA(KT+9)**2)
        CPHI = ADATA(KT+8) / PT
        SPHI = ADATA(KT+9) / PT
        X0 = ADATA(KT+5)
        Y0 = ADATA(KT+6)
        R0 = SQRT(X0**2+Y0**2)
        XE = ADATA(KT+12)
        YE = ADATA(KT+13)
        SE = R * ATAN2(SR*(CPHI*(XE-X0)+SPHI*(YE-Y0)),
     *                 SR*(SPHI*(XE-X0)-CPHI*(YE-Y0)+R))
        IF(ABS(R).GT.1.E5) SE=SQRT((XE-X0)**2+(YE-Y0)**2)
C                                           COLLECT ALL HITS OF TRACK I
C
C                                           ORDER CELLS
        NC = 0
        DO 12 J=1,6
          KC = IDATA(KT+40-J)
          IF(KC.LE.0 .OR. KC.GT.96) GOTO 12
          IF(NC.EQ.0) GOTO 11
          IF(KC.GT.IC(NC)) GOTO 11
          DO 10 K=1,NC
          IF(KC.GT.IC(K)) GOTO 10
          IF(KC.EQ.IC(K)) GOTO 12
          JC = IC(K)
          IC(K) = KC
          KC = JC
   10     CONTINUE
   11     NC = NC + 1
          IC(NC) = KC
   12   CONTINUE
C                                           LOOP OVER CELLS
        IM2 = I*2
        IM1 = IM2*65536
        DO 19 J=1,NC
          NCP = IJETC2 + IC(J) + 2
          NHCELL = (HDATA(NCP+1)-HDATA(NCP))/4
          KHCELL = (HDATA(NCP)-1)/4
C                                           LOOP OVER HITS
          DO 18 K=1,NHCELL
            KA = IJHTL + KHCELL + K + 1
            IF(IDATA(KA).EQ.0) GOTO 18
              LRFLAG = -1
              IMH=LAND(IDATA(KA),MTRNO1)
              IF(IMH.NE.IM1) GOTO 13
                IF(TBIT(IDATA(KA), 5)) GOTO 18
                IF(TBIT(IDATA(KA), 7)) LRFLAG=1
                KABIT = 15
                GOTO 15
   13       IMH=LAND(IDATA(KA),MTRNO2)
            IF(IMH.NE.IM2) GOTO 18
            IF(TBIT(IDATA(KA),21)) GOTO 18
            IF(TBIT(IDATA(KA),23)) LRFLAG=1
            KABIT = 31
C              HIT FOUND IN CELL IC(J), TRACK I
   15       IF(NHIT.EQ.60) GOTO 19
            NHIT = NHIT + 1
            NH1 = IJETC2 + (KHCELL+K-1)*4 + 101
            IJC(NHIT) = NH1
            IJH(NHIT) = KA
            IF(KABIT.EQ.15) IJH(NHIT)=-IJH(NHIT)
            LRFL(NHIT) = LRFLAG
            MHIT = HDATA(NH1)
            NW(NHIT) = MHIT / 8
C                                           FOLLOWING MOD TO HAVE
C                                           HIT COUNTING WORK FOR
C                                           MC AS WELL.
CCCCCC      LH(NHIT) = LAND(MHIT,7) + 1
            LH(NHIT) = 1
            II = NH1 + 4
   16       IF( II.GE.IJETC2+100+HDATA(IJETC2+99) .OR.
     *          NW(NHIT).NE.HDATA(II)/8 ) GO TO 17
              LH(NHIT) = LH(NHIT) + 1
              II = II + 4
              GO TO 16
   17       CONTINUE
C                                           END MOD
            IZB(NHIT) = LH(NHIT)
            NHF=NHF+1
   18     CONTINUE
   19   CONTINUE
C                                             CALIBRATE AMPLITUDES
C                                             AND UPDATE JETC BANK
        IF( .NOT. MC ) CALL ZRECAL(IJETU,IJETC)
        IF( MODE.EQ.1 .AND. .NOT. LWDEDX ) GOTO 59
C                                             X-Y COORDINATES AND
C                                             HIT CLEANING
        CALL ZSPUR(I,IPATR,MODE)
C
C                                            LOOP OVER ALL HITS OF TRACK
        NHF = 0
        DO 29 J=1,NHIT
          DEDXFL(J) = 0
          IF(IZB(J).EQ.0) GOTO 29
          NH1 = IJC(J)
C                                             CHECK AMPLITUDES
C                                             CALCULATE Z
          CALL AMPS2Z( NH1, IJETC, Z(J), G(J), IZGOOD )
C                                             PREVIOUS AMPLITUDE
          IF(LH(J).LT.2) GOTO 24
            IF(HDATA(NH1+5).LT.8 .OR. HDATA(NH1+6).LT.8) IZGOOD = 16
C                                             POINT IN Z-S
   24     CONTINUE
          IF( IZGOOD .NE.0 ) GO TO 26
            THETA = ATAN2(SR*(CPHI*(X(J)-X0)+SPHI*(Y(J)-Y0)),
     *                    SR*(SPHI*(X(J)-X0)-CPHI*(Y(J)-Y0)+R))
            S(J) = R * THETA
            IF(ABS(R).GT.1.E5) S(J)=SQRT((X(J)-X0)**2+(Y(J)-Y0)**2)
            NHF = NHF + 1
            GOTO 27
   26   IZB(J) = 0
        IF( IZGOOD .EQ. 32 ) DEDXFL(J) = 32
   27   CONTINUE
   29   CONTINUE
C                                             STRAIGHT LINE FIT
        CALL ZSLINE(NHZ,Z0,TANL,SIGZ,ZMIN,ZMAX)
C
        IF (NHZ.LT.3) GOTO 59
        IF (MODE.EQ.1) GOTO 55
C
          IF (IFLAG.EQ.1) GOTO 51
C                                             UPDATE PATR BANK
            IDATA(KT+44) = 3
            ADATA(KT+ 7) = Z0
            COSL = 1./SQRT(1.+TANL**2)
            ADATA(KT+ 8) = CPHI*COSL
            ADATA(KT+ 9) = SPHI*COSL
            ADATA(KT+10) = TANL*COSL
            ADATA(KT+14) = Z0 + SE*TANL
            PTL = SQRT(1.-ADATA(KT+17)**2)
            ADATA(KT+15) = ADATA(KT+15)*COSL/PTL
            ADATA(KT+16) = ADATA(KT+16)*COSL/PTL
            ADATA(KT+17) = ADATA(KT+10)
            ADATA(KT+30) = TANL
            ADATA(KT+31) = Z0 - R0*TANL
            ADATA(KT+32) = SIGZ
            IDATA(KT+33) = NHZ
            ADATA(KT+45) = ZMIN
            ADATA(KT+46) = ZMAX
   51     CONTINUE
C                                             UPDATE JHTL BANK
          DO 52 J=1,NHIT
            KA = IABS(IJH(J))
            KABIT = 31
            IF(IJH(J).LT.0) KABIT=15
            IZBIT = 0
            IF(IZB(J).NE.0) IZBIT=1
            CALL MVB(IDATA(KA),KABIT,IZBIT,31,1)
   52     CONTINUE
C
   55     CONTINUE
          IF( .NOT. LWDEDX ) GO TO 58
            DO 57 J=1,NHIT
              KA = IJH(J)
              NSHIFT = 0
              IF( KA.GT.0 ) GO TO 56
                KA = -KA
                NSHIFT = 1
   56         KA = KA - IJHTL - 1
              IF( DEDXFL(J) .NE. 32 ) DEDXFL(J) = IZB(J)
              IF(DEDXFL(J).EQ.0) GO TO 57
                NRW = (KA-1)/NHPW + 1
                NQW = NPJHTQ + 1 + NRW
                NSHIFT = MOD(KA-1,NHPW)*JHTQBT + NSHIFT
                IDATA(NQW) = LOR(IDATA(NQW),ISHFTL(ONE,NSHIFT))
   57       CONTINUE
C
   58     CONTINUE
C
C                                            FILL ZSPD BANK FOR GRAPHICS
          IF( LZSPDF ) CALL ZSPDBK( I )
C
   59 KT = KT + LT
C
      HDATA(IJETC2+2)=IPVERS
      RETURN
C
   60 CONTINUE
      DO 61 II=1,60
        IZB(II)=0
        IJC(II)=0
        IJH(II)=0
        NW(II)=0
        LH(II)=0
        LRFL(II)=0
        ALU(II)=0.
        ARU(II)=0.
        X(II)=0.
        Y(II)=0.
        Z(II)=0.
        S(II)=0.
        G(II)=0.
        DT(II)=0.
        DL(II)=0.
        DW(II)=0.
        FCORR(II)=0.
   61 CONTINUE
      GOTO 70
C
   83 MODE=0
      IFLAG=1
      GOTO 85
C
   84 MODE=2
      IFLAG=1
      GOTO 85
C
   80 PRINT 81
   81 FORMAT ('  ***** ERROR IN ZSFIT: BANK POINTER = 0')
      RETURN
C
   86 PRINT 87
   87 FORMAT ('  ***** ERROR IN ZSFIT: MODE NOT ALLOWED')
      RETURN
C
   88 PRINT 89
   89 FORMAT ('  ***** ERROR IN ZSFIT: # OF TRACKS < 0 OR > 60')
      RETURN
      END
