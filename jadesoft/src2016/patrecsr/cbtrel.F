      SUBROUTINE CBTREL(IPR1,IPR2)
      IMPLICIT INTEGER*2 (H)
      LOGICAL TBIT
#include "cworkpr.for"
#include "cworkeq.for"
      DIMENSION IPC(12), IAUX(8),RAUX(8),HAUX(2,8)
      EQUIVALENCE (IAUX(1),HAUX(1,1))
      INTEGER*2 HZW(2)
      EQUIVALENCE (HZW(1),IZW,HZW1) , (HZW(2),HZW2)
      COMMON /CPTSLM/ BKTRLM(20)
                      INTEGER LMBKTR(20)
                      REAL CBTRLM(10)
                      EQUIVALENCE (BKTRLM(11),LMBKTR(11),CBTRLM(1))
      IPTR0 = HPTE0
      IPTR9 = HPTE9
      LDTRK = HLDTE
      NPR  = 0
      NPR9 = MIN0(IPR2,100)
      IF(IPR1.EQ.0) NPR9 = 0
      DO 13000 IPTR = IPTR0,IPTR9,LDTRK
        ICELL = IWRK(IPTR   )
        LBTREL= IWRK(IPTR+19)
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
        RMSMIN = 1000.
        ALGHT1 = 0.
        ALGHT2 = 200.
      ASSIGN 17003 TO IZZZ02
      GOTO 17002
17003 CONTINUE
      IF(
     - RMSMIN.GT.CBTRLM(4)
     -)THEN
          IPC( 5) = 0
          IPC( 6) = 0
          IPC( 7) = 0
          IPC( 8) = 0
          IPC( 9) = IPC( 1)
          IPC(10) = IPC( 2)
          IPC(11) = IPC( 3)
          IPC(12) = IPC( 4)
          ALGHT2 = 400.
      ASSIGN 17004 TO IZZZ02
      GOTO 17002
17004 CONTINUE
      ENDIF
13000 CONTINUE
13001 CONTINUE
      ASSIGN 17006 TO IZZZ03
      GOTO 17005
17006 CONTINUE
      RETURN
17000 CONTINUE
      IF(
     - ICELL.LE.24
     -)THEN
      IF(
     - ICELL.EQ.1
     -)THEN
            IPC( 5) = IPTR0
            IPC( 6) = IPTR0 + (HNTCEL( 3)-2)*LDTRK
            IPC( 7) = IPTR0 + (HNTCEL(24)-1)*LDTRK
            IPC( 8) = IPTR0 + (HNTCEL(25)-2)*LDTRK
            IPC( 9) = IPTR0 + (HNTCEL(25)-1)*LDTRK
            IPC(10) = IPTR0 + (HNTCEL(27)-2)*LDTRK
            IPC(11) = IPTR0 + (HNTCEL(48)-1)*LDTRK
            IPC(12) = IPTR0 + (HNTCEL(49)-2)*LDTRK
            IPC( 1) = IPTR0 + (HNTCEL(49)-1)*LDTRK
            IPC( 2) = IPTR0 + (HNTCEL(53)-2)*LDTRK
            IPC( 3) = IPTR0 + (HNTCEL(95)-1)*LDTRK
            IPC( 4) = IPTR0 + (HNTCEL(97)-2)*LDTRK
      ELSE
      IF(
     - ICELL.LT.24
     -)THEN
            IPC( 5) = IPTR0 + (HNTCEL(ICELL- 1)-1)*LDTRK
            IPC( 6) = IPTR0 + (HNTCEL(ICELL+ 2)-2)*LDTRK
            IPC( 7) = 0
            IPC( 8) = 0
            IPC( 9) = IPTR0 + (HNTCEL(ICELL+23)-1)*LDTRK
            IPC(10) = IPTR0 + (HNTCEL(ICELL+26)-2)*LDTRK
            IPC(11) = 0
            IPC(12) = 0
            ICLL3 = ICELL*2 + 46
            IPC( 1) = IPTR0 + (HNTCEL(ICLL3-1)-1)*LDTRK
            IPC( 2) = IPTR0 + (HNTCEL(ICLL3+5)-2)*LDTRK
            IPC( 3) = 0
            IPC( 4) = 0
      ELSE
            IPC( 5) = IPTR0 + (HNTCEL(23)-1)*LDTRK
            IPC( 6) = IPTR0 + (HNTCEL(25)-2)*LDTRK
            IPC( 7) = IPTR0
            IPC( 8) = IPTR0 + (HNTCEL( 2)-2)*LDTRK
            IPC( 9) = IPTR0 + (HNTCEL(47)-1)*LDTRK
            IPC(10) = IPTR0 + (HNTCEL(49)-2)*LDTRK
            IPC(11) = IPTR0 + (HNTCEL(25)-1)*LDTRK
            IPC(12) = IPTR0 + (HNTCEL(26)-2)*LDTRK
            IPC( 1) = IPTR0 + (HNTCEL(93)-1)*LDTRK
            IPC( 2) = IPTR0 + (HNTCEL(97)-2)*LDTRK
            IPC( 3) = IPTR0 + (HNTCEL(49)-1)*LDTRK
            IPC( 4) = IPTR0 + (HNTCEL(51)-2)*LDTRK
      ENDIF
      ENDIF
      ELSE
      IF(
     - ICELL.LE.48
     -)THEN
      IF(
     - ICELL.EQ.25
     -)THEN
            IPC( 1) = IPTR0
            IPC( 2) = IPTR0 + (HNTCEL( 3)-2)*LDTRK
            IPC( 3) = IPTR0 + (HNTCEL(24)-1)*LDTRK
            IPC( 4) = IPTR0 + (HNTCEL(25)-2)*LDTRK
            IPC( 5) = IPTR0 + (HNTCEL(25)-1)*LDTRK
            IPC( 6) = IPTR0 + (HNTCEL(27)-2)*LDTRK
            IPC( 7) = IPTR0 + (HNTCEL(48)-1)*LDTRK
            IPC( 8) = IPTR0 + (HNTCEL(49)-2)*LDTRK
            IPC( 9) = IPTR0 + (HNTCEL(49)-1)*LDTRK
            IPC(10) = IPTR0 + (HNTCEL(53)-2)*LDTRK
            IPC(11) = IPTR0 + (HNTCEL(95)-1)*LDTRK
            IPC(12) = IPTR0 + (HNTCEL(97)-2)*LDTRK
      ELSE
      IF(
     - ICELL.LT.48
     -)THEN
            IPC( 1) = IPTR0 + (HNTCEL(ICELL-25)-1)*LDTRK
            IPC( 2) = IPTR0 + (HNTCEL(ICELL-22)-2)*LDTRK
            IPC( 3) = 0
            IPC( 4) = 0
            IPC( 5) = IPTR0 + (HNTCEL(ICELL- 1)-1)*LDTRK
            IPC( 6) = IPTR0 + (HNTCEL(ICELL+ 2)-2)*LDTRK
            IPC( 7) = 0
            IPC( 8) = 0
            ICLL3 = (ICELL-24)*2 + 46
            IPC( 9) = IPTR0 + (HNTCEL(ICLL3-1)-1)*LDTRK
            IPC(10) = IPTR0 + (HNTCEL(ICLL3+5)-2)*LDTRK
            IPC(11) = 0
            IPC(12) = 0
      ELSE
            IPC( 1) = IPTR0 + (HNTCEL(23)-1)*LDTRK
            IPC( 2) = IPTR0 + (HNTCEL(25)-2)*LDTRK
            IPC( 3) = IPTR0
            IPC( 4) = IPTR0 + (HNTCEL( 2)-2)*LDTRK
            IPC( 5) = IPTR0 + (HNTCEL(47)-1)*LDTRK
            IPC( 6) = IPTR0 + (HNTCEL(49)-2)*LDTRK
            IPC( 7) = IPTR0 + (HNTCEL(25)-1)*LDTRK
            IPC( 8) = IPTR0 + (HNTCEL(26)-2)*LDTRK
            IPC( 9) = IPTR0 + (HNTCEL(93)-1)*LDTRK
            IPC(10) = IPTR0 + (HNTCEL(97)-2)*LDTRK
            IPC(11) = IPTR0 + (HNTCEL(49)-1)*LDTRK
            IPC(12) = IPTR0 + (HNTCEL(51)-2)*LDTRK
      ENDIF
      ENDIF
      ELSE
          IPC( 9) = 0
          IPC(10) = 0
          IPC(11) = 0
          IPC(12) = 0
      IF(
     - ICELL.EQ.49
     -)THEN
            IPC( 1) = IPTR0 + (HNTCEL(48)-1)*LDTRK
            IPC( 2) = IPTR0 + (HNTCEL(49)-2)*LDTRK
            IPC( 3) = IPTR0 + (HNTCEL(25)-1)*LDTRK
            IPC( 4) = IPTR0 + (HNTCEL(26)-2)*LDTRK
            IPC( 5) = IPTR0 + (HNTCEL(49)-1)*LDTRK
            IPC( 6) = IPTR0 + (HNTCEL(51)-2)*LDTRK
            IPC( 7) = IPTR0 + (HNTCEL(96)-1)*LDTRK
            IPC( 8) = IPTR0 + (HNTCEL(97)-2)*LDTRK
      ELSE
      IF(
     - ICELL.LT.96
     -)THEN
            ICLL2 = (ICELL-50)/2 + 25
            IPC( 1) = IPTR0 + (HNTCEL(ICLL2   )-1)*LDTRK
            IPC( 2) = IPTR0 + (HNTCEL(ICLL2+ 2)-2)*LDTRK
            IPC( 3) = 0
            IPC( 4) = 0
            IPC( 5) = IPTR0 + (HNTCEL(ICELL- 1)-1)*LDTRK
            IPC( 6) = IPTR0 + (HNTCEL(ICELL+ 2)-2)*LDTRK
            IPC( 7) = 0
            IPC( 8) = 0
      ELSE
            IPC( 1) = IPTR0 + (HNTCEL(48)-1)*LDTRK
            IPC( 2) = IPTR0 + (HNTCEL(49)-2)*LDTRK
            IPC( 3) = IPTR0 + (HNTCEL(25)-1)*LDTRK
            IPC( 4) = IPTR0 + (HNTCEL(26)-2)*LDTRK
            IPC( 5) = IPTR0 + (HNTCEL(95)-1)*LDTRK
            IPC( 6) = IPTR0 + (HNTCEL(97)-2)*LDTRK
            IPC( 7) = IPTR0 + (HNTCEL(49)-1)*LDTRK
            IPC( 8) = IPTR0 + (HNTCEL(50)-2)*LDTRK
      ENDIF
      ENDIF
      ENDIF
      ENDIF
        IF(IPC( 1).GT.IPC( 2)) IPC( 1) = 0
        IF(IPC( 3).GT.IPC( 4)) IPC( 3) = 0
        IF(IPC( 5).EQ.IPC( 6)) IPC( 5) = 0
        IF(IPC( 7).GT.IPC( 8)) IPC( 7) = 0
        IF(IPC( 9).GT.IPC(10)) IPC( 9) = 0
        IF(IPC(11).GT.IPC(12)) IPC(11) = 0
      GOTO IZZZ01
17002 CONTINUE
        JPTRI = IPTR
        LRFLGI = ISHFTR(LBTREL,10)
        LRFLGI = LAND(LRFLGI,3)
        XXIM = AMIN1(ABS(WRK(JPTRI+11)),ABS(WRK(JPTRI+15)))
        YYIM = AMIN1(ABS(WRK(JPTRI+12)),ABS(WRK(JPTRI+16)))
        ICLOOP = 5
16000 CONTINUE
      IF(
     - IPC(ICLOOP).GT.0
     -)THEN
            JPTRO = IPC(ICLOOP  )
            JPTR9 = IPC(ICLOOP+1)
15000 CONTINUE
      IF(
     - JPTRO.LE.JPTR9
     -)THEN
      IF(
     - JPTRO.NE.JPTRI
     -)THEN
                NCAND = 255
      IF(
     - ABS(XXIM).GT.ABS(YYIM)
     -)THEN
                  XXOM = AMAX1(ABS(WRK(JPTRO+ 3)),ABS(WRK(JPTRO+ 7)))
                  IF(XXOM-XXIM .GT. CBTRLM(1)) NCAND=0
      ELSE
                  YYOM = AMAX1(ABS(WRK(JPTRO+ 4)),ABS(WRK(JPTRO+ 8)))
                  IF(YYOM-YYIM .GT. CBTRLM(1)) NCAND=0
      ENDIF
      IF(
     - NCAND.EQ.0
     -)THEN
                  NCOMB = 0
                  JCLLI = IWRK(JPTRI)
                  IF(JCLLI.GT.48) JCLLI = ISHFTR(JCLLI-46,1)
                  IF(JCLLI.GT.24) JCLLI = JCLLI - 24
                  JCLLO = IWRK(JPTRO)
                  IF(JCLLO.GT.48) JCLLO = ISHFTR(JCLLO-46,1)
                  IF(JCLLO.GT.24) JCLLO = JCLLO - 24
      IF(
     - IABS(JCLLO-JCLLI).EQ.0
     -)THEN
                    DRLIM = CBTRLM(2)
                    DANGL = CBTRLM(3)
      ELSE
                    DRLIM = CBTRLM(6)
                    DANGL = CBTRLM(7)
      ENDIF
                  ALGHT0 = ALGHT1
                  IF(ICLOOP.GE.9) ALGHT0 = ALGHT2
                  LRFLGO = ISHFTR(IWRK(JPTRO+19),10)
                  LRFLGO = LAND(LRFLGO,3)
                  LRFLIO = LRFLGI + LRFLGO*4
                  LBLR = 0
                  RMSMZW = 1000.
      IZZZSE            =LRFLIO
      GOTO 14002
14003 CONTINUE
                    LRFLAG = 1
      ASSIGN 17008 TO IZZZ04
      GOTO 17007
17008 CONTINUE
                    LBLR = 1
      GOTO 14001
14004 CONTINUE
                    LRFLAG = 2
      ASSIGN 17009 TO IZZZ04
      GOTO 17007
17009 CONTINUE
                    LBLR = 2
      GOTO 14001
14005 CONTINUE
                    LRFLAG = 1
      ASSIGN 17010 TO IZZZ04
      GOTO 17007
17010 CONTINUE
                    LRFLAG = 2
      ASSIGN 17011 TO IZZZ04
      GOTO 17007
17011 CONTINUE
                    LBLR = 3
      GOTO 14001
14006 CONTINUE
                    LRFLAG = 3
      ASSIGN 17012 TO IZZZ04
      GOTO 17007
17012 CONTINUE
                    LBLR = 4
      GOTO 14001
14007 CONTINUE
                    LRFLAG = 4
      ASSIGN 17013 TO IZZZ04
      GOTO 17007
17013 CONTINUE
                    LBLR = 8
      GOTO 14001
14008 CONTINUE
                    LRFLAG = 3
      ASSIGN 17014 TO IZZZ04
      GOTO 17007
17014 CONTINUE
                    LRFLAG = 4
      ASSIGN 17015 TO IZZZ04
      GOTO 17007
17015 CONTINUE
                    LBLR = 12
      GOTO 14001
14009 CONTINUE
                    LRFLAG = 1
      ASSIGN 17016 TO IZZZ04
      GOTO 17007
17016 CONTINUE
                    LRFLAG = 3
      ASSIGN 17017 TO IZZZ04
      GOTO 17007
17017 CONTINUE
                    LBLR = 5
      GOTO 14001
14010 CONTINUE
                    LRFLAG = 2
      ASSIGN 17018 TO IZZZ04
      GOTO 17007
17018 CONTINUE
                    LRFLAG = 4
      ASSIGN 17019 TO IZZZ04
      GOTO 17007
17019 CONTINUE
                    LBLR = 10
      GOTO 14001
14002 CONTINUE
      GOTO(
     -14000,14000,14000,14000,14003,14004,14005,14000,14006,14007,14008,
     -14000,14009,14010
     -), IZZZSE
14000 CONTINUE
14001 CONTINUE
      IF(
     - RMSMZW.GT.CBTRLM(4)
     -)THEN
                    IF(RMSMZW.LE.CBTRLM(5)) NCOMB = 0
      IF(
     - LAND(LBLR,1).EQ.0
     -)THEN
                      LRFLAG = 1
      ASSIGN 17020 TO IZZZ04
      GOTO 17007
17020 CONTINUE
      ENDIF
      IF(
     - LAND(LBLR,2).EQ.0
     -)THEN
                      LRFLAG = 2
      ASSIGN 17021 TO IZZZ04
      GOTO 17007
17021 CONTINUE
      ENDIF
      IF(
     - LAND(LBLR,4).EQ.0
     -)THEN
                      LRFLAG = 3
      ASSIGN 17022 TO IZZZ04
      GOTO 17007
17022 CONTINUE
      ENDIF
      IF(
     - LAND(LBLR,8).EQ.0
     -)THEN
                      LRFLAG = 4
      ASSIGN 17023 TO IZZZ04
      GOTO 17007
17023 CONTINUE
      ENDIF
      ENDIF
              IF(RMSMZW.LT.RMSMIN) RMSMIN = RMSMZW
      ENDIF
      ENDIF
              JPTRO = JPTRO + LDTRK
      GOTO 15000
      ENDIF
15001 CONTINUE
      ENDIF
          ICLOOP = ICLOOP + 2
      IF(.NOT.(
     - ICLOOP.GE.12
     -))GOTO 16000
16001 CONTINUE
      GOTO IZZZ02
17007 CONTINUE
      IZZZSE  =LRFLAG
      GOTO 14013
14014 CONTINUE
          JPTII = JPTRI+3
          JPTIO = JPTRI+11
          JPTOI = JPTRO+3
          JPTOO = JPTRO+11
      GOTO 14012
14015 CONTINUE
          JPTII = JPTRI+7
          JPTIO = JPTRI+15
          JPTOI = JPTRO+3
          JPTOO = JPTRO+11
      GOTO 14012
14016 CONTINUE
          JPTII = JPTRI+3
          JPTIO = JPTRI+11
          JPTOI = JPTRO+7
          JPTOO = JPTRO+15
      GOTO 14012
14017 CONTINUE
          JPTII = JPTRI+7
          JPTIO = JPTRI+15
          JPTOI = JPTRO+7
          JPTOO = JPTRO+15
      GOTO 14012
14013 CONTINUE
      GOTO(
     -14014,14015,14016,14017
     -), IZZZSE
14011 CONTINUE
14012 CONTINUE
        XXII = WRK(JPTII   )
        YYII = WRK(JPTII+ 1)
        DXII = WRK(JPTII+ 2)
        DYII = WRK(JPTII+ 3)
        XXOI = WRK(JPTOI   )
        YYOI = WRK(JPTOI+ 1)
        DXOI = WRK(JPTOI+ 2)
        DYOI = WRK(JPTOI+ 3)
        XXIO = WRK(JPTIO   )
        YYIO = WRK(JPTIO+ 1)
        DXIO = WRK(JPTIO+ 2)
        DYIO = WRK(JPTIO+ 3)
        XXOO = WRK(JPTOO   )
        YYOO = WRK(JPTOO+ 1)
        DXOO = WRK(JPTOO+ 2)
        DYOO = WRK(JPTOO+ 3)
16002 CONTINUE
          ZW1 = (XXII-XXOI)**2 + (YYII-YYOI)**2
          ZW2 = (XXOO-XXIO)**2 + (YYOO-YYIO)**2
          ZW0 = (XXII-XXOO)**2 + (YYII-YYOO)**2
      IF(
     - ZW1.GT.ZW0 .OR. ZW2.GT.ZW0
     -)THEN
      GOTO 16003
      ENDIF
          XOA = ( XXII + XXOO ) *.5
          YOA = ( YYII + YYOO ) *.5
          COSTH = XXOO - XXII
          SINTH = YYOO - YYII
          ZWZ = SQRT(COSTH**2+SINTH**2)
          COSTH = COSTH / ZWZ
          SINTH = SINTH / ZWZ
          XX = XXII - XOA
          YY = YYII - YOA
          YT1 = 0.
          XT1 =-.5*ZWZ
          XX = XXIO - XOA
          YY = YYIO - YOA
          YT2 = YY * COSTH - XX * SINTH
          XT2 = XX * COSTH + YY * SINTH
          XX = XXOI - XOA
          YY = YYOI - YOA
          YT3 = YY * COSTH - XX * SINTH
          XT3 = XX * COSTH + YY * SINTH
          XX = XXOO - XOA
          YY = YYOO - YOA
          YT4 = 0.
          XT4 =-XT1
      IF(
     - ABS(XT2).LT.ABS(XT3)
     -)THEN
            X1 = XT1
            Y1 = YT1
            X2 = XT2
            XC = XT2
            Y2 = YT2
            YC = YT2
            XL = XT3
            YL = YT3
      ELSE
            X1 = XT3
            XC = XT3
            Y1 = YT3
            YC = YT3
            X2 = XT4
            Y2 = YT4
            XL = XT2
            YL = YT2
      ENDIF
          X0 = 0.
          Y0 = 1000000.
          DY = Y2 - Y1
          IF(ABS(DY).GT..01)
     *       Y0 = ((X2-X1)*(X2+X1)/DY + Y2+Y1)*.5
          T  = Y0 - YC
          R0 = ABS(T)
          CHARGE = SIGN(1.,T)
          R0 = .5*(R0 + ((Y0-YC)**2+XC**2) / R0)
          DR0 = (XL**2-XC**2 + (Y0-YL)**2-(Y0-YC)**2)*.5/R0
          DSQ = .75*DR0**2
          XCIRC = X0*COSTH - Y0*SINTH + XOA
          YCIRC = Y0*COSTH + X0*SINTH + YOA
          ALGHT = ABS(XT1)*2.
          ALRAT = AMAX1(ALGHT,ALGHT0) / R0
          DRLIM1 = AMAX1(ALRAT,1.)*CBTRLM(4) + DRLIM
      IF(JPTRI.GE.IPR1) NPR = NPR + 1
      IF(
     - ABS(DR0).GT.DRLIM1
     -)THEN
      GOTO 16003
      ENDIF
          NCAND = 2
      IF(
     - ABS(XXIO-XCIRC).LT.ABS(YYIO-YCIRC)
     -)THEN
            TG1  = DYII / DXII
            TGC1 =-(XXII-XCIRC) / (YYII-YCIRC)
            TG2  = DYIO / DXIO
            TGC2 =-(XXIO-XCIRC) / (YYIO-YCIRC)
            TG3  = DYOI / DXOI
            TGC3 =-(XXOI-XCIRC) / (YYOI-YCIRC)
            TG4  = DYOO / DXOO
            TGC4 =-(XXOO-XCIRC) / (YYOO-YCIRC)
      ELSE
            TG1  = DXII / DYII
            TGC1 =-(YYII-YCIRC) / (XXII-XCIRC)
            TG2  = DXIO / DYIO
            TGC2 =-(YYIO-YCIRC) / (XXIO-XCIRC)
            TG3  = DXOI / DYOI
            TGC3 =-(YYOI-YCIRC) / (XXOI-XCIRC)
            TG4  = DXOO / DYOO
            TGC4 =-(YYOO-YCIRC) / (XXOO-XCIRC)
      ENDIF
          DLTS1 = (TG1-TGC1) / (1.+TG1*TGC1)
          DLTS2 = (TG2-TGC2) / (1.+TG2*TGC2)
          DLTS3 = (TG3-TGC3) / (1.+TG3*TGC3)
          DLTS4 = (TG4-TGC4) / (1.+TG4*TGC4)
          DLTSQ1 = DLTS1**2
          DLTSQ2 = DLTS2**2
          DLTSQ3 = DLTS3**2
          DLTSQ4 = DLTS4**2
          DELTSQ = DLTSQ1 + DLTSQ2 + DLTSQ3 + DLTSQ4
      IF(
     - AMAX1(DLTSQ1,DLTSQ2,DLTSQ3,DLTSQ4).GT.DANGL
     -)THEN
      GOTO 16003
      ENDIF
          RMS = (DSQ/.06 + DELTSQ/.0020) * .2
          IF(RMS.LT.RMSMZW) RMSMZW = RMS
          NCAND = RMS
          IF(NCAND.GE.256) NCAND = 255
      IF(
     - IWRK(JPTRO+20).LT.8 .AND.
     ?       IWRK(JPTRI+21).LT.8 .AND. NCOMB.LT.2
     -)THEN
            NCOMB = NCOMB + 1
            HZW(1) = JPTRO
            HZW(2) = LRFLAG - 1 + NCAND*16
            NCNTRI = IWRK(JPTRI+21) + 1
            IWRK(JPTRI+NCNTRI+29) = IZW
            IWRK(JPTRI+21) = NCNTRI
            HZW(1) = JPTRI
            NCNTRO = IWRK(JPTRO+20) + 1
            IWRK(JPTRO+NCNTRO+21) = IZW
            IWRK(JPTRO+20) = NCNTRO
      ENDIF
      IF(.NOT.(
     - .TRUE.
     -))GOTO 16002
16003 CONTINUE
      GOTO IZZZ04
17005 CONTINUE
        IPTR = HPTE0
16004 CONTINUE
          NCNT= IWRK(IPTR+20)
          IP0 = IPTR+22
      IF(
     - NCNT.GT.1
     -)THEN
            ORFACT =-1.
      ASSIGN 17025 TO IZZZ05
      GOTO 17024
17025 CONTINUE
      ENDIF
          NCNT= IWRK(IPTR+21)
          IP0 = IPTR+30
      IF(
     - NCNT.GT.1
     -)THEN
            ORFACT =+1.
      ASSIGN 17026 TO IZZZ05
      GOTO 17024
17026 CONTINUE
      ENDIF
        IPTR = IPTR + HLDTE
      IF(.NOT.(
     - IPTR.GT.HPTE9
     -))GOTO 16004
16005 CONTINUE
      GOTO IZZZ03
17024 CONTINUE
      CALL MVC(IAUX(1),0,IWRK(IP0),0,32)
      LBORD = 0
      DO 13002 I=1,NCNT
        IZW = IAUX(I)
        IPCN = HZW1
      IF(
     - LAND(IZW,2).EQ.0
     -)THEN
          RSQ = WRK(IPCN+11)**2 + WRK(IPCN+12)**2
      ELSE
          RSQ = WRK(IPCN+15)**2 + WRK(IPCN+16)**2
      ENDIF
        RAUX(I) = RSQ
        IF(I.GT.1 .AND. (RSQ-RAUX(I-1))*ORFACT.LT.0) LBORD = 1
13002 CONTINUE
13003 CONTINUE
      IF(
     - LBORD.NE.0
     -)THEN
      IF(
     - NCNT.EQ.2
     -)THEN
            IWRK(IP0  ) = IAUX(2)
            IWRK(IP0+1) = IAUX(1)
      ELSE
          LBORD = 0
          NCNT9 = NCNT - 1
      DO 13004 I1=1,NCNT9
            NCNT1 = I1 + 1
      DO 13006 I2=NCNT1,NCNT
      IF(
     - (RAUX(I2)-RAUX(I1))*ORFACT .LT. 0.
     -)THEN
                IZW = IAUX(I2)
                IAUX(I2) = IAUX(I1)
                IAUX(I1) = IZW
                ZW  = RAUX(I2)
                RAUX(I2) = RAUX(I1)
                RAUX(I1) = ZW
      ENDIF
13006 CONTINUE
13007 CONTINUE
13004 CONTINUE
13005 CONTINUE
          CALL MVC(IWRK(IP0),0,IAUX(1),0,32)
      ENDIF
      ENDIF
      GOTO IZZZ05
      END
