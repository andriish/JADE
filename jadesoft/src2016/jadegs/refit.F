      SUBROUTINE REFIT(IPTR,IPJHTL)
      IMPLICIT INTEGER*2 (H)
      LOGICAL DEADCL
#include "cdata.for"
#include "calibr.for"
#include "cworkpr.for"
#include "cworkeq.for"
      EQUIVALENCE
     ,          (HPCO0 ,HPWRK(17)),(HPCO9 ,HPWRK(18)),(HLDCO ,HPWRK(19))
     ,         ,(ICELL ,IDWRK( 1)),(MHIT  ,IDWRK( 2)),(IRING ,IDWRK( 3))
     ,         ,(IERRCD,IDWRK( 4)),(NTRKEL,IDWRK( 5))
#include "cjdrch.for"
#include "cdsmax.for"
      INTEGER DATE(5), IDAY /0/
      DIMENSION ITRCLL(6), NCNCK(24), NHTRNG(3)
      INTEGER MKLRT1 /Z1000000/, MKLRT2 /Z100/
      INTEGER MKBDCL(3) /Z10,Z20,Z40/
      INTEGER MKDDCL(3) /Z01,Z02,Z04/
 2005 FORMAT('0TRACK:',I6,/,(1X,3I6,4F8.3,I4,F8.3,2I4,F8.3,I6,F8.3))
3001  FORMAT('0NHTRNG',3I6)
      DATA LBINIT /0/
      IF(
     - LBINIT .EQ. 0
     -)THEN
        LBINIT = 1
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
      ENDIF
      IPHEAD = IDATA(IQHEAD)*2
      NRUN = HDATA(IPHEAD+10)
      NEV  = HDATA(IPHEAD+11)
      ITRK = IDATA(IPTR+1)
      HPFREE = 1
      HPFRE1 = HPFREE
      HPCO0  = HPFREE
      LHIT   = 14
      INDFET = 2
      CALL JFETCH(IPTR,IPJHTL,WRK(HPCO0),LHIT,IPRES,INDFET)
      HLDCO  = LHIT
      HPCO9  = IPRES - 1
      HPAR0  = IPRES
      HLDPA  = 20
      HPAR9  = HPAR0 + HLDPA - 1
      HPFREE = HPAR9 + 1
      XT     = WRK (IPRES   )
      YT     = WRK (IPRES+ 1)
      CSROT  = WRK (IPRES+ 2)
      SNROT  = WRK (IPRES+ 3)
      WRK(HPAR0+ 4) = 0.
      WRK(HPAR0+ 5) = 0.
      WRK(HPAR0+ 6) = 0.
      WRK(HPAR0+ 7) = 1000.
      WRK(HPAR0+ 8) = 0.
      CSTH   = WRK (IPRES+11)
      SNTH   = WRK (IPRES+12)
      ZVERT = ADATA(IPTR+31)
      TGTH = ADATA(IPTR+30)
      HPTR0 = HPFREE
      CALL MVC(IWRK(HPTR0),0,IDATA(IPTR+1),0,192)
      IWRK(HPTR0+1) = 0
      IPCO = HPCO0 - HLDCO
      NHTRNG(1) = 0
      NHTRNG(2) = 0
      NHTRNG(3) = 0
      ILAYOL = -1
16000 CONTINUE
        IPCO = IPCO + HLDCO
        ILAY = IWRK(IPCO   )
      IF(
     - ILAY.NE.ILAYOL
     -)THEN
         JRING = IWRK(IPCO+12)
         NHTRNG(JRING) = NHTRNG(JRING) + 1
         ILAYOL = ILAY
      ENDIF
      IF(.NOT.(
     - IPCO.GE.HPCO9-HLDCO
     -))GOTO 16000
16001 CONTINUE
      HPFREE = HPFREE + 48
16002 CONTINUE
        JRINGL = 3
      ASSIGN 17003 TO IZZZ02
      GOTO 17002
17003 CONTINUE
        ALBLM1 = 0.6
        ALBLM2 = 3.0
      ASSIGN 17005 TO IZZZ03
      GOTO 17004
17005 CONTINUE
16004 CONTINUE
      ASSIGN 17006 TO IZZZ02
      GOTO 17002
17006 CONTINUE
      ASSIGN 17007 TO IZZZ03
      GOTO 17004
17007 CONTINUE
      IF(.NOT.(
     - NHGOOD-NHFIT .LT.4
     -))GOTO 16004
16005 CONTINUE
      IF(
     - SIG.LT.1.
     -)THEN
      ASSIGN 17009 TO IZZZ04
      GOTO 17008
17009 CONTINUE
      ENDIF
      IF(
     - SIG.LT..10
     -)THEN
      GOTO 16003
      ENDIF
      IF(
     - ABS(PAR1).LT..00030
     -)THEN
      GOTO 16003
      ENDIF
      IF(
     - NHTRNG(1)+NHTRNG(2).LE.16
     -)THEN
      GOTO 16003
      ENDIF
        ALBLM1 = 2.0
        ALBLM2 = 3.0
      ASSIGN 17010 TO IZZZ03
      GOTO 17004
17010 CONTINUE
        JRINGL = 2
      ASSIGN 17011 TO IZZZ02
      GOTO 17002
17011 CONTINUE
        ALBLM1 = 1.0
      ASSIGN 17012 TO IZZZ03
      GOTO 17004
17012 CONTINUE
      ASSIGN 17013 TO IZZZ02
      GOTO 17002
17013 CONTINUE
      ASSIGN 17014 TO IZZZ03
      GOTO 17004
17014 CONTINUE
      IF(
     - SIG.LT..20
     -)THEN
      ASSIGN 17016 TO IZZZ05
      GOTO 17015
17016 CONTINUE
      ENDIF
      IF(
     - SIG.LT..20
     -)THEN
      GOTO 16003
      ENDIF
      IF(
     - ABS(PAR1).LT..00150
     -)THEN
      GOTO 16003
      ENDIF
      IF(
     - NHTRNG(1).LE.9
     -)THEN
      GOTO 16003
      ENDIF
        ALBLM1 = 2.0
        ALBLM2 = 3.0
      ASSIGN 17017 TO IZZZ03
      GOTO 17004
17017 CONTINUE
        JRINGL = 1
        NHTFIT = NHTRNG(1)
      IF(
     - NHTFIT.LE.5
     -)THEN
          NHTFIT = NHTFIT + NHTRNG(2)
          JRINGL = 2
      ENDIF
      IF(
     - NHTFIT.GT.9
     -)THEN
      ASSIGN 17018 TO IZZZ02
      GOTO 17002
17018 CONTINUE
          ALBLM1 = 1.0
      ASSIGN 17019 TO IZZZ03
      GOTO 17004
17019 CONTINUE
      ASSIGN 17020 TO IZZZ02
      GOTO 17002
17020 CONTINUE
      ASSIGN 17021 TO IZZZ03
      GOTO 17004
17021 CONTINUE
      IF(
     - SIG.LT..20
     -)THEN
      ASSIGN 17022 TO IZZZ05
      GOTO 17015
17022 CONTINUE
      ENDIF
      ENDIF
      IF(.NOT.(
     - .TRUE.
     -))GOTO 16002
16003 CONTINUE
      HPFREE = HPFRE1
      RETURN
17002 CONTINUE
      S0 = 0.
      S1 = 0.
      S2 = 0.
      S3 = 0.
      S4 = 0.
      S7 = 0.
      S6 = 0.
      S5 = 0.
      IPCO = HPCO0
16006 CONTINUE
      IF(
     - IWRK(IPCO+ 10).EQ.0 .AND. IWRK(IPCO+12).LE.JRINGL
     -)THEN
          X = WRK(IPCO+3)
          Y = WRK(IPCO+4)
          X2 = X**2
          S1 = S1 + X
          S2 = S2 + X2
          S3 = S3 + X*X2
          S4 = S4 + X2**2
          S5 = S5 + Y*X2
          S6 = S6 + Y*X
          S7 = S7 + Y
          S0 = S0 + 1.
      ENDIF
      IPCO = IPCO + HLDCO
      IF(.NOT.(
     - IPCO.GT.HPCO9
     -))GOTO 16006
16007 CONTINUE
      IF(
     - S0.LT.3.5
     -)THEN
        SIG = 1000.
      ELSE
        F1 = 1. / S4
        XX12 = S3*F1
        XX13 = S2*F1
        YY1  = S5*F1
        XX22 = S2 - S3*XX12
        XX23 = S1 - S3*XX13
        YY2  = S6 - S3*YY1
        XX32 = S1 - S2*XX12
        XX33 = S0 - S2*XX13
        YY3  = S7 - S2*YY1
      IF(
     - XX22.GT.XX32
     -)THEN
          XX23 = XX23 / XX22
          YY2  = YY2  / XX22
          PAR3 = (YY3 - XX32*YY2) / (XX33 - XX32*XX23)
          PAR2 = YY2 - XX23*PAR3
      ELSE
          XX33 = XX33 / XX32
          YY3  = YY3  / XX32
          PAR3 = (YY2 - XX22*YY3) / (XX23 - XX22*XX33)
          PAR2 = YY3 - XX33*PAR3
      ENDIF
        PAR1 = YY1 - XX12*PAR2 - XX13*PAR3
        DEG = S0 - 3.
        NHFIT = S0 + .1
        CHISQ = 0.
        DCHIM1 = 0.
        IHITM1 = 0
        XST    = 999999.
        XEN    =-999999.
        IPCO = HPCO0
16008 CONTINUE
      IF(
     - IWRK(IPCO+ 10).EQ.0 .AND. IWRK(IPCO+12).LE.JRINGL
     -)THEN
            X = WRK(IPCO+3)
            IF(X.LT.XST) XST = X
            IF(X.GT.XEN) XEN = X
            Y = WRK(IPCO+4)
            F = (PAR1 *X + PAR2 )*X + PAR3
            DCHI = Y - F
            WRK(IPCO+13) = DCHI
            CHISQ = CHISQ + DCHI**2
      ENDIF
        IPCO = IPCO + HLDCO
      IF(.NOT.(
     - IPCO.GT.HPCO9
     -))GOTO 16008
16009 CONTINUE
        SIG    =      CHISQ  / DEG
      ENDIF
      GOTO IZZZ02
17008 CONTINUE
      YST  = (PAR1 *XST + PAR2 )*XST + PAR3
      YEN  = (PAR1 *XEN + PAR2 )*XEN + PAR3
      TGST = PAR1*XST*2 + PAR2
      DXST = 1./SQRT(TGST**2+1.)
      DYST = DXST * TGST
      TGEN = PAR1*XEN*2 + PAR2
      DXEN = 1./SQRT(TGEN**2+1.)
      DYEN = DXEN * TGEN
      XMIN = -PAR2*.5 / PAR1
      YMIN = (PAR1*XMIN + PAR2)*XMIN + PAR3
      CVZW = TGST**2+1.
      CVST =-PAR1 * 2 / (SQRT(CVZW)*CVZW)
      DET = (S2*S0-S1*S1)*S4 + (S2*S1-S3*S0)*S3 + (S3*S1-S2*S2)*S2
      SIG11 = (S2*S0 - S1*S1)/DET
      SIG22 = (S4*S0 - S2*S2)/DET
      SIG33 = (S4*S2 - S3*S3)/DET
      SIG12 = (S3*S0 - S2*S1)/DET
      SIG13 = (S3*S1 - S2*S2)/DET
      SIG23 = (S4*S1 - S3*S2)/DET
      IP    = HPTR0 - 1
      IWRK(IP+ 1) = ITRK
      IWRK(IP+ 2) = 16
      IWRK(IP+ 3) = IDAY
      IWRK(IP+ 4) =  0
      WRK (IP+ 5) = XST *CSROT - YST *SNROT + XT
      WRK (IP+ 6) = XST *SNROT + YST *CSROT + YT
      WRK (IP+ 7) = SQRT(WRK(IP+ 5)**2 + WRK(IP+ 6)**2) * TGTH + ZVERT
      WRK (IP+ 8) = (DXST*CSROT - DYST*SNROT)*CSTH
      WRK (IP+ 9) = (DXST*SNROT + DYST*CSROT)*CSTH
      WRK (IP+10) = SNTH
      IWRK(IP+11) = 0
      WRK (IP+12) = XEN *CSROT - YEN *SNROT + XT
      WRK (IP+13) = XEN *SNROT + YEN *CSROT + YT
      WRK (IP+14) = SQRT(WRK(IP+12)**2 + WRK(IP+13)**2) * TGTH + ZVERT
      WRK (IP+15) = (DXEN*CSROT - DYEN*SNROT)*CSTH
      WRK (IP+16) = (DXEN*SNROT + DYEN*CSROT)*CSTH
      WRK (IP+17) = SNTH
      IWRK(IP+18) = 2
      WRK (IP+19) = ATAN2(SNROT,CSROT)
      WRK (IP+20) = XMIN*CSROT - YMIN*SNROT + XT
      WRK (IP+21) = XMIN*SNROT + YMIN*CSROT + YT
      WRK (IP+22) = PAR1
      IF(SIG  .LT.0) PRINT 2021,WRK(IP+1),S0,SIG
 2021 FORMAT(' REFIT(PST): -VE SQRT:',I4,5E13.5)
      WRK (IP+23) = SIG
      IF(SIG  .GT.0) WRK(IP+23) = SQRT(SIG)
      IWRK(IP+24) = S0 + .001
      WRK (IP+25) = CVST
      IF(SIG11.LT.0) PRINT 2021,WRK(IP+1),S0,SIG,SIG11
      WRK (IP+26) = SIG*SIG11
      IF(WRK(IP+26) .GT. 0) WRK(IP+26) = SQRT(WRK(IP+26))*2.
      WRK (IP+27) = CVST
      WRK (IP+28) = CVST
      I0 = IP+ 1
      I9 = IP+48
      GOTO IZZZ04
17015 CONTINUE
      YST  = (PAR1 *XST + PAR2 )*XST + PAR3
      TGST = PAR1*XST*2 + PAR2
      DXST = 1./SQRT(TGST**2+1.)
      DYST = DXST * TGST
      XMIN = -PAR2*.5 / PAR1
      YMIN = (PAR1*XMIN + PAR2)*XMIN + PAR3
      CVZW = TGST**2+1.
      CVST =-PAR1 * 2 / (SQRT(CVZW)*CVZW)
      DET = (S2*S0-S1*S1)*S4 + (S2*S1-S3*S0)*S3 + (S3*S1-S2*S2)*S2
      SIG11 = (S2*S0 - S1*S1)/DET
      SIG22 = (S4*S0 - S2*S2)/DET
      SIG33 = (S4*S2 - S3*S3)/DET
      SIG12 = (S3*S0 - S2*S1)/DET
      SIG13 = (S3*S1 - S2*S2)/DET
      SIG23 = (S4*S1 - S3*S2)/DET
      IP    = HPTR0 - 1
      WRK (IP+ 5) = XST *CSROT - YST *SNROT + XT
      WRK (IP+ 6) = XST *SNROT + YST *CSROT + YT
      WRK (IP+ 7) = SQRT(WRK(IP+ 5)**2 + WRK(IP+ 6)**2) * TGTH + ZVERT
      WRK (IP+ 8) = (DXST*CSROT - DYST*SNROT)*CSTH
      WRK (IP+ 9) = (DXST*SNROT + DYST*CSROT)*CSTH
      WRK (IP+10) = SNTH
      IWRK(IP+18) = 2
      WRK (IP+19) = ATAN2(SNROT,CSROT)
      WRK (IP+20) = XMIN*CSROT - YMIN*SNROT + XT
      WRK (IP+21) = XMIN*SNROT + YMIN*CSROT + YT
      WRK (IP+22) = PAR1
      IF(SIG  .LT.0) PRINT 2022,WRK(IP+1),S0,SIG
 2022 FORMAT(' REFIT(PST): -VE SQRT(1):',I4,5E13.5)
      WRK (IP+23) = SIG
      IF(SIG  .GT.0) WRK(IP+23) = SQRT(SIG)
      IWRK(IP+24) = S0 + .001
      WRK (IP+25) = CVST
      IF(SIG11.LT.0) PRINT 2022,WRK(IP+1),S0,SIG,SIG11
      WRK (IP+26) = SIG*SIG11
      IF(WRK(IP+26) .GT. 0) WRK(IP+26) = SQRT(WRK(IP+26))*2.
      WRK (IP+27) = CVST
      GOTO IZZZ05
17004 CONTINUE
        NHGOOD = 0
        IWL = -999
      DO 13000 IP = HPCO0,HPCO9,HLDCO
          IW0 = IWRK(IP)
          X   = WRK(IP+3)
          Y   = WRK(IP+4)
          F   = (PAR1*X + PAR2)*X + PAR3
          DF  = F - Y
          LBGOOD = 4
          IF(ABS(DF).LT.ALBLM2) LBGOOD = 1
          IF(ABS(DF).LT.ALBLM1) LBGOOD = 0
          IWRK(IP+ 10) = LBGOOD
          IF(LBGOOD.EQ.0) NHGOOD = NHGOOD + 1
          WRK (IP+13) = DF
      IF(
     - IWL.EQ.IW0
     -)THEN
      IF(
     - ABS(DFL).LT.ABS(DF)
     -)THEN
              IF(IWRK(IP +10).EQ.0) NHGOOD = NHGOOD - 1
              IWRK(IP +10) = 16
      ELSE
              IF(IWRK(IPL+10).EQ.0) NHGOOD = NHGOOD - 1
              IWRK(IPL+10) = 16
      ENDIF
      ENDIF
          IWL = IW0
          IPL = IP
          DFL = DF
13000 CONTINUE
13001 CONTINUE
      GOTO IZZZ03
17000 CONTINUE
        IQHEAD = IBLN('HEAD')
        CALL DAY2(DATE)
        IDAY = DATE(1)*1000 + DATE(2)
      GOTO IZZZ01
      END
