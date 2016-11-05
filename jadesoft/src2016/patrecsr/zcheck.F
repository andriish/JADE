      SUBROUTINE ZCHECK(DHRLIM)
      IMPLICIT INTEGER*2 (H)
      LOGICAL TBIT
#include "cgraph.for"
      COMMON / CJCELL / NCELL(3),NWIRES(3)
#include "cjdrch.for"
#include "cdata.for"
      COMMON /CHEADR/ HEAD(108)
#include "cdsmax.for"
      COMMON /CEE1/ EE1(96,2)
#include "cpatlm.for"
#include "cworkpr.for"
      EQUIVALENCE
     ,      (ADWRK(11),LAYER ),(ADWRK(12),NI    ),(ADWRK(13),NH    )
     ,     ,(ADWRK(14),XHIT  ),(ADWRK(15),YHIT  ),(ADWRK(16),ZHIT  )
     ,     ,(ADWRK(17),RHIT  ),(ADWRK(18),IERZRF),(ADWRK(19),LRFLAG)
     ,     ,(ADWRK(20),INCELL),(ADWRK(21),IERFLG),(ADWRK(22),BETA  )
      COMMON/CJTRIG/ PI,TWOPI
      DIMENSION LCL(4,100),MASKY(4)
      DIMENSION HELP1(2),HELP2(2),HCELLI(6),HELPX(2)
      EQUIVALENCE (LABL1,HELP1(1)),(HELP2(1),LABL2),(LABLX,HELPX(1))
      EQUIVALENCE (LCL(1,1),IWRK(1))
      DATA HELP1/0,0/ ,HELP2/0,0/, LNRHIT/100/, MK1/Z7F/,MKZLAB /Z1/
      DATA MKZ0LB /ZFFFE/
      DATA HELPX/0,0/,ICALL /0/
      IPATR=IBLN('PATR')
      IPPATR = IDATA(IPATR)
      IF(IPPATR.LE.0) RETURN
      NTR = IDATA(IPPATR+2)
      IF(NTR.LE.0) RETURN
      IJETC=IBLN('JETC')
      IJHTL=IBLN('JHTL')
      DELPHI = TWOPI/24.
      LO = IDATA(IPPATR+1)
      LTRBK = IDATA(IPPATR+3)
      IPJCA = IDATA(IJETC)
      IPJ = 2*IPJCA + 2
      NHT = (HDATA(IPJ+97)-HDATA(IPJ+1))/4
      HPFREE = 401
      LP1 = HPFREE - 1
      LP2 = LP1 + LNRHIT
      LP3 = LP2 + LNRHIT
      LP4 = LP3 + LNRHIT
      LP5 = LP4 + LNRHIT
      LP6 = LP5 + LNRHIT
      LP7 = LP6 + LNRHIT
      LP8 = LP7 + LNRHIT
      LP9 = LP8 + LNRHIT
      LPA = LP9 + LNRHIT
      LPB = LPA + LNRHIT
      HPFREE = LPB + LNRHIT + 1
      IPJ = IPJ + 95
      IPJHTL = IDATA(IJHTL)
      IPJH = 2*IPJHTL + 2
      IF(
     - ICALL.EQ.0
     -)THEN
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
         ICALL = 1
      ENDIF
      ASSIGN 17003 TO IZZZ02
      GOTO 17002
17003 CONTINUE
      DO 13000 ITR = 1,NTR
         IPPO1 = IPPATR + (ITR-1)*LTRBK + LO
         IPPO = IPPO1
         ITYPTR = IDATA(IPPO + 18)
      IF(
     - ITYPTR.EQ.1
     -)THEN
      ASSIGN 17005 TO IZZZ03
      GOTO 17004
17005 CONTINUE
      IF(
     - IPRFL.EQ.0
     -)THEN
               RMIN1 = RMIN
               RMAX1 = RMAX
               XMIT1 = XMIT
               YMIT1 = YMIT
      ENDIF
      ENDIF
      IF(
     - ITYPTR.EQ.2
     -)THEN
      ASSIGN 17007 TO IZZZ04
      GOTO 17006
17007 CONTINUE
      IF(
     - IPRFL.EQ.0
     -)THEN
               C1R = C1
               C2R = C2
               C3R = C3
               C4R = C4
               C5R = C5
               C6R = C6
      ENDIF
      ENDIF
         IPRFL1 = IPRFL
      IF(
     - IPRFL1.EQ.0
     -)THEN
            NHIT = 0
            NI = IPJ
            IHITH = -1
      DO 13002 I = 1,6
               HCELLI(I) = IDATA(IPPO + 33 + I)
13002 CONTINUE
13003 CONTINUE
      DO 13004  IHT = 1,NHT
               NI = NI + 4
               IHITH = IHITH + 2
               IHITG = IHITH + 1
               HELP1(2) = HDATA(IPJH + IHITH)
               HELP2(2) = HDATA(IPJH + IHITG)
               MTR = LAND(ISHFTR(LABL1,1),MK1)
               MTR2 = LAND(ISHFTR(LABL2,1),MK1)
               IAMB = 0
               IF(MTR.EQ.ITR) IAMB = 1
               IF(MTR2.EQ.ITR) IAMB = 2
      IF(
     - IAMB.NE.0
     -)THEN
                  IF(IAMB.EQ.1) NH = IPJH + IHITH
                  IF(IAMB.EQ.2) NH = IPJH + IHITG
                  IF(IAMB.EQ.1) LABLX = LABL1
                  IF(IAMB.EQ.2) LABLX = LABL2
                  LRFLAG = -1
                  IF(TBIT(LABLX,23)) LRFLAG = 1
                  BETA = 1.
                  CALL JETXYZ
                  ITST = 1
      DO 13006 I = 1,6
                     IF(INCELL.EQ.HCELLI(I)) ITST = 0
13006 CONTINUE
13007 CONTINUE
      IF(
     - ITST.EQ.0
     -)THEN
                     NHIT = NHIT + 1
                     WRK(LP1+NHIT) = RHIT
                     FIHIT = ATAN2(YHIT,XHIT)
                     IF(FIHIT.LT.0.) FIHIT = FIHIT + TWOPI
                     WRK(LP2+NHIT) = FIHIT
                     WRK(LP3+NHIT) = XHIT
                     WRK(LP4+NHIT) = YHIT
                     IWRK(LP5+NHIT) = NI
                     IWRK(LP6+NHIT) = LAYER + (INCELL - 1)*16
                     IWRK(LP7+NHIT) = INCELL
                     IWRK(LP8+NHIT) = NH
                     IWRK(LP9+NHIT) = LRFLAG
                     WRK(LPA+NHIT) = ZHIT
                     IWRK(LPB+NHIT) = LAND(LABLX,MKZLAB)
      ENDIF
      ENDIF
13004 CONTINUE
13005 CONTINUE
      IF(
     - NHIT.GT.3
     -)THEN
      DO 13008 IHT = 1,NHIT
      ASSIGN 17009 TO IZZZ05
      GOTO 17008
17009 CONTINUE
      IF(
     - ITYPTR.EQ.1
     -)THEN
                     RMIN = RMIN1
                     RMAX = RMAX1
                     XMIT = XMIT1
                     YMIT = YMIT1
      ASSIGN 17011 TO IZZZ06
      GOTO 17010
17011 CONTINUE
      ENDIF
      IF(
     - ITYPTR.EQ.2
     -)THEN
                     C1 = C1R
                     C2 = C2R
                     C3 = C3R
                     C4 = C4R
                     C5 = C5R
                     C6 = C6R
      ASSIGN 17013 TO IZZZ07
      GOTO 17012
17013 CONTINUE
      ENDIF
                  ICRFL1 = ICRFL
      IF(
     - ICRFL1.EQ.0
     -)THEN
                     DISTA=(XTS-XW)**2+(YTS-YW)**2
                     DISTA = SQRT(DISTA)
                     XTS1 = XTS
                     YTS1 = YTS
                     RTS = SQRT(XTS**2 + YTS**2)
                     INCL1 = IWRK(LP7+IHT)
      ASSIGN 17015 TO IZZZ08
      GOTO 17014
17015 CONTINUE
      DO 13010 KTR = 1,NTR
      IF(
     - KTR.NE.ITR
     -)THEN
                           ICHKFL = LAND(MASKY(1),LCL(1,KTR))
     $                            + LAND(MASKY(2),LCL(2,KTR))
     $                            + LAND(MASKY(3),LCL(3,KTR))
     $                            + LAND(MASKY(4),LCL(4,KTR))
      IF(
     - ICHKFL.GT.0
     -)THEN
                              IPPO = IPPATR + (KTR-1)*LTRBK + LO
      ASSIGN 17017 TO IZZZ09
      GOTO 17016
17017 CONTINUE
      IF(
     - IOKZ.EQ.0
     -)THEN
                                 ITYPTS = IDATA(IPPO+18)
      IF(
     - ITYPTS.EQ.1
     -)THEN
      ASSIGN 17018 TO IZZZ03
      GOTO 17004
17018 CONTINUE
      IF(
     - IPRFL.EQ.0
     -)THEN
      ASSIGN 17019 TO IZZZ06
      GOTO 17010
17019 CONTINUE
      ENDIF
      ENDIF
      IF(
     - ITYPTS.EQ.2
     -)THEN
      ASSIGN 17020 TO IZZZ04
      GOTO 17006
17020 CONTINUE
      IF(
     - IPRFL.EQ.0
     -)THEN
      ASSIGN 17021 TO IZZZ07
      GOTO 17012
17021 CONTINUE
      ENDIF
      ENDIF
      IF(
     - IPRFL.EQ.0.AND.ICRFL.EQ.0
     -)THEN
      ASSIGN 17023 TO IZZZ10
      GOTO 17022
17023 CONTINUE
      IF(
     - ICLSAM.EQ.1
     -)THEN
                                       DISTB=(XTS-XW)**2+(YTS-YW)**2
                                       DISTB = SQRT(DISTB)
                                       DIST = ABS(DISTA-DISTB)
      IF(
     - DIST.LT.DHRLIM
     -)THEN
                                          NH = IWRK(LP8+IHT)
                                          HELPX(2) = HDATA(NH)
                                          LABLX = LAND(LABLX,MKZ0LB)
                                          HDATA(NH) = HELPX(2)
      GOTO 13011
      ENDIF
      ENDIF
      ENDIF
      ENDIF
      ENDIF
      ENDIF
13010 CONTINUE
13011 CONTINUE
      ENDIF
13008 CONTINUE
13009 CONTINUE
      ENDIF
      ENDIF
13000 CONTINUE
13001 CONTINUE
      RETURN
17002 CONTINUE
      IPPO = IPPATR - LTRBK + LO
      DO 13012 ITR = 1,NTR
         LCL(1,ITR) = 0
         LCL(2,ITR) = 0
         LCL(3,ITR) = 0
         LCL(4,ITR) = 0
         IPPO = IPPO + LTRBK
         IPP = IPPO + 33
      DO 13014 I = 1,6
            INCE = IDATA(IPP+I)
      IF(
     - INCE.GT.0
     -)THEN
               IRNG = (INCE-1)/24 + 1
               INCE = INCE - (IRNG-1)*24
               MASK = 2**(INCE-1)
               LCL(IRNG,ITR) = LOR(LCL(IRNG,ITR),MASK)
      ELSE
      GOTO 13015
      ENDIF
13014 CONTINUE
13015 CONTINUE
13012 CONTINUE
13013 CONTINUE
      GOTO IZZZ02
17000 CONTINUE
      DO 13016 INCELL = 1,96
      IF(
     - INCELL.LT.49
     -)THEN
               II = INCELL
               IF(II.GT.24) II = II - 24
               FINC = FLOAT(II-1)*DELPHI + DELPHI*.5
      ELSE
               II = INCELL - 48
               FINC = FLOAT(II-1)*DELPHI*.5 + DELPHI*.25
      ENDIF
            IF(FINC.LT.0.) FINC = FINC + TWOPI
            EE1(INCELL,1) = TAN(FINC + .5*PI + DRIROT(INCELL,1))
            EE1(INCELL,2) = TAN(FINC + .5*PI + DRIROT(INCELL,2))
13016 CONTINUE
13017 CONTINUE
      GOTO IZZZ01
17008 CONTINUE
         NI = IWRK(LP5 + IHT)
         LRFLG = IWRK(LP9 + IHT)
         IF(LRFLG.LT.0) LRFLG = 0
         XHIT = WRK(LP3+IHT)
         YHIT = WRK(LP4+IHT)
         INCELL = IWRK(LP7+IHT)
         E1 = EE1(INCELL,LRFLG+1)
         NRING = INCELL - 1
         NRING = NRING/24 + 1
         IF(NRING.GT.3) NRING = 3
         NCL = NCELL(NRING)
         RADD = RINCR(NRING)
         RW = FSENSW(NRING) - RADD
         NIW = IWRK(LP6+IHT) + 1
         NEW = NIW - (NRING-1)*384
         SHIFI = PSIIN(NRING)
         NCE = ISHFTR(NEW-1,4)
         NWE = NEW - NCE*16
         RW = RW + NWE*RADD
         NUMCLO = 2
         IF(NWE.LT.9) NUMCLO = 1
         FACT = -1.
         IF(TBIT(NWE,31)) FACT = 1.
         FIW = SHIFI + NCE*TWOPI/FLOAT(NCL)
         IF(FIW.LT.0.) FIW = FIW + TWOPI
         IF(FIW.GT.TWOPI) FIW = FIW - TWOPI
         COSPH = COS(FIW)
         SINPH = SIN(FIW)
         XW = RW*COSPH  - FACT*SINPH*SWDEPL
         YW = RW*SINPH  + FACT*COSPH*SWDEPL
         E2 = YW - E1*XW
      GOTO IZZZ05
17010 CONTINUE
         ICRFL = 0
         CONST1 = RMAX*RMIN + E2*E2 - 2.*E2*YMIT
         CONST1 = CONST1/(1.+E1*E1)
         CONST2 = E1*E2 - E1*YMIT - XMIT
         CONST2 = CONST2/(1.+E1*E1)
         RADD = CONST2*CONST2 - CONST1
         IF(RADD.LE.0.) ICRFL = -1
      IF(
     - ICRFL.EQ.0
     -)THEN
            RADD = AMAX1(RADD,.000001)
            RADD = SQRT(RADD)
            X1 = -CONST2 + RADD
            X2 = -CONST2 - RADD
            XTS = X1
            IF(ABS(XHIT-X2).LT.ABS(XHIT-X1)) XTS = X2
            YTS = E2 + E1*XTS
      ENDIF
      GOTO IZZZ06
17004 CONTINUE
         IPRFL = 0
         RAD = ADATA(IPPO+19)
         RMIN = ADATA(IPPO+20)
         PHIMIT = ADATA(IPPO+21)
      IF(
     - ABS(RAD).GT.1.E-08
     -)THEN
            RAD = ABS(1./RAD)
            RMIT= RAD + RMIN
            RMAX = RMIT + RAD
            XMIT = RMIT*COS(PHIMIT)
            YMIT = RMIT*SIN(PHIMIT)
      ELSE
            IPRFL = -1
      ENDIF
      GOTO IZZZ03
17006 CONTINUE
         IPRFL = 0
         A = ADATA(IPPO+19)
         B = ADATA(IPPO+20)
         C = ADATA(IPPO+21)
         D = ADATA(IPPO+22)
         COSA = COS(A)
         SINA = SIN(A)
         C1 = D*SINA*SINA
         C2 = - 2.*D*(B*COSA*SINA + C*SINA*SINA) - COSA
         C3 = D*COSA*COSA
         C4 = - 2.*D*(B*COSA*COSA + C*COSA*SINA) + SINA
         C5 = 2.*D*COSA*SINA
         C6 = (B*COSA+C*SINA)
         C6 = D*C6*C6 + C*COSA - B*SINA
      GOTO IZZZ04
17012 CONTINUE
         ICRFL = 0
         C7 = C1*E1*E1 + C3 + C5*E1
         C8 = 2.*C1*E1*E2 + C2*E1 + C4 + C5*E2
         C9 = C1*E2*E2 + C2*E2 + C6
      IF(
     - ABS(C7).GT..000001
     -)THEN
            DD = C8*C8 - 4.*C7*C9
      IF(
     - DD.GT..000001
     -)THEN
               X1 = (-C8 + SQRT(DD))/(2.*C7)
               X2 = (-C8 - SQRT(DD))/(2.*C7)
               XTS = X1
               IF(ABS(XHIT-X2).LT.ABS(XHIT-X1)) XTS = X2
               YTS = E2 + E1*XTS
      ELSE
               ICRFL = -1
      ENDIF
      ELSE
            ICRFL = -1
      ENDIF
      GOTO IZZZ07
17014 CONTINUE
      MASKY(1) = 0
      MASKY(2) = 0
      MASKY(3) = 0
      MASKY(4) = 0
      IF(
     - INCL1.LE.24
     -)THEN
      IF(
     - INCL1.EQ.1
     -)THEN
            MASKY(1) = 8388611
            MASKY(2) = MASKY(1)
            MASKY(3) = 7
            MASKY(4) = 8388608
      ELSE
      IF(
     - INCL1.LT.24
     -)THEN
               MASKY(1) = LOR(MASKY(1),2**(INCL1-2))
               MASKY(1) = LOR(MASKY(1),2**(INCL1-1))
               MASKY(1) = LOR(MASKY(1),2**INCL1)
               MASKY(2) = MASKY(1)
      IF(
     - INCL1.LT.12
     -)THEN
                  MASKY(3) = LOR(MASKY(3),2**(2*INCL1-3))
                  MASKY(3) = LOR(MASKY(3),2**(2*INCL1-2))
                  MASKY(3) = LOR(MASKY(3),2**(2*INCL1-1))
                  MASKY(3) = LOR(MASKY(3),2**(2*INCL1))
      ENDIF
      IF(
     - INCL1.GT.13
     -)THEN
                  INCK1 = INCL1 - 12
                  MASKY(4) = LOR(MASKY(4),2**(2*INCK1-3))
                  MASKY(4) = LOR(MASKY(4),2**(2*INCK1-2))
                  MASKY(4) = LOR(MASKY(4),2**(2*INCK1-1))
                  MASKY(4) = LOR(MASKY(4),2**(2*INCK1))
      ENDIF
      IF(
     - INCL1.EQ.12
     -)THEN
                  MASKY(3) = 14680064
                  MASKY(4) = 1
      ENDIF
      IF(
     - INCL1.EQ.13
     -)THEN
                  MASKY(4) = 7
                  MASKY(3) = 8388608
      ENDIF
      ELSE
               MASKY(1) = 12582913
               MASKY(2) = MASKY(1)
               MASKY(3) = 1
               MASKY(4) = 14680064
      ENDIF
      ENDIF
      ELSE
      IF(
     - INCL1.LE.48
     -)THEN
      IF(
     - INCL1.EQ.25
     -)THEN
               MASKY(1) = 8388611
               MASKY(2) = MASKY(1)
               MASKY(3) = 7
               MASKY(4) = 8388608
      ELSE
      IF(
     - INCL1.LT.48
     -)THEN
                  INCK1 = INCL1 - 24
                  MASKY(1) = LOR(MASKY(1),2**(INCK1-2))
                  MASKY(1) = LOR(MASKY(1),2**(INCK1-1))
                  MASKY(1) = LOR(MASKY(1),2**INCK1)
                  MASKY(2) = MASKY(1)
      IF(
     - INCK1.LT.12
     -)THEN
                     MASKY(3) = LOR(MASKY(3),2**(2*INCK1-3))
                     MASKY(3) = LOR(MASKY(3),2**(2*INCK1-2))
                     MASKY(3) = LOR(MASKY(3),2**(2*INCK1-1))
                     MASKY(3) = LOR(MASKY(3),2**(2*INCK1))
      ENDIF
      IF(
     - INCK1.GT.13
     -)THEN
                     INCK2 = INCK1 - 12
                     MASKY(4) = LOR(MASKY(4),2**(2*INCK2-3))
                     MASKY(4) = LOR(MASKY(4),2**(2*INCK2-2))
                     MASKY(4) = LOR(MASKY(4),2**(2*INCK2-1))
                     MASKY(4) = LOR(MASKY(4),2**(2*INCK2))
      ENDIF
      IF(
     - INCK1.EQ.12
     -)THEN
                     MASKY(3) = 14680064
                     MASKY(4) = 1
      ENDIF
      IF(
     - INCK1.EQ.13
     -)THEN
                     MASKY(4) = 7
                     MASKY(3) = 8388608
      ENDIF
      ELSE
                  MASKY(1) = 12582913
                  MASKY(2) = MASKY(1)
                  MASKY(3) = 1
                  MASKY(4) = 14680064
      ENDIF
      ENDIF
      ELSE
      IF(
     - INCL1.EQ.49
     -)THEN
               MASKY(1) = 8388609
               MASKY(2) = MASKY(1)
               MASKY(3) = 3
               MASKY(4) = 8388608
      ELSE
      IF(
     - INCL1.LT.96
     -)THEN
                  INCK1 = INCL1 - 48
                  INCK2 = INCK1/2
                  MASKY(1) = LOR(MASKY(1),2**(INCK2-1))
                  MASKY(1) = LOR(MASKY(1),2**INCK2)
                  MASKY(2) = MASKY(1)
      IF(
     - INCK2.LT.12
     -)THEN
                     MASKY(3) = LOR(MASKY(3),2**(INCK1-2))
                     MASKY(3) = LOR(MASKY(3),2**(INCK1-1))
                     MASKY(3) = LOR(MASKY(3),2**INCK1)
      ENDIF
      IF(
     - INCK2.EQ.12
     -)THEN
                     IF(INCK1.EQ.24) MASKY(3) = 12582912
                     IF(INCK1.EQ.25) MASKY(3) = 8388608
                     IF(INCK1.EQ.24) MASKY(4) = 1
                     IF(INCK1.EQ.25) MASKY(4) = 3
      ENDIF
      IF(
     - INCK2.GT.12
     -)THEN
                     INCK1 = INCK1 - 24
                     MASKY(4) = LOR(MASKY(4),2**(INCK1-2))
                     MASKY(4) = LOR(MASKY(4),2**(INCK1-1))
                     MASKY(4) = LOR(MASKY(4),2**INCK1)
      ENDIF
      ELSE
                  MASKY(1) = 8388609
                  MASKY(2) = MASKY(1)
                  MASKY(3) = 1
                  MASKY(4) = 12582912
      ENDIF
      ENDIF
      ENDIF
      ENDIF
      GOTO IZZZ08
17022 CONTINUE
      ICLSAM = 0
      FII = ATAN2(YTS,XTS)
      IF(FII.LT.0.) FII = FII + TWOPI
      NFII = FII/DELPHI
      IF(INCL1.GT.48) NFII = 2.*FII/DELPHI
      NFII = NFII + 1
      IF(INCL1.GT.24) NFII = NFII + 24
      IF(INCL1.GT.48) NFII = NFII + 24
      IF(INCL1.EQ.NFII) ICLSAM = 1
      GOTO IZZZ10
17016 CONTINUE
         IOKZ = 0
         ZTEST = ADATA(IPPO+30)*RTS + ADATA(IPPO+31)
         IF(ABS(ZTEST).GT.ZMAX) IOKZ = 1
      GOTO IZZZ09
      END
