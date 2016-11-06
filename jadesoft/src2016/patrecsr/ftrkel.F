      SUBROUTINE FTRKEL
      IMPLICIT INTEGER*2 (H)
#include "cworkpr.for"
#include "cworkeq.for"
      EQUIVALENCE
     ,           (ICELL ,IDWRK(1)),(MHIT  ,IDWRK(2)),(IRING ,IDWRK(3))
     ,         , (IERRCD,IDWRK(4)),(NTRKEL,IDWRK(5))
#include "cpatlm.for"
#include "cjdrch.for"
#include "cdsmax.for"
      DIMENSION HPBAK(16),HLYAR(16),HLBAR(16),DSPAR(16),RESAR(16)
      DATA MKGDLN / Z'107'/
      DATA MKDIVL / Z'100'/
      DATA LBNOCN / Z'1'/
      DATA LBKINK / Z'2'/
      DATA LBWALL / Z'4'/
      DATA LBWIRE / Z'8'/
      DATA LBTREL /Z'1000'/
      INTEGER LBLFRT(3) /Z'10000',Z'30000',Z'20000'/
      FPAR0(X) = (PAR10*X + PAR20)*X + PAR30
      FPAR (X) = (PAR1 *X + PAR2 )*X + PAR3
      MTRK = 0
      HPTR0= HPFREE
      HLDTR= 17
      IPTR = HPTR0
      IP=HPHT0
16000 CONTINUE
        LB = LAND(IWRK(IP+4),MKGDLN)
        IFREEU = IWRK(IP+5)
        IF(IFREEU.GT.0) IFREEU = IWRK(IFREEU+9)
        IFREED = IWRK(IP+7)
        IF(IFREED.GT.0) IFREED = IWRK(IFREED+9)
      IF(
     - IWRK(IP+8).EQ.0 .AND. LB.EQ.7
     .     .AND. ABS(WRK(IP+11)).GT..1
     .     .AND. IFREEU.EQ.0 .AND. IFREED.EQ.0
     -)THEN
          DS0 = WRK(IP+2)
          SL0 = WRK(IP+3)
          NLINEL = 1
          DDSOFF = SIGN(TRELLM(1),WRK(IP+11))
          IF(LAND(IWRK(IP),1).NE.0) DDSOFF =-DDSOFF
          INEXT = 5
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
          LBSTRT = LBEND
          IPL = IWRK(IPH+5)
          IWRK(IPH+8) = 1
          INEXT = 7
      ASSIGN 17002 TO IZZZ01
      GOTO 17000
17002 CONTINUE
          IPH = IWRK(IPH+7)
      IF(
     - NLINEL+2.GE.LMTREL(13)
     -)THEN
      ASSIGN 17004 TO IZZZ02
      GOTO 17003
17004 CONTINUE
      ENDIF
      ENDIF
      IP = IP + HLDHT
      IF(.NOT.(
     - IP.GT.HPHT9
     -))GOTO 16000
16001 CONTINUE
      HPFREE= IPTR
      HPTR9 = IPTR - 1
      NTRKEL = MTRK
      RETURN
17000 CONTINUE
          IPH   = IP
          IPA   = IWRK(IP+INEXT)
          LBEND = 0
15000 CONTINUE
      IF(
     - IPA.GT.0
     -)THEN
              IPNXT = IWRK(IPA+INEXT)
      IF(
     - IPNXT.LE.0
     -)THEN
                LBEND = LBNOCN
      GOTO 15001
      ENDIF
      IF(
     - IWRK(IPNXT+8).NE.0 .OR. LAND(IWRK(IPA+4),MKDIVL).NE.0
     -)THEN
                LBEND = LBKINK
      GOTO 15001
      ENDIF
              NLINEL = NLINEL + 1
              IPH    = IPA
              IPA    = IPNXT
      GOTO 15000
      ENDIF
15001 CONTINUE
      GOTO IZZZ01
17003 CONTINUE
        IHIT = 0
        IPA = IPL
16002 CONTINUE
          IWRK(IPA+8) = 1
          IHIT = IHIT + 1
          HPBAK(IHIT  ) = IPA
          HLYAR(IHIT  ) = IWRK(IPA  )
          DSPAR(IHIT) =  WRK(IPA+2)
        IPA = IWRK(IPA+ 7)
      IF(.NOT.(
     - IPA.GE.IPH
     -))GOTO 16002
16003 CONTINUE
        IWRK(IPA+8) = 1
        HPBAK(IHIT+1) = IPA
        HLYAR(IHIT+1) = IWRK(IPA  )
        DSPAR(IHIT+1) =  WRK(IPA+2)
        JHIT0 = 1
        JHIT9 = IHIT + 1
        IHIT9 = JHIT9
        IHIT0 = 1
        MHIT = IHIT9 - IHIT0 + 1
        MKCUT = 0
15002 CONTINUE
      IF(
     - MHIT.GE.LMTREL(13)
     -)THEN
          DSTAG0 = 0.
          NITER = LMTREL(6)
      ASSIGN 17006 TO IZZZ03
      GOTO 17005
17006 CONTINUE
      IF(
     - STAGSG.LE.SIGLM .AND. YPMIN.GE.TRELLM(10)
     -)THEN
            DSIGLR = ABS(SIGL-SIGR)
      IF(
     - DSIGLR.GT.TRELLM(19) .AND.
     ?         (MHIT.GT.6 .OR. STAGSG.LE.TRELLM(14)**2)
     -)THEN
              LBLR =-1
              IF(SIGR.LT.SIGL) LBLR = 1
      ELSE
              LBLR = 0
      ENDIF
      ASSIGN 17008 TO IZZZ04
      GOTO 17007
17008 CONTINUE
            MHIT = 0
      ELSE
      ASSIGN 17010 TO IZZZ05
      GOTO 17009
17010 CONTINUE
            MHIT = IHIT9 - IHIT0 + 1
      ENDIF
      IF(
     - MKCUT.NE.0 .AND. MHIT.LT.LMTREL(13)
     -)THEN
            IHIT0 = JHIT0
            IHIT9 = JHIT9
            MKCUT = 0
            MHIT = IHIT9 - IHIT0 + 1
      ENDIF
      GOTO 15002
      ENDIF
15003 CONTINUE
      GOTO IZZZ02
17009 CONTINUE
        IHITM1 = IHITM0
        IHITM2 = IHITM0
        MHIT = IHIT9 - IHIT0 + 1
16004 CONTINUE
      IF(
     - YPMIN.LT.TRELLM(10) .AND. STAGSG.LE.SIGLM
     -)THEN
            HLYMIN = XPMIN
      DO 13000 IHIT=IHIT0,IHIT9
              IF(HLYAR(IHIT).LE.HLYMIN) IHITM1 = IHIT
13000 CONTINUE
13001 CONTINUE
            IHITM2 = IHITM1 + 1
      GOTO 16005
      ENDIF
      IF(
     - YPMIN.LT.10. .AND. MHIT.GE.5
     -)THEN
            IHMIN  = IHIT0
            DSMIN  = 100000.
      DO 13002 IHIT=IHIT0,IHIT9
      IF(
     - DSPAR(IHIT).LT.DSMIN
     -)THEN
                DSMIN = DSPAR(IHIT)
                IHMIN = IHIT
      ENDIF
13002 CONTINUE
13003 CONTINUE
            IHMINS = IHMIN
            DDSP   = 1000.
      IF(
     - IHMIN.EQ.IHIT9
     -)THEN
              IHMIN = IHMIN - 1
      ELSE
      IF(
     - IHMIN.GT.IHIT0
     -)THEN
                DDSP = DSPAR(IHMIN+1) - DSPAR(IHMIN-1)
                IF(DDSP.GT.0) IHMIN = IHMIN - 1
      ENDIF
      ENDIF
            LBCUT = 0
      IF(
     - IHMIN-2.LT.IHIT0
     -)THEN
              IF(DSPAR(IHMIN+3)-DSPAR(IHMIN+1).GT.2.0) LBCUT = 1
      ELSE
      IF(
     - IHMIN+3.GT.IHIT9
     -)THEN
                IF(DSPAR(IHMIN-2)-DSPAR(IHMIN  ).GT.2.0) LBCUT = 1
      ELSE
                ZSL1 = DSPAR(IHMIN-2) - DSPAR(IHMIN  )
                ZSL2 = DSPAR(IHMIN+3) - DSPAR(IHMIN+1)
                IF(ZSL1.GT.2.0 .OR. ZSL2.GT.2.0) LBCUT = 1
      ENDIF
      ENDIF
      IF(
     - LBCUT.NE.0
     -)THEN
      IF(
     - ABS(DDSP).GT..6 .AND. DSMIN.GT..6
     -)THEN
                IHITM1 = IHMIN
                IHITM2 = IHMIN + 1
      ELSE
                IHITM1 = IHMINS - 1
                IHITM2 = IHMINS + 1
      ENDIF
      GOTO 16005
      ENDIF
      ENDIF
      IF(
     - MHIT.GE.6
     -)THEN
            DRES1 = 0.
            DRES2 = 0.
            IHMX1 =-1
            IHMX2 =-1
            DSL0  = PAR10*8.
            IHIT4 = IHIT0+4
      DO 13004 IHIT = IHIT0,IHIT9
              X = HLYAR(IHIT)
              Y = DSPAR(IHIT)
              F = FPAR0(X)
              DCHI = F - Y
              RESAR(IHIT) = DCHI
      IF(
     - IHIT.GT.IHIT0
     -)THEN
                DDRES1 = ABS(DCHI-DCHI1)
      IF(
     - DDRES1.GT.DRES1
     -)THEN
                  DRES1 = DDRES1
                  IHMX1 = IHIT
      ENDIF
      IF(
     - IHIT.GE.IHIT4
     -)THEN
                  DDRES2 = ABS(2.*DSPAR(IHIT-2)-DSPAR(IHIT-4)-Y + DSL0)
      IF(
     - DDRES2.GT.DRES2
     -)THEN
                    DRES2 = DDRES2
                    IHMX2 = IHIT-2
      ENDIF
      ENDIF
      ENDIF
              DCHI1 = DCHI
13004 CONTINUE
13005 CONTINUE
            DRES1 = DRES1 * 1.5
            DRES0 = TRELLM(16)*2.5
      IF(
     - DRES0.LT.DRES2 .OR. DRES0.LT.DRES1
     -)THEN
      IF(
     - DRES1-DRES2 .LT. TRELLM(14)*1.4
     -)THEN
      IF(
     - DSPAR(IHMX2).LT.TRELLM(10)
     -)THEN
      IF(
     - DSPAR(IHMX2-1).LT.DSPAR(IHMX2+1)
     -)THEN
                    IHITM2 = IHMX2
                    IHITM1 = IHMX2 - 1
      ELSE
                    IHITM1 = IHMX2
                    IHITM2 = IHMX2 + 1
      ENDIF
      ELSE
      IF(
     - DRES2.GT.TRELLM(14)*5.
     -)THEN
                    IHITM1 = IHMX2
                    IHITM2 = IHMX2
      ELSE
                    IHITM1 = IHMX2 - 1
                    IHITM2 = IHMX2 + 1
      ENDIF
      ENDIF
      ELSE
                IHITM1 = IHMX1 - 1
                IHITM2 = IHMX1
      ENDIF
      ENDIF
      ENDIF
      IF(.NOT.(
     - .TRUE.
     -))GOTO 16004
16005 CONTINUE
      IF(
     - MKCUT.EQ.2  .OR.
     ?     ((IHITM1-IHIT0).GE.(IHIT9-IHITM2) .AND. MKCUT.EQ.0)
     -)THEN
      IF(
     - IHITM1.GT.IHIT0
     -)THEN
            IF(IHITM1.EQ.IHIT9) IHITM1 = IHITM1 - 1
            IHIT9 = IHITM1
            IF(MKCUT.NE.1) JHIT0 = MIN0(IHITM2,JHIT9)
            MKCUT = 2
      ELSE
            IHIT9 = JHIT9
            IHIT0 = IHIT0+1
            JHIT0 = IHIT0
            MKCUT = 0
      ENDIF
      ELSE
      IF(
     - IHITM2.LT.IHIT9
     -)THEN
            IF(IHITM2.EQ.IHIT0) IHITM2 = IHITM2 + 1
            IHIT0 = IHITM2
            IF(MKCUT.NE.2) JHIT9 = MAX0(IHITM1,JHIT0)
            MKCUT = 1
      ELSE
            IHIT0 = JHIT0
            IHIT9 = IHIT9-1
            JHIT9 = IHIT9
            MKCUT = 0
      ENDIF
      ENDIF
      GOTO IZZZ05
17007 CONTINUE
        DSTAG0 = SWDEPL*LBLR
        NITER = 0
      ASSIGN 17011 TO IZZZ03
      GOTO 17005
17011 CONTINUE
      IF(
     - MHIT.GE.LMTREL(13)
     -)THEN
      DO 13006 IHIT = IHIT0,IHIT9
      IF(
     - HLBAR(IHIT).EQ.0
     -)THEN
              IPA = HPBAK(IHIT)
              IWRK(IPA+10) = IWRK(IPA+9)
              IWRK(IPA+ 9) = IPTR
      ENDIF
13006 CONTINUE
13007 CONTINUE
          LRIND = ISHFTR(LBLR+3,1)
          IF(DSMAX(ILAY0+1,IRING,LRIND)-5. .LT. DS0)
     ?      LBSTRT = LOR(LBSTRT,LBWALL)
          IF(DS0.LT.1.5) LBSTRT = LOR(LBSTRT,LBWIRE)
          IF(DSMAX(ILAY9+1,IRING,LRIND)-5. .LT. DS9)
     ?      LBEND  = LOR(LBEND ,LBWALL)
          IF(DS9.LT.1.5) LBEND  = LOR(LBEND ,LBWIRE)
          LBTR = LOR(ISHFTL(LBEND,4),LBSTRT)
          LBTR = LOR(LBTR,LBTREL)
          LBTR = LOR(LBTR,LBLFRT(LBLR+2))
          IWRK(IPTR   ) = IPHIT0
          IWRK(IPTR+ 1) = IPHIT9
          IWRK(IPTR+ 2) = MHIT
          WRK (IPTR+ 3) = PAR1
          WRK (IPTR+ 4) = PAR2
          WRK (IPTR+ 5) = PAR3
          WRK (IPTR+ 6) = SQRT(SIG)
          IWRK(IPTR+ 7) = ILAY0
          WRK (IPTR+ 8) = DS0
          WRK (IPTR+ 9) = SLOP0
          IWRK(IPTR+10) = ILAY9
          WRK (IPTR+11) = DS9
          WRK (IPTR+12) = SLOP9
          IWRK(IPTR+13) = LBSTRT
          IWRK(IPTR+14) = LBEND
          IWRK(IPTR+15) = LBTR
          I9 = IPTR + HLDTR - 1
          MTRK = MTRK + 1
          IPTR = IPTR + HLDTR
      ENDIF
      GOTO IZZZ04
17005 CONTINUE
      ITER = 0
      IF(DSPAR(IHIT0).LT.1.5 .AND. DSPAR(IHIT0+1).GT.5.) IHIT0 = IHIT0+1
C      00053900
      IF(DSPAR(IHIT9).LT.1.5 .AND. DSPAR(IHIT9-1).GT.5.) IHIT9 = IHIT9-1
C      00054000
      IF(
     - NITER.GT.0
     -)THEN
      DO 13008 IHIT=IHIT0,IHIT9
          HLBAR(IHIT) = 0
13008 CONTINUE
13009 CONTINUE
      ENDIF
16006 CONTINUE
      ASSIGN 17013 TO IZZZ06
      GOTO 17012
17013 CONTINUE
      IF(
     - MHIT.LT.LMTREL(13)
     -)THEN
          STAGSG = 10000000.
          IHITM0 = IHIT0 + 1
      GOTO 16007
      ENDIF
        CHISQ = 0.
        STAGS1 = 0.
        STGSL1 = 0.
        STGSL2 = 0.
        STGSR1 = 0.
        STGSR2 = 0.
        DCHIM1 = 0.
        IHITM1 = 0
      DO 13010 IHIT = IHIT0,IHIT9
      IF(
     - HLBAR(IHIT).EQ.0
     -)THEN
            X = HLYAR(IHIT)
            DSTAG = DSTAG0
            IF(hLAND(HLYAR(IHIT),hint(1)).NE.0) DSTAG =-DSTAG0 ! PMF 10/06/99: hland,hint
            Y = DSPAR(IHIT) + DSTAG
            F = FPAR(X)
            DCHI = Y - F
            RESAR(IHIT) = DCHI
            CHISQ = CHISQ + DCHI**2
      IF(
     - ABS(DCHI).GE.DCHIM1
     -)THEN
              DCHIM1 = ABS(DCHI)
              IHITM1 = IHIT
      ENDIF
      IF(
     - hLAND(HLYAR(IHIT),hint(1)).NE.0 ! PMF 10/06/99: hland,hint
     -)THEN
              STAGS1 = STAGS1 + DCHI
              DCHIL = DCHI + SWDEPL
              DCHIR = DCHI - SWDEPL
      ELSE
              STAGS1 = STAGS1 - DCHI
              DCHIL = DCHI - SWDEPL
              DCHIR = DCHI + SWDEPL
      ENDIF
            STGSL1 = STGSL1 + DCHIL
            STGSL2 = STGSL2 + DCHIL**2
            STGSR1 = STGSR1 + DCHIR
            STGSR2 = STGSR2 + DCHIR**2
      ENDIF
13010 CONTINUE
13011 CONTINUE
        SIG    =      CHISQ  / DEG
        SIGL   = STGSL2 / DEG
        SIGR   = STGSR2 / DEG
        STAGSG = AMIN1(SIGL,SIGR)
        STAGAV = STAGS1 / S0
      IF(
     - ITER.EQ.0
     -)THEN
          PAR10 = PAR1
          PAR20 = PAR2
          PAR30 = PAR3
          IHITM0 = IHITM1
      ENDIF
        YPMIN = 1000.
        XPMIN = 1000.
        IF(PAR1.NE.0.) XPMIN = -.5 * PAR2 / PAR1
        IF(XPMIN.GT.HLYAR(IHIT0) .AND. XPMIN.LT.HLYAR(IHIT9))
     ?    YPMIN = FPAR(XPMIN)
      IF(
     - STAGSG.LE.SIGLM .OR. MHIT.LE.LMTREL(13)
     -)THEN
      GOTO 16007
      ENDIF
      IF(
     - MHIT.LE.6 .AND. ABS(PAR1).GT.TRELLM(9)
     -)THEN
      GOTO 16007
      ENDIF
      IF(
     - YPMIN.LT. 100.
     -)THEN
      GOTO 16007
      ENDIF
      IF(
     - MHIT.GE.6.AND.ABS(PAR1).LT..010.AND.STAGSG.LT.TRELLM(14)*1.5
     -)THEN
          DCHI = RESAR(IHITM1)
      IF(
     - hLAND(HLYAR(IHITM1),hint(1)).NE.0 ! PMF 10/06/99: hland,hint
     -)THEN
            DCHIL = DCHI + SWDEPL
            DCHIR = DCHI - SWDEPL
      ELSE
            DCHIL = DCHI - SWDEPL
            DCHIR = DCHI + SWDEPL
      ENDIF
          DCHI = DCHIL
          IF(SIGR.GT.SIGL) DCHI = DCHIR
      IF(
     - DCHI**2*.33 .LT. STAGSG
     -)THEN
            SIGLM = AMAX1(STAGSG*1.01,SIGLM)
      GOTO 16007
      ENDIF
      ENDIF
        IDHITM = IABS(IHITM1-IHITM0)
        DDCHIM = DCHIM1
        IF(IHITM1.NE.IHIT0 .AND. IHITM1.NE.IHIT9)
     ?  DDCHIM = ABS(RESAR(IHITM1)-(RESAR(IHITM1-1)+RESAR(IHITM1+1))*.5)
      IF(
     - IDHITM.EQ.1
     -)THEN
      GOTO 16007
      ENDIF
      IF(
     - DDCHIM**2.LT.SIGLM*4.0
     -)THEN
      GOTO 16007
      ENDIF
      IF(
     - IHITM1-IHIT0 .EQ. 1
     -)THEN
          SL0   = HLYAR(IHITM1)*PAR1*2. + PAR2
          DDSP1 = ABS(DSPAR(IHIT0+1)+SL0    - DSPAR(IHIT0+2))
          DDSP2 = ABS(DSPAR(IHIT0  )+SL0*2. - DSPAR(IHIT0+2))
          IF(DDSP1 .LT. DDSP2) IHITM1 = IHIT0
      ELSE
      IF(
     - IHIT9-IHITM1 .EQ. 1
     -)THEN
            SL0   = HLYAR(IHITM1)*PAR1*2. + PAR2
            DDSP1 = ABS(DSPAR(IHIT9-1)-SL0    - DSPAR(IHIT9-2))
            DDSP2 = ABS(DSPAR(IHIT9  )-SL0*2. - DSPAR(IHIT9-2))
            IF(DDSP1 .LT. DDSP2) IHITM1 = IHIT9
      ENDIF
      ENDIF
        IF(ITER.NE.NITER) HLBAR(IHITM1) = 1
        ITER = ITER + 1
      IF(.NOT.(
     - ITER.GT.NITER
     -))GOTO 16006
16007 CONTINUE
      IF(
     - ITER.GT.0 .AND. STAGSG.LE.SIGLM
     -)THEN
        IF(HLBAR(IHIT0).NE.0) IHIT0 = IHIT0 + 1
        IF(HLBAR(IHIT0).NE.0) IHIT0 = IHIT0 + 1
        IF(HLBAR(IHIT9).NE.0) IHIT9 = IHIT9 - 1
        IF(HLBAR(IHIT9).NE.0) IHIT9 = IHIT9 - 1
        IF(IHIT9.LT.JHIT0) JHIT0 = IHIT9
        IF(IHIT0.GT.JHIT9) JHIT9 = IHIT0
      ENDIF
      GOTO IZZZ03
17012 CONTINUE
      S0 = 0.
      S1 = 0.
      S2 = 0.
      S3 = 0.
      S4 = 0.
      S5 = 0.
      S6 = 0.
      S7 = 0.
      DO 13012 IHIT = IHIT0,IHIT9
      IF(
     - HLBAR(IHIT).EQ.0
     -)THEN
          IX = HLYAR(IHIT)
          X  = IX
          DSTAG = DSTAG0
          IF(LAND(IX,1).NE.0) DSTAG =-DSTAG0
          Y = DSPAR(IHIT) + DSTAG
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
13012 CONTINUE
13013 CONTINUE
      MHIT = S0 + .5
      IF(
     - MHIT.LT.3
     -)THEN
        SIG = TRELLM(12)**2
        PAR1 = 0.
        PAR2 = 0.
        PAR3 = 0.
        DEG = 1.
        S0  = 1.
      ELSE
      IF(
     - MHIT.LE.4
     -)THEN
          ZW1  = (S6*S0 - S1*S7)
          ZW2  = (S2*S0 - S1**2)
          PAR1   = 0.
          PAR2   = ZW1 / ZW2
          PAR3   =(S7 - PAR2*S1) / S0
          DEG = S0 - 2.
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
      IF(
     - MHIT.LT.6 .AND. ABS(PAR1).GT.TRELLM(9)
     -)THEN
            ZW1  = (S6*S0 - S1*S7)
            ZW2  = (S2*S0 - S1**2)
            PAR1   = 0.
            PAR2   = ZW1 / ZW2
            PAR3   =(S7 - PAR2*S1) / S0
            DEG = S0 - 2.
      ENDIF
      ENDIF
      ENDIF
        if (IHIT0.LE.0 .or. IHIT0.GT.16) then
        write(*,*) 'Bad bounds in ftrkel,1'
        write(*,*)IHIT0
        write(*,*)HLYAR
        IHIT0=1
        endif
        ILAY0 = HLYAR(IHIT0)
        WR0   = ILAY0
        DS0   = FPAR(WR0)
        SLOP0 = WR0*PAR1*2. + PAR2
        if (IHIT9.LE.0 .or. IHIT9.GT.16) then
        write(*,*) 'Bad bounds in ftrkel,2'
        write(*,*)IHIT9
        write(*,*)HLYAR
        IHIT9=IHIT0
        endif 
        ILAY9 = HLYAR(IHIT9)
        WR9   = ILAY9
        DS9   = FPAR(WR9)
        SLOP9 = WR9*PAR1*2. + PAR2
        MDHIT = IHIT9-IHIT0 + 1 - MHIT
      IF(
     - ABS(PAR1).GT..05 .OR. (MHIT.LT.6 .AND. MDHIT.GT.0)
     -)THEN
          SIGLM = TRELLM(16)**2
      ELSE
          SIG1 = TRELLM(16)
          SIG2 = 0.
          SIG3 = 0.
          IF(DSMAX(ILAY0+1,IRING,1)-5. .LT. DS0) SIG2 = TRELLM(17)
          IF(DS0.LT.1.5) SIG3 = TRELLM(18)
          IF(DSMAX(ILAY9+1,IRING,1)-5. .LT. DS9) SIG2 = SIG2+TRELLM(17)
          IF(DS9.LT.1.5) SIG3 = SIG3 + TRELLM(18)
          SIGLM = SIG1**2 + SIG2**2 + SIG3**2
      ENDIF
        SIGSL = 0.
        SLRAT =  AMIN1(ABS(SLOP0+SLOP9)*.1, 1.0)
        IF(ABS(PAR1).LT..5 .AND. SLRAT .GT. 0.2)
     ?  SIGSL = (SLRAT*TRELLM(17))**2 + TRELLM(16)**2
        IF(SIGSL.GT.SIGLM) SIGLM = SIGSL
      GOTO IZZZ06
      END
