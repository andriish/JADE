      SUBROUTINE BSTORE
      IMPLICIT INTEGER*2 (H)
#include "cworkpr.for"
#include "cworkmx.for"
#include "cpatlm.for"
      IF(HNREL(NTR).EQ.9) RETURN
      HNREL(NTR) = HNREL(NTR) + 1
      KR = HNREL(NTR)
      HISTR(KR,NTR) = IKX*LR
      HUSE(IKX) = 1
      RETURN
      END
      FUNCTION SLCOR(SL,LRS)
      IMPLICIT INTEGER*2 (H)
#include "cjdrch.for"
      IF(LRS.EQ.-1)SLCOR=DRICOS*SL/(1.+SL*DRISIN)
      IF(LRS.EQ.1) SLCOR=DRICOS*SL/(1.-SL*DRISIN)
      RETURN
      END
      SUBROUTINE CHOOSE
      IMPLICIT INTEGER*2 (H)
#include "cworkmx.for"
#include "cpatlm.for"
#include "cworkpr.for"
      ICHOOS=1
      IT=IRL-1
15000 CONTINUE
      IF(
     - ICHOOS.EQ.1
     -)THEN
      DO 13000 I=1,IT
      ITMP=I+1
      DO 13002 J=ITMP,IRL
      IF(
     - DTEMP(I).GT.DTEMP(J)
     -)THEN
      TEMP=DTEMP(I)
      DTEMP(I)=DTEMP(J)
      DTEMP(J)=TEMP
      DO 13004 JK=1,4
      TEMP=ITK(I,JK)
      ITK(I,JK)=ITK(J,JK)
      ITK(J,JK)=TEMP
13004 CONTINUE
13005 CONTINUE
      ENDIF
13002 CONTINUE
13003 CONTINUE
13000 CONTINUE
13001 CONTINUE
      IP=ITK(1,4)
      IC=ITK(1,1)
      IF(
     - DTEMP(1).NE.100.
     -)THEN
      IF(
     - IP.NE.K
     -)THEN
      DO 13006 J=1,IRL
      IF(
     - DTEMP(J).NE.100.
     -)THEN
      IF(ITK(J,4).EQ.IP.OR.ITK(J,1).EQ.IC) DTEMP(J)=100.
      ENDIF
13006 CONTINUE
13007 CONTINUE
      ELSE
      GOTO 15001
      ENDIF
      ELSE
      GOTO 15001
      ENDIF
      GOTO 15000
      ENDIF
15001 CONTINUE
      IF(
     - DTEMP(1).NE.100.
     -)THEN
      ITWO=2
      ITHREE=ITWO+1
      ELSE
      IRIFLG=0
      ENDIF
      RETURN
      END
      SUBROUTINE INTJN1(KK,KX,INTFLG,DTMP)
      IMPLICIT INTEGER*2 (H)
#include "cpatlm.for"
#include "cworkpr.for"
#include "cworkmx.for"
#include "cjdrch.for"
      DIMENSION HTEMP(9)
      DATA MSKCR1 /Z100/,MSKFIT/Z20000/,MSKAIT/ZFFFDFFFF/
      MAMB=0
      IF(NRHT(KK).GE.IBKK(19).AND.NRHT(KX).GE.IBKK(19)
     * .AND.IBKK(20).NE.0) MAMB=1
      INTFLG=0
      IWX=ITRKAR(KX,6)
      IOL=IBKK(1)-1
      IGAP=IBKK(2)+1
      IF(
     - IWX.LE.IW+IOL.AND.IWX.GE.IW-IGAP
     -)THEN
      ICROSS=1
      SLA=SL1(KK)/RINCR(KRING)
      SLB=SL2(KX)/RINCR(KRING)
      JT=IKX
      IKX=KX
      CALL LFRT(LRA)
      IKX=KK
      CALL LFRT(LR2)
      IKX=JT
      IF(IUDFLG.EQ.3.AND.LR.NE.0.AND.LRA.EQ.0) LRA=LR
      IF(IUDFLG.EQ.6.AND.LR.NE.0.AND.LR2.EQ.0) LR2=LR
      IDIW=IW-IWX
      IF(IDIW.LT.0) IDIW=0
      IF(
     - MAMB.NE.0.AND.LAND(MSKCR1,LBL(KK)).EQ.0.AND.
     ¢ LAND(MSKCR1,LBL(KX)).EQ.0
     -)THEN
      IF(LRA.NE.LR2) ICROSS=-1
      IF(LRA.EQ.0.OR.LR2.EQ.0) ICROSS=0
      ENDIF
      DSEX=TRKAR(KK,4)-SL1(KK)*IDIW
      DX=DSEX-TRKAR(KX,7)
      IF(
     - MAMB.EQ.0.OR.ICROSS.EQ.0
     -)THEN
      IF(SL2(KX).LT.0..AND.SL1(KK).GT.0..AND.LAND(LBL(KK),MSKCR1).EQ.0
     * .AND.LAND(LBL(KX),MSKCR1).EQ.0.AND.(DS1(KK).LT.BKK(5)
     ¢ .OR.DS2(KX).LT.BKK(5))) ICROSS=-1
      ENDIF
      IF(
     - ICROSS.EQ.-1
     -)THEN
      SLA=-SLA
      DX=TRKAR(KX,7)+DSEX
      ENDIF
      IF(
     - LAND(LBL(KK),MSKCR1).NE.0.AND.LAND(LBL(KX),MSKCR1).NE.0
     -)THEN
      DX=TRKAR(KX,7)+DSEX
      SLA=-SLA
      IF(MAMB.NE.0.AND.LRA*LR2.LT.0) DX=1000000.
      ENDIF
      IF(ICROSS.EQ.0) ICROSS=1
      IF(LAND(MSKCR1,LBL(KK)).EQ.0.AND.LAND(MSKCR1,LBL(KX)).NE.0
     * .AND.LRA*LR2.GT.0.AND.MAMB.NE.0) DX=10000000.
      IF(LAND(MSKCR1,LBL(KX)).EQ.0.AND.LAND(MSKCR1,LBL(KK)).NE.0
     * .AND.LRA*LR2.LT.0.AND.MAMB.NE.0) DX=10000000.
      IF(
     - ABS(DX).LT.DCELL
     -)THEN
      SLOLIM=(ABS(SLA)+ABS(SLB))/2.*BKK(14)+BKK(15)
      DTMP=ABS(SLA-SLB)
      IF(
     - DTMP.LT.SLOLIM
     -)THEN
      IB=0
      IF(
     - IBKK(16).NE.0.AND.IBFIT.EQ.0
     -)THEN
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
      ENDIF
      IF(
     - IB.EQ.0
     -)THEN
      INTFLG=ICROSS
      DTMP=ABS(DTMP*DX)
      ENDIF
      ENDIF
      KMP1=HISTR(1,NTR)
      KMP1=IABS(KMP1)
      KMP1=IPCL(KMP1)
      IF(KMP1.EQ.IPCL(KX).AND.IBKK(20).NE.0.AND.MAMB.EQ.0) IJFLG=1
      ENDIF
      ENDIF
      RETURN
17000 CONTINUE
      IB=0
      IF(
     - HNREL(NTR).LT.9
     -)THEN
      CALL MVC2(HTEMP(1),0,HISTR(1,NTR),0,18) !PMF 28/06/99 MVC -> MVC2
      IKST=IPST
      IKFLG=IJFLG
      IBJ=IKX
      IKX=KX
      LR3=1
      IF(ICROSS.EQ.-1.OR.(LAND(LBL(KX),MSKCR1).NE.0.AND.
     * LAND(LBL(KK),MSKCR1).EQ.0)) LR3=-1
      ASSIGN 17003 TO IZZZ02
      GOTO 17002
17003 CONTINUE
      HNREL(NTR)=HNREL(NTR)+1
      LRC=LR2
      HISTR(HNREL(NTR),NTR)=LRC*KX
      CALL BAKFIT(IB,1)
      IAB=HNREL(NTR)
      HNREL(NTR)=HNREL(NTR)-1
      CALL MVC2(HISTR(1,NTR),0,HTEMP(1),0,18) !PMF 28/06/99 MVC -> MVC2
      IKX=IBJ
      IJFLG=IKFLG
      IPST=IKST
      ENDIF
      GOTO IZZZ01
17002 CONTINUE
      CALL LFRT(LR2)
      IF(
     - LR2.EQ.0
     -)THEN
      IF(
     - LR.NE.0
     -)THEN
      LR2=LR*LR3
      ELSE
      LR2=LR3
      IJFLG=1
      IPST=1
      ENDIF
      ELSE
      IF(
     - LR.EQ.0
     -)THEN
      IF(LR2*LR3.EQ.-1) HISTR(1,NTR)=-HISTR(1,NTR)
      ELSE
      IF(
     - LR2.NE.LR*LR3
     -)THEN
      LR2=-LR2
      ENDIF
      ENDIF
      ENDIF
      GOTO IZZZ02
      END
      SUBROUTINE COREC
      IMPLICIT INTEGER*2 (H)
#include "cworkmx.for"
#include "cpatlm.for"
#include "cworkpr.for"
      ITMP=HNREL(NTR)
      DO 13000 I=1,ITMP
      HISTR(I,NTR)=-HISTR(I,NTR)
13000 CONTINUE
13001 CONTINUE
      IJFLG=0
      RETURN
      END
      SUBROUTINE SIDE1
      IMPLICIT INTEGER*2 (H)
#include "cworkpr.for"
#include "cworkmx.for"
#include "cpatlm.for"
#include "cdsmax.for"
#include "cjdrch.for"
      DIMENSION LSTCL(3),LFTCL(3),NCELL(3),TANDEL(3)
      EQUIVALENCE (IBCK(1),LSTCL(1)),(IBCK(4),LFTCL(1))
      EQUIVALENCE (IBCK(7),NCELL(1)),(DBCK(1),TANDEL(1))
      DIMENSION HTEMP(9)
      DATA MSKCR1 /Z100/,MSKFIT/Z20000/,MSKAIT/ZFFFDFFFF/
      IIWW=IW+1
      IF(IIWW.GT.16) IIWW=16
      IF(IIWW.LT.1) IIWW=1
      BSL=A
      IF(IUDFLG.EQ.6) BSL=-BSL
      DRIFT=DS+.5*BSL
      IRT=LR1
      IM1=6
      IM2=3
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
      IWEX=HMCH(IIWW,KRING,LR1)+1
      IF(IWEX.GT.16) IWEX=16
      IF(IWEX.LT.1) IWEX=1
      IF(
     - DTMP.LT.2.*CLIM.OR.DTMP.LT.2.
     -)THEN
      ASSIGN 17003 TO IZZZ02
      GOTO 17002
17003 CONTINUE
      IF(
     - IKX.GT.0
     -)THEN
      IF(
     - LRCORN.EQ.0
     -)THEN
      ICX=ICT
      IRIFLG=1
      KT=IKX
      ENDIF
      ELSE
      IWT=ILIM
      ENDIF
      ELSE
      IWT=ILIM
      ENDIF
      RETURN
17002 CONTINUE
      IKX=0
      ICNFLG=0
      IF(LRCORN.EQ.0) IRL=0
      NTRLX1 = HNTCEL(ICT)
      NTRLX2 = HNTCEL(ICT+1)-1
      DO 13000 KX = NTRLX1,NTRLX2
      IF(
     - HUSE(KX).EQ.0
     -)THEN
      IPER=0
      IF(
     - IBKK(20).NE.0.AND.NRHT(KX).GE.IBKK(19)
     -)THEN
      JT=IKX
      IKX=KX
      CALL LFRT(LK)
      IKX=JT
      IF(LAND(MSKCR1,LBL(KX)).NE.0.AND.IUDFLG.EQ.6) LK=-LK
      IF(LK.NE.3-2*LR1) IPER=1
      IF(LK.EQ.0) IPER=0
      ENDIF
      IF(
     - IPER.EQ.0
     -)THEN
      IIWW= ITRKAR(KX,IUDFLG)+1
      IF(IIWW.GT.16) IIWW=16
      IF(IIWW.LT.1) IIWW=1
      BSL=TRKAR(KX,IUDFLG+2)
      IF(IUDFLG.EQ.3) BSL=-BSL
      DRIFT=TRKAR(KX,IUDFLG+1)+.5*BSL
      IRT=3-LR1
      IM1=3
      IM2=6
      ASSIGN 17004 TO IZZZ01
      GOTO 17000
17004 CONTINUE
      IWX=IIWW
      IF(
     - DTMP.LT.2.*CLIM.OR.DTMP.LT.2.
     -)THEN
      IF(
     - IABS(IWX-IWEX).LE.IBKK(6)-1
     -)THEN
      LRS=2*LR1-3
      SLB=A/RINCR(KRING)
      SL=SLCOR(SLB,LRS)
      T=TANDEL(KRING)
      SLEX=(T-SL)/(1.+SL*T)
      LRS=-LRS
      SLE=TRKAR(KX,IUDFLG+2)/RINCR(KRING)
      SLC=SLCOR(SLE,LRS)
      SLOLIM=(ABS(SLE)+ABS(SLB))/2.*BKK(12)+BKK(13)
      DTMP=SLEX-SLC
      IF(
     - ABS(DTMP).LT.SLOLIM
     -)THEN
      IF(
     - LRCORN.NE.0
     -)THEN
      IKX=KX
      GOTO 13001
      ENDIF
      IB=0
      IF(
     - IBKK(17).NE.0.AND.IBFIT.EQ.0
     -)THEN
      ASSIGN 17006 TO IZZZ03
      GOTO 17005
17006 CONTINUE
      ENDIF
      IF(
     - IB.EQ.0
     -)THEN
      IRL=IRL+1
      DTEMP(IRL)=ABS(DTMP)
      ITK(IRL,1)=KX
      ITK(IRL,4)=KT
      ICNFLG=1
      ELSE
      ICNFLG=0
      ENDIF
      ELSE
      ICNFLG=0
      ENDIF
      IF(IRL.GT.0.AND.LRCORN.EQ.0) ICNFLG=1
      ENDIF
      ENDIF
      ELSE
      ENDIF
      ENDIF
13000 CONTINUE
13001 CONTINUE
      IF(
     - ICNFLG.EQ.1
     -)THEN
      IF(
     - IRL.GT.1
     -)THEN
      CALL CHOOSE
      ENDIF
      IKX=ITK(1,1)
      ENDIF
      GOTO IZZZ02
17000 CONTINUE
      CLIM=.5*BSL
      DXNEW=DSMAX(IIWW,KRING,IRT)-DRIFT
      DXOLD=9999.
15000 CONTINUE
      IF(
     - DXNEW.GT.CLIM
     -)THEN
      IF(
     - IIWW.EQ.16.OR.IIWW.EQ.1
     -)THEN
      GOTO 15001
      ENDIF
      DRIFT=DRIFT+BSL
      IF(IUDFLG.EQ.IM1) IIWW=IIWW-1
      IF(IUDFLG.EQ.IM2) IIWW=IIWW+1
      DSMX=DSMAX(IIWW,KRING,IRT)
      DXNEW=DSMX-DRIFT
      IF(
     - ABS(DXNEW).GT.ABS(DXOLD)
     -)THEN
      DTMP=100.
      GOTO 15001
      ENDIF
      IF(
     - DXNEW.LT.1.
     -)THEN
      GOTO 15001
      ELSE
      DXOLD=DXNEW
      ENDIF
      GOTO 15000
      ENDIF
15001 CONTINUE
      DTMP=DXNEW
      GOTO IZZZ01
17005 CONTINUE
      IB=0
      IF(
     - HNREL(NTR).LT.9
     -)THEN
      IKFLG=IJFLG
      CALL MVC2(HTEMP(1),0,HISTR(1,NTR),0,18)  !PMF 28/06/99 MVC -> MVC2
      IF(LR.EQ.-1.AND.LR1.EQ.2.OR.LR.EQ.1.AND.LR1.EQ.1) CALL COREC
      IF(LR.EQ.0.AND.LR1.EQ.1) HISTR(1,NTR)=-HISTR(1,NTR)
      HNREL(NTR)=HNREL(NTR)+1
      IF(LR1.EQ.2) LRC=-1
      IF(LR1.EQ.1) LRC=1
      IF(LAND(LBL(KX),MSKCR1).NE.0.AND.IUDFLG.EQ.6) LRC=-LRC
      IKRA=HISTR(1,NTR)
      IKRA=IABS(IKRA)
      IF(IKRA.EQ.KT.AND.IUDFLG.EQ.3.AND.LAND(MSKCR1,LBL(KT)).NE.0)
     * CALL COREC
      HISTR(HNREL(NTR),NTR)=KX*LRC
      IAB=HNREL(NTR)
      CALL BAKFIT(IB,3)
      HNREL(NTR)=HNREL(NTR)-1
      CALL MVC2(HISTR(1,NTR),0,HTEMP(1),0,18) !PMF 28/06/99 MVC -> MVC2
      IJFLG=IKFLG
      ENDIF
      GOTO IZZZ03
      END
      SUBROUTINE LFRT(LR2)
      IMPLICIT INTEGER*2 (H)
#include "cworkmx.for"
#include "cworkpr.for"
#include "cpatlm.for"
      DATA MSKLFT,MSKRT/Z400,Z800/
      LR2=0
      IPST=0
      I1=LAND(LBL(IKX),MSKLFT)
      I2=LAND(LBL(IKX),MSKRT)
      IF(I1.NE.0) LR2=-1
      IF(I2.NE.0) LR2=1
      IF(I1*I2.NE.0) LR2=0
      IF(LR2.NE.0) IPST=1
      RETURN
      END
