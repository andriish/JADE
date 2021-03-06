C   16/02/84 711162037  MEMBER NAME  DEDXV10  (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE DEDXV1
C-----------------------------------------------------------------------
C
C   ADAPTED FROM J.OLSSON'S DEDXVW
C
C             J.A.J.SKARD   28/08/86 :
C   LAST MOD: E ELSEN       13/11/87 : NOW ALSO WORKS FOR MC
C
C         SHOW DEDX FOR EACH WIRE VS SQRT(R**2+Z**2) FOR ONE TRACK
C
C         DEDX NN     NN>0 SHOWS DEDX PER WIRE FOR TRACK NN
C
C         THE TRAILING INTEGER FROM DEDX COMMAND IS IN ACMD IN /CGRAPH/
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C
#include "cgraph.for"
#include "cdata.for"
#include "chsym.for"
#include "cjdrch.for"
C
      COMMON / CALIBR / IPOINT(100)
       COMMON /CWORK1/ IER,NTR ,TRES(10,60),EWIR(60),XWIR(60),YWIR(60),
     *                 RWIR(60),ZWIR(60),HMW(132)
C
      DIMENSION XCAL(1000), ITRES(10,60)
      EQUIVALENCE (IPOINT(1),XCAL(1))
      EQUIVALENCE (TRES(1,1),ITRES(1,1))
      DIMENSION ARR(60)
C
C------------------  C O D E  ------------------------------------------
C
      NN = ACMD
      NTRR = MIN0(NTR,60)
C     WRITE(6,3204)NTRR,NN
C3204 FORMAT(' *** DEDXV1: NTRR,NN=',2I6)
      IF( .NOT. (NTRR.GT.0.AND.NN.GT.0.AND.NN.LE.NTRR) ) GO TO 998
C
C                                           NUMBER OF HITS
      ITR=NN
      NHF = ITRES(1,ITR)
      IF( NHF.LE.0 ) GO TO 998
C
C                            DEFINE WINDOW SIZE
C
      CALL ERASE
        CALL TWINDO(0,4095,0,4095)
      XMINRE = XMIN
      XMAXRE = XMAX
      YMINRE = YMIN
      YMAXRE = YMAX
C        FIND MAX AND MIN (NON-ZERO) ENERGY DEPOSIT VALUE, AND X-SCALE
      EMIN=1.E10
      EMAX=0.
      XHI=0.
      DO 2801 I=1,NHF
      IF(ABS(EWIR(I)).LT.1.E-10)GO TO 2801
      XTMP=RWIR(I)**2+ZWIR(I)**2
      IF(EWIR(I).LT.EMIN)EMIN=EWIR(I)
      IF(EWIR(I).GT.EMAX)EMAX=EWIR(I)
      IF(XTMP.GT.XHI)XHI=XTMP
 2801 CONTINUE
      EINTER=AMAX1(10.,EMAX-EMIN)
      XHI=(IFIX(SQRT(XHI)/100.)+2)*100
      XMIN=-80.
      XMAX=AMAX1(1100.,XHI)+120.
      XVMIN=100.0
      XVMAX=XMAX-220.
      YVMIN=IFIX(EMIN-1.-EINTER/8.)
      IF(EINTER/EMIN.GT.2.0)YVMIN=0.
      YVMAX=IFIX(EMAX+1.+EINTER/8.)
      YMIN=YVMIN-(YVMAX-YVMIN)/8.
      YMAX=YMIN+(YVMAX-YMIN)/0.60
C     WRITE(6,3205)XMIN,XMAX,YMIN,YMAX,EMAX,EINTER
C3205 FORMAT(' *** DEDXV1: XMIN,XMAX,YMIN,YMAX=',4F10.2,/,
C    *       '             EMAX,EINTER=',2F10.2)
C     WRITE(6,3206)XVMIN,XVMAX,YVMIN,YVMAX
C3206 FORMAT(' *** DEDXV1: XVMIN,XVMAX,YVMIN,YVMAX=',4F10.2)
      CALL DWINDO(XMIN,XMAX,YMIN,YMAX)
C
C                            DRAW DIAGRAM FRAME
C
      CALL MOVEA(XVMIN,YVMIN)
      CALL DRAWA(XVMAX,YVMIN)
      CALL DRAWA(XVMAX,YVMAX)
      CALL DRAWA(XVMIN,YVMAX)
      CALL DRAWA(XVMIN,YVMIN)
C
C                            DRAW SCALE MARKERS
C
      CALL CHRSIZ(2)
      CALL CSIZE(IHOR,IVER)
      SSX = IHOR*(XMAX-XMIN)/4096.
      SSY = IVER*(YMAX-YMIN)/6240.
      XRANG=XVMAX-XVMIN
      YRANG=YVMAX-YVMIN
      XTICK=YRANG*0.015
      YTICK=XRANG*0.01
      XTDIS=200.
      IF(XRANG.LT.2000.)XTDIS=100.
      IF(XRANG.LT.1000.)XTDIS=50.
      IF(XRANG.LT.400.)XTDIS=10.
      YTDIS=50.
      IF(YRANG.LT.300.)YTDIS=10.
      IF(YRANG.LT.100.)YTDIS=5.
      IF(YRANG.LT.30.)YTDIS=2.
      IF(YRANG.LT.10.)YTDIS=0.5
      IF(YRANG.LT.3.)YTDIS=0.1
      IF(YRANG.LT.0.5)YTDIS=0.02
C
C                            WRITE TO INTERNAL ARRAY HMW
C                            THEN OUTPUT EBCDIC CHARACTERS ON SCREEN
C
      XCURR=XVMIN
      YCURR=YVMIN
 2802 CONTINUE
      IF(XCURR+XTDIS.GE.XVMAX)GO TO 503
      ITEST=(XCURR-XVMIN)*1.0001/XTDIS
      IF(MOD(ITEST,2).NE.0)GO TO 502
C        TICK LABELS
      CALL MOVEA(XCURR-3.0*SSX,YCURR-2.5*SSY)
      IXCURR=XCURR
      CALL CORE(HMW,5)
      WRITE(JUSCRN,245)IXCURR
245   FORMAT(' ',I4)
      CALL EOUTST(5,HMW)
C
C        TICK MARKS
C
502   XCURR=XCURR+XTDIS
C     WRITE(6,3207)XCURR,YCURR
C3207 FORMAT(' *** DEDXV1: X-TICK MARK AT X,Y=',2F10.2)
      CALL MOVEA(XCURR,YCURR)
      CALL DRAWA(XCURR,YCURR+XTICK)
      GO TO 2802
C
503   CONTINUE
      XCURR=XVMIN
      YCURR=YVMIN
2803  CONTINUE
      NYTICK=IFIX((YVMAX-YVMIN)/YTDIS)
      ITEST=(YCURR-YVMIN)*1.0001/YTDIS
      IF(NYTICK.LE.8)GO TO 601
      IF(MOD(ITEST,2).NE.0)GO TO 504
  601 IF(YCURR+2.*YTDIS.GE.YVMAX)GO TO 504
C        TICK LABELS
      IYCURR=YCURR+0.001
      IF(YCURR-FLOAT(IYCURR).GT.0.3)GO TO 504
      CALL MOVEA(XCURR-4.5*SSX,YCURR-0.5*SSY)
      CALL CORE(HMW,4)
      WRITE(JUSCRN,246)IYCURR
246   FORMAT(' ',I3)
      CALL EOUTST(4,HMW)
C        TICK MARKS
504   CONTINUE
C     WRITE(6,3208)XCURR,YCURR
C3208 FORMAT(' *** DEDXV1: Y-TICK MARK AT X,Y=',2F10.2)
      CALL MOVEA(XCURR,YCURR)
      CALL DRAWA(XCURR+YTICK,YCURR)
      YCURR=YCURR+YTDIS
      IF(YCURR.LT.YVMAX)GO TO 2803
C
C                            DEDX PLOT PER WIRE
C
        DEDX   = TRES(2,ITR)
        XBAR = XRANG*0.007
        YBAR = YRANG*0.007
C
C        LOOP OVER POINTS ON TRACK
C
      DO 505 NP=1,NHF
      IF(EWIR(NP).LT.0.01)GO TO 505
        SWIR = SQRT(RWIR(NP)**2+ZWIR(NP)**2)
        CALL MOVEA( SWIR-XBAR, EWIR(NP))
        CALL DRAWA( SWIR+XBAR, EWIR(NP))
C
C                            DRAW ERROR BAR
C
        CALL MOVEA(SWIR,EWIR(NP)+YBAR)
        CALL DRAWA(SWIR,EWIR(NP)-YBAR)
  505 CONTINUE
C
C        DRAW TRUNCATED MEAN DEDX
C
      CALL MOVEA(XVMIN,DEDX)
      CALL DRAWA(XVMAX,DEDX)
C
C        TRUNCATED MEAN DEDX FOR EACH RING SEPARATELY
C
      DO 515 IR=1,3
      NPC=0
      DO 514 I=1,NHF
      IF(RWIR(I).LE.RDEC(IR).OR.RWIR(I).GT.RDEC(IR+1))GO TO 514
      NPC=NPC+1
      ARR(NPC)=EWIR(I)
514   CONTINUE
      IF(NPC.EQ.0)GO TO 515
      CALL DSORT(NPC,ARR,TRCM,DTRCM)
      XCURR=XVMAX+30.*IR
      YCURR=TRCM
      CALL MOVEA(XCURR-XBAR,YCURR)
      CALL DRAWA(XCURR+XBAR,YCURR)
      CALL MOVEA(XCURR,YCURR-DTRCM)
      CALL DRAWA(XCURR,YCURR+DTRCM)
515   CONTINUE
C
C                            DRAW AXES CAPTIONS (LOWER CASE!)
C
      CALL CHRSIZ(2)
      CALL CSIZE(IHOR,IVER)
      SSX=IHOR*(XMAX-XMIN)/4096.
      SSY=IVER*(YMAX-YMIN)/6240.
      CALL MOVEA(XVMAX-XRANG*0.1,YVMIN-4.5*SSY)
      CALL CORE(HMW,3)
      WRITE(JUSCRN,3209)
3209  FORMAT(' mm')
      CALL EOUTST(3,HMW)
C
      CALL MOVEA(XVMAX,YVMIN)
      CALL DRAWA(XVMAX,YVMIN-4.5*SSY)
      CALL MOVEA(XVMAX+15.,YVMIN-2.5*SSY)
      CALL CORE(HMW,9)
      WRITE(JUSCRN,3214)
3214  FORMAT('Trc. mean')
      CALL EOUTST(9,HMW)
      CALL MOVEA(XVMAX+15.,YVMIN-4.5*SSY)
      CALL CORE(HMW,9)
      WRITE(JUSCRN,3215)
3215  FORMAT(' per ring')
      CALL EOUTST(9,HMW)
C
      CALL MOVEA(XVMAX-0.5*XRANG-8.*SSX,YVMIN-4.5*SSY)
      CALL CORE(HMW,16)
      WRITE(JUSCRN,3210)
3210  FORMAT(' SQRT(R**2+Z**2)')
      CALL EOUTST(16,HMW)
C
      CALL MOVEA(XVMIN-7.5*SSX,YVMAX-3.*SSY)
      CALL CORE(HMW,7)
      WRITE(JUSCRN,3211)
3211  FORMAT(' KeV/cm')
      CALL EOUTST(7,HMW)
C
      IF(NN.GE.10)GO TO 518
      CALL MOVEA(AMAX1(XVMIN-8.5*SSX,XMIN),YVMAX+0.5*SSY)
      CALL CORE(HMW,8)
      WRITE(JUSCRN,3212)NN
3212  FORMAT(' Track',I2)
      CALL EOUTST(8,HMW)
      GO TO 519
 518  CALL MOVEA(AMAX1(XVMIN-9.5*SSX,XMIN),YVMAX+0.5*SSY)
      CALL CORE(HMW,9)
      WRITE(JUSCRN,3213)NN
3213  FORMAT(' Track',I3)
      CALL EOUTST(9,HMW)
 519  CONTINUE
      CALL MOVEA(XVMIN+0.5*(XVMAX-XVMIN)-7.*SSX,YVMAX+0.5*SSY)
      CALL CORE(HMW,14)
      WRITE(JUSCRN,3216)
 3216 FORMAT('dE/dx per wire')
      CALL EOUTST(14,HMW)
C***
C***                    WRITE CAPTION
C***
      INDES = -1
      XMIN=-10.
      YMIN=-10.
      XMAX=50.
      YMAX=50
      CALL DWINDO(XMIN,XMAX,YMIN,YMAX)
      CALL CAPMRK(INDES,IESUM)
998   CALL TWINDO(0,4095,0,4095)
      XMIN = XMINRE
      XMAX = XMAXRE
      YMIN = YMINRE
      YMAX = YMAXRE
      CALL SETSCL(LASTVW)
 999  RETURN
      END
