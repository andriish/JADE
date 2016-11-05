C   20/12/85 807241307  MEMBER NAME  BANKLS   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE BANKLS( INDEX, IPPATR )
C-----------------------------------------------------------------------
C
C    DISPLAY TABLE OF RESULTS FOR PATTERN RECOGNITION BANK "PATR"
C        IPO:          START ADRESS OF BANK CONTENT
C        NBK:          BOS NUMBER OF BANK
C        INDEX:        VIEW INDEX ACCORDING TO DISPLAY PROGRAM
C             J.OLSSON,   1.3.79                CHANGE 06.12.82
C             C.BOWDERY                         CHANGE 20.12.85
C             J.HAGEMANN                   LAST CHANGE 12.01.88
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL DSPDTM
C
#include "cdata.for"
#include "cgraph.for"
#include "cgeo1.for"
C
      COMMON / CGRAP2 / BCMD,DSPDTM(30),ISTVW,JTVW
      COMMON / CHEADR / HEAD(108)
      COMMON / CJTRIG / PI,TWOPI
      COMMON / CWORK2 / HWORK(40),JNDEX,NTR,LTR,ITR,IPO,
     +                  ICNT,NBK,NCLST,NWPCL,
     +                  PMOM,PZ,PTRANS,RMS,NHTF,RAD,RAD1,THE,
     +                  PHI,XXX,YYY,SSS,IPP,IHO,IVE,
     +                  HSIGN,HDUM,NTRRES,IW52
C
      DATA  LNAME/4HLGCL/, HMINUS/2H- /
*** PMF 17/11/99: add variable needed for emulation of DESYLIB routine 'CORE'  
      CHARACTER cHWORK*80
      EQUIVALENCE (cHWORK,HWORK(1))
      REAL SF/.75/
*** PMF(end)
CC
C------------------  C O D E  ------------------------------------------
C
      IFL = 0
      IF(DSPDTL(14).AND.ACMD.NE.0..AND.LSTCMD.NE.111.AND.LSTCMD.NE.112)
     $ IFL = 1
      IPP = IPO
      ICNT = 0
      JCNT = 0
      CALL CLOC(IMR20,'MUR2',0)
      CALL CLOC(IMR21,'MUR2',1)
C
      IMBOTH=IMR20*IMR21
C     IF(ACMD.NE.0.) IMBOTH=0
      IF(IMBOTH.EQ.0) GO TO 7704
      IPMUR2=IMR21
      IMSTEP=IDATA(IMR20+2)
7704  CONTINUE
      IF(DSPDTL(29)) ICNT = NTR
C
      XXX = XMIN
      YYY = YMIN+.76*(YMAX-YMIN) *1.05 ! PMF 20/11/99 add factor 1.05
      CALL XXXYYY(XXX,YYY,SSS,0)
      IF(DSPDTL(29)) GO TO 2117
C     IF(DSPDTL(14).AND.ACMD.NE.0.) GO TO 2277
      IF(IFL.NE.0) GO TO 2277
C
C                            IF OPT 46 IS ON AND OPT 13 IS OFF
C                            FOR PUBLICATION PICTURES WITHOUT CAPTIONS,
C                            SUPPRESS THE RESULTS HEADER.
C
      IF( DSPDTM(16)  .AND.  .NOT. DSPDTL(13) ) GO TO 221
C
        call setcol('TRCK')! PMF 24/11/99 set colour
        CALL CORE(HWORK,80)
        INAME = IDATA(IPPATR-3)
        WRITE(cHWORK,220) INAME,NBK,NTR ! PMF 17/11/99: UNIT=10 changed to cHWORK
220     FORMAT('BANK ',A4,I3,'  NR OF TRACKS ',I3)
        CALL SYSSYM(XXX,YYY,SSS,HWORK,31,0.)
C
221   IF( .NOT. DSPDTL(13) ) GO TO 2277
      YYY = YYY - 2.*SSS *sf ! PMF 20/11/99: add factor sf
      CALL CORE(HWORK,80)
      WRITE(cHWORK,219)         ! PMF 17/11/99: UNIT=10 changed to cHWORK
219   FORMAT('NR +-  RMSRFI RMSRZ/HIT  PHI')
      CALL SYSSYM(XXX,YYY,SSS,HWORK,28,0.)
      YYY = YYY - 1.5*SSS *sf ! PMF 20/11/99: add factor sf
      CALL CORE(HWORK,80)
      WRITE(cHWORK,218)         ! PMF 17/11/99: UNIT=10 changed to cHWORK
218   FORMAT('PTOT  PLONG  PTRANS   COSTHE')
      CALL SYSSYM(XXX,YYY,SSS,HWORK,28,0.)
      IF(IMBOTH.EQ.0) GO TO 2277
      YYY = YYY - 1.5*SSS *sf ! PMF 20/11/99: add factor sf
      CALL CORE(HWORK,80)
      WRITE(cHWORK,7706)        ! PMF 17/11/99: UNIT=10 changed to cHWORK
7706  FORMAT(' MUQUAL  CHIP  MUPR   PIPR')
      CALL SYSSYM(XXX,YYY,SSS,HWORK,26,0.)
C
2277  IF(IFL.NE.0) YYY = YYY - 8.*SSS *sf ! PMF 20/11/99: add factor sf
C2277  IF(DSPDTL(14).AND.ACMD.NE.0.) YYY = YYY - 8.*SSS
C     IF(.NOT.DSPDTL(14).OR.ACMD.EQ.0.) YYY = YYY - 4.5*SSS
      IF(IFL.NE.0.AND.IMBOTH.NE.0) YYY = YYY - 1.5*SSS *sf ! PMF 20/11/99: add factor sf
      IF(IFL.EQ.0) YYY = YYY - 4.5*SSS *sf ! PMF 20/11/99: add factor sf
      SUMP = 0.
      SUMPT = 0.
      SUMPZ = 0.
      LADSUM = 0
200   ICNT = ICNT + 1
      JCNT = JCNT + 7
      IF(ICNT.GT.NTR) GO TO 21
      IF(JCNT.LT.187) GO TO 7702
      JCNT=-10000
      XXX = XMIN+.82*(XMAX-XMIN)*1.03 !PMF 20/1/99: add factor 1.03
      YYY = YMIN+.76*(YMAX-YMIN)*1.05 ! PMF 20/11/99 add factor 1.05
      CALL XXXYYY(XXX,YYY,SSS,0)
      YYY = YYY - 12.*SSS *sf ! PMF 20/11/99: add factor sf
      IF(LASTVW.EQ.2) YYY = YYY - 2.*SSS *sf ! PMF 20/11/99: add factor sf
7702  IPP = IPP + LTR
      CALL TRTEXT(IPPATR,IPP,XXX,YYY,SSS)
      IF( IMBOTH .EQ. 0  .OR.  .NOT. DSPDTL(13) ) GO TO 7703
      IQUALF=IDATA(IPMUR2+6)
      CHIP=ADATA(IPMUR2+11)
      AMUPR=ADATA(IPMUR2+25)
      PIPR=ADATA(IPMUR2+26)
      IPMUR2=IPMUR2+IMSTEP
C
C                             LSTCMD = 55 IS MUR2
C
      IF(IQUALF.LE.0.AND.LSTCMD.NE.55) GO TO 7703
      JCNT=JCNT+3
      IF(DSPDTL(14).AND.NTRRES.NE.IDATA(IPP+1).AND.NTRRES.NE.0) GO TO 7
      CALL CORE(HWORK,80)
      WRITE(cHWORK,7705) IQUALF,CHIP,AMUPR,PIPR ! PMF 17/11/99: UNIT=10 changed to cHWORK
7705  FORMAT(I5,F8.2,F6.2,F7.3)
      YYY=YYY+0.5*SSS *sf ! PMF 20/11/99: add factor sf
      CALL SYSSYM(XXX,YYY,SSS,HWORK,26,0.)
      YYY=YYY-2.0*SSS *sf ! PMF 20/11/99: add factor sf
      GO TO 7703
7     YYY = YYY - 1.5*SSS *sf ! PMF 20/11/99: add factor sf
7703  CONTINUE
C     IF(DSPDTL(14).AND.ACMD.NE.0.) GO TO 200
      IF(IFL.NE.0) GO TO 200
      SUMP = SUMP + PMOM
      SUMPT = SUMPT + PTRANS
      SUMPZ = SUMPZ + ABS(PZ)
      LADSUM = LADSUM + 1
      IF(HSIGN.EQ.HMINUS) LADSUM = LADSUM - 2
      GO TO 200
21    ICNT = ICNT - 1
      JCNT = JCNT - 7
2117  IF(.NOT.DSPDTL(28).AND..NOT.DSPDTL(29)) GO TO 2121
C                            LSTCMD = 52 IS VRES
      IF(LSTCMD.EQ.52.AND.IW52.EQ.0) GO TO 2121
      IF(NTRRES.NE.0) GO TO 2121
C
C                            WRITE RESULTS FOR LEAD GLASS CLUSTERS
C
      IPLGCL = IDATA(IBLN('LGCL'))
      IF(IPLGCL.EQ.0) GO TO 2121
      NBK = IDATA(IPLGCL-2)
      NWPCL = IDATA(IPLGCL+25)
      IPP = IDATA(IPLGCL+3) + IPLGCL - 1 - NWPCL
      NCLST = IDATA(IPLGCL+7)
      ISTEP = IDATA(IPLGCL+21)
      IPPATR=IDATA(IBLN('PATR'))
      IPALGN=IDATA(IBLN('ALGN'))
      IF(ISTEP.NE.2) CALL LGCDIR(IPPATR,IPALGN,IPLGCL)
      IF(JCNT.LT.172) GO TO 2118
      JCNT=-10000
      XXX = XMIN+.82*(XMAX-XMIN)*1.03 !PMF 20/1/99: add factor 1.03
      YYY = YMIN+.76*(YMAX-YMIN)*1.05 ! PMF 20/11/99 add factor 1.05
      CALL XXXYYY(XXX,YYY,SSS,0)
      YYY = YYY - 9.*SSS *sf ! PMF 20/11/99: add factor sf
      IF(LASTVW.EQ.2) YYY = YYY - 2.*SSS *sf ! PMF 20/11/99: add factor sf
2118  YYY = YYY - 3.*SSS *sf ! PMF 20/11/99: add factor sf
C
C                            IF OPT 46 IS ON AND OPT 13 IS OFF
C                            FOR PUBLICATION PICTURES WITHOUT CAPTIONS,
C                            SUPPRESS THE RESULTS HEADER.
C
      IF( DSPDTM(16)  .AND.  .NOT. DSPDTL(13) ) GO TO 2201
C
        call setcol('ECAL')! PMF 24/11/99 set colour
        CALL CORE(HWORK,80)
        WRITE(cHWORK,2200) LNAME,NBK,NCLST ! PMF 17/11/99: UNIT=10 changed to cHWORK
2200    FORMAT('BANK ',A4,I2,' NR OF CLUSTERS',I3)
        CALL SYSSYM(XXX,YYY,SSS,HWORK,29,0.)
C
2201  YYY = YYY - 2.*SSS *sf ! PMF 20/11/99: add factor sf
      IF( .NOT. DSPDTL(13) ) GO TO 2121
      IGMX = 0
      IICNT = 0
2000  ICNT = ICNT + 1
      JCNT = JCNT + 7
      IICNT = IICNT + 1
      IF(ICNT.GT.NCLST+NTR) GO TO 2121
      IF(JCNT.LT.182) GO TO 7712
      JCNT=-10000
      XXX = XMIN+.82*(XMAX-XMIN)*1.03 !PMF 20/1/99: add factor 1.03
      YYY = YMIN+.76*(YMAX-YMIN)*1.05 ! PMF 20/11/99 add factor 1.05
      CALL XXXYYY(XXX,YYY,SSS,0)
      YYY = YYY - 12.*SSS *sf ! PMF 20/11/99: add factor sf
      IF(LASTVW.EQ.2) YYY = YYY - 2.*SSS *sf ! PMF 20/11/99: add factor sf
7712  IPP = IPP + NWPCL
      IF(IDATA(IPP+8).EQ.0) IGMX = IGMX + 1
      CALL GMTEXT(IPP,NTRRES,IICNT,IGMX)
      GO TO 2000
2121  CONTINUE
C     IF(DSPDTL(14).AND.ACMD.NE.0.) RETURN
      IF(IFL.NE.0) RETURN
C
C                            WRITE OUT SUMS
C
      XXX = XMIN+.33*(XMAX-XMIN)
      YYY = YMIN+.01*(YMAX-YMIN)
      CALL XXXYYY(XXX,YYY,SSS,1)
      IF(DSPDTL(29).AND.IPLGCL.NE.0) GO TO 2119
      IF(DSPDTL(29)) RETURN
C
C                            IF OPT 46 IS ON AND OPT 13 IS OFF
C                            FOR PUBLICATION PICTURES WITHOUT CAPTIONS,
C                            SUPPRESS THE SUMS PRINTOUT.
C
      IF( DSPDTM(16)  .AND.  .NOT. DSPDTL(13) ) RETURN
C
      CALL CORE(HWORK,80)
      call setcol('TEXT')! PMF 24/11/99: set colour
      WRITE(cHWORK,865) SUMP,SUMPT,SUMPZ,LADSUM ! PMF 17/11/99: UNIT=10 changed to cHWORK
865   FORMAT(' *** SUMS (GEV) ***    PTOT ',F7.3,'   PTRANS ',F7.3,
     $' PLONG ',F7.3,' CHARGE ',I3)
      CALL SYSSYM(XXX,YYY,SSS,HWORK,77,0.)
C
      IF(.NOT.DSPDTL(28).OR.IPLGCL.EQ.0) goto 999!RETURN
C
      YYY = YYY - 1.5*SSS *sf ! PMF 20/11/99: add factor sf
 2119 CALL CORE(HWORK,80)
      call setcol('TEXT')! PMF 24/11/99: set colour
      WRITE(cHWORK,866)
     +     ADATA(IPLGCL+11),ADATA(IPLGCL+16),IDATA(IPLGCL+15) ! PMF 17/11/99: UNIT=10 changed to cHWORK
866   FORMAT(' TOTAL CLUSTER ENERGY ',F7.3,'  PHOTON ENERGY ',F7.3,' NR
     $OF PHOTONS ',I3)
      CALL SYSSYM(XXX,YYY,SSS,HWORK,70,0.)
C
 999  continue
      call setcol(' ')! PMF 24/11/99: reset colour
      RETURN
      END
