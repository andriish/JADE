C   26/04/84 703121401  MEMBER NAME  FWDISP   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE FWDISP( IVIEW )
C-----------------------------------------------------------------------
C
C
C   AUTHOR:    J. OLSSON    10.3.1987:  DISPLAY 4V IMPACTS IN FW VIEW
C                                       DISPLAY TAGAN RESULTS IN FW VIEW
C
C  LAST MODIFICATION    12.3.1987
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C----------------------------------------------------------------------
C             MACRO CDATA .... BOS COMMON.
C
C             THIS MACRO ONLY DEFINES THE IDATA/HDATA/ADATA NAMES.
C             THE ACTUAL SIZE OF /BCS/ IS FIXED ON MACRO CBCSMX
C             OR BY OTHER MEANS. A DEFAULT SIZE OF 40000 IS GIVEN HERE.
C
C----------------------------------------------------------------------
C
      COMMON /BCS/ IDATA(40000)
      DIMENSION HDATA(80000),ADATA(40000),IPNT(50)
      EQUIVALENCE (HDATA(1),IDATA(1),ADATA(1)),(IPNT(1),IDATA(55))
      EQUIVALENCE (NWORD,IPNT(50))
C
C------------------------ END OF MACRO CDATA --------------------------
C-----------------------------------------------------------------------
C                            MACRO CGRAPH .... GRAPHICS COMMON
C-----------------------------------------------------------------------
C
      LOGICAL DSPDTL,SSTPS,PSTPS,FREEZE
C
      COMMON / CGRAPH / JUSCRN,NDDINN,NDDOUT,IDATSV(11),ICREC,MAXREC,
     +                  LSTCMD,ACMD,LASTVW,ISTANV,
     +                  SXIN,SXAX,SYIN,SYAX,XMIN,XMAX,YMIN,YMAX,
     +                  DSPDTL(30),SSTPS(10),PSTPS(10),FREEZE(30),
     +                  IREADM,LABEL,LSTPS(10),IPSVAR
C
C------- END OF MACRO CGRAPH -------------------------------------------
C
C-----------------------------------------------------------------------
C                            MACRO CGEO2 .... JADE TAGGING GEOMETRY
C-----------------------------------------------------------------------
C
      COMMON / CGEO2 / FENDC,XYHOL1,XYHOL2,BLDPFW,ZMINBL,ZPLUBL,
     +                 XSC(2),YSC(2),RSC(2),ZMISC(2),ZPLSC(2),DZSC,
     +                 CHX(3,4),CHY(3,4),CHZ(3,4),WLEN,PITCH,WZDIS
C
C--------------------------- END OF MACRO CGEO2 ------------------------
C
C----------------------------------------------------------------------
C      MACRO CGEO3 .... JADE FORWARD DETECTOR GEOMETRY, 1981-3 VERSION
C----------------------------------------------------------------------
C
      COMMON / CGEO3 / ZPLUM2,ZMINM2,NRPBSC,PBSCR(4),PBSCZ(4)
C
C------------------------ END OF MACRO CGEO3 --------------------------
C
C
      COMMON /CHEADR/ HEAD(108)
      COMMON /CJTRIG/ PI,TWOPI
C
      DIMENSION HWORK(50)
      DIMENSION ZSC(2)
      EQUIVALENCE (ZSC(1),ZMINBL)
      LOGICAL*1 LCH,LPLUS,LMINU,LNULL
      DATA LPLUS/'+'/, LMINU/'-'/, LNULL/'0'/
      DATA INAME/'TAGG'/, NBK/2/, INAMV/'VECT'/, NBKV/0/
*** PMF 17/11/99: add variables needed for emulation of DESYLIB routine 'CORE'  
      CHARACTER cHWORK*100
      EQUIVALENCE (cHWORK,HWORK(1))
*** PMF(end)
C
C------------------  C O D E  ------------------------------------------
C
      IF(LASTVW.NE.12) GO TO 9999
      ADX = (XMAX-XMIN) * .25
      ADY = 0.
      RADI = FENDC*.5
C
      IPVECT = IDATA(IBLN('VECT'))
      IF(IPVECT.GT.0) GO TO 2633
      IF(HEAD(18).LT.100) WRITE(6,2526)
2526  FORMAT(' NO VECT BANK ....... ')
      GO TO 999
C
2633  IVPS = 0
15001 LO = IDATA(IPVECT+1)
      LTR = IDATA(IPVECT+2)
      NTRVEC = IDATA(IPVECT+4)
C
      IPO  = IPVECT + LO
      ICNT = 0
15005 ICNT = ICNT + 1
      IF( ICNT .GT. NTRVEC.AND.IVPS.EQ.1) GO TO 888
      IF( ICNT .GT. NTRVEC.AND.IVPS.EQ.0) GO TO 15010
15007 IPP = IPO + (ICNT-1)*LTR
C
C CODE FOR FI AND THETA
C
      PX = ADATA(IPP+1)
      PY = ADATA(IPP+2)
      PZ = ADATA(IPP+3)
C
      XFRO = 0.
      YFRO = 0.
      XBAC = 0.
      YBAC = 0.
C
      IF(ABS(PX).LT..001.AND.ABS(PY).LT..001) GO TO 7400
      RPP4 = SQRT(PX*PX + PY*PY)
      IF(ABS(PZ).LT.10.*RPP4) GO TO 15005
      TGTHE = RPP4/ABS(PZ)
      RFRO = PBSCZ(3)*TGTHE
      RBAC = PBSCZ(4)*TGTHE
      PHI = ATAN2(PY,PX)
      IF(PHI.GT.TWOPI) PHI = PHI - TWOPI
      IF(PHI.LT.0.) PHI = PHI + TWOPI
      XFRO = RFRO*COS(PHI)
      YFRO = RFRO*SIN(PHI)
      XBAC = RBAC*COS(PHI)
      YBAC = RBAC*SIN(PHI)
C
7400  XOFF = ADX
      IF(PZ.LT.0.) XOFF = -ADX
      YOFF = ADY
      XXX = XOFF - XFRO * 1.5
      YYY = YOFF + YFRO * 1.5
      XXX1 = XOFF - XBAC * 1.5
      YYY1 = YOFF + YBAC * 1.5
C
      CALL PLYGON(10,RADI*.6,XXX,YYY,0)
      CALL DRAMOV(XXX,YYY,XXX1,YYY1,0)
C
      CALL NUMBWR(30,ICNT,XXX-3.*RADI,YYY,RADI)
C
      GO TO 15005
15010 CONTINUE
C     IPVECT = IDATA(IPVECT-1)
C     IF(IPVECT.LT. 1 ) GO TO 888
      GO TO 888
C SECOND VECT BANK     SECONDARIES BLUR THE DISPLAY, DISCARD...
C     IVPS = 1
C     GO TO 15001
C
C   WRITE TRACK INFORMATION ON RIGHTHAND SIDE OF PICTURE
C
888   XXX = XMIN+.79*(XMAX-XMIN)
      YYY = YMIN+.76*(YMAX-YMIN)
      CALL XXXYYY(XXX,YYY,SSS,0)
C
        CALL CORE(HWORK,80)
        WRITE(cHWORK,320) INAMV,NBKV,NTRVEC ! PMF 17/11/99: UNIT=10 changed to cHWORK
320     FORMAT(' BANK ',A4,I3,' NR OF 4-VECTORS',I3)
        CALL SYSSYM(XXX,YYY,SSS,HWORK,32,0.)
C
      YYY = YYY - 2.*SSS
      CALL CORE(HWORK,80)
      WRITE(cHWORK,319)         ! PMF 17/11/99: UNIT=10 changed to cHWORK
319   FORMAT('NR  +-   X       Y       PHI(DEG)')
      CALL SYSSYM(XXX,YYY,SSS,HWORK,33,0.)
      YYY = YYY - 1.5*SSS
      CALL CORE(HWORK,80)
      WRITE(cHWORK,318)         ! PMF 17/11/99: UNIT=10 changed to cHWORK
318   FORMAT('ETOT     PT    PZ    MASS    COSTHE')
      CALL SYSSYM(XXX,YYY,SSS,HWORK,35,0.)
C                    |     |       |      |
      YYY = YYY - 4.5*SSS
C
      IPO  = IPVECT + LO
      ICNT = 0
300   ICNT = ICNT + 1
      IF(ICNT.GT.NTRVEC) GO TO 31
      IPP = IPO + (ICNT-1)*LTR
C
C CODE FOR FI AND THETA
C
      PX = ADATA(IPP+1)
      PY = ADATA(IPP+2)
      PZ = ADATA(IPP+3)
      ET = ADATA(IPP+4)
      XM = ADATA(IPP+5)
      IC = IDATA(IPP+6)
C
      XFRO = 0.
      YFRO = 0.
      RPP4 = 0.
      TPHI = 0.
      COSTH = 1.0
      IF(PZ.LT.0.) COSTH = -1.0
      LCH = LNULL
      IF(IC.GT.0) LCH = LPLUS
      IF(IC.LT.0) LCH = LMINU
C
      IF(ABS(PX).LT..001.AND.ABS(PY).LT..001) GO TO 350
      RPP4 = SQRT(PX*PX + PY*PY)
      IF(ABS(PZ).LT.10.*RPP4) GO TO 300
      TGTHE = RPP4/ABS(PZ)
      RFRO = PBSCZ(3)*TGTHE
      RBAC = PBSCZ(4)*TGTHE
      PHI = ATAN2(PY,PX)
      IF(PHI.GT.TWOPI) PHI = PHI - TWOPI
      IF(PHI.LT.0.) PHI = PHI + TWOPI
      TPHI = PHI*180./PI
      THE = ATAN2(RPP4,PZ)
C     IF(PZ.LT.0.) THE = PI - THE
      COSTH = COS(THE)
      XFRO = RFRO*COS(PHI)
      YFRO = RFRO*SIN(PHI)
C
350   CALL CORE(HWORK,80)
      WRITE(cHWORK,321) ICNT,LCH,XFRO,YFRO,TPHI ! PMF 17/11/99: UNIT=10 changed to cHWORK
321   FORMAT(I2,1X,A1,1X,F7.2,2X,F7.2,4X,F5.1)
      CALL SYSSYM(XXX,YYY,SSS,HWORK,30,0.)
      YYY = YYY - 1.5*SSS
      CALL CORE(HWORK,80)
      WRITE(cHWORK,322) ET,RPP4,PZ,XM,COSTH ! PMF 17/11/99: UNIT=10 changed to cHWORK
322   FORMAT(F6.2,1X,F5.2,1X,F6.2,1X,F6.4,1X,F8.5)
      CALL SYSSYM(XXX,YYY,SSS,HWORK,35,0.)
      YYY = YYY - 2.*SSS
C
      GO TO 300
31    CONTINUE
C//////// TAGAN DISPLAY /////////////////////////////////////
C
999   CONTINUE
C
C
       JRUN = 0
       CALL TAGAN(IERR,JRUN)
C
       IF (IERR.NE.0) WRITE(6,9800)IERR,JRUN
9800   FORMAT(' IERR=',I10,' JRUN=',I10)
       IF (IERR.NE.0) GO TO 9999
C
       ITAGG0 = IDATA(IBLN('TAGG'))
       ITAGG1 = IDATA(ITAGG0 - 1)
       ITAGG2 = IDATA(ITAGG1 - 1)
       ITAGG3 = IDATA(ITAGG2 - 1)
       ITAGG4 = IDATA(ITAGG3 - 1)
       NCLST = HDATA(2*ITAGG0 + 6)
       NCLZMI= HDATA(2*ITAGG0 + 7)
       NCLZPL= HDATA(2*ITAGG0 + 8)
       NWPCL = HDATA(2*ITAGG0 + 53)
C
       DO 9805 N = 1,NCLST
       IB = (N-1)*NWPCL
        XFN = ADATA(ITAGG2 + IB + 7)
        YFN = ADATA(ITAGG2 + IB + 8)
       JPARTN = HDATA(ITAGG2*2+2*IB+2)
C
      XOFF = ADX
      IF(JPARTN.LT.0) XOFF = -ADX
      YOFF = ADY
      XXX = XOFF - XFN * 1.5
      YYY = YOFF + YFN * 1.5
C
      CALL PLYGON(7,RADI,XXX,YYY,0)
C
      CALL NUMBWR(13,N,XXX+2.*RADI,YYY,RADI)
C
9805   CONTINUE
C********************************************************
C  WRITE TAGAN RESULTS IN LEFT HAND COLUMN
C
      XXX = XMIN
      YYY = YMIN+.76*(YMAX-YMIN)
      CALL XXXYYY(XXX,YYY,SSS,0)
C
        CALL CORE(HWORK,80)
        WRITE(cHWORK,220) INAME,NBK,NCLST   ! PMF 17/11/99: UNIT=10 changed to cHWORK
220     FORMAT(' BANK ',A4,I3,' NR OF CLUSTERS',I3)
        CALL SYSSYM(XXX,YYY,SSS,HWORK,31,0.)
C
      YYY = YYY - 2.*SSS
      CALL CORE(HWORK,80)
      WRITE(cHWORK,219)         ! PMF 17/11/99: UNIT=10 changed to cHWORK
219   FORMAT('NR  Z     X       Y       PHI')
      CALL SYSSYM(XXX,YYY,SSS,HWORK,29,0.)
      YYY = YYY - 1.5*SSS
      CALL CORE(HWORK,80)
      WRITE(cHWORK,218)             ! PMF 17/11/99: UNIT=10 changed to cHWORK
218   FORMAT('ETAG     +-  DETAG     COSTHE')
      CALL SYSSYM(XXX,YYY,SSS,HWORK,29,0.)
C
      YYY = YYY - 4.5*SSS
C
      ICNT = 0
200   ICNT = ICNT + 1
      IF(ICNT.GT.NCLST) GO TO 21
       IB = (ICNT-1)*NWPCL
        XFN = ADATA(ITAGG2 + IB + 7)
        YFN = ADATA(ITAGG2 + IB + 8)
        EFN = ADATA(ITAGG2 + IB + 5)
        DEFN = ADATA(ITAGG2 + IB + 6)
       JPARTN = HDATA(ITAGG2*2+2*IB+2)
C
C         CALCULATION OF FI AND THETA ANGLES FOR TAG.
C
       TAGFI = ATAN2(YFN,XFN)
       IF(TAGFI.LT.0.) TAGFI = TAGFI + TWOPI
      TAGFI = TAGFI*180./PI
      RFN = SQRT(XFN*XFN+YFN*YFN)
      THE = ATAN2(RFN,PBSCZ(3))
      IF(JPARTN.LT.0) THE = PI - THE
C
      CALL CORE(HWORK,80)
      WRITE(cHWORK,221) ICNT,JPARTN,XFN,YFN,TAGFI ! PMF 17/11/99: UNIT=10 changed to cHWORK
221   FORMAT(I2,1X,I2,1X,F7.2,2X,F7.2,2X,F5.1)
      CALL SYSSYM(XXX,YYY,SSS,HWORK,29,0.)
      YYY = YYY - 1.5*SSS
      COSTH = COS(THE)
      CALL CORE(HWORK,80)
      WRITE(cHWORK,222) EFN,DEFN,COSTH ! PMF 17/11/99: UNIT=10 changed to cHWORK
222   FORMAT(F8.3,' +- ',F6.3,3X,F8.5)
      CALL SYSSYM(XXX,YYY,SSS,HWORK,29,0.)
      YYY = YYY - 2.*SSS
C
      GO TO 200
21    CONTINUE
C
9999  CONTINUE
      RETURN
      END
