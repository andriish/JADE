C   20/12/85 512202013  MEMBER NAME  GMTEXT   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE GMTEXT(IPP,NTRRES,KCNT,IGMX)
C-----------------------------------------------------------------------
C
C     WRITE RESULT TABLE FOR A SINGLE CLUSTER
C         J.OLSSON,  18.07.79           LAST CHANGE 11.12.81
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
C                            MACRO CGEO1 .... JADE GEOMETRY
C-----------------------------------------------------------------------
C
      COMMON / CGEO1 / BKGAUS,
     +                 RPIP,DRPIP,XRLPIP,   RBPC,DRBPC,XRLBPC,
     +                 RITNK,DRITNK,XRLTKI, R0ROH,DR0ROH,XR0ROH,
     +                 R1ROH,DR1ROH,XR1ROH, R2ROH,DR2ROH,XR2ROH,
     +                 R3ROH,DR3ROH,XR3ROH, ROTNK,DROTNK,XRLTKO,
     +                 RTOF,DRTOF,XRTOF,    RCOIL,DRCOIL,XRCOIL,
     +                 ZJM,DZJM,XRZJM,ZJP,DZJP,XRZJP,ZTKM,DZTKM,XRZTKM,
     +                 ZTKP,DZTKP,XRZTKP,ZBPPL,ZBPMI,ZTOFPL,ZTOFMI,
     +                 XRJETC,RLG,ZLGPL,ZLGMI,OUTR2,CTLIMP,
     +                 CTLIMM,DELFI,BLXY,BLZ,BLDEP,ZENDPL,ZENDMI,DEPEND,
     +                 XHOL1,XHOL2,YHOL1,YHOL2,BLFI
C
C------------------------- END OF MACRO CGEO1 --------------------------
C
C
      COMMON /CJTRIG/ PI,TWOPI
      COMMON/CWORK2/HWORK(40),JNDEX,NTR,LTR,ITR,IPO,ICNT,NBK,NCLST,NWPCL
     $,PMOM,PZ,PTRANS,RMS,NHTF,RAD,RAD1,THE,PHI,XXX,YYY,SSS
C
      DIMENSION HSIGN(3),HDETEC(3,2),HPART(3,2),HTRCK(3)
C
      DATA HDETEC/'BA','RR','EL','EN','DC','AP'/
      DATA HPART/'PH','OT','ON','CH','AR','GE'/
*** PMF 17/11/99: add variables needed for emulation of DESYLIB routine 'CORE'  
      CHARACTER cHWORK*80
      EQUIVALENCE (cHWORK,HWORK(1))
*** PMF(end)
C
C------------------   C O D E  -----------------------------------------
C
      JPART = IDATA(IPP + 1)
      JPART = IABS(JPART) + 1
      DO 2354  I = 1,3
2354  HSIGN(I) = HDETEC(I,JPART)
      JTYP = IDATA(IPP + 8)
      IF(JTYP.NE.0) JTYP = 1
      DO 2355  I = 1,3
2355  HTRCK(I) = HPART(I,JTYP+1)
C** WRITE OUT VALUES
      IF(DSPDTL(14).AND.NTRRES.NE.KCNT.AND.NTRRES.NE.0.AND.LASTVW.EQ.13)
     $ GO TO 2287
      CALL CORE(HWORK,80)
      IF(JTYP.EQ.1)
     + WRITE(cHWORK,221) KCNT,(HSIGN(I),I=1,3),(HTRCK(I),I=1,3)! PMF 17/11/99: UNIT=10 changed to cHWORK
     $ ,IDATA(IPP+8)
221   FORMAT('NR ',I2,'   ',3A2,'   ',3A2,' ',I3)
      IF(JTYP.EQ.0) 
     +     WRITE(cHWORK,227) KCNT,(HSIGN(I),I=1,3),(HTRCK(I),I=1,3)! PMF 17/11/99: UNIT=10 changed to cHWORK
     $ ,IGMX
227   FORMAT('NR ',I2,'   ',3A2,'   ',3A2,' ',I2)
      IF(JTYP.EQ.1) NLGT = 27
      IF(JTYP.EQ.0) NLGT = 26
      CALL SYSSYM(XXX,YYY,SSS,HWORK,NLGT,0.)
      YYY = YYY - 1.5*SSS*.75 !PMF 20/11/99: add factor .75
      PHI = ATAN2(ADATA(IPP+10),ADATA(IPP+9))
      IF(PHI.LT.0.) PHI = PHI + TWOPI
      COSTH = ADATA(IPP+11)
C     IF(JPART.EQ.2) GO TO 2356
C     PHI = ADATA(IPP+4)
C     THE = ATAN2(ADATA(IPP+5),RLG)
C     THE = PI*.5 - THE
C     GO TO 2357
C2356  RR = SQRT(ADATA(IPP+4)**2 + ADATA(IPP+5)**2)
C      THE = ATAN2(RR,ZENDPL)
C      IF(IDATA(IPP+1).LT.0) THE = PI - THE
C      PHI = ATAN2(ADATA(IPP+5),ADATA(IPP+4))
C      IF(PHI.LT.0.) PHI = PHI + TWOPI
2357  PHI = PHI*180./PI
C     COSTH = COS(THE)
      CALL CORE(HWORK,80)
      WRITE(cHWORK,222) ADATA(IPP+2),PHI,COSTH ! PMF 17/11/99: UNIT=10 changed to cHWORK
222   FORMAT('E ',F7.3,' FI ',F5.1,' COST',F6.3)
      CALL SYSSYM(XXX,YYY,SSS,HWORK,29,0.)
      GO TO 2277
2287  YYY = YYY - 1.5*SSS*.75 !PMF 20/11/99: add factor .75
2277  YYY = YYY - 2.*SSS*.75 !PMF 20/11/99: add factor .75
      RETURN
      END
