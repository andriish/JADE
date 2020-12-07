C   20/12/85 807251626  MEMBER NAME  TRTEXT   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE TRTEXT(IPPATR,IPP,XXX,YYY,SSS) 
C-----------------------------------------------------------------------
C
C     WRITE INFORMATION FROM BANK 'PATR' FOR ONE TRACK, GIVEN BY IPO,IPP
C     POSITION AND SIZE OF TEXT GIVEN BY XXX,YYY,SSS
C          J.OLSSON,  19.10.79           LAST CHANGE 28.01.80
C            LAST CHANGE 15.01.88  J. HAGEMANN
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
      LOGICAL TBIT
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
      COMMON /CHEADR/ HEAD(108)
      COMMON /CJTRIG/ PI,TWOPI
      COMMON/CWORK2/HWORK(40),JNDEX,NTR,LTR,ITR,IPO,ICNT,NBK,NCLST,NWPCL
     $,PMOM,PZ,PTRANS,RMS,NHTF,RAD,RAD1,THE,PHI,DUM(6),HSIGN,HDUM,NTRRES
     $,IW52
C
      DATA HBLANK/'  '/, HPLUS/'+ '/,HMINUS/'- '/
      DATA HPLCFT/'+¢'/, HMICFT/'-¢'/
*** PMF 17/11/99: add variables needed for emulation of DESYLIB routine 'CORE'  
      CHARACTER cHWORK*80
      EQUIVALENCE (cHWORK,HWORK(1))
*** PMF(end)
C
C------------------  C O D E  ------------------------------------------
C
      ITR   = IDATA(IPP + 1)
      IF(DSPDTL(14).AND.NTRRES.NE.ITR.AND.NTRRES.NE.0) GO TO 7722
      HSIGN = HBLANK
      RMS   = ADATA(IPP + 23)
      RMSZ  = ADATA(IPP + 32)
      NHTF  = IDATA(IPP + 24)
      NHTZ  = IDATA(IPP + 33)
      NBK   = IDATA(IPPATR-2)
      IF(NBK.EQ.12) RMS = RMS/NHTF
      LTR   = IDATA(IPPATR + 3)
      CALL MOMENT(IPP,PX,PY,PZ,PTRANS,PMOM,FI,THE)
      RAD = ADATA(IPP+25)
      IF( LTR .LT. 64 ) GOTO 641
         ICD = IDATA(IPP + 2)
         IRC  = -100
         IF( ICD .EQ. 301 ) ICD = 65536
         VERVAL = -100.0
         IF( LTR .GT. 62 ) VERVAL = ABS(ADATA(IPP+51))
         IF(TBIT(ICD,19) .AND. LTR.EQ.64 .AND. VERVAL.GT.0.01)
     *                     ICD = IBITON(ICD,15)
         IF( TBIT(ICD,15) .AND. LTR .GE. 64 ) IRC = IDATA(IPP + 62)
         IF(.NOT.TBIT(ICD,15) .OR. IRC.NE.0) GOTO 641
            HSIGN = HPLCFT
            IF(RAD*BKGAUS.LT.0.) HSIGN = HMICFT
            GOTO 642
 641  CONTINUE
         HSIGN = HPLUS
         IF(RAD*BKGAUS.LT.0.) HSIGN = HMINUS
 642  CONTINUE
C** WRITE OUT VALUES
      IF( .NOT. DSPDTL(13) ) GO TO 7544
      FI = FI*180./PI
      CALL CORE(HWORK,80)
      WRITE(cHWORK,221) ITR,HSIGN,RMS,NHTF,RMSZ,NHTZ,FI! PMF 17/11/99: UNIT=10 changed to cHWORK
221   FORMAT(I2,1X,A2,1X,F4.2,'/',I2,1X,F5.1,'/',I2,1X,F5.1)
      CALL SYSSYM(XXX,YYY,SSS,HWORK,29,0.)
7722  YYY = YYY - 1.5*SSS*.75 !PMF 20/11/99: add factor .75
      IF(DSPDTL(14).AND.NTRRES.NE.ITR.AND.NTRRES.NE.0) GO TO 7723
      COSTH = COS(THE)
      CALL CORE(HWORK,80)
      WRITE(cHWORK,222) PMOM,PZ,PTRANS,COSTH ! PMF 17/11/99: UNIT=10 changed to cHWORK
222   FORMAT(F6.3,1X,F7.3,1X,F6.3,1X,F6.3)
      CALL SYSSYM(XXX,YYY,SSS,HWORK,29,0.)
7723  YYY = YYY - 2.*SSS*.75 !PMF 20/11/99: add factor .75
7544  RETURN
      END
