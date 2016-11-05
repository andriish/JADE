C   20/12/85 512202036  MEMBER NAME  LGCLST   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE LGCLST
C-----------------------------------------------------------------------
C
C
C    DISPLAY TABLE OF RESULTS FOR CLUSTER ANALYSIS BANK  "LGCL"
C        IPO:          START ADRESS OF BANK CONTENT
C        NBK:          BOS NUMBER OF BANK
C        INDEX:        VIEW INDEX ACCORDING TO DISPLAY PROGRAM
C            J.OLSSON,  18.07.79           LAST CHANGE 01.12.79
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
      LOGICAL DSPDTM
C
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
      COMMON / CGRAP2 / BCMD,DSPDTM(30),ISTVW,JTVW
      COMMON/CWORK2/HWORK(40),JNDEX,NTR,LTR,ITR,IPO,ICNT,NBK,NCLST,NWPCL
     $,PMOM,PZ,PTRANS,RMS,NHTF,RAD,RAD1,THE,PHI,XXX,YYY,SSS,IPP,IHO,IVE
     $,DUMM,NTRRES,IW52
      COMMON /CJTRIG/ PI,TWOPI
C
      DATA INAME /'LGCL'/
*** PMF 17/11/99: add variables needed for emulation of DESYLIB routine 'CORE'  
      CHARACTER cHWORK*80
      EQUIVALENCE (cHWORK,HWORK(1))
*** PMF(end)
C
C------------------  C O D E  ------------------------------------------
C
      IPP = IPO
      ICNT = 0
      IFL = 0
C
C LSTCMD=111,112: NEXT EVENT, WRIT EVENT   SPECIAL FOR AUTO DISPLAY
C
      IF(DSPDTL(14).AND.ACMD.NE.0..AND.LSTCMD.NE.111.AND.LSTCMD.NE.112)
     $ IFL = 1
C---
      XXX = XMIN
      YYY = YMIN+.76*(YMAX-YMIN)
      CALL XXXYYY(XXX,YYY,SSS,2)
C     IF(DSPDTL(14).AND.ACMD.NE.0..AND.LASTVW.EQ.13) GO TO 2277
      IF(IFL.NE.0.AND.LASTVW.EQ.13) GO TO 2277
C
C                            IF OPT 46 IS ON AND OPT 13 IS OFF
C                            FOR PUBLICATION PICTURES WITHOUT CAPTIONS,
C                            SUPPRESS THE RESULTS HEADER.
C
      IF( DSPDTM(16)  .AND.  .NOT. DSPDTL(13) ) GO TO 2277
C
      CALL CORE(HWORK,80)
      WRITE(cHWORK,220) INAME,NBK,NCLST ! PMF 17/11/99: UNIT=10 changed to cHWORK
220   FORMAT('BANK ',A4,I2,' NR OF CLUSTERS',I3)
      CALL SYSSYM(XXX,YYY,SSS,HWORK,29,0.)
C
2277  IF( .NOT. DSPDTL(13) ) GO TO 21
      YYY = YYY - 3.*SSS
      IGMX = 0
200   ICNT = ICNT + 1
      IF(ICNT.GT.NCLST.OR.ICNT.GT.26) GO TO 21
      IPP = IPP + NWPCL
      IF(IDATA(IPP+8).EQ.0) IGMX = IGMX + 1
      CALL GMTEXT(IPP,NTRRES,ICNT,IGMX)
      GO TO 200
21    CONTINUE
      XXX = XMIN+.33*(XMAX-XMIN)
      YYY = YMIN+.01*(YMAX-YMIN)
      CALL XXXYYY(XXX,YYY,SSS,1)
      IF(IFL.NE.0.AND.LASTVW.EQ.13) GO TO 2278
C     IF(DSPDTL(14).AND.ACMD.NE.0..AND.LASTVW.EQ.13) GO TO 2278
C
C                            IF OPT 46 IS ON AND OPT 13 IS OFF
C                            FOR PUBLICATION PICTURES WITHOUT CAPTIONS,
C                            SUPPRESS THE RESULTS HEADER.
C
      IF( DSPDTM(16)  .AND.  .NOT. DSPDTL(13) ) RETURN
C
      CALL CORE(HWORK,80)
      IPLGCL=IDATA(IBLN('LGCL'))
      WRITE(cHWORK,865) 
     +     ADATA(IPLGCL+11),ADATA(IPLGCL+16),IDATA(IPLGCL+15) ! PMF 17/11/99: UNIT=10 changed to cHWORK
865   FORMAT(' TOTAL CLUSTER ENERGY ',F7.3,'  PHOTON ENERGY ',F7.3,' NR
     $OF PHOTONS ',I3)
      CALL SYSSYM(XXX,YYY,SSS,HWORK,70,0.)
2278  CONTINUE
      RETURN
      END
