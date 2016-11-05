C   21/07/79 504191913  MEMBER NAME  GAMDIS   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE GAMDIS(INDEX,VX,VY,VZ)
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. OLSSON   21/07/79 :  DISPLAY PHOTONS
C
C       MOD:   J. OLSSON   15/02/84 :
C       MOD:   C. BOWDERY   8/06/84 :  NEW COMMAND NUMBERS
C  LAST MOD:   C. BOWDERY  19/04/85 :  RECOMMENTED. CHANGE MADE FOR VC
C
C     DISPLAY PHOTONS FROM THE BANK 'LGCL'
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL DSPDTM
C
      COMMON / CGRAP2 / BCMD,DSPDTM(30)
      COMMON / CHEADR / HEAD(108)
      COMMON /CJTRIG/ PI,TWOPI
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
      DATA  ZDEEP / 5800.0 /
C
C-----------------------------------------------------------------------
C
      IPLGCL=IDATA(IBLN('LGCL'))
      IF(IPLGCL.LE.0) RETURN
      IF(IDATA(IPLGCL).LE.26) RETURN
      IPALGN=IDATA(IBLN('ALGN'))
      IPPATR=IDATA(IBLN('PATR'))
      LASTRM = LASTVW
      IF(LASTVW.EQ.14) LASTVW = 1
      ISTEP = IDATA(IPLGCL+21)
      IF(ISTEP.NE.2) CALL LGCDIR(IPPATR,IPALGN,IPLGCL)
      IPLGCL=IDATA(IBLN('LGCL'))
      IPALGN=IDATA(IBLN('ALGN'))
      IPPATR=IDATA(IBLN('PATR'))
      NWPCL = IDATA(IPLGCL+25)
      IPO = IDATA(IPLGCL+3) + IPLGCL - 1 - NWPCL
      NCLST = IDATA(IPLGCL+7)
      JUMPCL = IDATA(IPLGCL+2)
      RTS = RLG
      ZZPL = ZENDPL
      ZZMI = ZENDMI
      IF(INDEX.EQ.1.OR.INDEX.EQ.4.OR.INDEX.EQ.8) RTS = RTOF
      IF(INDEX.GE.17.AND.INDEX.LE.19) RTS = 0.33*(XMAX-XMIN)
      IF(INDEX.GE.17.AND.INDEX.LE.19) ZZPL = RTS
      IF(INDEX.GE.17.AND.INDEX.LE.19) ZZMI = -RTS
      IGAM = 0
C
      DO  1  I = 1,NCLST
        IPO   = IPO + NWPCL
        JTYP  = IDATA(IPO+8)
C
C                            IF THIS CLUSTER IS NOT A PHOTON, IGNORE
C
        IF( JTYP .NE. 0 ) GO TO 1
        IGAM  = IGAM + 1
C
C                            IF CLUSTER IS A SPINNER, IGNORE FOR CDTL 3400004810
C
        IF( .NOT. DSPDTM(4) ) GO TO 11
          IPBL  = HDATA(2*(IPLGCL+JUMPCL+I-1)-1)
          IPBL2 = HDATA(2*(IPLGCL+JUMPCL+I-1))
          NBL   = HDATA(2*(IPALGN + 3 + IPBL)-1)
          IER   = 0
          IF( IPBL .EQ. IPBL2 ) CALL SPINNR( NBL,IER,HEAD(18) )
          IF( IER .EQ. 0 ) GO TO 11
          GO TO 1
C
C                            CUT PHOTONS BELOW 200 MEV FOR CDTL 35
C
  11    IF( DSPDTM(5)  .AND.  ADATA(IPO+2) .LT. 0.200 ) GO TO 1
C
C                            CHECK LG DETECTOR PART ( JPART )
C
        JPART = IDATA(IPO+1)
        IF( JPART .NE. 0 ) GO TO 2
C
C                            DRAW BARREL PHOTONS  ( JPART = 0 )
C
        PHI  = ATAN2(ADATA(IPO+10),ADATA(IPO+9))
        IF( PHI .LT. 0.0 ) PHI = PHI + TWOPI
        THE  = ARCOS(ADATA(IPO+11))
        IF( VZ .EQ. 0.0 ) GO TO 14
          THE = ATAN2( ADATA(IPO+5) - VZ, RLG )
          THE = PI*0.5 - THE
  14    XP   = RTS*COS(PHI)
        YP   = RTS*SIN(PHI)
C
        ZP   = VZ
        IF( ABS(PI*0.5-THE) .GE. 0.001 ) ZP = RTS/TAN(THE) + VZ
        VXP  = VX
        VYP  = VY
        VZP  = VZ
C
C                            SKIP IF NOT CYLINDER VIEW
C
        IF( INDEX .NE. 14 ) GO TO 3
          FP   = (ZDEEP -  ZP) / (ZDEEP + ZLGPL)
          FP1  = (ZDEEP - VZP) / (ZDEEP + ZLGPL)
          XP   = FP*XP
          YP   = FP*YP
          VXP  = FP1*VXP
          VYP  = FP1*VYP
          GO TO 3
C
C                            ENDCAP PHOTONS  ( JPART NE 0 )
C
   2    PHI = ATAN2(ADATA(IPO+10),ADATA(IPO+9))
        IF(PHI.LT.0.) PHI = PHI + TWOPI
        THE = ARCOS(ADATA(IPO+11))
C
        ZZ  = ZZPL
        IF( JPART .LT. 0 ) ZZ = ZZMI
        RR  = ZZ*TAN(THE)
        XP  = RR*COS(PHI)
        YP  = RR*SIN(PHI)
        ZZ  = ZZ - VZ
C
        ZP  = ZZPL
        IF( JPART .LT. 0 ) ZP = ZZMI
        VXP = VX
        VYP = VY
        VZP = VZ
C
C                            SKIP IF NOT CYLINDER VIEW
C
        IF( INDEX .NE. 14 ) GO TO 3
          FP  = (ZDEEP -  ZP) / (ZDEEP + ZLGPL)
          FP1 = (ZDEEP - VZP) / (ZDEEP + ZLGPL)
          XP  = FP*XP
          YP  = FP*YP
          VXP = FP1*VXP
          VYP = FP1*VYP
C
C                            ACTUAL DISPLAY PART
C
C                            FIRST THE RPHI VIEWS EXCEPT CYLINDER
C
  3     IF( INDEX .GT. 3   .AND.  INDEX .NE. 14  .AND.
     +      INDEX .NE. 17  .AND.  INDEX .NE. 20        ) GO TO 4
          XX  = XP
          YY  = YP
          XX1 = VXP
          YY1 = VYP
C
          CALL TRNUMB(IGAM,0,XX,YY,DUM1)
          XX  = - XX
          XX1 = - XX1
          GO TO 5
C
C                            NEXT THE Z VIEWS INCL CYLINDER
C
  4      XX  = ZP
         XX1 = VZP
C
C                            IF PROJECT MODE (CDTL 9 OFF) DO 1ST BLOCK
C                            IF ROTATE  MODE (CDTL 9 ON ) DO 2ND BLOCK
C
        IF( DSPDTL(9) ) GO TO 6
          YY  = XP
          YY1 = VXP
          IF( INDEX .GT. 7  .AND.  INDEX .NE. 18 ) YY = YP
          IF( INDEX .GT. 7  .AND.  INDEX .NE. 18 ) YY1 = VYP
          GO TO 7
C
  6       YY = SQRT(XP*XP+YP*YP)
          YY1 = SQRT(VXP*VXP+VYP*VYP)
          IF( YP .LT. 0.0 .AND. INDEX.GT.7.AND.INDEX.NE.18) YY = - YY
          IF( XP .LT. 0.0 .AND. (INDEX.LT.8.OR.INDEX.EQ.18)) YY = - YY
          IF(VYP.LT.0..AND.INDEX.GT.7.AND.INDEX.NE.18) YY1 = - YY1
          IF(VXP.LT.0..AND.(INDEX.LT.8.OR.INDEX.EQ.18)) YY1 = - YY1
C
  7     CALL TRNUMB(IGAM,0,-XX,YY,DUM1)
C
  5     CALL MOVEA( XX1, YY1)
        CALL DASHA( XX, YY, 14)
  1   CONTINUE
C
      LASTVW = LASTRM
C
      RETURN
      END
