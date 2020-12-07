      SUBROUTINE ZVERTF
      IMPLICIT INTEGER*2 (H)
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
C====MACRO CZVPAR===================================
C-------------------------------------------
C   RESULTS FROM ZVERTF
C   P. STEFFEN (79/01/26)
C---------------------------------------------
      COMMON /CZVPAR/ LBZVDF,ZLOW,  BINZ,  NBINZ,
     ,                NWRDR1,LWRDC0,LWRDC1,LWRDC2,
     ,                IDZ1LM,IDZ2LM,NPKMIN,SBRAT,DFIMAX,
     ,                DZVPAR(5)
C==ENDMACRO CZVPAR========================================
C-----------------------------------------------------------------------
C                            MACRO CJDRCH .... JET CHAMBER CONSTANTS.
C-----------------------------------------------------------------------
C
      COMMON / CJDRCH / RDEC(4),PSIIN(3),RINCR(3),FIRSTW(3),FSENSW(3),
     +                  RDEPTH,SWDEPL,YSUSPN,TIMDEL(2,3),ZMAX,ZOFFS,
     +                  ZRESOL,ZNORM,ZAL,ZSCAL,DRIDEV,DRICOS,DRISIN,
     +                  PEDES,TZERO(3),DRIROT(96,2),SINDRI(96,2),
     +                  COSDRI(96,2),DRIVEL(96,2),T0FIX(3),
     +                  ABERR(8), DUMJDC(20)
C
C      BLOCK DATA SET TO MC VALUES, KALIBR WILL SET REAL DATA VALUES
C--->  A CHANGE OF THIS COMMON MUST BE DONE SIMULTANEOUSLY WITH  <----
C--->  A CHANGE OF THE BLOCK DATA                                <----
C
C--------------------------- END OF MACRO CJDRCH -----------------------
C
C----------------------------------------------------------------------
C           MACRO CDSMAX .... PATTERN RECOGNITION CONSTANTS.
C----------------------------------------------------------------------
      COMMON/CDSMAX/DSMAX(16,3,2),DIRWR1(24,2),DIRWR3(48,2)
     *             ,DHALF(16,3,2),DTWICE(16,3,2),HMCH(16,3,2)
     *             ,IBCK(9),DBCK(30),TRMATS(96,2),TRMATC(96,2)
C------------------------ END OF MACRO CDSMAX -------------------------
C------------------------------------------
C  MACRO CLBPGM ....
C------------------------------------------
      COMMON /CLBPGM/ LBPGM(30)
C--------- END OF MACRO CLBPGM ------------
C====  MACRO CWORKZV  ==================================
C-------------------------------------------
C   RESULTS + INTERM. STORAGE OF ZVERTF
C   P. STEFFEN (79/01/21)
C---------------------------------------------
      COMMON /CWORK/ FZRSLT(12)
     ,             , HUFLO,HOFLO,MAXZ,HIST(100)
     ,             , HPTSEC(98)
     ,             , NZ1(16),NZ2(16), HLB1(8),HLB2(8)
     ,        , HZ1(8,16),HZ2(16,16), FI1(8,16),FI2(8,16),HTMP(100)
                     INTEGER*4 HPTSEC
                     INTEGER IZRSLT(12)
                     EQUIVALENCE (IZRSLT(1),FZRSLT(1))
C
C==  ENDMACRO CWORKZV  ========================================
      LBPGM(2) = LBPGM(2) + 1
      DATA MKLAYR /15/
      DATA LBINIT /0/
      IF(
     - LBINIT.EQ.0
     -)THEN
        LBINIT = 1
        IQJETC = IBLN('JETC')
        IZCON = ZAL*.5
        IZMAX = ZMAX + 40.
      ENDIF
      IPJCA  = IDATA(IQJETC)
      IPJCA2 = IPJCA*2 + 2
      IP0 = IPJCA2 + 98
      ISEGO = IPJCA2
      DO 13000 ISEG=1,96
        ISEGO = ISEGO + 1
        HPTSEC(ISEG) = HDATA(ISEGO) + IP0
13000 CONTINUE
13001 CONTINUE
      HPTSEC(97) = HDATA(ISEGO+1) + IP0
      HPTSEC(98) = 0
      FZRSLT(1) = 0.
      FZRSLT(2) = 1000000.
      FZRSLT(3) = 1000000.
      FZRSLT(4) = 0.
      FZRSLT(5) = 0.
      IZRSLT(6) =-2
      INDLB = 3
      DFIMX0 = DFIMAX
      ASSIGN 17001 TO IZZZ01
      GOTO 17000
17001 CONTINUE
      IF(
     - IZRSLT(6).LT.3
     -)THEN
        IZRSLT( 7) = IZRSLT( 1)
        IZRSLT( 8) = IZRSLT( 2)
        IZRSLT( 9) = IZRSLT( 3)
        IZRSLT(10) = IZRSLT( 4)
        IZRSLT(11) = IZRSLT( 5)
        IZRSLT(12) = IZRSLT( 6)
        DFIMX0 = 1000.
      ASSIGN 17003 TO IZZZ02
      GOTO 17002
17003 CONTINUE
      IF(
     - IZRSLT(6).LE.IZRSLT(12)
     -)THEN
          IZRSLT( 1) = IZRSLT( 7)
          IZRSLT( 2) = IZRSLT( 8)
          IZRSLT( 3) = IZRSLT( 9)
          IZRSLT( 4) = IZRSLT(10)
          IZRSLT( 5) = IZRSLT(11)
          IZRSLT( 6) = IZRSLT(12)
      ENDIF
      ENDIF
      NWRES = 6
      CALL CCRE(IPHT,'ZVTX',0,NWRES,IERR)
      IF(IERR.NE.0) RETURN
      CALL BSAW(1,'ZVTX')
         ADATA(IPHT+1) = FZRSLT(1)
         ADATA(IPHT+2) = FZRSLT(2)
         ADATA(IPHT+3) = FZRSLT(3)
         ADATA(IPHT+4) = FZRSLT(4)
         ADATA(IPHT+5) = FZRSLT(5)
         ADATA(IPHT+6) = FZRSLT(6)
      RETURN
17000 CONTINUE
      BINZIV = 1. / BINZ
      PAR1 = FSENSW(1) / (FSENSW(2) - FSENSW(1))
      PAR2 =  RINCR(1) / (FSENSW(2) - FSENSW(1))
      HUFLO = 0
      HOFLO = 0
      MAXZ = 0
      NBACK = 0
      CALL SETS(HIST(1),0,200,0)
      IF(
     - HPTSEC(25)-HPTSEC(1) .LT. NWRDR1
     -)THEN
        IZRSLT(6) = -2
        INDLB = 1
      ELSE
      JSEC=1
16000 CONTINUE
        NWRD1 = HPTSEC(JSEC+1) - HPTSEC(JSEC)
        NWRD2 = HPTSEC(JSEC+25) - HPTSEC(JSEC+24)
      IF(
     - NWRD1.GE.LWRDC0
     -)THEN
      IF(
     - JSEC.EQ.1
     -)THEN
            NWRD1L= HPTSEC(25) - HPTSEC(24)
            NWRD2L= HPTSEC(49) - HPTSEC(48)
      ELSE
            NWRD1L= HPTSEC(JSEC   ) - HPTSEC(JSEC- 1)
            NWRD2L= HPTSEC(JSEC+24) - HPTSEC(JSEC+23)
      ENDIF
      IF(
     - JSEC.EQ.24
     -)THEN
            NWRD1R= HPTSEC( 2) - HPTSEC( 1)
            NWRD2R= HPTSEC(26) - HPTSEC(25)
      ELSE
            NWRD1R= HPTSEC(JSEC+ 2) - HPTSEC(JSEC+ 1)
            NWRD2R= HPTSEC(JSEC+26) - HPTSEC(JSEC+25)
      ENDIF
      IF(
     - NWRD1+NWRD1L.GE.LWRDC1 .OR. NWRD1+NWRD1R.GE.LWRDC1
     -)THEN
      IF(
     - NWRD2L+NWRD2.GE.LWRDC2 .OR. NWRD2R+NWRD2.GE.LWRDC2
     -)THEN
      ASSIGN 17005 TO IZZZ03
      GOTO 17004
17005 CONTINUE
      ASSIGN 17007 TO IZZZ04
      GOTO 17006
17007 CONTINUE
      ENDIF
      ENDIF
      ENDIF
      JSEC = JSEC + 1
      IF(.NOT.(
     - JSEC.GT.24
     -))GOTO 16000
16001 CONTINUE
      CALL MVC(HTMP(1),0,HIST(1),0,200)
      ASSIGN 17009 TO IZZZ05
      GOTO 17008
17009 CONTINUE
      ENDIF
      GOTO IZZZ01
17004 CONTINUE
      DO 13002 ILAYR=1,16
          NZ1(ILAYR) = 0
          NZ2(ILAYR) = 0
13002 CONTINUE
13003 CONTINUE
        DSBIN1 = TIMDEL(1,1)
        DSBIN2 = TIMDEL(2,1)
        IPT0 = HPTSEC(JSEC)
        IPT9 = HPTSEC(JSEC+1) - 1
      DO 13004 IPT = IPT0,IPT9,4
          IWIRE = HDATA(IPT)
          IWIRE = ISHFTR(IWIRE,3)
          ILAYR = LAND(IWIRE,MKLAYR) + 1
          IAMPL = HDATA(IPT+1)
          IAMPR = HDATA(IPT+2)
      IF(
     - IAMPL.GT.0 .AND. IAMPR.GT.0
     -)THEN
            IZ1 = (IZCON * (IAMPR-IAMPL)) / (IAMPR+IAMPL)
      IF(
     - IABS(IZ1).LT.IZMAX
     -)THEN
              NZ1(ILAYR) = NZ1(ILAYR) + 1
              IHIT = NZ1(ILAYR)
              HZ1(IHIT,ILAYR) = IZ1
              IF(ILAYR.GT.8) DSBIN1 = DSBIN2
              DRLAY = (ILAYR-1) * RINCR(1)
              FI1(IHIT,ILAYR) = HDATA(IPT+3)*DSBIN1 / (FSENSW(1)+DRLAY)
      ENDIF
      ENDIF
13004 CONTINUE
13005 CONTINUE
        JSECA=1
16002 CONTINUE
          JSEC2 = JSEC + JSECA + 22
          IF(JSEC2.LT.25) JSEC2 = 48
          IF(JSEC2.GT.48) JSEC2 = 25
          IPT0 = HPTSEC(JSEC2)
          IPT9 = HPTSEC(JSEC2+1) - 1
      IF(
     - IPT9.GT.IPT0
     -)THEN
            DSBIN1 = TIMDEL(1,2)
            DSBIN2 = TIMDEL(2,2)
      DO 13006 IPT = IPT0,IPT9,4
              IWIRE = HDATA(IPT)
              IWIRE = ISHFTR(IWIRE,3)
              ILAYR = LAND(IWIRE,MKLAYR) + 1
              IAMPL = HDATA(IPT+1)
              IAMPR = HDATA(IPT+2)
      IF(
     - IAMPL.GT.0 .AND. IAMPR.GT.0
     -)THEN
                IZ2 = (IZCON * (IAMPR-IAMPL)) / (IAMPR+IAMPL)
      IF(
     - IABS(IZ2).LT.IZMAX
     -)THEN
                  NZ2(ILAYR) = NZ2(ILAYR) + 1
                  IHIT = NZ2(ILAYR)
                  HZ2(IHIT,ILAYR) = IZ2
                  IF(ILAYR.GT.8) DSBIN1 = DSBIN2
                  DS = HDATA(IPT+3)*DSBIN1
                  IF(JSECA.NE.2)
     ?                     DS = DSMAX(ILAYR,2,1)+DSMAX(ILAYR,2,2) - DS
                  DRLAY = (ILAYR-1) * RINCR(2)
                  FI2(IHIT,ILAYR) = DS / (FSENSW(2)+DRLAY)
      ENDIF
      ENDIF
13006 CONTINUE
13007 CONTINUE
      ENDIF
        JSECA = JSECA + 1
      IF(.NOT.(
     - JSECA.GT.3
     -))GOTO 16002
16003 CONTINUE
      GOTO IZZZ03
17002 CONTINUE
      BINZIV = .5 / BINZ
      PAR1 = FSENSW(2) / (FSENSW(3) - FSENSW(2))
      PAR2 =  RINCR(2) / (FSENSW(3) - FSENSW(2))
      HUFLO = 0
      HOFLO = 0
      MAXZ = 0
      NBACK = 0
      CALL SETS (HIST(1),0,200,0)
      CALL SETSL(FI1(1,1),0,1024,0)
      IF(
     - HPTSEC(97)-HPTSEC(49) .LT. NWRDR1
     -)THEN
        IZRSLT(6) = -2
        INDLB = 1
      ELSE
      ICLL=49
16004 CONTINUE
        NWRD1 = HPTSEC(ICLL+1) - HPTSEC(ICLL)
      IF(
     - NWRD1.GE.LWRDC0
     -)THEN
      IF(
     - ICLL.EQ.49
     -)THEN
            NWRD1L= HPTSEC(97) - HPTSEC(96)
      ELSE
            NWRD1L= HPTSEC(ICLL   ) - HPTSEC(ICLL- 1)
      ENDIF
      IF(
     - ICLL.EQ.96
     -)THEN
            NWRD1R= HPTSEC(50) - HPTSEC(49)
      ELSE
            NWRD1R= HPTSEC(ICLL+ 2) - HPTSEC(ICLL+ 1)
      ENDIF
      IF(
     - NWRD1+NWRD1L.GE.LWRDC1 .OR. NWRD1+NWRD1R.GE.LWRDC1
     -)THEN
      IF(
     - ICLL.EQ.49 .OR. ICLL.EQ.96
     -)THEN
              NWRD2 = HPTSEC(25)-HPTSEC(24) + HPTSEC(49)-HPTSEC(48)
      ELSE
              ICLL2L = ICLL/2
              NWRD2 = HPTSEC(ICLL2L+2) - HPTSEC(ICLL2L)
      ENDIF
      IF(
     - NWRD2.GE.LWRDC2
     -)THEN
      ASSIGN 17011 TO IZZZ06
      GOTO 17010
17011 CONTINUE
      ASSIGN 17012 TO IZZZ04
      GOTO 17006
17012 CONTINUE
      ENDIF
      ENDIF
      ENDIF
      ICLL = ICLL + 1
      IF(.NOT.(
     - ICLL.GT.96
     -))GOTO 16004
16005 CONTINUE
      CALL MVC(HTMP(1),0,HIST(1),0,200)
      ASSIGN 17013 TO IZZZ05
      GOTO 17008
17013 CONTINUE
      ENDIF
      GOTO IZZZ02
17010 CONTINUE
      DO 13008 ILAYR=1,16
          NZ1(ILAYR) = 0
          NZ2(ILAYR) = 0
13008 CONTINUE
13009 CONTINUE
        IPT0 = HPTSEC(ICLL)
        IPT9 = HPTSEC(ICLL+1) - 1
      DO 13010 IPT = IPT0,IPT9,4
          IWIRE = HDATA(IPT)
          IWIRE = ISHFTR(IWIRE,3)
          ILAYR = LAND(IWIRE,MKLAYR) + 1
          IAMPL = HDATA(IPT+1)
          IAMPR = HDATA(IPT+2)
      IF(
     - IAMPL.GT.0 .AND. IAMPR.GT.0
     -)THEN
            IZ1 = (IZCON * (IAMPR-IAMPL)) / (IAMPR+IAMPL)
      IF(
     - IABS(IZ1).LT.IZMAX
     -)THEN
              NZ2(ILAYR) = NZ2(ILAYR) + 1
              IHIT = NZ2(ILAYR)
              HZ2(IHIT,ILAYR) = IZ1
      ENDIF
      ENDIF
13010 CONTINUE
13011 CONTINUE
        ICLLA=0
16006 CONTINUE
          ICLL2 = ICLL/2 + ICLLA
          IF(ICLL2.LT.25) ICLL2 = 48
          IF(ICLL2.GT.48) ICLL2 = 25
          IPT0 = HPTSEC(ICLL2)
          IPT9 = HPTSEC(ICLL2+1) - 1
      IF(
     - IPT9.GT.IPT0
     -)THEN
      DO 13012 IPT = IPT0,IPT9,4
              IWIRE = HDATA(IPT)
              IWIRE = ISHFTR(IWIRE,3)
              ILAYR = LAND(IWIRE,MKLAYR) + 1
              IAMPL = HDATA(IPT+1)
              IAMPR = HDATA(IPT+2)
      IF(
     - IAMPL.GT.0 .AND. IAMPR.GT.0
     -)THEN
                IZ1 = (IZCON * (IAMPR-IAMPL)) / (IAMPR+IAMPL)
      IF(
     - IABS(IZ1).LT.IZMAX
     -)THEN
                  NZ1(ILAYR) = NZ1(ILAYR) + 1
                  IHIT = NZ1(ILAYR)
                  HZ1(IHIT,ILAYR) = IZ1
      ENDIF
      ENDIF
13012 CONTINUE
13013 CONTINUE
      ENDIF
        ICLLA = ICLLA + 1
      IF(.NOT.(
     - ICLLA.GT.1
     -))GOTO 16006
16007 CONTINUE
      GOTO IZZZ06
17006 CONTINUE
      DO 13014 ILAYR=1,16
          MZ1 = NZ1(ILAYR)
          MZ2 = NZ2(ILAYR)
      IF(
     - MZ1.GT.0 .AND. MZ2.GT.0
     -)THEN
            FACT = (ILAYR-1)*PAR2 + PAR1
      DO 13016 IHIT1=1,MZ1
              Z1 = HZ1(IHIT1,ILAYR)
              FI01 = FI1(IHIT1,ILAYR)
      DO 13018 IHIT2=1,MZ2
                DFI = ABS(FI01 - FI2(IHIT2,ILAYR))
      IF(
     - DFI.LT.DFIMX0
     -)THEN
                  Z2 = HZ2(IHIT2,ILAYR)
                  ZV = Z1 - (Z2-Z1)*FACT
                  IZV = (ZV-ZLOW) * BINZIV + 1
      IF(
     - IZV.GT.0 .AND. IZV.LE.100
     -)THEN
                    HIST(IZV) = HIST(IZV) + 1
      ELSE
                    IF(IZV.LE.  0) HUFLO = HUFLO + 1
                    IF(IZV.GT.NBINZ) HOFLO = HOFLO + 1
      ENDIF
      ENDIF
13018 CONTINUE
13019 CONTINUE
13016 CONTINUE
13017 CONTINUE
      ENDIF
13014 CONTINUE
13015 CONTINUE
      GOTO IZZZ04
17008 CONTINUE
      IZCNT=0
      ICODE=0
      ZPREV=-1000000.
15000 CONTINUE
      IF(
     - IZCNT.LT.5
     -)THEN
        MAXHST = 0
        NHIST1 = 0
      DO 13020 IHIST = 1,NBINZ
          NHIST1 = NHIST1 + HTMP(IHIST)
          IF(HTMP(IHIST).GT.MAXHST) MAXHST =HTMP(IHIST)
13020 CONTINUE
13021 CONTINUE
        MAXZ = MAXHST
        NPEAK = 0
        IH9 = NBINZ-11
      DO 13022 IH=7,IH9
       IHSUM = HTMP(IH)+HTMP(IH+1)+HTMP(IH+2)+HTMP(IH+3)+HTMP(IH+4)
      IF(
     - IHSUM.GT.NPEAK
     -)THEN
            NPEAK = IHSUM
            HPEAK = IH
      ENDIF
13022 CONTINUE
13023 CONTINUE
      IF(
     - NPEAK.EQ.0
     -)THEN
      GOTO 15001
      ENDIF
        PEAK = NPEAK
          H1 = HPEAK - 7
          H2 = HPEAK + 7
       NBACK = HTMP(H1  )+HTMP(H1+1)+HTMP(H1+2)+HTMP(H1+3)+HTMP(H1+4)
     +       + HTMP(H2  )+HTMP(H2+1)+HTMP(H2+2)+HTMP(H2+3)+HTMP(H2+4)
          BACK = .5 * NBACK
          ZV = HTMP(HPEAK+1)   +HTMP(HPEAK+2)*2
     +        +HTMP(HPEAK+3)*3 +HTMP(HPEAK+4)*4
          ZV = ZV / PEAK
          ZVTX      = (HPEAK+ZV-.5)/BINZIV + ZLOW
          DZ =HTMP(HPEAK  )*(ZV   )**2 +HTMP(HPEAK+1)*(ZV-1.)**2
     +       +HTMP(HPEAK+2)*(ZV-2.)**2 +HTMP(HPEAK+3)*(ZV-3.)**2
     +       +HTMP(HPEAK+4)*(ZV-4.)**2
          IF(NPEAK.GT.NPKMIN) ICODE=ICODE+1
          SGN  = (PEAK - BACK)**2
          DSGN = BACK*.5 + PEAK
          IF(SGN/DSGN.GE.SBRAT .AND. NPEAK.GT.2) ICODE=ICODE+2
      IF(
     - ICODE.GE.IZRSLT(6)
     -)THEN
      IF(
     - ICODE.EQ.IZRSLT(6).AND.ABS(ZVTX).GT.ABS(ZPREV)
     -)THEN
      GOTO 15001
      ENDIF
          IZCNT=IZCNT+1
          SCPEAK = BACK * .2
          HTMP(HPEAK-2)=SCPEAK
          HTMP(HPEAK-1)=SCPEAK
          HTMP(HPEAK  )=SCPEAK
          HTMP(HPEAK+1)=SCPEAK
          HTMP(HPEAK+2)=SCPEAK
          HTMP(HPEAK+3)=SCPEAK
          HTMP(HPEAK+4)=SCPEAK
          HTMP(HPEAK+5)=SCPEAK
          HTMP(HPEAK+6)=SCPEAK
          FZRSLT(2) = DZ / (BINZIV**2 * PEAK)
          FZRSLT(3) = FZRSLT(2) / PEAK
          FZRSLT(2) = SQRT(FZRSLT(2))
          FZRSLT(3) = SQRT(FZRSLT(3))
          FZRSLT(4) = PEAK
          FZRSLT(5) = BACK
          FZRSLT(1) = ZVTX
          IND = ABS(ZVTX)*.01 + 1.
          IF(IND.GT.4) IND = 4
          IF(IZRSLT(6).EQ.1) IND = IND + 4
          INDLB = IND + 2
      ZPREV=ZVTX
      IZRSLT(6)=ICODE
      ICODE=0
      ELSE
      GOTO 15001
      ENDIF
      GOTO 15000
      ENDIF
15001 CONTINUE
      GOTO IZZZ05
      END
      SUBROUTINE INITZV
C====MACRO CZVPAR===================================
C-------------------------------------------
C   RESULTS FROM ZVERTF
C   P. STEFFEN (79/01/26)
C---------------------------------------------
      COMMON /CZVPAR/ LBZVDF,ZLOW,  BINZ,  NBINZ,
     ,                NWRDR1,LWRDC0,LWRDC1,LWRDC2,
     ,                IDZ1LM,IDZ2LM,NPKMIN,SBRAT,DFIMAX,
     ,                DZVPAR(5)
C==ENDMACRO CZVPAR========================================
        LBZVDF = 1
        ZLOW = -3500.
        BINZ = 70.
        NBINZ = 100
        NWRDR1 = 24
        LWRDC0 = 8
        LWRDC1 = 16
        LWRDC2 = 16
        IDZ1LM = 80
        IDZ2LM = 140
        NPKMIN = 8
        SBRAT  = 6.25
        DFIMAX = .1
      RETURN
      END
