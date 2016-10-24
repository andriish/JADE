C 31938  MEMBER NAME  JEOSUM3  (GRAFIX.S)    SHELTRAN
C Copyright Stanford  Mortran3.1   (FORTRAN 66 11JUN85)
      SUBROUTINE DISAXI
      IMPLICIT INTEGER*2 (H)
      LOGICAL FLVCDO
      LOGICAL LSETAX
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
C   23/03/97 703231948  MEMBER NAME  MVERTEX0 (PATRECSR)    SHELTRAN
C**HEADER*** MEMBER  MVERTEX0       SAVED BY F22HAG  ON 88/05/24  AT 17:56
C     PARAMETER MACRO FOR VERTEX-FIT ROUTINES                           
C01 MAY 80)    DISAXI    SYSTEM/370 FORTRAN H EXTENDED (ENHANCED)    DATE 97.082/19.55.23       PAGE   2
      COMMON /CVTXC/ XB,YB,ZB,RTANK,DTANK,X0INN,SIGX0,SIGZ0,PNTMIN,     
     *               DISTB,COLL2,MITER,DSCONV,PRCUT,IREJTR,EEDPMN,      
     *               EEDPMX,EEDTMX,EEDRMX,SEMAX,SIMAX,SIGFAC,EEXYMN,    
     *               EEXYMX,PHEMAX,SIG1,SIG2,SIG3,CSECV,                
     *               ITDLEN,IVDLEN,SP0,SP1,DFMASS,SFMUSC, SIGFCZ        
C     MACRO FOR VERTEX-FIT ROUTINES ( AXIS AND STATISTICS )             
      COMMON /CVTX2/ MODE,TAXIS(12),SVR,HVTXST(120)                     
C                                                                       00000300
C   NOTE (JEO 23.3.97)  THAT HVTXST HAS DIM. 140 ON F22KLE.VERTEX.S LIB 00000310
      DIMENSION IVTXST(1)                                               
C                                                                       00000500
C   23/03/97 703231949  MEMBER NAME  MTRINF   (PATRECSR)    SHELTRAN
C**HEADER*** MEMBER  MTRINF         SAVED BY F22HAG  ON 87/04/03  AT 18:35
C
C     -------------- GENERALL TRACK-INFO COMMON -------------------
      DIMENSION IP(10,400)
      COMMON // P(10,400)
      EQUIVALENCE(IP(1,1),P(1,1))
C
      DIMENSION IPE(10,400)
      COMMON /CTRCK0 / NTCH,NTALL,PE(10,400),INDTRK(400),ITRFLG(400)
      EQUIVALENCE(IPE(1,1),PE(1,1))
C
      COMMON / CGVCDO / FLVCDO(20)
      COMMON / CHEADR / HEAD(108)
      COMMON / CVX / NNPATR, NDUMM, NNJETC, NNVTXC
      DIMENSION IDUM(20),PSAVE(4,60)
      character*2 HDM, HDL, HB1, HB2, HB3, HB4, HC
      DIMENSION HDM(5,6),HDL(5,2),HB1(5,8),HB2(5,8),HB3(5,12),HB4(5,10)
      DIMENSION HC(5)
      DATA HDM / 'D0','  ','  ','  ','  ', 'D0',' b','ar','  ','  ', 'D+
     *','  ','  ','  ','  ', 'D-','  ','  ','  ','  ', 'Ds','+ ','  ','
     * ','  ', 'Ds','- ','  ','  ','  ' /
      DATA HDL / 'La','mc','  ','  ',' +', 'La','mc',' b','ar',' -' /
      DATA HB1 / 'bu','(b','ar',') ','- ', 'b(','ba','r)','u ','+ ', 'bd
     *','(b','ar',') ','0 ', 'b(','ba','r)','d ','0 ', 'bs','(b','ar',')
     * ','0 ', 'b(','ba','r)','s ','0 ', 'bc','(b','ar',') ','- ', 'b(',
     *'ba','r)','c ','+ ' /
      DATA HB2 / 'bu','u1','  ','  ',' +', 'bu','u1',' b','ar',' -', 'bd
     *','u1','  ','  ',' 0', 'bd','u1',' b','ar',' 0', 'bd','d1','  ','
     * ',' -', 'bd','d1',' b','ar',' +', 'bs','u1','  ','  ',' 0', 'bs',
     *'u1',' b','ar',' 0' /
      DATA HB3 / 'bd','u0','  ','  ',' 0', 'bd','u0',' b','ar',' 0', 'bs
     *','u0','  ','  ',' 0', 'bs','u0',' b','ar',' 0', 'bs','d0','  ','
     * ',' -', 'bs','d0',' b','ar',' +', 'bc','u0','  ','  ',' +', 'bc',
     *'u0',' b','ar',' -', 'bc','d0','  ','  ',' 0', 'bc','d0',' b','ar'
     *,' 0', 'bc','s0','  ','  ',' 0', 'bc','s0',' b','ar',' 0' /
      DATA HB4 / 'bu','u*','  ','  ',' +', 'bu','u*',' b','ar',' -', 'bd
     *','u*','  ','  ',' 0', 'bd','u*',' b','ar',' 0', 'bd','d*','  ','
     * ',' -', 'bd','d*',' b','ar',' +', 'bs','u*','  ','  ',' 0', 'bs',
     *'u*',' b','ar',' 0', 'bs','d*','  ','  ',' -', 'bs','d*',' b','ar'
     *,' +' /
      IF  ((LASTVW.GT.4).AND.((LASTVW.NE.17).AND.(LASTVW.NE.20))) GO TO
     *20
      N = IFIX(ACMD)
      JMODE = N
      IF((N .GT. 0 .AND. N .LT. 6))GOTO 30
40    CONTINUE
      CALL TRMOUT(80,'ENTER MODE :^')
      CALL TRMOUT(80,'1 = Tau-fligth-direction^')
C01 MAY 80)    DISAXI    SYSTEM/370 FORTRAN H EXTENDED (ENHANCED)    DATE 97.082/19.55.23       PAGE   3
      CALL TRMOUT(80,'2 = Sphericity axis^')
      CALL TRMOUT(80,'3 = Thrust axis^')
      CALL TRMOUT(80,'4 = Calculate impact-parameter^')
      CALL TRMOUT(80,'5 = Display C- or- B-hadron flight direction^')
      CALL FYRINT(JMODE,ID2,ID3,ID4)
30    CONTINUE
      IF((JMODE .LT. 1 .OR. JMODE .GT. 5))GOTO 40
      IF  (JMODE.NE.1) GO TO 60
      LSETAX = .FALSE.
      IPTTOP = IDATA(IBLN('TTOP'))
      IF  (IPTTOP.GT.0) GO TO 80
      CALL TRMOUT(80,' TTOP-bank not existing^')
      GOTO 90
80    CONTINUE
      LSETAX = .TRUE.
      XF3 = 0.5*(ADATA(IPTTOP+1) - ADATA(IPTTOP+16))
      YF3 = 0.5*(ADATA(IPTTOP+2) - ADATA(IPTTOP+17))
      ZF3 = 0.5*(ADATA(IPTTOP+3) - ADATA(IPTTOP+18))
      RF2 = SQRT(XF3**2 + YF3**2)
      XRV = 0.0
      YRV = 0.0
      DXR = 0.0
      DYR = 0.0
      IF  (.NOT.(FLVCDO(12))) GO TO 110
      CALL VTXCRV( INT(HEAD(18)), XRV, YRV, DXR, DYR ) ! PMF 08/11/99: add run argument HEAD(18)
110   CONTINUE
      CALL SETTAX( XRV, YRV, DXR, DYR, XF3, YF3, ZF3 )
      IF  (RF2.LE.0.0) GO TO 130
      GOTO 140
130   CONTINUE
      GO TO 50
60    IF(JMODE.NE.2)GO TO 150
      LSETAX = .FALSE.
      CALL CLOC(IPPATR,'PATR',NNPATR)
      IPHWDS = IDATA(IBLN('HWDS'))
      IF((FLVCDO(15) .AND. IPHWDS .GT. 0))IPPATR = IPHWDS
      CALL TRN4V(0,IPPATR,1,20,NCONT,IDUM,IERR)
      IF  (IERR.EQ.0) GO TO 170
      CALL TRMOUT(80,'Select PATR-bank first !^')
      GOTO 90
170   CONTINUE
      NTRK = IDATA(IPPATR+2)
      LSETAX = .TRUE.
      WRITE(6,180)
180   FORMAT(' Enter number of track which should be omitted in SPHERIC'
     * ,'ITY calculation : (or press only )')
190   CONTINUE
      CALL TRMIN( 4, ICD )
      CALL GETINT( ICD, ITRK )
      IF((ITRK .GE. 0 .AND. ITRK .LE. NTRK))GOTO 200
      CALL TRMOUT(80,' Track number invalid!  Enter new number!^')
      GOTO 190
200   CONTINUE
      IF  (ITRK.LE.0) GO TO 220
      IPOSAR = INDTRK(ITRK)
      IF  (IPOSAR.LE.0.OR.IPOSAR.GT.60) GO TO 240
      DO 251 J=1,4
      PSAVE(J,IPOSAR) = P(J,IPOSAR)
      P(J,IPOSAR) = 0.0
C01 MAY 80)    DISAXI    SYSTEM/370 FORTRAN H EXTENDED (ENHANCED)    DATE 97.082/19.55.23       PAGE   4
251   CONTINUE
252   CONTINUE
240   CONTINUE
220   CONTINUE
      NH = NTALL
      NSPH = NTALL + 1
      CALL SPTHAK(1,NH,0,THR,NSPH,SPHR,0,AKO,IER )
      IF  (ITRK.LE.0) GO TO 270
      DO 281 J=1,4
      P(J,IPOSAR) = PSAVE(J,IPOSAR)
281   CONTINUE
282   CONTINUE
270   CONTINUE
      XF3 = P(1,NSPH)
      YF3 = P(2,NSPH)
      ZF3 = P(3,NSPH)
      WRITE(6,290)XF3,YF3,ZF3
290   FORMAT(' SPHERICITY : ',3F8.3)
      RF2 = SQRT(XF3**2 + YF3**2)
      XRV = 0.0
      YRV = 0.0
      DXR = 0.0
      DYR = 0.0
      IF  (.NOT.(FLVCDO(12))) GO TO 310
      CALL VTXCRV( INT(HEAD(18)), XRV, YRV, DXR, DYR ) ! PMF 08/11/99: add run argument HEAD(18)
310   CONTINUE
      MODE = 1
      TAXIS(1) = XRV
      TAXIS(2) = YRV
      TAXIS(3) = 0.0
      TAXIS(4) = DXR
      TAXIS(5) = DYR
      TAXIS(6) = 10000.0
      TAXIS(7) = XF3
      TAXIS(8) = YF3
      TAXIS(9) = ZF3
      TAXIS(10) = 0.0
      TAXIS(11) = 0.0
      TAXIS(12) = 0.0
      IDC = 23
      IF  (RF2.LE.0.0) GO TO 330
      GOTO 140
330   CONTINUE
      GO TO 50
150   IF(JMODE.NE.3)GO TO 340
      LSETAX = .FALSE.
      CALL CLOC(IPPATR,'PATR',NNPATR)
      IPHWDS = IDATA(IBLN('HWDS'))
      IF((FLVCDO(15) .AND. IPHWDS .GT. 0))IPPATR = IPHWDS
      CALL TRN4V(0,IPPATR,1,20,NCONT,IDUM,IERR)
      IF  (IERR.EQ.0) GO TO 360
      CALL TRMOUT(80,'Select PATR-bank first !^')
      GOTO 90
360   CONTINUE
      NTRK = IDATA(IPPATR+2)
      LSETAX = .TRUE.
      WRITE(6,370)
370   FORMAT(' Enter number of track which should be omitted in THRUST '
     * ,'calculation : (or press only )')
C01 MAY 80)    DISAXI    SYSTEM/370 FORTRAN H EXTENDED (ENHANCED)    DATE 97.082/19.55.23       PAGE   5
380   CONTINUE
      CALL TRMIN( 4, ICD )
      CALL GETINT( ICD, ITRK )
      IF((ITRK .GE. 0 .AND. ITRK .LE. NTRK))GOTO 390
      CALL TRMOUT(80,' Track number invalid!  Enter new number!^')
      GOTO 380
390   CONTINUE
      IF  (ITRK.LE.0) GO TO 410
      IPOSAR = INDTRK(ITRK)
      IF  (IPOSAR.LE.0.OR.IPOSAR.GT.60) GO TO 430
      DO 441 J=1,4
      PSAVE(J,IPOSAR) = P(J,IPOSAR)
      P(J,IPOSAR) = 0.0
441   CONTINUE
442   CONTINUE
430   CONTINUE
410   CONTINUE
      NH = NTALL
      NH4 = NH+4
      CALL SPTHAK(1,NH,NH4,THR,0,SPHR,0,AKO,IER )
      IF  (ITRK.LE.0) GO TO 460
      DO 471 J=1,4
      P(J,IPOSAR) = PSAVE(J,IPOSAR)
471   CONTINUE
472   CONTINUE
460   CONTINUE
      XF3 = P(1,NH4)
      YF3 = P(2,NH4)
      ZF3 = P(3,NH4)
      WRITE(6,480)XF3,YF3,ZF3
480   FORMAT(' THRUST : ',3F8.3)
      RF2 = SQRT(XF3**2 + YF3**2)
      XRV = 0.0
      YRV = 0.0
      DXR = 0.0
      DYR = 0.0
      IF  (.NOT.(FLVCDO(12))) GO TO 500
      CALL VTXCRV( INT(HEAD(18)), XRV, YRV, DXR, DYR ) ! PMF 08/11/99: add run argument HEAD(18)
500   CONTINUE
      MODE = 1
      TAXIS(1) = XRV
      TAXIS(2) = YRV
      TAXIS(3) = 0.0
      TAXIS(4) = DXR
      TAXIS(5) = DYR
      TAXIS(6) = 10000.0
      TAXIS(7) = XF3
      TAXIS(8) = YF3
      TAXIS(9) = ZF3
      TAXIS(10) = 0.0
      TAXIS(11) = 0.0
      TAXIS(12) = 0.0
      IDC = 1234
      IF  (RF2.LE.0.0) GO TO 520
      GOTO 140
520   CONTINUE
      GO TO 50
340   IF(JMODE.NE.4)GO TO 530
      IF  (.NOT.(.NOT. LSETAX)) GO TO 550
C01 MAY 80)    DISAXI    SYSTEM/370 FORTRAN H EXTENDED (ENHANCED)    DATE 97.082/19.55.23       PAGE   6
      CALL TRMOUT(80, ' Axis is NOT set! Type DAX without argument.^')
      GOTO 90
550   CONTINUE
      CALL VTXINI
      IH = IDATA(IBLN('HEAD'))
      CALL CLOC(IPO,'PATR',NNPATR)
      IPHWDS = IDATA(IBLN('HWDS'))
      IF((FLVCDO(15) .AND. IPHWDS .GT. 0))IPO = IPHWDS
      IF((IPO .LE. 0))GOTO 90
      NTRK = IDATA(IPO+2)
      CALL VTXPRE(IH,IPO)
      CALL TRMOUT(80, ' Enter track number for impact-parameter calculat
     *ion :^')
560   CONTINUE
      CALL TRMIN( 4, ICD )
      CALL GETINT( ICD, ITRK )
      IF((ITRK .GT. 0 .AND. ITRK .LE. NTRK))GOTO 570
      CALL TRMOUT(80,' Track number invalid!  Enter new number!^')
      GOTO 560
570   CONTINUE
      CALL VTXCRV( INT(HEAD(18)), XRV, YRV, DXR, DYR ) ! PMF 08/11/99: add run argument HEAD(18)
      J = (ITRK-1)*ITDLEN
      CALL VTXIMP(J,XRV,YRV,XT,YT,ZT,DXT2,DYT2,DZT2,PHIT,DPHIT,ST, TAXIS
     *(7),TAXIS(8),AIMP)
      CALL MOVEA( -XRV, YRV )
      CALL DRAWA( -XT , YT )
      IPOSAR = INDTRK(ITRK)
      IF  (IPOSAR.LE.0) GO TO 590
      RNORM = SQRT((TAXIS(7)**2 + TAXIS(8)**2)* (P(1,IPOSAR)**2 + P(2,IP
     *OSAR)**2))
      IF  (RNORM.LE.0.0) GO TO 610
      COSTRK = (P(1,IPOSAR)*TAXIS(7) + P(2,IPOSAR)*TAXIS(8))/RNORM
610   CONTINUE
      IF((COSTRK .LT. 0.0))AIMP = -AIMP
590   CONTINUE
      WRITE(6,620)ITRK,AIMP
620   FORMAT(' Impact-parameter for track',I3,' is :',F9.3)
      GO TO 50
530   IF(JMODE.NE.5)GO TO 630
      IF  (HEAD(18).LE.100) GO TO 650
      CALL TRMOUT(80,' DAX 5 not available for real data!^')
      GOTO 90
650   CONTINUE
      IPPALL = IDATA(IBLN('PALL'))
      IF((IPPALL .LE. 0))GOTO 90
      IFL = IDATA(IPPALL+9)
      IF((IFL .NE. 4 .AND. IFL .NE. 5))GOTO 90
      LPHEAD = IDATA(IPPALL+1)
      LPINF = IDATA(IPPALL+2)
      NPPAR = IDATA(IPPALL+4)
      DO 661 J=1,NPPAR
      IPPL = IPPALL + LPHEAD + (J-1)*LPINF
      ITYS = IDATA(IPPL+7)
      ITYP = IABS(ITYS)
*** PMF 06/12/99: transform my code numbers for particles into
*** JETSET 6.3 CODE + 1000
      if(ityp.ge.100000) ityp=ityp-100000
*** PMF(end)
      IF((((ITYP.LT.1101 .OR. ITYP.GT.1104) .AND. (ITYP.LT.1145 .OR. ITY
     *P.GT.1148) .AND. (ITYP.LT.1241 .OR. ITYP.GT.1246) .AND. (ITYP.LT.1
     *293 .OR. ITYP.GT.1297)) .AND. IFL.EQ.5))GO TO661
      IF(((ITYP.LT.1020 .OR. ITYP.GT.1022) .AND. ITYP.NE.1058 .AND. IFL.
     *EQ.4))GO TO661
C01 MAY 80)    DISAXI    SYSTEM/370 FORTRAN H EXTENDED (ENHANCED)    DATE 97.082/19.55.23       PAGE   7
      PX = ADATA(IPPL+1)
      PY = ADATA(IPPL+2)
      P2 = SQRT(PX**2+PY**2)
      EX = PX/P2
      EY = PY/P2
      CALL MOVEA(0.0,0.0)
      XE = -EX*5.50*16. !PMF 06/12/99: append factor 16.0
      YE = EY*5.50*16. !PMF 06/12/99: append factor 16.0
      CALL DRAWA(XE,YE)
      CALL MOVEA(XE+0.02,YE+0.02)
      CALL CHRSIZ(4)
      IF  (IFL.NE.4) GO TO 680
      IF  (ITYP.NE.1058) GO TO 700
      IA = 1
      IF((ITYS .LT. 0))IA = 2
      DO 711 K=1,5
      HC(K) = HDL(K,IA)
711   CONTINUE
712   CONTINUE
      GO TO 690
700   CONTINUE
      IA = 2*(ITYP-1020) + 1
      IF((ITYS .LT. 0))IA = IA + 1
      DO 721 K=1,5
      HC(K) = HDM(K,IA)
721   CONTINUE
722   CONTINUE
690   CONTINUE
      GO TO 670
680   CONTINUE
      IF  (ITYP.LT.1101.OR.ITYP.GT.1104) GO TO 740
      IA = 2*(ITYP-1101) + 1
      IF((ITYS .LT. 0))IA = IA + 1
      DO 751 K=1,5
      HC(K) = HB1(K,IA)
751   CONTINUE
752   CONTINUE
      GO TO 730
740   IF(ITYP.LT.1145.OR.ITYP.GT.1148)GO TO 760
      IA = 2*(ITYP-1145) + 1
      IF((ITYS .LT. 0))IA = IA + 1
      DO 771 K=1,5
      HC(K) = HB2(K,IA)
771   CONTINUE
772   CONTINUE
      GO TO 730
760   IF(ITYP.LT.1241.OR.ITYP.GT.1246)GO TO 780
      IA = 2*(ITYP-1241) + 1
      IF((ITYS .LT. 0))IA = IA + 1
      DO 791 K=1,5
      HC(K) = HB3(K,IA)
791   CONTINUE
792   CONTINUE
      GO TO 730
780   IF(ITYP.LT.1293.OR.ITYP.GT.1297)GO TO 800
      IA = 2*(ITYP-1293) + 1
      IF((ITYS .LT. 0))IA = IA + 1
      DO 811 K=1,5
      HC(K) = HB4(K,IA)
C01 MAY 80)    DISAXI    SYSTEM/370 FORTRAN H EXTENDED (ENHANCED)    DATE 97.082/19.55.23       PAGE   8
811   CONTINUE
812   CONTINUE
730   CONTINUE
800   CONTINUE
670   CONTINUE
      CALL EOUTST(10,HC)
661   CONTINUE
662   CONTINUE
50    CONTINUE
630   CONTINUE
      GOTO 90
140   CONTINUE
      XF2 = XF3/RF2
      YF2 = YF3/RF2
      RAB = RTOF - 50.0
      IF((LASTVW .EQ. 17))RAB = 6.0
      IF((LASTVW .EQ. 20))RAB = RITNK + 110.0
      XX = RAB*XF2
      YY = RAB*YF2
      CALL MOVEA( -XX-XRV, YY+YRV )
      CALL DASHA( XX-XRV, -YY+YRV, IDC )
20    CONTINUE
90    CONTINUE
      RETURN
      END
