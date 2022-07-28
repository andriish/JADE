C   03/12/83 312030149  MEMBER NAME  JINIT0   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE  JINIT
C-----------------------------------------------------------------------
C
C   AUTHOR:   E. ELSEN    03/10/78 :  INITIALISE THE MC TRACKING PROGRAM
C
C        MOD  E. ELSEN    18/11/82 :
C        MOD  J. HAGEMANN 18/01/83 :  CHANGED RADIUS OF RADIAL ELECTRIC
C             R. RAMCKE            :  FIELD AROUND SIGNAL WIRES
C        MOD  C. BOWDERY  30/08/83 :  PRINT WARNING IF VALIDATION TESTS
C                                  :  ARE DISABLED VIA COMMON/CVFLAG/
C   LAST MOD  C. BOWDERY   2/12/83 :  CHANGE /CVFLAG/
C
C-----------------------------------------------------------------------
C
C
      LOGICAL  VTEST , ALLSET
C
      COMMON / CVFLAG / VTEST(20)
C
      COMMON / CJCELL / NCELL(3),
     +                  NWIRES(3)
C
      COMMON / CJDRCH / RDEC(4),
     +                  PSIIN(3),
     +                  RINCR(3),
     +                  FIRSTW(3),
     +                  FSENSW(3),
     +                  RDEPTH,
     +                  SWDEPL,
     +                  YSUSPN,
     +                  TIMDEL(6), ZMAX, ZOFFS, ZRESOL, ZNORM,ZAL,ZSCAL,
     +                  DRIDEV,DRICOS,DRISIN
C
      COMMON / CGEO1  / BKGAUS, RPIP,DRPIP,XRLPIP, RBPC,DRBPC,XRLBPC,
     +                  RITNK,DRITNK,XRLTKI, R0ROH,DR0ROH,XR0ROH,
     +                  R1ROH,DR1ROH,XR1ROH, R2ROH,DR2ROH,XR2ROH,
     +                  R3ROH,DR3ROH,XR3ROH, ROTNK,DROTNK,XRLTKO,
     +                  RTOF,DRTOF,XRTOF, RCOIL, DRCOIL, XRCOIL,
     +                  ZJM,DZJM,XRZJM, ZJP,DZJP,XRZJP,
     +                  ZTKM,DZTKM,XRZTKM, ZTKP,DZTKP,XRZTKP,
     +                  ZBPPL,ZBPMI,ZTOFPL,ZTOFMI,
     +                  XRJETC,
     +                  RLG,ZLGPL,ZLGMI,OUTR2,CTLIMP,CTLIMM,DELFI,
     +                  BLXY,BLZ,BLDEP,ZENDPL,ZENDMI,DEPEND,
     +                  XHOL1,XHOL2,YHOL1,YHOL2,BLFI
CAV     +                  XHOL1,XHOL2,YHOL1,YHOL2
CAV  Same size required
C
      COMMON / CJXDAT / XSLOPE, YSLOPE, XL(3), XH(3), R3P, RD3P,
     +                  S, S2,
     +                  XSL3L, X3L, XSL3H, X3H, YSL3L, Y3L, YSL3H,
     +                  YHWIDT, SINHLF, COSHLF, DRITAN
C
C------------------  C O D E  ------------------------------------------
C
       PHI    = 6.2831853 / FLOAT( NCELL(3) )
       COSA   = COS( PHI )
       SINA   = SIN( PHI )
       YSLOPE = SINA / COSA
       XSLOPE = - YSLOPE
C
       PHI    = PHI / 2.
       SINHLF = SIN( PHI )
       COSHLF = COS( PHI )
       YHWIDT = 0.5
C
       XL(1)  = R0ROH + DR0ROH
       XL(2)  = R1ROH + DR1ROH
       XL(3)  = R2ROH + DR2ROH
       XH(1)  = R1ROH
       XH(2)  = R2ROH
       XH(3)  = R3ROH
C
       R3P    = R3ROH / COSA
       RD3P   = ( R3ROH + DR3ROH ) / COSA
       S      = RINCR(1)*DRICOS*.5
       S2     = S*S
C
       XSL3L  = SINHLF/COSHLF
       X3L    = XL(3) / COSHLF
       XSL3H  = - XSL3L
       X3H    = XH(3) / COSHLF
       YSL3H  = XSL3L
       YSL3L  = -XSL3L
       Y3L    = YHWIDT / COSHLF
       DRITAN = DRISIN / DRICOS
C                                 INITIALISATION CALLS
       CALL LGINIT
       CALL INIFWD
C
C                                 PRINT A WARNING IF ANY VALIDATION
C                                 TEST HAS BEEN DISABLED FOR MCVALI.
C
       ALLSET = .TRUE.
       DO  1  I = 1,20
         IF( VTEST(I) ) GO TO 1
         WRITE(6,10) I
 10      FORMAT(' *** WARNING ***    INPUT DATA TEST ',I2,' DISABLED')
         ALLSET = .FALSE.
  1    CONTINUE
C
C                                 IF ANY TEST IS DISABLED THEN PRINT
C                                 THE LIST OF TESTS.
C
       IF( ALLSET ) RETURN
C
       WRITE(6,20)
 20    FORMAT(/'  1 = BEAM ENERGY TEST        2 = PX, PY, PZ TEST'/
     +         '  3 = ENERGY TEST             4 = MASS(TYPE) TEST'/
     +         '  5 = E/P/MASS TEST           6 = VERTEX X,Y,Z TEST'/
     +         '  7 = VERTEX R TEST           8 = PTOT TEST'/
     +         '  9 = MASS TEST              10 = NULL TEST'/
     +         ' 11 = PARTICLE TYPE TEST     12 = CHARGE TEST'/
     +         ' 13 = EVENT NUMBER TEST      14 = NF TEST'/
     +         ' 15 = NCF TEST               16 = NNF TEST'/
     +         ' 17 = NF = NCF + NNF TEST    '//)
       RETURN
       END
