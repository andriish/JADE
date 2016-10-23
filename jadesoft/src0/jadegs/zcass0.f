C   05/05/85 707281245  MEMBER NAME  ZCASS0   (ZS)          FORTRAN77
************************************************************************
      SUBROUTINE ZCASS(IPATT,NTPAT,NWPAT,NASS1,NASS2)
************************************************************************
*                                                                      *
*     ASSOCIATES Z-CHAMBER HITS WITH PATR TRACKS AND FILLS RESULTS     *
*     INTO RZTRK, IZTRK AND IZASS ARRAYS                               *
*                                                                      *
*     INPUT ARGUMENTS : IPATT     = POINTER TO 1ST TRACK IN PATR       *
*                       NTPAT     = NUMBER OF PATR TRACKS              *
*                       NWPAT     = NUMBER OF WORDS/TRACK IN PATR      *
*                                                                      *
*     OUTPUT          : FILLED COMMON/CZTRK/ AND COMMON/CZASS/         *
*                       NASS1     = NO. OF TRACKS WITH .GE.1 Z-C HIT   *
*                       NASS2     = NO. OF TRACKS WITH 2 Z-CH HITS     *
*                                                                      *
************************************************************************
*
******COMMONBLOCKS
      COMMON/CZDATA/JZDATA(64),IZDATA(3,8,64),RZDATA(3,8,64)
******              JZDATA(NW)        NUMBER OF HITS ON WIRE NW
*                   IZDATA(*,         ADC1,ADC2,TDC
*                            NH,      HIT NUMBER (1--8)
*                               NW)   WIRE NUMBER (1--64)
*                   RZDATA(*,         Z-DZ,Z+DZ,PHI
******                       NH,NW)   HIT NO, WIRE NO.
      COMMON/CZTRK /RZTRK(5,100),IZTRK(3,2,100)
******              RZTRK(*,NTR)      ZI1,PHI1,ZI2,PHI2,TANTH
*                                         FOR TRACK NUMBER NTR
*                   IZTRK(*,          IM(=1,-DZ,=2,+DZ),NH,NW
*                           IL,       LAYER(=1,INNER,=2,OUTER)
******                         NTR)   TRACKNO
      COMMON/CZASS /IZASS(8,64)
******
*                   IZASS(I,NW)       NTR ASS. WITH I'TH HIT ON WIRE NW
******
      COMMON/CZCALI/IBAD(64),IZPED(2,64),IZT0(64)
******              IBAD(NW)          =1 IF ONLY ONE PRE-AMP WORKS
*                   IZPED(I,NW)       PEDESTAL FOR ADCI OF WIRE NW
*                   IZT0(NW)          TIME ZERO FOR WIRE NW
******                                ALL SET IN ZCHCAL
      COMMON/CZEXT /XT, YT, ZT,XI1,YI1,ZI1,XI2,YI2,ZI2
******              XT, YT, ZT        LAST POINT ON TRACK
*                   XI1,YI1,ZI1       INTERSECTION WITH INNER LAYER
*                   XI2,YI2,ZI2            ''       ''  OUTER LAYER
******              COMMON FILLED FOR EACH PATR TRACK BY SUBR *ZEXTRA*
*%MACRO CBCS
C ----------------------------------------------- BOS COMMON
      COMMON/BCS/IW(1)
      REAL RW(1)
      INTEGER*2 HW(1)
      EQUIVALENCE(IW(1),RW(1),HW(1))
C ----------------------------------------------- END BOS DECLARATIONS
******ARRAYS AND DECLARATIONS
      REAL ZWIR(64)      /-1126.,-1124.,  -976., -974.,  -826., -824.,
     *                     -676., -674.,  -526., -524.,  -376., -374.,
     *                     -226., -224.,   -76.,  -74.,
     *                                      76.,   74.,   226.,  224.,
     *                      376.,  374.,   526.,  524.,   676.,  674.,
     *                      826.,  824.,   976.,  974.,  1126., 1124.,
     *                    -1126.,-1124.,  -976., -974.,  -826., -824.,
     *                     -676., -674.,  -526., -524.,  -376., -374.,
     *                     -226., -224.,   -76.,  -74.,
     *                                      76.,   74.,   226.,  224.,
     *                      376.,  374.,   526.,  524.,   676.,  674.,
     *                      826.,  824.,   976.,  974.,  1126., 1124./
      REAL DZMIN(2),DZ(2),DFIMIN(2)
      LOGICAL LPHI/.TRUE./
      PARAMETER(PI=3.1415927,TWOPI=6.2831853,
     *          PHICUT=0.349,ZCUT=75.,DWIR=10.)
***** END ARRAYS AND DECLARATIONS
********************************** C O D E *****************************
      DO 100 I=1,NTPAT
         IPAT = IPATT + (I-1)*NWPAT
         CALL ZEXTRA(IPAT,IER)
*             ZEXTRA EXTRAPOLATES TRACK TO Z-CH.
*                    IER NON-ZERO IF NO INTERCEPT, OR INTERCEPT
*                    WITHIN DEAD SPACES OF Z-CHAMBER.
         IF(IER.NE.0)              THEN
            RZTRK(1,I) = 9999.
            RZTRK(2,I) = IER
            PHI        = ATAN2(YT,XT)
            IF(PHI.LT.0.)          PHI  = PHI  + TWOPI
            RZTRK(4,I) = PHI
            GOTO 100
         ELSE
            RZTRK(1,I) = ZI1
            PHI1       = ATAN2(YI1,XI1)
            IF(PHI1.LT.0.)         PHI1 = PHI1 + TWOPI
            RZTRK(2,I) = PHI1
            RZTRK(3,I) = ZI2
            PHI2       = ATAN2(YI2,XI2)
            IF(PHI2.LT.0.)         PHI2 = PHI2 + TWOPI
            RZTRK(4,I) = PHI2
            RZTRK(5,I) = RW(IPAT+30)
         ENDIF
         NCELL = (ZI1+1200.)/150.
         NWIRE = 2*NCELL + 1
         IF(XI1.LT.0.)             NWIRE = NWIRE + 32
         NWLOW = NWIRE - 2
         NWHIGH= NWIRE + 3
*********
*        TAKE CARE OF WIRES NEAR THE CHAMBER ENDS
*********
         IF(NWLOW .LT. 1)                         NWLOW =  1
         IF(NWLOW .LE.32 .AND. NWIRE.GE.33)       NWLOW = 33
         IF(NWHIGH.GT.64)                         NWHIGH= 64
         IF(NWHIGH.GE.33 .AND. NWIRE.LE.32)       NWHIGH= 32
*********
*        LOOP OVER HITS IN CELLS N-1 TO N+1 (WHERE N IS THE
*        CELL HIT BY THE TRACK) LOOKING FOR MATCHING Z-HITS.
*********
         DZMIN(1)  = 9999.
         DZMIN(2)  = 9999.
         DFIMIN(1) = 9999.
         DFIMIN(2) = 9999.
         DO 110 NW=NWLOW,NWHIGH
            NH = JZDATA(NW)
            IF(NH.LE.0)            GOTO 110
            IL = MOD(NW-1,2) + 1
            ZI = RZTRK(2*IL-1,I)
            FI = RZTRK(2*IL,  I)
            DO 120 N=NH,1,-1
               IF(N.NE.NH)         THEN
                  DTDC = IZDATA(3,N,NW)-IZDATA(3,N+1,NW)
                  FIN  = RZDATA(3,N,NW)
                  FIN1 = RZDATA(3,N+1,NW)
C  I DON'T UNDERSTAND WHAT THE FOLLOWING STATEMENTS
C  ARE INTENDED FOR BUT PROBABLY DO NOT DO IT    J.SPITZER  24/7/87
                  IF(FIN .LT.PI/2.)     FIN = FIN +TWOPI
                  IF(FIN1.LT.PI/2.)     FIN1= FIN1+TWOPI
                  DFIZ = ABS(FIN-FIN1)
                  IF(DTDC.LT.30 .AND. DFIZ.LT.PHICUT)        THEN
                     IZASS(N,NW) = -1
                     GOTO 120
                  ENDIF
               ENDIF
************
*              IF DPHI > PHICUT (20 DEG) NO MATCH UNLESS WIRE IS BAD
************
C              DPHI  = FI - RZDATA(3,N,NW)
C  REPLCED   J.SPITZER  24/7/87
               DPHI  = ACOS(COS(FI - RZDATA(3,N,NW)))
               IF(IBAD(NW).EQ.0 .AND. ABS(DPHI).GT.PHICUT)   GOTO 120
               DZ(1) = ZI - RZDATA(1,N,NW)
               DZ(2) = ZI - RZDATA(2,N,NW)
               IM    = 1
               IF(ABS(DZ(2)) .LT.ABS(DZ(1)))                 IM = 2
               IF(LPHI .AND. IBAD(NW).EQ.0)                  THEN
                  IF(ABS(DPHI).GE.ABS(DFIMIN(IL)))           THEN
                     GOTO 120
                  ELSEIF(ABS(DZ(IM)).LT.ZCUT)                THEN
                     DFIMIN(IL) = DPHI
                  ENDIF
               ELSEIF(ABS(DZ(IM)).GE.ABS(DZMIN(IL)))         THEN
                  GOTO 120
               ENDIF
**************
*              ACCEPT ONLY IF DZ<ZCUT (75 MM)
**************
               IF(ABS(DZ(IM)).LT.ZCUT)                       THEN
                  DZMIN(IL)     = DZ(IM)
                  IZTRK(1,IL,I) = IM
                  IZTRK(2,IL,I) = N
                  IZTRK(3,IL,I) = NW
               ENDIF
 120        CONTINUE
 110     CONTINUE
*********
*        NOW DO ANGULAR CORRECTIONS USING TANTH (TRACK)
*********
         TTR = RZTRK(5,I)
         CTR = ABS(COS(ATAN(TTR)))
C        CALL HIST(902,CTR)
C        CALL CORR(903,TTR,CTR)
         IMI = IZTRK(1,1,I)
         IHI = IZTRK(2,1,I)
         IWI = IZTRK(3,1,I)
         IMO = IZTRK(1,2,I)
         IHO = IZTRK(2,2,I)
         IWO = IZTRK(3,2,I)
C        TZC = (RZDATA(IMO,IHO,IWO)-RZDATA(IMI,IHI,IWI))/DWIR
C INDEX IS JUST GOING TO BE CHECKED
C MOVE ABOVE STATEMENT DOWN TO WHERE IT BELONGS     J.SPITZER  24/7/87
         IF(IMI.NE.0)              THEN
            IZASS(IHI,IWI) = I
            ZTDCO          = ZWIR(IWI)-RZDATA(1,IHI,IWI)
C           CALL HIST(900,ZTDCO)
            IF(ZTDCO.LE.DWIR/2.)   THEN
               ZTDC = ZTDCO/CTR
            ELSE
               ZTDC = ZTDCO + 0.5*DWIR*(1./CTR - 1.)
            ENDIF
C           CALL HIST(901,ZTDC)
C           CALL CORR(904,ZTDCO,ZTDC)
            RZDATA(1,IHI,IWI) = ZWIR(IWI)-ZTDC
            RZDATA(2,IHI,IWI) = ZWIR(IWI)+ZTDC
         ENDIF
         IF(IMO.NE.0)              THEN
            IZASS(IHO,IWO) = I
            ZTDCO          = ZWIR(IWO)-RZDATA(1,IHO,IWO)
C           CALL HIST(900,ZTDCO)
            IF(ZTDCO.LE.DWIR/2.)   THEN
               ZTDC = ZTDCO/CTR
            ELSE
               ZTDC = ZTDCO + 0.5*DWIR*(1./CTR - 1.)
            ENDIF
C           CALL HIST(901,ZTDC)
C           CALL CORR(904,ZTDCO,ZTDC)
            RZDATA(1,IHO,IWO) = ZWIR(IWO)-ZTDC
            RZDATA(2,IHO,IWO) = ZWIR(IWO)+ZTDC
         ENDIF
         IF(IMI.NE.0  .OR. IMO.NE.0)              NASS1 = NASS1+1
         IF(IMI.NE.0 .AND. IMO.NE.0)              THEN
            TZC = (RZDATA(IMO,IHO,IWO)-RZDATA(IMI,IHI,IWI))/DWIR
            NASS2 = NASS2+1
************
*           CHECK CHOICE OF MIRROR USING TAN THETA
************
            IF(ABS(TTR-TZC).GT.0.2)               THEN
               TZ1 = RZDATA(3-IMO,IHO,IWO)-RZDATA(3-IMI,IHI,IWI)
               TZ2 = RZDATA(  IMO,IHO,IWO)-RZDATA(3-IMI,IHI,IWI)
               TZ3 = RZDATA(3-IMO,IHO,IWO)-RZDATA(  IMI,IHI,IWI)
               IF(ABS(TTR-TZ1/DWIR).LT.0.3)       THEN
                  IZTRK(1,1,I) = 3-IMI
                  IZTRK(1,2,I) = 3-IMO
               ELSEIF(ABS(TTR-TZ2/DWIR).LT.0.3)   THEN
                  IZTRK(1,1,I) = 3-IMI
               ELSEIF(ABS(TTR-TZ3/DWIR).LT.0.3)   THEN
                  IZTRK(1,2,I) = 3-IMO
               ENDIF
            ENDIF
         ENDIF
 100  CONTINUE
      RETURN
      END
