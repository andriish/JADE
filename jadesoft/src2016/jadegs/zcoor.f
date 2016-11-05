C   05/05/85 707281248  MEMBER NAME  ZCOOR    (ZS)          FORTRAN77
************************************************************************
      SUBROUTINE ZCOOR(JZETC,NZHIT,WIRLEN,ZCXCH,ZCVDR)
************************************************************************
*                                                                      *
*     LOADS RAW Z-CHAMBER DATA FROM ZETC BANK INTO IZDATA ARRAY        *
*     THEN CONVERTS TO Z AND PHI COORDINATES IN RZDATA ARRAY           *
*                                                                      *
*     INPUT ARGUMENTS : JZETC     = POINTER TO ZETC IN HALF-INTEGERS   *
*                       NZHIT     = NUMBER OF Z-CHAMBER HITS           *
*                       WIRLEN(2) = LENGTH OF INNER * OUTER Z-CH WIRES *
*                       ZCXCH     = CHARGE DIVISION CONSTANT           *
*                       ZCVDR     = DRIFT VELOCITY                     *
*                                                                      *
*     OUTPUT          : FILLED COMMON/CZDATA/                          *
*                                                                      *
************************************************************************
*                                                                      *
******COMMONBLOCKS
      COMMON/CZDATA/JZDATA(64),IZDATA(3,8,64),RZDATA(3,8,64)
******              JZDATA(NW)        NUMBER OF HITS ON WIRE NW
*                   IZDATA(*,         ADC1,ADC2,TDC
*                            NH,      HIT NUMBER (1--8)
*                               NW)   WIRE NUMBER (1--64)
*                   RZDATA(*,         Z-DZ,Z+DZ,PHI
******                       NH,NW)   HIT NO, WIRE NO.
*%MACRO CBCS
C ----------------------------------------------- BOS COMMON
      COMMON/BCS/IW(1)
      REAL RW(1)
      INTEGER*2 HW(1)
      EQUIVALENCE(IW(1),RW(1),HW(1))
C ----------------------------------------------- END BOS DECLARATIONS
      COMMON/CZCALI/IBAD(64),IZPED(2,64),IZT0(64)
******              IBAD(NW)          =1 IF ONLY ONE PRE-AMP WORKS
*                   IZPED(I,NW)       PEDESTAL FOR ADCI OF WIRE NW
*                   IZT0(NW)          TIME ZERO FOR WIRE NW
******                                ALL SET IN ZCHCAL
******END COMMONBLOCKS
******ARRAYS AND DECLARATIONS
      REAL ZWIR(64)      /-1126.,-1124.,  -976., -974.,  -826., -824.,
     +                     -676., -674.,  -526., -524.,  -376., -374.,
     +                     -226., -224.,   -76.,  -74.,
     +                                      76.,   74.,   226.,  224.,
     +                      376.,  374.,   526.,  524.,   676.,  674.,
     +                      826.,  824.,   976.,  974.,  1126., 1124.,
     +                    -1126.,-1124.,  -976., -974.,  -826., -824.,
     +                     -676., -674.,  -526., -524.,  -376., -374.,
     +                     -226., -224.,   -76.,  -74.,
     +                                      76.,   74.,   226.,  224.,
     +                      376.,  374.,   526.,  524.,   676.,  674.,
     +                      826.,  824.,   976.,  974.,  1126., 1124./
      REAL WIRLEN(2)
      PARAMETER(PI=3.1415927,TWOPI=6.2831853)
******END ARRAYS AND DECLARATIONS
********************************** C O D E *****************************
      J = JZETC+2
      DO 100 I=1,NZHIT
         NWIR              = HW(J+1)/8 + 1
         JZDATA(NWIR)      = JZDATA(NWIR) + 1
         NH                = JZDATA(NWIR)
         IZDATA(1,NH,NWIR) = HW(J+2) - IZPED(1,NWIR)
         IZDATA(2,NH,NWIR) = HW(J+3) - IZPED(2,NWIR)
         IZDATA(3,NH,NWIR) = HW(J+4) - IZT0(NWIR)
*
         TDC               = MAX(IZDATA(3,NH,NWIR),0)
*        ZTDC              = ZCVDR * (TDC+0.5)
         ZTDC              = ZCVDR * (TDC+RN(DUMMY))
*********
*        FOR THE TIME BEING, NO ANGULAR CORRECTIONS TO ZTDC
*********
         RZDATA(1,NH,NWIR) = ZWIR(NWIR) - ZTDC
         RZDATA(2,NH,NWIR) = ZWIR(NWIR) + ZTDC
*
         A1                = MAX(IZDATA(1,NH,NWIR),0)
         A2                = MAX(IZDATA(2,NH,NWIR),0)
         IF(A1.GE.1.E-6 .OR. A2.GE.1.E-6)         THEN
            ADIF = (A2-A1)/(A2+A1)
         ELSE
            ADIF = 0.
         ENDIF
         ILAY              = MOD(NWIR-1,2) +1
         WLEN              = WIRLEN(ILAY)
         WL1               = 0.5 * WLEN * (1. + ADIF/ZCXCH)
*********
*        FORCE 0<WL1<WIRLEN
*********
         IF(WL1.LT.0.)       WL1 = 0.
         IF(WL1.GT.WLEN)     WL1 = WLEN
*********
         CHI               = (WL1+100.)/(WLEN+200.) * PI
         IF(NWIR.LE.32)      CHI = CHI + PI
         PHI               = MOD(CHI+PI/2.,TWOPI)
         RZDATA(3,NH,NWIR) = PHI
         J=J+4
 100  CONTINUE
      RETURN
      END
