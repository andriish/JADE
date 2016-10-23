C   01/11/84 807251545  MEMBER NAME  FICOOV   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE FICOOV( IFLAG, NIV, NWEV, IM, IPTRK )
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. HAGEMANN   02/12/82 :  CALCULATE COORDINATES OF
C              R. RAMCKE                 VERTEX CHAMBER HITS
C
C         MOD: J. HAGEMANN   10/10/84  : NOW OWN MEMBER (FROM EVDISP)
C         MOD: J. HAGEMANN   05/03/86  : CHANGES FOR REAL AND MC DATA
C         MOD: J. HAGEMANN   18/04/86  : CHANGES FOR REAL DATA
C    LAST MOD: J. HAGEMANN   27/01/88  : FOR VTXC-9 CORRECTION
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL TBIT, FL18, FL22, FL24
C
#include "cgraph.for"
#include "cdata.for"
#include "cjvcel.for"
#include "cjvtxc.for"
#include "mvccal.for"
C
      COMMON/CWORK1/R,FI,R1,FI1,X1,Y1,R2,FI2,X2,Y2,ZET,EX,EY,COSPH,SINPH
     +             ,KZAMP
C
      COMMON / CJTRIG / PI,TWOPI
      COMMON / CPROJ  / XMINR,XMAXR,YMINR,YMAXR,IPRJC,FL18,FL22,FL24
      COMMON / CHEADR / HEAD(108)
      COMMON / CVX    / NNPATR,NDUMM,NNJETC,NNVTXC
C
C-----------------  C O D E  -------------------------------------------
C
      IPRJC = 0
C
C                       COMPUTE FI OF WIRE, RADIUS ETC.
      NIWV = HDATA(NIV)
      IF( NIWV .LE. 168 .AND. NIWV .GT. 0 ) GO TO 10
         CALL DIAGIN('ILLEGAL WIRE NO.    ',2,NIWV,NIV)
         CALL TRMOUT(80,'THIS CAN HAVE SEVERAL REASONS, E.G. WRONG POINT
     $ERS, WRONG BANK LENGTH OR^')
         CALL TRMOUT(80,'(LESS LIKELY), CORRUPT DATA. THE DISPLAY OF VER
     $TEX CHAMBER HITS STOPS HERE.^')
         NWEV = -1000
         RETURN
C
   10 NCEV = (NIWV - 1)/ 7
      NWEV = NIWV - NCEV*7
      VANG = 0.0
      IF( HEAD(18) .GT. 100 ) VANG = VROT
C
      IF( DSPDTL(26) .AND. TBIT(NWEV,31) ) GO TO 100
C
      R  = DISTW1 + FLOAT(NWEV-1)*DISTPW
      FI = ANG1 + NCEV*ANG2 + VANG
      IF( FI .LT. 0.0 )   FI = FI + TWOPI
      IF( FI .GT. TWOPI ) FI = FI - TWOPI
      IF( IFLAG .NE. 1 ) GO TO 20
C                        DECIDE POS/NEG X OR Y
      IF( DSPDTL(9) ) R = R*WRAP(FI)
C                        COORDINATE CALCULATIONS
   20 CONTINUE
      NIV = NIV + 1
C                        GET Z-COORDINATE
      IAV1 = HDATA(NIV)
      IAV2 = HDATA(NIV + 1)
      IF( (IAV1 + IAV2) .GT. 0 ) GOTO 30
        ZET = 0.
        GOTO 40
   30 ZET = ZALV * .5 * FLOAT(IAV1 - IAV2)/FLOAT(IAV1 + IAV2)
C
   40 NIV = NIV + 2
      IF( FL18 .AND. FL22 ) GO TO 50
      IF((IFLAG .EQ. 1).AND.DSPDTL(9).AND.(.NOT.DSPDTL(10))) GO TO 90
C                        DRIFT TIME
   50 NDTV = HDATA(NIV)
      RDTV = FLOAT(NDTV)
      COSPH = COS(FI)
      SINPH = SIN(FI)
      XXX = ABS(R)*COSPH
      YYY = ABS(R)*SINPH
C
      IF( HEAD(18) .GT. 100) GO TO 60
C                            MONTE CARLO
         FIDR = RDTV*DRVELO(NCEV+1)
         EX = -SINPH*COSLOR - COSPH*SINLOR
         EY =  COSPH*COSLOR - SINPH*SINLOR
         IF( IM .NE. 1 ) GO TO 55
C
            IF( IDATA(IPTRK + 18) .EQ. 1 ) GO TO 51
C
               ALF = ADATA(IPTRK + 19)
               CRV = ADATA(IPTRK + 22)*2.0
               IF( ABS(CRV) .LT. 1.0E-8 ) CRV = SIGN(1.0E-8,CRV)
               XC  = -SIN(ALF)/CRV + ADATA(IPTRK + 20)
               YC  =  COS(ALF)/CRV + ADATA(IPTRK + 21)
               CHG = -SIGN(1.0,ADATA(IPTRK + 22))
               GO TO 52
   51       CONTINUE
C
               ALF = ADATA(IPTRK + 21)
               CRV = ADATA(IPTRK + 19)
               IF( ABS(CRV) .LT. 1.0E-8 ) CRV = SIGN(1.0E-8,CRV)
               RAD = 1.0/CRV + ADATA(IPTRK + 20)
               XC  = COS(ALF)*RAD
               YC  = SIN(ALF)*RAD
               CHG = SIGN(1.0,ADATA(IPTRK + 25))
C
   52       CONTINUE
C
            RDF = DISTPW*0.5*COSLOR
            RF  = DISTW1 + 3.0*DISTPW
            XF  = RF*COSPH - XC
            YF  = RF*SINPH - YC
            RR  = SQRT(XF**2 + YF**2)*CHG
            CSB = XF/RR
            SNB = YF/RR
C
            IF( FIDR .LT. RDF ) GO TO 53
C
               XDR = (FIDR - RDF)*EX + RDF*CSB
               YDR = (FIDR - RDF)*EY + RDF*SNB
               GO TO 54
C
   53       CONTINUE
C
               XDR = FIDR*CSB
               YDR = FIDR*SNB
C
   54       CONTINUE
C
            X1 = XXX - XDR
            Y1 = YYY - YDR
            X2 = XXX + XDR
            Y2 = YYY + YDR
            GO TO 70
C
   55    AAA = FIDR*EX
         BBB = FIDR*EY
         X1  = XXX - AAA
         Y1  = YYY - BBB
         X2  = XXX + AAA
         Y2  = YYY + BBB
         GO TO 70
C                            REAL
   60 CONTINUE
      ADNSK2 = 0.0
      IF( NNVTXC .NE. 9 ) GOTO 199
         IPED   = MOD(IAV1,256)
         ICA1   =  IAV1/256 - IPED
         ICA2   =  MOD(IAV2,256) - IPED
         ADNSK2 = CRVT9(ICA1,ICA2)
  199 CONTINUE
      FIDR1 = (RDTV - T0)*VD*CVD(1,NIWV) - S0R(1,NIWV)
      CRR = 0.0
C
      DRIFT = FIDR1
      ISD = 1
      IF (DRIFT.GT.0.0) GOTO 201
         ISD = 2
         DRIFT = -DRIFT
  201 CONTINUE
C
      IDL = IFIX(DRIFT+0.5)
      IDH = IDL + 1
      IF (IDL.GE.1)  GOTO 202
      CRR = VIHCRR(NWEV,ISD,1)
      GOTO 205
C
  202 CONTINUE
      IF (IDH.LE.25)  GOTO 203
      CRR = VIHCRR(NWEV,ISD,25)
      GOTO 205
C
  203 CONTINUE
      WL = FLOAT(IDH) - 0.5 - DRIFT
      WH = DRIFT - FLOAT(IDL) + 0.5
      CRR = WL*VIHCRR(NWEV,ISD,IDL) + WH*VIHCRR(NWEV,ISD,IDH)
C
  205 CONTINUE
C
      FIDR1 = FIDR1 - CRR - ADNSK2
C
      FIDR2 = (RDTV - T0)*VD*CVD(2,NIWV) - S0R(2,NIWV)
      CRR = 0.
C
      DRIFT = FIDR2
      ISD = 2
      IF (DRIFT.GT.0.0) GOTO 211
         ISD = 1
         DRIFT = -DRIFT
  211 CONTINUE
C
      IDL = IFIX(DRIFT+0.5)
      IDH = IDL + 1
      IF (IDL.GE.1)  GOTO 212
      CRR = VIHCRR(NWEV,ISD,1)
      GOTO 215
C
  212 CONTINUE
      IF (IDH.LE.25)  GOTO 213
      CRR = VIHCRR(NWEV,ISD,25)
      GOTO 215
C
  213 CONTINUE
      WL = FLOAT(IDH) - 0.5 - DRIFT
      WH = DRIFT - FLOAT(IDL) + 0.5
      CRR = WL*VIHCRR(NWEV,ISD,IDL) + WH*VIHCRR(NWEV,ISD,IDH)
C
  215 CONTINUE
C
      FIDR2 = FIDR2 - CRR - ADNSK2
C
      ZH1 = 0.0
      ZH2 = 0.0
C
      XH1   = ABS(R) + FIDR1*SNLOR
      YH1   = -FIDR1*CSLOR
      XH2   = ABS(R) - FIDR2*SNLOR
      YH2   =  FIDR2*CSLOR
C
      IF( IM .NE. 1 ) GO TO 61
         P1   = ADATA(IPTRK+30)
         P2   = ADATA(IPTRK+31)
C
         RH1  = SQRT(XH1**2 + YH1**2)
         ZH1  = P1*RH1 + P2
         RH2  = SQRT(XH2**2 + YH2**2)
         ZH2  = P1*RH2 + P2
   61 CONTINUE
C
      VDXX1  = VDX + VZX*ZH1
      VDYY1  = VDY + VZY*ZH1
      VZROT1 = VFREE(50)*ZH1
      VDXX2  = VDX + VZX*ZH2
      VDYY2  = VDY + VZY*ZH2
      VZROT2 = VFREE(50)*ZH2
C
      SNNEW1 = SINPH + COSPH*VZROT1
      CSNEW1 = COSPH - SINPH*VZROT1
      SNNEW2 = SINPH + COSPH*VZROT2
      CSNEW2 = COSPH - SINPH*VZROT2
C
      X1 = XH1*CSNEW1 - YH1*SNNEW1 + VDXX1
      Y1 = YH1*CSNEW1 + XH1*SNNEW1 + VDYY1
      X2 = XH2*CSNEW2 - YH2*SNNEW2 + VDXX2
      Y2 = YH2*CSNEW2 + XH2*SNNEW2 + VDYY2
C
   70 R1  = SQRT(X1*X1 + Y1*Y1)
      FI1 = ATAN2(Y1,X1)
      R2  = SQRT(X2*X2 + Y2*Y2)
      FI2 = ATAN2(Y2,X2)
      IF( FI1 .LT. 0.0 ) FI1 = FI1 + TWOPI
      IF( FI2 .LT. 0.0 ) FI2 = FI2 + TWOPI
      IF( DSPDTL(9) ) R1 = R1*WRAP(FI1)
      IF( DSPDTL(9) ) R2 = R2*WRAP(FI2)
      IF( .NOT. FL22 .OR. .NOT. FL18 ) GO TO 80
         IF( -X1 .LT. XMINR .OR. -X1 .GT. XMAXR ) IPRJC = 1
         IF( -X2 .LT. XMINR .OR. -X2 .GT. XMAXR ) IPRJC = 1
         IF(  Y1 .LT. YMINR .OR.  Y1 .GT. YMAXR ) IPRJC = 1
         IF(  Y2 .LT. YMINR .OR.  Y2 .GT. YMAXR ) IPRJC = 1
   80 CONTINUE
   90 NIV = NIV + 1
      RETURN
  100 NWEV = 0
C                       ENTER HERE FOR MISSING EVEN WIRE HITS
      NIV = NIV + 4
      RETURN
      END
