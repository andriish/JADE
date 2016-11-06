      SUBROUTINE VTXEE 
C*800604*DITTMANN********************************************
C*  COPIED FROM F22KLE.VERTEX.S(VTXEE)   18.9.86
C*                                                          *
C*              X - Y - Z    E E   P A I R S                *
C*                                                          *
C*       A DESCRIPTION OF THE T AND V ARRAYS CAN BE FOUND   *
C*       IN SUBR. VERTEX                                    *
C*860612*KLEINWORT*******************************************
C*                                                          *
C*       MODIFIED TO BE USED WITH VTXC                      *
C*                                                          *
C************************************************************
      IMPLICIT INTEGER*2(H)
C PMF 03.11.98 
      LOGICAL TBIT
C
C%MACRO MVERTEX1
C     MACRO FOR VERTEX-FIT ROUTINES
      COMMON /CWORK1/ NT,T(2000),NV,V(200),A(300),B(24),NTIND(20),S(20),
     *                CHITR(20),
     *                JTGOD(50),JTBAD(50),VSAVE(10),V2(20,20)
C
      DIMENSION IT(2),IV(2)
      EQUIVALENCE (T(1),IT(1)),(V(1),IV(1))
C%MACRO MVERTEX2
C     MACRO FOR VERTEX-FIT ROUTINES ( AXIS AND STATISTICS )
      COMMON /CVTX2/ MODE,TAXIS(12),SVR,HVTXST(120)
C
      DIMENSION IVTXST(1)
C
C        CONSTANTS
      COMMON /CVTXC/ XB,YB,ZB,RTANK,DTANK,X0INN,SIGX0,SIGZ0,PNTMIN,
     *               DISTB,COLL2,MITER,DSCONV,PRCUT,IREJTR,EEDPMN,
     *               EEDPMX,EEDTMX,EEDRMX,SEMAX,SIMAX,SIGFAC,EEXYMN,
     *               EEXYMX
C
      IF(NT.LE.1) GOTO 100
C****
C****    LOOP OVER ALL TRACK PAIRS
      J1 = 0
      ITEST = 0
      NT1 = NT - 1
      DO 9 I=1,NT1
      IF(IT(J1+1).EQ.0) GOTO 9
      M = I + 1
      J2 = I*40
      DO 28 K=M,NT
      IPOINT = 1
      IF(IT(J2+1).EQ.0) GOTO 8
C
      ITEST  = 1
      IT1  = I
      IT2  = K
C        OPPOSITE CHARGE
      IF(T(J1+2)*T(J2+2).GT.0.) GOTO 8
C        MEASURED R-PHI OPENING
      ITEST  = 2
      DPHI = T(J1+3) - T(J2+3)
      IF(T(J1+2).LT.0.) DPHI=-DPHI
      IF(ABS(DPHI).GT.3.141593) DPHI=DPHI-SIGN(1.,DPHI)*6.283185
      IF(DPHI.LT.EEDPMN .OR. DPHI.GT.EEDPMX) GOTO 8
      ITANK = 0
      IPOINT = 2
      ITEST  = 6
      XM1 = T(J1+5) - T(J1+2)*T(J1+21)
      YM1 = T(J1+6) + T(J1+2)*T(J1+22)
      XM2 = T(J2+5) - T(J2+2)*T(J2+21)
      YM2 = T(J2+6) + T(J2+2)*T(J2+22)
      ICFT1 = IT(J1+31)
      ICFT2 = IT(J2+31)
C        EXTRAPOLATION LENGTH
      IF(T(J1+18).EQ.0. .OR. T(J2+18).EQ.0.) GOTO 6
      ITEST  = 7
      IF(T(J1+18).LT.SEMAX .OR. T(J2+18).LT.SEMAX) GOTO 6
C        PAIR ORIGIN IN TANK (ADD SQRT(2/3)*COULOMB ERROR)
      ST1 = T(J1+18)
      ST2 = T(J2+18)
C
C   VTXS IS AN ENTRY POINT IN VTXPNT       J.O.  COMMENT
C
      CALL VTXS(J1,ST1,XT1,YT1,ZT1,DXT12,DYT12,DZT12,PHIT1,DPHIT1)
      CALL VTXS(J2,ST2,XT2,YT2,ZT2,DXT22,DYT22,DZT22,PHIT2,DPHIT2)
C        DISTANCE IN TANK
      DPHI = PHIT1 - PHIT2
      IF(T(J1+2).LT.0.) DPHI=-DPHI
      IF(ABS(DPHI).GT.3.141593) DPHI=DPHI-SIGN(1.,DPHI)*6.283185
      PTCFT2 =
     &   (FLOAT(1-ICFT1)*(T(J1+16)**2+T(J1+17)**2)
     &   +FLOAT(1-ICFT2)*(T(J2+16)**2+T(J2+17)**2)) / 1.5
      STCFT2 = 49. * PTCFT2
      PTANK2 = (T(J1+16)**2+T(J2+16)**2+T(J1+17)**2+T(J2+17)**2) / 1.5
      STANK2 = 49. * PTANK2
      DX2 = (XT1-XT2)**2
      DY2 = (YT1-YT2)**2
      DXY = SQRT(DX2+DY2)
      DIST = SQRT((XT2-XM1)**2+(YT2-YM1)**2)
      IF(DIST.LT.ABS(T(J1+2))) DXY=-DXY
      DZ = ABS(ZT1-ZT2)
      DTH = ABS(T(J1+4)-T(J2+4))
      SDTH = DTH / SQRT(T(J1+9)**2+T(J2+9)**2+PTANK2)
      ITEST  = 3
      IF(SDTH.GT.EEDTMX) GOTO 6
      CTH = COS((T(J1+4)+T(J2+4))/2.)**2
      SDZ = DZ / SQRT(DZT12+DZT22+STANK2/CTH**2)
      ITEST  = 5
      IF(SDZ.GT.EEDTMX) GOTO 5
      SDPHI = ABS(DPHI)/SQRT(DPHIT1**2+DPHIT2**2+PTCFT2)
      ITEST  = 2
      IF(SDPHI.GT.EEDRMX) GOTO 5
      SDXY = ABS(DXY) / SQRT(1./(1./DXT12+1./DYT12)+
     *                       1./(1./DXT22+1./DYT22)+STCFT2)
      ITEST  = 4
      IF(SDXY.GT.EEDRMX .AND. (DXY.LT.0..OR.DXY.GT.EEXYMN)) GOTO 5
      ITANK = 1
      GOTO 6
C
    5 CONTINUE
      IPOINT = 3
      ITEST  = 6
C        EXTRAPOLATION LENGTH
      IF(T(J1+18).EQ.0. .OR. T(J2+18).EQ.0.) GOTO 6
      ITEST  = 7
      IF(T(J1+18).LT.SEMAX .OR. T(J2+18).LT.SEMAX) GOTO 6
C        PAIR ORIGIN IN BEAMPIPE (ADD SQRT(2/3)*COULOMB ERROR)
      ST1 = T(J1+19)
      ST2 = T(J2+19)
      CALL VTXS(J1,ST1,XT1,YT1,ZT1,DXT12,DYT12,DZT12,PHIT1,DPHIT1)
      CALL VTXS(J2,ST2,XT2,YT2,ZT2,DXT22,DYT22,DZT22,PHIT2,DPHIT2)
C        DISTANCE IN BEAMPIPE
      DPHI = PHIT1 - PHIT2
      IF(T(J1+2).LT.0.) DPHI=-DPHI
      IF(ABS(DPHI).GT.3.141593) DPHI=DPHI-SIGN(1.,DPHI)*6.283185
      PTCFT2 = PTCFT2 +
     &   (FLOAT(ICFT1)*T(J1+17)**2+FLOAT(ICFT2)*T(J2+17)**2) / 1.5
      STCFT2 = DRPIPE(DUM)**2 * PTCFT2
      STANK2 = DRPIPE(DUM)**2 * PTANK2
      DX2 = (XT1-XT2)**2
      DY2 = (YT1-YT2)**2
      DXY = SQRT(DX2+DY2)
      DIST = SQRT((XT2-XM1)**2+(YT2-YM1)**2)
      IF(DIST.LT.ABS(T(J1+2))) DXY=-DXY
      DZ = ABS(ZT1-ZT2)
      CTH = COS((T(J1+4)+T(J2+4))/2.)**2
      SDZ = DZ / SQRT(DZT12+DZT22+STANK2/CTH**2)
      ITEST  = 5
      IF(SDZ.GT.EEDTMX) GOTO 6
      SDPHI = ABS(DPHI)/SQRT(DPHIT1**2+DPHIT2**2+PTCFT2)
      ITEST  = 2
      IF(SDPHI.GT.EEDRMX) GOTO 6
      SDXY = ABS(DXY) / SQRT(1./(1./DXT12+1./DYT12)+
     *                       1./(1./DXT22+1./DYT22)+STCFT2)
      ITEST  = 4
      IF(SDXY.GT.EEDRMX .AND. (DXY.LT.0..OR.DXY.GT.EEXYMN)) GOTO 6
      ITANK = 1
C
    6 CONTINUE
      IPOINT = 4
      ITEST  = 4
C        VERTEX WHERE TRACKS ARE PARALLEL
      CALL VTXPNT
     &   (J1,XM2,YM2,XT1,YT1,ZT1,DXT12,DYT12,DZT12,PHIT1,DPHIT1,ST1)
      CALL VTXPNT
     &   (J2,XM1,YM1,XT2,YT2,ZT2,DXT22,DYT22,DZT22,PHIT2,DPHIT2,ST2)
      DX2 = (XT1-XT2)**2
      DY2 = (YT1-YT2)**2
      DXY = SQRT(DX2+DY2)
      DIST = SQRT((XT2-XM1)**2+(YT2-YM1)**2)
      IF(DIST.LT.ABS(T(J1+2))) DXY=-DXY
      IF(ABS(DXY).GT.EEXYMX) GOTO 8
      DZ = ABS(ZT1-ZT2)
      IF(ITANK.EQ.1) GOTO 7
C        PAIR ORIGIN IN CHAMBER
      IPOINT = 5
      ITEST  = 7
      IF(ST1.LT.SEMAX .OR. ST2.LT.SEMAX) GOTO 8
      ITEST  = 8
      IF(ST1.GT.SIMAX .OR. ST2.GT.SIMAX) GOTO 8
      ITEST  = 9
      IF(T(J1+18).NE.0. .AND. ST1-T(J1+18).LT.0.) GOTO 8
      IF(T(J2+18).NE.0. .AND. ST2-T(J2+18).LT.0.) GOTO 8
C        DISTANCE IN CHAMBER
      DTH = ABS(T(J1+4)-T(J2+4))
      SDTH = DTH / SQRT(T(J1+9)**2+T(J2+9)**2)
      ITEST  = 3
      IF(SDTH.GT.EEDTMX) GOTO 8
      SDZ = DZ / SQRT(DZT12+DZT22)
      ITEST  = 5
      IF(SDZ.GT.EEDTMX) GOTO 8
      SDXY = ABS(DXY)/SQRT(1./(1./DXT12+1./DYT12)+1./(1./DXT22+
     *       1./DYT22))
      ITEST  = 4
      IF(SDXY.GT.EEDRMX .AND. (DXY.LT.0..OR.DXY.GT.EEXYMN)) GOTO 8
C
    7 ITEST = 0
      IF(NV.EQ.20) GOTO 14
      NV = NV + 1
      LV = (NV-1)*10
      IV(LV+1) = 4
      DT1 = SQRT(DXT12)
      DT2 = SQRT(DXT22)
      DXX2 = 1./(1./DT1+1./DT2)
      V(LV+2) = (XT1/DT1+XT2/DT2) * DXX2
      DT1 = SQRT(DYT12)
      DT2 = SQRT(DYT22)
      DYY2 = 1./(1./DT1+1./DT2)
      V(LV+3) = (YT1/DT1+YT2/DT2) * DYY2
      DT1 = SQRT(DZT12)
      DT2 = SQRT(DZT22)
      DZZ2 = 1./(1./DT1+1./DT2)
      V(LV+4) = (ZT1/DT1+ZT2/DT2) * DZZ2
      V(LV+5) = SQRT(DXX2)
      V(LV+6) = SQRT(DYY2)
      V(LV+7) = SQRT(DZZ2)
      V(LV+8) = SQRT(DX2+DY2)
      IV(LV+9) = I
      IV(LV+10) = K
    8 CONTINUE
C****
      IF (TBIT(MODE,25)) CALL VTXEER(IT1,IT2,IPOINT,ITEST)
C****L
   28 J2 = J2 + 40
    9 J1 = J1 + 40
      IF(NV.EQ.0) GOTO 100
   14 I = 1
   15 LV1 = (I-1)*10
      IF(I.EQ.NV) GOTO 19
      M = I + 1
      LV2 = I*10
      DO 16 K=M,NV
      IF ( IV(LV1+ 9).EQ.IV(LV2+ 9) .OR. IV(LV1+10).EQ.IV(LV2+ 9) .OR.
     *     IV(LV1+10).EQ.IV(LV2+10) ) GOTO 17
   16 LV2 = LV2 + 10
      GOTO 19
C        TWO PAIRS WITH SAME TRACK, TAKE PAIR WITH SMALLER DISTANCE
   17 M = I
      IF(V(LV1+8).LT.V(LV2+8)) M=K
      NV = NV - 1
      IF(M.GT.NV) GOTO 15
      DO 18 K=M,NV
      LV2 = K*10
      LV1 = LV2 - 10
      DO 18 J=1,10
   18 V(LV1+J) = V(LV2+J)
      GOTO 15
   19 J1 = (IV(LV1+9)-1)*40
      J2 = (IV(LV1+10)-1)*40
      V(LV1+9) = 0.
      IV(LV1+8) = 2
      IV(LV1+10) = 2
      IT(J1+1) = 3
      IT(J2+1) = 3
      IT(J1+14) = I
      IT(J2+14) = I
      I = I + 1
      IF(I.LE.NV) GOTO 15
C
  100 RETURN
      END
