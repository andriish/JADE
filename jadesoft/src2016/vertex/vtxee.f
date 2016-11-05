C   09/01/80 006251729  MEMBER NAME  VTXEE    (JADEGS)      FORTRAN
      SUBROUTINE VTXEE
C*800604*DITTMANN********************************************
C*                                                          *
C*              X - Y - Z    E E   P A I R S                *
C*                                                          *
C*       A DESCRIPTION OF THE T AND V ARRAYS CAN BE FOUND   *
C*       IN SUBR. VERTEX                                    *
C************************************************************
      COMMON /CWORK1/ NT,T(1500),NV,V(200)
      DIMENSION IT(2),IV(2)
      EQUIVALENCE (T(1),IT(1)),(V(1),IV(1))
C        CONSTANTS
      COMMON /CVTXC/ XB,YB,ZB,RTANK,DTANK,X0INN,SIGX0,SIGZ0,PNTMIN,
     +               DISTB,COLL2,MITER,DSCONV,PRCUT,IREJTR,EEDPMN,
     +               EEDPMX,EEDTMX,EEDRMX,SEMAX,SIMAX,SIGFAC,EEXYMN,
     +               EEXYMX
C
      IF(NT.LE.1) GOTO 100
C****
C****    LOOP OVER ALL TRACK PAIRS
      J1 = 0
      NT1 = NT - 1
      DO 9 I=1,NT1
      IF(IT(J1+1).EQ.0) GOTO 9
      M = I + 1
      J2 = I*30
      DO 8 K=M,NT
      IF(IT(J2+1).EQ.0) GOTO 8
C        OPPOSITE CHARGE
      IF(T(J1+2)*T(J2+2).GT.0.) GOTO 8
C        MEASURED R-PHI OPENING
      DPHI = T(J1+3) - T(J2+3)
      IF(T(J1+2).LT.0.) DPHI=-DPHI
      IF(ABS(DPHI).GT.3.141593) DPHI=DPHI-SIGN(1.,DPHI)*6.283185
      IF(DPHI.LT.EEDPMN .OR. DPHI.GT.EEDPMX) GOTO 8
      ITANK = 0
      XM1 = T(J1+5) - T(J1+2)*T(J1+21)
      YM1 = T(J1+6) + T(J1+2)*T(J1+22)
      XM2 = T(J2+5) - T(J2+2)*T(J2+21)
      YM2 = T(J2+6) + T(J2+2)*T(J2+22)
C        EXTRAPOLATION LENGTH
      IF(T(J1+27).EQ.0. .OR. T(J2+27).EQ.0.) GOTO 6
      IF(T(J1+27).LT.SEMAX .OR. T(J2+27).LT.SEMAX) GOTO 6
C        PAIR ORIGIN IN TANK (ADD SQRT(2/3)*COULOMB ERROR)
      ST1 = T(J1+27)
      ST2 = T(J2+27)
      CALL VTXS(J1,ST1,XT1,YT1,ZT1,DXT12,DYT12,DZT12,PHIT1)
      CALL VTXS(J2,ST2,XT2,YT2,ZT2,DXT22,DYT22,DZT22,PHIT2)
C        DISTANCE IN TANK
      DPHI = PHIT1 - PHIT2
      IF(T(J1+2).LT.0.) DPHI=-DPHI
      IF(ABS(DPHI).GT.3.141593) DPHI=DPHI-SIGN(1.,DPHI)*6.283185
      PTANK2 = (T(J1+16)**2+T(J2+16)**2) / 1.5
      STANK2 = 49. * PTANK2
      DX2 = (XT1-XT2)**2
      DY2 = (YT1-YT2)**2
      DXY = SQRT(DX2+DY2)
      DIST = SQRT((XT2-XM1)**2+(YT2-YM1)**2)
      IF(DIST.LT.ABS(T(J1+2))) DXY=-DXY
      DZ = ABS(ZT1-ZT2)
      DTH = ABS(T(J1+4)-T(J2+4))
      SDTH = DTH / SQRT(T(J1+9)**2+T(J2+9)**2+PTANK2)
      IF(SDTH.GT.EEDTMX) GOTO 6
      CTH = COS((T(J1+4)+T(J2+4))/2.)**2
      SDZ = DZ / SQRT(DZT12+DZT22+STANK2/CTH**2)
      IF(SDZ.GT.EEDTMX) GOTO 6
      SDPHI = ABS(DPHI)/SQRT(T(J1+8)**2+T(J2+8)**2+PTANK2)
      IF(SDPHI.GT.EEDRMX) GOTO 5
      SDXY = ABS(DXY) / SQRT(1./(1./DXT12+1./DYT12)+
     +                       1./(1./DXT22+1./DYT22)+STANK2)
      IF(SDXY.GT.EEDRMX .AND. (DXY.LT.0..OR.DXY.GT.EEXYMN)) GOTO 5
      ITANK = 1
      GOTO 6
C        DISTANCE AT BEAM PIPE
    5 DPHIT = DTANK/ABS(T(J1+2)) + DTANK/ABS(T(J2+2))
      DPHI = DPHI - DPHIT
      SDPHI = ABS(DPHI)/SQRT(T(J1+8)**2+T(J2+8)**2+PTANK2)
      IF(SDPHI.GT.EEDRMX) GOTO 6
      DXY = DXY - DPHIT*DTANK/2.
      SDXY = ABS(DXY) / SQRT(1./(1./DXT12+1./DYT12) +
     +             1./(1./DXT22+1./DYT22) + STANK2)
      IF(SDXY.GT.EEDRMX .AND. (DXY.LT.0..OR.DXY.GT.EEXYMN)) GOTO 6
      ITANK = 1
C        VERTEX WHERE TRACKS ARE PARALLEL
    6 CALL VTXPNT(J1,XM2,YM2,XT1,YT1,ZT1,DXT12,DYT12,DZT12,PHIT1,ST1)
      CALL VTXPNT(J2,XM1,YM1,XT2,YT2,ZT2,DXT22,DYT22,DZT22,PHIT2,ST2)
      DX2 = (XT1-XT2)**2
      DY2 = (YT1-YT2)**2
      DXY = SQRT(DX2+DY2)
      IF(DXY.GT.EEXYMX) GOTO 8
      DIST = SQRT((XT2-XM1)**2+(YT2-YM1)**2)
      IF(DIST.LT.ABS(T(J1+2))) DXY=-DXY
      DZ = ABS(ZT1-ZT2)
      IF(ITANK.EQ.1) GOTO 7
C        PAIR ORIGIN IN CHAMBER
      IF(ST1.LT.SEMAX .OR. ST2.LT.SEMAX) GOTO 8
      IF(ST1.GT.SIMAX .OR. ST2.GT.SIMAX) GOTO 8
      IF(T(J1+27).NE.0. .AND. ST1-T(J1+27).LT.0.) GOTO 8
      IF(T(J2+27).NE.0. .AND. ST2-T(J2+27).LT.0.) GOTO 8
C        DISTANCE IN CHAMBER
      DTH = ABS(T(J1+4)-T(J2+4))
      SDTH = DTH / SQRT(T(J1+9)**2+T(J2+9)**2)
      IF(SDTH.GT.EEDTMX) GOTO 8
      SDZ = DZ / SQRT(DZT12+DZT22)
      IF(SDZ.GT.EEDTMX) GOTO 8
      SDXY = ABS(DXY)/SQRT(1./(1./DXT12+1./DYT12)+1./(1./DXT22+
     +       1./DYT22))
      IF(SDXY.GT.EEDRMX .AND. (DXY.LT.0..OR.DXY.GT.EEXYMN)) GOTO 8
C
    7 IF(NV.EQ.20) GOTO 14
      NV = NV + 1
      LV = (NV-1)*10
      IV(LV+1) = 4
      DT1 = SQRT(DXT12)
      DT2 = SQRT(DXT22)
      V(LV+2) = (XT1/DT1+XT2/DT2) / (1./DT1+1./DT2)
      DT1 = SQRT(DYT12)
      DT2 = SQRT(DYT22)
      V(LV+3) = (YT1/DT1+YT2/DT2) / (1./DT1+1./DT2)
      DT1 = SQRT(DZT12)
      DT2 = SQRT(DZT22)
      V(LV+4) = (ZT1/DT1+ZT2/DT2) / (1./DT1+1./DT2)
      V(LV+5) = SQRT(DXT12+DXT22) / 2.
      V(LV+6) = SQRT(DYT12+DYT22) / 2.
      V(LV+7) = SQRT(DZT12+DZT22) / 2.
      V(LV+8) = SQRT(DX2+DY2)
      IV(LV+9) = I
      IV(LV+10) = K
    8 J2 = J2 + 30
    9 J1 = J1 + 30
C****
C****    CLEAN UP
      IF(NV.EQ.0) GOTO 100
   14 I = 1
   15 LV1 = (I-1)*10
      IF(I.EQ.NV) GOTO 19
      M = I + 1
      LV2 = I*10
      DO 16 K=M,NV
      IF(IV(LV1+9).EQ.IV(LV2+9) .OR. IV(LV1+10).EQ.IV(LV2+9) .OR.
     +   IV(LV1+10).EQ.IV(LV2+10)) GOTO 17
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
   19 J1 = (IV(LV1+9)-1)*30
      J2 = (IV(LV1+10)-1)*30
      IV(LV1+8) = 2
      V(LV1+9) = 0.
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
