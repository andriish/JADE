C   07/11/79 006251731  MEMBER NAME  VTXSRC   (JADEGS)      FORTRAN
      SUBROUTINE VTXSRC
C*800623*DITTMANN********************************************
C*                                                          *
C*        X - Y - Z    V E R T E X    S E A R C H           *
C*                                                          *
C*       1. SEARCH FOR E+E- PAIRS                           *
C*       2. SEARCH FOR MAIN VERTEX AROUND AVERAGE BEAM      *
C*       3. SEARCH FOR SECONDARY VERTICES                   *
C*                                                          *
C*       A DESCRIPTION OF THE T AND V ARRAYS CAN BE FOUND   *
C*       IN SUBR. VERTEX                                    *
C************************************************************
      COMMON /CWORK1/ NT,T(1500),NV,V(200),XXXXX(151),
     +                JTGOD(50),JTBAD(50),VSAVE(10),V2(20,7)
      DIMENSION IT(2),IV(2),IV2(20,7)
      EQUIVALENCE (T(1),IT(1)),(V(1),IV(1)),(V2(1,1),IV2(1,1))
C        CONSTANTS
      COMMON /CVTXC/ XB,YB,ZB,RTANK,DTANK,X0INN,SIGX0,SIGZ0,PNTMIN,
     +               DISTB,COLL2,MITER,DSCONV,PRCUT,IREJTR,EEDPMN,
     +               EEDPMX,EEDTMX,EEDRMX,SEMAX,SIMAX,SIGFAC,EEXYMN,
     +               EEXYMX,PHEMAX,SIG1,SIG2,SIG3,CSECV
C
      LOGICAL VREP,RESTOR
C
      NV = 0
      IF(NT.LE.0) RETURN
      IPRVTX = 0
C****
C****    SEARCH FOR GAMMA CONVERSIONS
      CALL VTXEE
      IF(NV.EQ.0) GOTO 5
      LV = 0
      DO 4 I=1,NV
      J = 0
      DO 3 K=1,NT
      IF(IT(J+14).EQ.I) IT(J+1)=-IT(J+1)
    3 J = J + 30
    4 LV = LV + 10
C****
C****    SEARCH FOR PRIMARY VERTEX AROUND AVERAGE BEAM
    5 NTGOD = 0
      NTBAD = 0
      J = 0
      DO 7 I=1,NT
      IF(IT(J+1).LE.0) GOTO 7
      CALL VTXPNT(J,XB,YB,XT,YT,ZT,DXT2,DYT2,DZT2,PHI,SS)
      DXY = SQRT((XT-XB)**2+(YT-YB)**2)
      IF(DXY.GT.DISTB) GOTO 6
      IF(SS.LT.SEMAX-RTANK) GOTO 6
      NTGOD = NTGOD + 1
      JTGOD(NTGOD) = J
      GOTO 7
    6 IT(J+1) = 1
      NTBAD = NTBAD + 1
      JTBAD(NTBAD) = J
    7 J = J + 30
      IF(NTGOD.EQ.0) GOTO 39
      IF(NV.EQ.20) GOTO 90
      NV = NV + 1
      LV = (NV-1)*10
      VREP = .FALSE.
      RESTOR = .FALSE.
C****
C****    VERTEX FIT
   10 CALL VERTEX
      IF(IV(LV+1).GE.1) GOTO 20
      IF(VREP) GOTO 60
      GOTO 40
C****
C****    CHECK VERTEX AND ITS TRACKS
   20 NTBAD = 0
      NTBAD3 = 0
      NTGOD = 0
      NTGOD3 = 0
      J = 0
      DO 25 I=1,NT
      IF(IT(J+1).LE.0) GOTO 25
C        SMALLEST DISTANCE TO VERTEX
      CALL VTXPNT(J,V(LV+2),V(LV+3),XT,YT,ZT,DXT2,DYT2,DZT2,PHI,SS)
      DXT2 = DXT2 + V(LV+5)**2
      DYT2 = DYT2 + V(LV+6)**2
      DZT2 = DZT2 + V(LV+7)**2
      SDX = ABS(XT-V(LV+2)) / SQRT(DXT2)
      SDY = ABS(YT-V(LV+3)) / SQRT(DYT2)
      SDZ = ABS(ZT-V(LV+4)) / SQRT(DZT2)
      T(J+15) = SS
      T(J+28) = SDX
      T(J+29) = SDY
      T(J+30) = SDZ
      IF(IT(J+1).EQ.1) GOTO 23
      IF(SDX.GT.SIG2 .OR. SDY.GT.SIG2 .OR. SDZ.GT.SIG2) GOTO 23
C        EXTRAPOLATION LENGTH
      IF(SS.GT.SIMAX) GOTO 23
      SSJC = SS
      IF(T(J+27).EQ.0.) GOTO 22
      IF(SS.LT.T(J+26)-SIMAX) GOTO 23
      IF(SS.LT.T(J+27)) SSJC=T(J+27)
   22 IF(SSJC.LT.SEMAX) GOTO 23
      IF(ABS(SS/T(J+2)).GT.PHEMAX) GOTO 23
      NTGOD = NTGOD + 1
      JTGOD(NTGOD) = J
      IF(IT(J+1).EQ.3) NTGOD3=NTGOD3+1
      GOTO 25
   23 NTBAD = NTBAD + 1
      JTBAD(NTBAD) = J
      IF(IT(J+1).EQ.3) NTBAD3=NTBAD3+1
   25 J = J + 30
      IF(RESTOR) GOTO 30
      IF(NTGOD3.EQ.7) GOTO 30
      IF(NTBAD3.EQ.0 .AND. NTGOD.EQ.NTGOD3) GOTO 30
      IF(.NOT.VREP .AND. NTGOD.LT.3) GOTO 40
      IF(NTGOD.LE.2) GOTO 60
C        REPEAT VERTEX FIT
      DO 27 I=1,NTGOD
      J = JTGOD(I)
   27 IT(J+1) = 2
      IF(NTBAD.EQ.0) GOTO 10
      DO 28 I=1,NTBAD
      J = JTBAD(I)
   28 IT(J+1) = 1
      GOTO 10
C****
C****    GOOD VERTEX FOUND
   30 IF(NTBAD.EQ.0) GOTO 37
      IF(IV(LV+8).EQ.1) GOTO 37
C        COLLECT ALL TRACKS FITTING TO VERTEX
      NTB = 0
      DO 33 I=1,NTBAD
      J = JTBAD(I)
      IF(T(J+28).GT.SIG3.OR.T(J+29).GT.SIG3.OR.T(J+30).GT.SIG3) GOTO 32
      IF(T(J+15).GT.SIMAX) GOTO 32
      SSJC = T(J+15)
      IF(T(J+27).EQ.0.) GOTO 31
      IF(T(J+15).LT.T(J+26)-SIMAX) GOTO 32
      IF(T(J+15).LT.T(J+27)) SSJC=T(J+27)
   31 IF(SSJC.LT.SEMAX) GOTO 32
      IF(ABS(T(J+15)/T(J+2)).GT.PHEMAX) GOTO 32
      NTGOD = NTGOD + 1
      JTGOD(NTGOD) = J
      IT(J+1) = 1
      GOTO 33
   32 NTB = NTB + 1
      JTBAD(NTB) = J
   33 CONTINUE
      NTBAD = NTB
   37 IV(LV+10) = NTGOD
      DO 38 I=1,NTGOD
      J = JTGOD(I)
      IT(J+1) = -IT(J+1)
      IT(J+14) = NV
   38 CONTINUE
      IF(IPRVTX.NE.0) GOTO 39
      IF(ABS(V(LV+2)-XB).LT.3.*DISTB .AND. ABS(V(LV+3)-YB).LT.3.*DISTB)
     +   IPRVTX=NV
   39 IF(NTBAD.EQ.0) GOTO 90
      NTGOD = 0
      IF(NV.EQ.20) GOTO 90
      NV = NV + 1
      LV = (NV-1)*10
C****
C****    VERTEX SEARCH
   40 IF(NTBAD.EQ.0) GOTO 241
      DO 41 I=1,NTBAD
      J = JTBAD(I)
   41 IT(J+1) = 1
  241 IF(NTGOD.EQ.0) GOTO 43
      DO 141 I=1,NTGOD
      J = JTGOD(I)
      NTBAD = NTBAD + 1
      JTBAD(NTBAD) = J
  141 IT(J+1) = 1
   43 NVL = NV
      NV2 = 0
      N1 = 1
      IF(NTBAD.LT.2) GOTO 50
C        TWO TRACK VERTEX
   42 J1 = JTBAD(N1)
      IT(J1+1) = 2
      N21 = N1 + 1
      IF(N21.GT.NTBAD) GOTO 45
      DO 44 N2=N21,NTBAD
      J2 = JTBAD(N2)
      IT(J2+1) = 2
      CALL VERTEX
      IF(IV(LV+1).LE.1) GOTO 44
C        CHECK THIS VERTEX
C        EXTRAPOLATION LENGTH TRACK 1
      CALL VTXPNT(J1,V(LV+2),V(LV+3),XT1,YT1,ZT1,DXT12,DYT12,DZT12,
     +            PHIT1,ST1)
      IF(ST1.GT.SIMAX) GOTO 44
      SSJC = ST1
      IF(T(J1+27).EQ.0.) GOTO 143
      IF(ST1.LT.T(J1+26)-SIMAX) GOTO 44
      IF(ST1.LT.T(J1+27)) SSJC=T(J1+27)
  143 IF(SSJC.LT.SEMAX) GOTO 44
      IF(ABS(ST1/T(J1+2)).GT.PHEMAX) GOTO 44
C        EXTRAPOLATION LENGTH TRACK 2
      CALL VTXPNT(J2,V(LV+2),V(LV+3),XT2,YT2,ZT2,DXT22,DYT22,DZT22,
     +            PHIT2,ST2)
      IF(ST2.GT.SIMAX) GOTO 44
      SSJC = ST2
      IF(T(J2+27).EQ.0.) GOTO 243
      IF(ST2.LT.T(J2+26)-SIMAX) GOTO 44
      IF(ST2.LT.T(J2+27)) SSJC=T(J2+27)
  243 IF(SSJC.LT.SEMAX) GOTO 44
      IF(ABS(ST2/T(J2+2)).GT.PHEMAX) GOTO 44
C        COMPARE TRACK DIRECTIONS WITH BEAM
      DZ = ABS(ZT1-ZT2)
      COSW12 = 0.
      IF(IPRVTX.EQ.0) GOTO 147
      LPV = (IPRVTX-1)*10
      DVX = V(LV+2) - V(LPV+2)
      DVY = V(LV+3) - V(LPV+3)
      DVZ = V(LV+4) - V(LPV+4)
      DV12 = SQRT(DVX**2+DVY**2+DVZ**2)
      COSW1 = (COS(PHIT1)*T(J1+24)*DVX + SIN(PHIT1)*T(J1+24)*DVY +
     +         SIN(T(J1+4))*DVZ) / DV12
      IF(COSW1.LT.CSECV) GOTO 44
      COSW2 = (COS(PHIT2)*T(J2+24)*DVX + SIN(PHIT2)*T(J2+24)*DVY +
     +        SIN(T(J2+4))*DVZ) / DV12
      IF(COSW2.LT.CSECV) GOTO 44
      COSW12 = COSW1 * COSW2
      GOTO 47
  147 DVX = V(LV+2) - XB
      DVY = V(LV+3) - YB
      DV12 = SQRT(DVX**2+DVY**2)
      IF(DV12.LT.DISTB) GOTO 47
      COSW1 = (COS(PHIT1)*DVX + SIN(PHIT1)*DVY) / DV12
      IF(COSW1.LT.CSECV) GOTO 44
      COSW2 = (COS(PHIT2)*DVX + SIN(PHIT2)*DVY) / DV12
      IF(COSW2.LT.CSECV) GOTO 44
      COSW12 = COSW1 * COSW2
C        CHECK IF MORE TRACKS FIT
   47 NT2 = 2
      DO 149 K=1,NTBAD
      J = JTBAD(K)
      IF(IT(J+1).EQ.3) GOTO 149
      CALL VTXPNT(J,V(LV+2),V(LV+3),XT,YT,ZT,DXT2,DYT2,DZT2,PHI,SS)
      SDX = ABS(XT-V(LV+2)) / SQRT(DXT2)
      SDY = ABS(YT-V(LV+3)) / SQRT(DYT2)
      SDZ = ABS(ZT-V(LV+4)) / SQRT(DZT2)
      IF(SDX.GT.SIG2 .OR. SDY.GT.SIG2 .OR. SDZ.GT.SIG2) GOTO 149
      IF(SS.GT.SIMAX) GOTO 149
      SSJC = SS
      IF(T(J+27).EQ.0.) GOTO 148
      IF(SS.LT.T(J+26)-SIMAX) GOTO 149
      IF(SS.LT.T(J+27)) SSJC=T(J+27)
  148 IF(SSJC.LT.SEMAX) GOTO 149
      IF(ABS(SS/T(J+2)).GT.PHEMAX) GOTO 149
      NT2 = NT2 + 1
  149 CONTINUE
C        TEMPORARY STORE OF ALL TWO TRACK VERTICES
      NV2 = NV2 + 1
      IV2(NV2,1) = NV
      IV2(NV2,2) = NT2
      IV2(NV2,3) = J1
      IV2(NV2,4) = J2
      V2(NV2,5) = COSW12
      V2(NV2,6) = DZ
      IV2(NV2,7) = IV(LV+1)
      IF(NV.EQ.20) GOTO 45
      NV = NV + 1
      LV = (NV-1)*10
C
   44 IT(J2+1) = 1
      IT(J1+1) = 1
      N1 = N1 + 1
      IF(N1.LE.NTBAD) GOTO 42
C        CHOOSE BEST TWO TRACK VERTEX
   45 IT(J1+1) = 1
      IT(J2+1) = 1
      IF(NV2.EQ.0) GOTO 50
      NT2MAX = 0
      DO 46 I=1,NV2
      IF(IV2(I,2).LT.NT2MAX) GOTO 46
      IF(IV2(I,2).EQ.NT2MAX) GOTO 144
      NT2MAX = IV2(I,2)
      LV2 = I
      GOTO 46
  144 IF(IV2(I,7).EQ.IV2(LV2,7)) GOTO 145
      IF(IV2(I,7).GT.IV2(LV2,7)) LV2=I
      GOTO 46
  145 IF(IPRVTX.EQ.0) GOTO 146
      IF(V2(I,5).GT.V2(LV2,5)) LV2=I
      GOTO 46
  146 IF(V2(I,6).LT.V2(LV2,6)) LV2=I
   46 CONTINUE
C        REPEAT VERTEX FIT
      J1 = IV2(LV2,3)
      J2 = IV2(LV2,4)
      NV = NVL
      LV = (NVL-1)*10
      LV2 = (IV2(LV2,1)-1)*10
      J1SAVE = J1
      J2SAVE = J2
      DO 48 K=1,10
      VSAVE(K) = V(LV2+K)
   48 V(LV+K) = V(LV2+K)
      DO 49 K=1,NTBAD
      J = JTBAD(K)
      IT(J+1)=2
   49 CONTINUE
      IT(J1+1) = 3
      IT(J2+1) = 3
      VREP = .TRUE.
      RESTOR = .FALSE.
      GOTO 20
C        SINGLE TRACK VERTEX
   50 DO 52 N1=1,NTBAD
      J1 = JTBAD(N1)
      IF(N1.EQ.1) GOTO 51
      IF(NV.EQ.20) GOTO 90
      NV = NV + 1
      LV = (NV-1)*10
   51 IV(LV+1) = 5
      V(LV+2) = T(J1+5)
      V(LV+3) = T(J1+6)
      V(LV+4) = T(J1+7)
      V(LV+5) = T(J1+10)
      V(LV+6) = T(J1+11)
      V(LV+7) = T(J1+12)
      IV(LV+8) = 1
      V(LV+9) = 0.
      IV(LV+10) = 1
      IT(J1+1) = -3
      IT(J1+14) = NV
   52 CONTINUE
      GOTO 90
C        SEARCH NOT CONVERGING
C        EMERGENCY ACTION: RESTORE 2-TRACK VERTEX (OTHERWISE DEAD LOOP)
   60 J1 = J1SAVE
      J2 = J2SAVE
      DO 62 I=1,10
   62 V(LV+I) = VSAVE(I)
      IF(NTBAD.EQ.0) GOTO 66
      DO 63 I=1,NTBAD
      J = JTBAD(I)
      IT(J+1)=2
   63 CONTINUE
   66 IF(NTGOD.EQ.0) GOTO 69
      DO 67 I=1,NTGOD
      J = JTGOD(I)
      IT(J+1)=2
   67 CONTINUE
   69 IT(J1+1) = 3
      IT(J2+1) = 3
      RESTOR = .TRUE.
      GOTO 20
C****
C****    CLEAN UP
   90 IF(NV.EQ.0) GOTO 100
      J = 0
      DO 91 I=1,NT
      IF(IT(J+1).LT.0) IT(J+1)=-IT(J+1)
   91 J = J + 30
      IF(NV.EQ.1) GOTO 100
      IF(IPRVTX.NE.0) GOTO 93
      IPRVTX = 1
      DXYP = 1.E20
      LV = 0
      DO 92 I=1,NV
      DXY = V(LV+2)**2+V(LV+3)**2
      IF(DXY.GT.DXYP) GOTO 92
      IPRVTX = I
      DXYP = DXY
   92 LV = LV + 10
   93 IF(IPRVTX.EQ.1) GOTO 100
C        MOVE PRIMARY VERTEX TO FIRST POSITION
      M = IPRVTX - 1
      LV = M*10
      DO 94 I=1,10
   94 VSAVE(I) = V(LV+I)
      DO 96 I=1,M
      DO 95 J=1,10
   95 V(LV+J) = V(LV+J-10)
   96 LV = LV - 10
      DO 97 I=1,10
   97 V(I) = VSAVE(I)
      J = 0
      DO 99 I=1,NT
      IF(IT(J+1).EQ.0) GOTO 99
      IF(IT(J+14).EQ.IPRVTX) GOTO 98
      IF(IT(J+14).LT.IPRVTX) IT(J+14)=IT(J+14)+1
      GOTO 99
   98 IT(J+14) = 1
   99 J = J + 30
C
  100 RETURN
      END
