      SUBROUTINE VTXPNT(J,XP,YP,XT,YT,ZT,DXT2,DYT2,DZT2,PHIT,DPHIT,ST) 
C*800623*DITTMANN************************************************
C*  COPIED FROM F22KLE.VERTEX.S(VTXPNT)   18.9.86
C*                                                              *
C*     S U P P O R T   F O R   X Y Z   V E R T E X   F I T      *
C*                                                              *
C*860611*KLEINWORT***********************************************
C*                                                              *
C*  REWRITTEN FOR PROPER ERRORS AND USE WITH 'COMFIT'ED TRACKS  *
C*                                                              *
C*  - AS NEW PARAMETER DPHIT THE ERROR OF PHI IS CALCULATED     *
C*    ( WITHOUT MULTIPLE SCATTERING ! )                         *
C*                                                              *
C*       COMMON CWORK1 MUST BE FILLED PROPERLY                  *
C*                                                              *
C*       ENTRY: VTXS                                            *
C****************************************************************
CAV   CVTX2 common block comes with INTEGER*2 (H) elsewhere
      IMPLICIT INTEGER*2 (H)
C PMF 03.11.98 
      LOGICAL TBIT
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
C
      REAL*8 XM,YM,RR,PHI,SPHI,CPHI,SR
C        ENTRY VTXPNT
C****    CALCULATE POINT XT,YT,ZT ON HELIX J WHICH IS NEAREST
C****    TO POINT XP,YP IN X-Y PROJECTION
      RR = DBLE(T(J+2))
      XM = DBLE(T(J+5)) - RR*DBLE(T(J+21))
      YM = DBLE(T(J+6)) + RR*DBLE(T(J+22))
      SR = DBLE(SIGN(1.,T(J+2)))
      PHI = DATAN2(-SR*(XM-DBLE(XP)),SR*(YM-DBLE(YP)))
      SPHI = DSIN(PHI)
      CPHI = DCOS(PHI)
      PHIT = SNGL(PHI)
      PHI = PHI - DBLE(T(J+3))
      IF (DABS(PHI).GT.3.141593D0) PHI=PHI-DSIGN(1.D0,PHI)*6.283185D0
      ST = SNGL(RR*PHI)
      XT = SNGL(XM + RR*SPHI)
      YT = SNGL(YM - RR*CPHI)
      ZT = T(J+7) + ST*T(J+23)
      SSPACE = ST
      GOTO 100
C
C
         ENTRY VTXS(J,SJ,XT,YT,ZT,DXT2,DYT2,DZT2,PHIT,DPHIT)
C****    CALCULATE POINT XT,YT,ZT ON HELIX J FOR A GIVEN
C****    PROJECTED ARC LENGTH SJ
      RR = DBLE(T(J+2))
      PHI = DBLE(SJ) / RR + DBLE(T(J+3))
      PHIT = SNGL(PHI)
      SPHI = DSIN(PHI)
      CPHI = DCOS(PHI)
      XT = SNGL(DBLE(T(J+5)) + RR*(SPHI-DBLE(T(J+21))))
      YT = SNGL(DBLE(T(J+6)) - RR*(CPHI-DBLE(T(J+22))))
      ZT = T(J+7) + SJ*T(J+23)
      SSPACE = SJ
C
C *** CALCULATION OF RPHI-ERRORS
C
  100 CONTINUE
C
      SSCOUL = SSPACE - T(J+18)
      IF ((SSCOUL.GT.0.).OR.(T(J+18).GT.0.)) SSCOUL=0.
      SSCVXC = SSPACE - T(J+19)
      IF(SSCVXC.GT.0.) SSCVXC=0.
C
      X = SSPACE - 0.5*T(J+20)
C                                      ERROR IN XY (SQUARED)
C
C                                      MULTIPLE SCATTERRING ( TANKWALL )
      DXY = ( T(J+16) * SSCOUL )**2 +
C                                      MULTIPLE SCATTERRING ( BEAMPIPE )
     &      ( T(J+17) * SSCVXC )**2 +
C                                      PARABOLA FIT
     &      ((( T(J+32)   *X + T(J+33) ) *X +
     &          T(J+34) ) *X + T(J+35) ) *X + T(J+36)
C
C                                      ERROR IN PHI (NOT SQUARED)
C
C                                      NO MULTIPLE SCATTERRING
C                                      PARABOLA FIT
      XX = 2.0 * X
      DPHIT = ( T(J+32) *XX + T(J+33) ) *XX + T(J+40)
      IF (DPHIT.GE.0.) DPHIT = SQRT( DPHIT )
C
      DXYLIM = 1. / (2.*ABS(T(J+2))*SQRT(ABS(DXY)))
      DXT2 = SNGL(SPHI**2) / DXY
      DYT2 = SNGL(CPHI**2) / DXY
      IF(ABS(DXT2).LT.DXYLIM) DXT2=DXYLIM
      IF(ABS(DYT2).LT.DXYLIM) DYT2=DXYLIM
      DXT2 = 1. / DXT2
      DYT2 = 1. / DYT2
C
C *** CALCULATION OF ZS-ERRORS
C
      SSPACE = SSPACE + T(J+39)
C
      SSCOUL = SSPACE - T(J+18)
      IF(SSCOUL.GT.0.) SSCOUL=0.
      SSCVXC = SSPACE - T(J+19)
      IF(SSCVXC.GT.0.) SSCVXC=0.
C
      X = SSPACE/T(J+38) - 0.5
C                                      MULTIPLE SCATTERRING
      DZT2 = ( T(J+16) * SSCOUL )**2 + ( T(J+17) * SSCVXC )**2 +
C                                      LINE FIT (WITH TRIPLETS)
     &         T(J+37) * ( 1.5 + 18.*X**2 )
C
      RETURN
C
      END
