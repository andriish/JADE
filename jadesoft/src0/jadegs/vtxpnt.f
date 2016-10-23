C   11/12/79 006251730  MEMBER NAME  VTXPNT   (JADEGS)      FORTRAN
      SUBROUTINE VTXPNT(J,XP,YP,XT,YT,ZT,DXT2,DYT2,DZT2,PHIT,ST)
C*800623*DITTMANN************************************************
C*                                                              *
C*     S U P P O R T   F O R   X Y Z   V E R T E X   F I T      *
C*                                                              *
C*       COMMON CWORK1 MUST BE FILLED PROPERLY                  *
C*                                                              *
C*       ENTRY: VTXS                                            *
C****************************************************************
      COMMON /CWORK1/ NT,T(1500)
C
C        ENTRY VTXPNT
C****    CALCULATE POINT XT,YT,ZT ON HELIX J WHICH IS NEAREST
C****    TO POINT XP,YP IN X-Y PROJECTION
      XM = T(J+5) - T(J+2)*T(J+21)
      YM = T(J+6) + T(J+2)*T(J+22)
      SR = SIGN(1.,T(J+2))
      PHIT = ATAN2(-SR*(XM-XP),SR*(YM-YP))
      THETA = PHIT - T(J+3)
      IF(ABS(THETA).GT.3.141593) THETA=THETA-SIGN(1.,THETA)*6.283185
      ST = T(J+2)*THETA
      SPHI = SIN(PHIT)
      CPHI = COS(PHIT)
      XT = XM + T(J+2)*SPHI
      YT = YM - T(J+2)*CPHI
      ZT = T(J+7) + ST*T(J+23)
      SSPACE = ST
      SSCOUL = ST-T(J+27)
      IF(SSCOUL.GT.0.) SSCOUL=0.
      DXY = T(J+10)**2 + (T(J+8)*SSPACE)**2 + (T(J+16)*SSCOUL)**2
      DZ = (T(J+9)*SSPACE)**2 + (T(J+16)*SSCOUL)**2
      DXYLIM = 1. / (2.*ABS(T(J+2))*SQRT(DXY))
      DXT2 = SPHI**2 / DXY
      DYT2 = CPHI**2 / DXY
      IF(DXT2.LT.DXYLIM) DXT2=DXYLIM
      IF(DYT2.LT.DXYLIM) DYT2=DXYLIM
      DXT2 = 1. / DXT2
      DYT2 = 1. / DYT2
      DZT2 = T(J+12)**2 + DZ/(T(J+24)**2)**2
      RETURN
C
         ENTRY VTXS(J,S,XT,YT,ZT,DXT2,DYT2,DZT2,PHIT)
C****    CALCULATE POINT XT,YT,ZT ON HELIX J FOR A GIVEN
C****    PROJECTED ARC LENGTH
      THETA = S / T(J+2)
      PHIT = T(J+3) + THETA
      SPHI = SIN(PHIT)
      CPHI = COS(PHIT)
      XT = T(J+5) + T(J+2)*(SPHI-T(J+21))
      YT = T(J+6) - T(J+2)*(CPHI-T(J+22))
      ZT = T(J+7) + S*T(J+23)
      SSPACE = S
      SSCOUL = S-T(J+27)
      IF(SSCOUL.GT.0.) SSCOUL=0.
      DXY = T(J+10)**2 + (T(J+8)*SSPACE)**2 + (T(J+16)*SSCOUL)**2
      DZ = (T(J+9)*SSPACE)**2 + (T(J+16)*SSCOUL)**2
      DXYLIM = 1. / (2.*ABS(T(J+2))*SQRT(DXY))
      DXT2 = SPHI**2 / DXY
      DYT2 = CPHI**2 / DXY
      IF(DXT2.LT.DXYLIM) DXT2=DXYLIM
      IF(DYT2.LT.DXYLIM) DYT2=DXYLIM
      DXT2 = 1. / DXT2
      DYT2 = 1. / DYT2
      DZT2 = T(J+12)**2 + DZ/(T(J+24)**2)**2
      RETURN
C
      END
