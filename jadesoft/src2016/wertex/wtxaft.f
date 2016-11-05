      SUBROUTINE VTXAFT 
C*800124*DITTMANN***************************************************
C*  COPIED FROM F22KLE.VERTEX.S(VTXAFT)   18.9.86
C*                                                                 *
C*          F I N I S H   X Y Z   V E R T E X   F I T              *
C*                                                                 *
C*       CALCULATE TRACK PARAMETERS AT VERTEX-NEAREST POINT        *
C*       FOR DETAILS OF THE T,V-ARRAYS SEE COMMENT IN SUBR. VERTEX.*
C*                                                                 *
C*******************************************************************
C
C PMF 03.11.98 
      LOGICAL TBIT
C%MACRO MVERTEX1
C     MACRO FOR VERTEX-FIT ROUTINES
      COMMON /CWORK1/ NT,T(2000),NV,V(200),A(300),B(24),NTIND(20),S(20),
     +                CHITR(20),
     +                JTGOD(50),JTBAD(50),VSAVE(10),V2(20,20)
C
      DIMENSION IT(2),IV(2)
      EQUIVALENCE (T(1),IT(1)),(V(1),IV(1))
C
      IF(NV.EQ.0) RETURN
C****
C****    EXTRAPOLATE ALL TRACKS TO VERTEX
      J = 0
      DO 9 I=1,NT
      IF(IT(J+1).EQ.0) GOTO 9
      IF(IT(J+14).EQ.0) GOTO 9
      LV = (IT(J+14)-1)*10
      CALL VTXPNT(J,V(LV+2),V(LV+3),XT,YT,ZT,DXT2,DYT2,DZT2,PHI,DPHI,SS)
      T(J+3) = PHI
      T(J+5) = XT
      T(J+6) = YT
      T(J+7) = ZT
C *** CORRECT ERROR OF PHI,THETA FOR MULT. SCATTERING
      SSCOUL = SS - T(J+18)
      SSCVXC = SS - T(J+19)
      DPHMS2 = 0.
      IF ((SSCOUL.LT.0.).AND.(T(J+18).LT.0.)) DPHMS2 = T(J+16)**2
      IF (SSCVXC.LT.0.) DPHMS2 = DPHMS2 + T(J+17)**2
      T(J+8) = SQRT( DPHI**2 + DPHMS2 )
C
      SSCOUL = SS + T(J+39) - T(J+18)
      SSCOUL = SS + T(J+39) - T(J+19)
      DTHMS2 = 0.
      IF(SSCOUL.LT.0.) DTHMS2 = T(J+16)**2
      IF (SSCVXC.LT.0.) DTHMS2 = DTHMS2 + T(J+17)**2
      T(J+9) = SQRT( T(J+9)**2 + DTHMS2 )
C ***
      T(J+10) = SQRT(DXT2)
      T(J+11) = SQRT(DYT2)
      T(J+12) = SQRT(DZT2)
      T(J+15) = SS
    9 J = J + 40
C
  100 RETURN
      END
