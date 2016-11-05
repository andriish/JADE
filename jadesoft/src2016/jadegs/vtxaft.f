C   05/08/79 002080919  MEMBER NAME  VTXAFT   (JADEGS)      FORTRAN
      SUBROUTINE VTXAFT
C*800124*DITTMANN***************************************************
C*                                                                 *
C*          F I N I S H   X Y Z   V E R T E X   F I T              *
C*                                                                 *
C*       CALCULATE TRACK PARAMETERS AT VERTEX-NEAREST POINT        *
C*       FOR DETAILS OF THE T,V-ARRAYS SEE COMMENT IN SUBR. VERTEX.*
C*                                                                 *
C*******************************************************************
C
      COMMON /CWORK1/ NT,T(1500),NV,V(200)
      DIMENSION IV(2),IT(2)
      EQUIVALENCE (V(1),IV(1)),(T(1),IT(1))
C
      IF(NV.EQ.0) RETURN
C****
C****    EXTRAPOLATE ALL TRACKS TO VERTEX
      J = 0
      DO 9 I=1,NT
      IF(IT(J+1).EQ.0) GOTO 9
      IF(IT(J+14).EQ.0) GOTO 9
      LV = (IT(J+14)-1)*10
      CALL VTXPNT(J,V(LV+2),V(LV+3),XT,YT,ZT,DXT2,DYT2,DZT2,PHI,SS)
      T(J+3) = PHI
      T(J+5) = XT
      T(J+6) = YT
      T(J+7) = ZT
      T(J+10) = SQRT(DXT2)
      T(J+11) = SQRT(DYT2)
      T(J+12) = SQRT(DZT2)
      T(J+15) = SS
    9 J = J + 30
C
  100 RETURN
      END
