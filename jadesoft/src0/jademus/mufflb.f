C   01/06/83 306011450  MEMBER NAME  MUFFLB   (JADEMUS1)    FORTRAN
C
C  NEW S/R   13.45  06/05/83  CHRIS BOWDERY
C
C-----------------------------------------------------------------------
      SUBROUTINE MUFFLB
C-----------------------------------------------------------------------
C
C             PROCEDURE FOR BACK-TRACKING AND SETTING-UP 1ST PART
C             FOR MUFFLE STEPS 2 AND 3.
C
C-----------------------------------------------------------------------
C
C             COMMONS
C
#include "cmufwork.for"
#include "cmuffl.for"
C
C-----------------------------------------------------------------------
C
C             PRESERVE COORDS.
C
      CALL UCOPY(X,X0,NSPECI)
      X3 = X
      Y3 = Y
      Z3 = Z
C
C             CALCULATE DISTANCE  TRAVELLED  FROM  VERTEX  (INTERACTION
C             POINT), INCLUDING CORRECTION FOR CURVATURE.
C
      D3 = SQRT(X3**2 + Y3**2 + Z3**2)
      XYCHOR = SQRT(X3**2 + Y3**2)
      D3 = D3 + (CURV**2 * XYCHOR**3 / 24.) * COSEC
C
C             BACK-TRACK.
C
      X = X3 - DCX * D3
      Y = Y3 - DCY * D3
      Z = Z3 - DCZ * D3
C
C             MAKE A STEP TO MID-POINT OF TRACK. FOR  THIS  FIRST  STEP
C             THE QUANTITIES WHICH DETERMINE  MULTIPLE  SCATTERING  AND
C             ENERGY LOSS ARE SET TO  ZERO  BECAUSE  (A)  THE  MULTIPLE
C             SCATTERING IS DETERMINED BY THE TRACK FITTING  PARAMETERS
C             AT THE MID-POINT, AND (B) THE ENERGY IS DETERMINED BY THE
C             MOMENTUM EFFECTIVELY DETERMINED AT THE MID-POINT.
C
      DSTEP  = D3 / 2.0
      GMSTEP = (GMBP + GMJETI + GMJET1 + GMJET2) * COSEC
      ABSTEP = (ABBP + ABJETI + ABJET1 + ABJET2) * COSEC
      RDSTEP = 0.0
      DESTEP = 0.0
C
      RETURN
      END
