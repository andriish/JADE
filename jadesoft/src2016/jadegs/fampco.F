C   14/03/84 403202301  MEMBER NAME  FAMPCO   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE FAMPCO( IPLT, X1, Y1, Z1, X2, Y2, Z2, RR )
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY  14/03/84 :  EXTRACT COORDINATES FROM FAMP BANK
C
C
C        THIS ROUTINE EXTRACTS COORDINATES FROM THE FAMP BANK 'BK17'
C        WHICH USES CYLINDRICAL POLAR VERSION OF 'PATR' FORMAT
C
C        INPUT:  IPLT        POINTER TO HDATA WHERE TRACK STARTS
C        OUTPUT: X1,Y1,Z1    COORDINATES OF FIRST POINT OF TRACK (MM)
C                X2,Y2,Z2         "      "  LAST    "   "    "   (MM)
C                RR          RADIUS OF CURVATURE                 (MM)
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
C
      COMMON / CJTRIG / PI,TWOPI
C
      DIMENSION HRAD(2)
      EQUIVALENCE (IRAD,HRAD(1))
C
C------------------  C O D E  ------------------------------------------
C
      IFTR    = HDATA(IPLT)
      IF( IFTR .LT. 0 ) RETURN
C
C                            HALF WORDS 1,2,3 ARE R1,PHI1,Z1
C                            1ST POINT ON TRACK, UNITS: 1/10 (MM / DEG)
C
      RAD1    = HDATA(IPLT + 1) * 0.1
      PHI1D   = HDATA(IPLT + 2) * 0.1
      Z1      = HDATA(IPLT + 3) * 0.1
C
C                            HALF WORDS 4,5,6 ARE R2,PHI2,Z2
C                            LAST POINT ON TRACK, UNITS: 1/10 (MM / DEG)
C
      RAD2    = HDATA(IPLT + 4) * 0.1
      PHI2D   = HDATA(IPLT + 5) * 0.1
      Z2      = HDATA(IPLT + 6) * 0.1
C
C                            HALF WORDS 8,9   ARE R RADIUS OF TRACK
C                            (32 BIT WORD IN 2 PARTS, UNITS = 1/10 MM )
C
      HRAD(1) = HDATA(IPLT + 8)
      HRAD(2) = HDATA(IPLT + 9)
      RR      = FLOAT( IRAD )  * 0.1
C
C                            CONVERT PHI TO RADIANS
C
      PHI1R   = PHI1D * PI / 180.0
      PHI2R   = PHI2D * PI / 180.0
C
C                            REVERSE SIGN OF CURVATURE, TO CONFORM TO
C                            JADE STANDARD
C
      RR      = - RR
C
      X1      = RAD1 * COS( PHI1R )
      X2      = RAD2 * COS( PHI2R )
C
      Y1      = RAD1 * SIN( PHI1R )
      Y2      = RAD2 * SIN( PHI2R )
C
C     Z1      = SEE ABOVE
C     Z2      = SEE ABOVE
C
      RETURN
      END
