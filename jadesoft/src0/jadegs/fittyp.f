C   07/09/84 508191705  MEMBER NAME  FITTYP   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE FITTYP( IPATR, IFIT )
C-----------------------------------------------------------------------
C
C   AUTHOR:    C. BOWDERY      7/09/84 : DETERMINE FIT USED IN PATR BANK
C
C        MOD:  C. BOWDERY     27/06/85 : NEW ALGORITHM
C   LAST MOD:  C. BOWDERY     19/08/85 : IFIT TYPES WERE REVERSED.
C
C    IPATR  IS POINTER TO THE PATR BANK
C    IFIT = 0  ON RETURN IF FIT IS UNKNOWN
C         = 1  ON RETURN IF FIT IS ELSEN'S HELIX FIT
C         = 2  ON RETURN IF FIT IS PARABOLIC
C
C    METHOD:  EXAMINE THE NUMBER OF WORDS PER TRACK  (WORD 3 OF PATR)
C             48 = PARABOLA FIT (STEFFEN)
C             62 = HELIX FIT    (ELSEN)
C-----------------------------------------------------------------------
C
      IMPLICIT  INTEGER*2 (H)
C
#include "cdata.for"
C
C------------------  C O D E  ------------------------------------------
C
      LTR  = IDATA( IPATR + 3 )
C
      IFIT = 0
      IF( LTR .EQ. 62 ) IFIT = 1
      IF( LTR .EQ. 48 ) IFIT = 2
C
      RETURN
      END
