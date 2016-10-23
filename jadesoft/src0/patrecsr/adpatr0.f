C   26/06/87 707201014  MEMBER NAME  ADPATR0  (S)           FORTRAN77
      SUBROUTINE ADPATR( NRUN )
C-----------------------------------------------------------
C   VERSION OF 26/06/87     LAST MOD 26/06/87   E ELSEN
C   ADJUST RUN DEPENDENT PATREC LIMITS
C-----------------------------------------------------------
#include "cpatlm.for"
C
      IF( NRUN .GE. 24200 ) THEN
        FLINLM(2) = 1.0
        GFP(3) = 2.0
        ZFITLM(1) = 70.
        ZFITLM(2) = 40.
      ELSE
        FLINLM(2) = 3.0
        GFP(3) = 4.5
        ZFITLM(1) = 50.
        ZFITLM(2) = 20.
      ENDIF
      END
