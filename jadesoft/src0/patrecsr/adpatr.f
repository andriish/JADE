C   26/06/87 712141919  MEMBER NAME  ADPATR   (S)           FORTRAN77
      SUBROUTINE ADPATR( NRUN )
C-----------------------------------------------------------
C   VERSION OF 26/06/87     LAST MOD 08/12/87   E ELSEN
C   ADJUST RUN DEPENDENT PATREC LIMITS
C-----------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
#include "cpatlm.for"
      COMMON /BCS/ HW(1)
      INTEGER IW(1)
      EQUIVALENCE (IW(1),HW(1))
      INTEGER IPHEAD / 0 /
C
      IF( IPHEAD .LE. 0 ) IPHEAD = IBLN('HEAD')
      NPHEAD = IW(IPHEAD)
C
      IF( NRUN .GE. 24200 .OR.
     *  ( NRUN .LT. 100 .AND. HW(NPHEAD*2+8) .GT. 1985 ) ) THEN
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
