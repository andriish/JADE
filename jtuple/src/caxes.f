
CDECK  ID>, CAXES.
      SUBROUTINE CAXES(NJET,PNJ,IERR)
*.-----------------------------------------------------------------------
*.
*.    CAXES: Returns the jet four-momenta for a required n-jet configuration
*.           (corresponding to largest ycut values)
*.           INPUT:   NJET (integer) Number of jets required
*.           OUTPUT:  PNJ(4,*)(real) Array of jets 4-vectors 
*.                    IERR (integer) Error flag, 0=OK.
*.           CALLS:   CASSO
*.
*.  CREATED :  11-12-1997, STAN BENTVELSEN
*.  LAST MOD:
*.-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMXY , NMXP
      PARAMETER (NMXY = 300)
      PARAMETER (NMXP = 30)
      DOUBLE PRECISION YTRANS(NMXY)
      DOUBLE PRECISION PCMJ(NMXP,4,NMXP)
      INTEGER          NTRANS(NMXY), NJITER, NJMAX, NTRACK
      INTEGER          ICMJ(NMXP,NMXY)
      COMMON / CKCOM / YTRANS, PCMJ, NTRANS, ICMJ, NJITER, NJMAX, NTRACK
      REAL PNJ(4,*)
      INTEGER ITMP(300)
      INTEGER IERR, NJET

      CALL CASSO(NJET,PNJ,ITMP,IERR)

      RETURN
      END
