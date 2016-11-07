
CDECK  ID>, CYGET.
      SUBROUTINE CYGET(NJI,YAR,NAR)
*.----------------------------------------------------------------------
*.    CYGET: Get information on all yflip values and corresponding Njet
*.           values
*.           OUTPUT:  NJI (integer) Number of found yflip values (upto
*.                                  first appearance of NJREQ jets)
*.                    YAR(*)(real)  Array containing all yflip values
*.                    NAR(*)(integer) Array containing all Njet values
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
      INTEGER NJI,NAR(*),I
      REAL    YAR(*)

      NJI = NJITER
      DO I=1,NJITER
         YAR(I) = 1.*YTRANS(I)
         NAR(I) = NTRANS(I)
      ENDDO

      RETURN
      END
