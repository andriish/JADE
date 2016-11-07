
CDECK  ID>, CDBLE.
      SUBROUTINE CDBLE(IDUB,NJDUB,NJC)
*.-----------------------------------------------------------------------
*.
*.    CDBLE: Returns information about Njet configurations found at least
*.           two times.
*.           OUTPUT:  IDUB (integer) Number of Njet configurations for 
*.                                   which at least two Njet configurations 
*.                                   have been found
*.                    NJDUB(*)(integer) Array of length IDUB containing the 
*.                                   values for Njet
*.                    NJC(*)(integer) Array of length IDUB containing the
*.                                   number of different Njet regions
*.                                   NB: Entry NJDUB(i) correspond to NJC(i) 
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
      INTEGER IDUB,NJDUB(*),NJC(*)
      INTEGER NJOC(NMXY)
      INTEGER I

      DO I=1,NMXY
         NJOC(I) = 0
      ENDDO

      DO I=1,NJITER
         IF(NTRANS(I).GT.0.AND.NTRANS(I).LT.NMXY) THEN
            NJOC(NTRANS(I)) = NJOC(NTRANS(I)) + 1
         ENDIF
      ENDDO
      IDUB = 0
      DO I=1,NJITER
         IF(NJOC(I).GE.2) THEN
            IDUB = IDUB + 1
            NJDUB(IDUB) = I
            NJC(IDUB)   = NJOC(I)
         ENDIF
      ENDDO

      RETURN
      END
