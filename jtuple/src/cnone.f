CDECK  ID>, CNONE.
      SUBROUTINE CNONE(INON,NJNON)
*.-----------------------------------------------------------------------
*.
*.    CNONE: Returns information about Njet configurations that could not
*.           be resolved.
*.           OUTPUT: INONE(integer) Number of Njet configurations that
*.                                  could not be resolved
*.                   NJNON(*)(integer) Array of length INON containing the
*.                                  Njet for which the configuration 
*.                                  could not be resolved.
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
      INTEGER INON,NJNON(*)
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
      INON = 0
      DO I=1,NJMAX
         IF(NJOC(I).EQ.0) THEN
            INON = INON + 1
            NJNON(INON) = I
         ENDIF
      ENDDO

      RETURN
      END
