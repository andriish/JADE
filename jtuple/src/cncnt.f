CDECK  ID>, CNCNT.
      INTEGER FUNCTION CNCNT(NJET)
*.-----------------------------------------------------------------------
*.
*.    CNCNT: (INTEGER FUNCTION) Returns the number of different Njet
*.           configurations found for given Njet.
*.           INPUT:  NJET(integer) Number of required jets
*.
*.
*.  CREATED :  08-01-1998, STAN BENTVELSEN
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
      INTEGER NJET, I

      CNCNT = 0
      DO I=1,NJITER
         IF(NTRANS(I).EQ.NJET) THEN
            CNCNT = CNCNT + 1
         ENDIF
      ENDDO
      
      RETURN
      END
