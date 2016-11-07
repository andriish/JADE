
CDECK  ID>, CNJET.
      SUBROUTINE CNJET(YCUT,NJET,IERR)
*.-----------------------------------------------------------------------
*.
*.    CNJET: Returns number of jets for given value of ycut
*.           INPUT:   YCUT    (real) Normalised ycut value
*.           OUTPUT:  NJET (integer) Number of jets
*.                    IERR (integer) Error flag, 0=OK.
*.
*.  CREATED :  11-12-1997, STAN BENTVELSEN
*.  LAST MOD:  27-02-1998, IMeyer&SBentvelsen Fixed bug
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
      INTEGER I,NJET,IERR
      REAL    YCUT
      REAL    EPSILON
      INTEGER NPRINT
      DATA    NPRINT / 0 /
      DATA    EPSILON / 1E-7 /
      SAVE    NPRINT
      SAVE    EPSILON
      
      IERR = 0
      IF(YCUT.GE.1.) THEN
         IERR = 1
         NJET = 0
         RETURN
      ENDIF

      DO I=1,NJITER
         IF(YCUT.GT.(1.*YTRANS(I)+EPSILON)) THEN
            NJET = NTRANS(I-1)
            GOTO 889
         ENDIF
      ENDDO

      IF(NPRINT.LT.10) THEN
         NPRINT = NPRINT + 1
         WRITE(*,*) '#################################################'
         WRITE(*,*) '## CNJET: CAMBRIDGE JET FINDER CANNOT RESOLVE  ##'
         WRITE(*,'(A,I2,A)') ' ##         THIS CONFIGURATION TO A '
     +        ,NJET,' JET      ##'
         WRITE(*,*) '#################################################'
      ENDIF
      IERR = 2
      NJET = 0


 889  CONTINUE
      RETURN
      END
