CDECK  ID>, CWRIT.
      SUBROUTINE CWRIT(ITKDIM,NT,PT,NDBLE,IERR)
C-----------------------------------------------------------------------
C
C     ROUTINE TO WRITE 4-VECTORS OF THE EVENT IN ASCII FILE 'scam.evdata'
C     AND YFLIP VALUES OF THE EVENT IN ASCII FILE 'scam.yflip'
C     WRITE ONLY WHEN NUMBER OF MULTIPLE REGIONS FOR NJET >= NDBLE
C
C  CREATED :  11-12-1997, STAN BENTVELSEN
C  LAST MOD:
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMXY , NMXP
      PARAMETER (NMXY = 300)
      PARAMETER (NMXP = 30)
      DOUBLE PRECISION YTRANS(NMXY)
      DOUBLE PRECISION PCMJ(NMXP,4,NMXP)
      INTEGER          NTRANS(NMXY), NJITER, NJMAX, NTRACK
      INTEGER          ICMJ(NMXP,NMXY)
      COMMON / CKCOM / YTRANS, PCMJ, NTRANS, ICMJ, NJITER, NJMAX, NTRACK
      INTEGER ITKDIM, NT, NDBLE, IERR
      INTEGER I,J
      INTEGER IDUB,NJDUB(20),NJOC(20)
      REAL    PT(ITKDIM,*)
      LOGICAL FIRST
      DATA    FIRST / .TRUE. /
      SAVE    FIRST

      IF(FIRST) THEN
         FIRST = .FALSE.
         OPEN(20,FILE='scam.evdata',STATUS='UNKNOWN')
         OPEN(21,FILE='scam.ycut',STATUS='UNKNOWN')
      ENDIF

      CALL CDBLE(IDUB,NJDUB,NJOC)
      IF(IDUB.GE.NDBLE) THEN
         WRITE(20,'(I8)') NT
         DO I=1,NT
            WRITE(20,'(I8,4F12.5)') I,(PT(J,I),J=1,4)
         ENDDO
         WRITE(21,'(I8)') NJITER
         DO I=1,NJITER
            WRITE(21,'(I8,F13.10,I8)') I,YTRANS(I),NTRANS(I)
         ENDDO
      ENDIF

      RETURN
      END
