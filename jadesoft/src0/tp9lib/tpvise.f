C   09/12/87 712091733  MEMBER NAME  TPVISE   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPVISE( NP, ITRNA, EVISC, EVISN, SIGEC, SIGEN )
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery       9/12/87:  Do visible E calculations
C
C
C
C     Routine to calculate charged and neutral energy and errors.
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     NP        In       I*4      No. of particles in ITRNA array
C     ITRNA     In       I*4      Array of particle (TPTR) numbers
C
C     EVISC     Out      R*4      Charged visible energy (GeV)
C     EVISN     Out      R*4      Neutral visible energy (GeV)
C     SIGEC     Out      R*4      Sigma(charged visible energy)
C     SIGEN     Out      R*4      Sigma(neutral visible energy)
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

#include "bosdata.for"

      INTEGER  NP
      INTEGER  ITRNA(200)

C------------------  C O D E  ------------------------------------------

C                            Clear running charged and neutral energies
C                            and corresponding variances.

      EVISC  = 0.0
      EVISN  = 0.0
      VEVISC = 0.0
      VEVISN = 0.0

C                            Loop over all primary tracks using ITRNA

      DO  10  I = 1,NP

        CALL CLOC( ITPTR, 'TPTR', ITRNA(I) )

        IF( ITPTR .GT. 0 ) THEN

          CHARGE = ADATA( ITPTR + 23 )
          PTOT   = ADATA( ITPTR + 24 )
          SIGP   = ADATA( ITPTR + 25 )
          ETOT   = ADATA( ITPTR + 36 )
          SIGESH = ADATA( ITPTR + 38 )

          IF( CHARGE .EQ. 0.0 ) THEN

            EVISN  = EVISN  + ETOT
            VEVISN = VEVISN + SIGESH**2

          ELSE

            EVISC  = EVISC  + ETOT
            VEVISC = VEVISC + ( PTOT * SIGP / ETOT )**2

          ENDIF

C                            Compute errors on visible energies

          SIGEC   = SQRT( VEVISC )
          SIGEN   = SQRT( VEVISN )

        ELSE

C                            TPTR bank is missing!

          CALL TPERRH

        ENDIF

  10  CONTINUE

      RETURN
      END
