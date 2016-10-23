C   07/02/87 809291231  MEMBER NAME  TPTFTP   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE TPTFTP
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery       7/02/87:  Add TOF data to TPTR banks
C
C     Last mod: C. Bowdery      29/09/88:  CALL TPSEFL now
C
C     Based on routine written by S. Yamada.
C
C     Routine to copy information from the TOFR bank into the TPTR
C     banks.
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

#include "bosdata.for"

      INTEGER  LENAR, NSTEP, TOF

      PARAMETER  ( LENAR = 100, TOF = 3 )
      PARAMETER  ( NSTEP =  14 )

      INTEGER  IPNTA( LENAR ), ITOFR, NTRK, NBNKS, ITPTRI, I, K, NT

C------------------  C O D E  ------------------------------------------

      ITOFR = IDATA( IBLN('TOFR') )

      IF( ITOFR .GT. 0 ) THEN

        NTRK = IDATA( ITOFR + 1 )

C                            Get the pointers to the TPTR banks

        CALL BDAR( 'TPTR', NBNKS, IPNTA, LENAR )

C                            Warn if no. of TPTR banks exceeds space

        IF( NBNKS .EQ. LENAR ) THEN
          CALL TPWARN
        ENDIF

C                            Process each TPTR bank in turn

        DO  100  I = 1,NBNKS

          ITPTRI  = IPNTA(I)

C                            Is the track seen in the Jet Chamber?

          IF( HDATA( 2*ITPTRI + 3 ) .LT. 1000 ) GO TO 100

          INDEX = HDATA( 2*ITPTRI + 4 )

          IF( INDEX .LE. 0  .OR.  INDEX .GT. NTRK ) THEN
            CALL TPERRH
            CALL TPSEFL( TOF, 1000 )
          ELSE
            NT = ITOFR + 4 + ( INDEX - 1 ) * NSTEP

            IF( IDATA( NT + 1 ) .NE. INDEX ) THEN
              CALL TPERRH
              CALL TPSEFL( TOF, 1000 )
            ELSE

C                            TOF quality

              IDATA( ITPTRI + 58 ) = IDATA( NT + 2 )

C                            TOF identification probabilities

              DO  20  K = 4,14
                ADATA( ITPTRI + 55 + K ) = ADATA( NT + K )
   20         CONTINUE

            ENDIF

          ENDIF

  100   CONTINUE

      ELSE

        CALL TPSEFL( TOF, 1000 )
        CALL TPWARN

      ENDIF

      RETURN
      END
