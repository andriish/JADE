C   09/12/87 807261208  MEMBER NAME  TPTHRU   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPTHRU( NP, P )
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery       9/12/87:  Do thrust calculation.
C
C          mod: C. Bowdery      31/01/88:  CALL TPEVI2 etc
C     Last mod: C. Bowdery      26/07/88:  Change to TPTPUT call
C
C     Routine to coordinate thrust calculation and storing of results
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     NP        In       I*4      No. of particles in P array
C     P         In       R*4      Momentum array
C                                   P(1,i) = px   of particle i
C                                   P(2,i) = py   of particle i
C                                   P(3,i) = pz   of particle i
C                                   P(4,i) = ptot of particle i
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      INTEGER  NP, NMAXP, ITMERR
      REAL     P(4,200)
      REAL     T, AXIS(3)

      LOGICAL  WANTED

C------------------  C O D E  ------------------------------------------

C                            Compute thrust and thrust axis if wanted

      CALL TPEVI2( WANTED )

      IF( WANTED  .AND.  NP .GT. 0 ) THEN

C                           Calc. limited to 15 most energetic particles

        NMAXP = 15
        CALL THRUST( P, NP, NMAXP, T, AXIS, ITMERR )

C                            Store thrust and thrust axis

        CALL TPTPUT( NP, NMAXP, T, AXIS )

      ENDIF

      RETURN
      END
