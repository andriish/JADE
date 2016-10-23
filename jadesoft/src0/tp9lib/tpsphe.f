C   09/12/87 807261205  MEMBER NAME  TPSPHE   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPSPHE( NP, P )
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery       9/12/87:  Do sphericity calculation.
C
C          mod: C. Bowdery      12/01/88:  Add ARGUMENTS comment.
C          mod: C. Bowdery      31/01/88:  Add CALL TPEVI1, etc
C     Last mod: C. Bowdery      26/07/88:  NP added to TPSPUT call
C
C     Routine to coordinate sphericity calculation & storing of results
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

      INTEGER  NP
      REAL     P(4,200)
      REAL     SPHITY(3), AXES(9)

      LOGICAL  WANTED

C------------------  C O D E  ------------------------------------------

C                            Is the sphericity calculation wanted?

      CALL TPEVI1( WANTED )

      IF( WANTED  .AND.  NP .GT. 0 ) THEN

C                            Compute sphericty eigenvalues, SPHITY(1..3)
C                            and eigenvectors, AXES(1..9), where 1..3
C                            are direction cosines of minor axis, etc.

        CALL SPHRCY( P, NP, SPHITY, AXES )

C                            Store eigenvalues and eigenvectors

        CALL TPSPUT( NP, SPHITY, AXES )

      ENDIF

      RETURN
      END
