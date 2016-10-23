C   07/02/87 809301223  MEMBER NAME  TPTOFA   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPTOFA
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery       7/02/87:  Call TOF analysis
C
C          mod: C. Bowdery      16/12/87:  & replaced by *
C          mod: C. Bowdery       1/02/88:  No analysis if only PATR/12
C     Last mod: C. Bowdery      29/09/88:  CALL TPSEFL
C
C     Routine to call for TOF analysis if user options request it.
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      LOGICAL  ANALYS

      INTEGER  IRUN, IPATR, IATOF, NPATR

#include "bosdata.for"

C------------------  C O D E  ------------------------------------------

C                            Get instruction about TOF analysis

      CALL TPTFI1( ANALYS )

C                            If analysis wanted, delete old banks first

      IF( ANALYS ) THEN
C
        CALL BMLT( 1, 'TOFR')
        CALL BDLM

C                            Get ATOF bank and latest PATR bank

        CALL CLOC( IATOF, 'ATOF', 0 )

        IF( IATOF .GT. 0 ) THEN

          IPATR = IDATA( IBLN('PATR'))
          IF( IPATR .GT. 0 ) THEN
            NPATR = IDATA( IPATR - 2 )
            IF( NPATR .NE. 12 ) THEN

C                            Perform TOF analysis

              CALL TPRUN( IRUN )
              CALL TOFINT( IRUN, IATOF, IPATR, *5 )

              CALL TPDIAG()

            ENDIF

          ENDIF

        ENDIF

      ENDIF

      RETURN

C                            Error return from TOFINT

  5   CALL TPWARN
      RETURN

      END
