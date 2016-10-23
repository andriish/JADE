C   29/09/86 807212230  MEMBER NAME  TPSTAR   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPSTAR
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery       3/10/86:  Initialise the TP program
C
C          mod: C. Bowdery       4/02/88:  Add CALL TPVOPT here
C     Last mod: C. Bowdery      21/07/88:  TPRDBR --> TPBANR
C
C     Routine to initialise the TP program before events are read.
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      INTEGER  JU

      LOGICAL  ERR, ERR2

C------------------  C O D E  ------------------------------------------

C                            Prevent timer overflow by call to JUHR

      JU = JUHR( 2 )

C                            Get the required TP options.

      CALL TPGOPT( ERR )

C                            Print banner twice including COMMENTs

      CALL TPBANR
      CALL TPBANR

C                            Print message about options selected

      CALL TPPOPT

C                            Verify the options in the table

      CALL TPVOPT( ERR2 )

      IF( ERR  .OR.  ERR2 ) THEN

        WRITE(6,1)
  1     FORMAT(/////' P R E M A T U R E    E N D    O F    P R O G R A M
     +   D U E   T O   O P T I O N S    E R R O R')
        STOP  999

      ELSE
C                            Initialise BOS memory manager

        CALL TPBOSI

C                            Initialise the various packages to be used

        CALL TPPINT

      ENDIF

      RETURN
      END
