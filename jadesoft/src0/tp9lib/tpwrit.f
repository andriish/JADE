C   29/09/86 901181551  MEMBER NAME  TPWRIT   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPWRIT
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery       1/10/86:  Write out an event for TP
C
C          mod: C. Bowdery      16/12/87:  LISTNW now CHARACTER array
C          mod: C. Bowdery      27/01/88:  Reduction done here
C          mod: C. Bowdery      23/07/88:  Add new bank names
C          mod: C. Bowdery      27/07/88:  Add TPCHOP
C     Last mod: C. Bowdery      18/01/89:  Add TPMCON
C
C     Routine to check an event and then write it out.
C     If reduction is wanted and the event is rejected, it is written
C     out to a 'reject' dataset.
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

C                            Parameters for output unit numbers

#include "units.for"

      PARAMETER  ( NNEWBK = 15 )

      CHARACTER*4  LISTNW( NNEWBK )

      LOGICAL  REJECT

C                            COMMON for output statistics

      COMMON / CTPOUT / NACC, NREJ


      DATA  LISTNW / 'PATR', 'ALGN', 'LGCL', 'MUR1', 'MUR2',
     +               'TPEV', 'TPTR', 'TPVX', 'TOFR', 'JHTL',
     +               'VTXC', 'VPAT', 'VTHT', 'HWDS', 'HHTL'   /

C------------------  C O D E  ------------------------------------------

C                            Chop off the unused part of TPVX/1

      CALL TPCHOP

C                            Check the event

      CALL TPCHCK

C                            Add new bank names to the special list

      CALL BSAW( NNEWBK, LISTNW )

C                            If event reduction wanted, do it.
C                            If REJECT = .TRUE. write event onto
C                            'rejects' output dataset not standard one.

      CALL TPREDC( REJECT )

C                            Ensure that MC constants will be output
C                            if required. Flag may need setting!

      CALL TPMCON( REJECT )

C                            Steer event to the correct output stream:
C                            ACCEPT dataset or the REJECT dataset

      IF( REJECT ) THEN

        CALL EVWRIT( JUNITR )
        NREJ = NREJ + 1

      ELSE

        CALL EVWRIT( JUNITA )
        NACC = NACC + 1

      ENDIF

      CALL TPDIAG

      RETURN
      END
