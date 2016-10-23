C   09/02/87 809301316  MEMBER NAME  TPTRSM   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPTRSM
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery      22/07/87:  Create track summary banks
C
C          mod: C. Bowdery      26/07/87:  HNPV --> INPV
C          mod: C. Bowdery       6/11/87:  Renamed from TPTRTP
C          mod: C. Bowdery       6/11/87:  No PATR is not an error
C          mod: C. Bowdery      11/01/88:  SETSL--> VZERO
C          mod: C. Bowdery       1/02/88:  No tracks is not an error
C     Last mod: C. Bowdery      30/09/88:  CALL TPSEFL now
C
C     Routine to create summary TPTR banks for ID tracks from PATR bank.
C     Based upon TPPATR by S. Yamada.
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

#include "bosdata.for"

      INTEGER  NVLM, PATR

C                            NVLM = no. of words in the TPVX/1 bank

      PARAMETER  ( NVLM   = 100, PATR = 1 )

C                            IV: work space for the event vertex

      INTEGER    IV( NVLM )
      INTEGER*2  HV( 2*NVLM )

      EQUIVALENCE ( HV(1), IV(1) )

C------------------  C O D E  ------------------------------------------

C                            Get magnetic field in gauss --> kgauss

      CALL TPMAGF( BGAUSS )
      BKG = BGAUSS * 0.001

C                            Clear the vertex bank array  IV

      CALL VZERO( IV, NVLM )

C                            Initialise pointer in vertex bank for
C                            storing next track number
      INPV = 30

C                            Get the number of PATR tracks

      IPATR = IDATA( IBLN( 'PATR') )
      IF( IPATR .EQ. 0 ) THEN
        CALL TPWARN
        CALL TPSEFL( PATR, 1000 )
      ELSE

        NTRK = IDATA( IPATR + 2 )
        IF( NTRK .LE. 0 ) THEN

C                            No tracks. Look into this

          CALL TPWARN
          CALL TPSEFL( PATR, 1000 )

        ELSE

C                            NTPTR is the next TPTR bank no. to be made
          NTPTR = 1

C                            Process each track, updating NTPTR

          DO  10  ITRK = 1,NTRK

            CALL TPTRTS( IPATR, ITRK, BKG, NTPTR, HV, INPV, IER )

            IF( IER .NE. 0 ) THEN
              CALL TPSEFL( PATR, 2000 )
              CALL TPWARN
              RETURN
            ENDIF

   10     CONTINUE

C                            Create the TPVX/1 bank with full length
C                            and copy contents of IV into it.

          CALL CCRE( NPVX, 'TPVX',1, NVLM, IER)

          IF( IER .NE. 0 ) THEN

            CALL TPERRH
            CALL TPSEFL( PATR, 2000 )

          ELSE

            CALL BSTR( NPVX, IV, NVLM )

C + + +                      debug start
C           CALL BPRS( 'TPVX',1 )
C + + +                      debug end

          ENDIF

        ENDIF

      ENDIF

      RETURN
      END
