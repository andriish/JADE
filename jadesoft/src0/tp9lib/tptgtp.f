C   06/02/87 809301211  MEMBER NAME  TPTGTP   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPTGTP
C-----------------------------------------------------------------------
C
C     Author:  C. Bowdery        6/02/87:  Add tagg info into TPTR banks
C              A. Finch
C
C          mod: C. Bowdery      25/07/88:  Add extra argument to TPTGT1
C     Last mod: C. Bowdery      30/09/88:  CALL TPSEFL now
C
C
C     Routine to copy information from TAGG banks into TPTR
C     banks. See JCN 16 for 'TAGG' bank contents.
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

#include "bosdata.for"

      INTEGER  LENAR, TAGG, NTAG, NTAG1, NTAG2, NHTAG, NTRACK
      INTEGER  NWPCL, NWPTR, NBNKS, LAST, ITPVX, MODVTX, ITRACK
      INTEGER  NVLM2, NPV2, IER, IB

      PARAMETER  ( LENAR = 100, TAGG = 7 )

      INTEGER  IPNTA( LENAR )

C------------------  C O D E  ------------------------------------------

C                            Get pointer to  TAGG banks (0,1,2)

      CALL CLOC( NTAG,  'TAGG', 0 )
      CALL CLOC( NTAG1, 'TAGG', 1 )
      CALL CLOC( NTAG2, 'TAGG', 2 )

C                            Set error flag = 1000 if TAGG banks missing

      IF( NTAG .LE. 0  .OR.  NTAG1 .LE. 0  .OR.  NTAG2. LE. 0 ) THEN
        CALL TPSEFL( TAGG, 1000 )
      ELSE

C                            Find number of tagging clusters

        NHTAG  = 2*NTAG
        NTRACK = HDATA( NHTAG + 6 )

        IF( NTRACK .GT. 0 ) THEN

          NWPCL  = HDATA(NHTAG+53)
          NWPTR  = HDATA(NHTAG+54)

          CALL BDAR( 'TPTR', NBNKS, IPNTA, LENAR )
          LAST = NBNKS

C                            Warn if no. of TPTR banks exceeds space

          IF( NBNKS .EQ. LENAR ) THEN
            CALL TPWARN
          ENDIF

C                            Locate TPVX/1 bank

          CALL CLOC( ITPVX, 'TPVX',1 )

          IF( ITPVX .LE. 0 ) THEN

            CALL TPWARN
            MODVTX = 2

          ELSE

            NVLM2  = 2*( IDATA(ITPVX) + ITPVX )
            NPV2   = 2*ITPVX + HDATA( 2*ITPVX + 22 ) + 30

            MODVTX = HDATA( 2*ITPVX + 2 )
            IF( MODVTX .NE. 0 ) MODVTX = 20

          ENDIF

          IB = NTAG2

          DO  50  ITRACK = 1,NTRACK

C                            Copy TAGG/2 cluster to new TPTR bank

            CALL TPTGT1( IB, ITRACK, LAST, MODVTX, IER )

            IF( IER .NE. 0 ) THEN
              CALL TPSEFL( TAGG, 2000 )
              CALL TPERRH
              RETURN
            ENDIF

C                            Include tagging tracks in TPVX/1 counts

            IF( ITPVX .GT. 0 ) THEN

              HDATA( 2*ITPVX + 22 ) = HDATA( 2*ITPVX + 22 ) + 1
              HDATA( 2*ITPVX + 26 ) = HDATA( 2*ITPVX + 26 ) + 1
              NPV2 = NPV2 + 1
              IF( NPV2 .LE. NVLM2 ) HDATA(NPV2) = LAST

            ENDIF

C                            Update pointer to TAGG/2 bank

            IB = IB + NWPCL

 50       CONTINUE

        ENDIF
      ENDIF

      RETURN
      END
