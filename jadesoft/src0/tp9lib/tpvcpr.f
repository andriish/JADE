C   09/02/87 807222021  MEMBER NAME  TPVCPR   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPVCPR
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery      22/07/88:  Process Vertex Chamber
C
C
C
C     Routine to process the Vertex Chamber and perform the COMFIT
C     that is, a combined fit along with the Jet Chamber.
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      LOGICAL  ANALYS, COMFIT

      INTEGER  MODE, IVRET1, IVRET2

C------------------  C O D E  ------------------------------------------

C                            Get instructions about VTXC analysis and
C                            combined fit with Jet Chamber.

      CALL TPVCI1( ANALYS )
      CALL TPVCI2( COMFIT )

      MODE = 0
      IF( ANALYS ) MODE = MODE + 1
      IF( COMFIT ) MODE = MODE + 2

      IF( MODE .GT. 0 ) THEN

C                            Set fudge factors then do analysis/comfit
C                            using lowest number PATR bank.

        CALL VTXCSF( IVRET1 )

        CALL VTXCSV( MODE, -1, -1, IVRET2 )

        CALL TPDIAG

      ENDIF

      RETURN
      END
