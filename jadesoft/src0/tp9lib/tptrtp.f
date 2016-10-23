C   06/11/87 810041241  MEMBER NAME  TPTRTP   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPTRTP
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery       6/11/87:  Create track summary banks.
C
C          mod: C. Bowdery       7/06/88:  TPTRFM now an option.
C          mod: C. Bowdery      30/09/88:  CALL TPSEFL now
C     Last mod: C. Bowdery       4/10/88:  Extra TPSEFL call added.
C
C     Routine to create summary banks for tracks and post-process
C     these TPTR banks to find vertices and create TPVX banks.
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      LOGICAL  SUMMRY, VPROC

      INTEGER  PATR, VXFIT

      PARAMETER ( PATR = 1, VXFIT = 2 )

C------------------  C O D E  ------------------------------------------

C                            Get instruction relating to track summary

      CALL TPTRI1( SUMMRY )
C                            Get instruction relating to vertex proc.

      CALL TPTRI2( VPROC )

      IF( SUMMRY ) THEN

C                            Create summary banks from PATR bank.

        CALL TPTRSM
        CALL TPDIAG()

        IF( VPROC ) THEN

C                            Find vertices and determine fine momentum
C                            correction.

          CALL TPTRFM
          CALL TPDIAG()

        ELSE

C                            Set flag to indicate no vxfit

          CALL TPSEFL( VXFIT, 4000 )

        ENDIF

      ELSE

C                            Set flag to indicate no summary

        CALL TPSEFL( PATR,  4000 )

C                            Set flag to say vertex processing was
C                            not wanted or was not possible

        IF( VPROC ) THEN

          CALL TPSEFL( VXFIT, 1000 )

        ELSE

          CALL TPSEFL( VXFIT, 4000 )

        ENDIF

      ENDIF

      RETURN
      END
