C   05/02/87            MEMBER NAME  TPTAGA   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPTAGA
C-----------------------------------------------------------------------
C
C     Author:  C. Bowdery        5/02/87:  Analyse tagging system
C              A. Finch
C
C
C     Routine to analyse tagging system to search for clusters if
C     requested to.
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      LOGICAL  ANALYS

C------------------  C O D E  ------------------------------------------

C                            Get instruction about tagging analysis

      CALL TPTGI1( ANALYS )

C                            If analysis wanted, delete old banks first

      IF( ANALYS ) THEN

        CALL BMLT( 2,'ACLSTAGG' )
        CALL BDLM

C                            Perform tagging analysis
C                              IER > 1  if error has occurred
C                                    except IER = 10

        CALL TAGAN( IER, 0 )

        IF( IER. GT. 1  .AND.  IER .NE. 10 ) THEN
          CALL TPERRH
        ELSE
          CALL TPDIAG()
        ENDIF

      ENDIF

      RETURN
      END
