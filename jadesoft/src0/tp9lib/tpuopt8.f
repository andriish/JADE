C   29/09/86 711061741  MEMBER NAME  TPUOPT8  (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPUOPT( CHAR, IOPTS )
C-----------------------------------------------------------------------
C
C     Author:  C. Bowdery        1/10/86:  Provide options
C
C
C
C     Routine to provide options  (DUMMY VERSION)
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      INTEGER  IOPTS(10)

      CHARACTER*4  CHAR

C------------------  C O D E  ------------------------------------------

      IF( CHAR .EQ. 'MUON' ) THEN
        IOPTS(1) = -1
        IOPTS(2) = 1
        IOPTS(3) = 0

      ELSE IF( CHAR .EQ. 'TAGG' ) THEN
        IOPTS(1) = 0
        IOPTS(2) = 0
        IOPTS(3) = 0

      ELSE IF( CHAR .EQ. 'TOFS' ) THEN
        IOPTS(1) = -1
        IOPTS(2) = 1
        IOPTS(3) = 0

      ELSE IF( CHAR .EQ. 'JETC' ) THEN
        IOPTS(1) = -1
        IOPTS(2) = 1
        IOPTS(3) = 1
        IOPTS(4) = -1
        IOPTS(5) = 1

      ELSE IF( CHAR .EQ. 'LGSC' ) THEN
        IOPTS(1) = -1
        IOPTS(2) = -1
        IOPTS(3) = 1
        IOPTS(4) = 1

      ELSE IF( CHAR .EQ. 'RFIT' ) THEN
        IOPTS(1) = -1
        IOPTS(2) = 1
        IOPTS(3) = -1
        IOPTS(4) = 1
        IOPTS(5) = 2

      ELSE IF( CHAR .EQ. 'DEDX' ) THEN
        IOPTS(1) = 1

      ELSE IF( CHAR .EQ. 'TRAK' ) THEN
        IOPTS(1) = 1

      ELSE
        CALL TPERRH
        IOPTS(1) = 0
        IOPTS(2) = 0
        IOPTS(3) = 0

      ENDIF

      RETURN
      END
