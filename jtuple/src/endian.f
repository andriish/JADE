      LOGICAL FUNCTION ENDIAN(CNAM)
      IMPLICIT NONE
*.***************************************************************
*.
*.  01/12/00   P.A. Movilla Fernandez
*.
*.  This routine checks if the endian format of current platform
*.  matches the endian format of the BINARY ZE4V input data.
*.
*.  Input:
*.        CNAM             ... name of the data file
*.  On return:
*.        ENDIAN = .TRUE.  ... format of current platform does match data format
*.        ENDIAN = .FALSE. ... format of current platform does NOT match data format
*.
*.  This is only an 'educated guess'. The routine checks the first
*.  integer words of the event record:
*.      1 = size of the fortran record length in bytes
*.      2 = size of the BOS record in full words
*.      4 = ZE4V BOS bank number (=1)
*.  The values should generally not exceed 65535 if the endian format
*.  of the data matches the endian format of the current platform.
*.
*.***************************************************************
      CHARACTER CNAM*(*)
C Check endian format of current platform
      INTEGER IBIG
      INTEGER*2 HBIG(2)
      EQUIVALENCE (IBIG,HBIG)
      DATA IBIG /1/
C
      INTEGER IUIN,IOS,N(5),LIM,NREAD
      DATA LIM/65535/
C Test input file
      CALL CFOPEN(IUIN,0,1,'r   ',0,CNAM,IOS )
      IF( IOS.NE.0 ) THEN
         WRITE(*,'(2A)') ' ENDIAN: Error while opening file ',CNAM
         WRITE(*,'(A)') ' ...will stop now!'
         STOP
      ENDIF
C Read first words of the record
      NREAD=5
      CALL CFGET(IUIN,0,NREAD,NREAD,N,IOS)
C Check words 1,2 and 4
      IF( ABS(N(1)).LE.LIM.AND.ABS(N(2)).LE.LIM.AND.ABS(N(4)).LE.LIM )
     >     THEN
         ENDIAN=.TRUE.
      ELSEIF(ABS(N(1)).GT.LIM.AND.ABS(N(2)).GT.LIM.AND.ABS(N(4)).GT.LIM)
     >        THEN
         ENDIAN=.FALSE.
         CALL VXINVB(N,2)
         CALL VXINVB(N(4),2)
      ELSE
         WRITE(*,'(3A)') ' ENDIAN: Cannot decide endian format'
     >        ,' of input data file ',CNAM
         WRITE(*,'(A,2I8,A5,I8)') ' Record starts with:',N
         WRITE(*,'(A)') ' ...will stop now!'
         STOP
      ENDIF
C I/O format info
      WRITE(*,'(/,A,/,A,2I8,A5,2I8)') 'ENDIAN:'
     >     ,' Data record starts with:',N
      WRITE(*,'(A,$)') ' JADE data I/O format: '
      IF( ENDIAN ) THEN
         WRITE(*,'(A,$)') 'binary ZE4V data and current platform with'
     >  //' EQUAL endian format.'
      ELSE
         WRITE(*,'(A,$)') 'binary ZE4V data and current platform with'
     >  //' DIFFERENT endian format.'
      ENDIF
      IF( HBIG(1).EQ.1 ) THEN
         WRITE(*,'(A)') ' Current platform is LITTLE ENDIAN.'
      ELSE
         WRITE(*,'(A)') ' Current platform is BIG ENDIAN.'
      ENDIF
C Close input file
      CALL CFCLOS(IUIN,0)
      RETURN
      END

