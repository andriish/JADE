C
C Routine RCONV reads data from a binary calibration file and
C shows the contents of a bank specified by variable BANK
C 
C
C Date:    16/11/98   Author:  P.M. Fernandez
C
      PROGRAM RCONV
      IMPLICIT NONE
C
C Insert HERE name of file to be converted
      CHARACTER CIN*100 /'aupdat1.b'/
      INTEGER NUNIT/10/
C 
C Insert HERE name, number and validation date of a bank
**      CHARACTER*4 BANK /'VTXC'/  
**      INTEGER NR /1/,IDATE /225118651/
*      CHARACTER*4 BANK /'JTPL'/  
*      INTEGER NR /1/,IDATE /14688000/
      CHARACTER*4 BANK /'MUCA'/  
      INTEGER NR /2/,IDATE /171331200/
C
      INTEGER ICYCL
      LOGICAL SEE/.FALSE./
C
      REAL ABUF(10000)
      INTEGER IBUF(10000)
      INTEGER*2 HBUF(20000)
      CHARACTER*4 CBUF(10000)
      EQUIVALENCE (ABUF(1),IBUF(1)),(CBUF(1),IBUF(1)),(HBUF(1),IBUF(1))
      INTEGER NWORD,NREC,IOUT,IRET,I,K
      CHARACTER*1 GOON
C
C Open unformatted input file
      OPEN (NUNIT,FILE=CIN,STATUS='UNKNOWN',FORM='UNFORMATTED')
      WRITE(*,'(A15,A)') 'Input file: ',CIN
      NREC=0
C
C Read length of record
 10   READ(NUNIT,ERR=1000,END=2000) NWORD
      BACKSPACE(NUNIT)
C
C Read one record
      CALL RDAT(NUNIT,NWORD,IBUF,IRET)
      IF(IRET.EQ.1) GOTO 1000
      IF(IRET.EQ.2) GOTO 2000
C
      NREC=NREC+1
      IOUT=NWORD
      IF(IOUT.GT.9) IOUT=9  
C
C Print out header
      IF (NWORD.EQ.1) THEN
         WRITE(6,901)  NWORD,IBUF(1)
         NREC=0
      ENDIF
      IF (NWORD.GT.1) WRITE(6,900)  NREC,NWORD,(IBUF(I),I=1,IOUT)
C
C Print out content
      IF(.NOT.SEE.AND.
     +     BANK.EQ.CBUF(1).AND.NR.EQ.IBUF(2).AND.IDATE.EQ.IBUF(6))
     +     SEE=.TRUE.
C
      IF(NWORD.GT.9.AND.SEE) THEN
         DO I=9,NWORD-1,10
            ICYCL=MIN(10,NWORD-I)
            print *,icycl,nword,i
            WRITE(6,902) NREC,CBUF(1),I
            WRITE(6,'(10I16)')  (I+K,K=1,ICYCL)
            WRITE(6,'(20I8)')  (I+K,K=1,2*ICYCL)
            WRITE(6,'(80('' .''))')
            WRITE(6,'(10I16)')  (IBUF(I+K),K=1,ICYCL)
            WRITE(6,'(20I8)')  (HBUF(2*I+K),K=1,2*ICYCL)
            WRITE(6,'(10E16.5)')  (ABUF(I+K),K=1,ICYCL)
            WRITE(6,'(10A16)')  (CBUF(I+K),K=1,ICYCL)
            WRITE(*,'(''Press RET or eXit'')')
            READ(*,'(A)') GOON
            IF(GOON.EQ.'X'.OR.GOON.EQ.'x') GOTO 100
         ENDDO
      ENDIF
C
 100  CONTINUE
      IF(.NOT.SEE) GOTO 10
      WRITE(*,'(''Continue with next bank ? (y/...)'')')
      READ (*,'(A1)') GOON
      IF (GOON.EQ.'y'.OR.GOON.EQ.'Y') GOTO 10
      GOTO 9999
C
 900  FORMAT(/1X,'Record number:',I5,4X,'Words:',I6
     +     ,2X,'Header:',' ',A4,' ',2I2,I2,2I10,I6,I10,I2)
 901  FORMAT(/1X,'Record number:',I5,4X,'Validation time:',I12)
 902  FORMAT(160('-')/1X,'Record number: ',I6,'   Bank: '
     +     ,A4,'   Word: ',I6)
C
 1000 CONTINUE
      WRITE(6,101) NREC
 101  FORMAT(//' *** READ ERROR ENCOUNTERED AFTER ',I6,' RECORDS.')
      GOTO 9999
 2000 CONTINUE
      WRITE(6,102) NREC
 102  FORMAT(//' NORMAL END OF FILE AFTER',I6,' RECORDS.')
      GOTO 9999
C
 9999 CONTINUE
      CLOSE(NUNIT)
      STOP
      END
C----------------------------------------
      SUBROUTINE RDAT(NUNIT,NWORD,IBUF,IRET)
      IMPLICIT NONE
      INTEGER IBUF,NWORD,IRET,NUNIT,I
      DIMENSION IBUF(NWORD)
      READ(NUNIT,ERR=1,END=2) NWORD,IBUF
      IRET=0
      RETURN
    1 CONTINUE
      IRET=1
      RETURN
    2 IRET=2
      RETURN
      END
