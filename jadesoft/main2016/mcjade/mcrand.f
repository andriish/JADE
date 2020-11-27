C
      SUBROUTINE MCRAND(COPT,FNAM,IFIRST,ILAST,IERR)
      IMPLICIT NONE
**********************************************************
*
*     Initialise random number generator RANMAR and
*     read or write random seeds from or to disk.
*
*     28/05/00:   Pedro Movilla Fernandez
*
*     INPUT:  COPT='R': Read initial seed from file
*             COPT='W': Write current seed to file
*             FNAM    : Name of the file with initial seed
*        IFIRST,ILAST : First and last event number corresponding to
*                       the start and the end of the current random seed 
*                       (only for users book keeping purposes)
*
*     OUTPUT: IERR    : = 0    o.k.
*                       =-1    read or write error
*                       =-2    invalid option COPT
*
**********************************************************
      CHARACTER COPT*1,FNAM*(*)
      INTEGER IERR,IFIRST,ILAST
C
      INTEGER I,IOS,IOS2,NEVTOT,IFIRST0,ILAST0
      LOGICAL LEXIST
      DATA NEVTOT/0/,IFIRST0/0/,ILAST0/0/
      SAVE NEVTOT
C
      INTEGER IJKLIN,IJKLUT,NTOTIN,NTOTUT,NTO2IN,NTO2UT
      INTEGER J,IBUF(106),MTOT,MTOT2
      SAVE MTOT,MTOT2
C Use modified common /RANMA1/ to initialize with the private version of RANMAR.
      CHARACTER C1*1
      INTEGER IJKL,NTOT,NTOT2,I97,J97
      REAL C,U
      LOGICAL RANINI,RANPRV
      COMMON/RANMA1/IJKL,NTOT,NTOT2,I97,J97,C,U(97),RANINI,RANPRV
      SAVE /RANMA1/
Code:
      write(*,*)'WOW',FNAM
      RANINI=.TRUE. ! If .true., then initialization via common /RANMA1/ is enabled
      IERR=0
      I=0
      CALL UZERO(IBUF,1,106)
C
      INQUIRE(FILE=FNAM,EXIST=LEXIST)
C --- Find last position in the random seed file
      IF( LEXIST ) THEN
         OPEN(UNIT=99,FILE=FNAM,STATUS='OLD',FORM='FORMATTED')
         PRINT *,'MCRAND: Random number seeds found in '//FNAM//':'
 100     READ(UNIT=99,IOSTAT=IOS,END=110,FMT=991) C1,(IBUF(J),J=1,106)
         IF( IOS.NE.0 ) THEN
            PRINT *,'MCRAND:  Something is wrong with file '//FNAM//'.'
            PRINT *,'... Will stop now!'
            IERR=-1
            GOTO 999
         ENDIF
         I=I+1
         WRITE(*,FMT=990) I,(IBUF(J),J=1,106)
         GOTO 100
C --- Create new random number seed file
      ELSE
         OPEN(UNIT=99,FILE=FNAM,STATUS='NEW',FORM='FORMATTED')
      ENDIF
 110  CONTINUE
      IF( COPT.EQ.'R' ) THEN
         IF( LEXIST ) THEN
C --- Initialise random number generator with last sequence found in the file
C     NB.: If using the private version of RANMAR, and if RANINI.EQ..TRUE., then
C          RMARIN will only set IJKL, NTOT, NTOT2 in common /RANMA1/, and RANPRV is
C          set to .TRUE.. In this case, initialization will be performed directly
C          via common /RANMA1/ (this is much faster than using RMARIN).
            IJKLIN=IBUF(1)
            NTOTIN=IBUF(2)
            NTO2IN=IBUF(3)
            IFIRST0=IBUF(4)
            ILAST0=IBUF(5)
            NEVTOT=IBUF(6)
            MTOT=NTOTIN
            MTOT2=NTO2IN
C        ... initialise via RMARIN (this call checks version of RANMAR used here)
            CALL RMARIN(IJKLIN,NTOTIN,NTO2IN)
C        ... or directly via /RANMA1/
            IF( RANPRV ) CALL UCOPY(IBUF(7),I97,100)
         ELSE
C --- No initialisation
            PRINT *,'MCRAND: Seed file '//FNAM//' does not exist.'
            PRINT *,'... Will use default seed!'
            GOTO 999
         ENDIF
         CLOSE(UNIT=99)
         PRINT *,'MCRAND: Initial seed for RANMAR used for this run:'
         WRITE(*,IOSTAT=IOS,FMT=990) I,IJKLIN,NTOTIN,NTO2IN
     >        ,IFIRST0,ILAST0,NEVTOT,I97,J97,C,(U(J),J=1,97)
      ELSE IF( COPT.EQ.'W' ) THEN 
C --- Write out current random number status to the file
C     NB.: The private version of RANMAR handles the counters NTOT and NTOT2 correctly.
         CALL RMARUT(IJKLUT,NTOTUT,NTO2UT)
         IF( .NOT.RANPRV ) THEN
            NTOTUT=NTOTUT+MTOT
            NTO2UT=NTO2UT+MTOT2
         ENDIF
         NEVTOT=NEVTOT+ILAST-IFIRST+1
         I=I+1
         PRINT *,'MCRAND: New random number seed added to '//FNAM//':'
         IBUF(1)=IJKLUT
         IBUF(2)=NTOTUT
         IBUF(3)=NTO2UT
         IBUF(4)=IFIRST
         IBUF(5)=ILAST
         IBUF(6)=NEVTOT
         CALL UCOPY(I97,IBUF(7),100)
         WRITE(*,FMT=990) I,(IBUF(J),J=1,106)
         WRITE(UNIT=99,IOSTAT=IOS,FMT=991) '#',(IBUF(J),J=1,106)
         CLOSE(UNIT=99,IOSTAT=IOS2)
         IF( IOS.NE.0 .OR. IOS2.NE.0 ) THEN
            PRINT *,'MCRAND: Error while writing out'
     >           //'current random number status to file '//FNAM//'!'
            IERR=-1
            GOTO 999
         END IF
      ELSE
         PRINT *,'MCRAND: Bad option COPT= '//COPT
         IERR=-2
      ENDIF
C
 999  RETURN
C 990  FORMAT(I2,':',3I12,'  (',3I8,'  )',/,2I12,8F12.8,9(/,10F12.8))
 990  FORMAT(I2,':',3I12,'  (',3I8,'  )',10(/,10I12))
 991  FORMAT(A1,3I12,3I8,10(/,10I12))
      END
