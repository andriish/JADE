**************************************************
* IPS
* ====
*
*   Emulation of some public IPS routines
*
*   06/10/98  P.A. Movilla Fernandez 
*
*   21/10/99  PMF last mod.
*   09/12/99  PMF last mod.
*
*     GETPDD
*     FREEDD
*     DDCHA
*
**************************************************
**************************************************
      SUBROUTINE GETPDD(NAME,VOLSR,DDN,LUN,HERR)
      IMPLICIT NONE
*
* GETPDD is a public IPS subroutine.
*
* GETPDD allocates a dataset 'NAME' and assigns it
* with a logical file number 'LUN'. If LUN=0 on entry,
* the value of LUN on exit will be decided here.
*
* Use simple OPEN and INQUIRE statement to emulate GETPDD.
*
* Still usefull error codes used here ( see also SUBROUTINE DSERR ) :
* HERR =    1 : all unit numbers have been used 
*      =    3 : dataset non-existent or name wrongly entered
*      =  528 : file for specified unit number already allocated
*               [REQUESTED DS NOT AVAILABLE]
*      =  863 : invalid logical file number 
*               [INVALID PARAMETER]
*      = 1040 : file already allocated
*
* Some 'inofficial' error codes are additionally introduced.
*
      INTEGER LUN
      INTEGER*2 HERR
      CHARACTER NAME*44
      REAL*8 VOLSR,DDN,DDN2
C
      CHARACTER CDDN2*8,CNAME*44,CFMT*11,CSTAT*7
      EQUIVALENCE (DDN2,CDDN2)
      LOGICAL LOCKED,EX,LWRIT
      INTEGER LUN0,LUN9,I,IERR,NUM
      PARAMETER(LUN0=2,LUN9=60)
C
      INTEGER IWFIL/0/
      CHARACTER WFIL*44(10)
      SAVE IWFIL
Code:
      HERR=0
C Check if logical file number and file name is valid
      IF(LUN.NE.0 .AND. (LUN.LT.LUN0 .OR. LUN.GT.LUN9)
     +     .OR. INDEX(NAME,' ').EQ.1 )! .OR. INDEX(NAME,'.').EQ.1 )
     +     THEN
         HERR=863               ! invalid parameter
         GOTO 15
      ENDIF

C Check if logical file number is already in use
      IF(LUN.EQ.0) THEN
         DO I=LUN0,LUN9
            INQUIRE(UNIT=I,OPENED=LOCKED)
            IF(.NOT.LOCKED) THEN
               LUN=I
               GOTO 5           ! found an unlocked unit number, continue
            ENDIF
         ENDDO
         HERR=1                 ! all unit numbers have been used
      ELSE
         INQUIRE(UNIT=LUN,OPENED=LOCKED)
         IF(LOCKED) HERR=528    ! file for specified unit number already allocated
      ENDIF
 5    CONTINUE

C Check if file is an output file ( indicated by a suffix '@' )
      IF( INDEX(NAME,'@').NE.0 ) THEN
         NAME(INDEX(NAME,'@'):INDEX(NAME,'@'))=' '
         LWRIT=.TRUE.
         IWFIL=IWFIL+1
         IF(IWFIL.GT.10) THEN
            HERR=531            ! Maximum number of OUTPUT files reached.
            GOTO 15
         ENDIF
         WFIL(IWFIL)=NAME
      ELSE
         LWRIT=.FALSE.
      ENDIF

C Check if file exists or if file is already allocated
      INQUIRE(FILE=NAME,EXIST=EX,OPENED=LOCKED,NUMBER=NUM)
      IF(.NOT.EX .AND. .NOT.LWRIT ) HERR=3
      IF(LOCKED) THEN 
         IF( .NOT. LWRIT ) THEN
            HERR=1040
            DO I=1,10
               IF( NAME.EQ.WFIL(I) ) THEN 
                  HERR=529 ! It is forbidden to read from an already allocated OUTPUT file.
                  GOTO 10
               ENDIF
            ENDDO
         ELSE
            HERR=530 ! It is forbidden to write into an already allocated INPUT file.
            WFIL(IWFIL)=' '
            IWFIL=IWFIL-1
         ENDIF
 10      LUN=NUM
      ENDIF

C Compose DDN
 15   CONTINUE
      IF(LUN.LT.10) THEN
         WRITE (CDDN2,'(''FT0'',I1,''F001'')') LUN
      ELSEIF(LUN.GE.10.AND.LUN.LT.100) THEN
         WRITE (CDDN2,'(''FT'',I2,''F001'')') LUN
      ELSE
         WRITE (CDDN2,'(''FT??F001'')')
      ENDIF
      DDN=DDN2
         
C Check for special DS names and open them as formatted files
C ( default is unformatted )
      IF( INDEX(NAME,'.HELP.').NE.0 .OR. INDEX(NAME,'.MACROS').NE.0 ) 
     + THEN
         CFMT='FORMATTED'
      ELSE
         CFMT='UNFORMATTED'
      ENDIF

C Output files do not need to exist
      IF( LWRIT ) THEN
         CSTAT='UNKNOWN'
      ELSE
         CSTAT='OLD'
      ENDIF

C Open file
      IF(HERR.EQ.0) THEN
         OPEN(FILE=NAME,UNIT=LUN,IOSTAT=IERR,STATUS=CSTAT,FORM=CFMT)
         IF(IERR.NE.0) HERR=ABS(IERR)+5888
      ENDIF
C
 999  CONTINUE
      WRITE(*,'(''GETPDD: FILE = '',A44)') NAME
      WRITE(*,'(T9,''DDN  ='',A9)') DDN
      WRITE(*,'(T9,''ERR  ='',I5)') HERR
      IF(HERR.EQ.0) THEN
         WRITE(*,'(T9,''ALLOCATION SUCCESFULL'')')
      ELSE
         IF( HERR.EQ.1040) THEN
            WRITE(*,'(T9,''FILE ALREADY ALLOCATED.'')')
         ENDIF
         IF( HERR.GE.528.AND.HERR.LE.531 ) THEN
            WRITE(*,'(T9,''REQUESTED DS NOT AVAILABLE.'')')
         ENDIF
         IF(HERR .EQ. 529) THEN
            WRITE(*,'(T9,''READING AN ALREADY ALLOCATED OUTPUT FILE'
     +           //' NOT ALLOWED.'')')
            HERR=528
         ELSEIF(HERR .EQ. 530) THEN
            WRITE(*,'(T9,''WRITING INTO AN ALREADY ALLOCATED INPUT FILE'
     +           //' NOT ALLOWED.'')')
          HERR=528
         ELSEIF(HERR .EQ. 531) THEN
            WRITE(*,'(T9,''MAXIMUM NUMBER OF OUTPUT FILES REACHED!'')')
            HERR=528
         ENDIF
         WRITE(*,'(T9,''LIST OF ALREADY ALLOCATED DATASETS:'')')
         DO I=LUN0,LUN9
            CNAME=' '
            INQUIRE(UNIT=I,OPENED=LOCKED,NAME=CNAME)
            IF(LOCKED) WRITE(*,'(T12,''LUN='',I2,'' NAME='',A)') 
     +           I,CNAME
         ENDDO
      ENDIF
C     
      RETURN
      END

**************************************************
      SUBROUTINE FREEDD(DDN,HERR)
      IMPLICIT NONE
*
* FREEDD is a public IPS subroutine.
*
* FREEDD releases allocated datasets.
* Use simple CLOSE and INQUIRE statement to emulate FREEDD.
* 
* Still usefull error code used here:
* HERR = 1080 : logical unit number is invalid
*               [DDNAME NOT FOUND]
*
      REAL*8 DDN,DDN2
      CHARACTER CDDN2*8,CNAME*44
      EQUIVALENCE(DDN2,CDDN2)
      INTEGER*2 HERR
      INTEGER LUN,IERR
      LOGICAL LOCKED
C
      INTEGER LUN0,LUN9,I
      PARAMETER(LUN0=11,LUN9=60)
Code:
      HERR=0
C Extract logical unit number from DDN
      DDN2=DDN
      READ(CDDN2,'(2X,I2)') LUN
C Check if logical unit number is valid
      IF( LUN.LT.LUN0 .OR. LUN.GT.LUN9 ) HERR=1080
C
C Verify that file is really allocated. If so, free this file.
      CNAME=' '
      IF(HERR.EQ.0) THEN
         INQUIRE(LUN,OPENED=LOCKED,NAME=CNAME)
         IF(LOCKED) THEN
            CLOSE(LUN,IOSTAT=IERR)
            HERR=IERR
         ELSE
            WRITE(*,FMT=20) DDN
 20         FORMAT('FREEDD: THERE IS NO ALLOCATED FILE'
     +           ,' CORRESPONDING TO DDN=',A8)
            HERR=1080
            CNAME='??'
         ENDIF
      ENDIF
C
 999  CONTINUE
      WRITE(*,'(''FREEDD: FILE = '',A)') CNAME
      WRITE(*,'(T9,''DDN  ='',A9)') DDN
      IF(HERR.EQ.0) THEN
         WRITE(*,'(T9,''RELEASE SUCCESFULL'')')
      ELSE
         WRITE(*,'(T9,''RELEASE SOMEHOW WRONG!!!'')')
         WRITE(*,'(T9,''LIST OF ALLOCATED DATASETS:'')')
         DO I=LUN0,LUN9
            CNAME=' '
            INQUIRE(UNIT=I,OPENED=LOCKED,NAME=CNAME)
            IF(LOCKED) WRITE(*,'(T12,''LUN='',I2,'' NAME='',A)') I,CNAME
         ENDDO
      ENDIF
C
      RETURN
      END

*****************************************************
      SUBROUTINE DDCHA( DDN, NAME, HERR, HVOL )
      IMPLICIT NONE
*
* Don't know where DDCHA comes from.
*
* Here, DCCHA asks for a dataset name and allocates
* the dataset. This emulation uses simply GETPDD.
* 
*
      REAL*8 DDN,DDN2,VOLSR
      CHARACTER CDDN2*8,NAME*44,C*44
      EQUIVALENCE(DDN2,CDDN2)
      INTEGER*2 HERR,HVOL(3)
      INTEGER LUN,INDEX
Code:
C Extract logical unit number from DDN
      DDN2=DDN
      READ(CDDN2,'(2X,I2)') LUN
C Read dataset name
      CALL VBLANK(C,11)
 10   WRITE(*,'(''ENTER DATASET NAME: '',$)')
      READ(*,'(A44)') C
      IF( INDEX(C,' ').EQ.1 ) GOTO 10 
C Allocate dataset
      NAME=C
      CALL GETPDD(NAME,VOLSR,DDN,LUN,HERR)
C
      RETURN
      END
**
