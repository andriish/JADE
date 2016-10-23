C   18/10/82 606101312  MEMBER NAME  MCMAIN9  (S)           FORTRAN
      PROGRAM MCMAIN
      IMPLICIT NONE
************************************************************************
*
*     Main program for the standard JADE tracking simulation
*     without muons.
*
*     This version: ??/10/99 Pedro Movilla Fernandez
*                   28/05/00 PMF: introduce FFREAD for I/O steering
*                   21/12/00 PMF: use CERNLIB package CFIO to read CPROD file
*                                 with other endian format
*
************************************************************************
C
C MCJADE parameters
*------------------------------------------------------------------------
C /TODAY/ HOLDS A DATE TO SPECIFY VERSION OF JADE DETECTOR IN MC
C                         TRACKING JOBS.
C
C       HDATE(1) GIVES SECONDS, HDATE(2) GIVES MINUTES
C       HDATE(3) GIVES HOURS,   HDATE(4) GIVES DAYS
C       HDATE(5) GIVES MONTHS,  HDATE(6) GIVES YEARS
C               (17.5 IS THE NORWEGIAN NATIONAL DAY...)
      INTEGER*2 HDATE
      COMMON /TODAY/ HDATE(6)
*------------------------------------------------------------------------
C                            FLAGS USED (MAINLY) FOR INPUT
C                            ERROR CHECKING IN THE MONTE CARLO
      LOGICAL*4 VTEST
      COMMON /CVFLAG/ VTEST(20)
*------------------------------------------------------------------------
C
C        LFLAG(1) = SMEAR GAMMA AND ELECTRON ENERGIES
C        LFLAG(2) = GAMMA CONVERSION IN OUTER TANK AND COIL (TRKGAM)
C        LFLAG(3) = ABSORPTION LOSSES
C        LFLAG(4) = 3 DIM SHOWER PROFILE FIT TO EGGS CODE
C        LFLAG(5) = .TRUE.   -->  WITH VERTEX CHAMBER TRACKING
C                 = .FALSE.  -->  WITHOUT VERTEX CHAMBER TRACKING
C                                 BUT OLD BEAM PIPE GEOMETRY AND
C                                 BEAM PIPE COUNTERS (BEFORE MAI 84)
      LOGICAL*1 LFLAG
      COMMON/CFLAG/LFLAG(10)
*------------------------------------------------------------------------
C    ALLOWS SECTIONS OF THE INPUT FILE TO BE TRACKED.
C    MINIMUM AND MAXIMUM NR OF EVENT SET IN SUBR. JGETEV
C    EVENT COUNT PASSED VIA /CIEVS/ KIEV,IEVMIN,IEVMAX
      INTEGER KIEV,IEVMIN,IEVMAX
      COMMON /CIEVS/ KIEV,IEVMIN,IEVMAX
*------------------------------------------------------------------------
C
C BOS
      INTEGER IW
      COMMON / BCS / IW(33000)
C
C PAW
      INTEGER NWPAW,ICYCLE,HMEMOR
      PARAMETER (NWPAW=2000000)
      COMMON / PAWC / HMEMOR(NWPAW)
C
C FFREAD
      REAL SPACE
      INTEGER FFSPAC
      PARAMETER (FFSPAC=20000)
      COMMON / CFREAD / SPACE(FFSPAC)
C
C User I/O
C     JADE convention: LUN=2: Event input  (cprod files)
C                         =3: Event output (bos files)
      INTEGER IOVECT,IOBOS,IOBANK
      LOGICAL LBANK,LHIST,LSTART,ENDIAN
      COMMON /MYIO/ IOVECT,IOBOS,IOBANK,LBANK,LHIST,LSTART
      EXTERNAL CMYIO
C
C Endian format of CPROD file
      INTEGER IFORM
      COMMON /CPFORM/ IFORM
C
C Misc
      INTEGER NAMLEN,NBYTEW,NWNAM,IOS
      PARAMETER( NAMLEN=120, NBYTEW=4, NWNAM=NAMLEN/NBYTEW )
      INTEGER  HISTF(NWNAM),VECTF(NWNAM),BANKF(NWNAM),BOSF(NWNAM)
      CHARACTER*(NAMLEN) CHISTF,CVECTF,CBANKF,CBOSF
      LOGICAL JFLAG(10),LEXIST
      INTEGER IYEAR,IMONTH,IDAY,NPRI,NEVT
      CHARACTER CSEEDF*11
      DATA CSEEDF / 'mcjade.stat' /
      INTEGER I,LENOCC,IERR
      COMMON /MYIO/ NEVT
C
*------------------------------------------------------------------------
C
C... Initialise BOS/HBOOK/FFREAD
      CALL BINT( 33000, 10000, 500, 0 )
      CALL HLIMIT(NWPAW)
      CALL FFINIT(0)
      CALL FFSET('SIZE',8)
C
C... Read FFREAD cards  
      WRITE(*,'(A)') 'MCMAIN: read FFREAD cards'
C Random number generator
      LSTART=.TRUE.
      CALL FFKEY('MCSTART',LSTART,1,'INTE')
C JADE configuration
      IYEAR=85
      CALL FFKEY('YEAR',IYEAR,1,'INTE')
      IMONTH=6
      CALL FFKEY('MONTH',IMONTH,1,'INTE')
      IDAY=15
      CALL FFKEY('DAY',IDAY,1,'INTE')
C MC tracking mode 
C     Changes from defaults in block data JADEBD:
C     - Perform gamma conversion in outer tank and coil
C     - Perform 3 dim shower profile fit to Eggs code
      JFLAG(2)=.TRUE.
      JFLAG(4)=.TRUE.
      CALL FFKEY('SMEAR',JFLAG(1),1,'LOGI')
      CALL FFKEY('CONVERT',JFLAG(2),1,'LOGI')
      CALL FFKEY('ABSORB',JFLAG(3),1,'LOGI')
      CALL FFKEY('SHOW3D',JFLAG(4),1,'LOGI')
      CALL FFKEY('VERTEX',JFLAG(5),1,'LOGI')
C Number of events
      NPRI=20
      CALL FFKEY('MCNPRI',NPRI,1,'INTE')
      IEVMIN=1
      CALL FFKEY('FIRST',IEVMIN,1,'INTE')
      IEVMAX=0
      CALL FFKEY('LAST',IEVMAX,1,'INTE')
C I/O options
      LBANK=.FALSE.
      CALL FFKEY('BANKS',LBANK,1,'LOGI')
      LHIST=.FALSE.
      CALL FFKEY('HISTO',LHIST,1,'LOGI')
C File names
      CALL VBLANK(HISTF,NWNAM)
      CALL FFKEY('MCHIST',HISTF,NWNAM,'MIXED')
      CALL VBLANK(VECTF,NWNAM)
      CALL FFKEY('MCVECT',VECTF,NWNAM,'MIXED')
      CALL VBLANK(BANKF,NWNAM)
      CALL FFKEY('MCBANK',BANKF,NWNAM,'MIXED')
      CALL VBLANK(BOSF,NWNAM)
      CALL FFKEY('MCBOS',BOSF,NWNAM,'MIXED')
      CALl FFGO
C... Convert hollerith to character
      CALL UHTOC(VECTF,NBYTEW,CVECTF,NAMLEN)
      CALL UHTOC(BOSF,NBYTEW,CBOSF,NAMLEN)
      CALL UHTOC(BANKF,NBYTEW,CBANKF,NAMLEN)
      CALL UHTOC(HISTF,NBYTEW,CHISTF,NAMLEN)
C
C... Output file with tracked events in binary BOS format
      OPEN(UNIT=IOBOS,FILE=CBOSF,STATUS='UNKNOWN',FORM='UNFORMATTED')
C
C... Output file with BOS bank contents in readable ASCII format
      IF(LBANK)
     +     OPEN(IOBANK,FILE=CBANKF,STATUS='UNKNOWN',FORM='FORMATTED')
C
C... Input file with 4-vector data in binary CPROD format
C    Check endian format of CPROD data 
      IF( .NOT.ENDIAN(CVECTF) ) THEN 
         IFORM=-1
      ELSE
         IFORM=1
      ENDIF
C    Open CPROD file
      IF( IFORM.GE.0 ) THEN
       OPEN(UNIT=IOVECT,FILE=CVECTF,STATUS='UNKNOWN',FORM='UNFORMATTED')
      ELSE
       CALL CFOPEN(IOVECT,0,1,'r   ',0,CVECTF,IOS )
      ENDIF
C
C...  MC tracking settings
C --- Check validity of input 4-vectors
C Disable MCVALI checks # 6 & 7 (production vertex checks)
      VTEST(6) = .FALSE.
      VTEST(7) = .FALSE.
C --- Specify version of the JADE detector in common /TODAY/
      HDATE(1)=1
      HDATE(2)=1
      HDATE(3)=1
      HDATE(4)=IDAY
      HDATE(5)=IMONTH
      HDATE(6)=MOD(IYEAR,100)+1900
C --- Tracking mode: shuffle FFREAD input into INTEGER*1 array
      DO I=1,5
         LFLAG(I)=JFLAG(I)
      ENDDO
C --- Maximal number of events to be tracked
      IEVMAX=MAX(0,IEVMAX)
      NEVT=IEVMAX
      IF( IEVMAX.EQ.0 ) IEVMAX=999999
      IEVMIN=MAX(0,IEVMIN)
      IEVMAX=MAX(IEVMIN,IEVMAX)
      IF( NEVT.NE.0 ) NEVT=IEVMAX-IEVMIN+1
C
C... Initialize random number generator
      WRITE(*,FMT='(/,120(''=''))')
      IF( .NOT. LSTART ) CALL MCRAND('R',CSEEDF,0,0,IERR)
      IF( IERR.LT.0 ) GOTO 999
C
C... Book control histograms
      IF(LHIST) CALL MCBOOK    
C
C... Print out some tracking parameters:
      WRITE(*,'(/,A,/,28(''-''))') 'MCMAIN: tracking parameters:'
      WRITE(*,'(A28,A)') 'Input 4-vector file is: '
     >     ,CVECTF(1:LENOCC(CVECTF))
      INQUIRE(FILE=CVECTF,EXIST=LEXIST)
      IF( .NOT. LEXIST ) THEN
         WRITE(*,'(A)') 'MCMAIN: Error: Input file does not exist!'
         WRITE(*,'(A)') '...will stop now!'
         GOTO 999
      ENDIF
      WRITE(*,'(A28,I8)') 'First event to be tracked: ',IEVMIN
      WRITE(*,'(A28,I8)') 'Last event to be tracked: ',IEVMAX
      WRITE(*,'(A28,A,5I3,I5)') 'JADE configuration date: '
     >     ,'HDATE(1..6) =',(HDATE(I),I=1,6)
      WRITE(*,'(A28,A,20L2)') 'MCVALI validation flags: '
     >     ,'VTEST(1..20) =',(VTEST(I),I=1,20)
      WRITE(*,'(A28,A,6L2)') 'MCJADE tracking options: '
     >     ,'LFLAG(1..6) =',(LFLAG(I),I=1,6)
C
C... MC tracking 
      WRITE(*,'(/,A,/,22(''-''))') 'MCMAIN: calling MCJADE:'
      CALL MCJADE( NEVT, NPRI )
C
C... Print out BOS statistics
      CALL BSTA
      CALL PALL
C     
C... Close files
      CLOSE(UNIT=IOBOS)
      IF( IFORM.GE.0 ) THEN
         CLOSE(UNIT=IOVECT)
      ELSE
         CALL CFCLOS(IOVECT,0)
      ENDIF
      IF(LBANK) CLOSE(UNIT=IOBANK) 
C
C... Write out current randon number status
      WRITE(*,FMT='(/,120(''=''))')
      CALL MCRAND('W',CSEEDF,IEVMIN,IEVMIN+KIEV-1,IERR)
C
C... Write out histograms
      IF(LHIST) THEN
         WRITE(*,'(/,A,/,27(''-''))') 'MCMAIN: Control histograms:'
         CALL MCEND
         CALL HCDIR('//PAWC',' ')
         CALL HRPUT(0,CHISTF(1:LENOCC(CHISTF)),'N')
      ENDIF
C
C... Some info
      WRITE(*,'(/,A,/,24(''-''))') 'MCMAIN: I/O information:'
      WRITE(*,'(A30,A)') 'Input 4-vector file was: '
     >     ,CVECTF(1:LENOCC(CVECTF))
      WRITE(*,'(A30,I8)') 'First tracked event was: ',IEVMIN
      WRITE(*,'(A30,I8)') 'Last tracked event was: ',IEVMIN+KIEV-1
      WRITE(*,'(A30,A)') 'Output BOS banks to: '
     >     ,CBOSF(1:LENOCC(CBOSF))
      IF( LHIST ) WRITE(*,'(A30,A)') 'Output histos to: '
     >     ,CHISTF(1:LENOCC(CHISTF))
      IF( LBANK ) WRITE(*,'(A30,A)') 
     >     'Output readable bank contents to: '
     >     ,CBANKF(1:LENOCC(CBANKF))
C
C... The End
 999  STOP
      END
C
      BLOCKDATA CMYIO
      IMPLICIT NONE
      INTEGER IOVECT,IOBOS,IOBANK
      LOGICAL LBANK,LHIST,LSTART
      COMMON /MYIO/ IOVECT,IOBOS,IOBANK,LBANK,LHIST,LSTART,NEVT
      INTEGER NEVT
C
      DATA IOVECT/3/,IOBOS/2/,IOBANK/42/
      DATA LBANK/.FALSE./,LHIST/.TRUE./,LSTART/.TRUE./
C
      END
C
      LOGICAL FUNCTION ENDIAN(CNAM)
      IMPLICIT NONE
*.***************************************************************
*.
*.  21/12/00   P.A. Movilla Fernandez
*.
*.  This routine checks if the endian format of current platform
*.  matches the endian format of the binary CPROD input data.
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
*.      7 = event flavour
*.      8 = number of 'all' particles
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
      INTEGER IUIN,IOS,N(8),LIM,NREAD
      DATA LIM/65535/
C Test input file
      CALL CFOPEN(IUIN,0,1,'r   ',0,CNAM,IOS )
      IF( IOS.NE.0 ) THEN
         WRITE(*,'(2A)') ' ENDIAN: Error while opening file ',CNAM
         WRITE(*,'(A)') ' ...will stop now!'
         STOP
      ENDIF
C Read first words of the record
      NREAD=8
      CALL CFGET(IUIN,0,NREAD,NREAD,N,IOS)
C Check words 1,7,8
      IF(  N(1).LE.LIM.AND.N(7).LE.LIM.AND.N(8).LE.LIM ) THEN
         ENDIAN=.TRUE.
      ELSEIF( .NOT.( N(1).LE.LIM.AND.N(7).LE.LIM.AND.N(8).LE.LIM) ) THEN
         ENDIAN=.FALSE.
         CALL VXINVB(N,8)
      ELSE
         WRITE(*,'(3A)') ' ENDIAN: Cannot decide endian format'
     >        ,' of input data file ',CNAM
         WRITE(*,'(A,2I8,A5,I8)') ' Record starts with:',N
         WRITE(*,'(A)') ' ...will stop now!'
         STOP
      ENDIF
C I/O format info
      WRITE(*,'(/,A,/,A,2I8,4F7.3,2I8)') 'ENDIAN:'
     >     ,' Data record starts with:',N
      WRITE(*,'(A,$)') ' JADE data I/O format: '
      IF( ENDIAN ) THEN
         WRITE(*,'(A,$)') 'CPROD data and current platform with'
     >  //' EQUAL endian format.'
      ELSE
         WRITE(*,'(A,$)') 'CPROD data and current platform with'
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
