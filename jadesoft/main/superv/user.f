C   14/12/83 401111710  MEMBER NAME  USER     (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE USER(INDX)
C-----------------------------------------------------------------------
C
C   AUTHOR:   L. O'NEILL   ?/??/78 :  USER ROUTINE CALLED BY SUPERV
C
C        MOD: C. BOWDERY  14/12/83 :  TO IMPROVE THE COMMENTING
C   LAST MOD: C. BOWDERY   9/01/84 :  TO REMOVE EXCESS CODE
C
C   LAST MOD:
C   P. Movilla Fernandez  Oct./98 :  Adapted for use on new platforms
C                                    Reorganize I/O handling
C                                    Introduce calls to analysis routines
C   Last mod:        PMF 28/05/00 :  Use FFREAD for I/O steering
C                    PMF 31/05/00 :  Introduce INDX=13 on return
C                                    Introduce INDX=10, 20
C
C        USER ROUTINE FOR CUTS AND INTERACTIVE DECISION MAKING.
C
C-----------------------------------------------------------------------
C
C        INDEX IS PASSED FROM SUPERV TO INDICATE WHICH LEVEL HAS BEEN
C        COMPLETED. THE MEANING OF THE VALUES IS EXPLAINED BELOW.
C
C         INDX=0   INITIAL CALL, BEFORE FIRST EVENT READ.
C              1   CALLED AT THE BEGINNING OF EACH NEW RUN.
C              2   CALLED IMMEDIATELY AFTER EVENT IS READ INTO CDATA.
C              3   LEAD GLASS ENERGIES HAVE BEEN COMPUTED.
C              4   FAST Z VERTEX RECONSTRUCTION HAS BEEN DONE.
C              5   INNER DETECTOR PATTERN RECOGNITION HAS BEEN RUN.
C              6   ENERGIES CLUSTERS IN THE LEAD GLASS HAVE BEEN FOUND.
C              7   TRACKS AND CLUSTERS HAVE BEEN ASSOCIATED.
C              8   UNUSED
C              9   MUON ANALYSIS HAS BEEN DONE
C    PMF:     10   If last event is processed, set INDEX=13 on return.
C    PMF:     20   Called immediately after event is read, but
C                  BEFORE inner detector smearing is done.
C
C            100   END OF JOB CALL
C
C   ON RETURN, IF INDX =   1 : THE SUPERVISOR WILL DROP CURRENT EVENT
C                              AND READ THE NEXT ONE.
C                         11 : EVENT WILL BE WRITTEN OUT AND NEW EVENT
C                              WILL BE READ
C                         12 : JOB WILL BE TERMINATED NORMALLY
C
C    PMF:                 13 : Current event will be written out
C                              and the job will be terminated normally.
C
C                              This additional index makes it easier for MC data
C                              to ensure a correct bookkeeping of the random number
C                              seed corresponding to the LAST EVENT WRITTEN OUT.
C                              This is in particular important if the job
C                              is terminated BEFORE the end of the MC input file.
C
C   OTHERWISE THE SUPERVISOR WILL GO TO THE LEVEL GIVEN BY "INDX" ON
C   RETURN FROM "USER". THUS, TO CONTINUE WITH THE ANALYSIS, THIS
C   ROUTINE MUST INCREMENT "INDX" BEFORE PASSING CONTROL BACK.
C
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
C
C I/O default parameters set in JADEBD:
C     IUNIT:  Event input
C     JUNIT:  Event output
C     NCALI:  Max no. of calibration files
C     KUNITA: ASTART calibration files (dead)
C     LUNITA: AUPDAT/BUPDAT calibration files
C     ( IUNIT=2 JUNIT=3 LUNITA(1)=21 LUNITA(2)=22 )
      INTEGER IUNIT,JUNIT,NCALI,KUNITA,LUNITA
      COMMON/CIOUNI/IUNIT,JUNIT,NCALI,KUNITA(10),LUNITA(10)
C Flags to read the calibration constants in klread
      INTEGER LBMC
      COMMON /CMCCAL/ LBMC(17)
C MC smearing veto flag when reading MC events in subroutine EVREAD
C NOSMEAR = .TRUE. : suppress MC smearing
C NOSMEAR = .FALSE.: allow MC smearing
      LOGICAL NOSMEAR
      COMMON /CMCSMEAR/ NOSMEAR
C BOS
      INTEGER IW
      INTEGER*2 HW
      REAL AW
      COMMON /BCS/ IW(60000)
      DIMENSION HW(120000),AW(60000)
      EQUIVALENCE (HW(1),IW(1),AW(1))
C PAW
      INTEGER NWPAW,ICYCLE,HMEMOR
      PARAMETER (NWPAW=5000000)
      COMMON /PAWC/HMEMOR(NWPAW)
C FFREAD
      REAL SPACE
      INTEGER FFSPAC
      PARAMETER (FFSPAC=20000)
      COMMON / CFREAD / SPACE(FFSPAC)
C
C USER Parameters
C - I/O options
      INTEGER IOHIST,IOBANK
      LOGICAL LBANK,LHIST
      DATA IOBANK/41/, IOHIST/42/
C - Misc
      INTEGER NAMLEN,NBYTEW,NWNAM
      PARAMETER( NAMLEN=120, NBYTEW=4, NWNAM=NAMLEN/NBYTEW )
      INTEGER  HISTF(NWNAM),MCBOSF(NWNAM),SVBOSF(NWNAM),BANKF(NWNAM)
     >     ,CALF(NWNAM,3)
      CHARACTER*(NAMLEN) CHISTF,CMCBOSF,CSVBOSF,CBANKF,CCALF(3),CCALFU
      LOGICAL LEXIST,LEX(3),LLBMC(17),LZSR,LVTX,LSTART
      CHARACTER CSEEDF*11
      DATA CSEEDF / 'superv.stat' /
      INTEGER INDEX,LENOCC,IERR
      DATA IERR/0/
C - Specify bank contents to print for each supervisor level if LBANK .EQ. .TRUE.
C   (HEAD ALGN JETC ZVTX PATR JHTL LGCL LATC ATOF VECT PALL HITL VTXC HTSL TRIG)
      INTEGER HBANK(NWNAM,7)
      CHARACTER*(NAMLEN) CBANK(7)
C     DATA CBANK(1) /'HEAD LATC ATOF ALGN JETC PATR'/
C     DATA CBANK(2) /'HEAD'/
C     DATA CBANK(2) /'LATC ATOF ALGN JETC PATR HTSL JHTL'/
C     DATA CBANK(3) /'ALGN'/, CBANK(4) /'ZVTX'/, CBANK(5) /'PATR'/
C     DATA CBANK(6) /'LGCL'/, CBANK(7) /'PATR LGCL'/
C     DATA CBANK(7) /'HEAD ALGN JETC ZVTX PATR JHTL LGCL LATC ATOF VECT
C     +     PALL HITL VTXC HTSL TRIG TR4V'/
C - Pointer, counters etc.
      INTEGER IHEAD,IBLN
      INTEGER INDX
      INTEGER IEVT,IEVTF,IEVTL,NEVT,ICOUNT
      INTEGER I,J,K,L,M,N,ISGN
      INTEGER*2 H
      DATA ICOUNT /0/
      SAVE IEVT,IEVTF,IEVTL,NEVT,IOBANK,IOHIST,LEX,LZSR,LVTX,LSTART
      SAVE ICOUNT
      SAVE CBANK
C
C------------------  C O D E  ------------------------------------------
C
C        CHECK WHETHER CALL AT END OF JOB
C
      IF(INDX.EQ.100) GOTO 11000
C
      IF(INDX.LT.0) THEN 
         INDX=-INDX
         ISGN=1
      ELSE
         ISGN=0
      ENDIF
      IF(INDX.EQ.20) GOTO 20000
      GOTO (1000,2000,3000,4000,5000,6000,7000,8000,9000,10000),INDX
C
      IF( INDX.NE.0 ) THEN
         WRITE(*,'(A,I4,A)') 'USER: Illegal index, INDX='
     >        ,INDX,'. Will stop now!'
         INDX=12
         GOTO 999
      ENDIF
C
C--------------------------
C  INDEX=0: INITIALIZATION
C--------------------------
      PRINT *,' '
C... Initialise HBOOK and FFREAD
      CALL HLIMIT(NWPAW)
      CALL FFINIT(0)
      CALL FFSET('SIZE',8)
C... Read FFREAD cards  
      WRITE(*,'(A)') 'USER: read FFREAD cards'
C Random number generator
      LSTART=.TRUE.
      CALL FFKEY('SVSTART',LSTART,1,'INTE')
C First and last event to be processed
      IEVTF = 1
      CALL FFKEY('FIRST',IEVTF,1,'INTE')
      IEVTL = 999999
      CALL FFKEY('LAST',IEVTL,1,'INTE')
C Supervisor reconstruction options
C     - Perform zs refit
      LZSR=.FALSE.
      CALL FFKEY('ZSRFTV',LZSR,1,'LOGI')
C     - Perform vertex fit
      LVTX=.FALSE.
      CALL FFKEY('VTXFIT',LVTX,1,'LOGI')
C Calibrations constants reading options
      CALL FFKEY('MUCA',LLBMC(1),1,'LOGI')
      CALL FFKEY('LGMA',LLBMC(2),1,'LOGI')
      CALL FFKEY('TAGS',LLBMC(3),1,'LOGI')
      CALL FFKEY('TOFC',LLBMC(4),1,'LOGI')
      CALL FFKEY('LGST',LLBMC(5),1,'LOGI')
      CALL FFKEY('DEDX',LLBMC(6),1,'LOGI')
      CALL FFKEY('SPTG',LLBMC(7),1,'LOGI')
      CALL FFKEY('RVTX',LLBMC(8),1,'LOGI')
      CALL FFKEY('ZCAL',LLBMC(9),1,'LOGI')
      CALL FFKEY('TAGF',LLBMC(10),1,'LOGI')
      CALL FFKEY('IDJS',LLBMC(11),1,'LOGI')
      CALL FFKEY('VTXC',LLBMC(12),1,'LOGI')
      CALL FFKEY('VTXR',LLBMC(13),1,'LOGI')
      CALL FFKEY('VTXB',LLBMC(14),1,'LOGI')
      CALL FFKEY('VTXF',LLBMC(15),1,'LOGI')
C I/O options
      LBANK=.FALSE.
      CALL FFKEY('BANKS',LBANK,1,'LOGI')
      CALL VBLANK(HBANK(1,1),NWNAM*7)
      CALL FFKEY('LEVEL1',HBANK(1,1),NWNAM,'MIXED')
      CALL FFKEY('LEVEL2',HBANK(1,2),NWNAM,'MIXED')
      CALL FFKEY('LEVEL3',HBANK(1,3),NWNAM,'MIXED')
      CALL FFKEY('LEVEL4',HBANK(1,4),NWNAM,'MIXED')
      CALL FFKEY('LEVEL5',HBANK(1,5),NWNAM,'MIXED')
      CALL FFKEY('LEVEL6',HBANK(1,6),NWNAM,'MIXED')
      CALL FFKEY('LEVEL7',HBANK(1,7),NWNAM,'MIXED')
      LHIST=.TRUE.
      CALL FFKEY('HISTO',LHIST,1,'LOGI')
C File names
      CALL VBLANK(CALF(1,1),NWNAM)
      CALL FFKEY('AUPDAT0',CALF(1,1),NWNAM,'MIXED')
      CALL VBLANK(CALF(1,2),NWNAM)
      CALL FFKEY('BUPDAT0',CALF(1,2),NWNAM,'MIXED')
      CALL VBLANK(CALF(1,3),NWNAM)
      CALL FFKEY('BUPDAT1',CALF(1,3),NWNAM,'MIXED')
      CALL VBLANK(HISTF,NWNAM)
      CALL FFKEY('SVHIST',HISTF,NWNAM,'MIXED')
      CALL VBLANK(MCBOSF,NWNAM)
      CALL FFKEY('MCBOS',MCBOSF,NWNAM,'MIXED')
      CALL VBLANK(BANKF,NWNAM)
      CALL FFKEY('SVBANK',BANKF,NWNAM,'MIXED')
      CALL VBLANK(SVBOSF,NWNAM)
      CALL FFKEY('SVBOS',SVBOSF,NWNAM,'MIXED')
      CALl FFGO
C... Convert hollerith to character
      CALL UHTOC(CALF,NBYTEW,CCALF,NAMLEN*3)
      CALL UHTOC(HBANK,NBYTEW,CBANK,NAMLEN*7)
      CALL UHTOC(MCBOSF,NBYTEW,CMCBOSF,NAMLEN)
      CALL UHTOC(SVBOSF,NBYTEW,CSVBOSF,NAMLEN)
      CALL UHTOC(BANKF,NBYTEW,CBANKF,NAMLEN)
      CALL UHTOC(HISTF,NBYTEW,CHISTF,NAMLEN)
C... Check some parameters
      IEVTL=MAX(IEVTL,0)
      IEVTF=MAX(IEVTF,1)
      IF( IEVTL.NE.0 ) THEN 
         IEVTL=MAX(IEVTF,IEVTL)
      ELSE
         IEVTL=9999999
      ENDIF
C... Set calibration constants reading options
      DO I=1,16
         IF( LLBMC(I) ) THEN
            LBMC(I)=1
         ELSE
            LBMC(I)=0
         ENDIF
      ENDDO
C... Input file with events in BOS format
      INQUIRE(FILE=CMCBOSF,EXIST=LEXIST)
      IF( LEXIST ) THEN
         OPEN(IUNIT,FILE=CMCBOSF,STATUS='UNKNOWN',FORM='UNFORMATTED')
      ELSE
         WRITE(*,'(A)') 'USER: Input BOS file does not exist!'
         WRITE(*,'(A)') '... will stop now!'
         INDX=12
         GOTO 999
      ENDIF
      OPEN(IUNIT,FILE=CMCBOSF,STATUS='OLD',FORM='UNFORMATTED')
C... Calibration files
      DO I=1,3
         CCALFU=CCALF(I)
         CALL CLTOU(CCALFU)
         IF( I.EQ.1 ) LEX(I)=(INDEX(CCALFU,'AUPDAT1').NE.0)
         IF( I.EQ.2 ) LEX(I)=(INDEX(CCALFU,'BUPDAT0').NE.0)
         IF( I.EQ.3 ) LEX(I)=(INDEX(CCALFU,'BUPDAT1').NE.0)
         IF( LEX(I) ) INQUIRE(FILE=CCALF(I),EXIST=LEX(I))
      ENDDO
C  - Get constants from AUPDAT1
      IF( LEX(1) ) THEN
      OPEN(LUNITA(2),FILE=CCALF(1),STATUS='UNKNOWN',FORM='UNFORMATTED')
      GOTO 100
      ENDIF
C  - Get constants from BUPDAT0 and/or BUPDAT1
      IF( LEX(2) )
     >     OPEN(LUNITA(1),FILE=CCALF(2),STATUS='OLD',FORM='UNFORMATTED')
      IF( LEX(3) )
     >     OPEN(LUNITA(2),FILE=CCALF(3),STATUS='OLD',FORM='UNFORMATTED')
      IF( .NOT.( LEX(2) .OR. LEX(3) ) ) THEN
         WRITE(*,'(A)') 'USER: No calibration files specified!'
         WRITE(*,'(A)') '... will stop now!'
         INDX=12
         GOTO 999
      ENDIF
C... SUPERV result BOS banks
 100  OPEN(JUNIT,FILE=CSVBOSF,STATUS='UNKNOWN',FORM='UNFORMATTED')
C... Output file with BOS bank contents in readable ASCII format
      IF(LBANK)
     >     OPEN(IOBANK,FILE=CBANKF,STATUS='UNKNOWN',FORM='FORMATTED')
C... Initialize random number generator
      WRITE(*,FMT='(/,120(''=''))')
      IF( .NOT. LSTART ) CALL MCRAND('R',CSEEDF,0,0,IERR)
      IF( IERR.LT.0 ) THEN
         INDX=12 
         GOTO 999
      ENDIF
C... Book control histograms
      IF( LHIST ) THEN 
         CALL HCDIR('//PAWC',' ')
         CALL HSTAF('YES')
         CALL HSTDEF
      ENDIF
C... Print out some parameters:
      WRITE(*,'(/,A,/,32(''-''))') 'USER: reconstruction parameters:'
      WRITE(*,'(A40,A)') 'Input BOS file is: '
     >     ,CMCBOSF(1:LENOCC(CMCBOSF))
      WRITE(*,'(A40,I8)') 'First event: ',IEVTF
      WRITE(*,'(A40,I8)') 'Last event: ',IEVTL
      WRITE(*,'(A40,$)') 'Perform ZSRFTV fit: ' 
      IF( LZSR ) THEN
         WRITE(*,'(A8)') 'YES' 
      ELSE
         WRITE(*,'(A8)') 'NO' 
      ENDIF
      WRITE(*,'(A40,$)') 'Perform Vertex fit with VTX*: ' 
      IF( LVTX ) THEN
         WRITE(*,'(A8)') 'YES' 
      ELSE
         WRITE(*,'(A8)') 'NO' 
      ENDIF
      WRITE(*,'(A40)') 'Calibration files used in this run: '
      DO I=1,3
         IF( LEX(I) ) WRITE(*,'(T42,A)') CCALF(I)(1:LENOCC(CCALF(I)))
      ENDDO
      WRITE(*,'(A40,A,20I2)') 'Calibration flags: '
     >     ,'/CMCCAL/ LBMC(1..17) =',(LBMC(I),I=1,17)
C
      WRITE(*,FMT='(/,120(''=''))')
C
      NEVT=0
      INDX=INDX+1
 999  RETURN
C
C------------------------------------------------------
C INDEX=100: END OF JOB: FINAL CALCULATIONS + PRINTOUT
C------------------------------------------------------
C
11000 CONTINUE
      IF( LBANK ) WRITE(IOBANK,11900) ICOUNT
11900 FORMAT(/'***'/'*** USER: INDEX=100  END OF JOB AFTER '
     +     ,I8,' EVENTS'/'***'/)
C BOS statistics
      CALL BSTA
C Close files
      CLOSE(IUNIT)
      CLOSE(JUNIT)
      CLOSE(LUNITA(1))
      CLOSE(LUNITA(2))
      IF( LBANK ) CLOSE(IOBANK)
      IF( LHIST ) CLOSE(IOHIST)
C... Save current random number generator status
      WRITE(*,FMT='(/,120(''=''))')
      CALL MCRAND('W',CSEEDF,IEVTF,IEVTF+NEVT-1,IERR)
C Save histograms
      IF( LHIST) THEN 
         WRITE(*,'(/,A,/,25(''-''))') 'USER: Control histograms:'
         CALL HSTEND(0)
         CALL HCDIR('//PAWC',' ')
         CALL HRPUT(0,CHISTF(1:LENOCC(CHISTF)),'N')
      ENDIF
C... Some info
      WRITE(*,'(/,A,/,22(''-''))') 'USER: I/O information:'
      WRITE(*,'(A35,A)') 'Input BOS file was: '
     >     ,CMCBOSF(1:LENOCC(CMCBOSF))
      WRITE(*,'(A35,I8)') 'First processed event was: ',IEVTF
      WRITE(*,'(A35,I8)') 'Last processed event was: ',IEVTF+NEVT-1
      WRITE(*,'(A35,$)') 'ZSRFTV fit performed: ' 
      IF( LZSR ) THEN
         WRITE(*,'(A8)') 'YES' 
      ELSE
         WRITE(*,'(A8)') 'NO' 
      ENDIF
      WRITE(*,'(A35,$)') 'Vertex fit with VTX* performed: ' 
      IF( LVTX ) THEN
         WRITE(*,'(A8)') 'YES' 
      ELSE
         WRITE(*,'(A8)') 'NO' 
      ENDIF
      WRITE(*,'(A35)') 'Calibration file used: '
      DO I=1,3
         IF( LEX(I) ) WRITE(*,'(T37,A)') CCALF(I)(1:LENOCC(CCALF(I)))
      ENDDO
      WRITE(*,'(A35,A)') 'Output BOS banks to: '
     >     ,CSVBOSF(1:LENOCC(CSVBOSF))
      IF( LHIST ) WRITE(*,'(A35,A)') 'Output histos to: '
     >     ,CHISTF(1:LENOCC(CHISTF))
      IF( LBANK ) THEN 
         WRITE(*,'(A35,A)') 
     >     'Output readable bank contents to: '
     >     ,CBANKF(1:LENOCC(CBANKF))
         WRITE(*,'(A35)') 'Printed BOS banks: '
         DO I=1,7
            WRITE(*,'(A31,I2,A1,2X,A)')
     >           '-Level',I,':',CBANK(I)(1:LENOCC(CBANK(I)))
         ENDDO
      ENDIF
C
      IF( NEVT.EQ. 0 ) WRITE(*,'(/,A)')
     >     'USER: WARNING!!! Number of first event to be reconstructed'
     >     //' exceeds number of events contained in the input file!'
C
      WRITE(*,FMT='(/,120(''=''))')
C
      RETURN
C
C---------------------------
C  INDEX=1: START OF RUN 
C---------------------------
C
 1000 CONTINUE
      IF( NEVT.EQ.0 )  WRITE(*,1900)
      IF( LBANK ) WRITE(IOBANK,1900)
 1900 FORMAT(/100('-')
     +     /'***'/'*** USER: INDEX=1  START OF NEW RUN'/'***'/)
      INDX=INDX+1
      RETURN
C
C----------------------------------------------------------------
C  INDEX=2: EVENT JUST READ IN, INNER DETECTOR SMEARING PERFORMED
C----------------------------------------------------------------
C
 2000 CONTINUE
C BOS Pointers
      IHEAD=IW(IBLN('HEAD'))
      IEVT=HW(IHEAD*2+11)
      ICOUNT=ICOUNT+1
C Skip first IEVTF-1 events
      IF( ICOUNT.LT.IEVTF ) GOTO 1
      IF( ICOUNT.EQ.IEVTF )
     +    WRITE(*,'(//''USER: First processed event is: '',I8,//)')
     +     IEVT
      NEVT=NEVT+1
C Log file
      IF( NEVT.EQ.1 )  WRITE(*,2900)
      IF( LBANK ) THEN
         WRITE(IOBANK,2900)
         WRITE(IOBANK,'(2X,A,6I8)') 'Date',(HW(IHEAD*2+K),K=3,8)
         WRITE(IOBANK,'(3(2x,A,I8))') 'Run',HW(IHEAD*2+10)
     +        ,' Event',HW(IHEAD*2+11),' Type:',HW(IHEAD*2+12) 
         WRITE(IOBANK,'(2X,A,I8)') 'Ebeam',HW(IHEAD*2+29)
         CALL SHOWB(IOBANK,CBANK(INDX))
      ENDIF
 2900 FORMAT(/'***'/'*** USER: INDEX=2  EVENT JUST READ IN'/'***'/)
C Histograming
      IF( LHIST ) CALL HSTFLL(INDX)
C
      IF( ISGN.EQ.0 ) INDX=INDX+1
      RETURN
C
C--------------------------------------------------
C  INDEX=3: LEAD GLASS ENERGIES HAVE BEEN COMPUTED
C--------------------------------------------------
C
 3000 CONTINUE
      IF( NEVT.EQ.1 )  WRITE(*,3900)
      IF( LBANK ) THEN
         WRITE(IOBANK,3900)
         CALL SHOWB(IOBANK,CBANK(INDX))
      ENDIF
 3900 FORMAT(/'***'/'*** USER: INDEX=3  LEAD GLASS ENERGIES'
     +     ,' HAVE BEEN COMPUTED'/'***'/)
      IF( LHIST ) CALL HSTFLL(INDX)
      INDX=INDX+1
      RETURN
C
C-----------------------------------
C  INDEX=4: Z-VERTEX NOW CALCULATED
C----------------------------------
C
 4000 CONTINUE
      IF( NEVT.EQ.1 )  WRITE(*,4900)
      IF( LBANK ) THEN
         WRITE(IOBANK,4900)
         CALL SHOWB(IOBANK,CBANK(INDX))
      ENDIF
 4900 FORMAT(/'***'/'*** USER: INDEX=4  Z-VERTEX NOW CALCULATED'
     +     /'***'/)
      IF( LHIST ) CALL HSTFLL(INDX)
      INDX=INDX+1
      RETURN
C
C-----------------------------------------------------------
C  INDEX=5: INNER DETECTOR PATTERN RECOGNITION NOW COMPLETE
C-----------------------------------------------------------
C
 5000 CONTINUE
      IF( NEVT.EQ.1 )  WRITE(*,5900)
      IF( LBANK ) THEN
         WRITE(IOBANK,5900)
         CALL SHOWB(IOBANK,CBANK(INDX))
      ENDIF
 5900 FORMAT(/'***'/'*** USER: INDEX=5 INNER DETECTOR PATTERN'
     +     ,' RECOGNITION NOW COMPLETE '/'***'/)
      IF( LHIST ) CALL HSTFLL(INDX)
      INDX=INDX+1
      RETURN

C
C     IF( LZSRFTV ) CALL ZSRFTV(1,2)
C
C     IF( LVERTEX ) THEN
C       IH=IBLN('HEAD')
C       IP=IBLN('PATR')
C       CALL VTXPRE(IH,IP) 
C       CALL VTXSRC
C       CALL VTXAFT
C       CALL VTXBNK(IP)
C     ENDIF
C
C
C--------------------------------------------------------------
C  INDEX=6: ENERGIES CLUSTERS IN THE LEAD GLASS HAVE BEEN FOUND
C--------------------------------------------------------------
C
 6000 CONTINUE
      IF( NEVT.EQ.1 )  WRITE(*,6900)
      IF( LBANK ) THEN
         WRITE(IOBANK,6900)
         CALL SHOWB(IOBANK,CBANK(INDX))
      ENDIF
 6900 FORMAT(/'***'/'*** USER: INDEX=6  ENERGIES CLUSTERS IN THE'
     +     ,' LEAD GLASS HAVE BEEN FOUND'/'***'/)
      IF( LHIST ) CALL HSTFLL(INDX)
      INDX=INDX+1
      RETURN

C
C----------------------------------------------------
C  INDEX=7: TRACKS AND CLUSTERS HAVE BEEN ASSOCIATED
C----------------------------------------------------
C
 7000 CONTINUE
      IF( NEVT.EQ.1 )  WRITE(*,7900)
      IF( LBANK ) THEN
         WRITE(IOBANK,7900)
         CALL SHOWB(IOBANK,CBANK(INDX))
      ENDIF
 7900 FORMAT(/'***'/'*** USER: INDEX=7  TRACKS AND CLUSTERS'
     +     ,' HAVE BEEN ASSOCIATED'/'***'/)
      IF( LHIST ) CALL HSTFLL(INDX)
      INDX=INDX+1
      RETURN
C
C------------------
C  INDEX=8: UNUSED
C------------------
C
 8000 CONTINUE
      IF( LHIST ) CALL HSTFLL(INDX) ! Histos combined from different levels
      GOTO 10                   ! Skip muon analysis
CCC   INDX=INDX+1
      RETURN
C
C---------------------------------------
C  INDEX=9: MUON ANALYSIS HAS BEEN DONE 
C---------------------------------------
C
 9000 CONTINUE
      IF( NEVT.EQ.1 )  WRITE(*,9900)
      IF( LBANK ) THEN
         WRITE(IOBANK,9900)
         CALL SHOWB(IOBANK,CBANK(INDX))
      ENDIF
 9900 FORMAT(/'***'/'*** USER: INDEX=9  MUON ANALYSIS'
     +     ,' HAS BEEN DONE'/'***'/)
      IF( LHIST ) CALL HSTFLL(INDX)
      INDX=INDX+1
      RETURN
C
C----------------------------------------------
C  INDEX=10: UNUSED (PMF: not any longer)
C  If the event limit is reached, write out the
C  current event and terminate the job normally.
C  For this, set INDX=13 on return.
C----------------------------------------------
C
10000 CONTINUE
      INDX=INDX+1
C Event counter
      IF( MOD(ICOUNT,50).EQ.0 )
     + WRITE(*,'(''USER: number of currently read/processed events:'''
     >     //',I8,''  /'',I8)') ICOUNT,NEVT
C End of job if event limit reached
      IF(ICOUNT.GE.IEVTL) THEN
       WRITE(*,'(//''USER:  Event limit reached after'',I8'
     +        //','' events'',//)')
     +        NEVT
         INDX=13
      ENDIF
      RETURN
C
C---------------------------------------------------------------------
C  INDEX=20: EVENT JUST READ IN, INNER DETECTOR SMEARING NOT PERFORMED
C---------------------------------------------------------------------
C
20000 CONTINUE
C Suppress smearing of MC until first event to process is read
      IF( ICOUNT.LT.IEVTF-1 ) THEN
         NOSMEAR=.TRUE.
      ELSE
         NOSMEAR=.FALSE.
      ENDIF
      RETURN
C---
C---     RETURNS FOR STEERING ANALYSIS TO DESIRED NEXT STEP.
C---     'GO TO 1' MEANS REJECT EVENT AND GO TO NEXT EVENT.
C---
    1 CONTINUE
      INDX=1
      GO TO 99999
    2 CONTINUE
      INDX=2
      GO TO 99999
    3 CONTINUE
      INDX=3
      GO TO 99999
    4 CONTINUE
      INDX=4
      GO TO 99999
    5 CONTINUE
      INDX=5
      GO TO 99999
    6 CONTINUE
      INDX=6
      GO TO 99999
    7 CONTINUE
      INDX=7
      GO TO 99999
    8 CONTINUE
      INDX=8
      GO TO 99999
    9 CONTINUE
      INDX=9
      GO TO 99999
   10 CONTINUE
      INDX=10
99999 RETURN
C
      END
C
