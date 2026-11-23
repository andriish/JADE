      PROGRAM FPINFO
C-----------------------------------------------------------------------
C Prints the run numbers, the numbers of events and the centre-of-mass
C energy of an archived raw data file in the FPACK format (e.g. the
C files of the JADE tapes), reading the FPACK file directly with the
C unpacking routine UNPJAD of fptobos (no BOS file is needed).
C
C UNPJAD puts each event into the buffer /CEVENT/ and the pointers to
C its banks into /CBANK/ (KB). The run, the event number and the beam
C energy (MeV) are the half-words 10, 11 and 29 of the data of the bank
C HEAD. The order of the two half-words of a word is found from the beam
C energy (500-30000 MeV), as in jtuple/src/jtevnt.F: if half-word 29 is
C not a plausible beam energy, the neighbouring half-words are used.
C
C usage: fpinfo <FPACK file>
C Exit code 0 if at least one event was read.
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER MRUN
      PARAMETER (MRUN=1000)
      INTEGER IEVENT(525000)
      INTEGER*2 HEV(1050000)
      COMMON /CEVENT/ IEVENT
      EQUIVALENCE (IEVENT(1),HEV(1))
      INTEGER N2,KB(12000),KNOWNB,NAMEBA(500),NBFREQ(500),
     $        KFORM(8,500),NFORM(500),NBLMIN(500),NBLMAX(500),
     $        NBLAVE(500),NELMIN,NELMAX,NELAVE
      COMMON /CBANK/ N2,KB,KNOWNB,NAMEBA,NBFREQ,KFORM,NFORM,
     $               NBLMIN,NBLMAX,NBLAVE,NELMIN,NELMAX,NELAVE
      INTEGER KEYWD1,KEYWD2,KRUNNR,KRUNEV,IPZ
      COMMON /CUNPAF/ KEYWD1,KEYWD2,KRUNNR,KRUNEV
      COMMON /CBUGGY/ IPZ
      CHARACTER*4 CHASCI
      INTEGER NRUN, IRUNS(MRUN), NEVR(MRUN), IMEV(MRUN)
      INTEGER IOS, NEVT, IRUN, IBM, I, J, LB, IPOS, IUNPFL, IHEAD
      INTEGER NHEAD
      CHARACTER*256 FNAME
      DOUBLE PRECISION ESUM

      CALL GET_COMMAND_ARGUMENT(1,FNAME)
      IF (FNAME.EQ.' ') THEN
        WRITE(*,*) 'usage: fpinfo <FPACK file>'
        STOP 2
      ENDIF
      IPZ=0
      OPEN(UNIT=1,FILE=FNAME,STATUS='OLD',ACCESS='DIRECT',RECL=27720,
     $     FORM='UNFORMATTED',IOSTAT=IOS)
      IF (IOS.NE.0) THEN
        WRITE(*,*) 'fpinfo: cannot open ',FNAME(1:LEN_TRIM(FNAME))
        STOP 2
      ENDIF
      NRUN=0
      NEVT=0
      ESUM=0.D0
   10 CONTINUE
      CALL UNPJAD(IUNPFL,1,1000000,0,0,0)
      IF (IUNPFL.EQ.-1) GO TO 900
C Find the bank HEAD of the event
      IHEAD=0
      DO 15 LB=1,N2-1
        IPOS=KB(LB)
        IF (CHASCI(IEVENT(IPOS+1)).EQ.'HEAD') THEN
          IHEAD=IPOS+4
          GO TO 16
        ENDIF
   15 CONTINUE
   16 CONTINUE
      IF (IHEAD.EQ.0) GO TO 10
      IF (HEV(2*IHEAD+29).GE.500 .AND. HEV(2*IHEAD+29).LE.30000) THEN
        IRUN=HEV(2*IHEAD+10)
        IBM=HEV(2*IHEAD+29)
      ELSE
        IRUN=HEV(2*IHEAD+9)
        IBM=HEV(2*IHEAD+30)
      ENDIF
      NEVT=NEVT+1
      ESUM=ESUM+IBM
      DO 20 I=1,NRUN
        IF (IRUNS(I).EQ.IRUN) THEN
          NEVR(I)=NEVR(I)+1
          IMEV(I)=IMEV(I)+IBM
          GO TO 10
        ENDIF
   20 CONTINUE
      IF (NRUN.LT.MRUN) THEN
        NRUN=NRUN+1
        IRUNS(NRUN)=IRUN
        NEVR(NRUN)=1
        IMEV(NRUN)=IBM
      ENDIF
      GO TO 10
  900 CONTINUE
      CLOSE(1)
      WRITE(*,'(A,I8)') ' events with bank HEAD:  ',NEVT
      WRITE(*,'(A,I8)') ' number of runs:         ',NRUN
      WRITE(*,'(A)') '      run    events   Ebeam/GeV   E_cm/GeV'
      DO 30 J=1,NRUN
        WRITE(*,'(I9,I10,2F12.3)') IRUNS(J),NEVR(J),
     +    IMEV(J)/1000./NEVR(J),2.*IMEV(J)/1000./NEVR(J)
   30 CONTINUE
      IF (NEVT.GT.0) THEN
        WRITE(*,'(A,F8.3,A)') ' mean CM energy: ',
     +    2.*ESUM/1000./NEVT,' GeV'
      ELSE
        WRITE(*,*) 'FAIL: no event with the bank HEAD'
        STOP 1
      ENDIF
      END
