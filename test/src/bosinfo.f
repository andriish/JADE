      PROGRAM BOSINFO
C-----------------------------------------------------------------------
C Prints the run numbers, the numbers of events and the centre-of-mass
C energy of a BOS file with real data or Monte Carlo events (the file
C written by fptobos, mcjade, superv, ...).
C
C The run, the event number and the beam energy are in the bank HEAD
C (half-words 10, 11 and 29, the beam energy in MeV). The order of the two
C half-words of a word is the one of the machine that wrote the bank (real
C data: big-endian machines), so it is found from the beam energy, as in
C jtuple/src/jtevnt.F: if half-word 29 is not a plausible beam energy
C (500-30000 MeV), the neighbouring half-words are used.
C
C usage: bosinfo <BOS file>
C The byte order of the file is set for the Fortran runtime as for the
C other programs (e.g. GFORTRAN_CONVERT_UNIT='big_endian;native:2').
C Exit code 0 if at least one event with the bank HEAD was read.
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER MBOS, MRUN
      PARAMETER (MBOS=80000, MRUN=1000)
      INTEGER IW(MBOS)
      INTEGER*2 HW(MBOS*2)
      REAL RW(MBOS)
      COMMON /BCS/ IW
      EQUIVALENCE (HW(1),IW(1),RW(1))
      INTEGER IBLN
      INTEGER NRUN, IRUNS(MRUN), NEVR(MRUN), IMEV(MRUN)
      INTEGER IOS, NPHEAD, NEVT, IRUN, IEV, IBM, I, J, NREC
      CHARACTER*256 FNAME
      DOUBLE PRECISION ESUM

      CALL GET_COMMAND_ARGUMENT(1,FNAME)
      IF (FNAME.EQ.' ') THEN
        WRITE(*,*) 'usage: bosinfo <BOS file>'
        STOP 2
      ENDIF
      CALL BINT(MBOS,MBOS/2,MBOS/8,0)
      OPEN(2,FILE=FNAME,STATUS='OLD',FORM='UNFORMATTED',IOSTAT=IOS)
      IF (IOS.NE.0) THEN
        WRITE(*,*) 'bosinfo: cannot open ',FNAME(1:LEN_TRIM(FNAME))
        STOP 2
      ENDIF
      NRUN=0
      NEVT=0
      NREC=0
      ESUM=0.D0
   10 CONTINUE
      CALL BDLG
      CALL BREAD(2,*900,*900)
      NREC=NREC+1
      NPHEAD=IW(IBLN('HEAD'))
      IF (NPHEAD.LE.0) GO TO 10
      IF (HW(NPHEAD*2+29).GE.500 .AND. HW(NPHEAD*2+29).LE.30000) THEN
        IRUN=HW(NPHEAD*2+10)
        IEV=HW(NPHEAD*2+11)
        IBM=HW(NPHEAD*2+29)
      ELSE
        IRUN=HW(NPHEAD*2+9)
        IEV=HW(NPHEAD*2+12)
        IBM=HW(NPHEAD*2+30)
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
      CLOSE(2)
      WRITE(*,'(A,I8)') ' records read:           ',NREC
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
