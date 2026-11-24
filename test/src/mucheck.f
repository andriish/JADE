      PROGRAM MUCHECK
C-----------------------------------------------------------------------
C Checks the muon chamber analysis (MUANA) in the BOS file of SUPERV: for
C each event the hits of the bank MUR1 and the muons found by the routine
C MUONS (the muon candidates selected by MUCUTS, types 1, 5 and 9) are
C counted. Used for the toy simulation of the muon chambers (the raw
C bank MUEV made by MUMCEV from the true muons, SUPERV run with the
C environment variable JADE_MUMC=1) and for the real data.
C
C usage: mucheck <BOS file of superv>
C Exit code 0 if there are events with hits in the bank MUR1.
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IW(80000), IBLN, NEV, NUM, IARR(2,10), IER, I, NR1
      INTEGER NHIST(0:10), NTYP(0:10), NSUMH, NEVHIT
      COMMON /BCS/ IW
      CHARACTER*256 F
      DO 1 I=0,10
        NHIST(I)=0
        NTYP(I)=0
    1 CONTINUE
      NSUMH=0
      NEVHIT=0
      CALL GET_COMMAND_ARGUMENT(1,F)
      IF (F.EQ.' ') THEN
        WRITE(*,*) 'usage: mucheck <BOS file of superv>'
        STOP 2
      ENDIF
      CALL BINT(80000,40000,10000,8000)
      OPEN(2,FILE=F,STATUS='OLD',FORM='UNFORMATTED',IOSTAT=I)
      IF (I.NE.0) THEN
        WRITE(*,*) 'mucheck: cannot open ',F(1:LEN_TRIM(F))
        STOP 2
      ENDIF
      NEV=0
   10 CALL BDLG
      CALL BREAD(2,*900,*900)
      IF (IW(IBLN('HEAD')).LE.0) GO TO 10
      NEV=NEV+1
      NR1=IW(IBLN('MUR1'))
      IF (NR1.GT.0) THEN
        NSUMH=NSUMH+IW(NR1+1)
        IF (IW(NR1+1).GT.0) NEVHIT=NEVHIT+1
      ENDIF
      CALL MUONS(NUM,IARR,IER)
      IF (NUM.GE.0 .AND. NUM.LE.10) NHIST(NUM)=NHIST(NUM)+1
      DO 20 I=1,NUM
        IF (IARR(2,I).GE.0 .AND. IARR(2,I).LE.10)
     +    NTYP(IARR(2,I))=NTYP(IARR(2,I))+1
   20 CONTINUE
      GO TO 10
  900 WRITE(*,'(A,I8,A,I8,A,I8)') ' events:',NEV,
     +  '  muon chamber hits (MUR1):',NSUMH,'  events with hits:',NEVHIT
      WRITE(*,'(A,11I7)') ' number of muons per event 0..10:',
     +  (NHIST(I),I=0,10)
      WRITE(*,'(A,11I7)') ' muon types 0..10:',(NTYP(I),I=0,10)
      IF (NEVHIT.EQ.0) THEN
        WRITE(*,*) 'FAIL: no events with muon chamber hits'
        STOP 1
      ENDIF
      WRITE(*,*) 'PASS'
      END
