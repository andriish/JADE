      PROGRAM CALCHK
C-----------------------------------------------------------------------
C Check that the binary calibration file can be used by KLREAD.
C
C KLREAD (jadesoft/src2016/jadegs/klread.F) reads the records of the
C file and applies a record only if the bank name in word 2 is equal to
C one of the Hollerith constants of its NAME list:
C     IF(NAMEBN.EQ.NAME(I)) GOTO 12
C The name word of the file has the byte order of the machine that wrote
C it, so a file made on a big-endian machine is silently ignored (no
C calibration at all, e.g. none for the muon chambers) on a little-endian
C one. This program reads the file in the way KLREAD does (unformatted,
C GFORTRAN_CONVERT_UNIT=big_endian) and counts the records.
C
C usage: calcheck <binary calibration file>
C Exit code 0 if all records are recognised and MUCA records are present.
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NNAME
      PARAMETER (NNAME=17)
      INTEGER NAME(NNAME),NCOUNT(NNAME)
C The same list (and order) as in KLREAD
      DATA NAME/4HMUCA,4HLGMA,4HTAGS,4HJTPL,4HJTAB,
     +          4HTOFC,4HLGST,4HDEDX,4HSPTG,4HRVTX,
     +          4HZCAL,4HTAGF,4HIDJS,4HVTXC,4HVTXR,
     +          4HVTXB,4HVTXF/
      INTEGER MAXW
      PARAMETER (MAXW=100000)
      INTEGER IBUF(MAXW)
      INTEGER NDUMMY,ITM1,NREC,NOK,NBAD,NW,I,J,IOS,IBAD
      CHARACTER*256 FNAME
      CHARACTER*4 CBAD
      INTEGER IBADV
      EQUIVALENCE (IBADV,CBAD)

      NREC=0
      NOK=0
      NBAD=0
      IBAD=0
      DO 1 I=1,NNAME
        NCOUNT(I)=0
    1 CONTINUE

      CALL GET_COMMAND_ARGUMENT(1,FNAME)
      IF (FNAME.EQ.' ') THEN
        WRITE(*,*) 'usage: calcheck <binary calibration file>'
        STOP 2
      ENDIF
      OPEN(10,FILE=FNAME,FORM='UNFORMATTED',STATUS='OLD',IOSTAT=IOS)
      IF (IOS.NE.0) THEN
        WRITE(*,*) 'cannot open ',FNAME(1:LEN_TRIM(FNAME))
        STOP 2
      ENDIF
C First record: two words (as checked by KLREAD)
      READ(10,IOSTAT=IOS) NDUMMY,ITM1
      IF (IOS.NE.0) THEN
        WRITE(*,*) 'cannot read the first record'
        STOP 2
      ENDIF
C The calibration records: IBUF(1)=number of words, IBUF(2)=bank name
   10 CONTINUE
      READ(10,IOSTAT=IOS) NW
      IF (IOS.NE.0) GOTO 20
      BACKSPACE(10)
      IF (NW.LT.2 .OR. NW.GE.MAXW) THEN
        WRITE(*,*) 'record ',NREC+1,' has a wrong length word ',NW
        STOP 1
      ENDIF
      READ(10,IOSTAT=IOS) NW,(IBUF(I),I=2,NW+1)
      IF (IOS.NE.0) GOTO 20
      NREC=NREC+1
      DO 11 J=1,NNAME
        IF (IBUF(2).EQ.NAME(J)) THEN
          NCOUNT(J)=NCOUNT(J)+1
          NOK=NOK+1
          GOTO 10
        ENDIF
   11 CONTINUE
      NBAD=NBAD+1
      IF (IBAD.EQ.0) IBAD=IBUF(2)
      GOTO 10
   20 CONTINUE
      CLOSE(10)

      WRITE(*,'(A,I8,A,I8,A,I8)') ' records:',NREC,'  recognised:',NOK,
     +  '  not recognised:',NBAD
      DO 21 J=1,NNAME
        IBADV=NAME(J)
        IF (NCOUNT(J).GT.0) WRITE(*,'(2X,A4,I8)') CBAD,NCOUNT(J)
   21 CONTINUE
      IF (NREC.EQ.0) THEN
        WRITE(*,*) 'FAIL: no calibration records read'
        STOP 1
      ENDIF
      IF (NBAD.NE.0) THEN
        IBADV=IBAD
        WRITE(*,*) 'FAIL: the bank names of ',NBAD,' records do not ',
     +    'match the names of KLREAD (first: ',CBAD,' = ',IBAD,'),'
        WRITE(*,*) 'the file has the wrong byte order for names of ',
     +    'this machine: KLREAD would ignore these records.'
        WRITE(*,*) 'Use the files of jadesoft/cal2026 on little-endian',
     +    ' machines (JADE_CAL_DIR_NAME).'
        STOP 1
      ENDIF
      IF (NCOUNT(1).EQ.0) THEN
        WRITE(*,*) 'FAIL: no MUCA (muon chamber constants) record'
        STOP 1
      ENDIF
      WRITE(*,*) 'PASS: all records are recognised by KLREAD'
      END
