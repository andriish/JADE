C   07/06/96 606071858  MEMBER NAME  NEWOLD2  (S4)          FORTRAN
      SUBROUTINE BINT(NA,NB,NC,ND)
C     BOS SUBPROGRAM =1.1=
      COMMON/ACS/
     1   ICOND,NLAST,NDUMP,NSPL,NAMAX,NAMAX1,NLIST,NPRIM,NFRS,NLST,
     2   NEXT,NCI,NFRE,IN,KPOS,LIND,ILOW,LFDI,LFDK,NCPL,
     3   NHISTH(11),
     4        NCOL,NZT,IBFI,NLAST1,ISLST,IMLST,INAMV,IOLST,IPLST,
     5   NER(10),
     6   NRECL,NERRL,NRIN,NROUT,NS,NEXTI,NEXTA,NEXTB,ISAVB,NSAVB,
     7   NHISTL(11),
     8        MARKWR,NOUT(3),IASW,NPRE,IDUMMY(1),NEOTP,NDUMP1,
     9        NDUMMY(20)
      COMMON/BCS/IW(1)
      REAL RW(1)
      EQUIVALENCE (RW(1),IW(1))
      COMMON/CCS/IUND,INDT,INDC,NPT,INDA,LISTE(5),IPAS
      REAL*8 DATE(2)
      EXTERNAL UCOPY2,BRNM,BCHM,BPOS,BSAW,BDLS,BDLG,BDMP,BCHL,CCHL
      LOGICAL INIT/.FALSE./
C
C     LAYOUT OF BANK COMMON/BCS/
C
C     INDEX    IW( )             I   MEANING
C     ------------------------------------------------------
C         1    IW(1)             I+  POINTER TO FIRST BANKS
C         .    ...               I
C     NAMAX    IW(NAMAX)         I
C      NFRS    SPACE FOR BANKS   I+  BANK SPACE
C              ...               I
C      NLST    SPACE FOR BANKS   I
C              NALIST(1)         I+  LIST OF NAMES
C              ...               I
C     ISLST    NALIST(NLIST)     I
C              NSLIST(1)         I+  SPECIAL LIST OF NAMES
C              ...               I
C     IMLST    NSLIST(NLIST)     I
C              NMLIST(1)         I+  MARKER FOR SPECIAL LIST
C              ...               I
C              NMLIST(NLIST)     I
C     INAMV    NAME              I+  NAME
C              NAMES(1)          I+  NAMES
C              ...               I
C     IOLST    NAMES(NAMAX)      I
C              IOLIST(1)         I+  IO-MARKER
C              ...               I
C     IPLST    IOLIST(NAMAX)     I
C              IK(1)             I+  POINTER FOR NAMES
C              ...               I
C              IK(NAMAX)         I
C              IK(NAMAX+1)       I
C              ...               I
C     NLAST    IK(NAMAX+NPRIM)   I
C
C
      IF(INIT) GOTO 100
      INIT=.TRUE.
      CALL VZERO(ICOND,100)
      CALL VZERO(IUND,11)
      NDUMMY(11)=6
      NLAST=NA
      NLAST1=NLAST
      CALL VZERO(IW,NLAST)
      NRECL=4000
      IF(NB.GT.0) NRECL=NB
      NDUMP=500
      IF(NC.GT.0) NDUMP=NC
      NDUMP1=NDUMP
      NSPL=NLAST/10
      IF(ND.GT.0) NSPL=ND
      CALL DAY(DATE(1),DATE(2))
      NZT=NTIME(DUMMY)
      WRITE(6,101) DATE,NLAST,NRECL,NDUMP,NSPL
      NAMAX=200
      NLIST=100
      NERRL=10
      NEOTP=2
      MARKWR=-1
      GOTO 10
C
      ENTRY BINT1(NA,NB,NC,ND)
C
      NAMAX=MAX0(10,NA)
      NLIST=MAX0(10,NB)
      NERRL=MAX0( 0,NC)
      NEOTP=ND
      WRITE(6,102) NAMAX,NLIST,NERRL,NEOTP
C
   10 NPRIM=(NAMAX/2)*2-1
   12 NPRIM=NPRIM+2
      J    =SQRT(FLOAT(NPRIM))
      DO 14 K=3,J,2
      L    =NPRIM/K
      IF(L*K.EQ.NPRIM) GOTO 12
   14 CONTINUE
C
      NAMAX1=NAMAX+1
      IPLST=NLAST-NAMAX-NPRIM
      IOLST=IPLST-NAMAX
      INAMV=IOLST-NAMAX
      IMLST=INAMV-NLIST-1
      ISLST=IMLST-NLIST
      NFRS =NAMAX1
      NEXT =NFRS
      NLST =ISLST-NLIST
      ILOW=NLST
      GOTO 100
C
  100 RETURN
C
  101 FORMAT(/'0START/BANK ORGANISATION PROGRAM   -   JOB DATE ',A8,
     1   ' TIME ',A8/'0',6X,'CALL BINT(',I6,',',5X,'NR OF WORDS IN'
     2 ,' COMMON/BCS/'/17X,I6,',',5X,'MAX NR OF WORDS ALLOWED IN A ',
     3   'BANK-RECORD'/17X,I6,',',5X,'NR OF WORDS PRINTED IN A DUMP'/
     4   17X,I6,')',5X,'NR OF WORDS FOR LOW PRIORITY BANK SPACE'/)
  102 FORMAT('0',5X,'CALL BINT1(',I6,',',5X,'MAX NR OF NAMES'/
     1  17X,I6,',',5X,'MAX LENGTH OF LIST OF NAMES'/
     2  17X,I6,',',5X,'MAX NR OF READ ERRORS'/
     3  17X,I6,')',5X,'MAX NR OF LISTINGS FOR INPUT-BANK ERRORS'/)
      END
      SUBROUTINE BDMPA(IARG)
      COMMON/ACS/
     1   ICOND,NLAST,NDUMP,NSPL,NAMAX,NAMAX1,NLIST,NPRIM,NFRS,NLST,
     2   NEXT,NCI,NFRE,IN,KPOS,LIND,ILOW,LFDI,LFDK,NCPL,
     3   NHISTH(11),
     4        NCOL,NZT,IBFI,NLAST1,ISLST,IMLST,INAMV,IOLST,IPLST,
     5   NER(10),
     6   NRECL,NERRL,NRIN,NROUT,NS,NEXTI,NEXTA,NEXTB,ISAVB,NSAVB,
     7   NHISTL(11),
     8        MARKWR,NOUT(3),IASW,NPRE,IDUMMY(1),NEOTP,NDUMP1,
     9        NDUMMY(20)
      ICOND=IARG
      CALL BDMP
      RETURN
      END
      SUBROUTINE PRINTA(BIN,NA,NB)
C
C     BAS13     SUBROUTINE PRINTA
C
C
C     PRINTA PRINTS THE ARRAY BIN(NA) TO BIN(NB). LINES WITH ALL
C     ZEROS ARE SUPPRESSED, FORMAT IS CHOOSEN FOR EACH WORD ACCORDING
C     TO THE CONTENT (I.E. A, F(E) OR I FORMAT).
C     PRINTING IS TO UNIT NUNTB, DEFINED IN COMMON/ACS/
C     FOR NUNTB = 0 OR NEGATIVE PRINTOUT IS SUPPRESSED.
C
C                 --- -- --
C     CALL PRINTA(BIN,NA,NB)
C
C     --------------------------------------------------------------
      COMMON/ACS/
     1   ICOND,NLAST,NDUMP,NSPL,NAMAX,NAMAX1,NLIST,NPRIM,NFRS,NLST,
     2   NEXT,NCI,NFRE,IN,KPOS,LIND,ILOW,LFDI,LFDK,NCPL,
     3   NHISTH(11),
     4        NCOL,NZT,IBFI,NLAST1,ISLST,IMLST,INAMV,IOLST,IPLST,
     5   NER(10),
     6   NRECL,NERRL,NRIN,NROUT,NS,NEXTI,NEXTA,NEXTB,ISAVB,NSAVB,
     7   NHISTL(11),
     8        MARKWR,NOUT(3),IASW,NPRE,IDUMMY(1),NEOTP,NDUMP1,
     9   JNAM,NLK,NLM,ILKNA,ILKIN,ILKND,LKNAME,LKNR,ICUST,IREV,
     *   NUNTB,DUMMY(9)
C     --------------------------------------------------------------
      COMMON/BCS/IW(1)
C     --------------------------------------------------------------
      REAL*8 FMT(13),FMTF/'  F12.4,'/,FMTG/'  G12.4,'/,
     1     FMTI/'    I12,'/,FMTA/'  8X,A4,'/,FMT1/'        '/,
     2     FMT2/'(11X,   '/
      REAL*8 FMZ(11)
      REAL*4 BIN(2)
      INTEGER NEQ/0/,LIM(2,8),IUNITP/6/
      LOGICAL*1 LFMT(96),LL(8),LAST/')'/
      DATA FMT(1)/'(1X,I5, '/,FMT(2)/''' -'',I3,'/
      DATA LIM/75,80,91,97,107,111,123,136,
     1     193,201,209,217,226,233,240,249/
      EQUIVALENCE(FMT(1),LFMT(1)),(LL(1),REQ),(LL(5),NEQ)
      EQUIVALENCE (FMT(3),FMZ(1))
      IUNITP=NUNTB
      IF(IUNITP.LE.0.OR.IUNITP.GT.99) GOTO 100
      IF(NA.GT.NB) GOTO 100
C
C
C
      IZERO=0
      NTEN=10
      NF=((NA-1)/10)*10+1
      IF(NA.LE.0) NF=1
      IF(NA.LT.0.AND.NA.GT.(-10)) NTEN=-(-10/NA)*NA
      N=NB+1-NF
C
C     LOOP OVER ARRAY IN UNITS OF 10 WORDS
C
      DO 60 I=NF,NB,NTEN
      IA=I
      IB=MIN0(I+NTEN-1,NB)
      IF(IB.EQ.NB) GOTO 4
      DO 2 J=IA,IB
      IF(BIN(J).NE.0) GOTO 4
    2 CONTINUE
      IF(IZERO.EQ.0) IZERO=IA
      GOTO 60
    4 IF(IZERO.EQ.0) GOTO 8
      WRITE(IUNITP,101) IZERO
      IZERO=0
    8 M=3
      DO 50 J=IA,IB
      REQ=BIN(J)
      M=M+1
      LA=0
C
C     CHECK WETHER ALL 4 BYTES CONTAIN PRINTABLE CHARACTERS
C
      DO 20 K=1,4
      LL(8)=LL(K)
      IF(NEQ.EQ.64) GOTO 20
      DO 10 L=1,8
      IF(NEQ.LT.LIM(1,L)) GOTO 40
      IF(NEQ.GT.LIM(2,L)) GOTO 10
      IF(L.LE.4) LA=LA+1
      GOTO 20
   10 CONTINUE
      GOTO 40
   20 CONTINUE
C
C     ALLOW ONLY ONE SPECIAL CHARACTER
C
      IF(LA.GT.1) GOTO 40
      FMT(M)=FMTA
      GOTO 50
C
C     CHOOSE F(E) OR INTEGER FORMAT ACCORDING TO VALUE
C
   40 IF(ABS(REQ).GT.1.E20.OR.ABS(REQ).LT.1.E-20) GOTO 30
      IF(ABS(REQ).LT.10000.0.AND.ABS(REQ).GE.0.1) GOTO 45
      FMT(M)=FMTG
      GOTO 50
   45 FMT(M)=FMTF
      GOTO 50
   30 FMT(M)=FMTI
   50 CONTINUE
      LFMT(8*M)=LAST
      IF(N.LT.10) GOTO 55
      FMT(3)=FMT1
      JB=MOD(IB,1000)
      WRITE(IUNITP,FMT) IA,JB,(BIN(II),II=IA,IB)
      GOTO 60
   55 FMT(3)=FMT2
      WRITE(IUNITP,FMZ) (BIN(II),II=IA,IB)
   60 CONTINUE
C
C
C
  100 RETURN
  101 FORMAT(1X,I5,2X,'AND FOLLOWING ALL ZERO')
      END
