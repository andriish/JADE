C   05/10/79 006211845  MEMBER NAME  CHANGE   (S)           FORTRAN
      SUBROUTINE CHANGE(KOMMON)
      IMPLICIT INTEGER*2(H),LOGICAL*1(Q)
C----------------------------------------------
C  MACRO CPATLM .... PATTERN RECOGNITION LIMITS
C----------------------------------------------
      COMMON /CPATLM/ PATRLM(5),FLINLM(10),TRELLM(20),ZFITLM(10),BKK(20)
     *               ,XYF(20),IGFP(20),XBKK(40),IADMIN(5),YBKK(20)
      INTEGER IXYF(20),LMPATR(5),LMFLIN(10)
      INTEGER LMTREL(20),LMZFIT(10),IBKK(20)
      DIMENSION GFP(20),IXBKK(40),IYBKK(20)
      EQUIVALENCE (PATRLM(1),LMPATR(1)),(IXBKK(1),XBKK(1)),(IYBKK(1),
     *YBKK(1))   ,(FLINLM(1),LMFLIN(1)),(TRELLM(1),LMTREL(1))
     *           ,(ZFITLM(1),LMZFIT(1)),(BKK(1),IBKK(1))
     *           ,(XYF(1),IXYF(1)),(GFP(1),IGFP(1)),(IADMIN(1),IMCERT)
     *           ,(IYBKK(20),IPPASS),(IADMIN(2),IPFAST)
C----------- END OF MACRO CPATLM --------------
C-----------------------------------------------------------------------
C                            MACRO CJDRCH .... JET CHAMBER CONSTANTS.
C-----------------------------------------------------------------------
C
      COMMON / CJDRCH / RDEC(4),PSIIN(3),RINCR(3),FIRSTW(3),FSENSW(3),
     +                  RDEPTH,SWDEPL,YSUSPN,TIMDEL(2,3),ZMAX,ZOFFS,
     +                  ZRESOL,ZNORM,ZAL,ZSCAL,DRIDEV,DRICOS,DRISIN,
     +                  PEDES,TZERO(3),DRIROT(96,2),SINDRI(96,2),
     +                  COSDRI(96,2),DRIVEL(96,2),T0FIX(3),
     +                  ABERR(8), DUMJDC(20)
C
C      BLOCK DATA SET TO MC VALUES, KALIBR WILL SET REAL DATA VALUES
C--->  A CHANGE OF THIS COMMON MUST BE DONE SIMULTANEOUSLY WITH  <----
C--->  A CHANGE OF THE BLOCK DATA                                <----
C
C--------------------------- END OF MACRO CJDRCH -----------------------
C
      INTEGER*4 IDEC(1)
      EQUIVALENCE (IDEC(1),RDEC(1))
C-----------------------------------------------------------------------
C                            MACRO CGRAPH .... GRAPHICS COMMON
C-----------------------------------------------------------------------
C
      LOGICAL DSPDTL,SSTPS,PSTPS,FREEZE
C
      COMMON / CGRAPH / JUSCRN,NDDINN,NDDOUT,IDATSV(11),ICREC,MAXREC,
     +                  LSTCMD,ACMD,LASTVW,ISTANV,
     +                  SXIN,SXAX,SYIN,SYAX,XMIN,XMAX,YMIN,YMAX,
     +                  DSPDTL(30),SSTPS(10),PSTPS(10),FREEZE(30),
     +                  IREADM,LABEL,LSTPS(10),IPSVAR
C
C------- END OF MACRO CGRAPH -------------------------------------------
C
C--------------------------------------------
C  MACRO CREPLY .... OUTPUT COMMON FOR ALPHAN
C--------------------------------------------
      LOGICAL*1 QREPLY
      COMMON/CREPLY/NREPLY,REPLY(20),QREPLY(20)
      INTEGER*4 IREPLY(20)
      EQUIVALENCE (IREPLY(1),REPLY(1))
C---------- END OF MACRO CREPLY -------------
C
C
C-----------------------------------------------------------------------
C
C         --------------- SUBROUTINE CHANGE  -----------------
C         --------------- AUTHOR : G.F.PEARCE ----------------
C         ---------- LAST UPDATE : 1200HRS 22/06/80 ----------
C
C     ROUTINE TO ALLOW INTERACTIVE ALTERATION OF STANDARD COMMONS
C     PRESENTLY CATERS FOR 2 COMMON BLOCKS, CPATLM AND CJDRCH.
C
C     KOMMON = 1 => CHANGE COMMON CPATLM (PATREC LIMITS)
C     KOMMON = 2 => CHANGE COMMON CJDRCH (JET CHAMBER CONSTANTS)
C
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
C                          INITIALISATION
C-----------------------------------------------------------------------
C
C LIST OF VALID ARRAY NAMES WITHIN COMMON BLOCKS
C
      REAL*8 ARRAY8(34)/
CPATLM - PATREC LIMITS
     + 'PATRLM  ','FLINLM  ','TRELLM  ','ZFITLM  ','BKK     ','XYF     '
     +,'GFP     ','XBKK    ','IADMIN  ','YBKK    '
CJDRCH - INNER DETECTOR CONSTANTS
     +,'RDEC    ','PSIIN   ','RINCR   ','FIRSTW  ','FSENSW  ','RDEPTH  '
     +,'SWDEPL  ','YSUSPN  ','TIMDEL  ','ZMAX    ','ZOFFS   ','ZRESOL  '
     +,'ZNORM   ','ZAL     ','ZSCAL   ','DRIDEV  ','DRICOS  ','DRISIN  '
     +,'PEDES   ','TZERO   ','DRIROT  ','SINDRI  ','COSDRI  ','DRIVEL  '
     +/
      REAL*4 ALL/'ALL '/
      REAL*4ARRAY4(2,34)
      EQUIVALENCE (ARRAY8(1),ARRAY4(1,1))
      EQUIVALENCE (VALUE,IVALUE)
C
C LENGTHS OF VALID ARRAY NAMES WITHIN COMMON BLOCKS
C
      INTEGER*2  HLEN(34)/
CPATLM - PATREC LIMITS
     + 5,10,20,10,20,20,20,40,5,20,
CJDRCH - INNER DETECTOR CONSTANTS
     + 4,3,3,3,3,1,1,1,6,1,1,1,1,1,1,1,1,1,1,3,192,192,192,192/
C
C RELATIVE POSITION OF VALID ARRAY NAMES WITHIN COMMON BLOCKS
C
      INTEGER*2 HRELPN(34) /
CPATLM - PATREC LIMITS
     + 0, 5,15,35,45,65,85,105,145,150,
CJDRCH - INNER DETECTOR CONSTANTS
     + 0,4,7,10,13,16,17,18,19,25,26,27,28,29,30,31,32,33,34,35,38,
     + 230,422,614/
C
C SET FLAGS TO COMMON BLOCK REQUESTED THIS CALL
C
      INTEGER*2 HPOS(2,2)/1,10,11,34/
      IF (KOMMON.LT.1.OR.KOMMON.GT.2) RETURN
      IPOS1 = HPOS(1,KOMMON)
      IPOS2 = HPOS(2,KOMMON)
C-----------------------------------------------------------------------
C                      PROGRAM VERSION NUMBER
C-----------------------------------------------------------------------
      CALL TRMOUT(80,' SUBROUTINE CHANGE .. VERSION FROM 30/06/80 ^')
      CALL TRMOUT(80,' ------------------------------------------ ^')
      CALL TRMOUT(80,'    ^')
C-----------------------------------------------------------------------
C                      LIST, CHANGE OR RESET ?
C-----------------------------------------------------------------------
 20   CALL TRMOUT(80,'ENTER 1 TO LIST, 2 TO CHANGE, 3 TO RESET ^')
      CALL ALPHAN(100)
      IF (NREPLY.EQ.0.OR.IREPLY(1).LE.0.OR.IREPLY(1).GT.3) RETURN
      ICOMM = IREPLY(1)
      GOTO(100,300,900),ICOMM
C
C
C
C-----------------------------------------------------------------------
C                        LIST CURRENT VALUES
C-----------------------------------------------------------------------
 100  CONTINUE
C
C
C
C
C ARRAY NAME ?
C
      IF (NREPLY.EQ.1) GOTO101
      REPLY(1) = REPLY(2)
      GOTO104
 101  WRITE(JUSCRN,9000)(ARRAY8(I),I=IPOS1,IPOS2)
 9000 FORMAT(' ARRAYS AVAILABLE ARE :',1X,5A8/5(24X,5A8/))
      CALL TRMOUT(80,'ENTER ARRAY TO BE LISTED (OR ALL):^')
      CALL ALPHAN(100)
      IF (NREPLY.EQ.0) GOTO20
 104  IF (REPLY(1).EQ.ALL) GOTO106
      DO 105 NAME=IPOS1,IPOS2
      IF (REPLY(1).EQ.ARRAY4(1,NAME)) GOTO107
 105  CONTINUE
      WRITE(JUSCRN,9001)
 9001 FORMAT(' **** ILLEGAL ARRAY NAME ')
      GOTO101
 106  NAM1 = IPOS1
      NAM2 = IPOS2
      GOTO110
 107  NAM1 = NAME
      NAM2 = NAME
C
C LIST ALL ELEMENTS OF REQUESTED ARRAY
C
 110  DO 119 J=NAM1,NAM2
      IL = HRELPN(J)
      LN = HLEN(J)
      LN2= LN/2
      GOTO(111,112),KOMMON
 111  WRITE(JUSCRN,9002)(ARRAY8(J),I,PATRLM(I+IL),LMPATR(I+IL),I=1,LN)
      GOTO119
 112  IF(J.LT.31)WRITE(JUSCRN,9002)
     +                    (ARRAY8(J),I,RDEC(I+IL),IDEC(I+IL),I=1,LN)
      IF(J.GE.31)WRITE(JUSCRN,9003)
     +           (ARRAY8(J),I,RDEC(I+IL),RDEC(I+IL+96),I=1,LN2)
 119  CONTINUE
 9002 FORMAT(5X,A8,I3,')',1X,E13.6,' (REAL)',I8,' (INTEGER)'/
     +   200(5X,A8,I3,')',1X,E13.6,' (REAL)',I8,' (INTEGER)'/))
 9003 FORMAT(5X,A8,'CELL',I3,  4X,'LEFT)',E13.6,  4X,'RIGHT)',E13.6/
     +   200(5X,A8,'CELL',I3,  4X,'LEFT)',E13.6,  4X,'RIGHT)',E13.6/))
      GOTO20
C-----------------------------------------------------------------------
C                     CHANGE CURRENT VALUES
C-----------------------------------------------------------------------
 300  WRITE(JUSCRN,9000)(ARRAY8(I),I=IPOS1,IPOS2)
 305  CALL TRMOUT(80,'CHANGE : ENTER ARRAY, ELEMENT AND NEW VALUE :^')
      CALL ALPHAN(100)
      IF (NREPLY.EQ.0)GOTO390
      ARRNAM=REPLY(1)
      DO 310 NAME=IPOS1,IPOS2
      IF (ARRNAM.EQ.ARRAY4(1,NAME))GOTO320
 310  CONTINUE
      WRITE(JUSCRN,9001)
      GOTO300
 320  IF(NAME.NE.27.AND.NAME.NE.28.AND.NAME.NE.32.AND.NAME.NE.33)GOTO32100015100
      CALL TRMOUT(80,'*** CHANGE THE ANGLE, NOT THE SIN/COS !!!!!!^')
      GOTO300
 321  IF(NREPLY.EQ.1)GOTO330
      IF(NREPLY.EQ.2)GOTO340
      IF(NREPLY.EQ.3)GOTO350
      IF(NREPLY.GE.4)GOTO360
 330  IF(HLEN(NAME).NE.1)GOTO331
      IEL1=1
      IEL2=1
      GOTO332
 331  CALL TRMOUT(80,'ENTER ARRAY ELEMENT OR RANGE OF ELEMENTS :^')
      CALLALPHAN(100)
      IF(NREPLY.EQ.0)GOTO305
      IEL1=IREPLY(1)
      IEL2=IEL1
      IF(NREPLY.GE.2)IEL2=IREPLY(2)
 332  CALL TRMOUT(80,'ENTER NEW VALUE :^')
      CALLALPHAN(100)
      IF(NREPLY.EQ.0)GOTO305
      VALUE=REPLY(1)
      GOTO370
 340  IF(HLEN(NAME).NE.1)GOTO341
      IEL1=1
      VALUE=REPLY(2)
      GOTO370
 341  CALL TRMOUT(80,'***** ILLEGAL ARRAY ELEMENT^')
      GOTO300
 350  IEL1=IREPLY(2)
      IEL2=IREPLY(2)
      VALUE=REPLY(3)
      GOTO370
 360  IEL1=IREPLY(2)
      IEL2=IREPLY(3)
      VALUE=REPLY(4)
C
C CHECK FOR ILLEGAL ARRAY ELEMENTS
C
 370  IF(IEL1.GT.HLEN(NAME).OR.IEL1.LT.1.OR.IEL2.GT.HLEN(NAME).OR.
     +IEL2.LT.1)GOTO341
C
C     SET TO NEW VALUE
C
 380  IPOSN1=HRELPN(NAME)+IEL1
      IPOSN2=HRELPN(NAME)+IEL2
      IEL=IEL1
      DO388IPOSN=IPOSN1,IPOSN2
      IF (KOMMON.EQ.1) PATRLM(IPOSN) = VALUE
      IF (KOMMON.EQ.2) RDEC  (IPOSN) = VALUE
      IF(NAME.NE.31)GOTO386
      RDEC(IPOSN+192)=SIN(VALUE)
      RDEC(IPOSN+384)=COS(VALUE)
 386  WRITE(JUSCRN,9110)ARRAY8(NAME),IEL,VALUE,IVALUE
 9110 FORMAT(' ARRAY ',A8,' ELEMENT',I3,' RESET TO ',E13.6,' (REAL)',
     +I8,' (INTEGER)')
 388  IEL=IEL+1
      GOTO305
C
C RESET ANY OTHER CONSTANTS THAT MAY BE AFFECTED
C
 390  DRISIN = SIN(DRIDEV)
      DRICOS = COS(DRIDEV)
      CALL INPATC
      CALL TRMOUT(80,' END OF CHANGE LOOP^')
      GOTO20
C-----------------
C     RESET LIMITS
C-----------------
 900  IF (KOMMON.EQ.1) CALL INPATR
      IF (KOMMON.EQ.1) CALL TRMOUT(80,'LIMITS RESET^')
      IF (KOMMON.EQ.2) CALL TRMOUT(80,'RESET NOT POSSIBLE FOR CJDRCH^')
      GOTO20
      END
