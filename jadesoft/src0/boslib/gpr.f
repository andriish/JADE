C   07/06/96 606071845  MEMBER NAME  GPR      (S4)          FORTRAN77
      SUBROUTINE GPR(IPAGE)
C
C
C
C
C
C
C
C
C
C                 ----------------------------
C                 ----- PLOT PROGRAM GPR -----
C                 ----------------------------
C
C
C     PURPOSE
C        PLOT IN 60 * 120 BINS  COUNTS, NUMBERS, TEXT, SYMBOLS
C
C     CALL GPR(IPAGE)           PRINT ONE PLOT (NEW PAGE FOR IPAGE=1,
C                               BUFFER IS CLEARED AFTER PRINTOUT,
C                               BUT NOT SCALES AND TEXT)
C     CALL GPL(LIM)             ALLOW ADDITIONAL LIM PLOTS TO BE
C                               PRINTED (DEFAULT IS 20)
C     CALL GTE('TEXT$')         ADD TEXT (DELIMITED BY $) TO THE
C                               PLOT (PRINTED ABOVE THE PLOT)
C     CALL GNE(NE)              ADD NE (POSITIVE INTEGER UP TO
C                               8 DIGITS) TO THE PLOT (PRINTED ABOVE
C                               THE PLOT), UP TO SIX NUMBERS CAN BE
C                               STORED
C
C
C                               DEFINITION OF SCALES
C                               --------------------
C                               60 BINS FOR Y, 120 BINS FOR X
C     CALL GRY(YA,YB)           Y-RANGE YA TO YB (ROUNDED)
C     CALL GRX(XA,XB)           X-RANGE XA TO XB (ROUNDED)
C     CALL GEY(YA,YB)           Y-RANGE YA TO YB (EXACT)
C     CALL GEX(XA,XB)           X-RANGE XA TO XB (EXACT)
C     CALL GMY(Y,N)             Y-RANGE FROM MIN AND MAX (SEVERAL
C                               CALLS ALLOWED)
C     CALL GMX(X,N)             X-RANGE FROM MIN AND MAX (SEVERAL
C                               CALLS ALLOWED)
C
C
C
C                               ADD TO PLOT (OVERWRITING PREVIOUS CONT.)
C                               ----------------------------------------
C     CALL GCN(YI,XJ)           INCREASE BIN I/J BY ONE
C     CALL GNR(YI,XJ,NR)        ADD NR (INTEGER UP TO 7 DIGITS) IN
C                               BIN I/J, I/J-1, . . .THE PRINTED NUMBER
C                               IS PRECEEDED BY BLANK OR -SIGN (OR *, IF
C                               SPACE INSUFFICIENT)
C     CALL GTX(YI,XJ,'TEXT$')   ADD TEXT (DELIMITED BY $) IN
C                               BIN I/J, I/J+1, . . .
C     CALL GPT(Y,X,N,'C')       ADD N POINTS Y,X (CONNECTED BY STRAIGHT
C     CALL GPT(Y,X,N,IS)        LINES) WITH CHARACTER 'C' OR CHAR IS
C                               (IS BETWEEN 0 AND 40)
C
C     ARGUMENTS YI OR XJ MAY BE INTEGERS I=1 TO 60 OR J=1 TO 120 RESP.
C     OR FLOATING POINT NUMBERS (SCALES ARE USED TO CONVERT TO BINS).
C     I/J = 1/1 IS LOWER LEFT EDGE. IN GPT Y AND X ARE NOT ALLOWED
C     TO BE INTEGERS EXCEPT FOR N = 1.
C
C          IS    0  1  2  3  4  5  6  7  8  9
C             +-------------------------------
C           0 I  0  1  2  3  4  5  6  7  8  9      E.G. IS = 22
C          10 I  A  B  C  D  E  F  G  H  I  J      EQUIV. TO 'M'
C          20 I  K  L  M  N  O  P  Q  R  S  T
C          30 I  U  V  W  X  Y  Z  +  -  *  .
C          40 I BLK
C
C
C
C
C
C
C
C
C
      INTEGER AR(31,62),BLK/'    '/
      LOGICAL*1 LR(124,62)
      EQUIVALENCE (LR(1,1),AR(1,1))
      LOGICAL OUT
      OUT(K,LIM)=K.LT.1.OR.K.GT.LIM
      REAL SYA/-1.0/,SYB/1.0/,SXA/-1.0/,SXB/1.0/
      INTEGER IDX/0/,IDY/0/,IEN/0/
      INTEGER TE(15)
      LOGICAL*1 LT(60)/60*' '/
      EQUIVALENCE (TE(1),LT(1))
      LOGICAL*1 LCN(4)
      INTEGER NCN/0/
      EQUIVALENCE (NCN,LCN(1))
      LOGICAL*1 LIST(42)/
     1   '0','1','2','3','4','5','6','7','8','9',
     2   'A','B','C','D','E','F','G','H','I','J',
     3   'K','L','M','N','O','P','Q','R','S','T',
     4   'U','V','W','X','Y','Z','+','-','*','.',' ','$'/
      REAL TS(12)
      REAL*8 TF(12)
      REAL X(1),Y(1)
      INTEGER LIMA/0/,LIMP/20/
      EQUIVALENCE (FE,NE)
      LOGICAL*1 LISYM(4),TEXT(60)
      EQUIVALENCE (ISYM,LISYM(1))
      LOGICAL*1 RL(8)
      REAL*8 R
      EQUIVALENCE (R,RL(1))
      REAL*8 TEXTNR(6)/6*'        '/,BLANK/'        '/
      INTEGER IT/0/
C
C     PRINTOUT
      IF(IEN.EQ.0) GOTO 86
      IEN=0
      IF(LIMA.GE.LIMP) GOTO 86
      LIMA=LIMA+1
      DO 6 J=2,61
      DO 4 I=1,31
      IF(AR(I,J).EQ.BLK) GOTO 4
      KB=4*I
      KA=KB-3
      DO 2 K=KA,KB
      LCN(4)=LR(K,J)
      IF(NCN.GT.36) GOTO 2
      LR(K,J)=LIST(NCN+1)
    2 CONTINUE
    4 CONTINUE
    6 CONTINUE
      DO 8 I=1,7
      T=FLOAT(I-1)/6.0
    8 TS(I)=T*SYA+(1.0-T)*SYB
      CALL BFMT(TS,7,TF,EF)
      IP=0
      IF(IPAGE.EQ.1) IP=1
      WRITE(6,101) IP,TEXTNR,LT,EF
      DO 10 J=1,62
      IF(MOD(J,10).EQ.2) WRITE(6,102) TF((J+8)/10),(AR(I,J),I=1,31)
      IF(MOD(J,10).NE.2) WRITE(6,103)              (AR(I,J),I=1,31)
   10 CONTINUE
      DO 12 J=1,12
      T=FLOAT(J-1)/12.0
   12 TS(J)=T*SXB+(1.0-T)*SXA
      CALL BFMT(TS,12,TF,EF)
      WRITE(6,104) TF,EF
      GOTO 86
C
C     DELIMIT PRINTOUT
      ENTRY GPL(LIM)
      LIMP=LIM
      LIMA=0
      GOTO 100
C
C     TEXT DEFINITION
      ENTRY GTE(TEXT)
      DO 14 I=1,15
   14 TE(I)=BLK
      DO 16 I=1,60
      IF(TEXT(I).EQ.LIST(42)) GOTO 100
      IF(TEXT(I).EQ.LIST(41)) GOTO 16
      LCN(4)=TEXT(I)
      IF(NCN.GE.0.AND.NCN.LE.74) GOTO 100
      IF(NCN.GE.128.AND.NCN.LE.191) GOTO 100
   16 LT(I)=TEXT(I)
      GOTO 100
C
C     NR DEFINITION
      ENTRY GNE(NE)
      IF(IT.GE.6) GOTO 100
      IT=IT+1
      CALL ITODA(IABS(NE),TEXTNR(IT),-8)
      GOTO 100
C
C     SCALE DEFINITION
      ENTRY GRY(YA,YB)
      IF(IEN.NE.0) GOTO 100
      IF(YA.EQ.YB) GOTO 20
      CALL XBINS(YA,YB,60,SYA,ST)
      SYB=SYA+60.0*ST
      GOTO 100
      ENTRY GRX(XA,XB)
      IF(IEN.NE.0) GOTO 100
      IF(XA.EQ.XB) GOTO 22
      CALL XBINS(XA,XB,120,SXA,ST)
      SXB=SXA+120.0*ST
      GOTO 100
      ENTRY GEY(YA,YB)
      IF(IEN.NE.0) GOTO 100
      IF(YA.EQ.YB) GOTO 20
      SYA=YA
      SYB=YB
      GOTO 100
      ENTRY GEX(XA,XB)
      IF(IEN.NE.0) GOTO 100
      IF(XA.EQ.XB) GOTO 22
      SXA=XA
      SXB=XB
      GOTO 100
   20 SYA=-1.0
      SYB=+1.0
      GOTO 100
   22 SXA=-1.0
      SXB=+1.0
      GOTO 100
      ENTRY GMY(Y,N)
      IF(IEN.NE.0) GOTO 100
      IF(IDY.EQ.0) YMIN=Y(1)
      IF(IDY.EQ.0) YMAX=Y(1)
      IDY=1
      DO 24 I=1,N
      YMIN=AMIN1(YMIN,Y(I))
   24 YMAX=AMAX1(YMAX,Y(I))
      GOTO 100
      ENTRY GMX(X,N)
      IF(IEN.NE.0) GOTO 100
      IF(IDX.EQ.0) XMIN=X(1)
      IF(IDX.EQ.0) XMAX=X(1)
      IDX=1
      DO 26 I=1,N
      XMIN=AMIN1(XMIN,X(I))
   26 XMAX=AMAX1(XMAX,X(I))
      GOTO 100
C
C     INCREASE COUNT IN BIN I/J
      ENTRY GCN(YI,XJ)
      IGT=1
      IF(IEN.EQ.0) GOTO 70
   30 GOTO 90
   32 LCN(4)=LR(1+J,62-I)
      IF(NCN.GT.36) NCN=0
      NCN=NCN+1
      IF(NCN.GT.36) NCN=36
      LR(1+J,62-I)=LCN(4)
      GOTO 100
C
C     INSERT NR
      ENTRY GNR(YI,XJ,NR)
      IGT=2
      IF(IEN.EQ.0) GOTO 70
   40 GOTO 90
   41 CALL ITODA(IABS(NR),R,-8)
      DO 48 K=1,8
      LR(1+J,62-I)=RL(9-K)
      J=J-1
      IF(J.GE.1) GOTO 44
   42 LR(2+J,62-I)=LIST(39)
      GOTO 100
   44 IF(RL(9-K).NE.LIST(41)) GOTO 46
      IF(NR.LT.0) LR(2+J,62-I)=LIST(38)
      GOTO 100
   46 IF(K.EQ.8) GOTO 42
   48 CONTINUE
      GOTO 100
C
C     INSERT TEXT
      ENTRY GTX(YI,XJ,TEXT)
      IGT=3
      IF(IEN.EQ.0) GOTO 70
   50 GOTO 90
   51 DO 52 K=1,120
      IF(TEXT(K).EQ.LIST(42)) GOTO 100
      IF(TEXT(K).EQ.LIST(41)) GOTO 53
      LCN(4)=TEXT(K)
      IF(NCN.GE.0.AND.NCN.LE.74) GOTO 100
      IF(NCN.GE.128.AND.NCN.LE.191) GOTO 100
   53 LR(1+J,62-I)=TEXT(K)
      J=J+1
      IF(J.GT.120) GOTO 100
   52 CONTINUE
      GOTO 100
C
C     INSERT POINTS
      ENTRY GPT(Y,X,N,IS)
      IF(N.LE.0) GOTO 100
      IGT=4
      ISYM=IS
      IF(OUT(ISYM+1,41)) GOTO 60
      LSYM=LIST(ISYM+1)
      GOTO 61
   60 LSYM=LISYM(1)
   61 IF(IEN.EQ.0) GOTO 70
      IF(N.EQ.1) GOTO 90
      IL=0
      DO 68 I=1,N
      II=1.0+60.0*(Y(I)-SYA)/(SYB-SYA)
      JJ=1.0+120.0*(X(I)-SXA)/(SXB-SXA)
      IF(OUT(II, 60)) GOTO 66
      IF(OUT(JJ,120)) GOTO 66
      LR(1+JJ,62-II)=LSYM
      IF(IL.EQ.0) GOTO 64
      F=(IL-II)**2+(JL-JJ)**2
      IF(F.LE.2.0) GOTO 64
      DT=1.0/SQRT(F)
      T=0.0
   62 YM=(1.0-T)*Y(I)+T*Y(I-1)
      XM=(1.0-T)*X(I)+T*X(I-1)
      IK=1.0+60.0*(YM-SYA)/(SYB-SYA)
      JK=1.0+120.0*(XM-SXA)/(SXB-SXA)
      LR(1+JK,62-IK)=LSYM
      T=T+DT
      IF(T.LT.1.0) GOTO 62
   64 IL=II
      JL=JJ
      GOTO 68
   66 IL=0
   68 CONTINUE
      GOTO 100
   69 LR(1+J,62-I)=LSYM
      GOTO 100
C
C     FIRST ENTRY

   70 IEN=1
      IF(IDY.EQ.0) GOTO 74
      IDY=0
      IF(YMIN.NE.YMAX) GOTO 72
      SYA=-1.0
      SYB=+1.0
      GOTO 74
   72 CALL XBINS(YMIN,YMAX,60,SYA,ST)
      SYB=SYA+60.0*ST
   74 IF(IDX.EQ.0) GOTO 78
      IDX=0
      IF(XMIN.NE.XMAX) GOTO 76
      SXA=-1.0
      SXB=+1.0
      GOTO 78
   76 CALL XBINS(XMIN,XMAX,120,SXA,ST)
      SXB=SXA+120.0*ST
   78 CONTINUE
C
C     CLEAR
   80 DO 81 J=1,62
      DO 81 I=1,31
   81 AR(I,J)=BLK
      DO 82 J=1,62
      LR(1,J)=LIST(19)
   82 LR(122,J)=LIST(19)
      DO 83 I=1,122
      LR(I,1)=LIST(38)
   83 LR(I,62)=LIST(38)
      DO 84 J=2,62,10
      LR(1,J)=LIST(37)
   84 LR(122,J)=LIST(37)
      DO 85 I=1,121,10
      LR(I,1)=LIST(37)
   85 LR(I,62)=LIST(37)
      LR(1,1)=LIST(37)
      LR(122,1)=LIST(37)
      LR(122,62)=LIST(37)
      GOTO (30,40,50,61),IGT
   86 IF(IT.LE.0) GOTO 100
      DO 88 I=1,6
   88 TEXTNR(I)=BLANK
      IT=0
      GOTO 100
C
C     GET I/J INDEX
   90 IF(IGT.NE.4) FE=YI
      IF(IGT.EQ.4) FE=Y(1)
      IF(OUT(NE,60)) GOTO 92
      I=NE
      GOTO 94
   92 I=1.0+60.0*(FE-SYA)/(SYB-SYA)
      IF(OUT(I,60)) GOTO 100
   94 IF(IGT.NE.4) FE=XJ
      IF(IGT.EQ.4) FE=X(1)
      IF(OUT(NE,120)) GOTO 96
      J=NE
      GOTO 98
   96 J=1.0+120.0*(FE-SXA)/(SXB-SXA)
      IF(OUT(J,120)) GOTO 100
   98 GOTO (32,41,51,69),IGT
  100 RETURN
  101 FORMAT(I1,10X,6(A8,2X),60A1/5X,A4)
  102 FORMAT(1X,A8,1X,30A4,A2)
  103 FORMAT(10X,     30A4,A2)
  104 FORMAT(5X,12(A8,2X),A4)
      END
