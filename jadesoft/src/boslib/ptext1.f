C   07/06/96 606071904  MEMBER NAME  PTEXT1   (S4)          FORTRAN
      SUBROUTINE PTEXT
      COMMON/BCS/IW(1)
      LOGICAL*1 AR(1),LTEXT(60),DOLL/'$'/
      INTEGER TEXT(15),BLANK/'    '/
      EQUIVALENCE (TEXT(1),LTEXT(1))
      REAL*8 TEXTE(6)/'UCOND   ','UTABL   ','UHIST   ','UCORR   ',
     1   'USTOR/S ','TEXT    '/
      CALL BPOS('TEX*')
      LK=0
  10  CALL BNXT(IND,&100)
      K=IW(IND-2)/100000000
      M=IW(IND-2)-K*100000000
      IF(K.LE.0) K=0
      K=MOD(K,6)
      IF(K.LE.0) K=6
      IF(K.EQ.LK) GOTO 20
      LK=K
      WRITE(6,101) TEXTE(LK)
   20 CONTINUE
      NF=IW(IND)
      WRITE(6,102) M,(IW(IND+I),I=1,NF)
      GOTO 10
C
      ENTRY TCOND(N,AR)
      K=1
      GOTO 50
      ENTRY TTABL(N,AR)
      K=2
      GOTO 50
      ENTRY THIST(N,AR)
      K=3
      GOTO 50
      ENTRY TCORR(N,AR)
      K=4
      GOTO 50
      ENTRY TSTOR(N,AR)
      ENTRY TSTOS(N,AR)
      K=5
      GOTO 50
      ENTRY TTEXT(N,AR)
      K=6
   50 M=N
      IF(M.LT.0) GOTO 100
      M=M+K*100000000
      DO 60 I=1,61
      IF(AR(I).EQ.DOLL) GOTO 62
   60 CONTINUE
      I=61
   62 II=I-1
      IL=(II+3)/4
      DO 70 L=1,IL
   70 TEXT(L)=BLANK
      DO 80 I=1,II
   80 LTEXT(I)=AR(I)
      CALL BCRE(IND,'TEX*',M,IL,&100,IER)
      IF(IER.NE.0) GOTO 100
      CALL BSTR(IND,TEXT,IL)
  100 RETURN
  101 FORMAT(/,1X,A8/)
  102 FORMAT(1X,I16,4X,15A4)
      END
