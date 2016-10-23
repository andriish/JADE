C   07/06/96 606071904  MEMBER NAME  PTEXT    (S4)          FORTRAN
      SUBROUTINE PTEXT(N,KK)
      COMMON/BCS/IW(1)
      LOGICAL*1 AR(1),LTEXT(60),DOLL/'$'/
      INTEGER TEXT(15),BLANK/'    '/
      EQUIVALENCE (TEXT(1),LTEXT(1))
      IF(N.LT.0) GOTO 10
      M=KK+10*N
      CALL BLOC(IND,'TEX*',M,&10)
      NF=IW(IND)
      IF(NF.LE.0) GOTO 10
      WRITE(6,101) (IW(IND+I),I=1,NF)
      GOTO 100
   10 WRITE(6,101)
      GOTO 100
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
      ENTRY TMEAN(N,AR)
      K=6
      GOTO 50
      ENTRY TPEAK(N,AR)
      K=7
   50 M=N
      IF(M.LT.0) GOTO 100
      M=K+10*M
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
  101 FORMAT('0',5('----'),1X,15A4)
      END
