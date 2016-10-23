C   07/06/96 606071859  MEMBER NAME  PCOND    (S4)          FORTG1
      SUBROUTINE PCOND
      COMMON/BCS/IW(1)
      INTEGER*2 NH(2)
      EQUIVALENCE (NF,NH(1))
      INTEGER NLT(16)
      NN=0
      M=0
      CALL BPOS('CON*')
   80 CALL BNXT(K,*98)
      IF(M.EQ.0) WRITE(6,101)
      M=1
      N=IW(K)
      NF=IW(K-2)
C     WRITE(6,103) NF,NH
C 103 FORMAT(1X,10I12)
      NF=NF
      IF(NN.EQ.0) GOTO 81
      IF(NH(1).EQ.NLT(1)) GOTO 90
      WRITE(6,102)      (NLT(J),J=1,NN)
   81 NN=1
   90 NLT(1)=NH(1)
      DO 96 L=1,8
      IF(IW(K+L).EQ.0) GOTO 96
      IF(NN.LT.16) GOTO 94
      WRITE(6,102)      (NLT(J),J=1,NN)
      NN=1
   94 NLT(NN+1)=IW(K+L)
      NLT(NN+2)=NH(1)
      NLT(NN+3)=NH(2)-1+L
C     NLT(NN+3)=NLT(NN+3)+L-1
C     NLT(NN+3)=NH(2)
      NN=NN+3
   96 IW(K+L)=0
      CALL BDLS('CON*',NF)
      GOTO 80
   98 IF(NN.EQ.0) GOTO 99
      WRITE(6,102)      (NLT(J),J=1,NN)
   99 CONTINUE
      RETURN
  101 FORMAT('0---',7('----')/' UCOND',4X,'ENTRIES *(    I,'
     1   ,'    J)')
  102 FORMAT(' I=',I5,5(I 9,' *(',I5,',',I5,')'))
      END
