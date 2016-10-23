C   07/06/96 606071917  MEMBER NAME  USTOS    (S4)          FORTG1
      SUBROUTINE USTOS(N,A,B,C,D)
      COMMON/BCS/IW(1000)
      REAL RW(1000)
      EQUIVALENCE (IW(1),RW(1))
C
C     STORE VECTORS IN BANK COMMON
C     (NAME,NUMBER)   =   ('STO*',N)
C        +1     NO OF VECTORS
C        +2     LENGTH OF VECTOR
C        +3     INTERNAL
C        +4 . . VECTORS
C
      INTEGER NHUN/100/
      REAL V(1)
      CALL BLOC(IND,'STO*',N,&20)
   10 I=IW(IND+3)
      M=IW(IND+2)
      IF(I.GE.IW(IND)) GOTO 100
      INDI=IND+I
      GOTO (1,2,3,4),M
    4 RW(INDI+4)=D
    3 RW(INDI+3)=C
    2 RW(INDI+2)=B
    1 RW(INDI+1)=A
      IW(IND+3)=IW(IND+3)+M
      IW(IND+1)=IW(IND+1)+1
      GOTO 100
   20 CALL NOARG(M)
      M=M-1
      IF(M.GT .4) M=4
      CALL BCRE(IND,'STO*',N,M*NHUN+3,&100,IER)
      IW(IND+1)=0
      IW(IND+2)=M
      IW(IND+3)=3
      GOTO 10
C
      ENTRY USTOR(N,V,NDIM)
      CALL BLOC(IND,'STO*',N,&50)
   30 I=IW(IND+3)
      IF(I.GE.IW(IND)) GOTO 100
      INDI=IND+I
      M=IW(IND+2)
      DO 40 J=1,M
   40 RW(INDI+J)=V(J)
      IW(IND+1)=IW(IND+1)+1
      IW(IND+3)=IW(IND+3)+M
      GOTO 100
   50 M=NDIM
      CALL BCRE(IND,'STO*',N,M*NHUN+3,&100,IER)
      IW(IND+1)=0
      IW(IND+2)=M
      IW(IND+3)=3
      GOTO 30
  100 RETURN
C
      ENTRY DSTOR(IARG)
      NHUN=IARG
      RETURN
       END
