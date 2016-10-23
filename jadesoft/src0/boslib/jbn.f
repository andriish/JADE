C   07/06/96 606071853  MEMBER NAME  JBN      (S4)          FORTG1
      FUNCTION JBN(X,A,B,C,D,E,F,G,H,P,Q)
C     PURPOSE
C        OBTAIN INDEX FROM X IN A NONEQUIDISTANT SCALE WITH
C        A = LOWEST CHANNEL LEFT ADGE
C        B = NEXT CHANNEL LEFT ADGE
C        C = NEXT CHANNEL LEFT ADGE
C        D = . . .
C
C     USAGE
C        INDEX=JBN(X,A,B,C,D, . . . )
C        WHERE A,B,C,D, . . .(VARIABLE NR OF ARGUMENTS) ARE NB+1
C        SORTED CHANNEL LEFT EDGES
C        (A MAXIMUN OF 10 ARE ALLOWED, I.E. NB=9)
C
C     REMARK
C        INDEX = 0    IF X LT A
C              = NB+1  IF X GE LAST ARGUMENT
C
C     EXAMPLE
C        INDEX=JBN(X,0.47,0.53)
C        INDEX = 0 FOR         X LT 0.47
C              = 1 FOR 0.47 LE X LT 0.53
C              = 2 FOR 0.53 LE X
C
      K=0
      CALL NOARG(N)
      GOTO (100,11,12,13,14,15,16,17,18,19,20),N
   20 IF(X.GE.Q) GOTO 10
   19 IF(X.GE.P) GOTO 9
   18 IF(X.GE.H) GOTO 8
   17 IF(X.GE.G) GOTO 7
   16 IF(X.GE.F) GOTO 6
   15 IF(X.GE.E) GOTO 5
   14 IF(X.GE.D) GOTO 4
   13 IF(X.GE.C) GOTO 3
   12 IF(X.GE.B) GOTO 2
   11 IF(X.GE.A) GOTO 1
      GOTO 100
    1 K=1
      GOTO 100
    2 K=2
      GOTO 100
    3 K=3
      GOTO 100
    4 K=4
      GOTO 100
    5 K=5
      GOTO 100
    6 K=6
      GOTO 100
    7 K=7
      GOTO 100
    8 K=8
      GOTO 100
    9 K=9
      GOTO 100
   10 K=10
  100 JBN=K
      RETURN
      END
