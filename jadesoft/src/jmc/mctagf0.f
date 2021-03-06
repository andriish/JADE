C   23/08/85 511061956  MEMBER NAME  MCTAGF0  (S)           FORTRAN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
      REAL FUNCTION MCTAGF(X,MARKMC)
C
C WEIGHTING FUNCTION FOR INTEGRATION OF TAGGING MC ENERGY DEPOSTION
C
C A.J.FINCH 8/3/84
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
CCCC  DATA SLOPE/0.025/
      DATA XMAX/190.0/
      DIMENSION SLOPE(10)
      DATA SLOPE  /3*0.025,7*0.0675/
      DATA SLOPE2/0.0325/
C  BEST SO FAR: 180,0.0675,0.035,0.40
      DATA PERCEN/0.45/
      LOGICAL FIRST/.TRUE./
C
      IF(FIRST)CUTOFF=-(LOG(PERCEN)/SLOPE(4))
      FIRST = .FALSE.
C
      IF(MARKMC.GE.4.AND.X.GE.CUTOFF)GOTO 4
         MCTAGF=EXP(-1.0*X*SLOPE(MARKMC))
         RETURN
C
C SPECIAL SECTION FOR MARKMC = 4 , AT LARGE X
 4    CONTINUE
         MCTAGF=PERCEN*EXP(-1.0*SLOPE2*(X-CUTOFF))
         IF(X.GT.XMAX)MCTAGF = 0.0
         RETURN
      END
