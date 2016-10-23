C   21/11/81 311221842  MEMBER NAME  DSORT    (S)           FORTRAN
      SUBROUTINE DSORT (N,PH,TRCM,DTRCM)
C         SUBROUTINE TO TAKE AVERAGE OF 60% LOWEST PULSE HEIGHTS
C         PUSE HEIGHTS IN PH(L) ,L=L'TH PULSE HEIGHT
C         N = NUMBER OF HITS
      DIMENSION PH(2)
C                            SET TRUNCATED MEAN FRACTION
C---  DATA TRMFLO /0.  /, TRMFHI /1.00/
      DATA TRMFLO /0.05/, TRMFHI /0.70/
      IF (N.LT.2) GOTO 99
C                            SORT PULSEHEIGHTS IN INCREASING ORDER
C                            USE SHELL ALGORITHM
      M=N
   10 M=M/2
      IF (M.EQ.0) GO TO 40
      K=N-M
      DO 30 J=1,K
      I=J
   20 IF (I.LT.1) GO TO 30
      IF (PH(I+M).GE.PH(I)) GO TO 30
      W=PH(I)
      PH(I)=PH(I+M)
      PH(I+M)=W
      I=I-M
      GO TO 20
   30 CONTINUE
      GO TO 10
   40 CONTINUE
C                            SHELLS ARE SORTED NOW
      NTRMLO = N*TRMFLO + 0.5
      NTRMHI = N*TRMFHI + 0.5
      TRM = NTRMHI - NTRMLO
      SUM=0.
      SUM2=0.
      NTRMLO = NTRMLO + 1
      DO 60 I=NTRMLO,NTRMHI
      SUM=SUM+PH(I)
      SUM2=SUM2+PH(I)**2
   60 CONTINUE
C                            CALCULATE MEAN AND SIGMA MEAN
      TRCM = SUM/TRM
      RMS = SUM2/TRM - TRCM**2
      DTRCM = SQRT(RMS/TRM)
      GOTO 100
   99 TRCM = 0.
      DTRCM = 0.
  100 CONTINUE
      RETURN
      END
