C   18/10/82            MEMBER NAME  APLU     (S)           FORTRAN
      SUBROUTINE APLU(IRB,IEV)
C     SPECIAL PLUTO PROGRAM
C
C     FOR NEW RUN   IRB = RUN NR       = 0 OTHERWISE
C                   IEV = IW(IHD+4)
C
      COMMON/BCS/IW(1)
      INTEGER LRN/0/
      IRB=0
      IEV=0
      CALL BLOC(IHD,'HEAD',0,*100)
      IRN=IW(IHD+1)
      IEV=IW(IHD+4)
      IF(IRN.EQ.LRN) GOTO 100
      LRN=IRN
      IRB=IRN
  100 RETURN
      END
