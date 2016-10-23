C   07/06/96 606071906  MEMBER NAME  QPEAK    (S4)          FORTG1
      SUBROUTINE QPEAK(NA,KA,Q)
      COMMON/BCS/IW(1)
      REAL RW(1)
      EQUIVALENCE (RW(1),IW(1))
      REAL FC(50),YS(100),YU(100)
      REAL R(3),S(3),RES(18,2)
      REAL Q(10,3),RE(4)
      DO 4 I=1,10
      DO 4 J=1,3
    4 Q(I,J)=0.0
      IF(KA.LT.0) GOTO 100
      CALL BLOC(IND,'PEA*',NA,&100)
      KAMAX=IW(IND+8)
      IF(KA.GT.KAMAX) GOTO 100
      AW=RW(IND+5)
      ST=RW(IND+6)
      NB  =IW(IND+7)
      INDK=IND+10+KA*(NB+2)
      SUMI=0.0
      DO 5 K=1,NB
      YU(K)=RW(INDK+K)
    5 SUMI=SUMI+RW(INDK+K)
      Q(1,1)=SUMI+RW(INDK+NB+1)+RW(INDK+NB+2)
      IF(SUMI.EQ.0.0) GOTO 100
C
C
      CALL ANLSIM(YU,YS,NB,RE)
      IF(RE(4).GT.3.0) GOTO 10
      Q(3,1)=RE(2)
      Q(4,1)=RE(3)
      Q(2,1)=1.0
      Q(9,1)=RE(1)
      Q(10,1)=SQRT(RE(1))
      GOTO 20
   10 CALL SMHIST(YU,YS,DY,NB,0)
      CALL ANLDET(YS,NB,Q)
   20 Q(1,1)=SUMI+RW(INDK+NB+1)+RW(INDK+NB+2)
      JJ=Q(2,1)+0.5
      DO 30 J=1,JJ
      Q(3,J)=AW+ST*(Q(3,J)-0.5)
      DO 30 I=4,6
   30 Q(I,J)=ST*Q(I,J)
  100 RETURN
      END
