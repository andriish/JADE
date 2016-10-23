C   07/06/96 606071906  MEMBER NAME  QTABL    (S4)          FORTG1
      SUBROUTINE QTABL(KA,I,A)
      REAL A(16)
      COMMON/BCS/IW(1)
      INTEGER NF(8),N/0/
      INTEGER*2 NH(16),NCH(2)
      EQUIVALENCE (NF(1),NH(1)),(N,NCH(1))
      DO 10 J=1,16
   10 A(J)=0.0
      IF(I.LT.0.OR.I.GT.255) GOTO 100
      II=I/16
      IJ=I-II*16
      NC=KA*16+II
      CALL BLOC(IND,'TAB*',NC,&100)
      IND=IND+IJ*8
      DO 20 J=1,8
   20 NF(J)=IW(IND+J)
      DO 30 J=1,16
      NCH(2)=NH(J)
   30 A(J)=N
  100 RETURN
      END
