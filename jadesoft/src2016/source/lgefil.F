C   26/07/79 C9073101   MEMBER NAME  LGEFIL   (SOURCE)      FORTRAN
      SUBROUTINE LGEFIL(I1,J1,J2,IE,*)
C     CODED BY Y.WATANABE ON 29/7/79 23:50
      IMPLICIT INTEGER*2 (H)
#include "clgwork2.for"
      DIMENSION INDX(6,6),IAD(2,2),IPP(2,2)
      DATA IAD/0,24,72,48/,IPP/1,2,4,3/
      DATA INDX/0,0,1,5,10,16, 0,0,2,6,11,17, 4,3,7,12,18,0,
     1     9,8,13,20,19,0, 15,14,22,21,0,0, 24,23,4*0/
      IS=2
      IF(I1.GT.5) IS=1
      JS=2
C
      DO 10 J=J1,J2
      IF(J.GT.5) JS=1
      IP=IPP(IS,JS)
C
      GO TO (1,2,3,4),IP
1     IX=I1-5
      IY=J-5
      GO TO 5
2     IX=J-5
      IY=6-I1
      GO TO 5
3     IX=6-I1
      IY=6-J
      GO TO 5
4     IX=6-J
      IY=I1-5
5     N=INDX(IX,IY)
C
      IF(N.EQ.0) GO TO 10
      NPOINT=NPOINT+1
      IF(NPOINT.GE.NPMAX) RETURN1
      N=N+IAD(IS,JS)
      IF(IE.GT.0) N=N+96
      N=N+2687
      HCLADR(NPOINT)=N
C     WRITE(6,600) IE,I1,J,IS,JS,IP,IX,IY,N,NPOINT
C600  FORMAT(' LGFIL',12I8)
10    CONTINUE
C
      RETURN
      END
