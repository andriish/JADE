C   25/06/78            MEMBER NAME  PAIRC2   (LGSOURCE)    FORTRAN
      SUBROUTINE PAIRC2(ENE,EPSI)
      COMMON /BRPAR/BRA,BRB,BDEL
      COMMON /SFCPA/SF,SFE
      BH(X)=4.184*ALOG(X+0.952)-0.280
      DIMENSION XRR(3),XRH(3)
      DO10 I=1,3
      XR=RN(DAM)
      XRR(I)=XR
 10    CONTINUE
      DO11 I=1,3
 11    XRH(I)=ABS(XRR(I)-0.5)
      RHMAX=XRH(1)
      I=1
      IF(RHMAX-XRH(2))15,16,16
 15    RHMAX=XRH(2)
      I=2
 16    IF(RHMAX-XRH(3))17,18,18
 17    I=3
 18    EPSI=XRR(I)
      XR=RN(DAM)
      DEL=BDEL/(ENE*EPSI*(1.-EPSI))
      IF(DEL-1.)20,20,21
 20    G2=1.-BRA*DEL*(3.898-0.9805*DEL)
      GO TO 2
 21    G2=1.-BRA*BH(DEL)
 2    IF(SF*XR-G2)30,30,31
 30    RETURN
 31    EPSI=0.
      RETURN
      END
