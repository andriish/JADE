C   03/08/78 C8082201   MEMBER NAME  BOUND2   (LGSOURCE)    FORTRAN
      SUBROUTINE BOUND2(JBACK,PA0,*)
C
C     S.YAMADA    03-08-78  21:00
C
C---- CHECK BOUNDARIES OF MATERIAL FOR SHOWER
C     JBACK=1 FOR FLANGE, JBACK=2 FOR LIGHT GUIDE
C---- BOUNDARY CHECK OF THE FRONT AND BACK SURFACES ARE DONE BY BOUND.
C
      DATA  RF2/4.540/, RG2/0.1406/
      DIMENSION PA0(9)
C
      IF(JBACK-1) 100,1,2
C
C---- FLANGE
    1 IF(PA0(5)**2+PA0(6)**2.LE.RF2) GO TO 101
      RETURN
C
C---- LIGHT GUIDE
    2 IF(PA0(5)**2+PA0(6)**2.GE.RG2) GO TO 101
C
  100 RETURN
  101 RETURN1
      END
