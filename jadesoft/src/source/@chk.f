C   19/02/80 002200259  MEMBER NAME  @CHK     (SOURCE)      FORTRAN
C        CHECK NEW ROUTINES
      DIMENSION IF(29)
      E=15.
      R=350.*SQRT(2.)
      YY=425
      DO 40 JY=1,29
      YY=YY-5.
      XX=275.
      DO 30 JX=1,29
      XX=XX+5.
      RR=XX*XX+YY*YY
      DR=SQRT((XX-350.)**2+(YY-350.)**2)
      DDR=0.
      IF(ABS(DR).GT.0.) DDR=(SQRT(RR)-R)/DR
      CALL POSEND(E,0.1*DR,DDR,FACT,*30)
      IF(JX)=FACT*1000.+0.5
30    CONTINUE
      WRITE(6,900) IF
900   FORMAT(29I4)
40    CONTINUE
      STOP
      END
