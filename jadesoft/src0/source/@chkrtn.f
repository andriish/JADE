C   09/03/79 002200411  MEMBER NAME  @CHKRTN  (SOURCE)      FORTRAN
C        CHECK NEW ROUTINES
      DIMENSION IF(29),A(15)
      EQUIVALENCE (A(1),IBE)
      DATA A/15*0./
      CALL LGINIT
      ZE=1522.
      ZE2=ZE*ZE
      IBE=1
      DO 40 I=10,20,5
      E=I
      DEP0=SHMAX(E*1000.,1)
      WRITE(6,950) E,DEP0
 950  FORMAT(' E,DEP=',2F12.3)
      YY=425
      DO 20 JY=1,29
      YY=YY-5.
      XX=275.
      DO 10 JX=1,29
      XX=XX+5.
      R=XX*XX+YY*YY
      RZ=SQRT(ZE2+R)
      R=SQRT(R)
      DEP=DEP0*ZE/RZ
      A(2)=E
      A(11)=ZE/RZ
      A(9)=XX/RZ
      A(10)=YY/RZ
      A(4)=XX
      A(5)=YY
      CALL LGECOR(A,DEP,IFLAG)
      IF(JX)=100.*E/A(2)
10    CONTINUE
      WRITE(6,900) IF
900   FORMAT(29I4)
20    CONTINUE
40    CONTINUE
      STOP
      END
