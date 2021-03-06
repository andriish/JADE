C   23/06/78 601062125  MEMBER NAME  BK7ABS   (JADEMC2)     FORTRAN
      FUNCTION BK7ABS(ALAMDA)
C
C---- S.YAMADA     17-11-77
C     LAST MODIFICATION  20-10-78  16:35
C
C---- CALCULATE ABSORPTION LENGTH IN THE BK7 LIGHT GUIDE.
C     THE NEW DATA FROM HTV IS USED.
C     ALAMDA IN NANO-METER,BK7ABS IN CM
C
      IF(ALAMDA-370.) 1,1,2
CC  1 BK7ABS = 10.**(0.0202*ALAMDA-5.445)
C---- FOR THE SCHOTT BK7
CC  1 BK7ABS = 10.0**(0.034*ALAMDA-10.38)
    1 BK7ABS = 10.0**(0.02275*ALAMDA-6.0026)
      RETURN
CC  2 BK7ABS = 10.**(0.001346*ALAMDA+1.531)
    2 BK7ABS = 10.**(0.00796*ALAMDA-0.531)
      RETURN
      END
