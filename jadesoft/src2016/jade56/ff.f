C   25/06/78 704231930  MEMBER NAME  FF       (S)           FORTRAN
      FUNCTION FF(Z)
      DATA ALP/0.007297/
      A2=(ALP*Z)**2
      FF=A2*(1./(1.+A2)+0.20206-0.0369*A2+0.0083*A2**2
     @    -0.002*A2**3)
      RETURN
      END
