C   25/06/78            MEMBER NAME  COMPT    (LGSOURCE)    FORTRAN
      SUBROUTINE COMPT(ENE,GAM,EPSI)
      COMMON /SFCPA/SF,SFE
      EP0=1./(1.+2.*ENE)
      XR=RN(DAM)
      EPSI=EP0*EXP(GAM*XR)
      IF(EPSI.GE.1.)GOTO31
      XR=RN(DAM)
      EK2=ENE**2
      G=(1.+EPSI*(EK2-2.*ENE-2.+EPSI*(1.+2.*ENE+EK2*EPSI)))/
     @        (2.*EPSI*EK2)
      IF(SF*XR-G)30,30,31
 30    RETURN
 31    EPSI=0.
      RETURN
      END
