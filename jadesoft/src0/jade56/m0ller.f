C   25/06/78            MEMBER NAME  M0LLER   (LGSOURCE)    FORTRAN
      SUBROUTINE M0LLER(ENE,EPSI)
      COMMON/M0PAR/CUTM
      COMMON/SFCPA/SFP,SFE
      EKIN=ENE-1.
      BETA2=1.-1./ENE**2
      EP0=CUTM/EKIN
      XR=RN(DAM)
      CC2=1.-2.*EP0
      EPSI=EP0/(1.-CC2*XR)
      XR=RN(DAM)
      CON=EPSI/(1.-EPSI)
      G=(CC2/3.)*(1.+CON**2+(EPSI*EKIN/ENE)**2
     @                       -CON*(EKIN+ENE)/ENE)
      G=G/BETA2
      IF(XR*SFE-G)30,30,31
 30    RETURN
 31    EPSI=0.
      RETURN
      END
