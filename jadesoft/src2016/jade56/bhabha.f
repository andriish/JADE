C   25/06/78            MEMBER NAME  BHABHA   (LGSOURCE)    FORTRAN
      SUBROUTINE BHABHA(ENE,EPSI)
      COMMON/M0PAR/CUTM
      COMMON/SFCPA/SFP,SFE
      EKIN=ENE-1.
      BETA2=1.-1./ENE**2
      EP0=CUTM/EKIN
      XR=RN(DAM)
      CC2=1.-EP0
      EPSI=EP0/(1.-CC2*XR)
      XR=RN(DAM)
      Y=1./(EKIN+2.)
      G=(CC2/3.)*(1./BETA2-EPSI*((2.-Y**2)-EPSI*((3.-
     @    Y*(6.-Y*(1.-2.*Y)))-EPSI*(2.-Y*(10.-Y*(16.-8.*Y))
     @    -EPSI*(1.-Y*(6.-Y*(12.-8.*Y)))))))
      IF(XR*SFE-G)30,30,31
 30    RETURN
 31    EPSI=0.
      RETURN
      END
