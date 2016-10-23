C   16/02/81            MEMBER NAME  INCOSM   (PATRECSR)    FORTRAN
      SUBROUTINE INCOSM
C
C     THIS SUBROTINE OVERWRITES SOME LIMITS IN CPATLM
C     FOR COSMIC DATA
C
C
      IMPLICIT INTEGER*2 (H)
C
#include "cpatlm.for"
C
       XYF(2)=20.
       XYF(4)=20.
       GFP(2)=20.
       GFP(4)=20.
       IGFP(9)=1
       IGFP(10)=1
       RETURN
       END
