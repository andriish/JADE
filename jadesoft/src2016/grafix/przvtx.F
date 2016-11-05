C   10/02/86 605031039  MEMBER NAME  PRZVTX   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE PRZVTX( IPZVTX, XS, YS , DEL, SIZE )
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. HAGEMANN 29/04/86 :  PRINT BOS BANK ZVTX
C
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
#include "cgraph.for"
C
      COMMON / CWORK1 / HWORK(70)
C
*** PMF 17/11/99: add variables needed for emulation of DESYLIB routine 'CORE'  
      CHARACTER cHWORK*140
      EQUIVALENCE (cHWORK,HWORK(1))
*** PMF(end)
C
C------------------  C O D E  ------------------------------------------
C
      LIM2 = IPZVTX
      CALL CORE(HWORK,61)
      WRITE(cHWORK,1) (ADATA(LIM2+I),I=1,5),IDATA(LIM2+6) ! PMF 17/11/99: JUSCRN changed to cHWORK
    1 FORMAT('  ',5F10.3,I9)
      CALL SYSSYM(XS,YS,SIZE,HWORK,61,0.)
      RETURN
      END
