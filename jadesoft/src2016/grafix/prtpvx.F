C   10/02/86 604291114  MEMBER NAME  PRTPVXR  (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE PRTPVX( IPTPVX, XS, YS , DEL, SIZE )
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. HAGEMANN 29/04/86 :  PRINT BOS BANK TPVX
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
      LIM2   = IPTPVX
      LIMLIM = 2*LIM2
      CALL CORE(HWORK,13)
      WRITE(cHWORK,1) HDATA(LIMLIM+1),HDATA(LIMLIM+2) ! PMF 17/11/99: JUSCRN changed to cHWORK
    1 FORMAT(' ',I4,I8)
      CALL SYSSYM(XS,YS,SIZE,HWORK,13,0.)
      YS = YS - DEL
      CALL CORE(HWORK,108)
      WRITE(cHWORK,2) (ADATA(LIM2+1+I),I=1,7),IDATA(LIM2+9), ! PMF 17/11/99: JUSCRN changed to cHWORK
     +                 ADATA(LIM2+10)
    2 FORMAT(' ',3E12.4,2X,3E12.4,2X,E12.4,I4,3X,E12.4)
      CALL SYSSYM(XS,YS,SIZE,HWORK,108,0.)
      YS = YS - DEL
      CALL CORE(HWORK,51)
      WRITE(cHWORK,3) (HDATA(LIMLIM+20+I),I=1,10) ! PMF 17/11/99: JUSCRN changed to cHWORK
    3 FORMAT(' ',10I5)
      CALL SYSSYM(XS,YS,SIZE,HWORK,51,0.)
      MULSEC = HDATA(LIMLIM+22)
      IF( MULSEC .LE. 0 ) GO TO 100
         IVX = 30
    4    IVX1 = 20
         IF( MULSEC+30 .LT. IVX+IVX1 ) IVX1 = MULSEC + 30 - IVX
         YS = YS - DEL
         CALL CORE(HWORK,101)
         WRITE(cHWORK,5) (HDATA(LIMLIM+IVX+I),I=1,IVX1) ! PMF 17/11/99: JUSCRN changed to cHWORK
    5    FORMAT(' ',20I5)
         LIMLO = IVX1*5 + 1
         CALL SYSSYM(XS,YS,SIZE,HWORK,LIMLO,0.)
         IVX = IVX + 20
         IF( IVX .GT. 30+MULSEC ) GO TO 100
            GO TO 4
  100 CONTINUE
      RETURN
      END
