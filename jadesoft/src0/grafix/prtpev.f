C   10/02/86 604291041  MEMBER NAME  PRTPEVR  (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE PRTPEV( IPTPEV, XS, YS , DEL, SIZE )
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. HAGEMANN 29/04/86 :  PRINT BOS BANK TPEV
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
C
C------------------  C O D E  ------------------------------------------
C
      LIM2 = IPTPEV
      LIMH2 = 2*LIM2
      CALL CORE(HWORK,62)
      WRITE(JUSCRN,1) IDATA(LIM2+1),IDATA(LIM2+2),
     &               (HDATA(LIMH2+I),I=5,14)
    1 FORMAT('  ',I8,2X,I8,2X,10I4)
      CALL SYSSYM(XS,YS,SIZE,HWORK,62,0.)
      YS = YS - DEL
      CALL CORE(HWORK,114)
      WRITE(JUSCRN,2) (HDATA(LIMH2+I),I=15,34)
    2 FORMAT('  ',16I5,4I8)
      CALL SYSSYM(XS,YS,SIZE,HWORK,114,0.)
      YS = YS - DEL
      CALL CORE(HWORK,122)
      WRITE(JUSCRN,3) (ADATA(LIM2+I),I=18,27)
    3 FORMAT('  ',10E12.4)
      CALL SYSSYM(XS,YS,SIZE,HWORK,122,0.)
      YS = YS - DEL
      CALL CORE(HWORK,54)
      WRITE(JUSCRN,4)(HDATA(LIMH2+I),I=55,56),(ADATA(LIM2+I),I=29,31)
    4 FORMAT('  ',2I5,3E14.6)
      CALL SYSSYM(XS,YS,SIZE,HWORK,54,0.)
      YS = YS - DEL
      CALL CORE(HWORK,110)
      WRITE(JUSCRN,5) (ADATA(LIM2+I),I=32,40)
    5 FORMAT('  ',9E12.4)
      CALL SYSSYM(XS,YS,SIZE,HWORK,110,0.)
      YS = YS - DEL
      CALL CORE(HWORK,120)
      WRITE(JUSCRN,6)(HDATA(LIMH2+I),I=81,82),(ADATA(LIM2+I),I=42,50)
    6 FORMAT('  ',2I5,9E12.4)
      CALL SYSSYM(XS,YS,SIZE,HWORK,120,0.)
      YS = YS - DEL
      CALL CORE(HWORK,72)
      WRITE(JUSCRN,7) (ADATA(LIM2+I),I=51,55)
    7 FORMAT('  ',5E14.6)
      CALL SYSSYM(XS,YS,SIZE,HWORK,72,0.)
      YS = YS - DEL
      CALL CORE(HWORK,52)
      WRITE(JUSCRN,8) (HDATA(LIMH2+I),I=111,120)
    8 FORMAT('  ',10I5)
      CALL SYSSYM(XS,YS,SIZE,HWORK,52,0.)
      RETURN
      END
