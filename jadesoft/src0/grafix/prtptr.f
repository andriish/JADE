C   10/02/86 604291051  MEMBER NAME  PRTPTRR  (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE PRTPTR( IPTPTR, XS, YS , DEL, SIZE )
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. HAGEMANN 29/04/86 :  PRINT BOS BANK TPTR
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
      LIM2 = IPTPTR
      LIMH2 = 2*LIM2
      CALL CORE(HWORK,110)
      WRITE(JUSCRN,1) (HDATA(LIMH2+I),I=1,18)
    1 FORMAT('  ',18I6)
      CALL SYSSYM(XS,YS,SIZE,HWORK,110,0.)
      YS = YS - DEL
      CALL CORE(HWORK,122)
      WRITE(JUSCRN,2) (ADATA(LIM2+I),I=10,19)
    2 FORMAT('  ',10E12.4)
      CALL SYSSYM(XS,YS,SIZE,HWORK,122,0.)
      YS = YS - DEL
      CALL CORE(HWORK,78)
      WRITE(JUSCRN,3) IDATA(LIM2+20),ADATA(LIM2+21),IDATA(LIM2+22),
     &               (ADATA(LIM2+I),I=23,25),IDATA(LIM2+26)
    3 FORMAT('  ',I6,E14.6,I6,3E14.6,I8)
      CALL SYSSYM(XS,YS,SIZE,HWORK,78,0.)
      YS = YS - DEL
      CALL CORE(HWORK,112)
      WRITE(JUSCRN,4)(ADATA(LIM2+I),I=27,33),(HDATA(LIMH2+I),I=67,68)
    4 FORMAT('  ',7E14.6,2I6)
      CALL SYSSYM(XS,YS,SIZE,HWORK,112,0.)
      YS = YS - DEL
      CALL CORE(HWORK,112)
      WRITE(JUSCRN,5) (ADATA(LIM2+I),I=35,38),(HDATA(LIMH2+I),I=77,78),
     &                (ADATA(LIM2+I),I=40,42)
    5 FORMAT('  ',4E14.6,2I6,3E14.6)
      CALL SYSSYM(XS,YS,SIZE,HWORK,112,0.)
      YS = YS - DEL
      CALL CORE(HWORK,74)
      WRITE(JUSCRN,6) (HDATA(LIMH2+I),I=85,96)
    6 FORMAT('  ',12I6)
      CALL SYSSYM(XS,YS,SIZE,HWORK,74,0.)
      YS = YS - DEL
      CALL CORE(HWORK,120)
      WRITE(JUSCRN,7) (ADATA(LIM2+I),I=49,57)
    7 FORMAT('   ',9E13.5)
      CALL SYSSYM(XS,YS,SIZE,HWORK,120,0.)
      YS = YS - DEL
      CALL CORE(HWORK,86)
      WRITE(JUSCRN,8) IDATA(LIM2+58),(ADATA(LIM2+I),I=59,64)
    8 FORMAT('   ',I5,6E13.5)
      CALL SYSSYM(XS,YS,SIZE,HWORK,86,0.)
      YS = YS - DEL
      CALL CORE(HWORK,100)
      WRITE(JUSCRN,9) (ADATA(LIM2+I),I=65,71)
    9 FORMAT('  ',7E14.6)
      CALL SYSSYM(XS,YS,SIZE,HWORK,100,0.)
      YS = YS - DEL
      CALL CORE(HWORK,98)
      WRITE(JUSCRN,10) IDATA(LIM2+72),(ADATA(LIM2+I),I=73,78),
     &                (IDATA(LIM2+I),I=79,80)
   10 FORMAT('  ',I6,6E13.5,2I6)
      CALL SYSSYM(XS,YS,SIZE,HWORK,98,0.)
      RETURN
      END
