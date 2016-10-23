C   10/02/86 604291105  MEMBER NAME  PRATOFR  (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE PRATOF( XS, YS , DEL, SIZE )
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. HAGEMANN 29/04/86 :  PRINT BOS BANK ATOF
C
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cgraph.for"
C
      COMMON / CWORK1 / HWORK(70)
      COMMON / CWORK  / HDUM(8000),
     +                  HADC(2,42),HTDC(2,42),HTDC1(2,42),HADCF(2,16),
     +                  HTDCF(2,16),HTSPAR(16)
C
C
C------------------  C O D E  ------------------------------------------
C
      CALL CORE(HWORK,107)
      WRITE(JUSCRN,1)
    1 FORMAT(' NR   ADC(L)  ADC(R)   TDC(L)  TDC(R)   TDC1(L)  TDC1(R)
     & ADCF(L)  ADCF(R)   TDCF(L)  TDCF(R)   TDC(SPARE)')
      CALL SYSSYM(XS,YS,SIZE,HWORK,107,0.)
      YS = YS - .9*DEL
      DO 6 I = 1,42
         IF( I .GT. 16 ) GO TO 3
            CALL CORE(HWORK,106)
            WRITE(JUSCRN,2) I,(HADC(J,I),J=1,2),(HTDC(J,I),J=1,2),
     &         (HTDC1(J,I),J=1,2),(HADCF(J,I),J=1,2),(HTDCF(J,I),J=1,2),
     &                                                         HTSPAR(I)
    2       FORMAT(' ',I2,2X,I6,2X,I6,3X,I6,2X,I6,3(3X,I7,2X,I7),3X,I10)
            CALL SYSSYM(XS,YS,SIZE,HWORK,106,0.)
            GO TO 5
    3    CALL CORE(HWORK,55)
         WRITE(JUSCRN,4) I,(HADC(J,I),J=1,2),(HTDC(J,I),J=1,2),
     &                     (HTDC1(J,I),J=1,2)
    4    FORMAT(' ',I2,2X,I6,2X,I6,3X,I6,2X,I6,3X,I7,2X,I7)
         CALL SYSSYM(XS,YS,SIZE,HWORK,55,0.)
    5    YS = YS - .9*DEL
         IF( MOD(I,10) .EQ. 0 ) YS = YS -.2*DEL
    6 CONTINUE
      RETURN
      END
