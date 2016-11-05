C   26/04/84 405241539  MEMBER NAME  DRAWFD   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE DRAWFD( IVIEW )
C-----------------------------------------------------------------------
C
C
C   AUTHOR:    J. OLSSON        ?    :  DRAW FORWARD DETECTOR
C
C        MOD:  C. BOWDERY   26/04/84 :  TAKEN FROM FORWRD. DOES FW NOW
C        MOD:  J. NYE       18/05/84 :  USE TAGMRK - WORKS FOR MC TOO
C   LAST MOD:  C. BOWDERY   24/05/84 :  IVIEW=1  NOW USED
C
C     DRAW THE FORWRD DETECTOR FOR THE VIEW IVIEW.
C       IVIEW = 1   : SUPERPOSITION OF TAGGING SYSTEM IN R-PHI VIEWS
C       IVIEW = 2   : ZXD AND ZYD VIEWS
C       IVIEW = 3   : FW VIEW
C
C     TAGMRK IS CALLED TO DETERMINE THE VERSION NUMBER OF THE APPARATUS
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cgraph.for"
#include "cgeo2.for"
#include "cgeo3.for"
#include "cwktag.for"
C
      COMMON /CHEADR/ HEAD(108)
C
      DIMENSION ZSC(2)
      EQUIVALENCE (ZSC(1),ZMINBL)
C
C------------------  C O D E  ------------------------------------------
C
      GO TO ( 10 , 20 , 30 ) , IVIEW
C
C                            R PHI VIEW  (FC COMMAND)
C
  10  CALL FWCAP(0.,0.,0,1.,0)
      RETURN
C
C                            Z VIEWS  (ZXD,ZYD)
C
C                            CALL TAGMRK TO DETERMINE APPARATUS
C                            MARK, FOR MC AND REAL DATA
C
  20  CALL TAGMRK(*21)
  21  IF( MARK .EQ. 0 ) GO TO 1
      IF( MARK .EQ. 1 ) GO TO 641
C
C                            1983 CAPS, SIDE VIEWS IN ZXD,ZYD VIEWS
C
      CALL FW83
      GO TO 1000
641   CALL FWNEW
C
C                            1981-2 CAPS, SIDE VIEWS IN ZXD,ZYD VIEWS
C
      GO TO 1000
    1 CONTINUE
C
C                            1979-80 CAPS, SIDE VIEWS IN ZXD,ZYD VIEWS
C
      K = 1
      IF(LASTVW.EQ.11) K = 2
C
C++++                        CHAMBER DISPLAY COMMENTED OUT
C      DO 1602  I = 1,4
C        IF(K.EQ.1) CALL FWCHM2(I,CHZ(2,I),CHX(2,I),CHZ(3,I),CHX(3,I),
C     +                         CHZ(1,I),CHX(1,I))
C        IF(K.EQ.2) CALL FWCHM1(I,CHZ(1,I),CHY(1,I),CHZ(2,I),CHY(2,I))
C1602  CONTINUE
C++++
C
      DO 1603  I = 1,2
        CALL FWCAP1(ZSC(I),0.,I,K)
C
C++++                        COUNTER DISPLAY COMMENTED OUT
C       J = 2*I-1
C       CALL FWSCN1(ZMISC(I),0.,J)
C       J = J + 1
C       CALL FWSCN1(ZPLSC(I),0.,J)
C++++
C
 1603 CONTINUE
C
1000  CONTINUE
      RETURN
C
C                            FW VIEW  (TAGGING SYSTEM ONLY VIEW)
C
  30  FDX = 0.25 * (XMAX - XMIN)
      FDY = 0.0
      LL  = 0
      IF( DSPDTL(15) ) LL = 14
      DO  42  J = 1,2
        FDX = -FDX
        CALL FWCAP(FDX,FDY,LL,1.5,1)
  42  CONTINUE
      RETURN
      END
