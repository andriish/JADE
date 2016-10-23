C   16/02/84 408232216  MEMBER NAME  DEDXVW   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE DEDXVW
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. OLSSON    05/05/80 :  DRAW DEDX V. MOMENTUM DIAGRAM
C
C        MOD: J. OLSSON    22/11/83 :  NEW THEORETICAL CURVES
C        MOD: C. BOWDERY   02/02/84 :  SMALLER CIRCLES FOR EACH TRACK
C        MOD: C. BOWDERY   03/02/84 :  BUG WITH ERROR BAR REMOVED
C        MOD: C. BOWDERY   15/02/84 :  ADD LABELS TO CURVES
C   LAST MOD: C. BOWDERY   21/03/84 :  BETTER MESSAGE IF NO TRACKS
C        MOD: J. OLSSON    23/08/84 :  NO CALL TO DEDXBN, DONE IN DEDXDS
C
C         SHOW DEDX VS MOMENTUM DIAGRAM FOR ONE EVENT
C
C         DEDX -2    DRAWS STANDARD DIAGRAM
C         DEDX -4    DRAWS THE SAME DIAGRAM BUT SMALLER ON CURRENT VIEW
C         DEDX -5    DRAWS DIAGRAM WITH LARGER DEDX SCALE
C
C         THE TRAILING INTEGER FROM DEDX COMMAND IS IN ACMD IN /CGRAPH/
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL SMALL, LARGE
C
#include "cgraph.for"
#include "cdata.for"
#include "chsym.for"
C
      COMMON / CALIBR / IPOINT(100)
      COMMON / CHEADR / HEAD(108)
      COMMON / CWORK1 / IER, NTR, TRES(10,60), HMW(132)
C
      DIMENSION XCAL(1000), ITRES(10,60)
      EQUIVALENCE (IPOINT(1),XCAL(1))
      EQUIVALENCE (TRES(1,1),ITRES(1,1))
C
C------------------  C O D E  ------------------------------------------
C
      NN = ACMD
C                            NN = -4:  SMALL VIEW
C                            NN = -2:  STANDARD VIEW
C                            NN = -5:  LARGE SCALE VIEW
      SMALL = .TRUE.
      LARGE = .FALSE.
      IF( NN .EQ. -5 ) LARGE = .TRUE.
      IF( NN .NE. -4 ) SMALL = .FALSE.
C
C                            DEFINE WINDOW SIZE
C
      IF( NN .NE. -4 ) GO TO 2801
        CALL TWINDO(370,1200,-30,800)
        GO TO 2802
2801  CALL ERASE
        CALL TWINDO(0,4095,0,4095)
2802  XMINRE = XMIN
      XMAXRE = XMAX
      YMINRE = YMIN
      YMAXRE = YMAX
      XMIN   = -10.
      YMIN   = -10.
      XMAX   =  50.
      YMAX   =  50.
      CALL DWINDO(XMIN,XMAX,YMIN,YMAX)
C
C                            DRAW DIAGRAM FRAME
C
      FRAC  = ALOG10(5.)
      XVMAX = 60.0 - FRAC * 15.0
      CALL MOVEA(0.,0.)
      CALL DRAWA(XVMAX,0.)
      CALL DRAWA(XVMAX,25.)
      CALL DRAWA(0.,25.)
      CALL DRAWA(0.,0.)
C
C                            DRAW SCALE MARKERS
C
      CALL CHRSIZ(2)
      CALL MOVEA(-2.0, -2.0)
      IF( NN .EQ. -4 ) GO TO 501
C
C                            WRITE TO INTERNAL ARRAY HMW
C                            THEN OUTPUT EBCDIC CHARACTERS ON SCREEN
C
      CALL CORE(HMW,5)
      WRITE(JUSCRN,245)
245   FORMAT(' 0.05')
      CALL EOUTST(5,HMW)
C
501   FRAC1 = (1.0 - FRAC) * 15.0
      CALL MOVEA(FRAC1, 0.0)
      CALL DRAWA(FRAC1, 0.7)
      CALL MOVEA(FRAC1 - 2.0, -2.0)
      IF(NN.EQ.-4) GO TO 502
      CALL CORE(HMW,4)
      WRITE(JUSCRN,247)
247   FORMAT(' 0.1')
      CALL EOUTST(4,HMW)
C
502   CALL MOVEA(FRAC1 + FRAC * 15.0, 0.0)
      CALL DRAWA(FRAC1 + FRAC * 15.0, 0.4)
      CALL MOVEA(FRAC1 + 15.0,  0.0)
      CALL DRAWA(FRAC1 + 15.0,  0.7)
      CALL MOVEA(FRAC1 + 14.0, -2.0)
      IF(NN.EQ.-4) GO TO 503
      CALL CORE(HMW,2)
      WRITE(JUSCRN,228)
228   FORMAT(' 1')
      CALL EOUTST(2,HMW)
C
503   CALL MOVEA(FRAC1 + (1.0 + FRAC) * 15.0, 0.0)
      CALL DRAWA(FRAC1 + (1.0 + FRAC) * 15.0, 0.4)
      CALL MOVEA(FRAC1 + 30.0,  0.0)
      CALL DRAWA(FRAC1 + 30.0,  0.7)
      CALL MOVEA(FRAC1 + 29.0, -2.0)
      IF(NN.EQ.-4) GO TO 504
      CALL CORE(HMW,3)
      WRITE(JUSCRN,229)
229   FORMAT(' 10')
      CALL EOUTST(3,HMW)
C
504   CALL MOVEA(FRAC1 + (2.0 + FRAC) * 15.0, 0.0)
      CALL DRAWA(FRAC1 + (2.0 + FRAC) * 15.0, 0.4)
      CALL MOVEA(FRAC1 + 42.5, -2.0)
      IF(NN.EQ.-4) GO TO 505
      CALL CORE(HMW,4)
      WRITE(JUSCRN,230)
230   FORMAT(' 100')
      CALL EOUTST(4,HMW)
C
505   CALL MOVEA(0.0, 5.0)
      CALL DRAWA(0.7, 5.0)
C
      IF(NN .NE. -4 .AND. .NOT. LARGE) CALL DNUM(5,-2.0,4.5,1.0,0.0)
      IF(LARGE) CALL DNUM(40,-2.5,4.5,1.0,0.0)
      CALL MOVEA(0.0, 10.0)
      CALL DRAWA(0.7, 10.0)
C
      IF(NN .NE. -4 .AND. .NOT. LARGE) CALL DNUM(10,-2.5,9.5,1.0,0.0)
      IF(LARGE) CALL DNUM(80,-2.5,9.5,1.0,0.0)
      CALL MOVEA(0.0, 15.0)
      CALL DRAWA(0.7, 15.0)
C
      IF(NN .NE. -4 .AND. .NOT. LARGE) CALL DNUM(15,-2.5,14.5,1.0,0.0)
      IF(LARGE) CALL DNUM(120,-3.0,14.5,1.0,0.0)
      CALL MOVEA(0.0, 20.0)
      CALL DRAWA(0.7, 20.0)
C
      IF(NN .NE. -4 .AND. .NOT. LARGE) CALL DNUM(20,-2.5,19.5,1.0,0.0)
      IF(LARGE) CALL DNUM(160,-3.0,19.5,1.0,0.0)
C
      IF(NN .NE. -4 .AND. .NOT. LARGE) CALL DNUM(25,-2.5,24.5,1.0,0.0)
      IF(LARGE) CALL DNUM(200,-3.0,24.5,1.0,0.0)
C
C                            DEDX ANALYSIS
C
C     CALL DEDXBN
      NTRR = MIN0(NTR,60)
      IF( NTRR .NE. 0 ) GO TO 3201
      CALL TRMOUT(80,'THERE ARE NO TRACKS TO DRAW^')
      GO TO 998
C
 3201 DO  3202  ITR = 1,NTRR
        DEDX   = TRES(2,ITR)
        ERDEDX = TRES(3,ITR)
        PDEDX  = TRES(9,ITR)
C
        IF(DEDX  .LT. 0.1)                      GO TO 3202
        IF(DEDX  .GT. 80  .AND. .NOT. LARGE)    GO TO 3202
        IF(PDEDX .GT. 100 .OR. PDEDX .LT. 0.05) GO TO 3202
C
        PDEDX  = ALOG10(PDEDX)
        PDEDX  = FRAC1 + (1.0 + PDEDX) * 15.0
        DEDXSC = DEDX
        ERRDSC = ERDEDX
C
        IF(LARGE) DEDXSC = DEDX   * 0.125
        IF(LARGE) ERRDSC = ERDEDX * 0.125
        BAR = .15
        IF(LARGE) BAR = BAR * 0.125
C
C                            DRAW 'SOLID' CIRCLE AT DATA POINT
C
        CALL PLYGON(16,BAR,PDEDX,DEDXSC,0)
C
        CALL MOVEA( PDEDX-BAR, DEDXSC )
        CALL DRAWA( PDEDX+BAR, DEDXSC )
C
C                            NOTE THAT SQRT(2.3) IS AN EMPIRICAL FACTOR
C
        CALL MOVEA( PDEDX+BAR/SQRT(2.3), DEDXSC+BAR/SQRT(2.3) )
        CALL DRAWA( PDEDX-BAR/SQRT(2.3), DEDXSC-BAR/SQRT(2.3) )
        CALL MOVEA( PDEDX+BAR/SQRT(2.3), DEDXSC-BAR/SQRT(2.3) )
        CALL DRAWA( PDEDX-BAR/SQRT(2.3), DEDXSC+BAR/SQRT(2.3) )
C
C                            DRAW ERROR BAR
C
        CALL MOVEA(PDEDX,DEDXSC+ERRDSC)
        CALL DRAWA(PDEDX,DEDXSC-ERRDSC)
C
C                            DRAW TRACK NUMBER
C
        DESIZE = .5
        IF(NN.EQ.-4) DESIZE = 1.5
        CALL DNUM(ITR, PDEDX + 0.35, DEDXSC + 0.35, DESIZE, 0.0)
3202  CONTINUE
C
C                            DRAW "THEORETICAL" CURVES FROM A.WAGNER
C
       CALL DRTHCV( FRAC1, LARGE, SMALL )
C
C                            DRAW AXES CAPTIONS (LOWER CASE!)
C
1441  IF(NN.EQ.-4) GO TO 998
      CALL CHRSIZ(2)
      CALL MOVEA(40.,-2.5)
      CALL CORE(HMW,6)
      WRITE(JUSCRN,3209)
3209  FORMAT(' GeV/c')
      CALL EOUTST(6,HMW)
C
      CALL MOVEA(22.,-2.5)
      CALL CORE(HMW,9)
      WRITE(JUSCRN,3210)
3210  FORMAT(' Momentum')
      CALL EOUTST(9,HMW)
C
      CALL MOVEA(-6.,22.5)
      CALL CORE(HMW,7)
      WRITE(JUSCRN,3211)
3211  FORMAT(' keV/cm')
      CALL EOUTST(7,HMW)
C
      CALL MOVEA(-6.,12.5)
      CALL CORE(HMW,6)
      WRITE(JUSCRN,3212)
3212  FORMAT(' dE/dx')
      CALL EOUTST(6,HMW)
C***
C***                    WRITE CAPTION
C***
      INDES = -1
      CALL CAPMRK(INDES,IESUM)
998   CALL TWINDO(0,4095,0,4095)
      XMIN = XMINRE
      XMAX = XMAXRE
      YMIN = YMINRE
      YMAX = YMAXRE
      CALL SETSCL(LASTVW)
      RETURN
      END
