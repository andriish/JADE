C   26/11/79 302221020  MEMBER NAME  TR3DSP   (JADEGS)      FORTRAN
C-----------------------------------------------------------------------
      SUBROUTINE TR3DSP
C-----------------------------------------------------------------------
C
C        ROUTINE FOR GRAPHIC DISPLAY OF THE T3 SUBTRIGGER RESULTS.
C
C                                      L.H. O'NEILL 26 NOVEMBER 1979.
C                                      CHANGED 21/02/83  C. BOWDERY
C
C-----------------------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
#include "cdata.for"
#include "cgraph.for"
C
      DIMENSION XD(36),YD(36)
C
      DATA XD/ 3* 3500.,    2400.,    1200.,       0.,   -1200.,
     +           -2400., 5*-3500.,   -2400.,   -1200.,       0.,
     +            1200.,    2400., 2* 3500.,
     +         8*-4100., 8* 4050./
      DATA YD/       0.,    1200.,    2400., 5* 3400.,    2400.,
     +            1200.,       0.,   -1200.,   -2400., 5*-3225.,
     +           -2400.,   -1200.,
     +           -3080.,   -2420.,   -1760.,   -1100.,
     +            1100.,    1760.,    2420.,    3080.,
     +           -3080.,   -2420.,   -1760.,   -1100.,
     +            1100.,    1760.,    2420.,    3080./
C-----------------------------------------------------------------------
C
C          IF ONE OF THE X,Y OR Z VIEWS,  THEN ACCESS 'TRIG' BANK
C
      IF(LASTVW.GT.7) RETURN
      LEVL=0
      IPNTR=IDATA(IBLN('TRIG'))
      IF(IPNTR.LT.1) GO TO 1000
      LEVL=1
      IF(IDATA(IPNTR-2).NE.1) GO TO 1000
      LEVL=2
      IPNTR=IDATA(IPNTR-1)
      IF(IPNTR.LT.1) GO TO 1000
      LEVL=3
      IF(IDATA(IPNTR-2).NE.2) GO TO 1000
      LEVL=4
      IPNTR=IDATA(IPNTR-1)
      IF(IDATA(IPNTR-2).NE.3) GO TO 1000
C
C             SELECT UPPER AND LOWER ARRAY INDICES DEPENDING ON VIEW
C
      IPNTR = IPNTR*2
      LIML  = 1
      IF(LASTVW.GT.3) LIML = 21
      LIMU  = 20
      IF(LASTVW.GT.3) LIMU = 36
C
C             LSTRIG = T3 SUB-TRIGGER LEVEL(0 - 4)
C
      DO  1  ISTR = LIML,LIMU
        IWORD = HDATA(IPNTR+2+ISTR)
        LSTRIG= 0
        MASK  = 4096
        DO  2  KBKT = 1,4
          MASK  = SHFTR(MASK,1)
          ITEST = LAND(IWORD,MASK)
          IF(ITEST.EQ.0) GO TO 2
          LSTRIG= 5 - KBKT
          GO TO 3
  2     CONTINUE
C
  3     X = XD(ISTR)
        IF(LASTVW.LE.3) X = -X
        Y = YD(ISTR)
        CALL DNUM(LSTRIG,X,Y,100.,0.)
    1 CONTINUE
C
1000  RETURN
C1000 WRITE(JUSCRN,100) LEVL
C 100 FORMAT(' TR3DSP ERROR EXIT LEVEL',I4)
      END
