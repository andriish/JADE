      SUBROUTINE BAKFIT(IB,ITYP)
      IMPLICIT INTEGER*2 (H)
#include "cpatlm.for"
#include "cworkeq.for"
#include "cworkpr.for"
       DIMENSION CHITR(9),HITIN(10)
       EQUIVALENCE (ADWRK(91),CHITR(1)),(HITIN(1),ADWRK(86))
       IB=0
       IREM=IXYF(1)
       IXYF(1)=LOR(IXYF(1),9)
       HPOLD=HPFREE
       IBTRK=NTR
       CALL FXYZ(IBTRK)
       HPTR0=HPFREE
       HPTR9=HPTR0+49
       HLDTR=50
      HPFREE=HPTR9+1
      IF(
     - HPFREE.LE.HPLAST
     -)THEN
      CALL XYFIT
      RES=CHITR(HNREL(NTR))
      RMS=WRK(HPTR0+22)
      NTOT=IWRK(HPTR0+23)
      NHT=HITIN(HNREL(NTR))
      IF(NHT.GT.0) RES=RES/FLOAT(NHT)
      IF(ITYP.EQ.1.AND.(RMS.GT.YBKK(2).OR.RES.GT.YBKK(3))) IB=-1
      IF(ITYP.EQ.2.AND.(RMS.GT.YBKK(4).OR.RES.GT.YBKK(5))) IB=-1
      IF(ITYP.EQ.3.AND.(RMS.GT.YBKK(6).OR.RES.GT.YBKK(7))) IB=-1
      IF(ITYP.EQ.4.AND.RMS.GT.YBKK(8)) IB=-1
      ELSE
      PRINT 33
 33   FORMAT(' +++++++++  NOT ENOUGH SPACE IN CWORK  +++++++')
      ENDIF
      IXYF(1)=IREM
      HPFREE=HPOLD
      RETURN
      END
