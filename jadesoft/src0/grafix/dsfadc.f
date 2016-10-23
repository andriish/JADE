C   14/05/80 309241511  MEMBER NAME  DSFADC   (JADEGS)      FORTRAN
      SUBROUTINE DSFADC
      IMPLICIT INTEGER*2 (H)
C
C         SHOW FLASH ADC HISTOGRAMS FROM BANK FADC.   TEST FEATURE
C                              J.OLSSON 25.11.81
C                          LAST CHANGE 25.11.81
C
#include "cgraph.for"
#include "cdata.for"
      COMMON /CWORK1/ HMW(112)
C--
      CALL ERASE
      CALL TWINDO(0,4095,0,4095)
      XMINRE = XMIN
      XMAXRE = XMAX
      YMINRE = YMIN
      YMAXRE = YMAX
      XMIN = -40.
      YMIN = -10.
      XMAX = 80.
      YMAX = 110.
      CALL DWINDO(XMIN,XMAX,YMIN,YMAX)
C  DRAW DIAGRAM FRAME
      CALL MOVEA(0.,0.)
      CALL DRAWA(64.,0.)
      CALL DRAWA(64.,64.)
      CALL DRAWA(0.,64.)
      CALL DRAWA(0.,0.)
      CALL MOVEA(0.,48.)
      CALL DRAWA(64.,48.)
      CALL MOVEA(0.,32.)
      CALL DRAWA(64.,32.)
      CALL MOVEA(0.,16.)
      CALL DRAWA(64.,16.)
C  WRITE HORIZONTAL SCALE     0  32  64  96  128
      DX = 16.
      XLX = -3. - DX
      XLY = -3.
      LIT = 32
      ITX = -LIT
      CALL CHRSIZ(3)
      DO 242  I = 1,5
      XLX = XLX + DX
      ITX = ITX + LIT
      CALL MOVEA(XLX,XLY)
      CALL CORE(HMW,4)
      WRITE(JUSCRN,241) ITX
241   FORMAT(' ',I3)
      CALL EOUTST(4,HMW)
242   CONTINUE
      CALL MOVEA(50.,-5.)
      CALL CORE(HMW,5)
      WRITE(JUSCRN,271)
271   FORMAT(' TIME')
      CALL EOUTST(5,HMW)
C--
C  WRITE VERTICAL SCALE     0  32  0  32   0  32    0  32  64
      DY = 8.
      XLX = -4.
      XLY = -DY - .5
      LIT = 32
      ITX = - LIT
      DO 244  I = 1,9
      XLY = XLY + DY
      ITX = ITX + LIT
      IF(ITX.EQ.2*LIT.AND.I.LT.9) ITX = 0
      CALL MOVEA(XLX,XLY)
      CALL CORE(HMW,3)
      WRITE(JUSCRN,243) ITX
243   FORMAT(' ',I2)
      CALL EOUTST(3,HMW)
244   CONTINUE
      CALL MOVEA(-11.,53.)
      CALL CORE(HMW,6)
      WRITE(JUSCRN,272)
272   FORMAT(' PULSE')
      CALL EOUTST(6,HMW)
      CALL MOVEA(-11.,50.5)
      CALL CORE(HMW,7)
      WRITE(JUSCRN,273)
273   FORMAT(' HEIGHT')
      CALL EOUTST(7,HMW)
C--
C  WRITE HORIZONTAL TIC MARKS
      DX = 1.
      DY = .25
      DYB = .55
      DO 253  KK = 1,4
      XLX = 0.
      YYY = 0. + (KK-1)*16.
      DO 252  I = 1,64
      DDYY = DY
      IF((I/8)*8.EQ.I) DDYY = DYB
      XLX = XLX + DX
      XLY = YYY-DDYY
      IF(KK.EQ.1) XLY = YYY
      YUPP = XLY+DDYY
      IF(KK.NE.1) YUPP = YUPP + DDYY
      CALL MOVEA(XLX,XLY)
      CALL DRAWA(XLX,YUPP)
252   CONTINUE
253   CONTINUE
C--
C  WRITE VERTICAL TIC MARKS
      DY = 1.
      DX = .25
      DXB = .55
      XLX = 0.
      DO 263  KK = 1,4
      XLX = 0.
      XLY = 0. + (KK-1)*16.
      DO 262  I = 1,16
      DDXX = DX
      IF((I/4)*4.EQ.I) DDXX = DXB
      XLY = XLY + DY
      CALL MOVEA(XLX,XLY)
      CALL DRAWA(XLX+DDXX,XLY)
262   CONTINUE
263   CONTINUE
C--
C--
      CALL MOVEA(0.,-10.)
      CALL CHRSIZ(2)
      CALL CORE(HMW,34)
      WRITE(JUSCRN,3209)
3209  FORMAT(' F L A S H   A D C   D I S P L A Y')
      CALL EOUTST(34,HMW)
      CALL CHRSIZ(4)
C
C     DISPLAY HISTOGRAMS
C
      IPFADC = IDATA(IBLN('FADC'))
      IF(IPFADC.GT.0) GO TO 1606
      WRITE(6,1607)
1607  FORMAT('   NO FADC BANK IN THIS EVENT ..')
      GO TO 3200
1606  IPF = 2*IPFADC + 2
      DX = .5
      DY = .25
      DO 300  I = 1,4
      IPFF = IPF + (I-1)*256
      XLX = -DX
      XLY = 0. + (I-1)*16.
      CALL MOVEA(0.,XLY)
      DO 301  J = 1,128
      KPH = IPFF + J
      IPH = HDATA(KPH)
      XIPH = FLOAT(IPH)
      XLYY = XLY + XIPH*DY
      XLX = XLX + DX
      CALL DRAWA(XLX,XLYY)
      CALL DRAWA(XLX+DX,XLYY)
301   CONTINUE
300   CONTINUE
C***
C***                    WRITE CAPTION
C***
3200  INDES = -3
      CALL CAPMRK(INDES,IESUM)
      CALL TWINDO(0,4095,0,4095)
      XMIN = XMINRE
      XMAX = XMAXRE
      YMIN = YMINRE
      YMAX = YMAXRE
C     CALL SETSCL(LASTVW)
      RETURN
      END
