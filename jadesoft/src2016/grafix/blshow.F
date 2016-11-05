C   13/02/80 504171555  MEMBER NAME  BLSHOW   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE BLSHOW
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. OLSSON       ?    :  DRAW LG BLOCK NUMBERS
C
C       MOD:   J. OLSSON   16/12/84 :
C  LAST MOD:   C. BOWDERY  17/04/85 :  COSMETIC CHANGES ONLY
C
C
C       DISPLAY ROUTINE FOR JADE, SHOWING LEAD GLASS BLOCK NUMBERS,
C       SPINNING BLOCKS AS KILLED IN LGERSE AND AS FAR AS KNOWN.
C       AND DEAD BLOCKS
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL FL18,FL22,FL24
C
      COMMON / CPROJ  / XMINR,XMAXR,YMINR,YMAXR,IPRJC,FL18,FL22,FL24
      COMMON / CWORK1 / DMY1(4),X1,Y1,DMY2(2),X2,Y2,ZET,X3,Y3,X4,Y4
      COMMON / CJTRIG / PI,TWOPI
      COMMON / CHEADR / HEAD(108)
C
#include "cgraph.for"
#include "cgeo1.for"
#include "cdata.for"
C
      DIMENSION HBLACK(40)
C
C------------------  C O D E  ------------------------------------------
C
      NN = ACMD + 1
      IF( NN .NE. 1 ) GO TO 100
      GO TO 101
102   CALL TRMOUT(80,'Code 0:   Return;  Code 1:  List;  Code 2:  Show b
     +lock numbers.^')
      CALL TRMOUT(80,'Code 3:   Show blocks killed in bank ALGN^')
      CALL TRMOUT(80,'Code 4:   Show spinning blocks as far as known^')
      CALL TRMOUT(80,'Code 5:   Show dead blocks^')
      CALL TRMOUT(80,'Code 345: Options 3,4 and 5 together^')
      CALL TRMOUT(80,'Code 6:   Explain pulse height code^')
      CALL TRMOUT(80,'Code 7:   Print cluster number for spinners ^')
101   CALL TRMOUT(80,'Enter option.  1=List^')
      NN = TERNUM(DUM) + 1
100   IF(NN.GE.1.AND.NN.LE.7.AND.LASTVW.EQ.13) GO TO 200
      IF(NN.EQ.346.AND.LASTVW.EQ.13) GO TO 400
      IF(NN.EQ.3.AND.LASTVW.EQ.12) GO TO 200
      IF(NN.EQ.8) GO TO 200
      CALL TRMOUT(80,' Not available in this view^')
      RETURN
200   GO TO (999,102,300,400,500,600,700,800),NN
C*** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** ***
C
C  DISPLAY SINGLE BLOCK NUMBERS
C
300   IF(LASTVW.EQ.13) CALL BLNUMB
      IF(LASTVW.EQ.12) CALL BLNMFW
      GO TO 999
C*** *** HARD SPINNERS (BAD BLOCKS)  *** *** *** *** *** *** *** *** ***
400   IPALGL = IDATA(IBLN('ALGL'))
      IPALGN = IDATA(IBLN('ALGN'))
      IF(IPALGL.GT.0.AND.IPALGN.GT.0) GO TO 1301
401   CALL TRMOUT(80,'ERROR: ALGL and/or ALGN do not exist^')
      GO TO 999
1302  CALL TRMOUT(80,'ERROR: ALGN is empty^')
      GO TO 999
1301  LNG = IDATA(IPALGL)
      IPL = 2*(IPALGL+3) + 1
      IPLEND =2*(IPALGL+LNG)
      NWO=IDATA(IPALGN)
      IF(NWO.LE.3) GO TO 1302
      IPN=2*IPALGN + 7
      IPNEND=IPN + 2*NWO - 7
      DO 1303  IPLL = IPL,IPLEND,2
      IADLGL = HDATA(IPLL)
      DO 1304 IPNN=IPN,IPNEND,2
      IADLGN = HDATA(IPNN)
      IF(IADLGN.EQ.IADLGL) GO TO 1303
1304  CONTINUE
C-- BLOCK FOUND THAT ONLY APPEARS IN LGCL:   DISPLAY IT
      CALL XYBLK(IADLGL,X1,Y1,DXEB,DYEB)
      X2 = X1
      Y2 = Y1 + DYEB
      Y3 = Y2
      X3 = X2 + DXEB
      X4 = X3
      Y4 = Y1
      CALL CRICRO(0.,0.)
1303  CONTINUE
      IF(NN.EQ.346.AND.LASTVW.EQ.13) GO TO 500
      GO TO 999
C*** *** SOFT SPINNERS * *** *** *** *** *** *** *** *** *** *** *** ***
500   CONTINUE
      HRUN = HEAD(18)
      IPALGN = IDATA(IBLN('ALGN'))
      IF(IPALGN.GT.0) GO TO 501
      GO TO 401
501   NWO=IDATA(IPALGN)
      IF(NWO.LE.3) GO TO 1302
      IPN=2*IPALGN + 7
      IPNEND=IPN + 2*NWO - 7
      DO 504 IPNN=IPN,IPNEND,2
      IADLGN = HDATA(IPNN)
      CALL SPINNR(IADLGN,IER,HRUN)
      IF(IER.EQ.0) GO TO 504
C-- SPINNING BLOCK FOUND:   DISPLAY IT
      CALL XYBLK(IADLGN,X1,Y1,DXEB,DYEB)
      EX = .05*DXEB
      EY = .05*DYEB
      DO 1305 I = 1,4
      XLO = X1 + (I-1)*EX
      YLO = Y1 + (I-1)*EY
      XHI = X1 - (I-1)*EX + DXEB
      YHI = Y1 - (I-1)*EY + DYEB
1305  CALL RECTAN(XLO,YLO,XHI,YHI,0)
504   CONTINUE
      IF(NN.EQ.346.AND.LASTVW.EQ.13) GO TO 600
      GO TO 999
C*** *** *** DEAD BLOCKS *** *** *** *** *** *** *** *** *** *** *** ***
600   CALL LGDEAD(HEAD(17),HBLACK)
      NBLACK = HBLACK(1)
      IF(NBLACK.LE.39) GO TO 113
      WRITE(6,114) NBLACK
114   FORMAT(' *** WARNING: LGDEAD error. No. of dead blocks = ',I5)
      GO TO 999
113   IF(NBLACK.LE.0) GO TO 999
      DO 601 I = 1,NBLACK
      IPNN = HBLACK(1+I)
      CALL XYBLK(IPNN,X1,Y1,DXEB,DYEB)
      XC = X1 + .5*DXEB
      YC = Y1 + .5*DYEB
      DO 602  K = 1,3
      BLRR = (0.6-K*.1)*DXEB
      CALL PLYGON(9,BLRR,XC,YC,0)
602   CONTINUE
601   CONTINUE
      GO TO 999
C*** *** *** PULSE HEIGHT CODE * *** *** *** *** *** *** *** *** *** ***
700   WRITE(6,701)
701   FORMAT(' 0: 0-10 MeV   1: 10-20 MeV  ...  9:  90-100 MeV')
      WRITE(6,702)
702   FORMAT(' A: 100-200 MeV  ... C: 300-400 MeV  ...  E: 500-600 MeV')
      WRITE(6,703)
703   FORMAT(' G: 700-800 MeV  ... I: 900-1000 MeV')
      WRITE(6,704)
704   FORMAT(' J: 1-2 GeV  ...  L: 3-4 GeV  ... N: 5-6 GeV')
      WRITE(6,705)
705   FORMAT(' P: 7-8 GeV  ...  R: 9-10 GeV')
      WRITE(6,706)
706   FORMAT(' S: 10-20 GeV   T: > 20 GeV')
      GO TO 999
C
C  WRITE PHOTON NUMBER FOR SPINNING BLOCK, IF ANY
C
800   CONTINUE
      HRUN = HEAD(18)
      IF(HRUN.GT.99) GO TO 805
      CALL TRMOUT(80,'  No spinners in Monte Carlo ...^')
      GO TO 999
805   IPALGN=IDATA(IBLN('ALGN'))
      IPLGCL=IDATA(IBLN('LGCL'))
      IF(IPALGN.GT.0.AND.IPLGCL.GT.0) GO TO 801
      CALL TRMOUT(80,' ALGN and/or LGCL do not exist ^')
      GO TO 999
801   NCLST = IDATA(IPLGCL+7)
      IF(NCLST.EQ.0) CALL TRMOUT(80,' No clusters...^')
      IF(NCLST.EQ.0) GO TO 999
      NWPCL = IDATA(IPLGCL+25)
      JUMPCL = IDATA(IPLGCL+2)
      IP = IDATA(IPLGCL+3) + IPLGCL - 1 - NWPCL
      DO 802  IGM = 1,NCLST
      IP = IP + NWPCL
      IF(IDATA(IP+8).NE.0) GO TO 802
      IPBL = HDATA(2*(IPLGCL+JUMPCL+IGM-1)-1)
      IPBL2 = HDATA(2*(IPLGCL+JUMPCL+IGM-1))
      IF(IPBL.NE.IPBL2) GO TO 802
      NBL = HDATA(2*(IPALGN + 3 + IPBL)-1)
      IER = 0
      CALL SPINNR(NBL,IER,HRUN)
      IF(IER.NE.0) WRITE(6,803) IGM,NBL
803   FORMAT(' ',10X,'  Cluster no. ',I3,' is a spinner, block no. ',I5)
802   CONTINUE
999   CONTINUE
      RETURN
      END
