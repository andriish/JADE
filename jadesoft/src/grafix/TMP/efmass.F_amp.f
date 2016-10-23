C   17/07/85 807251717  MEMBER NAME  EFMASS   (S)        M  FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE EFMASS
C-----------------------------------------------------------------------
C
C   AUTHOR:   R. EICHLER      ?    :  COMPUTES EFFECTIVE MASSES
C
C        MOD: J. OLSSON   22/02/83 :  HCONV PRINT,BRANCH
C        MOD: J. OLSSON    4/03/83 :  CONVERSION CORRECTION
C        MOD: J. OLSSON    4/05/83 :  INTERFACE /CVX/
C        MOD: C. BOWDERY  17/07/85 :  SMALL CHANGES
C        MOD: J. OLSSON   04/12/85 :  /CJJONI/ --> /CJIONI/  (IN JADEBD)
C        MOD: J. HAGEMANN 14/07/86 :  FOR NEW VERTEX ROUTINES
C   LAST MOD: J.OLSSON    20/05/87 :  IMPLICIT INTEGER*2 IN GETGAM
C
C        COMPUTES EFFECTIVE MASS OF CHARGED PARTICLES AND OR PHOTONS
C        INPUT GIVEN FROM SCREEN : TYPE AND TRACK NUMBER.
C        BOS NUMBER OF PATR BANK CAN BE ENTERED AS TRAILING NUMBER IN
C        COMMAND 'MASS'
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
#include "cgraph.for"
#include "cgeo1.for"
C
      COMMON / CJIONI / POTBEA, ZAROBE
     *                 ,POTTRI, ZAROTR
     *                 ,POTIVE, ZAROIV
     *                 ,POTRH0, ZAROR0
      COMMON / CVX    / NNPATR
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#include "mvertex0.for"
#include "mvertex1.for"
#include "mvertex2.for"
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      DIMENSION PP(6),IPOIN(20),HTEXT(5,7),RLIMS(2,6)
      DIMENSION HNNN(20),HNTP(20),PAR(20,4),PSAM(4),XMAS(20),ARMAS(6)
C
      DIMENSION HMW(5) ! PMF 26/10/99 DIMENSION statement added
C
      DATA ARMAS /0.0,.000511,.105659,.139567,.49367,.93828/
      DATA HTEXT /'  ','  ','  ','  ','  ','BE','AM','-P','IP','E ',
     $            'BP','-Z','AE','HL','ER','ID','-T','AN','K ','  ',
     $            'RO','HA','CE','LL',' 1','RO','HA','CE','LL',' 2',
     $            'RO','HA','CE','LL',' 3'/
      DATA RLIMS /115.,127.,127.,167.,167.,174.,174.,195.,370.,410.,
     $            580.,625./
C
C-----------------------------------------------------------------------
C
      XVT=0.0
      YVT=0.0
      ZVT=0.0
      IH=IDATA(IBLN('HEAD'))
      IF(NNPATR.LE.0) GO TO 3801
      NBK = NNPATR
      GO TO 2225
3801  NBK = ACMD
      IF(NBK.LE.0) GO TO 2222
2225  CALL CLOC(IPO,'PATR',NBK)
      IF(IPO.NE.0) GO TO 2224
      CALL DIAGIN('NOT EXISTING,PATR BANK NR',1,NBK,IPO,IPO,IPO,IPO,IPO)
2222  IPO=IDATA(IBLN('PATR'))
      IF(IPO.NE.0) GO TO 2224
      CALL TRMOUT(80,'PATR bank does not exist^')
      RETURN
2224  IPLGCL = IDATA(IBLN('LGCL'))
      IF(IPLGCL.NE.0) GO TO 2223
      CALL TRMOUT(80,'LGCL bank does not exist^')
      RETURN
2223  IPALGN=IDATA(IBLN('ALGN'))
      IF(IDATA(IPLGCL+21).NE.2) CALL LGCDIR(IPO,IPALGN,IPLGCL)
      NWPCL = IDATA(IPLGCL+25)
      IPOG = IDATA(IPLGCL+3) + IPLGCL - 1 - NWPCL
      NCLST = IDATA(IPLGCL+7)
      LO = IDATA(IPO+1)
      NTR = IDATA(IPO+2)
      LTR = IDATA(IPO+3)
      NN = 0
      JJ=0
      CALL TRMOUT(80,'  Part.Types:  -2 LG-clst,  -1 Gam,  1 Electron,
     $2 Mu,  3 Pi,  4 K,  5 P^')
1000  CALL TRMOUT(80,'             Enter Type and Track Number; Just RET
     $URN to finish input^')
      CALL FYRINT(ITYP,ITR,ID1,ID2)
      IF(ITYP.EQ.0) GO TO 1003
      IF(ITYP.GT.0) GO TO 1001
      IF(ITYP.LE.-3) GO TO 1011
C  PHOTON PART
      IF(ITR.GT.0.AND.ITR.LE.NCLST) GO TO 523
      GO TO 1006
523   IP = IPOG
      IGAM = 0
      DO 522  IGM = 1,NCLST
      IP = IP + NWPCL
      IF(ITYP .EQ. -2) GOTO 501
      IF(IDATA(IP+8).NE.0) GO TO 522
501   IGAM = IGAM + 1
      IF(IGAM.EQ.ITR) GO TO 524
522   CONTINUE
      GO TO 1006
524   IPOIN(NN+1)=IP
      JJ=JJ+1
      GO TO 1015
1011  CALL TRMOUT(80,'          Non-existing particle type. Try again^')
      GO TO 1000
1001  IF(ITYP.GT.5) GO TO 1011
      IF(ITR.GT.0.AND.ITR.LE.NTR) GO TO 1002
1006  CALL TRMOUT(80,'                 Non-existing track. Try again^')
      GO TO 1000
1002  IP = IPO + LO - LTR
      DO 1004  JTR = 1,NTR
      IP = IP + LTR
      IF(IDATA(IP+1).EQ.ITR) GO TO 1005
1004  CONTINUE
      GO TO 1006
1005  CALL MOMENT(IP,PX,PY,PZ,PT,PTOT,FI,THE)
1015  NN = NN + 1
      HNNN(NN) = ITR
      HNTP(NN) = ITYP
      ITYP = ITYP + 1
      IF(ITYP.LE.0) ITYP = 1
      XMAS(NN) = ARMAS(ITYP)
      PAR(NN,1) = PX
      PAR(NN,2) = PY
      PAR(NN,3) = PZ
      PAR(NN,4) = SQRT(PTOT*PTOT + XMAS(NN)*XMAS(NN))
      IF(ITYP.EQ.1) GO TO 4422
CORRECT HERE FOR ENERGY LOSS IN PIPE AND TANK
      PP(1) = PAR(NN,1)
      PP(2) = PAR(NN,2)
      PP(3) = PAR(NN,3)
      PP(4) = PAR(NN,4)
      PP(6) = SQRT(PP(1)**2+PP(2)**2+PP(3)**2)
      PP(5) = XMAS(NN)
      SINT = 1./SIN(THE)
C PASS THROUGH ROHACELL-0, INNER TANK, BEAMPIPECOUNTERS,BEAM PIPE
      STRAC = DR0ROH*SINT
      POT = POTRH0
      ZARO = ZAROR0
      ILV = 1
      CALL JEGAIN(PP,STRAC,POT,ZARO,*999)
      STRAC = DRITNK*SINT
      POT = POTIVE
      ZARO = ZAROIV
      ILV = 2
      CALL JEGAIN(PP,STRAC,POT,ZARO,*999)
      STRAC = DRBPC*SINT
      POT = POTTRI
      ZARO = ZAROTR
      ILV = 3
      CALL JEGAIN(PP,STRAC,POT,ZARO,*999)
      STRAC = DRPIP*SINT
      POT = POTBEA
      ZARO = ZAROBE
      ILV = 4
      CALL JEGAIN(PP,STRAC,POT,ZARO,*999)
      PAR(NN,1) = PP(1)
      PAR(NN,2) = PP(2)
      PAR(NN,3) = PP(3)
      PAR(NN,4) = PP(4)
      GO TO 4422
999   WRITE(6,867) ILV
867   FORMAT(' ERROR RETURN from JEGAIN at LEVEL ',I4)
C
4422  IF(NN.LT.20) GO TO 1000
1003  IF(NN.LE.1) RETURN
C IF LESS THEN 2 CHARGED PARTICLES DON'T DO VERTEX FIT
      IF(NN-JJ .GE. 2) GOTO 300
      CALL TRMOUT(80,'                              Enter vertex coordin
     1ates in mm (Integers!); ^')
      CALL TRMOUT(80,'                              Just RETURN for vert
     1ex = 0,0,0^')
      CALL FYRINT(IXVT,IYVT,IZVT,IDUM1)
      XVT=IXVT
      YVT=IYVT
      ZVT=IZVT
      CONTINUE !CALL CLRCON PMF 06/12/99
      GOTO 1510
C*************************
C   PERFORM VERTEX FIT   *
C*************************
 300  CONTINUE !CALL CLRCON PMF 06/12/99
      HCONV = 0
      CALL VTXINI
      CALL VTXPRE(IH,IPO)
      N=0
      J=0
      DO 320 M=1,NT
      DO 315 MM=1,NN
      IF(HNTP(MM) .LE. 0) GOTO 315
      IF(M .EQ. HNNN(MM)) GOTO 317
 315  CONTINUE
      IT(J+1)=0
      GOTO 320
 317  CONTINUE
      IF(IT(J+1) .EQ. 0) GOTO 321
      N=N+1
      IT(J+1)=2
      GO TO 320
321   WRITE(6,322) M
322   FORMAT(' Track ',I3,'  REJECTED for fit, by Vertex Program')
 320  J=J+ITDLEN
      NV=1
      IF(N+JJ .NE. NN) WRITE(JUSCRN,5000) N
5000  FORMAT(' Only',I4,' tracks accepted by Vertex Program')
      IF( N .NE. 2) GOTO 350
      IF(HNTP(1) .NE. 1 .OR. HNTP(2) .NE. 1) GOTO 350
      NV=0
      CALL VTXEE
      HCONV = 1
      IF(NV.NE.1) GO TO 361
      IF(IV(1).NE.4) GO TO 361
      GO TO 360
 361  HCONV = 2
      NV = 1
 350  CALL VERTEX
 360  CALL VTXAFT
C*****************************
C  GET RESULT OF VERTEX FIT  *
C*****************************
      R=V(2)**2+V(3)**2
      RL=SQRT(R+V(4)**2)
      R=SQRT(R)
      XVT=V(2)
      YVT=V(3)
      ZVT=V(4)
      IF(HCONV.EQ.2) WRITE(6,1379)
1379  FORMAT('  ***** Photon Conversion Fit FAILED, Standard Fit used **
     $*** ')
      IF(HCONV.NE.1) WRITE(6,1291)
1291  FORMAT('           ********* Vertex Fit Parameters ********* ')
      IF(HCONV.EQ.1) WRITE(6,1281)
1281  FORMAT(' ###   Photon Conversion:  Vertex Fit Parameters   ### ')
      NDF = 2*IV(8) - 3
      WRITE(6,1292) V(9),NDF
1292  FORMAT('  ChiSq / Ndf ',F6.2,' /',I2)
      WRITE(6,1293) IV(1),IV(8),NV
1293  FORMAT(' Quality Flag ',I3,'   Nr of used Tracks ',I3,'  Nr of Ver
     $tices found ',I3)
      WRITE(6,1294)
1294  FORMAT(' Vertex Coordinates         X                 Y
     $      Z')
      WRITE(6,1295) XVT,V(5),YVT,V(6),ZVT,V(7)
1295  FORMAT(20X,3(F6.1,' +- ',F4.1,4X))
      IRAD = 1
      DO 1296  KL = 1,6
      IF(R.GE.RLIMS(1,KL).AND.R.LT.RLIMS(2,KL)) IRAD = KL + 1
1296  CONTINUE
      DO 1297  KJ = 1,5
1297  HMW(KJ) = HTEXT(KJ,IRAD)
      WRITE(6,1298) R,(HMW(KL),KL=1,5)
1298  FORMAT('  Radius in RFI :',E12.4,10X,5A2)
      WRITE(6,1299) RL
1299  FORMAT('  Total Length :',E12.4)
      DO 1500 I=1,NN
      IF(HNTP(I) .LE. 0) GOTO 1500
      J=HNNN(I)
      INDEX=ITDLEN*(J-1)
      IF(IT(INDEX+1) .EQ. 0) GOTO 1500
      PH=T(3+INDEX)
      TH=1.5708-T(4+INDEX)
      XC=COS(PH)*SIN(TH)
      YC=SIN(PH)*SIN(TH)
      ZC=COS(TH)
      PTOT=PAR(I,1)**2+PAR(I,2)**2+PAR(I,3)**2
      PTOT=SQRT(PTOT)
      PAR(I,1)=XC*PTOT
      PAR(I,2)=YC*PTOT
      PAR(I,3)=ZC*PTOT
1500  CONTINUE
C -----FIND PHOTONS,RECALCULATE DIRECTION FROM NEW FOUND VERTEX
1510  DO 1600 I=1,NN
      IF(HNTP(I) .GE. 0) GOTO 1600
      IP=IPOIN(I)
      CALL GETGAM(IP,PX,PY,PZ,PTOT,XVT,YVT,ZVT)
      PAR(I,1)=PX
      PAR(I,2)=PY
      PAR(I,3)=PZ
      PAR(I,4)=PTOT
1600  CONTINUE
      PTOTSM = 0.
      PTOTM = 0.
      FACT = -1.
      DO 2000  J = 1,4
      IF(J.EQ.4) FACT = 1.
      PSAM(J) = 0.
      DO 2001  I = 1,NN
2001  PSAM(J) = PSAM(J) + PAR(I,J)
      IF(J.LT.4) PTOTM = PTOTM + PSAM(J)*PSAM(J)
2000  PTOTSM = PTOTSM + PSAM(J)*PSAM(J)*FACT
      PTOTSM = AMAX1(PTOTSM,1.E-08)
      PTOTM = AMAX1(PTOTM,1.E-08)
      XMASS = SQRT(PTOTSM)
      PTOTM = SQRT(PTOTM)
      IF(NN.NE.2) GO TO 2591
      COSOP = 0.
      PARB1 = 0.
      PARB2 = 0.
      DO 2592  IJN = 1,3
      PARB1 = PARB1+PAR(1,IJN)**2
      PARB2 = PARB2+PAR(2,IJN)**2
      COSOP = COSOP + PAR(1,IJN)*PAR(2,IJN)
2592  CONTINUE
      COSOP = COSOP/(SQRT(PARB1)*SQRT(PARB2))
2591  P1=0.
      P2=0.
      P3=0.
      DO 2005 I=1,NN
      P1=P1+PAR(I,1)
      P2=P2+PAR(I,2)
      P3=P3+PAR(I,3)
2005  CONTINUE
      AA=XVT*XVT+YVT*YVT+ZVT*ZVT
      PX=P1*P1+P2*P2+P3*P3
      AP=XVT*P1+YVT*P2+ZVT*P3
      XX=AA-AP*AP/PX
      IF(XX .GT. 0.)XX=SQRT(XX)
      WRITE(6,1391) XMASS
1391  FORMAT('                  Mass ------> ',E12.4)
      FI = ATAN2(PSAM(2),PSAM(1))
      COSTHE = PSAM(3)/PTOTM
      WRITE(6,1393) PTOTM,FI,COSTHE
1393  FORMAT(' Momentum ',E12.4,'    FI and COS(Theta)  ',2E12.4)
      WRITE(6,1392) PSAM
1392  FORMAT(' 4-Vector ',4E12.4)
      IF(NN.EQ.2) WRITE(6,1397) COSOP
1397  FORMAT(' COSINE Opening Angle ',F9.5)
      WRITE(JUSCRN,2002) XX
2002  FORMAT('     Dmin = ',E12.4)
*** PMF06/12/99      WRITE(JUSCRN,2003) ((HNNN(I),HNTP(I)),I=1,NN)
      do i=1,nn
         WRITE(JUSCRN,2003) HNNN(I),HNTP(I)
      enddo
*** PMF(end)
2003  FORMAT(' Particles and Types : ',8(I2,1X,I2,2X))
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE GETGAM(IP,PX,PY,PZ,PTOT,XVT,YVT,ZVT)
C-----------------------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
#include "cdata.for"
      COMMON /CLGPRM/ ITH,MAXCLS,IRLTHD,IRLTH2,IRLTH3, ZVRTX, DZVRTX
      COMMON /CLGDMS/ X0,RADIUS(6),RADSX0(6),THX0(4),
     $                ZEND(2),ZENDX0(2),ZWID(2),ZGAP(2),PHWID(2),
     $                ZECAP(4),ZECAPX(4),THECPX(2)
      ZTEMP=ZVRTX
      ZVRTX=ZVT
      JEG=1
      JPART=IDATA(IP+1)
      EGAM=ADATA(IP+2)
      EMEV=EGAM*1000.
      IF(JPART) 10,20,10
C***-------BARREL PHOTONS-----------
  20  ZAV=ADATA(IP+5)
      CALL LGAVDP(ZAV,EMEV,JEG,DEP)
      ZZ=ZAV-ZVT
      X =(RADIUS(3)+DEP)*COS(ADATA(IP+4))-XVT
      Y =(RADIUS(3)+DEP)*SIN(ADATA(IP+4))-YVT
      TR=SQRT(X*X+Y*Y+ZZ*ZZ)
      PX=X/TR*EGAM
      PY=Y/TR*EGAM
      PZ=ZZ/TR*EGAM
      PTOT=EGAM
      GOTO 30
C****------ENDCAP PHOTONS--------
  10  ZAV=ZECAP(JPART+2)
      CALL LGAVDP(ZAV,EMEV,JEG,DEP)
      ZZ=ZAV-ZVT+DEP*FLOAT(JPART)
      X =ADATA(IP+4)-XVT
      Y =ADATA(IP+5)-YVT
      TR=SQRT(X*X+Y*Y+ZZ*ZZ)
      PX=X/TR*EGAM
      PY=Y/TR*EGAM
      PZ=ZZ/TR*EGAM
      PTOT=EGAM
  30  CONTINUE
      ZVRTX=ZTEMP
      RETURN
      END
