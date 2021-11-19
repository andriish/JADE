C   12/09/79 412031748  MEMBER NAME  TOFMA2   (S)           FORTRAN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
       SUBROUTINE TOFMA2
C
C  DETERMINES TOF FOR EACH TRACK AND ITS QUALITY
C  CREATES BANK TOFR
C WARNING: TRANS MULTILIED INTO Z1 FOR MONTE CARLO DATA ; MAY BE WRONG F
C  REAL DATA
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
       INTEGER *2 HADC,HTDC,HB
       REAL *8 DYC1,DYC2,C,D,FNORM
       COMMON/TFPRM/ NRNC1,NRNC2,DYC1,DYC2,IFLC(42),CM(42),CP(42),
     -DTAU(42),WM(42),WP(42),SV(42),DM(42),DP(42),PEDLM(42),PEDLP(42)
       COMMON/CWORK/NR,RAW(5,42),NC,ICRT1(5,42),NTRK,ICRT2(50),TRK(5,50)
     - ,ITRC(50),NTC,ITRK(5,42),INFM(4),IRELT(14,50)
       COMMON/TFPED/ HADC(2,42),HTDC(2,42),HB(42)
      COMMON/TMTP/TAUM(42),TAUP(42)
       DIMENSION IRAW(5,42),IW(704)
       EQUIVALENCE (IRAW(1,1),RAW(1,1)),(IW(1),INFM(1)),
     -             (IW(5),IRELT(1,1))
       DIMENSION IAGREE(3),ZT(2),TMNUS(2),TPLUS(2),DELT(2)
      DATA IENTRY/0/
      DATA CVEL/ 2.9979246E2/,DTAU1/1./
      IF(IENTRY.NE.0)  GOTO 66
      IENTRY = IENTRY + 1
C     CALL HBOOK1( 555,' TOFMA2    $',100,-2.,2.)
      GOTO  66
   66 ZTOF = 0.
      ZTOF1 = 0.
       NNG= 0
       NOK= 0
       CALL SETSL(IW,0,704*4,0)
       CALL SETSL(TAUM,0,84*4,0.)
C
       DO 1000 I=1,42
       TRANS= 1./SV(I)
       MTPY= ICRT1(2,I)
            IF(MTPY.EQ.0) GO TO 1000
            IF(MTPY.EQ.1) GO TO 300
            IF(MTPY.EQ.2) GO TO 400
C/////////
                 MTPY= MTPY+2
                 IF(MTPY.GE.6) MTPY=5
                 DO 200 K=3,MTPY
                      NTK= ICRT1(K,I)
                      IRELT(1,NTK)= NTK
                      IRELT(2,NTK)= -MTPY+2
                      IRELT(3,NTK)= I
                      NNG= NNG+1
 200             CONTINUE
            GO TO 1000
C
C=========  ONE TRACK IN THE COUNTER
C
  300 NTR1= ICRT1(MTPY+2,I)
      Z1= TRK(1,NTR1)
      PATH1= TRK(3,NTR1)
      PMEV1= TRK(4,NTR1)
      TAUP1= RAW(3,I)+Z1*TRANS
      TAUM1= RAW(2,I)-Z1*TRANS
      C = FNORM(0)
      D = FNORM(0)
      C = C*0.566
      D = D*0.566
      TAUM1 = TAUM1 + C
      TAUP1 = TAUP1 + D
      TAUM(I) = TAUM1
      TAUP(I) = TAUP1
      NFLG= -1
            IF(HTDC(1,I).GT.10.AND.HTDC(1,I).LT.2048) GO TO 320
            TOF1= TAUP1
            GO TO 360
C
 320        IF(HTDC(2,I).LT.2048) GO TO 330
            TOF1= TAUM1
            GO TO 360
C
  330 IF(ABS(TAUM1-TAUP1).LE.3.*DTAU1) GO TO 350
                 IF(Z1.LT.0.) GO TO 340
                 TOF1= TAUP1
                 GO TO 360
 340  TOF1= TAUM1
      GO TO 360
C
 350  NFLG= 1
      TOF1= (TAUM1+TAUP1)/2.
      ZTOF1 = (RAW(2,I)-RAW(3,I))/2.*TRANS
 360  CALL CMASS( I,PMEV1,TOF1,PATH1,BET1,DBET1,EMAS12,DMAS1,
     -            PRO1,PKA1,PPI1,PEL1)
      CALL TFSTOR(NTR1,NFLG,I,TOF1,PATH1,BET1,DBET1,EMAS12,DMAS1,
     -            PRO1,PKA1,PPI1,PEL1,ZTOF1)
      NOK= NOK+1
CXXXXXXXXXX
      GO TO 1000
C
C=========   TWO HITS IN THE COUNTER
C
 400  NTR= ICRT1(3,I)
      Z= TRK(1,NTR)
      NTR1 = ICRT1(4,I)
      Z1 = TRK(1,NTR1)
            IF(Z.GT.Z1) GO TO 430
            NTRP= NTR
            NTR= NTR1
            NTR1= NTRP
            ZP1= Z
            Z= Z1
            Z1= ZP1
 430  PATH= TRK(3,NTR)
      P = TRK(4,NTR)
      P1 = TRK(4,NTR1)
      PATH1 = TRK(3,NTR1)
C//////////
      TOF = 100.
      TOF1 = 100.
      IFLG = -2
      IFLG1 =-2
      IF(HTDC(1,I).LT.10.OR.HTDC(1,I).GT.2047)  GOTO  550
      IF(HTDC(2,I).LT.10.OR.HTDC(2,I).GT.2047)  GOTO  550
      TMNUS(1) = RAW(2,I) - Z1/TRANS
      TPLUS(1) = RAW(3,I) + Z1/TRANS
      DELT(1) = TMNUS(1) - TPLUS(1)
      TMNUS(2) = RAW(2,I) - Z/TRANS
      TPLUS(2) = RAW(3,I) + Z/TRANS
      DELT(2) = TMNUS(2) - TPLUS(2)
      DELZ = (Z-Z1)/TRANS
      IF(ABS(DELZ).LE.1.5)  GOTO  560
      DO   431   L=1,2
      IAGREE(L) = 0
      IF(ABS(DELT(L)).LE.2.) IAGREE(L) = 1
  431 CONTINUE
      IAGREE(3) = 0
      TMM = TMNUS(1)+DELZ-TPLUS(2)
      TPP = TPLUS(2)+DELZ-TMNUS(1)
      IF(TMM.GT.-1..AND.TPP.GT.-1.) IAGREE(3) = 1
C
      IF(DELZ.LE.1.5)  GOTO  560
      IF(IAGREE(1)+IAGREE(2)+IAGREE(3).EQ.0)  GOTO  550
      IF(IAGREE(1)+IAGREE(2)+IAGREE(3).EQ.3)  GOTO  560
      IF(IAGREE(1).EQ.1.AND.IAGREE(2).EQ.0) GOTO  520
      IF(IAGREE(2).EQ.1.AND.IAGREE(1).EQ.0) GOTO  530
      IF(IAGREE(3).EQ.1.AND.IAGREE(1)+IAGREE(2).EQ.0) GOTO  540
      GOTO  550
  520 CONTINUE
      IFLG1 = 2
      TOF1= .5*(TMNUS(1)+TPLUS(1))
      CALL CMASS( I,P1,TOF1,PATH1,BET1,DBET1,EMAS12,DMAS1,
     -                 PRO1,PKA1,PPI1,PEL1)
      NOK = NOK + 1
      IF(IAGREE(3).GT.0)  GOTO  521
      NNG = NNG + 1
      GOTO  500
  521 IFLG1 = -2
      TOF = TPLUS(2)
      CALL CMASS( I,P,TOF,PATH,BET,DBET,EMAS2,DMAS,
     -                 PRO,PKA,PPI,PEL)
      NOK = NOK + 1
      GOTO  500
  530 CONTINUE
      IFLG = 2
      TOF = .5*(TMNUS(2)+TPLUS(2))
      CALL CMASS( I,P,TOF,PATH,BET,DBET,EMAS2,DMAS,
     -                 PRO,PKA,PPI,PEL)
      NOK = NOK + 1
      IF(IAGREE(3).GT.0)  GOTO  531
      NNG = NNG + 1
      GOTO 500
  531 IFLG = -2
      TOF1 = TMNUS(1)
      CALL CMASS( I,P1,TOF1,PATH1,BET1,DBET1,EMAS12,DMAS1,
     -                 PRO1,PKA1,PPI1,PEL1)
      NOK = NOK + 1
      GOTO  500
  540 CONTINUE
      IFLG = 2
      IFLG1 = 2
      TOF1= TMNUS(1)
      TOF  = TPLUS(2)
      CALL CMASS( I,P,TOF,PATH,BET,DBET,EMAS2,DMAS,
     -                 PRO,PKA,PPI,PEL)
      CALL CMASS( I,P1,TOF1,PATH1,BET1,DBET1,EMAS12,DMAS1,
     -                 PRO1,PKA1,PPI1,PEL1)
      NOK = NOK + 2
      GOTO  500
  560 CONTINUE
      TOF = .25*(TMNUS(1)+TPLUS(1)+TMNUS(2)+TPLUS(2))
      TOF1 = TOF
      CALL CMASS( I,P,TOF,PATH,BET,DBET,EMAS2,DMAS,
     -                 PRO,PKA,PPI,PEL)
      CALL CMASS( I,P1,TOF1,PATH1,BET1,DBET1,EMAS12,DMAS1,
     -                 PRO1,PKA1,PPI1,PEL1)
      NOK = NOK + 2
      GOTO  500
  550 NNG = NNG + 2
  500 CONTINUE
            CALL TFSTOR(NTR1,IFLG1,I,TOF1,PATH1,BET1,DBET1,EMAS12,DMAS1,
     -                  PRO1,PKA1,PPI1,PEL1,DEDX1)
            CALL TFSTOR(NTR,IFLG,I,TOF,PATH,BET,DBET,EMAS2,DMAS,
     -                  PRO,PKA,PPI,PEL,DEDX)

 1000  CONTINUE
C
C=========   HEADER PART
C
                 NXX= 0
                 DO 2000 I=1,NTRK
                 IF(IRELT(1,I).NE.0) GO TO 2000
                 CALL TFSTOR(I,10,0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.)
                 NXX= NXX+1
 2000            CONTINUE
            INFM(1)= NTRK
            INFM(2)= NOK
            INFM(3)= NNG
            INFM(4)= NXX
            LTFR= 14*NTRK+4
C
C=========   STORE RESULT INTO CURRENT BUFFER
C
       CALL BCRE(INTR,'TOFR',0,LTFR,*3000,IER)
       IF(INTR.EQ.0.AND.IER.EQ.2) GO TO 3000
       CALL BSTR(INTR,IW,LTFR)
       CALL BSAT(1,'TOFR')
       RETURN
3000   CONTINUE
C/////////
             WRITE(6,9300)
9300         FORMAT(1X,'BOS ERROR IN TOFMAS')
       RETURN
       END
