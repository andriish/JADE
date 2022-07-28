C   25/08/83 308251841  MEMBER NAME  JIPATR   (S1)          FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE JIPATR( P, R )
C-----------------------------------------------------------------------
C
C   AUTHOR:   E. ELSEN     17/05/79 :  FILL COMMON /CJPATR/ ==> PATR/12
C
C        MOD  E. ELSEN     13/11/81 :
C        MOD  J. HAGEMANN  17/01/83 :  S/R DAY2 NOW CALLED ONLY ONCE PER
C             R  RAMCKE             :  EVENT (IN EVTINI).
C                                   :  NEW COMMON /CDAY/ INTRODUCED.
C   LAST MOD  C. BOWDERY   19/08/83 :  MODIFIED COMMON /CWORK/.
C                                   :  HLIST(1600) I.E. 400 HITS/TRACK
C
C        P AND R ARE MOMENTUM AND PLACE AS IN TRCDET.
C        TRACK INFORMATION IS STORED IN  CJPATR,  KPATR  INDICATING  THE
C        TOTAL LENGTH OF ARRAY APATR. ( JADE COMP.NOTE #12 )
C        OVERFLOW CAUSES OVERWRITING OF LAST TRACK RECORD.
C        TYPE OF STARTING POINT ( APATR(KPATR+4) ) MUST BE SUPPLIED FROM
C        CALLING ROUTINE. TRACK NO ALSO SUPPLIED  FROM  CALLING  PROGRAM
C        NOW JPATR(KPATR+2)
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      DIMENSION P(10), R(5)
C
      COMMON / CWORK  / NHITS, INEXT, HLIST(1600)
C
      COMMON / CJDRCH / RDEC(4),
     *                  PSIIN(3),
     *                  RINCR(3),
     *                  FIRSTW(3),
     *                  FSENSW(3),
     *                  RDEPTH,
     *                  SWDEPL,
     *                  YSUSPN,
     *                  TIMDEL(6), ZMAX, ZOFFS, ZRESOL, ZNORM,ZAL,ZSCAL,
     *                  DRIDEV,DRICOS,DRISIN
C
      COMMON / CGEO1  / BKGAUS, RPIP,DRPIP,XRLPIP, RBPC,DRBPC,XRLBPC,
     *                  RITNK,DRITNK,XRLTKI, R0ROH,DR0ROH,XR0ROH,
     *                  R1ROH,DR1ROH,XR1ROH, R2ROH,DR2ROH,XR2ROH,
     *                  R3ROH,DR3ROH,XR3ROH, ROTNK,DROTNK,XRLTKO,
     *                  RTOF,DRTOF,XRTOF, RCOIL, DRCOIL, XRCOIL,
     *                  ZJM,DZJM,XRZJM, ZJP,DZJP,XRZJP,
     *                  ZTKM,DZTKM,XRZTKM, ZTKP,DZTKP,XRZTKP,
     *                  ZBPPL,ZBPMI,ZTOFPL,ZTOFMI,
     *                  XRJETC,
     *                  RLG,ZLGPL,ZLGMI,OUTR2,CTLIMP,CTLIMM,DELFI,
     *                  BLXY,BLZ,BLDEP,ZENDPL,ZENDMI,DEPEND,
     *                  XHOL1,XHOL2,YHOL1,YHOL2,BLFI
CAV     *                  XHOL1,XHOL2,YHOL1,YHOL2
CAV  Same size required
C
      COMMON / CDAY   / IDAY(5)
      COMMON / CJPATR / KPATR, APATR(1976)
C
      DIMENSION IPATR(1976), HPATR(1)
      EQUIVALENCE (IPATR(1),APATR(1),HPATR(1))
C
      DATA SIG / 1. /
C
C---------------------  C O D E  ---------------------------------------
C
C                                                TRACK NO.
C
      IPATR( KPATR + 1 ) = KPATR/IPATR(3) + 1
C                                                PROGR. IDENTIFIER
C                                           =TRACK NO, PARTICLE TYPE NOW
      HPATR( (KPATR + 2)*2 ) = P(8)
C                                                DATE OF GENERATION
      IPATR( KPATR +  3 ) = IDAY(1)*1000+IDAY(2)
C                                             FIRST POINT AND DIREC. COS
      APATR( KPATR +  5 ) = R(1)
      APATR( KPATR +  6 ) = R(2)
      APATR( KPATR +  7 ) = R(3)
      APATR( KPATR +  8 ) = P(1) / P(6)
      APATR( KPATR +  9 ) = P(2) / P(6)
      APATR( KPATR + 10 ) = P(3) / P(6)
C                                                FIT PARAMETERS
      R0X = R(1) + P(7)/(.3E-4*BKGAUS)*P(2)
      R0Y = R(2) - P(7)/(.3E-4*BKGAUS)*P(1)
      PRAD = SQRT( P(1)*P(1) + P(2)*P(2) )
C                                                CURVATURE
      SIG = P(7) * SIGN( 1., BKGAUS )
      APATR( KPATR + 19 ) = 0.3E-4*ABS(BKGAUS)/PRAD
C                                                RMIN
      APATR( KPATR + 20 ) = SQRT( R0X*R0X+R0Y*R0Y)
     *                      -1./   APATR( KPATR + 19 )
C                                                PHI
      APATR( KPATR + 21 ) = ATAN2( R0Y, R0X )
C                                                NOT USED
      APATR( KPATR + 22 ) = 0.
C                                                FIT TYPE IN R Z
      IPATR( KPATR + 29 ) = 1
C                                                DZ/DR
      APATR( KPATR + 30 ) = P(3)*R(4) /
     *                      AMAX1( 1.E-4,ABS(P(1)*R(1) + P(2)*R(2)))
C                                                Z0
      APATR( KPATR + 31 ) = R(3) - APATR(KPATR+30) * R(4)
C
      RETURN
C
C-----------------------------------------------------------------------
      ENTRY JUPATR( P, R )
C-----------------------------------------------------------------------
C
C  LOAD PARAMETERS BEHIND CHAMBER
C
C-----------------------------------------------------------------------
C
      PTOT = AMAX1( P(6), .001 )
      PRAD =SQRT(AMAX1(P(1)*P(1)+P(2)*P(2),1.E-6))
CCC  SECOND POINT AND DIREC. COSINE
      APATR( KPATR + 12 ) = R(1)
      APATR( KPATR + 13 ) = R(2)
      APATR( KPATR + 14 ) = R(3)
      APATR( KPATR + 15 ) = P(1) / PTOT
      APATR( KPATR + 16 ) = P(2) / PTOT
      APATR( KPATR + 17 ) = P(3) / PTOT
CCC  FIT TYPE IN R PHI
      IPATR( KPATR + 18 ) = 1
CCC  CURVATURE AT FIRST POINT
      APATR( KPATR + 27 ) = APATR(KPATR+19)*SIG
CCC  CURVATURE AT LAST POINT
      APATR( KPATR + 28 )=0.3E-4*BKGAUS*P(7)/PRAD
CCC  AVERAGE CURVATURE
      APATR( KPATR + 25 ) = ( APATR(KPATR+27) +
     *    APATR(KPATR+28) ) * .5
CCC  ERROR IN CURVATURE
      APATR( KPATR + 26 ) = ABS( APATR(KPATR+27) -
     *         APATR(KPATR+25) )
C
      RETURN
C
      ENTRY JAPATR( ITYPE )
C--------------------------------------------------------
C  ACCEPT TRACK. ITYPE DETERMINES TYPE OF MEASURED TRACK
C  POINTS. ONLY TRACKS WITH MORE THAN 3 HITS ARE REGISTERED.
C--------------------------------------------------------
C
      IF( NHITS .LT. 3 ) RETURN
C
      IF( ITYPE .NE. 0 ) IPATR(KPATR+4) = ITYPE
C
CCC  FILL CELL INFORMATION  INTO TRACK ARRAY
      ICLP = 34
      ICLOLD = -1
      JHIG = INEXT - 1
      JLOW = 1
C
      DO  100  J = JLOW, JHIG, 4
         ICL = HLIST(J) / 16 + 1
         IF( ICL .EQ. ICLOLD ) GOTO 100
         IPATR(KPATR+ICLP) = ICL
         ICLOLD = ICL
         IF( ICLP .LT. 40) ICLP = ICLP+1
  100 CONTINUE
      DO 200 J= ICLP, 48
  200 IPATR( KPATR + J ) = 0
CCC  CHI**2 AND NDF FOR R PHI FIT
      APATR(KPATR+23) = TIMDEL(1)/3.464
      IPATR( KPATR + 24 ) = NHITS
CCC  CHI**2 AND NDF FOR R Z FIT
      APATR( KPATR + 32 ) = ZRESOL
      IPATR( KPATR + 33 ) = NHITS
CCC  TYPE OF LAST POINT
      IPATR(KPATR+11) = IPATR(KPATR+4)
C
CCC  ACCEPT EVENT BY INCREMENTING AND POINTER UPDATING-----
      IPATR(2) = IPATR(2) + 1
      IPATR(5) = IPATR(5) + NHITS
      IPATR(7) = IPATR(7) + 1
      IF( KPATR .GE. 1928 ) RETURN
      KPATR = KPATR + IPATR(3)
CCC  PRESET NO OF NEXT PARTICLE TO THIS NO
      HPATR((KPATR+2)*2-1) = HPATR((KPATR+2-IPATR(3))*2-1)
C
      RETURN
      END
