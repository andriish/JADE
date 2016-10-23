C   13/07/79 008022059  MEMBER NAME  ILCTRC   (SOURCE)      FORTRAN
      SUBROUTINE ILCTRC(NCH,LCH,ID,RD)
C
C     S.YAMADA    01-12-78  13:30
C     LAST MODIFICATION  13-07-79    22:55
C
C---- TRACES INNER CHARGED TRCKS TO THE LEAD GLASS FRONT SURFACE
C
      DIMENSION ID(2),RD(2),P(7),R(3)
C
      COMMON /CLGCHG/ NCHIND,NSTEP,CXDCHG(9,100)
      DIMENSION JBCCHG(9,100)
      EQUIVALENCE (CXDCHG(1,1),JBCCHG(1,1))
C---- NCHIND=NO. OF CHARGED PARTICLES DETECTED BY THE INNER DET.
C---- NSTEP=1:TRACING FOR THE L.G. COUNTERS,   =2:TRACED FURTHER.
C---- CXDCHG  CONTAINS INNER TRACK INFORMATION
C     JBCCHG(1,N)     HITTING PART
C     CXDCHG(2,N)     CHARGE
C     CXDCHG(3-5,N)   HITTING POSITION ON THE COILOR ON THE END CAP
C     CXDCHG(6-8,N)   DIRECTION COSIGNS
C     CXDCHG(9,N)     ABSOLUTE MOMENTUM
C
      COMMON /CLGDMS/ X0,RADIUS(6),RADSX0(6),THX0(4),
     1                ZEND(2),ZENDX0(2),ZWID(2),ZGAP(2),PHWID(2),
     2                ZECAP(4),ZECAPX(4),THECPX(2),
     3                ECXLST(24), ECYLST(24)
C
      COMMON /CLGMSB/ MSGVAL(5)
      DIMENSION AMSGVL(5)
      EQUIVALENCE (MSGVAL(1),AMSGVL(1))
C
      COMMON /CTPCNS/ EBMGEV, BKGAUS
      DATA INIT/0/
C
      IF(INIT) 1001,1000,1001
 1000 RAD1SQ = RADIUS(1)**2
      RAD3SQ = RADIUS(3)**2
      INIT = 1
C
 1001 NCHIND = 0
      NSTEP = 1
      IF(NCH.LE.0) RETURN
C
      NB = 0
        DO 1 N=1,NCH
C----   PICK UP EACH TRACK FROM THE INNER RESULT
        PABS = 0.
          DO 2 K=1,3
          P(K) = RD(NB+K+14)
    2     R(K) = RD(NB+K+11)
        RSQO = R(1)**2+R(2)**2
        PABS = P(1)**2+P(2)**2+P(3)**2
C////////////////////////////////
C       WRITE(6,900) N,R,(P(K),K=1,3),PABS
C900    FORMAT(' ILCTRC;#',I4,' R=',3F10.1,'   P,PABS=',4E12.4)
C////////////////////////////////
C
C----   CHECK THE POSITION
        IF( R(3).LT.ZEND(1) .OR. R(3).GT.ZEND(2) ) GO TO 8
        IF( RSQO.GT.RAD3SQ )  GO TO 8
        GO TO 6
C
C----   THE TRACK IS OUT OF THE INNER DETECTOR FROM THE BIGINNING.
    8   MSGVAL(1) = N
        AMSGVL(2) = R(1)
        AMSGVL(3) = R(2)
        AMSGVL(4) = R(3)
        CALL LGMESG( 8, 3)
        IF(R(3).LT.-1230.)  R(3)=-1230.
        IF(R(3).GT. 1130.)  R(3)= 1130.
        IF( RSQO.LE.RAD3SQ )  GO TO 6
        RSQO=790./SQRT(RSQO)
        R(1)=R(1)*RSQO
        R(2)=R(2)*RSQO
        GO TO 6
C       GO TO 25
C
C----   REMOVE STOPPING TRACKS
    6   IF( PABS ) 25,25,7
C
    7   IF(PABS.GT.0.8) GO TO 3
C
C----   FUNNY DIRECTION COS'ES.
        MSGVAL(1) = N
        AMSGVL(2) = PABS
        CALL LGMESG( 8,2 )
        GO TO 25
C
C----   RD(NB+25) IS CURVETURE IN 1./MM.
C----   PROTECTION FOR ZERO-FIELD CASE IS DONE TAKING 1-GAUSS.
        DATA EBMIN/0.3E-7/
    3   EB = 0.3E-4*BKGAUS
        IF( ABS(EB).LE.EBMIN ) EB = EBMIN
        PT = EB/RD(NB+25)
C       WRITE(6,910) N,EB,PT
C910    FORMAT(' N,EB,PT=',I4,2E12.4)
        IF(ABS(R(3)).LT.1.E-10 .AND. ABS(PT).LT.0.05) GO TO 25
        DXY = SQRT(P(1)**2+P(2)**2)
        P(6) = ABS(PT)/DXY
C
C----   CHARGE
        P(7) = 1.0
        IF(PT.LT.0.) P(7) = -1.
C
C----   TRACE THE TRACK UNTIL IT HITS THE END CAP OR THE COIL.
C       NO.OF CURLING
        NCURL = 0
        IPOUT = 1
C/////  CALL JSTEP(R,P)  /////   SUBROUTINE IS COPIED //////////
C           E.ELSEN   FOR JADE M.C.  COPIED FROM F11BAR.JMC.S
C               AND MODIFIED BY S.YAMADA05-12-78  11:20
C
            DATA           DRMAX/ 10.0/, DRMAXH/  5.0/
C                          DRMAXH = .5*DRMAX
C
C
            P(1) = P(1)*P(6)
            P(2) = P(2)*P(6)
            DZ = P(3)*DRMAX
C
            DT = DRMAX/ P(6)
            DTH = DRMAXH/ P(6)
            ODTH = EB * DTH
 4010       IF( PT.LT.0. ) ODTH = -ODTH
C
            CORREC = 1. - 2.*ODTH*ODTH
C
    4       DP1 =  P(2)*ODTH
            DP2 = -P(1)*ODTH
C
            R(1) = ( P(1) + DP1 ) * DT  + R(1)
            R(2) = ( P(2) + DP2 ) * DT  + R(2)
CCCCC       R(3) = P(3)*DT + R(3)
            R(3) = DZ + R(3)
C
            P(1) = ( P(1) + DP1+DP1 ) * CORREC
            P(2) = ( P(2) + DP2+DP2 ) * CORREC
C
C
C----   CHECK IF THE TRACK GOES THROUGH THE GAP OF THE L.G.COUNTERS.
        IF(R(3).LT.ZEND(1) .OR. R(3).GT.ZEND(2)) GO TO 25
        RSQ = R(1)**2+R(2)**2
        IF(RSQ.GE.RAD1SQ) GO TO 10
        IF(RSQ.GE.8.1E5) GO TO 4
        IF(R(3).LE.ZECAP(1)) GO TO 20
        IF(R(3).GE.ZECAP(3)) GO TO 21
C
C----   CHECK ENDLESS LOOP
        IF( RSQ-RSQO ) 41,40,40
   40   IPOUT = 1
        GO TO 44
C----   BACKWARD
   41   IF(IPOUT) 44,42,42
   42   IF(NCURL) 43,43,25
   43   IPOUT = -1
        NCURL = 1
C----   CONTINUE TRACING
   44   RSQO = RSQ
        GO TO 4
C
C----   COIL SURFACE IS HIT.
   10   JBC = 0
        GO TO 30
C
C----   Z<0 END CAP
   20   JBC = -1
        GO TO 30
C----   Z>0 END CAP
   21   JBC = 1
        GO TO 30
C
C----   THE TRACK DOES NOT HIT THE L.G.
   25   JBC = 100
C
   30   NCHIND = NCHIND+1
        IF(NCHIND.GT.100) GO TO 90
        JBCCHG(1,NCHIND) = JBC
C
          DO 31 K=1,3
   31     CXDCHG(K+2,NCHIND) = R(K)
        CXDCHG(6,NCHIND) = P(1)/P(6)
        CXDCHG(7,NCHIND) = P(2)/P(6)
        CXDCHG(8,NCHIND) = P(3)
C
        CXDCHG(2,NCHIND) = P(7)
        CXDCHG(9,NCHIND) = P(6)
C
C----   MOVE THE POINTER
        NB = NB+LCH
    1   CONTINUE
      RETURN
C
   90 MSGVAL(1) = NCHIND
      MSGVAL(2) = N
      CALL LGMESG( 8,1 )
      NCHIND=100
      RETURN
C
      END
