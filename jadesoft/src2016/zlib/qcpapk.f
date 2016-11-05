C   26/01/85 804290922  MEMBER NAME  QCPAPK   (S)           FORTRAN77
      SUBROUTINE QCPAPK( NST, N, IMODE )
C-----------------------------------------------------------
C  VERSION OF 01/06/87       LAST MOD 28/04/88    M ZIMMER
C  ROUTINE FROM E ELSEN  OFFICIAL VERSION
C  ADDITIONAL ENTRY INCLUDED
C  ADDITIONAL PARTICLES INCLUDED FOR B/C TRACEBACK
C  LOAD PALL DATA INTO VECSUB COMMON
C  IMODE=0    CHARGED AND GAMMA
C       =1    CHARGED ONLY
C       =2    CHARGED ONLY, ALL PI
C
C    VECSUB FILLING SCHEME
C   P( 1..7,*)   PX,PY,PZ,E,M,P,CHARGE
C  HP(15   ,*)   0
C  HP(16   ,*)   PARTICLE TYPE ( JADE MC-CONVENTION )
C  HP(17   ,*)   0
C  HP(18   ,*)   0
C  HP(19   ,*)   PALL NUMBER
C  HP(20   ,*)   PARTICLE ORIGIN (> 0 MEANS LEPTON )
C-----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / BCS / IW(1)
      DIMENSION RW(1), HW(1)
      EQUIVALENCE (HW(1),RW(1),IW(1))
C
      COMMON  P(10,100)
      DIMENSION IP(10,100),HP(20,1)
      EQUIVALENCE (IP(1,1),P(1,1),HP(1,1))
C
      N = 0
      L = 0
      NPPALL = IW(IBLN('PALL'))
      IF( NPPALL.GT.0 .AND. IW(NPPALL+4).GT.0 ) THEN
        NLUN = IW(NPPALL+4)
        IP0 = NPPALL + IW(NPPALL+1)
        IP9 = IP0 + IW(NPPALL+2)*(NLUN-1)
        DO 1001 LP = IP0, IP9, IW(NPPALL+2)
          L = L + 1
           IF (IW ( LP+9) .LT. -2 ) GOTO 1001
           DO 1010 JP = IP0, IP9, IW(NPPALL+2)
C                                      SKIP IF PARTICLE IS PARENT
             IF( IW(JP+8).EQ.L )   GOTO 1001
 1010      CONTINUE
           CHARGE = IW(LP+6)
           IF( CHARGE.NE.0. .OR.
     *        (IMODE.EQ.0.AND.IABS(IW(LP+7)).EQ.1) ) THEN
             N = N + 1
             P(1,NST+N) = RW(LP+1)
             P(2,NST+N) = RW(LP+2)
             P(3,NST+N) = RW(LP+3)
             PTOT=SQRT(RW(LP+1)**2+RW(LP+2)**2+RW(LP+3)**2)
             XM = RW(LP+5)
             HP(16,NST+N) = IABS(IW(LP+7))
             IF( IMODE.EQ.2.AND.CHARGE.NE.0. ) THEN
               XM = .1396
               HP(16,NST+N) = 0
             ENDIF
             P(4,NST+N) = SQRT( PTOT**2 + XM**2 )
             P(5,NST+N) = XM
             P(6,NST+N) = PTOT
             P(7,NST+N) = CHARGE
             HP(15,NST+N) = 0
             HP(17,NST+N) = 0
             HP(18,NST+N) = 0
             HP(19,NST+N) = L
             HP(20,NST+N) = 0
C                                           CATEGORIZE
             IFC = MAX0( IABS(IW(NPPALL+9))-2, 1 )
             ISC = 1
             J = L
C                                      POINTER TO PARENT IS JP
             IPPAR = IP0 + (IW(LP+8)-1)*IW(NPPALL+2)
C                                      STOP IF PARENT IS A PARTON
             KF = MOD(IABS( IW(IPPAR+7) ), 1000 )
C
             IF( KF.LT.100 ) GO TO 300
             IF( (101 .GT. KF .OR. KF .GT. 104) .AND.
     *           (123 .GT. KF .OR. KF .GT. 126) .AND.
     *           (145 .GT. KF .OR. KF .GT. 158) .AND.
     *           (241 .GT. KF .OR. KF .GT. 246) .AND.
     *           (293 .GT. KF .OR. KF .GT. 307)
     *               )   GOTO 300
C                                           THIS IS A B-HAD
              ISC = 3
              GO TO 400
  300         IF( ( 20 .GT. KF .OR. KF .GT.  22) .AND.
     *            ( 30 .GT. KF .OR. KF .GT.  32) .AND.
     *            ( 51 .GT. KF .OR. KF .GT.  56) .AND.
     *            ( 58 .GT. KF .OR. KF .GT.  60) .AND.
     *            ( 71 .GT. KF .OR. KF .GT.  80)
     *              )   GO TO 400
C                                           THIS IS A C-HAD
              ISC = 2
  400         CONTINUE
  404         CONTINUE
              HP(20,NST+N) =-(10*IFC+ISC)
              IF( IFC.EQ.ISC ) HP(20,NST+N) = -ISC
              IF( HP(16,NST+N).EQ.2)HP(20,NST+N)=-HP(20,NST+N)
            ENDIF
C         ENDIF
 1001   CONTINUE
      ENDIF
      RETURN
C
C
      ENTRY QCJEPU( NSJET, NJET, IMD )
C------------------------------------------------------
C   UNPACK JETS AND STORE AS PARTICLES
C   FLAVOR CODE IS 1-5 FOR U,D,S,C, AND B
C------------------------------------------------------
C
C    VECSUB FILLING SCHEME
C   P( 1..7,*)   PX,PY,PZ,E,M,P,CHARGE
C  HP(15   ,*)   NOT USED
C  HP(16   ,*)   QUARK FLAVOUR( 0 = GLUON )
C  HP(17   ,*)   NOT USED
C  HP(18   ,*)   NOT USED
C  HP(19   ,*)   PALL NUMBER
C  HP(20   ,*)   ABS( QUARK FLAVOUR )
C-----------------------------------------------------------
C
      NJET = 0
      L = 0
      NPPALL = IW(IBLN('PALL'))
      IF( NPPALL.GT.0 .AND. IW(NPPALL+4).GT.0 ) THEN
         NLUN = IW(NPPALL+4)
         IP0 = NPPALL + IW(NPPALL+1)
         IP9 = IP0 + IW(NPPALL+2)*(NLUN-1)
         DO 2000 LP = IP0, IP9, IW(NPPALL+2)
            L = L + 1
            KF = MOD( IABS(IW(LP+7)), 1000 )
            IF( KF.GE.500 .AND.KF.LE.506 ) THEN
               IFL = KF - 500
               CHARGE = -1./3.
               IF( IFL.EQ.1 .OR.IFL.EQ.4 .OR. IFL .EQ.6 ) CHARGE = 2./3.
               IFL = IFL*ISIGN(1,IW(LP+7))
               IF( IFL.LT.0 ) CHARGE = -CHARGE
               NJET = NJET + 1
               PTOT = SQRT(RW(LP+1)**2+RW(LP+2)**2+RW(LP+3)**2)
               P(1,NSJET+NJET) = RW(LP+1)
               P(2,NSJET+NJET) = RW(LP+2)
               P(3,NSJET+NJET) = RW(LP+3)
               P(4,NSJET+NJET) = RW(LP+4)
               P(5,NSJET+NJET) = RW(LP+5)
               P(6,NSJET+NJET) = PTOT
               P(7,NSJET+NJET) = CHARGE
               HP(16,NSJET+NJET) = IFL
               HP(17,NSJET+NJET) = 0
               HP(18,NSJET+NJET) = 0
               HP(19,NSJET+NJET) = L
               HP(20,NSJET+NJET) = IP(8,NSJET+NJET)
            ENDIF
 2000    CONTINUE
      ENDIF

      RETURN
      ENTRY QCFRPU( NSFRG, NFRG, IMD )
C------------------------------------------------------
C   UNPACK PARTICLES PRODUCED BY FRAGMENTATION ( PARENTS ARE PARTONS )
C   FLAVOR CODE IS 3 FOR B-HADRONS, 2 FOR C-HADRONS AND 1 FOR OTHERS )
C------------------------------------------------------
C
C    VECSUB FILLING SCHEME
C   P( 1..7,*)   PX,PY,PZ,E,M,P,CHARGE
C  HP(15   ,*)   NOT USED
C  HP(16   ,*)   PARTICLE TYPE ( JADE MC-CONVENTION )
C  HP(17   ,*)   NOT USED
C  HP(18   ,*)   QUARK ORIGIN (PARTON PARENT )
C  HP(19   ,*)   PALL NUMBER
C  HP(20   ,*)   21,31=FRAGMENTATION 2=C 3=B LEADING HADRON
C-----------------------------------------------------------

      NFRG = 0
      L = 0
      NPPALL = IW(IBLN('PALL'))
      IF( NPPALL.GT.0 .AND. IW(NPPALL+4).GT.0 ) THEN
         NLUN = IW(NPPALL+4)
         IP0 = NPPALL + IW(NPPALL+1)
         IP9 = IP0 + IW(NPPALL+2)*(NLUN-1)
         DO 1400 LP = IP0, IP9, IW(NPPALL+2)
            L = L + 1
            IPPAR = IP0 + (IW(LP+8)-1) * IW(NPPALL+2)
            KFPAR = MOD( IABS(IW(IPPAR+7)), 1000 )
            IF( KFPAR.GE.500 .AND.KFPAR.LE.506 ) THEN
               KF = MOD( IABS( IW( LP+7 )), 1000 )
               ICODE = 1
               IF( KF.LT.100 ) GO TO 4300
               IF( (101 .GT. KF .OR. KF .GT. 104) .AND.
     *             (123 .GT. KF .OR. KF .GT. 126) .AND.
     *             (145 .GT. KF .OR. KF .GT. 158) .AND.
     *             (241 .GT. KF .OR. KF .GT. 246) .AND.
     *             (293 .GT. KF .OR. KF .GT. 307)
     *                  )   GOTO 4300
C                                           THIS IS A B-HAD
                 ICODE = 3
                 GO TO 4400
 4300          IF( ( 20 .GT. KF .OR. KF .GT.  22) .AND.
     *             ( 30 .GT. KF .OR. KF .GT.  32) .AND.
     *             ( 51 .GT. KF .OR. KF .GT.  56) .AND.
     *             ( 58 .GT. KF .OR. KF .GT.  60) .AND.
     *             ( 71 .GT. KF .OR. KF .GT.  80)
     *             )   GO TO 4400
C                                          THIS IS A C-HAD
                 ICODE = 2
 4400          CONTINUE
               NFRG = NFRG + 1
               PTOT = SQRT(RW(LP+1)**2+RW(LP+2)**2+RW(LP+3)**2)
               CHARGE = IW(LP+6)
               P(1,NSFRG+NFRG) = RW(LP+1)
               P(2,NSFRG+NFRG) = RW(LP+2)
               P(3,NSFRG+NFRG) = RW(LP+3)
               P(4,NSFRG+NFRG) = RW(LP+4)
               P(5,NSFRG+NFRG) = RW(LP+5)
               P(6,NSFRG+NFRG) = PTOT
               P(7,NSFRG+NFRG) = CHARGE
               HP(15,NSFRG+NFRG) = 0
               HP(16,NSFRG+NFRG) = KF
               HP(17,NSFRG+NFRG) = 0
               HP(18,NSFRG+NFRG) = IW(LP+9)
               HP(19,NSFRG+NFRG) = L
               HP(20,NSFRG+NFRG) = ICODE
            ENDIF
 1400    CONTINUE
      ENDIF

      RETURN
      END
