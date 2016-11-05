C   07/12/86 708311919  MEMBER NAME  QCLPCK   (S)           FORTRAN77
      SUBROUTINE QCLPCK( NST, N, IMODE )
C-----------------------------------------------------------
C  VERSION OF 26/01/85       LAST MOD 31/08/87    M ZIMMER
C  ORIGIN OF ALL FINAL STATE PARTICLES IT CATEGORIZED
C  LOAD LUND DATA INTO VECSUB COMMON
C  IMODE=0    CHARGED AND GAMMA
C       =1    CHARGED ONLY
C       =2    CHARGED ONLY, ALL PI
C       =5    CHARGED AND NEUTRAL ALL MASSES PI AND 0
C
C    VECSUB FILLING SCHEME
C   P( 1..7,*)   PX,PY,PZ,E,M,P,CHARGE
C  HP(15   ,*)   NOT USED
C  HP(16   ,*)   PARTICLE TYPE ( JADE MC-CONVENTION )
C  HP(17   ,*)   0 OR PALL NUMBER OF CORRESPONDING LEADING HADRON
C  HP(18   ,*)   QUARK ORIGIN (PARTON PARENT (QUARK OR ANTIQUARK)
C  HP(19   ,*)   PALL NUMBER
C  HP(20   ,*)   PARTICLE ORIGIN (> 0 MEANS ELECTRON )
C-----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
      COMMON / LUJETS / NLUN, KLUN(250,2),PLUN(250,5)
      COMMON  P(10,400)
      DIMENSION IP(10,1),HP(20,1)
      EQUIVALENCE (P(1,1),IP(1,1),HP(1,1))

      LOGICAL FIRST / .TRUE. /
      LOGICAL FIRSTP / .TRUE. /

      IF( FIRST ) THEN
         FIRST = .FALSE.
         WRITE(6,9201) NLUN, (KLUN(I,1),KLUN(I,2),(PLUN(I,J),J=1,5),
     +                 I=1,NLUN)
 9201    FORMAT(2X,' NLUN ',I5/(1X,2I8,5F12.3))
      ENDIF

      N = 0

      IF( NLUN.GT.0 ) THEN
         L=0
         IFL=0
C                                      GET FLAV. OF EVT
         DO 1000 UNTIL( (IFL .GT. 0 .AND. IFL .LT. 6) .OR. L .GT. NLUN)
            L = L+1
            IFL = IABS(KLUN(L,2))-500
 1000    CONTINUE
         IFC = MAX0( IFL-2,1 )
         DO 1001 L = 1, NLUN
            IF( KLUN(L,1).LT.2000 ) THEN
               CHARGE = FLOAT(LUCHGE(KLUN(L,2)))/3.
                IF( CHARGE.NE.0. .OR.
     +              ((IMODE.EQ.0 .OR. IMODE .EQ. 5)
     +                         .AND.IABS(KLUN(L,2)).EQ.1) ) THEN
                  N = N + 1
                  P(1,NST+N) = PLUN(L,1)
                  P(2,NST+N) = PLUN(L,2)
                  P(3,NST+N) = PLUN(L,3)
                  PTOT=SQRT(P(1,NST+N)**2+P(2,NST+N)**2+P(3,NST+N)**2)
                  P(6,NST+N) = PTOT
                  P(7,NST+N) = CHARGE
                  XM = PLUN(L,5)
                  HP(15,NST+N) = 0
                  HP(16,NST+N) = IABS(KLUN(L,2))
                  IF((IMODE.EQ.2 .OR. IMODE .EQ. 5)
     +                     .AND.CHARGE.NE.0.
     +                     .AND. IABS(KLUN(L,2)).NE.7 ) THEN
                     XM = .1396
                     HP(16,NST+N) = 0
                  ENDIF
                  IF(IMODE.EQ.5.AND.CHARGE.EQ.0. ) THEN
                     XM = 0.
                     HP(16,NST+N) = 0
                  ENDIF
                  P(5,NST+N) = XM
                  P(4,NST+N) = SQRT( PTOT**2 + XM**2 )
                  HP(17,NST+N) = 0
                  HP(19,NST+N) = L
C                                           CATEGORIZE
                  ISC = 1
                  J = L
                  DO 1002 WHILE ( MOD( KLUN(J,1), 1000 ).NE. 0)
                    JP = MOD( KLUN(J,1),1000)
                    IF(  ISC .LT. 2 ) THEN
                      KFP = IABS( KLUN(JP,2) )
                      IF( KFP .LT. 500 ) THEN
                        CALL LUIFLV( KFP, IFLA, IFLB, IFLC, KSP )
                        ISC = MAX0(IABS(IFLA)-2,1)
                      ENDIF
                    ENDIF
                    J = JP
 1002             CONTINUE
                  HP(18,NST+N) = ISIGN(1, KLUN(JP,2))
                  HP(20,NST+N) = -( 10*IFC+ISC )
                  IF( IFC.EQ.ISC ) HP(20,NST+N) = - ISC
                  IF( HP(16,NST+N).EQ.7 ) HP(20, NST+N) = - HP(20,NST+N)
               ENDIF
            ENDIF
 1001    CONTINUE
      ENDIF
      RETURN
C
C
      ENTRY QCJETU( NSJET, NJET, IMD )
C------------------------------------------------------
C   UNPACK JETS AND STORE AS PARTICLES
C   FLAVOR CODE IS 1-5 FOR U,D,S,C, AND B
C------------------------------------------------------
C
      IF( FIRSTP ) THEN
         FIRSTP = .FALSE.
C        WRITE(6,9202) NLUN, (KLUN(I,1),KLUN(I,2),(PLUN(I,J),J=1,5),
C    *                 I=1,NLUN)
C9202    FORMAT(2X,' NLUN ',I5/(1X,2I8,5F12.3))
      ENDIF


      NJET = 0

      IF( NLUN.GT.0 ) THEN
         DO 2000 L = 1, NLUN
            IFL = IABS(KLUN(L,2))-500
            IF( IFL.GE.0 .AND. IFL.LT.6 ) THEN
               ICODE = MAX0(1,IABS( IFL) - 2)
C                                      FOR GLUONS
               IF ( IFL .EQ. 0 ) ICODE = 0
               CHARGE = FLOAT(LUCHGE(KLUN(L,2)))/3.
               IF( CHARGE.EQ.0. ) GO TO 2000
                  NJET = NJET + 1
                  PTOT = SQRT(PLUN(L,1)**2+PLUN(L,2)**2+PLUN(L,3)**2)
                  P(1,NSJET+NJET) = PLUN(L,1)
                  P(2,NSJET+NJET) = PLUN(L,2)
                  P(3,NSJET+NJET) = PLUN(L,3)
                  P(4,NSJET+NJET) = PLUN(L,4)
                  P(5,NSJET+NJET) = PLUN(L,5)
                  P(6,NSJET+NJET) = PTOT
                  P(7,NSJET+NJET) = CHARGE
                  HP(15,NSJET+NJET) = 0
                  HP(16,NSJET+NJET) = KLUN(L,2)
                  HP(17,NSJET+NJET) = 0
                  HP(18,NSJET+NJET) = ISIGN(1,KLUN(L,2))
                  HP(19,NSJET+NJET) = L
                  HP(20,NSJET+NJET) = ICODE
               ENDIF
 2000    CONTINUE
      ENDIF

      RETURN
C
      ENTRY QCFRGU( NSFRG, NFRG, IMD )
C------------------------------------------------------
C   UNPACK PARTICLES PRODUCED BY FRAGMENTATION ( PARENTS ARE PARTONS )
C   FLAVOR CODE IS 3 FOR B-HADRONS, 2 FOR C-HADRONS AND 1 FOR OTHERS )
C------------------------------------------------------
C

      NFRG = 0

      IF( NLUN.GT.0 ) THEN
         DO 1400 L = 1, NLUN
            IPPAR =  MOD( KLUN( L,1) , 1000 )
            IFLPAR =  IABS(KLUN(IPPAR,2))-500
            IF( IFLPAR.GE.0 .AND. IFLPAR.LT.6 ) THEN
               IFL = KLUN(L,2)
               CALL LUIFLV( IFL, KFLVA, KFLVB, KFLVC, KSP )
               ICODE = MAX0( IABS(KFLVA) - 2, 1 )
               NFRG = NFRG + 1
               PTOT = SQRT(PLUN(L,1)**2+PLUN(L,2)**2+PLUN(L,3)**2)
               CHARGE = FLOAT(LUCHGE(KLUN(L,2)))/3.
               P(1,NSFRG+NFRG) = PLUN(L,1)
               P(2,NSFRG+NFRG) = PLUN(L,2)
               P(3,NSFRG+NFRG) = PLUN(L,3)
               P(4,NSFRG+NFRG) = PLUN(L,4)
               P(5,NSFRG+NFRG) = PLUN(L,5)
               P(6,NSFRG+NFRG) = PTOT
               P(7,NSFRG+NFRG) = CHARGE
               HP(15,NSFRG+NFRG) = 0
               HP(16,NSFRG+NFRG) = IFL
               HP(17,NSFRG+NFRG) = 0
               HP(18,NSFRG+NFRG) = ISIGN( 1, KLUN(IPPAR,2))
               HP(19,NSFRG+NFRG) = L
               HP(20,NSFRG+NFRG) = ICODE
            ENDIF
 1400    CONTINUE
      ENDIF

      RETURN
      END
