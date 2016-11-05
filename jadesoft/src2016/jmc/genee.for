C   29/05/80 312221222  MEMBER NAME  GENEE    (S)           FORTRAN
      SUBROUTINE GENMU( * )
C-----------------------------------------------------------
C
C  VERSION OF 03/03/80  LAST MOD 03/03/80    E.ELSEN
C  GENERATE MU-MU EVENTS AND FILL COMMON CPROD
C  RETURN 1 IF MORE THAN 10000 EVENTS
C----------------------------------------------------------
C
      COMMON/CPROD/ NEV,BEAM,PT,APHI,ATHETA,IFLAVR,
     +       NP,NC,NN,PP(4,500),XM(500),JCH(500),JTP(500),JP(500,2),
     +       NF,NCF,NNF,PF(4,300),XMF(300),ICF(300),ITF(300),
     +       PSTRT(3,300)
C
      DATA ICALL / 0 /, ICALIM / 10000 /
      DATA EBEAM /12. /
C
      ICALL = ICALL + 1
      IF( ICALL .GT. ICALIM ) RETURN 1
C                                           HEADER FOR CPROD
      NEV = ICALL
      IFLAVR = 0
      NP = 2
      NC = 2
      NN = 0
      NF = 2
      NCF = 2
      NNF = 0
C
      APHI = 6.28319 * RN(DUMMY)
      X = (1.-2*RN(DUMMY))*0.85
C
      ATHETA = ARCOS( X )
C     CALL UHIST( 1001, X )
      SINTH = SQRT( 1. - X**2 )
      PT = EBEAM * SINTH
C
      PP(1,1) = EBEAM * COS(APHI)*SINTH
      PP(2,1) = EBEAM * SIN(APHI)*SINTH
      PP(3,1) = EBEAM * X
      XM(1) = .000511
      PP(4,1) = SQRT( PP(1,1)**2+PP(2,1)**2+PP(3,1)**2+XM(1)**2)
      JCH(1) = 1
      JTP(1) = 2
      EBEAM = - EBEAM
      PP(1,2) = EBEAM * COS(APHI)*SINTH
      PP(2,2) = EBEAM * SIN(APHI)*SINTH
      PP(3,2) = EBEAM * X
      XM(2) = XM(1)
      PP(4,2) = SQRT( PP(1,2)**2+PP(2,2)**2+PP(3,2)**2+XM(2)**2)
      BEAM = SQRT(EBEAM**2 + XM(1)**2)
      JCH(2) = -1
      JTP(2) = JTP(1)
C
      DO 1000 J=1,2
      PF(1,J) = PP(1,J)
      PF(2,J) = PP(2,J)
      PF(3,J) = PP(3,J)
      PF(4,J) = PP(4,J)
      XMF(J) = XM(J)
      ICF(J) = JCH(J)
      ITF(J) = JTP(J)
      PSTRT(1,J) = 0.
      PSTRT(2,J) = 0.
      PSTRT(3,J) = 0.
 1000 CONTINUE
C
      RETURN
      END
