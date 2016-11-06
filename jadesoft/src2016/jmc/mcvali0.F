C   16/02/84 402271938  MEMBER NAME  MCVALI0  (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE MCVALI( VALID )
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY   15/11/83 :  CHECKS VALIDITY OF INPUT VECTORS
C
C        MOD: C. BOWDERY   22/11/83 :  FOR TEST DISABLING
C        MOD: C. BOWDERY    2/12/83 :  TEST EVENT NO. & MASS
C        MOD: C. BOWDERY    1/02/84 :  E/P/M IMBALANCE: EVENT KEPT
C                                   :  IF LOW E PHOTON
C        MOD: C. BOWDERY   16/02/84 :  PTOT = EBEAM NOW LEGAL
C   LAST MOD: C. BOWDERY   27/02/84 :  NO TESTS FOR PARTONS AT ALL NOW
C
C   THIS ROUTINE VALIDATES THE 4-VECTOR INFORMATION IN /CPROD/ BEFORE
C   TRACKING COMMENCES.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL VALID , FIRST , VALSP
C
      DOUBLE PRECISION DPTSQ , DMSQ , DESQ , DEPM , DPX , DPY, DPZ
      DOUBLE PRECISION DE , DM
C
#include "cprod.for"
C
      COMMON / CVERR / MESSAG(20)
C
      DIMENSION STMASS(6)
C
C                       PARTICLE MASSES (GAMMA,E,MU,PI,K,N)
C
      DATA STMASS / 0.0 , 0.551E-3 , 0.1057 , 0.1396 , 0.4937 , 0.938 /
C
C-------------------  C O D E  -----------------------------------------
C
C                  EVEN IF AN ERROR IS FOUND HERE, THE TEST MAY BE
C                  DISABLED SO THE RETURN FLAG 'VALID' MUST BE CHECKED
C                  AFTER CALLS TO MCVERR.
C
C
C                            FIRST CHECK EVENT NUMBER
C
C
      VALID = .TRUE.
      IF( NEV .GE. 0  .AND.  NEV .LE. 9999999 ) GO TO 111
      CALL MCVERR(13 ,'INVALID EVENT NUMBER^',0.,NEV,'    ',NEV,1,VALID)
C
C
C                            CHECK NF , NCF AND NNF
C
C
 111  IF( NF  .GT. 0   .AND.  NF  .LE. 300 ) GO TO 112
      CALL MCVERR(14 ,'INVALID NO. OF PARTICLES (NF)^',0.,NF,
     +                                               '    ',NEV,1,VALSP)
      IF( NF .LE. 0 ) VALSP = .FALSE.
      VALID = VALID .AND. VALSP
      IF( .NOT. VALSP ) RETURN
C
 112  IF( NCF .GE. 0   .AND.  NCF .LE. 300 ) GO TO 113
      CALL MCVERR(15 ,'INVALID NO. OF CHARGED PARTICLES^',0.,NCF,
     +                                               '    ',NEV,1,VALSP)
      VALID = VALID .AND. VALSP
C
 113  IF( NNF .GE. 0   .AND.  NNF .LE. 300 ) GO TO 114
      CALL MCVERR(16 ,'INVALID NO. OF NEUTRAL PARTICLES^',0.,NNF,
     +                                               '    ',NEV,1,VALSP)
      VALID = VALID .AND. VALSP
C
 114  IF( NF .EQ. NCF + NNF ) GO TO 115
      CALL MCVERR(17 ,'NF .NE. NCF + NNF . DIFFERENCE =^',0.,NF-NCF-NNF,
     +                                               '    ',NEV,1,VALSP)
      VALID = VALID .AND. VALSP
C
C
C                            CHECK VALIDITY OF THE BEAM ENERGY 'BEAM'
C
C
 115  IF( BEAM .GT. 0.0  .AND. BEAM .LT. 1000.0 ) GO TO 1
      CALL MCVERR( 1 ,'INVALID BEAM ENERGY^',BEAM,0,' GEV',NEV,1,VALSP )
      VALID = VALID .AND. VALSP
      IF( .NOT. VALID ) RETURN
C
C
C                            LOOP OVER THE FINAL LIST
C
C
   1  DO  2  I = 1,NF
        FIRST = .TRUE.
C
C                            IGNORE ANY PARTONS IN THE FINAL LIST.
C
        IF( ITF(I) .LE. -100 .AND. ITF(I) .GT. -120 ) GO TO 2
C
C
C                            CHECK EACH 3-MOMENTUM COMPONENT
C
        DO  3  J = 1,3
C
          IF( ABS( PF(J,I) ) .GT. 1.0E-20          .AND.
     +        ABS( PF(J,I) ) .LE. BEAM  .OR. PF(J,I) .EQ. 0.0  ) GO TO 300001030
C
          CALL MCVERR( 2,'INVALID MOMENTUM COMPONENT^',PF(J,I),0,' GEV',
     +                                                NEV,I,VALSP)
          VALID = VALID .AND. VALSP
C
C
C                            PRINT ADDITIONAL INFORMATION
C
C
          IF( FIRST .AND. MESSAG(2).LE.20 .AND. .NOT. VALSP )
     +      WRITE(6,101) (PF(K,I),K=1,4)
 101      FORMAT(34X,'PX = ',G12.6,' GEV    PY = ',G12.6,' GEV    PZ = '
     +          ,G12.6,' GEV    E = ',G12.6,' GEV')
          FIRST = .FALSE.
          IF( ABS( PF(J,I) ) .LE. 1.0E-20 )RETURN
          IF( ABS( PF(J,I) ) .GE. 1.0E+20 )RETURN
   3    CONTINUE
C
C
C                            CHECK THE PARTICLE ENERGY
C
C
   4    IF( PF(4,I) .GT. 1.0E-20 .AND. PF(4,I) .LE. 2 * BEAM ) GO TO 5
        CALL MCVERR( 3,'INVALID ENERGY COMPONENT^',PF(4,I),0,' GEV',
     +                                             NEV,I,VALSP )
        VALID = VALID .AND. VALSP
        IF( ABS( PF(4,I) ) .LE. 1.0E-20 ) RETURN
        IF( ABS( PF(4,I) ) .GE. 1.0E+20 ) RETURN
C
C
C                            CHECK THE TYPE. ONLY 1..6 IS LEGAL
C
C
   5    IF( ITF(I) .GT. 0  .AND. ITF(I) .LE. 6 ) GO TO 6
C
        CALL MCVERR( 11,'INVALID PARTICLE TYPE^',0.0,ITF(I),'    ',
     +                                             NEV,I,VALSP )
        VALID = VALID .AND. VALSP
        GO TO 7
C
C
C                            CHECK THAT THE MASS AND TYPE AGREE ROUGHLY
C
C
   6    PMASS = STMASS( ITF(I) )
        IF( ABS( PMASS - XMF(I) ) .LT. 0.01 ) GO TO 7
        CALL MCVERR( 4,'MASS DISAGREES WITH TYPE^',XMF(I),0,' GEV',
     +                                              NEV,I,VALSP )
        VALID = VALID .AND. VALSP
C
C
C                            PRINT ADDITIONAL INFORMATION
C
C
        IF( MESSAG(4) .LE. 20 .AND. .NOT. VALSP )
     +                        WRITE(6,102) ITF(I),PMASS
 102    FORMAT(34X,'PARTICLE TYPE ',I3,' SHOULD HAVE MASS = ',G10.4,
     +                                                       ' GEV')
C
C
C                            CHECK THE ELECTRIC CHARGE. ONLY -1,0,1  OK
C
C
   7    IF( IABS( ICF(I) ) .LE. 1 ) GO TO 8
        CALL MCVERR( 12,'INVALID ELECTRIC CHARGE^',0.0,ICF(I),'    ',
     +                                               NEV,I,VALSP )
        VALID = VALID .AND. VALSP
C
C
C                            CHECK THE TOTAL MOMENTUM
C
C
   8    DPX   = PF(1,I)
        DPY   = PF(2,I)
        DPZ   = PF(3,I)
        DE    = PF(4,I)
        DM    = XMF(I)
        DPTSQ = DPX*DPX + DPY*DPY + DPZ*DPZ
        PTOT  = DSQRT( DPTSQ )
        IF( PTOT .LE. BEAM * 1.001 ) GO TO 81
        CALL MCVERR( 8,'INVALID TOTAL MOMENTUM - PTOT^',PTOT,0,
     +                                      ' GEV',NEV,I,VALSP)
        VALID = VALID .AND. VALSP
        IF( FIRST .AND. MESSAG(8).LE.20 .AND. .NOT. VALSP )
     +                                      WRITE(6,101) (PF(K,I),K=1,4)
C
C
C                            CHECK THAT THE MASS IS WITHIN BOUNDS
C
C
  81    IF( XMF(I) .GT. 1.0E-20 .AND. XMF(I) .LT. 2 * BEAM
     +          .OR.  XMF(I) .EQ. 0.0 ) GO TO 85
C
        CALL MCVERR( 9,'INVALID PARTICLE MASS^',XMF(I),0,
     +                                      ' GEV',NEV,I,VALSP)
        VALID = VALID .AND. VALSP
        IF( ABS( XMF(I) ) .LE. 1.0E-20 ) RETURN
        IF( ABS( XMF(I) ) .GE. 1.0E+20 ) RETURN
C
  85    DMSQ = DM * DM
C
C
C                            DETERMINE THE ENERGY/MOMENTUM MISMATCH
C
C                               | ( P**2 + M**2 - E**2 ) |
C                               --------------------------
C                                          E**2
C
C
C                            THIS ERROR IS NOT SO SERIOUS FOR LOW E
C                            PHOTONS. - DON'T ALWAYS SET VALID=.FALSE.
C
        DESQ = DE * DE
        DEPM = DABS( DPTSQ + DMSQ - DESQ ) / DESQ
        IF( DEPM .LT. 2.5D-3 ) GO TO 9
        CALL MCVERR( 5,'E*E  .NE. M*M  + PTOT*PTOT      M = ^',
     +                            XMF(I),0,' GEV', NEV,I,VALSP )
        IF( FIRST .AND. MESSAG(5).LE.20 .AND. .NOT. VALSP )
     +                                      WRITE(6,101) (PF(K,I),K=1,4)
        XMISM = DSQRT( DEPM ) * 1.0D2
        IF( FIRST .AND. MESSAG(5).LE.20 .AND. .NOT. VALSP )
     +                                      WRITE(6,110) XMISM
 110    FORMAT(34X,'INCONSISTENCY IS ',F7.2,'%')
C
C                            DONT REJECT EVENT FOR BAD LOW ENERGY PHOTON
C
        IF( PF(4,I) .GT. 0.2  .OR. ITF(I) .NE. 1 )
     +                                         VALID = VALID .AND. VALSP
C
C
C                            CHECK THE VERTEX COMPONENTS
C
C
   9    FIRST = .TRUE.
        DO  10  K = 1,3
          APSTRT = ABS( PSTRT(K,I) )
          IF( APSTRT .GT. 1.0E-20 .AND. APSTRT .LT. 5000.0
     +        .OR. APSTRT .EQ. 0.0 )       GO TO 10
          CALL MCVERR( 6,'INVALID VERTEX COMPONENT^',PSTRT(K,I),0,' MM '
     +                                            ,NEV,I,VALSP )
          VALID = VALID .AND. VALSP
C
C
C                            PRINT ADDITIONAL INFORMATION
C
C
          IF( FIRST .AND. MESSAG(6) .LE. 20 .AND. .NOT. VALSP )
     +                                   WRITE(6,103) (PSTRT(L,I),L=1,3)
 103      FORMAT(34X,'X = ',G10.4,' MM    Y = ',G10.4,' MM    Z = '
     +          ,G10.4,' MM')
          FIRST = .FALSE.
          IF( ABS( PSTRT(K,I) ) .LT. 1.0E-20 ) RETURN
  10    CONTINUE
C
C
C                            CHECK THE VERTEX SCALAR VALUE
C
C
        VERTEX = SQRT( PSTRT(1,I)**2 + PSTRT(2,I)**2 + PSTRT(3,I)**2 )
        IF( VERTEX .LT. 8000.0 ) GO TO 2
        CALL MCVERR( 7,'INVALID VERTEX POINT^',VERTEX,0,
     +                                  ' MM ',NEV,I,VALSP )
        VALID = VALID .AND. VALSP
        IF( FIRST .AND. MESSAG(7) .LE. 20 .AND. .NOT. VALSP )
     +                                   WRITE(6,103) (PSTRT(L,I),L=1,3)
        FIRST = .FALSE.
   2  CONTINUE
C
C
C                            AT THE END, IF VALID = .TRUE. THEN ALL OK
C
C
      RETURN
      END
