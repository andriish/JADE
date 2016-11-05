C   02/11/81 312221239  MEMBER NAME  JELOSS   (S)           FORTRAN
      SUBROUTINE JELOSS( P, STRAC, POT, ZARO, XRAD, R, CA, SA )
C-----------------------------------------------------------
C
C  VERSION OF  3/12/77     LAST MOD 02/11/81   E.ELSEN
C  COMPONENTS OF P ARE CHANGED ACCORDING TO ENERGY LOSS.
C      P(1)...P(3) MOMENTUM THREE VECTOR
C      P(4)      ENERGY
C      P(5)      REST MASS
C      P(6)      TOTAL MOMENTUM
C
C      R(1..3)   COORDINATES OF POINT IN SYSTEM WHICH STILL HAS TO BE
C                ROTATED BY CA, SA!
C
C  FIND IONISATION ENERGY LOSS.
C      ZARO = Z / A * RO, WHERE RO IS DENSITY OF MATERIAL IN G/CM**3
C                    WITH CHARGE NUMBER Z AND MASS NUMBER A.
C      POT       IONISATION POTENTIAL IN EV ( ROUGHLY 10.*Z )
C   BETHE BLOCH FORMULA USED:
C   DE            Z          1.  (     (1.022E6 * BETA2   )         )
C   -- = 2. * D * - * RO * ----- ( ALOG(------------------) - BETA2 )
C   DX            A        BETA2 (     (POT * ( 1.- BETA2))         )
C
C   FIND BREMSSTRAHLUNG ENERGY LOSS
C   BETHE HEITLER XSECTION WITH COMPLETE SCREENING AND MULTIPLE
C   PHOTON EMISSION A LA EYGES
C   XI =(E0 - EPRIM)/E0,  B=4/3 , T=THICKNESS IN RADL
C   FI(XI) = XI**BT/BT -XI**(BT+1.)/(BT+1.) + 0.75/(BT+2.)*XI**(BT+2.)
C   LAST CHANGE                 20./11./83    W.BARTEL
C-----------------------------------------------------------
C
      DIMENSION P(6),R(3), RR(3)
      DIMENSION AK(10), IK(10)
      EQUIVALENCE (AK(1), IK(1))
      DIMENSION A2(3),A3(3)
C                                           MINIMUM OF RADIATED ENERGY
      DATA ARMIN / 0.001 /
C                                           MINIMUM PHOTON ENERGY
      DATA AKMIN / 0.01 /
      DATA D / .1535E-4 /
C                                           BREMSSTRAHLUNG LOSS
      COMMON /CFUNK/ BT,CONS,RHS
      DATA B /1.3333/
      EXTERNAL RADIAT
      FI(XI) = XI**BT/BT -XI**(BT+1.)/(BT+1.) + 0.75/(BT+2.)*XI**(BT+2.)
C
C  SCIP ELOSS IF STRAC < 1.E-06
C
      IF(STRAC.LT.1.E-06) RETURN
C
      BETA = P(6) / P(4)
      BETA2 = AMIN1( BETA * BETA, .9409 )
      DE = 2.*D*ZARO/BETA2* ( ALOG( 1.022E6/POT * BETA2/ (1.- BETA2 ) )
     +                                  - BETA2 ) * STRAC
C
C
C                                           DETERMINE TYPE OF LOSS
      IF( P(5) .GT. 1.E-3 ) GO TO 2000
C
C
C                                           BREMSSTRAHLUNG
C                                           TREATED STATISTICALLY
      T  = STRAC/XRAD
      BT = B*T
      CONS = BT*(1. + 0.577*BT)
      XLOW = 1.E-04
      FLOW = FI(XLOW)
      XNORM = (FI(1.) - FLOW) * CONS
      IF(RN(DUM).LT.FLOW*CONS) GO TO 2000
C
C      ELOSS ACCORDING TO EYGES FORMULA
C
      XIMIN = 1.E-04
      XIMAX = 1.
   11 RHS = XNORM*RN(DUM) + FLOW*CONS
      CALL RTMI(XIMSC,FF,RADIAT,XIMIN,XIMAX,1.E-3,20,IER)
      IF(XIMSC.GT.1.) GO TO 11
      XMSC = 1. - XIMSC
C                                           RADIATED PHOTON
      AK(4) = XIMSC*P(4)
      IF( AK(4) .LT. ARMIN ) GO TO 2000
      DE = DE + AK(4)
C
      IF( AK(4) .LT. AKMIN ) GO TO 2000
C                                           ASSUME ZERO
C                                           SCATTERING ANGLE
            FAC = AK(4) / P(6)
            AK(1) = P(1)*FAC
            AK(2) = P(2)*FAC
            AK(3) = P(3)*FAC
            AK(5) = 0.
            IK(6) = 0
            AK(7) = 0.
            AK(8) = 1.
            AK(9) = 0.
            AK(10) = 0.
            RR(1) = R(1)
            RR(2) = R(2)
            RR(3) = R(3)
C
      CALL JROTAT( RR, CA, -SA )
      CALL JROTAT( AK, CA, -SA )
      CALL SVECT1( AK, RR )
C
C
 2000 P(4) = AMAX1( P(5), P(4) - DE )
      PNEW = SQRT( P(4)*P(4) - P(5)*P(5) )
      P(1) = P(1) / P(6) * PNEW
      P(2) = P(2) / P(6) * PNEW
      P(3) = P(3) / P(6) * PNEW
      P(6) = PNEW
      RETURN
      END
