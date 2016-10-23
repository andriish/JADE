C   19/06/86 807251811  MEMBER NAME  EVTAX    (S)           FORTRAN
      SUBROUTINE EVTAX(EAXIS,EOUT,SPHITY,XMASS,IERR)
C-----------------------------------------------------------------
C     VERSION OF 19.06.86                   R.RAMCKE
C     FINDS THE EVT-AXIS IN MC-EVENTS
C     MOD 02.02.87                          R.RAMCKE
C     CALCULATES TRANSVERS MASS OF INITAL PARTONS
C
C-----------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C----------------------------------------------------------------------
C             MACRO CDATA .... BOS COMMON.
C
C             THIS MACRO ONLY DEFINES THE IDATA/HDATA/ADATA NAMES.
C             THE ACTUAL SIZE OF /BCS/ IS FIXED ON MACRO CBCSMX
C             OR BY OTHER MEANS. A DEFAULT SIZE OF 40000 IS GIVEN HERE.
C
C----------------------------------------------------------------------
C
      COMMON /BCS/ IDATA(40000)
      DIMENSION HDATA(80000),ADATA(40000),IPNT(50)
      EQUIVALENCE (HDATA(1),IDATA(1),ADATA(1)),(IPNT(1),IDATA(55))
      EQUIVALENCE (NWORD,IPNT(50))
C
C------------------------ END OF MACRO CDATA --------------------------
      COMMON / CKOPP / HKOPP(100)
C
      DIMENSION EAXIS(3),EOUT(3),P(4,10),T(6),R(9)
      DIMENSION SPHITY(3)
      DATA IPR / -1 /
C
      IF(HKOPP(10) .LT. 100) GOTO 1
         IERR = -1
         RETURN
 1    CONTINUE
C
      IPALL = IDATA(IBLN('PALL'))
      IF(IPALL .GT. 0) GOTO 2
         IERR = -2
         RETURN
 2    CONTINUE
C
      NALL = IDATA(IPALL+4)
      IERR = -3
      IF(NALL .LE. 0) GOTO 100
      L1 = IDATA(IPALL+1)
      L2 = IDATA(IPALL+2)
      IPALL = IPALL + L1 + NALL*L2
      NINAT = 0
C
      IPR = IPR - 1
      IF(IPR .GT. 0) WRITE(6,744)
 744  FORMAT(/'  ---- LAST PARTICLES OF PALL -------')
C
      DO 30 I = 1,NALL
         IPALL = IPALL - L2
         IF(IDATA(IPALL+9) .NE. -2 .AND.
     *      IDATA(IPALL+9) .NE. -100) GOTO 35
C
            IF(IPR .GT. 0) WRITE(6,745) IDATA(IPALL+9),
     *                                  (ADATA(IPALL+II),II=1,5)
 745        FORMAT('  TYP: ',I4,'  PX,PY,PY,E,M: ',5F9.3)
C
         NINAT = NINAT + 1
         P(1,NINAT) = ADATA(IPALL+1)
         P(2,NINAT) = ADATA(IPALL+2)
         P(3,NINAT) = ADATA(IPALL+3)
         P(4,NINAT) = ADATA(IPALL+4)
C
 30   CONTINUE
 35   CONTINUE
      IERR = -4
      IF(NINAT .LE. 0) GOTO 100
C                                      CALC SPHERICITY
      DO 50 I = 1,6
 50      T(I) = 0.0
C
      DO 70 J=1,NINAT
         T(1) = T(1) + P(2,J)*P(2,J) + P(3,J)*P(3,J)
         T(2) = T(2) - P(1,J)*P(2,J)
         T(3) = T(3) + P(1,J)*P(1,J) + P(3,J)*P(3,J)
         T(4) = T(4) - P(1,J)*P(3,J)
         T(5) = T(5) - P(2,J)*P(3,J)
         T(6) = T(6) + P(1,J)*P(1,J) + P(2,J)*P(2,J)
   70 CONTINUE
C
      CALL EIGEN( T, R, 3, 0 )
C
      SUM = T(1) + T(3) + T(6)
      SPHITY(1) = 3.* T(6) / SUM
      SPHITY(2) = 3.* T(3) / SUM
      SPHITY(3) = 3.* T(1) / SUM
C
      EAXIS(1) = R(7)
      EAXIS(2) = R(8)
      EAXIS(3) = R(9)
      EOUT(1) = R(1)
      EOUT(2) = R(2)
      EOUT(3) = R(3)
C
      IECM = 2*HKOPP(29)
      ECM = FLOAT(IECM)/1000.
      IF(ECM .LE. 0) ECM = 44.4
      EVIS = 0.
C
      SUM = 0.0
      DO 200 I = 1,NINAT
         PABS = SQRT(P(1,I)**2+P(2,I)**2+P(3,I)**2)
         EX = P(1,I)/PABS
         EY = P(2,I)/PABS
         EZ = P(3,I)/PABS
         SUM  = SUM+P(4,I)*ABS(EX*R(1)+EY*R(2)+EZ*R(3))
         EVIS = EVIS + P(4,I)
         IF(IPR .GT. 0)WRITE(6,779) SUM,EVIS,EX,EY,EZ,(P(JJ,I),JJ=1,3)
 779     FORMAT(' SUM,EVIS,EX,Y,X,PX,Y,Z: ',8F9.2)
 200  CONTINUE
C                                   JETMASSE
      XMASS = SUM * ECM / EVIS
C
      IERR = 0
      IF(IPR .LE. 0) RETURN
         WRITE(6,788) HKOPP(11),NINAT,(EAXIS(II),II=1,3),XMASS,SPHITY(1)
 788     FORMAT('  EVT: ',I5,' #INPA: ',I3,'  ',
     *             3F9.3,' MT,SPH: ',2F9.3)
         WRITE(6,789) (EOUT(II),II=1,3),SPHITY(3)
 789     FORMAT('  EOUT: ',3F9.3,'  SPH: ',F9.3)
         RETURN
C
 100  CONTINUE
      RETURN
      END
