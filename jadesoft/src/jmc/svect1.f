C   02/11/81 312061539  MEMBER NAME  SVECT1   (S)           FORTRAN
      SUBROUTINE SVECT1( P, R )
C-----------------------------------------------------------
C
C  VERSION OF 02/11/81   LAST MOD 04/11/81      E.ELSEN
C  STORE PARTICLE WITH P, R IN VECT,1 .
C  THE ORIGINATING PARTICLE GETS NEGATIVE TYPE.
C  ( STEERED VIA COMMON /CJTCDC / )
C ===> CHANGE: FILL RW(IP+7) FOR ELECTRONS    2/12/83 W.BARTEL
C-----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
      COMMON / BCS / IW(1)
      DIMENSION RW(1), HW(1)
      EQUIVALENCE (HW(1),RW(1),IW(1))
C
      COMMON / CJTCDC / IBANK, IPART
      COMMON / CJTRLE / TOTLEN, STPLEN, TRCOFS
C
      DIMENSION P(10),R(3)
C
      DIMENSION NPVC(2)
      EQUIVALENCE (NPVC(1),NPVEC1), (NPVC(2),NPVEC2)
C
      DATA L0, L1 / 4, 12 /
C
      DATA ICALL, IER / 0, 0 /
      IF( ICALL .GT. 0 ) GO TO 1
      ICALL = 1
      IPVECT = IBLN('VECT')
    1 CONTINUE
C
      NPVEC1 = IW(IPVECT)
      IF( NPVEC1 .LE. 0 ) GO TO 8100
      NPVEC2 = IW(NPVEC1-1)
      IF( NPVEC2 .GT. 0 ) GO TO 100
C
      CALL CCRE( NPVEC2, 'VECT', 1, L0, IER )
      IF( IER .EQ. 2 ) GO TO 8200
      IW(NPVEC2+1) = L0
      IW(NPVEC2+2) = L1
      IW(NPVEC2+3) = IW(NPVEC1+3)
      IW(NPVEC2+4) = 0
C
  100 CALL BCHM( NPVEC2, L1, IER )
      IF( IER .NE. 0 ) GO TO 8200
      IP = NPVEC2 + L0 + IW(NPVEC2+4)*L1
      DO 200 J=1,5
  200 RW(IP+J) = P(J)
      IW(IP+6) = P(7)
      IW(IP+7) = P(8)
      RW(IP+8) = R(1)
      RW(IP+9) = R(2)
      RW(IP+10) = R(3)
      HW(IP*2+21) = IBANK
      HW(IP*2+22) = IPART
      RW(IP+12) = TRCOFS
      IF( P(7) .EQ. 0. ) RW(IP+12) = SQRT(R(1)**2+R(2)**2+R(3)**2)
C                                           ACCEPT PARTICLE
      IW(NPVEC2+4) = IW(NPVEC2+4) + 1
C
C                                           MARK DECAY FOR ORIGINATING
C                                           PARTICLE ( TYPE < 0 )
C                                           EXCEPT FOR ELECTRONS
      NPOLD = NPVC(IBANK+1)
      NPOLD = NPOLD + IW(NPOLD+1) + (IPART-1)*IW(NPOLD+2)
      IF( IW(NPOLD+7) .EQ. 2 ) GO TO 8000
      IW(NPOLD+7) = -IABS(IW(NPOLD+7))
C
      GO TO 8000
C
C
 8100 IF( IER .GT. 10 ) GO TO 8000
      IER = IER + 1
      WRITE(6,9101)
 9101 FORMAT(' +++++ SVECT1 +++++ BANK VECT 0 MISSING' )
      GO TO 8000
C
 8200 IF( IER .GT. 10 ) GO TO 8000
      IER = IER + 1
      WRITE(6,9102)
 9102 FORMAT(' +++++ SVECT1 +++++ NO SPACE FOR BANK VECT 1' )
C
 8000 RETURN
      END
