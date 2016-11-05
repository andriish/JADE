C   01/11/84 411022059  MEMBER NAME  RDIEFV0  (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE RDIEFV( EPV, NHITV, HITV, IERASV )
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. HAGEMANN 07/12/82 :  SIMULATION OF INEFFICENCY
C             R. RAMCKE               IN VERTEX CHAMBER
C
C        MOD  J. HAGEMANN 12/01/84 :  INEFFICENCY NOW Z-DEPENDENT
C   LAST MOD  J. HAGEMANN 02/11/84 :  IMPROVED CODE
C
C      SET SOME HITS IN ARRAY HITV WITH NHITV HITS TO
C      ZERO TO ACOUNT FOR EFFICIENCY.
C      NUMBER OF EREASED HITS IS IERASV.
C      INEFFICENCY IS DESCRIBED BY TWO PARAMETERS:
C
C         EFF(Z) = EPV(1) - EPV(2)*(Z/Z05MX)**2
C                  ======   ======
C                                     (Z05MX = MAXIMUM HALF WIRE LENGTH)
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cjvtxc.for"
#include "cgeov.for"
C
      DIMENSION HITV(2), EPV(2)
C
C------------------  C O D E  ------------------------------------------
C
C                            INEFFIENCY-FUNCTION
C
      EFF(Z) = EPV(1) - EPV(2)*(Z/Z05MX)**2
C
      Z05MX  = ZVXCP
C
      IERASV = 0
C
      IF( EPV(1) .GT. 0.995 ) RETURN
         IF( NHITV .LE. 0 )   RETURN
C
      N4 = NHITV*4 - 3
C
      DO 1000 I = 1, N4, 4
          IF( HITV(I) .EQ. 0 ) GO TO 1000
C                            ERASE HITS ON NOT EXISTING WIRES
             IF( HITV(I) .EQ. 1 .OR. HITV(I) .EQ. 85 ) GO TO 500
C
                IAL = HITV(I+1)
                IAR = HITV(I+2)
                IF( (IAL + IAR) .GT. 0 ) GO TO 300
                   Z = 0.
                   GO TO 400
  300           Z = 0.5*ZALV*FLOAT(IAR - IAL)/FLOAT(IAR + IAL)
  400           IF( RN(DUM) .LE. EFF(Z) ) GO TO 1000
  500              HITV(I) = 0
                   IERASV = IERASV + 1
 1000 CONTINUE
      RETURN
      END
