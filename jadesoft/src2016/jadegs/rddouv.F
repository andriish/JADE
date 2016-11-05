C   01/11/84 411011456  MEMBER NAME  RDDOUV   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE RDDOUV( DIFV, NHITV, HITV, IERASE )
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. HAGEMANN 27/06/83 :  SIMULATION OF DOUBLE PULSE
C             R. RAMCKE               RESOLUTION IN VERTEX CHAMBER
C
C   LAST MOD  J. HAGEMANN 01/10/84 :  CHANGED SCALE (DIFV (MM))
C
C      USED FOR DOUBLE PULS RESOLUTION OF THE VERTEX CHAMBER
C      RESOLUTION IS DESCRIBED BY THREE PARAMETERS:
C
C         DIFV(1) = SLOPE OF DOUBLE HIT RESOLUTION
C                   EFFICIENCY DISTRIBUTION
C         DIFV(2) = ALL HITS WITH A DISTANCE LESS
C                   THAN DIFV(1) ARE NOT SEPARATED
C         DIFV(3) = ALL HITS WITH A DISTANCE GREATER
C                   THAN DIFV(3) ARE FULLY SEPARATED
C
C     EFFICIENCY
C     1 I            * * * I * * *
C       I          *     DIFV(3)
C       I         *
C       I        * DIFV(1)
C       I       *
C       I      *
C     0 I*****I------------------------- DISTANCE BETWEEN TWO HITS
C        DIFV(2)
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cjvtxc.for"
C
      DIMENSION HITV(2), HAREA(15), DIFV(3)
C
C------------------  C O D E  ------------------------------------------
C
C                            FUNCTION OF EFFICIENCY DISTRIBUTION
C
      EFF(DR) = 1. - EXP(-A1*(DR - A2))
C
      A1 = 1./DIFV(1)
      A2 = DIFV(2)
C
      IERASE = 0
      IF( NHITV .LT. 2 )  RETURN
C
C                            LOOP OVER ALL HITS AND SET
C                            AFFECTED WIRES TO ZERO
      N4  = NHITV*4
      N44 = N4 - 4
      IL  = 1
C
  100 IF( HITV(IL) .NE. 0 ) GO TO 200
         IL = IL + 4
         IF( IL .LT. N44 ) GO TO 100
            GO TO 2000
C
  200 IH = IL + 4
      ICALL = 0
C
  210 IF( IH .GT. N4 ) GO TO 2000
      IF( HITV(IH) .NE. 0 ) GO TO 300
C
  250 IH = IH + 4
      GO TO 210
C
  300 IF( HITV(IL) .NE. HITV(IH) ) GO TO 600
        ICALL = ICALL + 1
        HAREA(ICALL) = IH
        GO TO 250
  600 IF( ICALL .EQ. 0 ) GO TO 1000
         IDR = HITV(HAREA(1)+3) - HITV(IL+3)
         DR = ABS(FLOAT(IDR))*TIMEV
         IF( DR .GE. DIFV(3) )            GO TO 700
            IF( DR .LT. A2 )              GO TO 650
               IF( RN(DUM) .LE. EFF(DR) ) GO TO 700
  650    HITV(HAREA(1)) = 0
         IERASE = IERASE + 1
  700 IF( ICALL .LT. 2 ) GO TO 1000
         IEND = ICALL - 1
         DO 800 I = 1,IEND
            IDR = HITV(HAREA(I+1)+3) - HITV(HAREA(I)+3)
            DR = ABS(FLOAT(IDR))*TIMEV
            IF( DR .GE. DIFV(3) )            GO TO 800
               IF( DR .LT. A2 )              GO TO 750
                  IF( RN(DUM) .LE. EFF(DR) ) GO TO 800
  750       HITV(HAREA(I+1)) = 0
            IERASE = IERASE + 1
  800 CONTINUE
C
 1000 IL = IL + (1 + ICALL)*4
      GO TO 100
C
 2000 RETURN
      END
