C   01/11/84 411011909  MEMBER NAME  RDRNHV   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE RDRNHV( IRNV, HITV )
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. HAGEMANN 25/04/79 :  CREATE RANDOM HIT IN
C             R. RAMCKE               VERTEX CHAMBER
C   LAST MOD  J. HAGEMANN 17/05/83 :  IMPROVED CODE
C
C      CREATE IRNV RANDOM HITS IN VERTEX CHAMBER AND STORE HITS
C      IN ARRAY HITV WITH INCREASING WIRE NUMBER.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cjvtxc.for"
#include "cgeov.for"
C
      DIMENSION HWBUFV(8), HITV(2)
C
C                          MAXIMUM DRIFTSPACE FOR EACH WIRE
      DIMENSION TWREL(7)
C
      DATA TWREL / 12.6, 13.8, 15.0, 16.2, 17.4, 18.7, 19.9 /
C
C------------------  C O D E  ------------------------------------------
C
      IHVTOT = IRNV
      IF( IHVTOT .EQ. 0 ) RETURN
      IL = 1
      IH = 1
C
C                            COMPUTE NEW HITS
      DO  100  J = 1, IHVTOT
         IWIRV = IFIX(AMIN1(168.*RN(DUM)+1,168.))
         HITV(IH) = IWIRV
C                            Z - AMPLITUDES
         ZET = ZVXCP*(1. - 2.*RN(DUM))
         HITV(IH+1) = HFIX(ZNAMP*(1 - ZET/ZMAXV))
         HITV(IH+2) = HFIX(ZNAMP*(1 + ZET/ZMAXV))
C                            DRIFT TIME
         NCEV =( IWIRV - 1 ) / 7
         IRV = IWIRV - NCEV * 7
         IF( IRV .LT. 1 .OR. IRV .GT. 7 ) GO TO 103
            HITV(IH + 3) = HFIX(TWREL(IRV)*RN(DUM)/TIMEV + .5)
  100       IH = IH + 4
C
            GO TO 105
  103    WRITE(6,104) IRV
  104    FORMAT('  --WARNING FROM RDRDMV-- WRONG WIRE NUMBER:', I4 )
         IH = IH + 4
  105    CONTINUE
C
C                            SORT HITS
         IWV4 = IHVTOT * 4
         M = IHVTOT
  200    M = M / 2
         IF( M .LE. 0 ) GO TO 500
            M4 = M*4
            K = IH - M4 - 4
            DO 400 J = IL, K, 4
               I = J
  300          IF( I .LT. IL ) GO TO 400
                  IM = I + M4
                  IF( HITV(I) .LT. HITV(IM) ) GO TO 400
                  IF( HITV(I) .EQ. HITV(IM) .AND.
     +                HITV(I+3) .GE. HITV(IM+3)) GO TO 400
                     IM2 = ( IM - 1 ) * 2
                     I2 = ( I - 1 ) * 2
                     CALL MVC( HWBUFV, 0, HITV, IM2, 8 )
                     CALL MVC( HITV, IM2, HITV, I2, 8 )
                     CALL MVC( HITV, I2, HWBUFV, 0, 8 )
                     I = I - M4
                     GO TO 300
  400       CONTINUE
            GO TO 200
  500    CONTINUE
      RETURN
      END
