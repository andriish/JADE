C   19/02/84 402192331  MEMBER NAME  RDRDMH   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE RDRDMH( IRN, HITS )
C-----------------------------------------------------------------------
C
C   AUTHOR:   E. ELSEN    25/04/79 :  CREATE RANDOM HITS
C
C        MOD: E. ELSEN    29/09/83 :
C   LAST MOD: J. HAGEMANN 22/04/83 :  NO RANDOM HITS OUTSIDE CELLS
C             R. RAMCKE            :
C
C     CREATE IRN(I) RANDOM HITS FOR EVERY RING I AND STORE HITS
C     IN ARRAY HITS WITH INCREASING WIRE NUMBER.
C
C     RANDOM HITS OUTSIDE CELLS ARE NOW NOT POSSIBLE.
C     MAXIMUM DRIFTSPACE DRIMAX FOR EACH RING SET IN A DATA
C     STATEMENT.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / CJDRCH / RDEC(4),
     *                  PSIIN(3),
     *                  RINCR(3),
     *                  FIRSTW(3),
     *                  FSENSW(3),
     *                  RDEPTH,
     *                  SWDEPL,
     *                  YSUSPN,
     *                  TIMDEL(6), ZMAX, ZOFFS, ZRESOL, ZNORM,ZAL,ZSCAL,
     *                  DRIDEV,DRICOS,DRISIN
C
      COMMON / CBINMC / BINMC(6)
C
      DIMENSION HITS(2), IRN(3), IWOFF(3), WRANGE(3)
      DIMENSION HWBUF(8)
      DIMENSION DRIMAX(3)
C
      DATA WRANGE / 384., 384., 768. /
      DATA IWOFF / 0, 384, 768 /
      DATA DRIMAX / 47.5, 75.1, 51.2 /
C
C------------------  C O D E  ------------------------------------------
C
      IL = 1
      IH = 1
C
C                          LOOP OVER RINGS
      DO 2000 JR = 1, 3
      IHTOT = IRN(JR)
      IF( IHTOT .EQ. 0 ) GO TO 2000
C
C                          COMPUTE NEW HITS
      DO 100 J=1,IHTOT
C                          WIRE NUMBER
          IWIR = IFIX( WRANGE(JR)*RN(DUM) + .5 ) + IWOFF(JR)
      HITS(IH) = IWIR*8 + 1
C                          Z - AMPLITUDES
CCCC      XL = ( ZMAX*(1.-2.*RN(DUM)) - ZOFFS ) / ZAL
          XL = ( ZMAX*(1.-2.*RN(DUM))) / ZAL
          XA = ZNORM / ( .2 + .8*RN(DUM) )
      HITS(IH+1) = HFIX((  XL + .5 ) * XA)
      HITS(IH+2) = HFIX(( -XL + .5 ) * XA)
C                          DRIFT TIME
CCCCC HITS(IH+3) = 256.*RN(DUM) + .5
C----------------  N E W -----------------------------
      HITS(IH+3) = HFIX(256.*RN(DUM) + .5)
      IF(BINMC(JR) .GT. 0.)
     *         HITS(IH+3) = HFIX(DRIMAX(JR)*RN(DUM)/BINMC(JR) + .5)
C-----------------------------------------------------
  100 IH = IH + 4
C
C
CCC  ORDER HITS FOR THIS RING
      IW4 = IHTOT*4
      M = IHTOT
  200 M = M / 2
      IF( M .LE. 0 ) GO TO 500
      M4 = M*4
      K = IH - M4 - 4
      DO 400 J = IL, K, 4
             I = J
  300        IF( I .LT. IL ) GO TO 400
             IM = I + M4
          IF( HITS(I) .LT. HITS(IM) ) GO TO 400
          IF(HITS(I).EQ.HITS(IM) .AND. HITS(I+3).GE.HITS(IM+3)) GOTO 400
                  IM2 = ( IM - 1 ) * 2
                  I2 = ( I - 1 ) * 2
                  CALL MVC( HWBUF, 0, HITS, IM2, 8 )
                  CALL MVC( HITS, IM2, HITS, I2, 8 )
                  CALL MVC( HITS, I2, HWBUF, 0, 8 )
                  I = I - M4
                  GO TO 300
  400 CONTINUE
      GO TO 200
  500 CONTINUE
 2000 IL = IH
      RETURN
      END
