C   29/11/77 C8100201   MEMBER NAME  JPRFRE0  (CS7)         FORTRAN
      SUBROUTINE JPRFRE( R, P, PENETR, PENETZ, RDOWN, RUP,
     *                   ZDOWN, ZUP, *, *, *, * )
C  *--------------------------------------------------------
C  *
C  *  VERSION OF 02/10/78
C  *  PROPAGATES PARTICLE IN NON ABSORBING MEDIUM
C  *  DEFINED BY RDOWN, RUP, ZDOWN, ZUP.
C  *  UPON RETURN PENET((R) OR (Z)) IS PENETRATION LENGTH
C  *  IN NEXT ELEMENT IN R OR Z DIRECTION.
C  *  RETURN  : NEXT ELEMENT, INCREASING R
C  *  RETURN1 : NEXT ELEMENT, DECREASING R
C  *  RETURN2 : NEXT ELEMENT, INCREASING Z
C  *  RETURN3 : NEXT ELEMENT, DECREASING Z
C  *  RETURN4 : PARTICLE STOPPED IN SYSTEM
C  *--------------------------------------------------------
C
       DIMENSION R(5), P(7)
      COMMON / CJTRLE / TOTLEN, STPLEN
C
      IF( R(4) .GT. RUP ) RETURN
      IF( R(4) .LT. RDOWN ) RETURN1
      IF( R(3) .GT. ZUP ) RETURN2
      IF( R(3) .LT. ZDOWN ) RETURN3
C
      PENETZ = 0.
      PENETR = 0.
      DO 100 ITERAT = 1,300
      IF( R(4) .LE. RUP  ) GO TO 20
          PENETR = R(4) - RUP
          RETURN
   20 IF( R(4) .GE. RDOWN ) GO TO 30
          PENETR = RDOWN - R(4)
          RETURN 1
   30 IF( R(3) .LE. ZUP ) GO TO 40
          PENETZ = R(3) - ZUP
          RETURN 2
   40 IF( R(3) .GE. ZDOWN ) GO TO 50
          PENETZ = ZDOWN - R(3)
          RETURN 3
C
   50 CALL JSTEP( R, P, DRTOT )
        R(4) =  SQRT( R(1)*R(1) + R(2)* R(2) )
C        TOTAL TRACK LENGTH BIGGER THAN MAXIMUM VALUE?
            TOTLEN = TOTLEN  + DRTOT
            IF( TOTLEN .GT. STPLEN ) RETURN4
  100 CONTINUE
C
      RETURN 4
      END
