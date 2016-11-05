C   29/11/77 606101229  MEMBER NAME  JPRFRE9  (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE JPRFRE( R, P, PENETR, PENETZ, RDOWN, RUP,
     +                   ZDOWN, ZUP, DRMAX, *, *, *, * )
C-----------------------------------------------------------------------
C
C   AUTHOR:   E. ELSEN     02/10/78 :  PROPAGATES PARTICLE IN NON
C                                      ABSORBING MEDIUM DEFINED BY
C                                      PARAMETERS  ( RDOWN, RUP,
C                                                    ZDOWN, ZUP )
C
C  LAST MOD J. HAGEMANN    28/08/84 :  NEW VERSION OF SUBROUTINE JSTEP
C                                      INCLUDED
C
C        UPON RETURN PENET((R) OR (Z)) IS PENETRATION
C        LENGTH IN NEXT ELEMENT IN R OR Z DIRECTION.
C
C           RETURN  : NEXT ELEMENT, INCREASING R
C           RETURN1 : NEXT ELEMENT, DECREASING R
C           RETURN2 : NEXT ELEMENT, INCREASING Z
C           RETURN3 : NEXT ELEMENT, DECREASING Z
C           RETURN4 : PARTICLE STOPPED IN SYSTEM
C-----------------------------------------------------------------------
C
      COMMON / CJTRLE / TOTLEN, STPLEN
C
      DIMENSION R(5), P(7)
C
C------------------------  C O D E  ------------------------------------
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
   50 CALL JSTEP( R, P, DRTOT, DRMAX )
        R(4) =  SQRT( R(1)*R(1) + R(2)* R(2) )
C        TOTAL TRACK LENGTH BIGGER THAN MAXIMUM VALUE?
            TOTLEN = TOTLEN  + DRTOT
            IF( TOTLEN .GT. STPLEN ) RETURN4
  100 CONTINUE
C
      RETURN 4
      END
