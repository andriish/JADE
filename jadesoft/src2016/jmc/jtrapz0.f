C   11/09/78 110071140  MEMBER NAME  JTRAPZ0  (S)           FORTRAN
      SUBROUTINE JTRAPZ( IFORM, X, P, XLOW, XHIGH, ZLOW, ZHIGH,
     +                   POT, ZARO, XRAD, IRETRN, CA, SA )
C--------------------------------------------------------
C
C  VERSION OF 02/10/78   LAST MOD 07/10/81   E.ELSEN
C  PROPAGATES PARTICLE IN NON ABSORBING MEDIUM
C  DEFINED BY XLOW, XHIGH, ZLOW, ZHIGH.
C  CA AND SA ARE THE OVERALL COS AND SIN FOR ROTATION
C  BACK TO MAIN COORDINATE SYTEM
C  THE EXACT GEOMETRY IS DEFINED BY SWITCH IFORM
C  IRETRN GIVES RETURN CODE.
C  IRETRN=0 : NEXT ELEMENT, INCREASING X
C        =1 : NEXT ELEMENT, DECREASING X
C        =2 : NEXT ELEMENT, INCREASING Z
C        =3 : NEXT ELEMENT, DECREASING Z
C        =4 : PARTICLE STOPPED IN SYSTEM OR DECAY CONDITION
C             REACHED.
C        =5 : NEXT ELEMENT, CHANGING PHI
C--------------------------------------------------------
C
      DIMENSION X(5), P(7)
      COMMON / CJTRLE / TOTLEN, STPLEN
      COMMON / CJSWLO / ITIMOD, MULSC, ELOSS
      LOGICAL  ELOSS, MULSC
      COMMON / CJXDAT / XSLOPE, YSLOPE, XL(3), XH(3), R3P, RD3P,
     +                  S, S2,
     +                  XSL3L, X3L, XSL3H, X3H, YSL3L, Y3L, YSL3H,
     +                  YHWIDT, SINHLF, COSHLF, DRITAN
C
      DATA PMIN / 0.01 /
C
      IRETRN = 4
C
      DO 100 ITERAT = 1,300
C
      GO TO ( 10, 11, 12 ), IFORM

   10 IF( X(1) .LE. XHIGH ) GO TO 20
          IRETRN = 0
          RETURN
   20 IF( X(1) .GE. XLOW ) GO TO 30
          IRETRN = 1
          RETURN
C
   11 IF( X(1) .LE. XSLOPE*ABS(X(2))+XHIGH ) GO TO 21
          IRETRN = 0
          RETURN
   21 IF( X(1) .GE. XSLOPE*ABS(X(2))+XLOW ) GO TO 30
          IRETRN = 1
          RETURN
C
   30 IF( ABS( X(2) ) .LE.  YSLOPE*X(1) ) GO TO 40
          IRETRN = 5
          RETURN
C
   12 IF( X(1) .LE. XSLOPE*ABS(X(2))+XHIGH ) GO TO 22
          IRETRN = 0
          RETURN
   22 IF( X(1) .GE. XLOW ) GO TO 32
          IRETRN = 1
          RETURN
   32 IF( ABS( X(2) ) .LE.  YHWIDT ) GO TO 40
          IRETRN = 5
          RETURN
C
   40 IF( X(3) .LE. ZHIGH  ) GO TO 50
          IRETRN = 2
          RETURN
   50 IF( X(3) .GE. ZLOW  ) GO TO 60
          IRETRN = 3
          RETURN
C
   60 CALL JSTEP( X, P, DRTOT )
C      TOTAL TRACK LENGTH BIGGER THAN MAXIMUM VALUE?
              TOTLEN = TOTLEN  + DRTOT
              IF( TOTLEN .GT. STPLEN ) RETURN
C
          IF( MULSC ) CALL JMULSC( P, DRTOT / XRAD )
          IF( ELOSS ) CALL JELOSS( P, DRTOT, POT, ZARO, XRAD,
     +                             X, CA, SA )
          IF( P(6) .LT. PMIN ) RETURN
C
C
  100 CONTINUE
C
      RETURN
      END
