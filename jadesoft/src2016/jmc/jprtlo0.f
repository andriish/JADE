C   11/09/78 C8100201   MEMBER NAME  JPRTLO0  (CS7)         FORTRAN
      SUBROUTINE JPRTLO( R, P, PENETR, PENETZ, RLOW, BARHIG,
     *                   ZLOW, ZHIGH, *, *, *, * )
C  *--------------------------------------------------------
C  *
C  *  VERSION OF 02/10/78
C  *  PROPAGATES PARTICLE IN NON ABSORBING MEDIUM
C  *  DEFINED BY RLOW, BARHIG, ZLOW, ZHIGH.
C  *  UPON RETURN PENET((R) OR (Z)) IS PENETRATION LENGTH
C  *  IN NEXT ELEMENT IN R OR Z DIRECTION.
C  *  RETURN  : NEXT ELEMENT, INCREASING R
C  *  RETURN1 : NEXT ELEMENT, DECREASING R
C  *  RETURN2 : NEXT ELEMENT, INCREASING Z
C  *  RETURN3 : NEXT ELEMENT, DECREASING Z
C  *  RETURN4 : PARTICLE STOPPED IN SYSTEM OR DECAY CONDITION
C  *            REACHED.
C  *--------------------------------------------------------
C
       DIMENSION R(5), P(7)
      COMMON / CJTRLE / TOTLEN, STPLEN
C
      COMMON / CJCELL / NCELL(3),
     *                  NWIRES(3)
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
CAV
     + ,AVFOO(611)           
C
      DATA TWOPI / 6.283185 /
C
      IF( R(4) .LT. RLOW ) RETURN 1
      IF( R(3) .GT. ZHIGH ) RETURN 2
      IF( R(3) .LT. ZLOW  ) RETURN 3
C
      PENETZ = 0.
      PENETR = 0.
      DPSI =  TWOPI / FLOAT( NCELL(1) )
C
      DO 100 ITERAT = 1,300
C
         R(5) = ATAN2( R(2), R(1) )
                IF( R(5) .LT. PSIIN(1) ) R(5) = R(5) + TWOPI
                NPHI = ( R(5) - PSIIN(1) ) / DPSI + .5
                DPHI = R(5) - FLOAT( NPHI )*DPSI - PSIIN(1)
      IF( R(4)*COS( DPHI ) .GT. BARHIG ) RETURN
C
      CALL JSTEP( R, P, DRTOT )
        R(4) =  SQRT( R(1)*R(1) + R(2)* R(2) )
C      TOTAL TRACK LENGTH BIGGER THAN MAXIMUM VALUE?
              TOTLEN = TOTLEN  + DRTOT
              IF( TOTLEN .GT. STPLEN ) RETURN4
C
      IF( R(4) .GE. RLOW ) GO TO 110
          PENETR = RLOW - R(4)
          RETURN 1
  110 IF( R(3) .LE. ZLOW .AND. R(3) .LE. ZHIGH )  GO TO 100
          PENETZ = ZLOW - R(3)
          IF( PENETZ .GT. 0. ) RETURN 3
          PENETZ = R(3) - ZHIGH
          IF( PENETZ .GT. 0. ) RETURN 2
C
C
  100 CONTINUE
C
      RETURN 4
      END
