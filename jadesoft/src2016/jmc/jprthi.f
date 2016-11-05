C   11/09/78 606101230  MEMBER NAME  JPRTHI9  (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE JPRTHI( R, P, PENETR, PENETZ, BARLOW, RHIGH,
     *                   ZLOW, ZHIGH, DRMAX, *, *, *, * )
C-----------------------------------------------------------------------
C
C   AUTHOR:   E. ELSEN     02/10/78 :  PROPAGATES PARTICLE IN NON
C                                      ABSORBING MEDIUM DEFINED BY
C                                      PARAMETERS  ( BARLOW, RHIGH,
C                                                    ZLOW, ZHIGH )
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
C           RETURN4 : PARTICLE STOPPED IN SYSTEM OR DECAY CONDITION
C                     REACHED.
C-----------------------------------------------------------------------
C
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
C
      DIMENSION R(5), P(7)
C
      DATA TWOPI / 6.283185 /
*** PMF  06/02/00 Loop counter provided by subroutines TRCDTV, TRCDET
      INTEGER ILOOP1,ILOOP2
      COMMON/CLOOP/ ILOOP1,ILOOP2
*** PMF (end)
C
C------------------------  C O D E  ------------------------------------
C
      IF( R(4) .GT. RHIGH ) RETURN
      IF( R(3) .GT. ZHIGH ) RETURN2
      IF( R(3) .LT. ZLOW  ) RETURN3
C
      PENETZ = 0.
      PENETR = 0.
      DPSI =  TWOPI / FLOAT( NCELL(1) )
C
      DO 100 ITERAT = 1,300
C
         R(5) = ATAN2( R(2), R(1) )
                IF( R(5) .LT. PSIIN(1) ) R(5) = R(5) + TWOPI
                NPHI = ( R(5) - PSIIN(1) ) / DPSI
                DPHI = R(5) - ( FLOAT( NPHI ) + .5 ) *DPSI - PSIIN(1)
***PMF 06/02/00      IF( R(4)*COS( DPHI ) .LT. BARLOW ) RETURN 1
*   Check the loop counter provided by TRCDET, TRCDTV
*   in order to detect infinite loops which sometimes happen 
*   for decreasing R (RETURN1). This is due to rounding errors
*   in the calculation of the particle position here and in
*   subroutine JCHAMB.
          IF( R(4)*COS( DPHI ) .LT. BARLOW ) THEN
           IF( ILOOP2.LT.100 ) RETURN 1
           PRINT *,'*** JPRTHI: Infinite loop detected!!!(PMF 06/02/00)'
           PRINT *,'*** RETURN1 case ignored after ',iloop2,' loops!!!'
           ILOOP2=0
          ENDIF
***PMF(end)
C
      CALL JSTEP( R, P, DRTOT, DRMAX )
        R(4) =  SQRT( R(1)*R(1) + R(2)* R(2) )
C      TOTAL TRACK LENGTH BIGGER THAN MAXIMUM VALUE?
              TOTLEN = TOTLEN  + DRTOT
              IF( TOTLEN .GT. STPLEN ) RETURN4
C
      IF( R(4) .LE. RHIGH ) GO TO 110
          PENETR = R(4) - RHIGH
          RETURN
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
