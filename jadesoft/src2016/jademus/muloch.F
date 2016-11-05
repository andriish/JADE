C   16/12/83 312161621  MEMBER NAME  MULOCH   (JADEMUS)     FORTRAN
C
C   LAST CHANGE  16/12/83 CHRIS BOWDERY  VARIABLES RENAMED IN PLACES
C        CHANGE  12/07/82 CHRIS BOWDERY  TO CORRECT 3 BUGS
C
C-----------------------------------------------------------------------
      SUBROUTINE MULOCH
C-----------------------------------------------------------------------
C
C             THIS SUBROUTINE PERFORMS THE 'LET-OFF' CHECK
C
C    INPUT:   REGDEV : A VECTOR CONTAINING A SEQUENCE OF:
C                      < REGION NUMBER , ABSORBER ENCOUNTERED , HIT
C                        DEVIATION IN UNITS OF SIGMA >
C             REGDCE : A VECTOR CONTAINING A SEQUENCE OF :
C                      < REGION NUMBER , ABSORBER ENCOUNTERED , LOWER
C                        LIMIT OF 'DCE' , UPPER LIMIT OF 'DCE' >
C                      WHERE 'DCE' IS A DEAD CHAMBER OR EDGE
C             REGZE  : A VECTOR LIKE REGDCE BUT FOR LONGITUDINAL EDGES
C
C    OUTPUT:  INEFF  : INCREMENTED FOR EVERY 'MISSING' REGION FOUND
C             INFLAG : SET TO 2 IF THE TRACK IS OUTER LAYER 'MISSING'
C             LOCHRS : A VECTOR CONTAINING A FLAG FOR EVERY REGION
C                       0 = NOT AFFECTED
C                       1 = MULOCH COULD NOT TEST THE REGION
C       FLAG CODE =     2 = REGION WAS NOT 'LET-OFF'
C                       3 = REGION WAS GIVEN THE BENEFIT OF THE DOUBT
C                       4 = REGION WAS 'LET-OFF'
C
C             THE ACTUAL FLAG IS  10*(DRIFT CODE) + LONGITUDINAL CODE
C             ---> A 'MISSING' REGION HAS CODE '02' OR '20' OR '22'
C
C-----------------------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
C
C             COMMONS AND ARRAY DECLARATIONS
C
#include "cmufwork.for"
#include "cmureg.for"
#include "cmucalib.for"
      COMMON /CMULOC/ ALLDEV(12,2),ALLABS(12),REGDEV(30),REGDCE(60),
     +                REGZE(60),LOCHRS(60),ABLOCH(60)
C
      DIMENSION LVREGH(10),DEVIAT(10),ABSORB(10),SLOPE(9),AVEDEV(9)
      DIMENSION LVAL(10),LFLAG1(10)
C
C-------------------- CODE ---------------------------------------------
C
C             ARE THERE ANY 'LET-OFFS' TO BE CHECKED ?
C             ARE THERE ANY HITS ANYWAY ?
C             HAS THE TRACK ALREADY FAILED ?
C
      IF( (REGDCE(1).EQ.0 .AND. REGZE(1).EQ.0)
     +    .OR.   NTHIS.EQ.0
     +    .OR.   INEFF.GT.1                    ) RETURN
C
C             ARE THERE AT LEAST 2 ASSOCIATED HITS IN DIFFERENT
C             LAYERS OUTSIDE THE YOKE ?
C
      IF(NHLAYR.LT.4) RETURN
      CALL VZERO(LOCHRS,60)
      CALL VZERO(ABLOCH,60)
      CALL VZERO(LVREGH ,10)
C
C             UNPACK THE DEVIATIONS AND AVERAGE THE OVERLAP HITS
C             AND SEPARATE THE ABSORPTION VALUES.
C                LREGN   = PREVIOUS REGION NUMBER
C                NRGHIT  = NUMBER OF REGIONS WITH HITS
C                LVREGH  = VECTOR OF REGION NUMBERS WITH HITS
C
      LREGN  = 0
      NRGHIT = 0
C
      DO 1  I = 1,30,3
        IREGN  = REGDEV(I)
        IF(IREGN.EQ.0) GO TO 8
C
C                            BAD HITS HAVE DEVIATION=100 . IGNORE.
C
        IF(REGDEV(I + 2) .EQ.100.) GO TO 1
C
C                            LOOK FOR OVERLAP HITS AND AVERAGE
C
        IF(IREGN.NE.LREGN) GO TO 2
          DEVIAT(NRGHIT) = ( DEVIAT(NRGHIT) + REGDEV(I + 2) ) / 2.0
          LVAL(NRGHIT)   = 2
          GO TO 1
 2      LREGN  = IREGN
        NRGHIT = NRGHIT + 1
        LVREGH(NRGHIT)  = IREGN
        ABSORB(NRGHIT)  = REGDEV(I + 1)
        DEVIAT(NRGHIT)  = REGDEV(I + 2)
        LVAL(NRGHIT)    = 1
 1    CONTINUE
C
 8    KREGH = NRGHIT - 1
C+++
C     WRITE(6,97)(REGDCE(I),I=1,36)
C97   FORMAT(' REGDCE'/(12(X,F10.3) )   )
C     WRITE(6,96)(REGZE(I),I=1,24)
C96   FORMAT(' REGZE'/(12(X,F10.3) )   )
C     WRITE(6,98) NRGHIT,IDTRK,(DEVIAT(I),ABSORB(I),I=1,NRGHIT)
C98   FORMAT(' NRGHIT,IDTRK / DEVIAT,ABSORB ',2I3/2(X,F10.4) )
C+++
      IF(NRGHIT.GT.1) GO TO 3
C
C                  WE ONLY HAVE ONE HIT TO WORK ON. ASSUME THAT
C                  IT LIES ON A LINE WITH ZERO SLOPE IN UNITS OF
C                  SIGMA.
C
        KREGH    = 1
        SLOPE(1) = 0.0
        LFLAG1(1)= 1
        LFLAG2   = 0
        FCT      = 7.5
        IF(LVAL(1).EQ.2)     FCT = 6.0
        IF(DEVIAT(1).LT.0.5) FCT = 5.0
        GO TO 14
C
C                  TAKE EACH ADJACENT PAIR OF HITS AND CALCULATE
C                  THE SLOPE OF THE LINE JOINING THEM.
C
 3    DO 5  I = 1,KREGH
        DIFDEV =  DEVIAT(I + 1) - DEVIAT(I)
        SEPABS =  ABSORB(I + 1) - ABSORB(I)
        IF(SEPABS .LT. 0.1 ) SEPABS = 0.1
        SLOPE(I)  =  DIFDEV / SEPABS
        AVEDEV(I) =  ( DEVIAT(I + 1) + DEVIAT(I) ) / 2.0
C
C                  IS THE SLOPE REASONABLE ?
C
        LFLAG1(I) = 0
        IF(ABS(SLOPE(I) ) .GT. 1.5 ) LFLAG1(I) = 1
        IF(ABS(SLOPE(I) ) .GT. 5.0 ) SLOPE(I)  = 0.0
 5    CONTINUE
C
C                  ARE THE SLOPES CONSISTENT ?
C
 6    NPSLOP = KREGH - 1
      LFLAG2 = 0
      IF(NPSLOP.LT.1) GO TO 24
      DO 7  I = 1,NPSLOP
        IF(ABS(SLOPE(I)-SLOPE(I+1) ) .GT. 0.4 ) LFLAG2 = 1
        IF(ABS(SLOPE(I)-SLOPE(I+1) ) .GT. 1.0 ) LFLAG2 = 2
 7    CONTINUE
C
C             CALCULATE THE FACTORS USED FOR DETERMINING ALLOWANCES
C
 24   FCT = 4.5
      IF(LFLAG2.EQ.1) FCT =  7.0
      IF(LFLAG2.EQ.2) FCT = 10.0
      IF(LFLAG2.EQ.0 .AND. ABS(SLOPE(1))  .LT. 0.35) FCT = FCT - 1.0
      IF(LFLAG2.EQ.0 .AND. ABS(DEVIAT(1)) .LT. 0.35) FCT = FCT - 1.0
C
C             FIND THE LONGITUDINAL LET-OFFS AND CHECK THEIR
C             CONSISTENCY WITH THE HITS IN THE SAME UNIT.
C             THE PLINTH AND A SIDEWALL CAN BE TREATED TOGETHER HERE.
C             IF THERE IS A HIT AFTER A Z LET-OFF THEN THE LET-OFF
C             WAS WRONG PROVIDED THE CHAMBERS ARE THE SAME LENGTH.
C
 14   DO 17  I = 1,60,4
        IREGN = REGZE(I)
        IF(IREGN.EQ.0) GO TO 18
        ABSZE = REGZE(I+1)
        IFACE = HRFACE(IREGN)
C
C                       SELECT AS DEFAULT THE CODE FOR UNTESTABLE.
C
        IF(LOCHRS(IREGN).EQ.0) LOCHRS(IREGN) = 1
        ABLOCH(IREGN) = ABSZE
        DO 19  J = 1,NRGHIT
          JREGN = LVREGH(J)
C
C                            PLINTH AND SIDEWALLS ARE COMPATIBLE
C
          IF(HRFACE(JREGN).NE.IFACE .AND. .NOT.(IFACE.EQ.3 .AND.
     +        HRORI(JREGN).EQ.1)    ) GO TO 19
C
C                       SOME CHAMBERS ARE SHORTER THAN OTHERS
C                       SO THE CHECK CANNOT BE DONE. NOT FOR
C                       THE ENDWALLS.
C
          IF( HRORI(JREGN).EQ.3 ) GO TO 16
          IF( ABS(ZRLO(JREGN)-ZRLO(IREGN) ) .GT. 150. .OR.
     +        ABS(ZRHI(JREGN)-ZRHI(IREGN) ) .GT. 150.  ) GO TO 19
C
 16       IF(ABSZE.GE.ABSORB(J) ) GO TO 19
C
C                       IF THE TRACK IS ROUGHLY HEADING
C                       FOR THE Z GAP IN THE ROOF, WE
C                       CANNOT USE THE Z CONSISTENCY TEST.
C                       USE A COSEC THETA EXCLUSION CHECK.
C
C                       PASS TEST IF 120 < THETA < 60
C
          IF(IFACE.EQ.4 .AND. COSEC.LT.1.155 ) GO TO 19
C
          IF(LOCHRS(IREGN).LT.2) LOCHRS(IREGN) = 2
C
 19     CONTINUE
 17   CONTINUE
C
C             FIND THE DRIFT REGIONS THAT REQUEST TO BE 'LET-OFF'
C             AND DETERMINE WHETHER EXTRAPOLATION OR INTER-
C             POLATION CAN BE USED TO CHECK THEM.
C
 18   DO 10  I = 1,60,4
        IREGN = REGDCE(I)
        IF(IREGN.EQ.0) GO TO 28
C
C                       MARK THE REGION AS UNTESTABLE AS DEFAULT
C
        IF(LOCHRS(IREGN).LT.10 ) LOCHRS(IREGN) = LOCHRS(IREGN) + 10
        ABSDCE = REGDCE(I+1)
        ABLOCH(IREGN) = ABSDCE
C                            NEED EXTRAPOLATION BACKWARDS
        K = 1
        IF( ABSORB(K).LE.ABSDCE ) GO TO 12
C                            IS IT UNTESTABLE ? JUMP IF YES
          IF( HRORI(LVREGH(K)).EQ.3 .AND. HRORI(IREGN).EQ.1 ) GO TO 10
 38       IF(LVREGH(K+1).EQ.0) GO TO 41
          IF( HRORI(LVREGH(K)).NE.1 .OR. HRORI(LVREGH(K+1)).NE.3 )
     +                                           GO TO 39
 41         SLOPE(K) = 0.0
            ALW = (ABSORB(K)-ABSDCE) * 0.9
            IF(ALW.GT.2.0) ALW = 2.0
            IF(ALW.LT.0.3) ALW = 0.30
            GO TO 42
 39       IF(LFLAG1(K).EQ.1) FCT = FCT * 1.5
          ALW = FCT * (ABSORB(K)-ABSDCE) * 0.10
          IF(ALW.GT.2.0) ALW = 2.0
          IF(ALW.LT.0.3) ALW = 0.3
 42       CALL MULOC2 (LQ,I,ALW,SLOPE(K),ABSDCE,ABSORB(K),DEVIAT(K) )
          GO TO 15
C
C                            TAKE EACH ADJACENT HIT PAIR AND
C                            DETERMINE IF A 'LET-OFF' REGION LIES
C                            IN BETWEEN.
C
 12     DO 20  M = 1,KREGH
          IF( LVREGH(M+1).EQ.0) GO TO 20
          IF( ABSORB(M+1).LT.ABSDCE ) GO TO 20
C
C                            DON'T BOTHER IF THE 'LET-OFF' REGION IS
C                            IN A SIDEWALL AND THE HITS ARE IN THE
C                            PLINTH AND ENDWALL
C
            IF(HRORI(LVREGH(M)).EQ.2.AND.HRORI(IREGN).EQ.1 .AND.
     +         HRORI(LVREGH(M+1)).EQ.3) GO TO 10
C
C                            IF THE 2 HITS ARE IN THE PLINTH AND ENDWALL
C                            THEN INTERPOLATION BETWEEN THE HITS IS NOT
C                            POSSIBLE. USE EXTRAPOLATION.
C
          IF(HRORI(LVREGH(M)).EQ.1.AND.HRORI(LVREGH(M+1)).EQ.3) GO TO 35
CAV FIXME          00025500
C
C                            --->   INTERPOLATE TO CHECK 'LET-OFF'  <---
C
              IF(LFLAG1(M).EQ.1) FCT = FCT * 1.5
              ALW = FCT * (ABSORB(M+1)-ABSORB(M) ) * 0.035
              IF(ALW .GT. 1.2 ) ALW = 1.2
              IF(ALW.LT.0.25) ALW = 0.25
              CALL MULOC2 (LQ,I,ALW,SLOPE(M),ABSDCE,ABSORB(M),DEVIAT(M))
              GO TO 15
C
 35           IF(HRORI(IREGN).EQ.3) GO TO 36
                K = M
                IF(K.EQ.1) GO TO 44
              GO TO 37
 36             K = M + 1
                IF(K.EQ.NRGHIT) GO TO 41
              GO TO 38
 20     CONTINUE
C                            --->  EXTRAPOLATION FORWARDS   <---
        K = KREGH
        IF(LVREGH(K+1).EQ.0) GO TO 44
C                            IS IT UNTESTABLE ? JUMP IF YES
        IF(HRORI(IREGN).EQ.3 .AND. HRORI(LVREGH(K+1)).EQ.1) GO TO 10
        IF(HRORI(IREGN).NE.3 .OR. HRORI(LVREGH(K)).NE.1) GO TO 37
          K = K +1
 44       SLOPE(K) = 0.0
          ALW = (ABSDCE-ABSORB(K) ) * 0.9
          IF(ALW.GT.2.0) ALW = 2.0
          IF(ALW.LT.0.3) ALW = 0.3
          GO TO 43
C
 37     IF(LFLAG1(K).EQ.1) FCT = FCT * 1.5
        ALW = FCT * (ABSDCE-ABSORB(K+1) ) * 0.10
        IF(ALW.GT.2.0)  ALW = 2.0
        IF(ALW.LT.0.3) ALW = 0.3
 43     CALL MULOC2 (LQ,I,ALW,SLOPE(K),ABSDCE,ABSORB(K),DEVIAT(K) )
 15     IF(LQ*10 .GT. LOCHRS(IREGN) )
     +        LOCHRS(IREGN) = LQ * 10 + MOD(LOCHRS(IREGN),10)
 10   CONTINUE
C
C             DETERMINE THE NUMBER OF TRUE INEFFICIENCIES FOR
C             THE TRACK.
C
 28   DO 30  IREGN = 1,60
        LCODE = LOCHRS(IREGN)
        IF(LCODE.NE.22 .AND. LCODE.NE.2 .AND. LCODE.NE.20 ) GO TO 30
          INEFF = INEFF + 1
C                       IF THIS IS THE LAST REGION, FLAG IT AS O.L.I.
          IF(ABLOCH(IREGN).GE.ABSORB(NRGHIT) ) INFLAG = 2
 30   CONTINUE
C
C+++
C     IF(INFLAG.EQ.2) WRITE(6,99)IDTRK
C99   FORMAT(' TRACK ',I2,' REJECTED BY MULOCH')
C     WRITE(6,99) INEFF,NHLAYR,INFLAG,LOCHRS
C99   FORMAT(' INEFF,NHLAYR,INFLAG / LOCHRS ',3I3/2(X,30I3/))
C+++
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE MULOC2 (LQUAL,IP,ALLOW,SLOPE,ABSDCE,ABSORB,DEVIAT)
C-----------------------------------------------------------------------
C
C             THIS SUBROUTINE CALCULATES THE EXPECTED DEVIATION
C             AT A REGION SITTING BEHIND 'ABSDCE' OF ABSORBER
C             USING A STRAIGHT LINE WITH SLOPE 'SLOPE' AND PASSING
C             THROUGH A POINT WITH DEVIATION 'DEVIAT' SITTING BEHIND
C             'ABSORB' OF ABSORBER.
C
C     IP     IS A POINTER TO THE 1ST WORD IN REGDCE FOR THIS REGION
C     LQUAL  IS THE RETURNED QUALITY FLAG
C               2 = TRACK WAS INSIDE ACTIVE REGION
C               3 = TRACK PROBABLY WAS OUTSIDE ACTIVE REGION
C               4 = TRACK WAS ALMOST CERTAINLY OUTSIDE ACTIVE REGION
C     ALLOW  IS THE QUANTITY USED TO DETERMINE WHETHER A TRACK WAS CLOSE
C            TO THE EDGE OF THE ACTIVE REGION
C
C-----------------------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
C
      COMMON /CMULOC/ ALLDEV(12,2),ALLABS(12),REGDEV(30),REGDCE(50),
     +                REGZE(50),LOCHRS(60),ABLOCH(60)
C
      ESTDEV = SLOPE * (ABSDCE - ABSORB) + DEVIAT
C
      IF(ESTDEV.GE.REGDCE(IP+2) .AND. ESTDEV.LE.REGDCE(IP+3))GO TO 2
        IF(ESTDEV+ALLOW.GE.REGDCE(IP+2)  .AND.
     +     ESTDEV-ALLOW.LE.REGDCE(IP+3)      ) GO TO 1
             LQUAL = 2
             RETURN
 1         LQUAL = 3
           RETURN
C
 2      LQUAL = 4
        RETURN
        END
