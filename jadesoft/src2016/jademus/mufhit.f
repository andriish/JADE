C   26/07/83 308051411  MEMBER NAME  MUFHIT   (S)           FORTRAN77
C-----------------------------------------------------------------------
      SUBROUTINE MUFHIT(PHI,ALLOW,NUMHF)
C-----------------------------------------------------------------------
C
C        NAME:      MUFHIT
C        VERSION:   1.0
C        LANGUAGE:  FORTRAN 77
C        WRITTEN:   27/07/83              C. BOWDERY
C
C        THIS ROUTINE SEARCHES FOR MUON HITS IN LAYER 1 OF THE FILTER
C        WITHIN + /- 'ALLOW' DEGREES AROUND A  DIRECTION SPECIFIED BY
C        THE ANGLE 'PHI'. THE NUMBER FOUND IS RETURN IN 'NUMHF'.
C        NOTE: IF NO MUON BANKS EXIST, 'NUMHF' = -1 ON EXIT.
C
C        INPUT:     PHI         THE PHI ANGLE OF JADE(IN DEGREES)
C                   ALLOW       THE PLUS AND MINUS ANGULAR RANGE
C        OUTPUT:    NUMHF       THE NUMBER OF LAYER 1 MUON HITS FOUND
C-----------------------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
      LOGICAL FIRST
C
C                  BCS -- BOS COMMON.
C
      COMMON /BCS/ IDATA(40000)
      DIMENSION    HDATA(80000),ADATA(40000)
      EQUIVALENCE (HDATA(1),IDATA(1))
      EQUIVALENCE (ADATA(1),IDATA(1))
C
C-----------------------------------------------------------------------
C
C             FIRST CHECK TO SEE IF 'MUR1' BANKS EXIST. IF NOT, CREATE
C             THEM.
C
      NUMHF = 0
      FIRST = .TRUE.
 1    CALL CLOC(MUR10,'MUR1',0)
      CALL CLOC(MUR11,'MUR1',1)
C
      IF(MUR10.EQ.0 ) THEN
        IF(FIRST) THEN
          CALL MUANAC
          FIRST = .FALSE.
        ELSE
C                                      SET ERROR * RETURN
          NUMHF = -1
          RETURN
        ENDIF
      ELSE IF (MUR11.EQ.0) THEN
C                                      NO MUR1/1 BANK SO RETURN
        RETURN
      ENDIF
C
      IF( .NOT. FIRST) GO TO 1
C
C            LOOP OVER ALL MUON HITS:    NHITS = NUMBER OF HITS
C                                        NWHIT = NO. OF HALF WORDS/HIT
      NHITS = IDATA(MUR10 + 1)
      NWHIT = IDATA(MUR10 + 3)
C
      DO  2  I = 1,NHITS
        IP    = MUR11*2 + (I-1)*NWHIT
C                                        2 HALF WORD IS 10*LAYER + ORI.
        LAYER = HDATA(IP + 2) / 10
        IF(LAYER .NE. 1) GO TO 2
C                                        FIND X * Y OF LEFT AMBIGUITY
          XL = HDATA(IP + 3)
          YL = HDATA(IP + 4)
C                                        FIND PHI OF THIS COORDINATE
C
          PHIL = ATAN2(YL,XL) * 180.0 / 3.14159
          IF(PHIL .LT.0 ) PHIL = PHIL + 360.0
C
C                                        REPEAT FOR RIGHT AMBIGUITY
          XR = HDATA(IP + 6)
          YR = HDATA(IP + 7)
C
          PHIR = ATAN2(YR,XR) * 180.0 / 3.14159
          IF(PHIR .LT.0 ) PHIR = PHIR + 360.0
C
C                  IS EITHER AMBIGUITY WITHIN ALLOWED ANGULAR REGION?
C                  IF YES, THEN INCREMENT HIT FOUND COUNTER.
C
          ALLW = ABS(ALLOW)
          IF( ABS(PHIL-PHI) .LT. ALLW  .OR. ABS(PHIR-PHI) .LT. ALLW)
     *           THEN
             NUMHF = NUMHF + 1
          ENDIF
C
  2   CONTINUE
C
      RETURN
      END
