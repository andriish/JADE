C   05/08/86 705201513  MEMBER NAME  FADDRW   (S)           FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE FADDRW(LDOS,LSEP,LINEAR)
C-----------------------------------------------------------------------
C
C    AUTHOR:   G. ECKERLIN    5/08/86 :  JET CHAMBER RAW DATA DISPLAY
C       MOD:   G. ECKERLIN   18/08/86 :  USER KOORDINATES USED NOW
C       MOD:   G. ECKERLIN   20/08/86 :  WIRE RANGE SELECTION SUPPORTED
C       MOD:   G. ECKERLIN    4/09/86 :  DESPLAY DETAILS IMPLEMENTED
C  LAST MOD:   E. ELSEN      20/05/87 :  REDUCE SIZE OF BCS FROM 80K
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL LSEP, LDOS, LINEAR, LP, LTRACK, DSPDTM
C
      INTEGER MASK /Z'FF0000'/
C
      COMMON / BCS / IW(1)
      DIMENSION HW(1), RW(1)
      EQUIVALENCE (HW(1), IW(1), RW(1))
C
      COMMON / CGRAP2 / BCMD,DSPDTM(30),ISTVW,JTVW
      COMMON /CWORK1/ IAMP(300,2), ICWIR, IPED(2,16), NWIRNG(2), IT0
     +                ,ISTWIR(16), IENWIR(16), NP0WIR(16)
     +                ,ISPRE2(8), TPREV(8), ISPRE1(8)
      COMMON / CORDER / KWR(17), KSIDE, KSTART, KEND, KTIMB, KEV
      COMMON / CKORD  / RX1, RX2,
     +                  RY1, RY2
C
      DIMENSION IWRADE(7), HAMP(2,256), BUFFER(3,8)
C
C
      DATA IWRADE / 87, 105, 114, 101, 96, 96, 96 /
      DATA ICALL  / 0 /
      DATA IPOINT / 18 /, SCALE / 10. /
C
C-----------------  C O D E  -------------------------------------------
C
C                                   SET DISPLAY FLAG FOR TRACK
      LTRACK = DSPDTM(23)
      IF (LSEP) LTRACK = .FALSE.

C                                   SET POINTER FOR JHTL
      NPJHTL = IW(IBLN('JHTL')) * 2
C                                                FETCH FADC-DATA
      IPREL =(ICWIR - 1)*250
      DO 60 JS = 1,2
          DO 50 JJ = 1,256
              HAMP(JS,JJ) = IAMP(JJ,JS)
  50      CONTINUE
  60  CONTINUE
C                                   DIFFERENCIATE IF CDTL 51
      IF( .NOT. LDOS ) GO TO 100
         DO 90 JS = 1, 2
C
            DO 80 JJ = 0, 254
               H1 = HAMP(JS,255-JJ)
               H2 = HAMP(JS,256-JJ)
               HAMP(JS,256-JJ) = H2 - H1
  80        CONTINUE
            HAMP(JS,1) = 0
  90  CONTINUE
C
 100  IF( .NOT. LSEP ) GOTO 120
         KSIDE = 1
         RX1 = 300.
         RX2 = 2100.
 120  IF(ICALL .GT. 0) GOTO 130
         KRANGE = KEND - KSTART
         RYMAX  = INT(RY2 - RY1)/KWR(17) - 50
         ISCRCH = RX2 - RX1
C
 130  RYO = INT(RY1) + INT(RYMAX + 50)*ICALL
      IF( LDOS ) THEN
        IF (.NOT.LINEAR) THEN
          RYO = INT(RYO) + INT(RYMAX)*24/64
        ELSE
          RYO = INT(RYO) + INT(RYMAX)*96/256
        ENDIF
      ENDIF
C
C
      IXO = RX1
      RX  = RX1
      RY  = RYO
C
C                                               DRAW FADC-DATA
      LSIDE = KSIDE
 150  IF(LSIDE .EQ. 3) LSIDE = 2
      CALL MOVEA(RX,RY)
      KSTR1 = KSTART + 1
      DO 200 IREP = KSTR1,KEND
         LP = .FALSE.
         IF (.NOT.LINEAR) THEN
           RDY = INT(RYMAX*HAMP(LSIDE, IREP))/64
         ELSE
           RDY = INT(RYMAX*HAMP(LSIDE, IREP))/256/SCALE
         ENDIF
         IF(RDY .GE. RYMAX) RDY = RYMAX
         RYY = RY + RDY
         IF(KSIDE .NE. 3 .OR. LSIDE .NE. 2) GOTO 180
            CALL DASHA(RX, RYY, 34)
            RX = IXO + ISCRCH*(IREP-KSTART)/KRANGE
            IF(RDY .LT. RYMAX) GOTO 170
               CALL DASHA(RX, RYY, 1)
               GOTO 200
 170        CALL DASHA(RX, RYY, 34)
            GOTO 200
 180     CONTINUE
         CALL DRAWA(RX, RYY)
         RX = IXO + ISCRCH*(IREP-KSTART)/KRANGE
         IF(RDY .LT. RYMAX) GOTO 190
            CALL DASHA(RX, RYY, 1)
            GOTO 200
 190     IF( LP ) CALL DASHA(RX,RYY,1)
         IF( .NOT. LP ) CALL DRAWA(RX, RYY)
 200  CONTINUE
C
C                                                REPEAT IF KSIDE = BOTH
      IF(KSIDE .NE. 3 .OR. LSIDE .NE. 2) GOTO 210
      LSIDE = 1
      RX = IXO
      GOTO 150
 210  CONTINUE
C                                                WRITE WIRE NUMBER
      IF (LSIDE.EQ.1) THEN
        RYWR = 100
        IF( LDOS ) RYWR = 190
        CALL MOVEA(RX1 - 250., RYO + RYMAX - RYWR)
        CALL CHRSIZ(4)
        IF (ICWIR.LT.10) THEN
          IWRADE(7) = ICWIR + 48
          IWRADE(6) = 48
        ELSE
          IWRADE(7) = ICWIR - (ICWIR/10)*10 + 48
          IWRADE(6) = ICWIR/10 + 48
        ENDIF
        CALL HLABEL(7,IWRADE)
      ENDIF
C                                   POINTER FOR JETC TIME
      IPHIT = ISTWIR(ICWIR)
      IF (LTRACK) THEN
        DO 4002 IHIT = 1, 8
          BUFFER(1,IHIT) = 0
          BUFFER(2,IHIT) = 0
          BUFFER(3,IHIT) = 0
 4002   CONTINUE
        NHIT = 1
      ENDIF
 4000 IF (IPHIT.GE.IENWIR(ICWIR)) GOTO 4001
C       IAL = HW(IPHIT + 1)
C       IAR = HW(IPHIT + 2)
        ITJETC = HW(IPHIT + 3)
CX      WRITE (6,9101) ICWIR, IAL, IAR, ITJETC
 9101   FORMAT(' WIRE , AL, AR, T :',4I5)
        ITIM = (ITJETC + IT0)/85
        XPNT = (RX2 - RX1) * ITIM /(KEND - KSTART) + RX1
        IPOINT = 18
        IF (NPJHTL.GT.0) THEN
          NPHIT = (IPHIT-NP0WIR(ICWIR))/2+ 3
*** PMF 10/06/99
***          ITRK1 = ISHFTR(LAND(HW(NPJHTL+NPHIT),255),1)
***          ITRK2 = ISHFTR(LAND(HW(NPJHTL+NPHIT+1),255),1)
          ITRK1 = ISHFTR(hLAND(HW(NPJHTL+NPHIT),hint(255)),1)
          ITRK2 = ISHFTR(hLAND(HW(NPJHTL+NPHIT+1),hint(255)),1)
*** PMF (end)
CX        WRITE (6,9111) ITRK1, ITRK2, NPHIT
 9111     FORMAT(' TRACKS :',3I5)
          IF (ITRK1.GT.0 .OR. ITRK2.GT.0) THEN
C           IPOINT = 48 + ITRK1 - (ITRK1/10)*10
            IPOINT = 2
          ENDIF
C                                   FIND TRACKS
          IF (LTRACK) THEN
            DO 4003 IH = 1, 8
              IF (ITRK1.NE.0) THEN
                IF (ITRK1.EQ.ISPRE1(IH) .OR. ITRK1.EQ.ISPRE2(IH)) THEN
C                 CALL TRMOUT(80,' DRAWS LINE 1
                  CALL MOVEA(XPNT,RYO)
                  CALL DASHA(TPREV(IH),RYPREV,38)
                ENDIF
              ENDIF
              IF (ITRK2.NE.0) THEN
                IF (ITRK2.EQ.ISPRE1(IH) .OR. ITRK2.EQ.ISPRE2(IH)) THEN
                  CALL MOVEA(XPNT,RYO)
                  CALL DASHA(TPREV(IH),RYPREV,38)
                ENDIF
              ENDIF
 4003       CONTINUE
C
            BUFFER(1,NHIT) = XPNT
            IF (ITRK1.GT.0) BUFFER(2,NHIT) = ITRK1
            IF (ITRK2.GT.0) BUFFER(3,NHIT) = ITRK2
            NHIT = NHIT + 1
          ENDIF
        ENDIF
        CALL MOVEA(XPNT - 12,RYO - 25)
        CALL CHRSIZ(4)
        CALL CSIZE(IXWID,IYWID)
        CALL ANCHO(IPOINT)
CX      WRITE (6,9040) IXWID,IYWID
 9040   FORMAT(' IXWID, IYWID :',2I8)
        IPHIT = IPHIT + 4
        GOTO 4000
 4001 CONTINUE
      IF (LTRACK) THEN
        DO 4004 IHIT = 1, 8
          TPREV(IHIT) = BUFFER(1,IHIT)
          ISPRE1(IHIT) = BUFFER(2,IHIT)
          ISPRE2(IHIT) = BUFFER(3,IHIT)
 4004   CONTINUE
      ENDIF
C
      IF( (.NOT. LSEP) .OR. KSIDE .NE. 1) GOTO 220
         KSIDE = 2
         RX1 = 2200.
         RX2 = 4000.
         GOTO 120
C
 220  ICALL = ICALL + 1
      RYPREV = RYO
      IF(KWR(17) .NE. ICALL) RETURN
      ICALL = 0
      IF (LTRACK) THEN
        DO 4005 IHIT = 1, 8
          TPREV(IHIT) = 0
          ISPRE1(IHIT) = 0
          ISPRE2(IHIT) = 0
 4005   CONTINUE
      ENDIF
C
      RETURN
      END
