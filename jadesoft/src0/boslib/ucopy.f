C   14/10/82            MEMBER NAME  UCOPY    (S)           FORTRAN77
         MACRO
&NAME    ANFANG
&NAME    B     *+12
         DC    AL1(6)
         DC    CL6'&NAME'
         DS    CL1
         STM   2,9,28(13)
         LA    3,SA
         ST    3,8(13)
         ST    13,4(3)
         LR    13,3
         MEND
*
* CALL UCOPY(AVM,XVM,N)
* KEINE UEBERLAPPUNGEN ERLAUBT
*
         START
         ENTRY UCOPY2,VZERO,VBLANK,UZERO,UBLANK
         ENTRY VFILL,VADD,VSUB,VMUL,VDOT,UCOPY
         USING *,15
UCOPY    ANFANG
         LM    2,4,0(1)
         L     5,0(4)        ANZAHL WORTE
         SLL   5,2           LAENGE ZIELFELD
         LR    4,3           ADR. ZIELFELD
         LR    3,5           LAENGE QUELLFELD
         MVCL  4,2           XVM=AVM
         B     REST
*
* CALL UCOPY2(AVM,XVM,N)
* ALLE UEBERLAPPUNGEN ERLAUBT
*
         USING *,15
UCOPY2   ANFANG
         LM    2,4,0(1)
         CLR   2,3           QUELLADR.=ZIELADR.?
         BC    8,REST        WENN Q = Z,DANN FERTIG
         L     5,0(4)        ANZAHL WORTE
         SLL   5,2           LAENGE ZIELFELD
         LR    6,2
         SR    6,3           DIFF. QUELLE-ZIELADR.
         BC    2,NORMAL      DIFF. > 0 , VERZWEIGEN
         LPR   6,6           DIFF.ABS.
         CLR   6,5
         BC    11,NORMAL     DIFF. >= LAENGE ZIEL UND QUELLE, VERZW.
         L     7,=F'-4'
         AR    5,7           LAENGE ZIELFELD MINUS VIER BYTES
SCH1     L     4,0(5,2)      AVM INS R4
         ST    4,0(5,3)      R4 NACH XVM
         BXH   5,7,SCH1      NACH SCH1,WENN R5 GROESSER ALS R7
         B     REST
NORMAL   LR    4,3           ZIELADR.
         LR    3,5           LAENGE QUELLE
         MVCL  4,2           N*4 BYTES SCHIEBEN
         B     REST
*
* CALL VZERO(XVM,N)
*
         USING *,15
VZERO    ANFANG
         LM    2,3,0(1)
         LA    5,0
         L     4,0(3)
         LA    15,SCH2       SPRUNGADR. INS BASISREGISTER
         BR    15            NACH UBLANK
*
* CALL VBLANK(XVM,N)
*
         USING *,15
VBLANK    ANFANG
         LM    2,3,0(1)
         IC    5,=C' '
         L     4,0(3)
         LA    15,SCH2       SPRUNGADR. INS BASISREGISTER
         BR    15            NACH UBLANK
*
* CALL UZERO(XVM,JL,JR)
*
         USING *,15
UZERO    ANFANG
         LM    2,4,0(1)
         LA    5,0
         LA    15,BZ         SPRUNGADR. INS BASISREGISTER
         BR    15            NACH UBLANK
*
* CALL UBLANK(XVM,JL,JR)
*
         USING *,15
UBLANK   ANFANG
         LM    2,4,0(1)
         IC    5,=C' '
         LA    15,BZ
*
         USING *,15
BZ       L     3,0(3)        JL
         L     4,0(4)        JR
         BCTR  3,0           JL-1
         SR    4,3           JR-(JL-1)
         SLL   3,2
         AR    2,3           ANFADR. ZIELFELD
         LA    15,SCH2
*
         USING *,15
SCH2     SLL   4,2           LAENGE ZIELFELD
         LR    3,4
         LA    4,1(2)        DUMMYADR. QUELLE
         SLL   5,24          PADDING CHAR.  COUNT=0
         MVCL  2,4           KEIN MOVEN, NUR PADDING
         B     REST
*
* CALL VFILL(XVM,N,CM)
*
         USING *,15
VFILL    ANFANG
         LM    2,4,0(1)
         L     4,0(4)        CM
         L     3,0(3)        N
         L     5,=F'-4'
         SLL   3,2           N*4
         AR    3,5           N*4-4
SCH7     ST    4,0(3,2)
         BXH   3,5,SCH7
         B     REST
*
* CALL VADD(AV,BV,XV,N)
*
         USING *,15
VADD     ANFANG
         LM    2,5,0(1)
         L     9,0(5)        N
         SLL   9,2           N*4
         LA    8,4
         SR    9,8           N*4-4
         SR    7,7
SCH3     LE    6,0(7,2)      AV
         AE    6,0(7,3)      AV+BV
         STE   6,0(7,4)      XV=AV+BV
         BXLE  7,8,SCH3      NACH SCH3,WENN R7 KLEINER GLEICH R9
         B     REST
*
* CALL VSUB(AV,BV,XV,N)
*
         USING *,15
VSUB     ANFANG
         LM    2,5,0(1)
         L     9,0(5)        N
         SLL   9,2           N*4
         LA    8,4
         SR    9,8           N*4-4
         SR    7,7
SCH4     LE    6,0(7,2)      AV
         SE    6,0(7,3)      AV-BV
         STE   6,0(7,4)      XV=AV-BV
         BXLE  7,8,SCH4      NACH SCH4,WENN R7 KLEINER GLEICH R9
         B     REST
*
* CALL VMUL(AV,BV,XV,N)
*
         USING *,15
VMUL      ANFANG
         LM    2,5,0(1)
         L     9,0(5)        N
         SLL   9,2           N*4
         LA    8,4
         SR    9,8           N*4-4
         SR    7,7
SCH5     LE    6,0(7,2)      AV
         ME    6,0(7,3)      AV*BV
         STE   6,0(7,4)      XV=AV*BV
         BXLE  7,8,SCH5      NACH SCH5,WENN R7 KLEINER GLEICH R9
         B     REST
*
* CALL X=VDOT(AV,BV,N)
* X=AV(1)*BV(1)+AV(2)*BV(2)+...+AV(N)*BV(N)
*
         USING *,15
VDOT     ANFANG
         LM    2,4,0(1)
         L     7,0(4)        N
         SLL   7,2           N*4
         LA    6,4
         SR    7,6           N*4-4
         SER   0,0
         SR    5,5
SCH6     LE    4,0(5,2)      AV
         ME    4,0(5,3)      AV*BV
         AER   0,4           REGISTER 0 PLUS AV*BV
         BXLE  5,6,SCH6      NACH SCH6,WENN R5 KLEINER GLEICH R7
*
REST     L     13,4(13)      RESTAURIEREN
         LM    2,9,28(13)
         MVI   12(13),X'FF'
         BR    14
*
*     DATEN
*
SA       DS    18F
         END
