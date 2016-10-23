C   10/09/79 910212116  MEMBER NAME  ALPHAN   (JADEGS)      FORTRAN
      SUBROUTINE ALPHAN(LUN)
      IMPLICIT INTEGER*2(H),LOGICAL*1(Q)
#include "creply.for"
C-----------------------------------------------------------------------
C
C         ---------------- SUBROUTINE ALPHAN  ------------------
C         --------------- AUTHOR : G.F.PEARCE ----------------
C         ---------- LAST UPDATE : 1600HRS 10/09/79 ----------
C
C   SUBROUTINE TO READ A SINGLE LINE (72 CHARACTERS) FROM AN INPUT
C   FILE WRITTEN IN FREE FORMAT AND TO UNPACK THE CHARACTER STRINGS
C   INTO A SEQUENCE OF WORDS AND NUMBERS WHICH ARE THEN RETURNED
C   IN AN EASILY USEABLE FORM. THE ONLY REQUIREMENT IS THAT THE
C   CHARACTER STRINGS (WORDS AND NUMBERS) BE SEPERATED BY BLANKS OR
C   COMMAS. A WORD IS DEFINED HERE AS A SEQUENCE OF UP TO 4 UNINTERUPTED
C   ALPHABETIC CHARACTERS (DIGITS NOT ALLOWED). A NUMBER CAN BE EITHER
C   INTEGER OR DECIMAL (WITH + OR - SIGN). A MAXIMUM OF 20 REPLIES ARE
C   ALLOWED IN ANY ONE INPUT FIELD.
C   NOTE THAT TO AVOID PROBLEMS WHICH MAY ARISE WHEN NUMBERED TSO DATA
C   SETS ARE USED AS INPUT, CHARACTERS 73-80 IN THE INPUT FIELD ARE
C   ALWAYS IGNORED BY THIS ROUTINE.
C
C   LUN    = LOGICAL UNIT NUMBER FROM WHICH INPUT FIELD IS READ (EG 5)
C          = 100 TAKES AN IPS INPUT
C   NREPLY = NUMBER OF REPLIES (WORDS/NUMBERS) FOUND IN INPUT FIELD
C   REPLY  = REAL*4 ARRAY RETURNING ALPHABETIC WORDS AND/OR NUMBERS
C   QREPLY = LOGICAL ARRAY SHOWING TYPE OF RETURNED WORD
C            0 => ALPHABETIC   1 => REAL NUMBER   3 => INTEGER NUMBER
C
C-----------------------------------------------------------------------
C
C   CHARACTER CODES ASSUMED IN THIS ROUTINE ARE :
C                        A = 193
C                        B = 194
C                        " "  "
C                        " "  "
C                        Z = 233
C                        0 = 240
C                        1 = 241
C                        " "  "
C                        " "  "
C                        9 = 249
C                        ' = 107
C                        + = 78
C                        - = 96
C                        . = 75
C
C-----------------------------------------------------------------------
      REAL*4 BLANK4/4H    /
      LOGICAL*1 QZERO/Z00/,QONE/Z01/,QTHREE/Z03/
      LOGICAL*1 QWORD1(80),QB1(80),QTHIS(2)
      EQUIVALENCE (QWORD1(1),REPLY(1)) , (HCHAR,QTHIS(1))
C     ----------
C     INITIALISE
C     ----------
      NREPLY = 0
      DO 10 IPOSN=1,20
      QREPLY(IPOSN) = QZERO
  10  REPLY(IPOSN) = BLANK4
      IPOSN = 0
      HCHAR  = 0
      HSIGN  = 1
C     ----------
C     READ INPUT
C     ----------
      IF (LUN.EQ.100) CALL TRMIN(80,QB1)
      IF (LUN.NE.100) READ(LUN,20)QB1
  20  FORMAT(80A1)
C-------------------------
C     RESET LOCAL POINTERS
C-------------------------
  40  ILOCAL = 1
C-------------------------------------
C     PICK UP NEXT CHARACTER OF STRING
C-------------------------------------
  50  IPOSN = IPOSN + 1
C     DID WE HIT THE 72ND CHARACTER IN THE MIDDLE OF A WORD ?
      IF (IPOSN.LE.72) GOTO60
      IF (ILOCAL.GT.1) NREPLY = NREPLY + 1
      GOTO900
  60  QTHIS(2) = QB1(IPOSN)
C     IS IT AN ALPHABETIC CHARACTER ?
      IF (HCHAR.LT.193.OR.HCHAR.GT.233) GOTO80
C----------------------------------
C     LOAD NEW ALPHABETIC CHARACTER
C     INTO CURRENT WORD
C----------------------------------
      IF (NREPLY.GT.20.OR.ILOCAL.GT.4) GOTO50
      QWORD1(ILOCAL+NREPLY*4) = QTHIS(2)
      ILOCAL = ILOCAL + 1
      GOTO50
C----------------------------------
C     TEST FOR END OF CURRENT WORD
C----------------------------------
  80  IF (ILOCAL.EQ.1) GOTO90
      ILOCAL = 1
      NREPLY = NREPLY + 1
C-------------------------------
C     POSSIBLE START OF A NUMBER
C-------------------------------
  90  DIVIDE = 0.100
      IDEC = 1
      X = 0.0
      NDIGIT = 0
C     TEST FOR +/- SIGN , NUMERAL OR DECIMAL POINT
      IF (HCHAR.EQ.78) GOTO220
      IF (HCHAR.NE.96) GOTO300
C     FOUND MINUS SIGN
      HSIGN = 0
  220 IPOSN = IPOSN + 1
      IF (IPOSN.GT.72) GOTO900
      QTHIS(2) = QB1(IPOSN)
      GOTO300
C     NUMERAL .. START OF A NUMBER
  300 IF (HCHAR.NE.75) GOTO320
      IDEC = 2
      GOTO220
  320 IF (HCHAR.LT.240.OR.HCHAR.GT.249) GOTO380
  330 X1 = FLOAT(HCHAR-240)
      NDIGIT = NDIGIT + 1
      GOTO (340,350),IDEC
  340 X = X*10.0 + X1
      GOTO220
  350 X = X + X1*DIVIDE
      DIVIDE = DIVIDE/10.0
      GOTO220
C--------------------------
C     LOAD COMPLETED NUMBER
C--------------------------
  380 IF (NDIGIT.EQ.0) GOTO40
      NREPLY = NREPLY + 1
      IF (HSIGN.EQ.0) X = -X
      HSIGN = 1
      IF (NREPLY.GT.20) GOTO60
      IF (IDEC.EQ.1) GOTO385
      REPLY(NREPLY) = X
      QREPLY(NREPLY) = QONE
      GOTO60
 385  IREPLY(NREPLY) = X
      QREPLY(NREPLY) = QTHREE
      GOTO60
C----------------------
C     ALL OVER - RETURN
C----------------------
 900  CONTINUE
      RETURN
      END
