C   07/06/96 606071849  MEMBER NAME  IBCOMR   (S4)          FORTRAN77
IBCR     TITLE 'SUBROUTINE TO READ OR WRITE A RECORD, PART AT A TIME, U+
               NDER FORTRAN CONTROL'
IBCOMR   CSECT
*
*    MAIN  ENTRY  POINT  FOR  STARTING  TO  READ  A  RECORD .
*    CALLING SEQUENCE  IS :
*
*              CALL  IBCOMR (STREAM NO.,&END,&ERR)
*
*    AUTHOR:- J.C.HART, RUTHERFORD LABORATORY
*
         ENTRY IBCOMW,IBWORD,IB2WD,IBARR,IBARR1,IBCOMF
         B     12(,15)             BRANCH ROUND NAME
         DC    X'7'
         DC    CL7'IBCOMR'
         STM   14,5,12(13)         SAVE REGISTERS
         USING IBCOMR,15
         LA    2,READ
         B     PROLOGUE
*
*    IBCOMW  IS  ENTRY  POINT  FOR  STARTING  TO  WRITE  A  RECORD .
*    CALLING  SEQUENCE  IS :
*
*              CALL  IBCOMW (STREAM NO.,&END,&ERROR)
*
IBCOMW   B     12(,15)             BRANCH ROUND NAME
         DC    X'7'
         DC    CL7'IBCOMW'
         STM   14,5,12(13)         SAVE REGISTERS
         USING IBCOMW,15
         LA    2,WRITE
         B     PROLOGUE
*
*    IBWORD  IS  ENTRY  POINT  FOR  READING  ONE  4-BYTE  WORD .
*    CALLING  SEQUENCE  IS :
*
*              CALL  IBWORD (WORD)
*
IBWORD   B     12(,15)             BRANCH ROUND NAME
         DC    X'7'
         DC    CL7'IBWORD'
         STM   14,5,12(13)         SAVE REGISTERS
         USING IBWORD,15
         LA    2,WORD
         B     PROLOGUE
*
*    IB2WD  IS  ENTRY  POINT  FOR  READING  TWO  4-BYTE  WORDS .
*    CALLING  SEQUENCE  IS :
*
*              CALL  IB2WD (WORD1,WORD2)
*
IB2WD    B     10(,15)             BRANCH ROUND NAME
         DC    X'5'
         DC    CL5'IB2WD'
         STM   14,5,12(13)         SAVE REGISTERS
         USING IB2WD,15
         LA    2,TWOWD
         B     PROLOGUE
*
*    IBARR  IS  ENTRY  POINT  FOR  READING  AN  ARRAY  OF  4-BYTE
*    WORDS .  CALLING  SEQUENCE  IS :
*
*              CALL  IBARR (ARRAY, NO. OF WORDS)
*
IBARR    B     10(,15)             BRANCH ROUND NAME
         DC    X'5'
         DC    CL5'IBARR'
         STM   14,5,12(13)         SAVE REGISTERS
         USING IBARR,15
         LA    2,ARRAY
         B     PROLOGUE
*
*     IBARR1 IS ENTRY POINT FOR READING AN ARRAY OF 1-BYTE WORDS.
*     CALLING SEQUENCE IS :
*
*               CALL IBARR1 (ARRAY,NO.OF BYTES)
*
IBARR1   B     12(,15)             BRANCH ROUND NAME
         DC    X'7',CL7'IBARR1'
         STM   14,5,12(13)         SAVE REGISTERS
         USING IBARR1,15
         LA    2,ARRAY1
         B     PROLOGUE
*
*    IBCOMF  IS  ENTRY  POINT  FOR  FINAL  CALL  TO  IBCOM .
*    CALLING SEQUENCE  IS :
*
*              CALL  IBCOMF
*
IBCOMF   B     12(,15)             BRANCH ROUND NAME
         DC    X'7'
         DC    CL7'IBCOMF'
         STM   14,5,12(13)         SAVE REGISTERS
         USING IBCOMF,15
         LA    2,FINAL
         B     PROLOGUE
*
*    PROLOGUE .
*
PROLOGUE BALR  5,0                 ESTABLISH BASE REGISTER
         USING *,5
         DROP  15
         LR    3,13                KEEP OLD SAVE AREA ADDRESS
         LA    13,SAVEAREA         LOAD NEW SAVE AREA ADDRESS
         ST    13,8(,3)            & STORE IT IN OLD SAVE AREA
         ST    3,4(,13)            STORE OLD SAVE AREA ADDRESS
         BR    2                   BRANCH TO APPROPRIATE SECTION
SAVEAREA DS    18F                 RESERVE SPACE FOR SAVE AREA
*
*   INITIALISE  IBCOM  FOR  UNFORMATTED  READ .
*
READ     L     3,0(,1)             FETCH ARGUMENT ADDRESS
         L     4,0(,3)             FETCH UNIT NO.
         ST    4,UNIT
         CNOP  0,4
         L     15,=V(IBCOM#)
         BAL   14,20(15)           BRANCH TO IBCOM
         DC    XL1'31',AL3(UNIT)
         DC    AL4(ADEND)
         DC    AL4(ADERR)
         B     RETURN
*
*   INITIALISE  IBCOM  FOR  UNFORMATTED  WRITE .
*
WRITE    L     3,0(,1)             FETCH ARGUMENT ADDRESS
         L     4,0(,3)             FETCH UNIT NO.
         ST    4,UNIT
         CNOP  0,4
         L     15,=V(IBCOM#)
         BAL   14,24(15)           BRANCH TO IBCOM
         DC    XL1'31',AL3(UNIT)
         DC    AL4(ADEND)
         DC    AL4(ADERR)
         B     RETURN
*
*    END  &  ERROR  EXITS .
*
END      LA    15,4                FOR RETURN 1
         B     RETURNI
ERR      LA    15,8                FOR RETURN 2
         B     RETURNI
*
*    READ  ONE  4-BYTE  WORD .
*
WORD     L     3,0(,1)             FETCH ARGUMENT ADDRESS
         CNOP  0,4
         L     15,=V(IBCOM#)
         BAL   14,28(15)           BRANCH TO IBCOM
         DC    XL4'04503000'
         B     RETURN
*
*    READ  TWO  4-BYTE  WORDS .
*
TWOWD    LM    3,4,0(1)            FETCH ARGUMENT ADDRESSES
         CNOP  0,4
         L     15,=V(IBCOM#)
         BAL   14,28(15)           BRANCH TO READ FIRST WORD
         DC    XL4'04503000'
         L     15,=V(IBCOM#)
         BAL   14,28(15)           BRANCH TO READ SECOND WORD
         DC    XL4'04504000'
         B     RETURN
*
*    READ  ARRAY  OF  4-BYTE  WORDS .
*
ARRAY    LM    3,4,0(1)            FETCH ARGUMENT ADDRESSES
         ST    3,ADDR              STORE ARRAY ADDRESS
         L     2,0(,4)             LOAD ARRAY LENGTH
         ST    2,LENGTH            & STORE IT
         MVI   LENGTH,X'04'        INSERT WORD LENGTH
         OI    LENGTH+1,X'70'      COPY H-COMPILER
         CNOP  0,4
         L     15,=V(IBCOM#)
         BAL   14,32(15)           BRANCH TO IBCOM
ADDR     DS    1F                  ADDRESS OF ARRAY
LENGTH   DS    1F                  LENGTH OF ARRAY
         B     RETURN
*
*     READ ARRAY OF 1-BYTE WORDS.
*
ARRAY1   LM    3,4,0(1)            FETCH ARGUMENT ADDRESSES
         ST    3,ADDR1             STORE ARRAY ADDRESS
         L     2,0(,4)             LOAD ARRAY LENGTH
         ST    2,LENGTH1           & STORE IT
         MVI   LENGTH1,X'01'       INSERT WORD LENGTH
         CNOP  0,4
         L     15,=V(IBCOM#)
         BAL   14,32(15)           BRANCH TO IBCOM
ADDR1    DS    1F                  ADDRESS OF ARRAY
LENGTH1  DS    1F                  LENGTH OF ARRAY
         B     RETURN
*
*    FINAL  CALL  TO  IBCOM  ( AT  END  OF  READ  OR  WRITE ) .
*
         CNOP  0,4
FINAL    L     15,=V(IBCOM#)
         BAL   14,36(15)           BRANCH TO IBCOM
*
*    RETURN  TO  CALLING  PROGRAM .
*
RETURN   SR    15,15               CLEAR REGISTER 15 FOR NORMAL RETURN
RETURNI  L     13,SAVEAREA+4       LOAD OLD SAVE ADDRESS AGAIN
         LM    2,5,28(13)          RESTORE REGISTERS
         L     14,12(,13)          LOAD RETURN ADDRESS
         MVI   12(13),X'FF'        FLAG RETURN
         BR    14                  & JUMP BACK
*
*    CONSTANTS .
*
ADEND    DC    AL4(END)
ADERR    DC    AL4(ERR)
UNIT     DS    1F
         END
