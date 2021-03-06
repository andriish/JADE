C   14/10/82 606071832  MEMBER NAME  BSCOMT   (S4)          FORTRAN77
      SUBROUTINE BSCOMT
C
C                      BBBB    OOO    SSS
C                      B   B  O   O  S
C                      B   B  O   O  S
C                      BBBB   O   O   SSS
C                      B   B  O   O      S
C                      B   B  O   O      S
C                      BBBB    OOO   SSSS
C
C
C
C
C     -----------------------------------------------------
C     BANK ORGANISATION SYSTEM                      - BOS -
C     -----------------------------------------------------
C           DYNAMIC STORAGE ORGANISATION WITH FORTRAN
C
C
C
C
C
C     AUTHOR  -  V.BLOBEL
C                II.INSTITUTE F. EXP.PHYSIK
C                UNIVERSITY OF HAMBURG   AND   DESY
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C         TABLE OF CONTENT                      ENTRY NAMES
C     -----------------------------------------------------
C
C
C
C     1.  INTRODUCTION
C                                               BINT
C     2.  CREATION OF BANKS
C                                               BCRE   BCHL
C                                               BSTR   BMVE
C     3.  LOCATING BANKS
C                                               BLOC   BPOS
C                                               BNXT   BDAR
C     4.  INPUT FROM CARDS
C                                               BREADC
C     5.  DEFINING SETS OF BANKS
C                                               BMLT
C     6.  PRINTING OF BANKS
C                                               BPRS   BMLT
C     7.  DELETING BANKS
C                                               BDLS   BDLM
C     8.  GARBAGE COLLECTION
C                                               BGAR   BGAC
C     9.  UNFORMATED INPUT/OUTPUT OF BANKS
C                                               BWRITE BREAD
C     10. SPECIAL SET OF BANKS
C                                               BSAT   BSAW
C                                               BSLT   BSLW
C     11. PRINTING STATISTIC AND A DUMP
C                                               BSTA   BDMP
C     12. OPTIMIZATION
C                                               BDEF   IBLN
C     13. APPLICATION PROGRAMS
C                                               UCOND  PCOND
C                                               QCOND
C                                               UTABL  PTABL
C                                               QTABL
C                                               UHIST  PHIST
C                                               QHIST  DHIST
C                                               UCORR  PCORR
C                                               USTOR  USTOS
C                                               PSTOR  DSTOR
C                                               TCOND  TTABL
C                                               THIST  TCORR
C                                               TSTOR  TTEXT
C                                               PTEXT
C
C
C
C
C
C
C
C
C
C     -----------------------------------------------------------------100011100
C     1.  INTRODUCTION
C
C     LARGE AMOUNT OF DATA ARE TO BE ANALYSED IN HIGH-ENERGY
C     PHYSICS EXPERIMENTS. DATA ON ONE EVENT CONSIST OF DATA
C     FROM SEVERAL SOURCES, E.G. TRACK CHAMBER, SHOWER COUNTER
C     AND SCALER. DURING THE ANALYSIS NEW DATA MAY BE ADDED
C     BY COMPUTATION, E.G. DATA ON SINGLE TRACKS. ALSO DATA
C     MAY BE COMPRESSED BY DATA REDUCTION PROGRAMS.
C     THE COMPLEXITY OF ANALYSIS DEPENDS VERY MUCH ON THE
C     ORGANISATION OF THE DATA IN STORAGE AND ON MASS STORAGE
C     DEVICES, E.G. TAPES. A FLEXIBLE DATA ORGANISATION
C     SIMPLIFIES THE ANALYSIS.
C     THE SYSTEM DESCRIBED HERE IS DESIGNED ACCORDING TO THE
C     NEEDS OF HIGH-ENERGY PHYSICS EXPERIMENTS. ALL DATA ARE
C     STRUCTURED IN SO CALLED BANKS OF VARIABLE LENGTH. DATA
C     IN ONE BANK REFER E.G. TO A SINGLE PARTICLE TRACK. A
C     BANK HAS TWO IDENTIFIERS NA AND NR, CALLED NAME AND
C     NUMBER. NORMALLY THE IDENTIFIER NA CONSISTS OF UP TO
C     FOUR CHARACTERS (LEFT ADJUSTED), WHILE IDENTIFIER NR
C     IS AN INTEGER.
C     ALL BANKS ARE STORED IN A SINGLE COMMON/BCS/IW(NSPACE).
C     AN INDEX IND IS ASSIGNED TO EACH BANK, WHERE IW(IND)=NW
C     CONTAINS THE NUMBER OF DATA WORDS IN THAT BANK. DATA
C     ARE STORED IN IW(IND+1) TO IW(IND+NW). IN ADDITION
C     THERE ARE THREE WORDS CONTAINING NAME, NUMBER AND A
C     POINTER ACCORDING TO THE FOLLOWING SCHEMA.
C
C         I      IW(I)
C     --------   -----
C     IND -  3   NA (NAME)
C     IND -  2   NR (NUMBER)
C     IND -  1   POINTER TO NEXT BANK OF SAME NAME
C     IND        NW = NUMBER OF DATA WORDS IN THE BANK
C     IND +  1   1.DATA WORD
C     IND +  2   2.DATA WORD
C     ...        ...
C     IND + NW   LAST DATA WORD
C
C     NO LIMITATIONS EXIST FOR THE LENGTH OF A SINGLE BANK OR
C     THE NUMBER OF BANKS, EXCEPT FOR THE TOTAL LENGTH OF
C     ALL BANKS.
C     THE NUMBER OF DIFFERENT NAMES MAY NEVER EXCEED 200.
C     OUTPUT OF SETS OF BANKS IS SIMPLY DONE BY SPECIFYING
C     ALL NAMES OF BANKS, BELONGING TO THE OUTPUT RECORD.
C     BANK RECORDS MAY CONTAIN MANY BANKS, BANK AFTER BANK WITHOUT GAPS.
C     THE MAIN ADVANTAGES OF THE SYSTEM ARE -
C             - EFFICIENT USE OF STORAGE
C             - EASY ADDITION OF NEW BANKS
C             - FLEXIBLE INPUT/OUTPUT
C             - PRINTOUT OF BANK STRUCTURE FOR TEST PURPOSES
C
C
C     LIBRARIES. THE SUBROUTINES OF THE BANK-SYSTEM ARE ON
C     THE DESY-NEWLIB LIBRARIES
C              DSN=F1EBLO.DAHEP   (LOAD)
C         AND  DSN=F1EBLO.DAHEPS  (SOURCE)
C     A ORDERED LIST OF ALL SUBROUTINES FOLLOWS.
C
C     STANDARD SUBROUTINES
C
C     BINT    BFMT
C     BCRE    BDAR    BDEF    BDLM     BDLM    BDMP    BGAR
C     BLOC    BPOS    BPRM    BPUT     BREADC  BSAC    BWRITE
C     FSWRIT  IBLN    MOVERX  READFF   UCOPY2  UWP
C
C     ALTERNATIVE SUBROUTINES
C
C     CCRE    CLOC    CREAD   CPOS     CCHL
C
C     SUBROUTINES FROM DESYLIB
C
C     ITRACE  NTIME   NOARG
C
C     APPLICATION SUBROUTINES
C
C     CCOR    DEFST   IAF     ITODA   PCOND   PCORR   PHIST
C     PTABL   PSTOR   PVERT   SORT4   UCOND   UHIST   UTABL
C     USTOS   VALLPTEXT
C
C
C     COMPATIBILITY WITH NON-IBM FORTRAN. FOR ALL SUBROUTINES
C     WITH NUMBERED RETURN STATEMENTS (RETURN 1, RETURN 2,
C     STATEMENTNRS AS ARGUMENTS) THERE ARE ALTERNATIVES WITH
C     FIRST LETTER C, WHICH AVOID THIS POSSIBILITY OF IBM-FORTRAN.
C
C     INITIALIZATION IS DONE BY THE FOLLOWING STATEMENTS.
C
C     COMMON/BCS/IW(NSPACE)
C     REAL RW(NSPACE)
C     EQUIVALENCE (IW(1),RW(1))
C        WHERE NSPACE = INTEGER CONSTANT
C
C               ------ ---- ---- ----
C     CALL BINT(NSPACE,NREC,NDMP,NADD)
C
C
C        WHERE NREC  = MAX.NO OF WORDS IN A RECORD (1000)
C              NDMP  = NR OF WORDS, PRINTED IN A DUMP (500)
C              NADD  = NR OF WORDS FOR LOW PRIORITY BANKS(0)
C
C
C     NOTE. HERE AND IN THE FOLLOWING INPUT ARGUMENTS HAVE
C     A LINE ABOVE, OUTPUT ARGUMENTS HAVE A LINE BELOW.
C     ARGUMENTS, WHICH ARE CHANGED BY THE PROGRAM, HAVE A LINE
C     ABOVE AND BELOW.
C
C     IF ILLEGAL POINTER ARE DETECTED BY THE BANK PROGRAM,
C     A DUMP CONTAINING RELEVANT STATUS INFORMATION AND
C     NDMP WORDS OF COMMON/BCS/ IS PRINTED.
C     NAMES WITH A CHARACTER '*' AS THE FOURTH CHARACTER
C     (E.G. HST*) ARE CONSIDERED AS SPECIAL. THE TOTAL
C     SPACE ATTRIBUTED TO THEM NEVER EXCEEDS NADD WORDS.
C
C     -----------------------------------------------------------------200022500
C     2.  CREATION OF BANKS
C
C     2.1 CREATE A BANK
C
C                   -- -- --
C     CALL BCRE(IND,NA,NR,NW,&S1,IER)
C               ---          --- ---
C
C        WHERE NA = NAME OF THE BANK
C              NR = NUMBER OF THE BANK
C              NW = NUMBER OF WORDS OF THE BANK (NW.GE.0)
C        IND = INDEX OF BANK (IER=1, IF BANK ALREADY EXISTING)
C        ALL WORDS OF THE BANK ARE SET = 0
C        RETURN 1 (IER=2) IF NOT ENOUGH SPACE
C
C     ALTERNATIVE
C     CALL CCRE(IND,NA,NR,NW,IER)
C        TEST IER=2 EQUIVALENT TO RETURN 1
C
C     2.2 CHANGE LENGTH OF A BANK
C
C
C               --  --
C     CALL BCHM(IND,NW,IER)
C                      ---
C
C        WHERE NW = CHANGE IN LENGTH OF THE BANK (NW.LT.0 VALID)
C        IND= INDEX OF THE BANK TO BE CHANGED IN LENGTH
C
C     IF THE CHANGE NW IS GREATER THAN -5, THE BANK IS MOVED TO THE
C     END (IF THE BANK IS NOT ALREADY THERE). FOR NW=0, THE BANK
C     IS MOVED TO THE END.
C
C
C     A BANK LENGTH OF AT LEAST NW WORDS IS OBTAINED BY THE FOLLOWING
C     CALL.
C               --- --
C     CALL BCHF(IND,NW,IER)
C                      ---
C        WHERE NW = MINIMUM LENGTH OF THE BANK
C        IND = INDEX OF THE BANK
C
C     THE BANK LENGTH IS CHANGED, IF THE ACTUAL LENGTH IS LESS
C     THAN NW.
C
C        IER = 1 WRONG INDEX IND IN ARGUMENT, OR BANK LENGTH WILL
C                BECOME  NEGATIVE
C        IER = 2 NOT ENOUGH SPACE
C
C
C
C     2.2 CHANGE LENGTH OF THE LAST CREATED BANK
C
C
C               --
C     CALL BCHL(NW,&S1)
C                  ---
C
C        WHERE NW = CHANGE IN LENGTH OF THE BANK (NW.LT.0 VALID)
C
C        RETURN 1, IF NOT ENOUGH SPACE
C
C      ALTERNATIVE
C      CALL CCHL(NW,IER)
C         TEST IER=2 EQUIVALENT TO RETURN 1
C
C     NOTE, THAT ONLY THE LENGTH OF THE LAST CREATED BANK CAN
C     BE CHANGED DIRECTLY (SEE CHAPTER 2.4).
C
C
C     2.3 STORE DATA IN A BANK
C
C               --- -- --
C     CALL BSTR(IND,JW,NW)
C
C
C     WHERE IND = INDEX
C           JW  = ARRAY TO BE STORED
C           NW  = LENGTH OF JW
C     JW(1) IS STORED IN IW(IND+1) ETC
C
C     2.4 MOVING A BANK
C
C
C               ---
C     CALL BMVE(IND,&S1)
C               --- ---
C        WHERE IND = INDEX OF BANK TO BE MOVED
C     THE BANK IS MOVED TO THE END OF THE USED STORAGE, IND
C     IS CHANGED. AFTER BMVE THE LENGTH OF THE BANK CAN BE CHANGED
C     BY BCHL.
C        RETURN 1, IF NOT ENOUGH SPACE
C
C     ALTERNATIVE
C     CALL CMVE(IND,IER)
C        TEST IER=2 EQUIVALENT TO RETURN 1
C
C     -----------------------------------------------------------------300029100
C     3.  LOCATING BANKS
C
C     3.1 LOCATE A BANK
C
C
C                   -- --
C     CALL BLOC(IND,NA,NR,&S1)
C               ---       ---
C
C        WHERE NA = NAME OF THE BANK
C              NR = NUMBER OF THE BANK
C        IND = INDEX OF THE BANK (NORMAL RETURN)
C        RETURN 1, IF BANK NOT EXISTING (IND=0)
C
C     ALTERNATIVE
C     CALL CLOC(IND,NA,NR)
C        TEST IND=0 EQUIVALENT TO RETURN 1
C
C     3.2 LOCATING ALL BANKS OF A GIVEN NAME
C

C     LOCATING OF ALL BANKS OF A GIVEN NAME IN ASCENDING ORDER
C     OF NR IS DONE IN THE FOLLOWING WAY.
C
C               ----
C     CALL BPOS(NAME)
C
C        WHERE NAME = NAME OF THE BANKS
C
C               ---
C     CALL BNXT(IND,&S1)
C               --- ---
C
C        IND = INDEX OF NEXT BANK OF THE SAME NAME
C        RETURN 1 AFTER LAST BANK
C
C     ALTERNATIVE
C     CALL CPOS(NAME)
C     CALL CNXT(IND)
C        TEST IND=0 EQUIVALENT TO RETURN 1
C
C     EXAMPLE FOR A LOOP ON ALL BANKS OF THE SAME NAME
C        CALL BPOS(NAME)
C     10 CALL BNXT(IND,&20)
C        . . .
C        GOTO 10
C     20 CONTINUE
C
C     BPOS/BNXT-LOOPS MAY BE NESTED. BNXT MAY ALSO BE USED
C     WITHOUT BPOS TO OBTAIN THE INDEX OF THE NEXT BANK OF
C     THE SAME NAME. THE INDEX IND IS A INPUT- AND OUTPUT-
C     ARGUMENT IN BNXT, EXCEPT FOR THE FIRST CALL AFTER THE
C     CALL OF BPOS, WHERE IT IS AN OUTPUT-ARGUMENT.
C     SEE ALSO REMARKS IN CHAPTER 12.
C
C
C     3.3 ALTERNATIVE TO BPOS/BNXT
C
C     LOCATING ALL BANKS OF A GIVEN NAME CAN ALSO BE DONE WITH BDAR.
C
C
C                ----        ----
C     CALL  BDAR(NAME,N,INDA,NLIM)
C                     - ----
C
C        WHERE NAME = NAME OF THE BANKS
C              NLIM = NR.OF WORDS OF ARRAY INDA( )
C        INDA( ) = ARRAY CONTAING N INDICES (N.LE.NLIM)
C
C     EXAMPLE, EQUIVALENT TO THE ONE IN 3.2
C        INTEGER INDA(20)
C        CALL BDAR(NAME,N,INDA,20)
C        DO 20 I=1,N
C        IND=INDA(I)
C        . . .
C     20 CONTINUE
C
C     -----------------------------------------------------------------400036900
C     4.  INPUT FROM CARDS
C
C     DATA CAN BE READ FROM CARDS IN FREE FORMAT OR IN A FORMAT GIVEN
C     ON THE CARD ITSELF, AND STORED IN BANKS. THE LAST 8 COLS OF THE
C     CARDS ARE NOT USED IN FREE FORMAT MODE.
C
C     CALL BREADC
C
C     DATA ARE READ FROM CARDS (UNIT 5) UNTIL DATA END OR A CARD WITH
C     ENDQ IS READ.
C
C     4.1 FREE FORMAT
C
C     READING A CARD OF THE TYPE
C     NAME A B C . . .
C     CREATES A BANK WITH NA = NAME, NR = 0. A SECOND CARD WITH THE
C     SAME NAME CREATES A BANK WITH NA = NAME, NR = 1 ETC.
C
C     $NAM N A B C . . .
C     CREATES A BANK WITH NA = $NAM, NR = N. A SECOND CARD WITH THE
C     SAME $NAM AND N IS NOT STORED.
C
C     THE BANK NAME NA CONSISTS OF THE FIRST FOUR CHARACTERS OF
C     NAME OR $NAM, RESP. THE FIRST
C     CHARACTER OF $NAM IS THE DOLLAR-SIGN. N, A, B, C, . . .
C     ARE CONSTANTS, INTEGER CONSTANTS WRITTEN WITHOUT A DOT (.),
C     REAL CONSTANTS WITH DOT (.) AND OPTIONALLY WITH EXPONENT.
C     CONTINUATION CARDS STARTING IMMEDIATELY WITH CONSTANTS ARE
C     ALLOWED. NON-NUMERIC CHARACTERS BETWEEN CONSTANTS ARE
C     IGNORED.
C
C     4.2 FORMATED
C
C     AFTER READING A CARD
C     *NAM N NW 'FORMAT
C     NW WORDS ARE READ ACCORDING TO THE GIVEN FORMAT FROM CONSECUTIVE
C     CARDS AND STORED IN A BANK WITH NA = *NAM, NR = N. THE FORMAT
C     SHOULD START WITH 1X (SEE EXAMPLE), TO ALLOW ECHO-PRINTOUT
C     WITHOUT LINE SKIPPING.
C
C     4.3 SPECIAL CARDS
C
C     ENDQ
C     BREADC RETURNS.
C
C     POFF
C     PRINTING OF CARDS IS SWITCHED OFF. IT MAY BE SWITCHED ON AGAIN
C     BY THE CARD
C     PON
C
C     UNIT N
C     THE NEXT CARDS ARE READ FROM UNIT N UNTIL DATA END, THEN AGAIN
C     FROM UNIT 5.
C
C     4.4 EXAMPLE FOR CARDS AND BANKS STORED
C
C     CONSTANTS 1.7 2.3 7 1.23E-4
C     $RUN 2769 1.520 16.0
C     0.7 2.0
C     *TIT 2000 5 '(1X,5A4)
C      PI+ PI+ PI- PI- PI0
C     ENDQ
C
C     BANKS STORED
C
C     NAME             CONS   $RUN   *TIT
C     NUMBER              0   2769   2000
C     NR.OF WORDS         4      4      5
C
C     1.DATA WORD       1.7  1.520   PI+
C     2.DATA WORD       2.3   16.0   PI+
C     3.DATA WORD         7    0.7   PI-
C     4.DATA WORD   1.23E-4    2.0   PI-
C     5.DATA WORD                    PI0
C
C     INDICES ARE OBTAINED BY
C     CALL BLOC(IND,'CONS',0,&S1)
C     CALL BLOC(IND,'$RUN',2769,&S1)
C     CALL BLOC(IND,'*TIT',2000,&S1)   RESP.
C
C     -----------------------------------------------------------------500045000
C     5.  DEFINING SETS OF BANKS
C
C     DEFINITION. THE SET B(LIST) IS THE SET OF ALL BANKS WITH ALL
C     NAMES, APPEARING IN THE ARRAY LIST.
C
C     A SET  B(LIST) IS DEFINED BY THE FOLLOWING STATEMENT.
C
C               - ----
C     CALL BMLT(N,LIST)
C
C
C        WHERE LIST = ARRAY OF N NAMES
C
C     EXAMPLE
C
C     CALL BMLT(2,'HEADGEOM')
C
C     -----------------------------------------------------------------600046800
C     6.  PRINTING OF BANKS
C
C     6.1 PRINT A SINGLE BANK
C
C               -- --
C     CALL BPRS(NA,NR)
C
C
C        WHERE NA = NAME
C              NR = NUMBER OF BANK TO BE PRINTED
C
C     6.2 PRINT A SET OF BANKS
C
C     CALL BPRM
C
C     ALL BANKS OF THE PREVIOUS DEFINED SET B(LIST) ARE PRINTED.
C
C     THE PRINTING FORMAT OF EACH WORD IS CHOSEN AUTOMATICALLY
C     (TEXT, INTEGER, REAL), NOT ALWAYS CORRECT.
C
C     -----------------------------------------------------------------700048900
C     7.  DELETING BANKS
C
C     7.1 DELETE A SINGLE BANK
C
C
C               -- --
C     CALL BDLS(NA,NR)
C
C
C        WHERE NA = NAME
C              NR = NUMBER OF BANK TO BE DELETED
C
C     7.2 DELETE A SET OF BANKS
C
C     CALL BDLM
C
C     ALL BANKS OF THE PREVIOUS DEFINED SET B(LIST) ARE DELETED.
C
C     A DELETED BANK IS NO LONGER ACCESSIBLE BY A CALL BLOC, ALTHOUGH
C     IT IS STILL IN STORAGE UNTIL THE NEXT GARBAGE COLLECTION.
C
C     -----------------------------------------------------------------800051100
C     8.  GARBAGE COLLECTION
C
C     IN A GARBAGE COLLECTION THE ACTIVE (NOT DELETED) BANKS ARE
C     SHIFTED IN STORAGE, IF THERE ARE BANKS DELETED.
C
C
C
C     CALL BGAR(IGA)
C               ---
C
C        IGA = 0 NO GARBAGE COLLECTION DONE (IF NO BANKS WERE DELETED)
C        IGA = 1 GARBAGE COLLECTION DONE
C
C
C                   --
C     CALL BGAC(IGA,NW)
C               ---
C
C        WHERE NW = NR.OF WORDS OF A NEW BANK
C     NO GARBAGE COLLECTION IS DONE, IF THERE IS SPACE ENOUGH
C     FOR AN ADDITIONAL BANK WITH NW WORDS
C
C     WARNING. BECAUSE IN A GARBAGE COLLECTION, SOME BANKS (ALL BANKS
C     BEHIND DELETED BANKS) ARE SHIFTED, THE INDICES OF THE BANKS
C     CHANGE. AUTOMATIC GARBAGE COLLECTIONS ARE DONE IN THE INPUT-
C     OUTPUT ROUTINES (SEE 9.)
C
C     -----------------------------------------------------------------900053900
C     9.  UNFORMATED INPUT/OUTPUT OF BANKS
C
C     9.1 WRITING SETS OF BANKS
C
C
C
C                 ---
C     CALL BWRITE(IUN)
C
C     ALL BANKS OF THE PREVIOUS DEFINED SET  B(LIST) ARE WRITTEN ON
C     UNIT IUN. BECAUSE THE BANKS HAVE TO BE STORED CONSEC. IN
C     STORAGE BEFORE WRITING, THEY HAVE TO BE SHIFTED IN STORAGE.
C     IF NECESSARY, A GARBAGE COLLECTION IS DONE.
C     THE BANKS ARE NOT WRITTEN, IF THE TOTAL RECORD LENGTH IS LARGER
C     THAN NREC (SEE INTRODUCTION).
C
C     THE ORDER OF THE BANKS IN THE RECORD IS THE SAME AS WITHIN LIST,
C     BANKS WITH THE SAME NAME IN ASCENDING ORDER OF NUMBER.
C     THE RECORD CONSISTS OF N + 1 WORDS, WHERE
C     FIRST WORD IN RECORD   = N
C     NEXT WORD              = NAME
C     NEXT WORD              = NUMBER OF FIRST BANK
C     NEXT WORD              = POINTER
C     NEXT WORD              = NR.OF WORDS OF THE FIRST BANK
C     ETC.
C
C
C     9.2 READING
C
C                ---
C     CALL BREAD(IUN,&S1,&S2)
C                    --- ---
C
C     A RECORD IS READ FROM UNIT IUN (&S1 ERROR EXIT, &S2 END EXIT).
C     THE BANKS CONTAINED IN THE RECORD ARE STORED. IF BANKS WITH THE
C     SAME IDENTIFIERS ARE ALREADY EXISTING, THEY ARE DELETED.
C     A GARBAGE COLLECTION IS DONE AUTOMATICALLY.
C     IF THERE IS NOT ENOUGH SPACE FOR A RECORD OF NREC WORDS (SEE
C     INTRODUCTION), THE PROGRAM STOPS.
C
C     ALTERNATIVE
C     CALL CREAD(IUN,IER)
C        TEST IER=1 EQUIVALENT TO RETURN 1
C        TEST IER=2 EQUIVALENT TO RETURN 2
C
C
C     9.3 ALTERNATIVE FOR BWRITE/BREAD
C
C     INPUT-OUTPUT IS ALSO POSSIBLE WITH USER-WRITTEN SUBROUTINES IN
C     USER-CONTROLLED RECORDS.
C
C
C
C     CALL BOUTP(N,INIW)
C                - ----
C
C        THE N WORDS IW(INIW) TO IW(INIW+N-1) CONTAIN ALL BANKS
C        OF THE PREVIOUS DEFINED SET B(LIST).
C        N=0, IF RECORD TOO LARGE.
C
C
C               - --
C     CALL BINP(N,AR)
C
C
C        WHERE AR( ) = ARRAY OF N WORDS, CONTAINING BANKS.
C        THE BANKS CONTAINED IN THE ARRAY ARE STORED.
C
C     THE FOLLOWING STATEMENTS ARE EQUIVALENT
C
C     OUTPUT      CALL BWRITE(IUN)
C
C     ALTERNATIVE CALL BOUTP(N,INIW)
C                 IF(N.NE.0) WRITE(IUN) N,(IW(INIW+I-1),I=1,N)
C
C     INPUT       CALL BREAD(IUN,&S1,&S2)
C
C     ALTERNATIVE READ(IUN,ERR=&S1,END=&S2) N,(AR(I),I=1,N)
C                 CALL BINP(N,AR)
C
C     ----------------------------------------------------------------1000062000
C     10. SPECIAL SET OF BANKS
C
C     FOR THE USE IN AN EVENT-BY-EVENT PROCESSING A SPECIAL LIST
C     OF BANK NAMES IS SUPPORTED BY THE SYSTEM. THIS LIST CONTAINS
C     ALL NAMES OF BANKS BELONGING TO THE CURRENT EVENT. EACH
C     NAME HAS ASSOCIATED A MARKER, WITH A VALUE =1, IF BANKS
C     WITH THIS NAME SHOULD BE WRITTEN IN THE OUTPUT RECORD.
C     AFTER INPUT (BREAD/BINP) THE SPECIAL LIST CONSISTS OF
C     ALL NAMES OF BANKS JUST READ IN, WITH MARKER =1.
C     THE LIST CAN BE UPDATED DURING EVENT PROCESSING.
C
C     10.1 UPDATING THE SPECIAL LIST
C
C
C                - ----
C      CALL BSAT(N,LIST)
C
C                - ----
C      CALL BSAW(N,LIST)
C
C
C         WHERE LIST( ) = ARRAY OF N NAMES
C
C     IN BOTH CASES THE NAMES ARE ADDED TO THE SPECIAL LIST
C     (IF NOT ALREADY PRESENT), AND THE MARKER IS SET
C     =0 FOR BSAT AND =1 FOR BSAW.
C
C
C
C     10.2 DEFINING THE SET OF BANKS
C
C     CALL BSLT
C
C     CALL BSLW
C
C     THE SET B(LIST) IS DEFINED AS THE SET OF ALL NAMES OF THE
C     SPECIAL LIST FOR BSLT, AND AS THE SET OF ALL NAMES OF THE
C     SPECIAL LIST WITH MARKER =1 FOR BSLW.
C
C
C
C     10.3 EXAMPLE
C
C     THIS EXAMPLE EXPLAINS THE TYPICAL INPUT/OUTPUT -
C     PROCEDURE IN AN EVENT-BY-EVENT PROCESSING. ONE EVENT IS
C     READ IN BY BREAD. BY COMPUTATION NEW BANKS ARE CREATED.
C     SOME OF THESES BANKS SHOULD BE WRITTEN , BUT ALL BANKS
C     HAVE TO BE DELETED AFTER PROCESSING THE EVENT.
C
C
C     C     INPUT
C        10 CALL BREAD(IN-UNIT,&30,&40)
C     C
C     C     COMPUTATION
C           . . .
C           . . .
C     C
C     C      BANKS, WHICH ARE CREATED AND WHICH SHOULD NOT BE WRITTEN
C           CALL BSAT(NT,NAMT)
C
C     C      BANKS, WHICH ARE CREATED AND WHICH SHOULLD BE WRITTEN
C           CALL BSAW(NW,NAMW)
C
C     C     DECIDE ON OUTPUT FOR THIS EVENT
C           IF(.NOT.OUTPUT) GOTO 20
C
C     C
C     C     OUTPUT
C           CALL BSLW
C           CALL BWRITE(OUT-UNIT)
C     C
C     C     DELETE ALL EVENT-BANKS
C        20 CALL BSLT
C           CALL BDLM
C           GOTO 10
C     C     READ ERROR
C        30 WRITE(. . .)
C           GOTO 10
C     C     END OF DS
C        40 WRITE(. . .)
C           . . .
C
C
C     ----------------------------------------------------------------1100070400
C
C     11.   PRINTING STATISTIC AND A DUMP
C
C     A FINAL STATISTIC ON THE SYSTEM PERFORMANCE IS PRINTED BY
C
C     CALL BSTA
C
C
C
C     A DUMP CONTAINING RELEVANT STATUS INFORMATION AND NDMP
C     WORDS OF THE COMMON/BCS/IW(NSPACE) IS PRINTED BY
C
C     CALL BDMP
C
C     AND THE PROGRAM IS STOPPED.
C
C     ----------------------------------------------------------------1200072100
C
C     12.   OPTIMIZATION
C
C
C     LOCATING BANKS BY CALLS TO BLOC AND BPOS/BNXT IS DONE BY
C     FAST ALGORITHMS, BUT OF COURSE SOME TIME IS SPENT IN
C     THESE SUBROUTINES. HOWEVER, CALLS TO BLOC AND BPOS/BNXT
C     CAN BE REPLACED BY SOME FORTRAN-STATEMENTS, AVOIDING
C     THE SAVING AND RESTORING OF REGISTERS ETC IN THE
C     CALL/RETURN PROCEDURE.
C     THE METHOD USED FOR THE POINTERS TO BANKS IS AS FOLLOWS.
C     AT ANY TIME THE FIRST 200 WORDS OF THE COMMON /BCS/
C     CONTAIN THE INDICES FOR THE FIRST BANK OF EACH NAME.
C     THE ASSIGNMENT OF A NAME TO A LOCATION IS NEVER CHANGED
C     DURING PROGRAM EXECUTION. NORMALLY THE ASSIGNMENT IS DONE
C     IN THE ORDER OF APPEARENCE OF NAMES, BUT THE ASSIGNMENT
C     CAN ALSO BE FIXED AFTER INITIALIZATION OF THE SYSTEM
C     (AFTER CALL BINT) BY THE CALL
C                  - ----
C        CALL BDEF(N,LIST)
C           WHERE LIST( ) = ARRAY OF N NAMES
C
C     THE FIRST NAME OF THE ARRAY (LIST(1)) IS ASSIGNED TO
C     IW(1), THE SECOND TO IW(2) ETC.
C
C     EXAMPLE
C        CALL BDEF(3,'HEADTITLDATA')
C        CALL BDEF(2,'GEOMSHOW')
C
C     ASSUME THAT THE INDEX OF THE FIRST BANK WITH NAME 'GEOM'
C     IS NEEDED. THIS INDEX IS STORED IN IW(4).
C
C            IND=IW(4)
C            IF(IND.EQ.0) GOTO 10
C     C      USE IND
C            . . .
C            GOTO 20
C     C      NO BANK WITH NAME 'GEOM' EXISTS
C         10 CONTINUE
C
C     THE LOCATION ASSIGNED TO A NAME CAN ALSO BE OBTAINED
C     USING THE FUNCTION IBLN.
C               ----
C        I=IBLN(NAME)
C        -
C        IW(I) CONTAINS THE INDEX OF THE FIRST BANK WITH THE
C        THE GIVEN NAME OR 0, IF NO BANK IS EXISTING.
C
C     EXAMPLE
C        I=IBLN('GEOM')
C     WOULD YIELD I=4 IN THE PREVIOUS EXAMPLE.
C
C     A FAST (HASH) ALGORITHM IS USED IN THE FUNCTION IBLN,
C     WITH EXECUTION TIME NEARLY INDEPENDENT FROM THE TOTAL
C     NUMBER OF EXISTING NAMES.
C
C     ALL BANKS OF THE SAME NAME ARE CONNECTED BY (FORWARD)
C     POINTERS, STORED IN IW(IND-1) FOR A BANK WITH INDEX IND.
C     THE ORDER OF THE BANKS IS IN ASCENDING NUMBER. FROM A
C     GIVEN BANK WITH INDEX IND, THE NEXT BANK OF THE SAME
C     NAME CAN BE OBTAINED BY THE FOLLOWING STATEMENT.
C
C         IND=IW(IND-1)
C         IF(IND.EQ.0) GOTO 10
C     C   USE IND
C         . . .
C         GOTO 20
C     C   NO FURTHER BANK OF THE SAME NAME
C      10 CONTINUE
C
C     THUS LOCATING OF THE NEXT BANK OF A GIVEN NAME IS
C     ALWAYS FAST, BUT E.G. LOCATING ONLY THE LAST OF
C     MANY BANKS OF THE SAME NAME IS SLOW.
C
C     THE FOLLOWING EXAMPLE SHOWS THE REPLACEMENT OF THE
C     BPOS/BNXT USE BY SOME FORTRAN-STATEMENTS.
C
C     EXAMPLE
C
C         CALL BPOS(NAME)                 IND=IBLN(NAME)+1
C      10 CALL BNXT(IND,&20)           10 IND=IW(IND-1)
C         . . .                           IF(IND.EQ.0) GOTO 20
C         . . .                           . . .
C         . . .                           . . .
C         GOTO 10                         GOTO 10
C      20 CONTINUE                     20 CONTINUE
C          . . .                           . . .
C
C     NOTE, THAT ALSO THE USE OF IBLN CAN BE AVOIDED BY USING
C     BDEF AS EXPLAINED ABOVE.
C
C
C
C     ----------------------------------------------------------------1300081500
C     13.  APPLICATION PROGRAMS
C
C     THE SUBROUTINE DESCRIBED HERE USE BANK STORAGE OF LOW
C     PRIORITY. THEY CAN BE USED FOR COUNTING, HISTOGRAMMING,
C     STORING OF VECTORS ETC. AND MAY BE USEFUL IN THE TESTING
C     PHASE OF LARGE PROGRAMS.
C     THE TOTAL NUMBER OF WORDS USED IN THE BANK COMMON /BCS/ CAN
C     NEVER EXCEED NADD WORDS (NADD IS AN ARGUMENT OF BINT).
C     THE SUBROUTINES START WITH A LETTER U FOR A NEW ENTRY, AND
C     WITH A LETTER P FOR PRINTOUT. PRINTOUT CAN ALSO BE DONE FOR
C     ALL SUBROUTINES BY PALL.
C
C     CALL PALL
C        PRINTOUT FOR ALL SUBROUTINES
C
C     THE ARGUMENT N IN THE CALLS SHOULD ALWAYS BE A POSITIVE
C     INTEGER WITH LESS THAN 9 DIGITS.
C
C
C     13.1 COUNT IN A 32768 * 32768 ARRAY
C
C                - -
C     CALL UCOND(I,J)
C        INCREASE COUNT IN ELEMENT I,J BY ONE.
C        0 .LE. I .LE. 32767  AND 0 .LE. J .LE. 32767
C
C     CALL PCOND
C        PRINT ALL ELEMENTS .NE. 0. ALL BANKS USED ARE DELETED.
C
C                - -
C     CALL QCOND(I,J,NR)
C                    --
C        WHERE I,J = INDICES OF ELEMENT
C               NR = NUMBER OF COUNTS IN ELEMENT
C
C     BANKS (CON*,IJ) OF LENGTH 8(+4) ARE USED, WHERE IJ IS CALCULATED
C     FROM I AND J.
C
C     13.2   COUNT IN A 256 * 16 ARRAY
C
C                - - -
C     CALL UTABL(N,I,J)
C        INCREASE COUNT IN ELEMENT I,J BY ONE.
C        0 .LE. I .LE. 255  AND  0 .LE. J .LE. 15.
C        MAXIMUM COUNT IN ONE ELEMENT IS 65535.
C
C                -
C     CALL PTABL(N)
C        THE I/J ARRAY NUMBER N IS PRINTED. FOR N=0 ALL ARRAYS
C        ARE PRINTED.
C
C                - -
C     CALL QTABL(N,I,AR)
C                    --
C        WHERE AR( ) = ARRAY OF LENGTH 16 CONTAINING THE COUNTS
C                      IN THE I-ROW OF ARRAY NUMBER N.
C
C     A BANK (TAB*,N*16+I/16) OF LENGTH 128(+4) IS USED FOR EACH
C     FILLED 16 * 16 SUBARRAY.
C
C
C     13.3 HISTOGRAM
C
C                - -
C     CALL UHIST(N,X)
C        ADD NEW ENTRY X INTO HISTOGRAM N WITH 100 BINS. THE BIN SIZE
C        IS CHOOSEN AUTOMATICALLY, UNLESS DHIST IS USED.
C
C                - -- --
C     CALL DHIST(N,XL,XH)
C        FOR HISTOGRAM N THE VALUES XL AND XH ARE DEFINED AS LOWER
C        AND UPPER LIMITS, RESP.. (MUST BE CALLED BEFORE UHIST).
C
C                -
C     CALL PHIST(N)
C        THE HISTOGRAM N IS PRINTED, TOGETHER WITH MEAN VALUES ETC.
C        FOR N=0 ALL HISTOGRAMS ARE PRINTED.
C
C                -
C     CALL QHIST(N,AR)
C                  --
C
C        WHERE AR( ) = ARRAY OF LENGTH 115 FOR CONTENT OF
C        HISTOGRAM N (SEE BELOW).
C
C        I               AR(I)
C
C        1 . . . 100     CONTENT OF HISTOGRAM BINS
C        101             NR OF ENTRIES (TOTAL)
C        102             LOWEST BIN LEFT EDGE
C        103             BIN SIZE
C        104             NR OF ENTRIES OUTSIDE LOW
C        105             NR OF ENTRIES OUTSIDE HIGH
C        106             MIMIMUM VALUE OF X
C        107             MAXIMUM VALUE OF X
C        108             MEAN VALUE
C        109             MEDIAN (50 PC VALUE)
C        110             STANDARD DEVIATION
C        111                   . . .        FROM QUANTILES
C        112             QUANTILES   16 PC
C        113                . . .    84 PC
C        114                . . .   2.3 PC
C        115                . . .  97.7 PC
C
C     A BANK (HST*,N) OF LENGTH 120(+4) IS USED FOR EACH HISTOGRAM.
C
C
C     13.4   CORRELATION PLOT
C
C                - - -
C     CALL UCORR(N,Y,X)
C        ADD NEW ENTRY Y,X TO CORRELATION PLOT N WITH
C        50 * 100 BINS. BIN SIZE IS ALWAYS CHOOSEN AUTOMATICALLY.
C
C                -
C     CALL PCORR(N)
C        THE CORRELATION PLOT N IS PRINTED ON ONE PAGE TOGETHER
C        WITH PROJECTIONS, MEAN VALUES AND THE CORRELATION PARAMETER.
C        BINS WITH CONTENT .NE. 0 ARE PRINTED AS A X. FOR N=0
C        ALL CORRELATION PLOTS ARE PRINTED.
C
C     A BANK (COR*,N) OF LENGTH 380(+4) IS USED FOR EACH
C     CORRELATION PLOT.
C
C
C
C     13.5   STORE VECTORS
C
C                - -- ----
C     CALL USTOR(N,AR,NDIM)
C        STORE VECTOR AR(1) . . . AR(NDIM) IN STORAGE N. THE
C        ARGUMENT NDIM SHOULD ALWAYS BE THE SAME FOR A GIVEN
C        STORAGE N.
C
C     NOTE. USTOR IS AN ENTRY OF USTOS. IF USTOS IS NOT CALLED,
C     USE A STATEMENT
C        EXTERNAL USTOS
C
C                - -- -- -- --
C     CALL USTOS(N,A1,A2,A3,A4)
C        STORE VECTOR A1, . . . IN STORAGE N. THE NUMBER OF ELEMENTS
C        IN A VECTOR MAY BE BETWEEN 1 AND 4 (VARIABLE NR OF ARGUMENTS
C        ALLOWED), BUT SHOULD ALWAYS BE THE SAME FOR A GIVEN
C        STORAGE N.
C
C     THE NUMBER OF VECTORS STORED IN A STORAGE IS LIMITED TO 100.
C     THIS LIMIT CAN BE CHANGED BY DSTOR.
C
C                ---
C     CALL DSTOR(LIM)
C        THE LIMIT ON THE NR OF VECTORS IS SET TO LIM.
C
C                -
C     CALL PSTOR(N)
C        THE VECTORS STORED IN STORAGE N ARE PRINTED. FOR N=0
C        THE VECTORS OF ALL STORAGES ARE PRINTED.
C
C     A BANK (STO*,N) IS USED FOR EACH STORAGE. THE STORAGE IS USED
C     AS FOLLOWS.
C
C     INDEX OF THE BANK       CONTENT
C            +1               NUMBER OF STORED VECTORS
C            +2               LENGTH OF ONE VECTOR
C            +3               (USED INTERNAL)
C            +4               1. ELEMENT OF 1. VECTOR
C            +5               2. ELEMENT OF 1. VECTOR
C            +6                    ETC
C
C
C     13.6   COMMENT FOR APPLICATION PROGRAMS
C
C     CALL PTEXT
C        COMMENT IS PRINTED FOR ALL APPLICATION PROGRAMS
C
C     COMMENT IS DEFINED BY THE FOLLOWING CALLS.
C
C     CALL TCOND(N,'COMMENT    $')
C        COMMENT FOR UCOND
C
C     CALL TTABL(N,'COMMENT    $')
C        COMMENT FOR UTABL
C
C     CALL THIST(N,'COMMENT    $')
C        COMMENT FOR UHIST
C
C     CALL TCORR(N,'COMMENT    $')
C        COMMENT FOR UCORR
C
C     CALL TSTOR(N,'COMMENT    $')
C        COMMENT FOR USTOR/USTOS
C
C     CALL TTEXT(N,'COMMENT    $')
C        GENERAL COMMENT
C
C     THE COMMENT-TEXT IS DELIMITED BY A $ SIGN, MAXIMUM NUMBER
C     OF CHARACTERS IS 60.
C
C
C     A BANK WITH NAME 'TEX*' IS USED FOR EACH COMMENT.
C
C
C
      RETURN
      END
