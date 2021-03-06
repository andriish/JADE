C   11/07/79 C9071601   MEMBER NAME  ZBOS11   (S)           FORTRAN
      SUBROUTINE BSCOM
C     -----------------------------------------------------------------A
C
C     APPPENDIX A.   THE SBOS - SYSTEM
C
C
C     THE SBOS SYSTEM IS A SMALL SYSTEM FOR MEMORY MANAGEMENT.
C     IT CONSISTS OF TEN SUBROUTINES WITH A TOTAL LENGTH BELOW
C     10 K BYTES. NO MULTIPLE ENTRIES OR MULTIPLE RETURNS ARE
C     USED. ALL ENTRIES ARE FULLY COMPATIBLE WITH THE BOS SYSTEM.
C     THE SBOS SYSTEM CONTAINS ALL BASIC FUNCTIONS OF THE BOS
C     SYSTEM INCLUDING INPUT AND OUTPUT OF S-TYPE RECORDS.
C     SOME OF THE SUBPROGRAMS ARE IDENTICAL TO THE BOS SYSTEM.
C     THE ADDITIONAL SUBPROGRAMS MAY ALSO BE USED IN THE BOS
C     SYSTEM. ALL BOS SYSTEM SUBPROGRAMS MAY BE USED WITHIN THE
C     SBOS SYSTEM, HOWEVER, BECAUSE OF THE USE OF MULTIPLE ENTRIES
C     IN SOME BOS SUBPROGRAMS EVENTUALLY EXTERNAL STATEMENTS ARE
C     NECESSARY, IF THE MAIN ENTRY IS NOT USED OTHERWISE.
C
C     BASIC ELEMENTS FOR THE MEMORY MANAGEMENT IN THE SBOS AND
C     BOS SYSTEMS ARE SO CALLED BANKS. A BANK HAS TWO IDENTIFIERS
C     NA AND NR, CALLED NAME AND NUMBER, AND A DATA PART OF VARIABLE
C     LENGTH. NORMALLY THE IDENTIFIER NA CONSISTS OF FOUR
C     CHARACTERS, WHILE IDENTIFIER NR IS AN INTEGER.
C
C     ALL BANKS ARE STORED IN A SINGLE COMMON/BCS/IW(NSPACE).
C     AN INDEX IND IS ASSIGNED TO EACH BANK, WHERE IW(IND)=NW
C     CONTAINS THE NUMBER OF DATA WORDS IN THAT BANK. DATA
C     ARE STORED IN IW(IND+1) TO IW(IND+NW). IN ADDITION
C     THERE ARE THREE WORDS CONTAINING NAME, NUMBER AND A
C     POINTER ACCORDING TO THE FOLLOWING SCHEMA.
C
C              I      IW(I)
C          --------   -------------------------------------
C          IND -  3   NA (NAME)
C          IND -  2   NR (NUMBER)
C          IND -  1   POINTER TO NEXT BANK OF SAME NAME
C                     -------------------------------------
C          IND        NW = NUMBER OF DATA WORDS IN THE BANK
C                     -------------------------------------
C          IND +  1   DATA WORD 1
C          IND +  2   DATA WORD 2
C          ...        ...
C          IND + NW   DATA WORD NW
C          --------   -------------------------------------
C
C     NO LIMITATIONS EXIST FOR THE LENGTH OF A SINGLE BANK OR
C     THE NUMBER OF BANKS, EXCEPT FOR THE TOTAL LENGTH OF
C     ALL BANKS AND FOR THE NUMBER OF DIFFERENT NAMES.
C     FOR MORE DETAILS OF THE SYSTEM, CONSULT THE MANUAL OF
C     THE BOS SYSTEM.
C
C
C
C
C     THE SBOS SYSTEM CONSISTS OF THE FOLLOWING NINE SUBPROGRAMS
C
C      1.   SUBROUTINE   BENT   AND SUBROUTINE BDMPA
C      2.   FUNCTION     ILOC
C      3.   FUNCTION     IBLN
C      4.   SUBROUTINE   BMLT
C      5.   SUBROUTINE   BLST
C      6.   SUBROUTINE   BDLM
C      7.   SUBROUTINE   BGAR
C      8.   SUBROUTINE   BSIN
C      9.   SUBROUTINE   BOUT
C      +    ENTRIES      VZERO, UCOPY, UCOPY2
C
C
C
C
C     A.1   INITIALIZATION
C     ------------------
C
C     INITIALIZATION IS DONE BY THE FOLLOWING STATEMENTS.
C
C     COMMON/BCS/IW(NSPACE)
C     REAL RW(NSPACE)
C     EQUIVALENCE (IW(1),RW(1))
C        WHERE NSPACE = INTEGER CONSTANT
C
C     CALL BENT(NSPACE,NADD,NNA,NNL)
C
C
C        WHERE NADD  = NR OF WORDS FOR LOW PRIORITY BANKS
C                      (DEFAULT IS 10 PERCENT OF NSPACE)
C              NNA   = MAX. NR OF DIFFERENT NAMES      (DEFAULT 100)
C              NNL   = MAX. NR OF ENTRIES IN LISTS     (DEFAULT 100)
C
C
C
C     THE DEFAULT VALUES ARE USED, IF THE ARGUMENTS ARE ZERO.
C     IF SUBPROGRAMS OF THE SBOS AND OF THE BOS SYSTEM ARE USED,
C     EITHER BINT OR BENT HAS TO BE CALLED FOR INITIALIZATION.
C     IF ERROR CONDITIONS E.G. ILLEGAL VALUES OF POINTERS ARE
C     DETECTED, THE PROGRAM STOPS.
C         STOP 1   ILLEGAL POINTER DETECTED DURING GARBAGE COLLECTION
C         STOP 2   MORE THAN NNA NAMES USED
C         STOP 3   MORE THAN NNL NAMES USED IN LISTS
C         STOP 4   UNKNOWN CONDITION
C
C
C     BANKS WITH NAMES, WHERE THE FOURTH CHARACTER IS A '*',
C     E.G. 'HST*', ARE CONSIDERED AS BANKS OF LOW PRIORITY.
C     THEY CAN ONLY BE CREATED UP TO A TOTAL AMOUNT OF
C     NADD WORDS.
C
C
C
C     ABOUT 5*NNA + 3*NNL WORDS IN THE COMMON/BCS/ ARE USED FOR
C     INTERNAL TABLES ( 801 WORDS FOR DEFAULT VALUES). BANKS ARE
C     STORED INTO THE COMMON BEGINNING WITH WORD IW(NNA+1).
C
C     IN THE FOLLOWING ARGUMENTS OF SUBPROGRAMS, WHICH ARE CHANGED
C     BY THE CALLED PROGRAM, ARE INDICATED BY A LINE BELOW THE ARGUMENT.
C
C
C
C     A.2   FUNCTION ILOC
C     -----------------
C
C     THIS FUNCTION PERFORMS DIFFERENT OPERATIONS, DESCRIBED BELOW.
C
C
C     2.1    IND=ILOC(NA,NR,NW)    CREATION OF BANK (NA,NR) WITH
C                                  NW DATA WORDS. IF THE BANK IS
C                                  ALREADY EXISTING, THE LENGTH IS
C                                  CHANGED TO THE GIVEN VALUE,
C                                  ADDITIONAL VALUES ARE SET TO 0.
C                                  IND=0, IF INSUFFICIENT SPACE.
C
C     /SBOS/ IND=ILOC(NA,NR,NW)    /BOS/ CALL BCRE(IND,NA,NR,NW,&S1,IER)
C            IF(IND.EQ.0) GOTO S1
C
C
C     2.2    IND=ILOC(NA,NR,0)     LOCATING BANK (NA,NR). IND=0, IF
C                                  BANK NOT EXISTING.
C
C     /SBOS/ IND=ILOC(NA,NR,0)     /BOS/ CALL BLOC(IND,NA,NR,&S1)
C            IF(IND.EQ.0) GOTO S1
C
C
C     2.3    IND=ILOC(NA,NR,-1)    DELETE BANK (NA,NR). IND=0, IF BANK
C                                  NOT EXISTING, OTHERWISE IND = LAST
C                                  INDEX OF BANK, HOWEVER IW(IND) IS
C                                  CHANGED TO -(NW+4).
C
C     /SBOS/ IND=ILOC(NA,NR,-1)    /BOS/ CALL BDLS(NA,NR)
C
C
C     2.4    IND=ILOC(NA,NR,-2)    DELETE BANK (NA,NR) AND DO GARBAGE
C                                  COLLECTION, BUT NOT IN THE REGION
C                                  BEFORE THE DELETED BANK.
C
C
C     2.5    IND=ILOC(NA,NR,-3)    MOVE BANK (NA,NR) TO THE END. IND=0,
C                                  IF INSUFFICIENT SPACE.
C
C     /SBOS/ IND=ILOC(NA,NR,-3)    /BOS/ CALL BLOC(IND,NA,NR,&1)
C            IF(IND.EQ.0) GOTO S1        CALL BCHM(IND,0,IER)
C                                        IF(IER.NE.0) GOTO S1
C
C
C     2.6    IND=ILOC(NA,NR,-4)    CHANGE THE NAME AND NUMBER OF THE
C                                  BANK OF THE PREVIOUS CALL OF ILOC
C                                  TO NA AND NR, RESP. IND=0 IN CASE
C                                  OF AN ERROR (BANK NOT EXISTING OR
C                                  NEW NAME,NR ALREADY IN USE).
C
C     /SBOS/ IND=ILOC(NA1,NR1,0)   /BOS/ CALL BRNM(NA1,NR1,NA2,NR2)
C            IND=ILOC(NA2,NR2,-4)
C
C
C     2.7    IND=ILOC(0,0,0)       OBTAIN INDEX OF UNUSED SPACE BEHIND
C                                  BANKS. THE LENGTH OF THE UNUSED SPACE
C                                  IS STORED IN IW(IND)=NUS. (THERE IS
C                                  ENOUGH SPACE TO CREATE A BANK OF UP
C                                  TO NUS WORDS).
C
C
C
C
C     A.3   FUNCTION IBLN
C     -----------------
C
C     THE VALUE OF THE FUNCTION IBLN(NA) WITH NAME NA IS THE INDEX I
C     CORRESPONDING TO THIS NAME. IW(I) WILL CONTAIN THE INDEX OF THE
C     FIRST BANK OF THE GIVEN NAME.
C                                  /BOS/ IDENTICAL
C
C
C     3.1    IND=IW(IBLN(NA))      OBTAIN INDEX IND OF THE FIRST BANK
C                                  WITH THE NAME NA. IND=0, IF NO BANK
C                                  BANK OF THE NAME EXISTS.
C
C
C     3.2    IND=1+IBLN(NA)        INITIALIZATION OF A LOOP, SEE BELOW.
C
C     /SBOS/ IND=1+IBLN(NA)        /BOS/ CALL BPOS(NA)
C         S1 IND=IW(IND-1)            S1 CALL BNXT(IND,&S2)
C            IF(IND.EQ.0) GOTO S2        . . .
C            . . .                       . . .
C            GOTO S1                     GOTO S1
C         S2 . . .                    S2 . . .
C
C
C
C
C     A.4   SUBROUTINE BMLT
C     -------------------
C
C     4.1    CALL BMLT(NL,LIST)    DEFINE LIST(1) . . . LIST(NL) TO BE
C                                  THE CURRENT LIST.
C                                  /BOS/ IDENTICAL
C
C
C     4.2    CALL BMLT(-1,0)       ALL NAMES OF THE SPECIAL LIST ARE
C                                  DEFINED AS THE CURRENT LIST.
C
C     /SBOS/ CALL BMLT(-1,0)       /BOS/ CALL BSLT
C
C
C     4.3    CALL BMLT(-1,1)       ALL NAMES OF THE SPECIAL LIST WITH
C                                  MARKER=1 ARE DEFINED AS THE CURRENT
C                                  LIST.
C
C     /SBOS/ CALL BMLT(-1,1)       /BOS/ CALL BSLW
C
C
C
C
C     A.5   SUBROUTINE BLST
C     -------------------
C
C     5.1 CALL BLST(NL,LIST,0)     THE NAMES LIST(1) . . . LIST(NL) ARE
C                                  ADDED TO THE SPECIAL LIST WITH
C                                  MARKER=0.
C     /SBOS/ CALL BLST(NL,LIST,0)  /BOS/ CALL BSAT(NL,LIST)
C
C
C     5.2 CALL BLST(NL,LIST,1)     THE NAMES LIST(1) . . . LIST(NL) ARE
C                                  ADDED TO THE SPECIAL LIST WITH
C                                  MARKER=1.
C
C     /SBOS/ CALL BLST(NL,LIST,1)  /BOS/ CALL BSAW(NL,LIST)
C
C
C     5.3    CALL BLST(NL,LIST,-1) THE NAMES LIST(1) . . . LIST(NL) ARE
C                                  DELETED FROM THE SPECIAL LIST.
C
C     /SBOS/ CALL BLST(NL,LIST,-1) /BOS/ CALL BSAD(NL,LIST)
C
C
C     5.4    CALL BLST(0,0,-1)     THE SPECIAL LIST IS CLEARED.
C
C     /SBOS/ CALL BLST(0,0,-1)     /BOS/ CALL BSAC
C
C
C     5.5    CALL BLST(NL,LIST,ND) THE NAMES OF THE CURRENT LIST ARE
C                      --          STORED IN THE USER ARRAY LIST( ),
C                                  WHERE ND IS THE DIMENSION OF LIST( ).
C                                  ND GREATER THAN 1. UP TO ND NAMES
C                                  ARE STORED.
C
C     /SBOS/ CALL BLST(NL,LIST,ND) /BOS/ CALL BMRT(NL,LIST,ND)
C
C
C
C
C     A.6  SUBROUTINE BDLM
C     ------------------
C
C     6.1 CALL BDLM                ALL BANKS OF THE CURRENT LIST ARE
C                                  DELETED.
C                                  /BOS/ IDENTICAL
C
C
C
C
C     A.7  SUBROUTINE BGAR
C     ------------------
C
C     7.1 CALL BGAR(IGA)           THE ACTIVE (NOT DELETED) BANKS ARE
C                   ---            SHIFTED TO THE BEGIN OF THE BANK
C                                  SPACE. IGA=0, IF NO BANKS WERE
C                                  DELETED.
C                                  /BOS/ IDENTICAL
C
C
C
C     A.8  SUBROUTINE BSIN
C     ------------------
C
C     THIS SUBROUTINE IS TO BE USED, IF BANK RECORDS OF TYPE S ARE
C     TO BE READ. THE ACTUAL READ STATEMENT TO READ A RECORD HAS TO
C     BE GIVEN IN THE USER PROGRAM, THE RECORD HAS TO BE STORED
C     AT THE BEGIN OF THE FREE SPACE OF THE COMMON/BCS/. THEN SUBROU-
C     TINE BSIN HAS TO BE CALLED, TO ACCEPT THE BANKS OF THE RECORD,
C     I.E. TO DEFINE THE POINTERS.
C
C     8.1 CALL BSIN(NRW,ICD)       THE BANKS OF THE RECORD WITH NRW
C                       ---        WORDS, READ INTO THE UNUSED BANKSPACE
C                                  IS ACCEPTED. ICD IS AN ERROR CODE.
C                                  ICD=1  RECORD LENGTH LESS THAN 4
C                                  ICD=2  BANK LENGTH NEGATIVE
C                                  ICD=3  BANK LENGTH TOO LARGE
C                                  ICD=4  WRONG ORDER IN BANK NRS
C                                  ICD=5  WRONG RECORD END
C
C     EXAMPLE
C
C     IND=ILOC(0,0,0)              SUBROUTINE FREAD(IUN,NTOT,BIN,*,*)
C     CALL FREAD(IUN,NRW,          REAL BIN(NTOT)
C                IW(IND),&S1,&S2)  READ(IUN,ERR=1,END=2) NTOT,BIN
C     CALL BSIN(NRW,ICD)           RETURN
C     IF(ICD.NE.0) GOTO S1       1 RETURN 1
C                                2 RETURN 2
C                                  END
C
C
C
C
C     A.9   SUBROUTINE BOUT
C     -------------------
C
C     THIS SUBROUTINE IS TO BE USED, IF BANK RECORDS OF TYPE S ARE
C     TO BE WRITTEN. THE SUBROUTINE BGAR IS CALLED INTERNALLY.
C
C
C     9.1   CALL BOUT(NRW,INIW)    ALL BANKS BELONGING TO THE CURRENT
C                     --- ----     LIST ARE ARRANGED IN CONTIGUOUS
C                                  BANK SPACE. THE ACTUAL WRITESTATEMENT
C                                  SHOULD WRITE A RECORD, CONSISTING OF
C                                  THE WORD NRW (LENGTH), FOLLOWED BY
C                                  THE NRW WORDS IW(INIW), IW(INIW+1)
C                                  . . . IW(INIW+NRW-1).
C                                  NRW=0 IF NO BANKS.
C
C     EXAMPLE
C
C     DEFINE CURRENT LIST          SUBROUTINE FWRITE(IUN,NTOT,BIN)
C     CALL BOUT(NRW,INIW)          REAL BIN(NTOT)
C     IF(NRW.EQ.0) GOTO S1         WRITE(IUN) NTOT,BIN
C     CALL FWRITE(IUN,NRW,         RETURN
C                IW(INIW))         END
C
C
C
C
      RETURN
      END
