C   14/10/82            MEMBER NAME  BSCOM4   (S)           FORTRAN77
      SUBROUTINE BSCOM
C
C
C
C
C     4.  DEFINING SETS OF BANKS
C
C     SEVERAL SUBROUTINES WORK ON SET OF BANKS. THESE SETS ARE
C     DEFINED BY DEFINING A LIST OF NAMES, EVERY BANK WITH A NAME,
C     WHICH IS CONTAINED IN THE LIST, BELONGS TO THE SET OF BANKS.
C     THERE ARE TWO LISTS, ONE CALLED THE CURRENT LIST AND THE OTHER
C     THE SPECIAL LIST. THE CURRENT LIST IS USED IN THE SUBROUTINES
C            BDLM AND BDLG      TO DELETE A SET OF BANKS,
C            BPRM               TO PRINT A SET OF BANKS,
C            BWRITE ETC.        TO WRITE A SET OF BANKS (EVENT).
C     THE SPECIAL LIST IS DEFINED BY THE INPUT ROUTINES, IT CONTAINS
C     AFTER THE INPUT OF ONE EVENT ALL NAMES BELONGING TO THAT
C     EVENT, AND IS LATER USED TO WRITE THE BANKS AND TO DELETE
C     THE BANKS. IF NEW BANKS ARE CREATED, THEIR NAMES CAN BE ADDED
C     TO THE SPECIAL LIST.
C
C
C     4.1  DEFINING TH CURRENT LIST
C
C     THERE ARE THREE CALL TO DEFINE THE CURRENT LIST.
C
C               - ----
C     CALL BMLT(N,LIST)
C
C        WHERE LIST = ARRAY OF N NAMES
C
C     CALL BSLT
C
C     ALL NAMES OF THE SPECIAL LIST ARE DEFINED AS THE CUURENT LIST.
C
C     CALL BSLW
C
C     THE NAMES OF THE SPECIAL LIST WITH THE MARKER = 1 ARE DEFINED
C     AS THE CURRENT LIST.
C
C     THE LATTER TWO CALL ARE USUALLY USED IN THE FOLLOWING
C     CONNECTION.
C
C     CALL BSLW
C     CALL BWRITE(IUN)      WRITE AN EVENT OF BANKS WITH NAMES OF
C                           THE SPECIAL LIST WITH MARKER +1.
C
C     CALL BSLT
C     CALL BDLG             DELETE ALL BANKS WITH NAMES OF THE
C                           SPECIAL LIST AND MAKE A GARBAGE
C                           COLLECTION.
C
C     EXAMPLE
C
C     CALL BMLT(2,'HEADGEOM')
C
C
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
C     4.2  UPDATING THE SPECIAL LIST
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
C     THE CONNECTION BETWEEN THE TWO LISTS AND THE DIFFERENT
C     CALLS IS ILLUSTRATED IN THE FLOW DIAGRAM BELOW
C
C
C
C
C                        +---------------+
C                        I               I
C                        I SPECIAL LIST  I
C                        I     OF NAMES  I
C                ADDS    I               I
C        BSAW   ->->->->-I->->->-+-<-<-<-I-<-<-<-<-   INPUT ROUTINE
C                CHANGES I       |       I DEFINES
C                        I       V       I
C                        I       |       I
C                        I       V       I
C                ADDS    I       |       I
C        BSAT   ->->->->-I->-+-<-+->-+   I
C                CHANGES I   |       |   I
C                        I   V       V   I
C                        I   |       |   I
C                        +---------------+
C                            |       |
C                            V       V
C                            |       |
C
C          (USUALLY FOR    BSLT    BSLW   (USUALLY FOR
C       DELETING BANKS)                   WRITING BANKS)
C                            |       |
C                            V       V
C                            |DEFINES|
C                            V       V
C                            |       |
C                        +---------------+
C                        I               I
C                        I CURRENT LIST  I
C                        I     OF NAMES  I
C                DEFINES I               I
C        BMLT   ->->->->-I               I
C                        I               I
C                        I               I
C                        I               I
C                        +---------------+
C                                |
C                                V
C                                |
C                                V
C
C                        USED IN BPRM
C                                BDLM, BDLG
C                                OUTPUT ROUTINES
C
C
C
     RETURN
     END
