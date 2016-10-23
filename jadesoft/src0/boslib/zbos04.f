C   21/11/78 C9012401   MEMBER NAME  ZBOS04   (S)           FORTRAN
      SUBROUTINE BSCOM
C     -----------------------------------------------------------------400000200
C     4.  SETS OF BANKS
C
C     SEVERAL SUBROUTINES WORK ON SET OF BANKS. THESE SETS ARE
C     DEFINED BY DEFINING A LIST OF NAMES, EVERY BANK WITH A NAME,
C     WHICH IS CONTAINED IN THE LIST, BELONGS TO THE SET OF BANKS.
C     THERE ARE TWO LISTS, ONE CALLED THE CURRENT LIST AND THE OTHER
C     THE SPECIAL LIST. THE CURRENT LIST IS USED IN THE SUBROUTINES
C            BDLM AND BDLG      TO DELETE A SET OF BANKS,
C            BPRM               TO PRINT A SET OF BANKS,
C            BWRITE ETC.        TO WRITE A SET OF BANKS (EVENT).
C
C     THE SPECIAL LIST IS DEFINED BY EACH INPUT ROUTINE, IT CONTAINS
C     AFTER THE INPUT OF ONE EVENT ALL NAMES (WITH MARKER EQUAL
C     TO 1) BELONGING TO THAT EVENT, AND IS LATER USED TO WRITE
C     THE BANKS AND TO DELETE THE BANKS. IF NEW BANKS ARE CREATED,
C     THEIR NAMES CAN BE ADDED TO THE SPECIAL LIST.
C
C
C     4.1  THE CURRENT LIST
C
C     THERE ARE THREE CALLS TO DEFINE THE CURRENT LIST.
C
C               - ----
C     CALL BMLT(N,LIST)
C
C        WHERE LIST( ) = ARRAY OF N NAMES
C
C
C
C     CALL BSLT
C
C     ALL NAMES OF THE SPECIAL LIST ARE DEFINED AS THE CURRENT LIST.
C
C
C
C     CALL BSLW
C
C     THE NAMES OF THE SPECIAL LIST WITH THE MARKER = 1 ARE DEFINED
C     AS THE CURRENT LIST.
C
C
C
C     THE LATTER TWO CALLS ARE USUALLY USED IN THE FOLLOWING
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
C
C
C     THE CURRENT LIST OF NAMES CAN BE STORED IN AN USER DEFINED
C     ARRAY BY THE CALL
C
C                      -----
C     CALL BMRT(N,LIST,NLIST)
C
C        WHERE LIST( ) = ARRAY OF LENGTH NLIST
C                    N = NR OF NAMES STORED IN THE LIST
C
C     UP TO NLIST NAMES OF THE CURRENT LIST OF NAMES ARE STORED
C     INTO THE ARRAY LIST( ).
C
C
C
C     4.2  THE SPECIAL LIST
C
C
C                - ----                      - ----
C      CALL BSAT(N,LIST)           CALL BSAW(N,LIST)
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
C               - ----
C     CALL BSAD(N,LIST)
C
C     THE NAMES OF THE LIST ARE DELETED FROM THE SPECIAL LIST.
C
C
C
C
C
C     THERE ARE THREE ADDITIONAL CALLS, WHICH MAY BE USEFUL
C     UNDER SPECIAL CIRCUMSTANCES. THESE CALLS ALLOW TO CLEAR
C     THE SPECIAL LIST, TO SAVE THE SPECIAL LIST AND TO RESTORE
C     THE SAVED SPECIAL LIST.
C
C     CALL BSLC
C
C     THE SPECIAL LIST IS CLEARED (IT WILL CONTAIN NO NAMES).
C
C               --                     --
C     CALL BSLS(IS)          CALL BSLR(IS)
C
C     THE SPECIAL LIST IS SAVED BY BSLS UNDER AN ARBITARY
C     NUMBER IS. BY BSLR THE SAVED LIST IS RESTORED AS THE
C     SPECIAL LIST. THE ROUTINES USE A BANK TO STORE THE
C     SPECIAL LIST.
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
C
     RETURN
     END
