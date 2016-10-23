C   21/11/78 C9012401   MEMBER NAME  ZBOS03   (S)           FORTRAN
      SUBROUTINE BSCOM
C     -----------------------------------------------------------------300000200
C     3.  LOCATING OF BANKS
C
C     3.1  LOCATING A BANK
C
C
C                   -- --                                     -- --
C     CALL BLOC(IND,NA,NR,&S1)                  CALL CLOC(IND,NA,NR)
C               ---       ---                             ---
C
C        WHERE NA = NAME OF THE BANK
C              NR = NUMBER OF THE BANK
C              IND = INDEX OF THE BANK (NORMAL RETURN)
C
C     SPECIAL CONDITIONS
C                           BLOC             CLOC
C
C     BANK NOT EXISTING     IND=0            IND=0
C                           RETURN 1
C
C
C     3.2  LOCATING ALL BANKS WITH THE SAME NAME
C
C
C     LOCATING OF ALL BANKS OF A GIVEN NAME IN ASCENDING ORDER
C     OF NR IS DONE IN THE FOLLOWING WAY.
C
C               ----                               ----
C     CALL BPOS(NAME)                    CALL CPOS(NAME)
C
C        WHERE NAME = NAME OF THE BANKS
C
C               ---                                ---
C     CALL BNXT(IND,&S1)                 CALL CNXT(IND)
C               --- ---                            ---
C
C        WHERE IND = INDEX OF NEXT BANK OF THE SAME NAME
C
C     SPECIAL CONDITION
C                              BNXT             CNXT
C
C     NO FURTHER BANK          IND=0            IND=0
C                              RETURN 1
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
C
C
C
C     LOCATING ALL BANKS OF A GIVEN NAME CAN ALSO BE DONE USING
C     SUBROUTINE BDAR.
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
C
C
C
C     3.3  FASTER METHODS
C
C
C     TO LOCATE BANKS, FASTER METHODS CAN BE USED THAN THE METHODS
C     DESCRIBED ABOVE. THE DECREASE OF TIME IS OBTAINED BY AVOIDING
C     ANY SUBROUTINE CALLS.
C
C     THE INDEX IND OF THE FIRST BANK OF A GIVEN NAME (THE BANK WITH
C     THE SMALLEST VALUE OF NR) IS STORED IN ONE OF THE LOCATIONS
C     IW(1) TO IW(NAMAX), WHERE NAMAX IS THE MAXIMUM NR OF DIFFERENT
C     NAMES. THE INDEX I OF THIS LOCATION IS FIXED DURING THE JOB.
C     IT CAN BE OBTAINED BY THE FUNCTION IBLN, E.G.
C                  I = IBLN(NAME)
C     THUS IW(I) IS ZERO, IF NO BANK WITH THAT NAME EXISTS. IF
C                IND = IW(I)
C     IS NOT EQUAL TO ZERO, THEN IND IS THE INDEX OF THE
C     FIRST BANK FOR THAT NAME. THE POINTER TO THE NEXT BANK
C     OF THE SAME NAME (NEXT LARGER VALUE OF NR) IS STORED
C     IN THE LOCATION IW(IND-1), THUS CAN BE OBTAINED BY THE
C     STATEMENT
C                IND = IW(IND-1)
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
C     BELOW THE PROGRAMMING IS DESCRIBED FOR THE CASE, WHERE A LOOP
C     ON ALL BANKS OF THE SAME NAME IS EXECUTED. A LOCAL VARIABLE
C     INDF IS INITIALIZED TO ZERO BY A DATA STATEMENT, AND IS
C     DEFINED AS
C               INDF = IBLN(NAME)+1
C     AT THE FIRST EXECUTION OF THE LOOP.
C
C           DATA INDF/0/
C           IF(INDF.EQ.0) INDF=IBLN(NAME)+1
C           IND=INDF
C        10 IND=IW(IND-1)                         CALL BPOS(NAME)
C           IF(IND.EQ.0)    GOTO 30            10 CALL BNXT(IND,&30)
C      C    NEXT BANK FOUND                       . . .
C           . . .                                 . . .
C           . . .                                 . . .
C           GOTO 10                               GOTO 10
C      C    END OF LOOP                        30 . . .
C        30 . . .
C
C
C     BELOW THE PROGRAMMING IS DESCRIBED FOR THE CASE, WHERE A
C     CERTAIN BANK (NAME,NR) HAS TO BE LOCATED.
C
C
C           DATA INDF/0/
C           IF(INDF.EQ.0) INDF=IBLN(NAME)+1
C           IND=INDF
C        10 IND=IW(IND-1)
C           IF(IND.EQ.0)    GOTO 30
C           IF(IW(IND-2)-NR) 10,20,30         CALL BLOC(IND,NAME,NR,&30)
C      C    BANK (NAME,NR) FOUND              . . .
C        20 . . .
C
C      C    BANK NOT FOUND
C        30 . . .                          30 . . .
C
      RETURN
      END
