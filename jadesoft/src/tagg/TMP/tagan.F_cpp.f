C   12/03/84 709172208  MEMBER NAME  TAGAN    (S)           FORTRAN
C
C
C
C------   T A G A N   ------   T A G A N   ------   T A G A N   ------
C
C------   T A G A N   ------   T A G A N   ------   T A G A N   ------
C
C------   T A G A N   ------   T A G A N   ------   T A G A N   ------
C
C
C
      SUBROUTINE TAGAN(IER,NRUN)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C TAGAN IS THE CONTROLLING ROUTINE FOR FORWARD DETECTOR ANALYSIS
C              ITS CHIEF PURPOSE IS TO DECIDE WHAT ROUTINE TO CALL
C              EITHER ANATAG OR TAGGTP
C
C     TAGGTP IS WRITTEN BY A.J.FINCH TO ANALYSE 1981 ONWARDS DATA
C     ANATAG IS WRITTEN BY H.WRIEDT  TO ANALYSE PRE 1981 DATA
C
C INPUT - NRUN -> THIS IS USED TO OVERIDE THE ATTEMPT BY
C                  TAGAN TO DECIDE WHAT SIMULATION HAS BEEN DONE
C                  WHEN MONTE CARLO DATA IS ANALYSED.
C
C                 IF NRUN = 0 TAGAN LOOKS AT THE FIRST WORD OF
C                  ATAG BANK DATA -
C                              - IF IT EQUALS ZERO THIS IS MARK 1
C                              - IF IT EQUALS ONE THIS IS MARK 2
C                              - IF IT EQUALS TWO THIS IS MARK 3
C
C                    (IN MCJADE THIS WORD IS SET IN STATAG/STAG81/..)
C
C OUTPUT - IER -> ERROR CODE:
C                     0 - ATAG EXISTS AND CLUSTERS WERE FOUND
C                     1 - ATAG EXISTS BUT CLUSTERS WERE NOT FOUND
C                     2 - ANALYSIS COMPLETED BUT NOT ENOUGH SPACE
C                         TO CREATE ONE OR MORE OUTPUT BANK
C                    10 - ATAG BANK NOT FOUND - NO ANALYSIS DONE
C                    11 - HEAD BANK NOT FOUND - NO ANALYSIS DONE
C
C 24/2/84 A.J.FINCH
C
C INSTALLED ON TAGG.S 12/3/84
C
C      MOD  J. NYE  24/05/84  CALLS TAGMRK
C      MOD  J. NYE  22/06/84  CHANGES MADE TO OUTPUT
C LAST MOD  J. NYE  13/09/84  CHANGES MADE TO OUTPUT
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       IMPLICIT INTEGER*2 (H)
C----------------------------------------------------------------------
C             MACRO CDATA .... BOS COMMON.
C
C             THIS MACRO ONLY DEFINES THE IDATA/HDATA/ADATA NAMES.
C             THE ACTUAL SIZE OF /BCS/ IS FIXED ON MACRO CBCSMX
C             OR BY OTHER MEANS. A DEFAULT SIZE OF 40000 IS GIVEN HERE.
C
C----------------------------------------------------------------------
C
      COMMON /BCS/ IDATA(40000)
      DIMENSION HDATA(80000),ADATA(40000),IPNT(50)
      EQUIVALENCE (HDATA(1),IDATA(1),ADATA(1)),(IPNT(1),IDATA(55))
      EQUIVALENCE (NWORD,IPNT(50))
C
C------------------------ END OF MACRO CDATA --------------------------
C
C
C FOLLOWING ARE ALL THE COMMONS USED BY TAGGTP ROUTINES
C
      COMMON/COMTAG/LISTOK,NLIST(64,9,2),
     1XMAP(64),YMAP(64)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C COMTAG - THESE NUMBERS ARE NEEDED THROUGHOUT THE JOB BY ANALYSIS
C           ROUTINES
C ================================================================
C
C
C LISTOK->  FLAGS THAT NEIGHBOUR LIST HAS BEEN GENERATED
C
C NLIST ->  LIST OF CLOSEST NEIGHBOURS TO A BLOCK   (I,1) = NUMBER
C           OF CLOSEST NEIGHBOURS (I,2-9)ADC ADDRESSES OF NEIGHBOURING
C           BLOCKS  (CLUSFN).THE THIRD ARGUMENT REFERS TO THE MARK
C           OF TAGGING SYSTEM - EITHER MARK 2 OR MARK 3.
C
C XMAP(64) -> XMAP AND YMAP CONTAIN THE X,Y ADDRESSES OF THE BLOCKS IN
C YMAP(64) -> THE 1982 TAGGING SYSTEM
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C
C------------ C O M M O N    C W O R K   F O R   T A G A N -------------
C
C
       COMMON/CWORK/MARK,IFLMRK,IMC,NCLST,NNEI,
     *              ISTMZ,ISTPZ,IENDMZ,IENDPZ,
     *              SIGX,SIGY,SIGEN,
     *              CAND(3),CLUS(9,2),CMAP(10,9),
     *              SADC(32,2),CATAG(192)
C
C
C CWORK - WORKSPACE USED ONLY ONCE PER EVENT FOR INTERNAL PROCESSING
C ==================================================================
C
C MARK   ->  WHICH 'MARK' OF TAGGER - 1 = 1981,2
C                                   - 2 = 1983 ONWARDS
C
C IFLMRK ->  SET TO '1' BY TAGMRK
C
C IMC    ->  SET TO '1' BY TAGMRK IF MC DATA
C
C CATAG  ->  CONTAINS THE ADC CONTENTS UNPACKED FROM ATAG
C
C SADC   ->  COMMON FOR ADC'S AFTER SORTING  (SORT 1)
C
C CMAP(I,1...9) ->  ADDRESS OF ADC'S IN CLUSTER I,SORT23 PUTS THESE IN
C                   ORDER OF ENERGY.
C
C CAND(3) ->  X, Y, AND ENERGY OF A FOUND CLUSTER IN AFTER CLSPS
C
C SIGX,SIGY,SIGEN ->  ERROR ON X, Y, AND ENERGY AFTER CLSPS
C
C CLUS(9,2) ->  ADC ADDRESS AND CONTENTS OF CLUSTERS - SORTED BY ENERGY
C
C NCLST   ->  NUMBER OF CLUSTERS THIS END
C ISTMZ   ->  POINTER TO START OF -Z DATA IN CATAG ( ALWAYS  1       )
C ISTPZ   ->  POINTER TO START OF +Z DATA IN CATAG ( EITHER 33 OR 25 )
C IENDMZ  ->  POINTER TO   END OF -Z DATA IN CATAG ( EITHER 32 OR 24 )
C IENDPZ  ->  POINTER TO   END OF +Z DATA IN CATAG ( EITHER 64 OR 48 )
C
C A.J.FINCH 24/2/84
C MODIFIED 12/3/84 CATAG PUT TO END AND INCREASED TO 192
C  TO ALLOW IT TO BE USED FOR 1979,80 TAGGER IN GRAPHICS
C LAST MOD : J. NYE  30/05/84  RE-ORGANIZED INCLUDING IFLMRK
C
C
C-----------------------------------------------------------------------
C
C
C   12/03/84 412041226  MEMBER NAME  CTAGGO   (S)           FORTRAN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C HERE ARE THE COMMONS THAT EVENTUALLY BECOME THE TP BANKS ACCLS -
C TAGG/O ,1,2,3
C CONVENTION IN NAMING -
C
C FIRST LETTER -
C         C - NAME OF COMMON
C         H - NAME OF VARIABLES *2 TYPE      INTEGER
C         I -   "       "       *4 TYPE         "
C         A -   "       "                    REAL
C
C REST OF NAME AS FOR TAGG BANKS SEE JADE COMPUTER NOTE 16 FOR MORE DETA
C
C ACTUALLY ALL THIS COULD ALSO GO IN WORKSPACE BUT I CANT BE BOTHERED!
C
C**** IMPORTANT :  IF YOU CHANGE THE SIZE OF THESE COMMONS PLEASE
C                  REMEMBER TO CHANGE 'TAGINT' WHICH CLEARS THEM.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
      COMMON / CACLS  / HID1,HID2,HIPM,HIPZ,HDUM,HIPL,HDUMM1(134)
      DIMENSION HACLS(140)
      EQUIVALENCE ( HACLS(1) , HID1 )
C
      COMMON / CTAGG0 / HID3,HID4,HTRACK,HTRZMI,HTRZPL,HCLST,
     *                  HCLZMI,HCLZPL,HNEUT,HCOL,HTYPE,HER,HCORR,
     *                  HPBLAT,HDUM1,HDUM2,HSCAL(36),HWPCL,HWPTR,
     *                  HDUMM2(14)
      DIMENSION     ATAGG0(34)
      EQUIVALENCE ( HID3 , ATAGG0(1) )
C
      COMMON / CTAGG1 / HMAP(2,20)
C
      COMMON / CTAGG2 / ATAGG2(260)
      DIMENSION         ITAGG2(260)
      INTEGER*2         HTAGG2(520)
      EQUIVALENCE ( HTAGG2(1) , ITAGG2(1) , ATAGG2(1) )
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
       COMMON / CONTRL / JWRITE
       LOGICAL NFIRST
       DATA NFIRST  / .FALSE. /
       DATA NDEBUG  /      25 /
       DATA MARK0 / -1 /
C
C
C------------------------ C O D E --------------------------------------
C
C
C
C---                                    CHECK ATAG BANK EXISTS
C
C
CCC    JWRITE = 0
CCC    IF ( NDEBUG .GT. 0 ) JWRITE = 1
CCC    NDEBUG = NDEBUG - 1
       IWRITE = JWRITE
       IER = 0
       CALL BLOC(IND,'ATAG',0,*100)
       GOTO 200
  100  IER = 10
       RETURN
C
C
C---                                    CHECK HEAD BANK EXISTS
C
C
  200  IHEAD = IDATA(IBLN('HEAD'))
       IF ( IHEAD .GT. 0 ) GOTO 400
  300  IER = 11
       RETURN
  400  CONTINUE
C
C
C---                                    GET DATA FROM HEAD BANK
C
C
       IHHEAD = IHEAD + IHEAD
       NDATE  = HDATA(IHHEAD +  8)
       NEXP   = HDATA(IHHEAD +  9)
       NRUNTG = HDATA(IHHEAD + 10)
       NEVEN  = HDATA(IHHEAD + 11)
       NERGY  = HDATA(IHHEAD + 29)
       IF ( (NERGY .GT. 30000) .OR. (NERGY .LT. 5000) ) NERGY = 17500
       ERGY   = 0.001 * FLOAT( NERGY )
C
C
C---                                    DECIDE A) MC OR REAL DATA
C---                                           B) WHICH DETECTOR
C
C
C
C   CALL TAGMRK - RETURNS 1 IF NO HEAD BANK
C   SETS  IMC = 1 IF MC DATA
C   SETS MARK = 0,1,OR 2 FOR 79/80,81/82, AND 83
C                             DETECTORS
C
        CALL TAGMRK(*300)
C
C
C----------------------------------------MONTE CARLO DATA ONLY----------
C
C
C
C---                                    IF MC DATA AND NRUN GIVEN THEN
C---                                    FORCE MARK TO AGREE WITH NRUN
C
C
C
       IF ( (IMC .EQ. 0) .OR. (NRUN .EQ. 0) ) GOTO 5
C
       NRUNTG = NRUN
       IF (  NRUNTG .LT.  6000 )                            MARK = 0
       IF ( (NRUNTG .GE.  6000) .AND. (NRUNTG .LT. 12948) ) MARK = 1
       IF (  NRUNTG .GE. 12948 )                            MARK = 2
C
C-----------------------------------------------------------------------
C
    5  CONTINUE
C
C
C---                                    PRINT MARK USED ETC
C  NOTE  :  PRINTS REAL MARK (1-3)
C  RATHER THAN SOFTWARE MARK (0-2)
C
CCC    IF ( NFIRST ) GOTO 1
       IF ( MARK .EQ. MARK0 ) GOTO 1
       MARK0  = MARK
       NYMARK = MARK + 1
       WRITE(6,601)
       IF (  IMC .EQ. 0 ) WRITE(6,602) NYMARK
       IF (  IMC .EQ. 1 ) WRITE(6,603) NYMARK
       IF ( MARK .EQ. 0 ) WRITE(6,605)
       IF ( MARK .EQ. 1 ) WRITE(6,606)
       IF ( MARK .EQ. 2 ) WRITE(6,607)
       IF ( (IMC .EQ. 1) .AND. (NRUN.NE.0) ) WRITE(6,608) NRUN
       WRITE(6,604)
       NFIRST = .TRUE.
  601  FORMAT(/,/,
     * /,' ++++ T A G A N ++++++++ T A G A N ++++++++ T A G A N ++++',
     * /,' +                                                       +')
  602  FORMAT(
     *   ' +  REAL DATA ',9X,' MARK ',I3,'   TAGGER                +',
     * /,' +                                                       +')
  603  FORMAT(
     *   ' +  MC DATA   ',9X,' MARK ',I3,'   TAGGER SIMULATION     +',
     * /,' +                                                       +')
  605  FORMAT(
     *   ' +                      1979 - 1980                      +')
  606  FORMAT(
     *   ' +                      1981 - 1982                      +')
  607  FORMAT(
     *   ' +                      1983 - 1986                      +')
  608  FORMAT(
     *   ' +                                                       +',
     * /,' +  FORCED  BY  NRUN  =  ',I6,'                          +')
  604  FORMAT(
     *   ' +                                                       +',
     * /,' +++++++++++++++++++++++++++++++++++++++++++++++++++++++++',
     * /,/)
C
C
C
C---                                    CALL ANALYSIS ROUTINES
C
C
    1  CONTINUE
       IF ( MARK .GT. 0    ) GOTO 2
C
C
C----------------------------------------MARK = 0 ONLY------------------
C
C
C
C---                                    CALL ANATAG FOR PRE 1981 DATA
C
C
       IF ( IMC .EQ. 0 ) CALL ANATAG(IER1)
       IF ( IMC .EQ. 1 ) CALL AMCTAG(IER1)
       GOTO 1000
C
C
C----------------------------------------MARK = 1 OR 2 ONLY-------------
C
C
C---                                    CALL TAGGTP ROUTINE
C
C
C IWRITE CAN BE SET TO 1 TO TURN ON DEBUGGING INFO
C
C RETURN 1 FROM TAGGTP IS DONE IF ATAG NOT FOUND - SHOULD BE
C REDUNDANT WHEN CALLED FROM TAGAN
C RETURN 2 FROM TAGGTP IS DONE IF ERROR IN TAGSTO -
C  IE NO ROOM FOR TAGG BANKS IN COMMON/BCS/
C
    2  CALL TAGGTP(IWRITE,ERGY,*100,*100)
C
C
C-----------------------------------------------------------------------
C
C
C
C---                                    CHECK TAGG/0 BANK FOR CLUSTERS
C
C
 1000  CONTINUE
       CALL BLOC(INTAG0,'TAGG',0,*1001)
       NCLST = HDATA( INTAG0 + INTAG0 + 6 )
       IF ( NCLST .LE. 0 ) IER = 1
       RETURN
 1001  IER = 2
       RETURN
       END
