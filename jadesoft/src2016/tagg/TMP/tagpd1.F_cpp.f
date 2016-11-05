C   12/03/84 412041858  MEMBER NAME  PEDFX2   (S)           FORTRAN
C
C
C
C
C
C
C
      SUBROUTINE TAGPD1
C
C ROUTINE TO SUBTRACT PEDESTALS - 1981/1982
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
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
       INTEGER*4 TEST(64)/  3,4,6,7 ,5,8,9,12 ,10,11,13,14 ,15,16,17,1
     *             ,19,20,22,23 ,21,24,25,28 ,26,27,29,30 ,31,32,1,2
     *             ,33,34,63,64 ,35,36,38,39 ,37,40,41,44 ,42,43,45,46
     *             ,47,48,49,50 ,51,52,54,55 ,53,56,57,59 ,58,59,61,62/
       VALUE  = 800.0
       APICKM = 0.0
       APICKP = 0.0
       NUMM   = 0
       NUMP   = 0
C
C
C
C
C 1/4 OF A TAGGING SYSTEM =
C
C                               ___________
C                               I    I    I
C                     __________I    I    I
C                     I    I    L____L____I
C                     I    I    I    I    I
C                     L____L____I    I    I
C                     I    I    L____L____I
C                     I    I    I
C                  ___L____L____I
C                  I    I    I
C                  I    I    I
C                  L____L____I
C                  I    I    I
C                  I    I    I
C                  L____L____I__
C                     I    I    L
C                     I    I    I__________
C                     L____L____I    I    I
C                     I    I    L    I    I
C                     I    I    I____L____I
C                     L____L____I    I    I
C                               I    I    I
C                               L____L____I
C
C   'SUPERBLOCK' =
C
C                  ___________
C                  I    I    I
C                  I    I    I
C                  L____L____I
C                  I    I    I
C                  I    I    I
C                  L____L____I
C
C

C
C LOOP OVER ALL 'SUPERBLOCKS' (SET OF FOUR BLOCKS) IN -Z TAGGER
C
       DO 10 I = 1,32,4
C
          K = I + 3
C
C LOOP OVER ALL MEMBERS OF SUPERBLOCK
C
          DO 20 J = I,K
             ITEST = TEST(J)
C
C      TEST FOR GREATER THAN 'VALUE'
C
             IF ( CATAG(ITEST) .GT. VALUE ) GOTO 10
   20     CONTINUE
C
C   NONE OF THE BLOCKS IN THE SUPERBLOCK UNDER TEST HAVE FAILED
C   TO BE LESS THAN 800,SO ANY HITS MUST BE PEDESTALS
C
          DO 30 L = I,K
             ITEST = TEST(L)
             IF ( CATAG(ITEST) .EQ. 0 ) GOTO 30
             APICKM = APICKM+CATAG(ITEST)
             NUMM = NUMM + 1
   30     CONTINUE
   10  CONTINUE
C
C NOW PLUS ZED
C
       DO 11 I = 33,64,4
C
          K = I+3
          DO 21 J = I,K
             ITEST = TEST(J)
             IF ( CATAG(ITEST) .GT. VALUE ) GOTO 11
   21     CONTINUE
C
C   NONE OF THE BLOCKS IN THE SUPERBLOCK UNDER TEST HAVE FAILED
C   TO BE LESS THAN 800
C
          DO 31 L = I,K
             ITEST = TEST(L)
             IF ( CATAG(ITEST) .EQ. 0 ) GOTO 31
             APICKP = APICKP+CATAG(ITEST)
             NUMP = NUMP + 1
   31     CONTINUE
   11  CONTINUE
C
C    CALCULATE THE MEAN NON ZERO PEDESTAL
C
       IF ( NUMM .EQ. 0 ) GOTO 50
       APICKM = APICKM/NUMM
C
C NOW WE HAVE THE VALUES APICKP AND APICKM TO BE SUBTRACTED
C  DO THE SUBTRACTION
C
       IF ( NUMM .LE. 4 ) GOTO 50
       DO 40 I = 1,32
          IF ( CATAG(I) .EQ. 0 ) GOTO 40
          CATAG(I) = CATAG(I) - APICKM
          IF ( CATAG(I) .LT. 0) CATAG(I) = 0
   40  CONTINUE
C
C
*** PMF 15/10/99: new GOTO label introduced behind the 'END DO'-CONTINUE statement at label 41
*   50  IF ( NUMP .EQ. 0 ) GOTO 41
*       IF ( NUMP .LE. 4 ) GOTO 41
   50  IF ( NUMP .EQ. 0 ) GOTO 42
       IF ( NUMP .LE. 4 ) GOTO 42
*** PMF (end)
C
       APICKP = APICKP/NUMP
C
       DO 41 I = 33,64
          IF ( CATAG(I) .EQ. 0 ) GOTO 41
          CATAG(I) = CATAG(I) - APICKP
          IF ( CATAG(I) .LT. 0) CATAG(I) = 0
   41  CONTINUE
*** PMF 15/10/99: new label added for a former GOTO statement
   42  CONTINUE
*** PMF (end) 
C
C
        RETURN
        END
