C   05/12/84 511071141  MEMBER NAME  TAGS2H   (S)           FORTRAN
C
C
C
C------  T A G S 2 H  ------  T A G S 2 H  ------  T A G S 2 H  ------
C
C------  T A G S 2 H  ------  T A G S 2 H  ------  T A G S 2 H  ------
C
C------  T A G S 2 H  ------  T A G S 2 H  ------  T A G S 2 H  ------
C
C
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C
      FUNCTION TAGS2H(I)
C
C CONVERT SOFTWARE ADDRESS TO HARDWARE ADDRESS
C SEE ALSO TAGH2S
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
C
      IF ( MARK .LT. 2 ) GOTO 100
C
C---                               HERE FOR 1983... TAGGER
C
      IF ( (I .LT.  1) .OR.  (I .GT. 48) ) TAGS2H =  -1
      IF ( (I .GE.  1) .AND. (I .LE.  8) ) TAGS2H = I +  3
      IF ( (I .GE.  9) .AND. (I .LE. 16) ) TAGS2H = I + 11
      IF ( (I .GE. 17) .AND. (I .LE. 24) ) TAGS2H = I + 19
      IF ( (I .GE. 25) .AND. (I .LE. 32) ) TAGS2H = I + 27
      IF ( (I .GE. 33) .AND. (I .LE. 40) ) TAGS2H = I + 35
      IF ( (I .GE. 41) .AND. (I .LE. 48) ) TAGS2H = I + 43
      RETURN
C
C---                                HERE FOR 1981/2 TAGGER
C
 100  TAGS2H = I - 1
      RETURN
      END