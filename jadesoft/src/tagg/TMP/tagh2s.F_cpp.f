C   05/12/84 705211147  MEMBER NAME  TAGH2S   (S)           FORTRAN
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
       SUBROUTINE TAGH2S(ISAD,IHAD,*)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  THIS ROUTINE CONVERTS HARDWARE ADDRESSES TO SOFTWARE ADDRESSES
C
C  ISAD - OUTPUT - SOFTWARE ADDRESS
C  IHAD - INPUT  - HARDWARE ADDRESS
C
C  RETURN 1 IF IT CANT DO IT ( ILLEGAL ADDRESS SUPPLIED )
C
C
C  NEW VERSION 29/07/84     A.J.FINCH
C  LAST MOD :  18/067/84    J NYE  TIDIED UP
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      INTEGER*2 HDATA
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
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   CONVERT HARWARE ADDRESSES TO SOFTWARE ADDRESSES
C
C    THE PATTERN FOR 1983 ONWARDS IS -
C
C    HARDWARE ADDRESS                              SOFTWARE ADDRESS
C
C         0 - 3             NOT USED
C         4 - 7           INNER RING - Z               1 - 4
C         8 - 11            "      "                   5 - 8
C        12 - 15            NOT USED
C        16 - 19             "   "
C        20 - 23           MIDDLE RING - Z             9- 12
C        24 - 27              "  "                     13-16
C        28 - 31            NOT USED
C        32 - 35             "   "
C        36 - 39            OUTER RING - Z         17-20
C        40 - 43              "   "                21-24
C        44 - 47            NOT USED
C
C        48 - 51            NOT USED
C        52 - 55          INNER RING + Z              25 - 28
C        56 - 59            "      "                  29 - 32
C        60 - 63            NOT USED
C        64 - 67             "   "
C        68 - 71           MIDDLE RING + Z            33- 36
C        72 - 75              "  "                    37 -40
C        76 - 79            NOT USED
C        80 - 83             "   "
C        84 - 87            OUTER RING + Z         41-44
C        88 - 91              "   "                45-48
C        92 - 95            NOT USED
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C
C---------------------------- C O D E ----------------------------------
C
C
C
       IF ( MARK .LT. 2 ) GOTO 100
C
C---                                     HERE FOR 1983 ONWARDS TAGGER
C
       K    =   0
       J    = -12
       ISAD =   0
       DO 10 K = 3,43,8
          J = J + 16
          IF ( (IHAD .GE. J) .AND. (IHAD .LE. (J+7)) ) ISAD = IHAD - K
          IF ( ISAD .NE. 0 ) GOTO 30
  10   CONTINUE
       RETURN 1
C
C---                                     CORRECT FOR BAD CABLING
C
C   FOR REAL DATA BETWEEN RUNS 14589
C   AND 15689 CABLES FROM COUNTERS
C   38 AND 40 TO ADCS WERE INTERCHANGED
C
  30    CONTINUE
        IF ( (ISAD .NE. 38) .AND. (ISAD .NE. 40) ) RETURN
        IF ( IMC .EQ. 1 ) RETURN
        IND   = IDATA(IBLN('HEAD'))
        IF ( IND .LE. 0 ) RETURN
        NRUN  = HDATA(IND+10)
        IF ( (NRUN .LT. 14589) .OR. (NRUN .GT. 15689) ) RETURN
        ISAVE = ISAD
        IF ( ISAVE .EQ. 38 ) ISAD = 40
        IF ( ISAVE .EQ. 40 ) ISAD = 38
        RETURN
C
C---                                     HERE FOR 1981/2 TAGGER
C
C  THE PATTERN IS MUCH SIMPLER !
C
  100  ISAD = IHAD + 1
       RETURN
       END
