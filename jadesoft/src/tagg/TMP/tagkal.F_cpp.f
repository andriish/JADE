C   12/03/84 503141427  MEMBER NAME  TAGKAL   (S)           FORTRAN
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C   THIS SUBROUTINE RECALIBRATES ADC'S USING FACTORS IN
C  COMMON CALIBR
C
C INPUT - IWRITE - IF THIS IS EQUAL TO ONE THE ROUTINE
C                  WRITES OUT SOME DEBUGGING INFORMATION
C
C  A.J.FINCH 6/10/83
C  LAST MOD :   J. NYE  24/05/83  TIDIED UP
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
       SUBROUTINE TAGKAL(IWRITE)
C
C
       IMPLICIT INTEGER * 2 (H)
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
C            MACRO CALIBR .... JADE CALIBRATION DATA COMMON
C----------------------------------------------------------------------
      COMMON/CALIBR/ ACALIB(1000)
                     DIMENSION HCALIB(100),ICALIB(100)
                     EQUIVALENCE(ACALIB(1),HCALIB(1),ICALIB(1))
C------------------------ END OF MACRO CALIBR -------------------------
C
       DIMENSION FACTOR(64)
C
       DATA ICOUNT / 0 /
       DATA IDONT  / 0 /
C
C
C------------------------- C O D E -------------------------------------
C
C
C---                                     TEST FOR MONTE CARLO DATA
C---                                     - DONT CALIBRATE IF MC
C
       IF ( IMC .EQ. 1 ) GOTO 15
C
C
C===DEBUG  START=========================DEBUG  START===================
C
       IF ( IWRITE .NE. 1 ) GOTO 9
          WRITE(6,610)
          WRITE(6,605 ) ( CATAG(I),I=1,IENDPZ )
  610     FORMAT(1X,'CATAG ARRAY BEFORE RECALIBRATION ')
C
C===DEBUG  END===========================DEBUG  END=====================
C
C
C
C
C---                                     GET POINTER TO CALIBRATION
C---                                     CONSTANTS IN COMMON/CALIBR/
C
    9  IPOINT = 2 * ICALIB(12)
C
C---                                    ON FIRST CALL ONLY - CHECK
C---                                     CALIB CONSTANTS ARE SENSIBLE
C
       IF ( ICOUNT .EQ. 0 ) GOTO 8
       ICOUNT = 1
C
C                                        THIS LOOP ON FIRST EVENT ONLY
C
       DO 108 I = ISTMZ,IENDPZ
          J = IPOINT + I
          FACT = 0.01 * HCALIB(J)
          IF (  (FACT .LT. 0) .OR. (FACT .GT. 10.0) ) IDONT = 1
  108  CONTINUE
C
       IF ( IDONT .EQ. 1 ) WRITE(6,608)
  608  FORMAT(/,/,' WARNING FROM TAGGING ANALYSIS ROUTINE -TAGKAL-',
     *          /,' ****   ILLEGAL CALIBRATION CONSTANT',
     *               ' FOUND ON FIRST CALL   ****')
C
C
C--------------------------------------- READ FACTORS FROM CALIBR
C
    8  CONTINUE
       IF ( IDONT .EQ. 1 ) GOTO 11
C
C
       DO 10 I = ISTMZ,IENDPZ
          J = IPOINT + I
          CATAG (I) = 0.01 * HCALIB(J) * CATAG(I)
          FACTOR(I) = 0.01 * HCALIB(J)
   10  CONTINUE
C
   11  CONTINUE
       IF ( IWRITE .NE. 1 ) RETURN
C
C
C
C
C--------------------------------------- D E B U G   O N L Y -----------
C
C
C
       IF ( IDONT .EQ. 0 ) WRITE(6,600)
       IF ( IDONT .EQ. 0 ) WRITE(6,605 ) ( FACTOR(I),I=1,IENDPZ )
       IF ( IDONT .EQ. 1 ) WRITE(6,607)
       IF ( IDONT .EQ. 1 ) WRITE(6,611)
       WRITE(6,606)
       WRITE(6,605 ) ( CATAG(I),I=1,IENDPZ )
  600  FORMAT(' CALIBRATION CONSTANTS IN TAGKAL ')
  606  FORMAT(' CATAG ARRAY AFTER RECALIBRATION ')
  605  FORMAT(6(/,8(2X,F8.2) ) )
  607  FORMAT(/,' MESSAGE FROM TAGKAL --- NO RECALIBRATION OF TAGGING',
     *             ' DATA IS BEING PERFORMED ')
  611  FORMAT('  BECAUSE ILLEGAL CALIBRATION CONSTANTS WERE FOUND',
     *           ' IN FIRST EVENT')
C
       RETURN
C
C
C
C--------------------------------------- MONTE CARLO DEBUG -------------
C
C
15    CONTINUE
      IF ( IWRITE .EQ. 1 ) WRITE(6,607)
      IF ( IWRITE .EQ. 1 ) WRITE(6,612)
 612  FORMAT('  BECAUSE THIS IS MONTE CARLO DATA ')
      RETURN
      END
