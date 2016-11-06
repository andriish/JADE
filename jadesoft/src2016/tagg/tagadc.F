C   12/03/84 412051642  MEMBER NAME  TAGADC   (S)           FORTRAN
C
C
C
C
C---   N O T E  :   THIS MEMBER INCLUDES  BLOCK DATA   T A G D A T
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   THIS SUBROUTINE TAKES THE DATA FROM HDATA BLOCK
C   'ATAG' AND PUTS THE ADC CONTENTS IN THE ARRAY CATAG
C                        IN ORDER OF ADC ADDRESS
C
C   NOTE:- IT ALSO MULTIPLIES CHANNEL NUMBER TIMES A FACTOR TO MAKE IT
C   APPROXIMATELY EQUAL TO ENERGY IN MEV
C
C  A.J.FINCH 10/12/92
C       MOD. :  JOHN NYE 29/05/84   TEST FOR INVALID MARK & TIDY-UP
C  LAST MOD. :  JOHN NYE 20/07/84   CHANGED ERROR HANDLING
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
       SUBROUTINE TAGADC(IWRITE,*)
C
C    IWRITE IS A FLAG TO TELL WHETHER TO PRINT THE BANK 'CATAG' AFTER
C    IT HAS BEEN FILLED.
C
       IMPLICIT INTEGER*2 (H)
#include "cdata.for"
C
#include "cgraph.for"
C
#include "comtag.for"
C
#include "cwktag.for"
C
       LOGICAL NOMSG / .FALSE. /
       LOGICAL LERROR  / .FALSE. /
       DIMENSION CALIB(2)
       DATA CALIB / 10.0 , 6.66667 /
       DATA ICOUNT / 0 /
C
C
C------------------------- C O D E -------------------------------------
C
C
C---                                     INITIALISATION
C
C
       IF ( NDDINN .NE. 0 ) NOMSG = .TRUE.
C
       CALL BLOC(IND,'ATAG',0,*1000)
       NW    = IDATA(IND)
       IHIND = IND + IND
       IHSTA = IHIND + 7
       IHSTO = IHIND + NW + NW - 1
C
C---                                     CLEAR THE CATAG ARRAY
C
       DO 10 I =1,64
   10     CATAG(I)= 0.0
C
C---                                     TEST FOR ILLEGAL MARK
C---                                     OR TAGMRK NOT CALLED
C
       IF ( (MARK .LT. 0) .OR. (MARK .GT. 2) ) CALL TAGMRK(*1000)
       IF ( IFLMRK .NE. -1 ) CALL TAGMRK(*1000)
C
C---                                     CHECK FOR RAW DATA
C
       IHRAW = IHIND + 3
       ILOOK = HDATA(IHRAW)
       IF ( ILOOK .LT. 0 ) GOTO 1100
C
C---                                     LOOP OVER ALL DATA IN ATAG
C---                                     AND COPY TO CATAG ARRAY
C
        DO 20 J = IHSTA , IHSTO , 2
C
           IHAD = HDATA(J)
C
C---                                     CONVERT HARDWARE ADDRESSIHAD
C---                                     TO SOFTWARE ADDRESS ISAD
C
           CALL TAGH2S(ISAD,IHAD,*20)
C
           IF ( (ISAD .GT. IENDPZ) .OR. (ISAD .LT. 0) ) GOTO 19
C
C---                                     FILL CATAG ARRAY MEMBER
C---                                     ( SOFTARE ADDRESS ) WITH
C---                                     APPROX ENERGY IN MEV
C
           CATAG(ISAD) = HDATA(J+1) * CALIB(MARK)
C
C---                                     CHECK FOR CRAZY DATA
C
           IF ( (HDATA(J+1).LT.0) .OR. (HDATA(J+1) .GT. 4090) ) GOTO 18

C
           GOTO 20
C
C
C--------------------------------------- ERROR CONDITIONS --------------
C
C
C--------------------------------------- HERE FOR BAD ENERGY
C
   18      CONTINUE
C
           IF ( .NOT. NOMSG ) WRITE(6, 11)ISAD,CATAG(ISAD)
   11  FORMAT(//,' +++  WARNING FROM TAGGING ROUTINE - TAGADC -',
     *                '   CATAG(',I4,') HAS CRAZY VALUE -',F12.3,
     *                ' ---  IT HAS BEEN SET TO ZERO ')
C
C---                                     SET THE ARRAY TO ZERO
C
           CATAG(ISAD) = 0.0
           IF ( .NOT. NOMSG ) LERROR = .TRUE.
           GOTO 20
C
C--------------------------------------- HERE FOR ILLEGAL ADDRESS
C
   19      IF ( NOMSG ) GOTO 20
           LERROR = .TRUE.
           WRITE(6, 21)IHAD,ISAD
   21  FORMAT(' +++ ILLEGAL ADDRESS FOUND IN TAGADC IHAD,ISAD ',2I10)
C
C
C--------------------------------------- END OF LOOP -------------------
C
   20  CONTINUE

C
C
C---                                     O.K. THAT'S THE COMMON FILLED
C---                                     NOW TEST FOR ANY ERRORS
C
       IF ( NOMSG .OR. ( .NOT. LERROR ) )  GOTO 90
       LERROR = .FALSE.
C
C---                                     IF THERE WAS AN ERROR THEN
C---                                     DUMP OUT THE OFFENDING BANK
C
       CALL HPRS('ATAG',0)
C
C---                                     COUNT THE NUMBER OF ERRORS
C
       ICOUNT = ICOUNT + 1
       IF ( ICOUNT .NE. 10 ) GOTO 90
       NOMSG = .TRUE.
       WRITE(6, 31)
   31  FORMAT(///,'+++  MORE THAN TEN ERRORS DETECTED BY TAGADC',
     *            '  -- NO MORE MESSAGES  +++',///)
C
C---                                     RETURN  IF NO DEBUG
C
   90  CONTINUE
       IF ( IWRITE .NE. 1 ) RETURN
C
       WRITE(6,911)CALIB(MARK)
  911  FORMAT(' CATAG ARRAY =  ADC VALUES TIMES ',F6.3)
C
       DO 900 I = 1,IENDPZ,8
          WRITE(6,921) CATAG(I),CATAG(I+1),CATAG(I+2),CATAG(I+3),
     *                 CATAG(I+4),CATAG(I+5),CATAG(I+6),CATAG(I+7)
  900  CONTINUE
  921  FORMAT(8(3X,F10.2))
       RETURN
C
 1000  RETURN 1
C
C----------------------------------------RAW DATA-----------------------
C
 1100  CONTINUE
       CALL TAGRAW(IWRITE,*1000)
       RETURN
       END
C
C
C
C
C
C
C   12/03/84            MEMBER NAME  TAGDAT   (S)           FORTRAN
       BLOCK DATA TAGDAT
C       BLOCK DATA
C
       COMMON / COMTAG / LISTOK,NLIST(64,9,2),
     1                   XMAP(64),YMAP(64)
C
       DIMENSION NDUM(64,4)
       DIMENSION NDIM(64,4)
       EQUIVALENCE (NLIST(1,5,1),NDUM(1,1)),(NLIST(1,1,1),NDIM(1,1))
       DATA NDIM/5,6,5,7,6,3,4*5,3,6,7,5,6,5,
     *           5,6,5,7,6,3,4*5,3,6,7,5,6,5,
     *           5,6,5,7,6,3,4*5,3,6,7,5,6,5,
     *           5,6,5,7,6,3,4*5,3,6,7,5,6,5,
     *           3,4,6,7,4,3,6,7,8,9,10,5,12,11,13,14,
     *           16,15,17,17,20,19,22,20,21,25,26,21,28,31,1,1,
     *           64,64,33,37,36,35,36,36,37,41,42,37,41,42,45,45,
     *           47,47,49,49,60,51,51,52,56,57,58,53,60,58,61,61,
     *           4,3,7,6,7,4,3,4,5,12,13,8,15,10,14,13,
     *           15,16,18,19,23,20,19,21,24,28,29,24,26,32,2,2,
     *           63,63,34,34,39,36,35,37,40,43,45,40,42,43,46,46,
     *           48,48,50,50,52,52,52,53,53,60,61,56,57,59,62,62,
     *           2,1,4,3,8,7,4,5,12,13,14,9,10,13,16,15,
     *           18,17,20,21,24,23,20,23,28,30,30,25,27,29,32,31,
     *           34,33,36,35,40,39,38,39,42,44,46,41,43,45,48,47,
     *           50,49,52,51,55,55,53,55,58,61,62,57,58,64,64,63/
       DATA NDUM/31,32,2,2,9,0,5,12,13,14,0,10,11,15,17,18,
     *           20,20,23,23,25,0,21,25,29,29,0,26,30,27,30,30,
     *           36,36,38,38,41,0,37,41,45,45,0,42,44,47,49,49,
     *           51,51,54,53,56,0,54,57,60,62,0,58,59,63,33,33,
     *           32,31,1,1,12,0,8,9,10,11,0,13,14,16,18,17,
     *           19,19,22,22,28,0,24,28,26,27,0,29,31,26,29,29,
     *           35,35,39,39,44,0,40,44,44,46,0,45,46,48,50,50,
     *           52,52,55,54,57,0,56,60,61,59,0,61,62,64,34,34,
     *           0,5,0,5,2,6*0,15,16,0,12,0,0,21,0,24,18,
     *           6*0,31,32,0,28,0,0,37,0,40,34,6*0,47,47,0,44,0,0,
     *           53,0,55,50,6*0,63,64,0,60,0,3*0,8,8*0,9,6*0,18,
     *           8*0,25,6*0,33,8*0,48,6*0,56,8*0,63,3*0/
      DATA XMAP/216.5,135.5,204.5,123.5,42.5,204.5,123.5,42.5,
     *          56*0.0/
      DATA YMAP/40.5,40.5,121.5,121.5,136.0,202.5,202.5,217.0,
     *          56*0.0/
      END
