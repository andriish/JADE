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
#include "cdata.for"
C
C
C FOLLOWING ARE ALL THE COMMONS USED BY TAGGTP ROUTINES
C
#include "comtag.for"
C
#include "cwktag.for"
C
#include "ctaggo.for"
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
     + /,' ++++ T A G A N ++++++++ T A G A N ++++++++ T A G A N ++++',
     + /,' +                                                       +')
  602  FORMAT(
     +   ' +  REAL DATA ',9X,' MARK ',I3,'   TAGGER                +',
     + /,' +                                                       +')
  603  FORMAT(
     +   ' +  MC DATA   ',9X,' MARK ',I3,'   TAGGER SIMULATION     +',
     + /,' +                                                       +')
  605  FORMAT(
     +   ' +                      1979 - 1980                      +')
  606  FORMAT(
     +   ' +                      1981 - 1982                      +')
  607  FORMAT(
     +   ' +                      1983 - 1986                      +')
  608  FORMAT(
     +   ' +                                                       +',
     + /,' +  FORCED  BY  NRUN  =  ',I6,'                          +')
  604  FORMAT(
     +   ' +                                                       +',
     + /,' +++++++++++++++++++++++++++++++++++++++++++++++++++++++++',
     + /,/)
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
