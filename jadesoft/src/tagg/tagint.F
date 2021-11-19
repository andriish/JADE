C   16/08/85 508301807  MEMBER NAME  TAGINT   (S)           FORTRAN
C   12/03/84 412041843  MEMBER NAME  TAGINT   (S)           FORTRAN
C
C
C
C
C
C------  T A G I N T  ------  T A G I N T  ------  T A G I N T  ------
C
C------  T A G I N T  ------  T A G I N T  ------  T A G I N T  ------
C
C------  T A G I N T  ------  T A G I N T  ------  T A G I N T  ------
C
C
C
C
      SUBROUTINE TAGINT(*)
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C         THIS IS THE INITIALISATION ROUTINE THAT SHOULD BE CALLED
C      ONCE BEFORE ANALYZING ANY EVENTS USING THE TAGGING ROUTINES.
C      TO BE PRECISE IT MUST BE CALLED ONCE PER EVENT TO SET UP
C      THE CWORK COMMON AND TO CLEAR OTHER TAGG COMMONS ETC.
C
C      IT DOES A RETURN 1 IF TAGMRK DOES A RETURN 1 ,WHICH IT DOES
C      IF THE 'HEAD' BANK IS MISSING
C
C      MOD : 13/03/84  A.J.FINCH   NO MESSAGES WRITTEN IF  GRAPHICS
C                                     PACKAGE CALLED THIS ROUTINE
C      MOD : 18/06/84  J. NYE      USES IFLMRK --- SEE TAGMRK
C      MOD : 22/06/84  J. NYE      NO OUTPUT
C LAST MOD : A.J.FINCH 16/08/85 INCLUDED SETTING OF ISTMZ ETC., FOR
C                                     GRAPHICS PROGRAM
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
#include "cgraph.for"
C
#include "comtag.for"
C
#include "cwktag.for"
C
C
C-----------------------------------------------------------------------
C
C
C
        DIMENSION IDUM(380)
        EQUIVALENCE (IDUM(1),MARK    )
C
        COMMON / CACLS  /  IACLS( 70)
C
        COMMON / CTAGG0 / ITAGG0( 34)
C
        COMMON / CTAGG1 / ITAGG1( 20)
C
        COMMON / CTAGG2 / ITAGG2(260)
C
C
C
C
C------------------------ C O D E --------------------------------------
C
C
C
C
C---                                     CLEAR ARRAYS
C
C
       CALL VZERO(   IDUM , 380)
       CALL VZERO(  IACLS ,  70)
       CALL VZERO( ITAGG0 ,  34)
       CALL VZERO( ITAGG1 ,  20)
       CALL VZERO( ITAGG2 , 260)
C
C
C---                                     INITIALIZE OTHER VARIABLES
C
C
       NCLST  = 0
       NNEI   = 0
       ISTMZ  = 0
       ISTPZ  = 0
       IENDMZ = 0
       IENDPZ = 0
C
C
C---                                     DECIDE ON DETECTOR MARK
C
C
C   CALL TAGMRK TO CHOOSE 'MARK'
C
C
      CALL TAGMRK(*100)
C
C
C---                                     CREATE NEIGHBOUR LIST IF
C---                                     FIRST EVENT
C
C
C   TAGNEB  GENERATES THE NEIGHBOUR LIST
C   IT NEED ONLY BE CALLED ONCE PER JOB
C   FLAG  LISTOK = 1 AFTERWARDS
C
       IF ( LISTOK .NE. 1 ) CALL TAGNEB
C
C
C---                                     SET UP 'MARK' DEPENDENT VALUES
C
C
C
       IF ( MARK .EQ. 0 ) GOTO 10
       IF ( MARK .EQ. 1 ) GOTO 1
       IF ( MARK .EQ. 2 ) GOTO 2
       WRITE(6,6999)MARK
6999   FORMAT('  TAGINT: ILLEGAL VALUE OF MARK: ',I10,/,
     1'                   SHOULD BE 0,1 OR 2 ')
      RETURN
C
C
C----------------------------------------MARK = 0  1979/80 DATA
C
C
   10  CONTINUE
       ISTMZ     =  1
       ISTPZ     = 97
       IENDMZ    = 96
       IENDPZ    = 192
C
       RETURN
C
C
C----------------------------------------MARK = 1  1981/82 DATA
C
C
    1  CONTINUE
       ISTMZ     =  1
       ISTPZ     = 33
       IENDMZ    = 32
       IENDPZ    = 64
C
       RETURN
C
C
C----------------------------------------MARK = 2  1983 -> DATA
C
C
    2  CONTINUE
       ISTMZ     =  1
       ISTPZ     = 25
       IENDMZ    = 24
       IENDPZ    = 48
C
       RETURN
C
C
C----------------------------------------R E T U R N   1----------------
C
C
C---                                     HERE IF TAGMRK DID RETURN 1
C
C
  100  RETURN 1
       END
