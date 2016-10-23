C   12/03/84 504221644  MEMBER NAME  TAGMRK   (S)           FORTRAN
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
       SUBROUTINE TAGMRK(*)
C
C
C  AUTHOR   :  A. J. FINCH
C
C       MOD :  J. M. NYE  - DOESNT RETURN 1 IF ATAG BANK NOT THERE
C                           - ALSO USES SMEARING DATE FOR MC DATA IF
C                             NO ATAG BANK.                 24/05/84
C
C  LAST MOD :  18/06/84  J. NYE  - SETS IFLMRK = -1 WHEN CALLED
C
C
C
C THIS ROUTINE SETS THE VALUE OF 'MARK' IN CWORK TO INDICATE WHICH
C TAGGER THIS IS - VALUE OF MARK          TAGGER
C                  =============          ======
C                       1                 1981/2
C                       2                 1983...
C
C IT DOES A RETURN 1 IF 'HEAD'  BANK DOESNT EXIST
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
       IMPLICIT INTEGER*2 (H)
#include "cdata.for"
C
#include "cwktag.for"
C
#include "cgraph.for"
C
       LOGICAL FIRST / .TRUE. /
C
C
C-------------------------- C O D E ------------------------------------
C
C
C
C---                                     GET HEAD AND ATAG POINTERS
C
C
       INHEAD = IDATA( IBLN('HEAD') )
       INATAG = IDATA( IBLN('ATAG') )
       IF ( INHEAD .LE. 0 ) GOTO   1
C
C---                                     GET HEAD BANK DATA
C
       IHHEAD = INHEAD + INHEAD
       NRUNTG = HDATA(IHHEAD + 10)
       NDATE  = HDATA(IHHEAD +  8)
C
       IMC = 0
       IF ( NRUNTG .GT. 0 ) GOTO 200
C
C
C--------------------------------------- MONTE CARLO DATA ONLY ---------
C
C---                                     NRUNTG = 0 => MONTE CARLO DATA
C---                                     SET FLAG FOR TAGKAL ETC.
C
       IF ( NRUNTG .EQ. 0 ) IMC = 1
C
C---                                     IF ATAG BANK EXISTS THEN USE
C---                                     WORD SET BY MC SIMULATION
C
C
       IF ( INATAG .LE. 0 ) GOTO   2
       MCMARK = HDATA( 2 * INATAG + 2 )
       IF ( MCMARK .EQ. 0 ) NRUNTG =  5000
       IF ( MCMARK .EQ. 1 ) NRUNTG = 10000
       IF ( MCMARK .EQ. 2 ) NRUNTG = 15000
       GOTO 200
C
C---                                     ELSE USE SMEARING DATE
C
    2  CONTINUE
       NRUNTG = 10000
       IF ( NDATE .LE. 1980 ) NRUNTG =  5000
       IF ( NDATE .GE. 1983 ) NRUNTG = 15000
C
C
C--------------------------------------- FOR BOTH REAL AND MC DATA -----
C
C
  200  CONTINUE
C
C
C---                                     SET VALUE OF 'MARK'
C
       IF   (NRUNTG.LT. 6000)                           MARK = 0
       IF ( (NRUNTG.GE. 6000) .AND. (NRUNTG.LT.12948) ) MARK = 1
       IF   (NRUNTG.GE.12948)                           MARK = 2
C
C
C---                                     SET IFLMRK = -1 TO FLAG THAT
C---                                     TAGMRK HAS BEEN CALLED
C
       IFLMRK = -1
       FIRST  = .FALSE.
       RETURN
C
C---------------------------------------- R E T U R N   1 --------------
C
C                                        RETURN 1 IF NO HEAD BANK
C
    1  RETURN 1
       END
