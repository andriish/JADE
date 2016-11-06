C   16/03/84 807251613  MEMBER NAME  KOMMAN   (S)        M  FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE KOMMAN
C-----------------------------------------------------------------------
C
C    AUTHOR:   C. BOWDERY   5/06/84 :  COMMAND INTERPRETER FOR GRAPHICS
C
C       MOD:   C. BOWDERY  15/06/84 :  SMALL IMPROVEMENTS
C       MOD:   C. BOWDERY  18/06/84 :  SMALL IMPROVEMENTS
C       MOD:   C. BOWDERY  17/07/85 :  MULTIPLE COMMANDS
C       MOD:   C. BOWDERY  18/07/85 :  MACRO FEATURE ADDED
C       MOD:   C. BOWDERY  22/07/85 :  EXEMAC RENAMED EXPMAC
C       MOD:   C. BOWDERY   5/08/85 :  PROFIL CALLED
C       MOD:   C. BOWDERY   9/08/85 :  ERROR CONDITION IMPROVED
C       MOD:   C. BOWDERY  11/08/85 :  SLIGHT CHANGE IN STRUCTURE
C       MOD:   C. BOWDERY   8/06/86 :  INITIALISATION DONE IN MAIN
C  LAST MOD:   J. HAGEMANN 26/06/87 :  DO NOT REPEAT "WRIT" IF ONLY
C                                      "CR" WAS ENTERED
C
C
C     THIS IS THE COMMAND INTERPRETER FOR THE JADE GRAPHICS PROGRAM.
C
C     NOTE: THIS MEMBER CONTAINS SOME LOWER CASE LETTERS
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL  DSPDTM
      LOGICAL  LERROR
      LOGICAL  LCARET
C
      LOGICAL*1  CHARS, CHARSP, CDEF
C
      COMMON / CGRMAC / MACNR, MACSTA, MACDEP, MACPNT(2,10), CDEF(80,31)
      COMMON / CGRAP2 / BCMD,DSPDTM(30)
      COMMON / CGRAP5 / KOMCAL, LERROR
      COMMON / CGRBUF / CHARS(80),NFIELD,NEXFLD,ISTAFL(80),IENDFL(80),
     +                  CHARSP(80)
*PMF 19/11/99
      CHARACTER CHARS2*80
      EQUIVALENCE (CHARS2,CHARS(1))
*PMF(end)
C
#include "cgraph.for"
C
C-------------------  C O D E  -----------------------------------------
C
C
C                            IF THIS IS THE FIRST CALL,SETTING
C                            LERROR = TRUE INDICATES FIRST EVENT ONLY.
C
      IF( KOMCAL .EQ. 1 ) GO TO 1
        KOMCAL = 1
        LERROR = .TRUE.
        GO TO 10
C
C                            ANY COMMANDS LEFT IN THE BUFFER STILL?
C                            IF YES, GO CONTINUE WITH CURRENT BUFFER
C
   1  IF( NEXFLD .LE. NFIELD ) GO TO 20
C
C                            ANY MACROS OUTSTANDING? MACDEP > 1
C                            IF SO, RESTORE 1 LEVEL OF MACRO
C                            NOTE THAT LEVEL 1 IS THE 1ST CALLING LEVEL
C
      IF( MACDEP .LE. 1 ) GO TO 5
        CALL RESMAC( CHARS, NEXFLD )
        CALL LOCFLD( CHARS, NFIELD, ISTAFL, IENDFL )
        GO TO 1
C
C                            SAVE CURRENT BUFFER IF ALL COMMANDS DONE
C                            CLEAR ANY POSSIBLE ERROR CONDITION
C
   5  DO  8  K = 1,80
        CHARSP(K) = CHARS(K)
   8  CONTINUE
      LERROR = .FALSE.
C                            SEND PROMPTING CHARACTER TO THE SCREEN
C                            AND READ COMMAND CHARACTER STRING
C
  10  WRITE(*,'(''---> '',$)') ! PMF 19/11/99: CALL TRMOUT(4,'--->')
      LCARET = .FALSE.
      call iuwk(0,1)            ! PMF 24/11/99: update workstation
      READ(5,12) CHARS
  12  FORMAT(80A1)
      CALL CLTOU(CHARS2)      ! PMF 19/11/99: Conversion lower -> upper case letters
C
C                            DETERMINE HOW MANY FIELDS (NFIELD) IN THE
C                            COMMAND STRING (CHARS). STORE STARTING
C                            AND ENDING POSITIONS IN ISTAFL & IENDFL
C                            FOR EACH FIELD.
C
  15  NEXFLD = 1
      CALL LOCFLD( CHARS, NFIELD, ISTAFL, IENDFL )
C
      IF( NFIELD .GT. 0 ) GO TO 20
C
C                            ONLY C.R. --> RESTORE THE LAST BUFFER
C                                          EXCEPT IF LAST COMMAND
C                                          HAD ERROR OR IF 1ST COMMAND
        LCARET = .TRUE.
        IF( .NOT. LERROR ) GO TO 17
          WRITE(6,16)
  16      FORMAT(' Error: There is no valid command to repeat.')
          GO TO 10
C
  17    DO  18  K = 1,80
          CHARS(K)  = CHARSP(K)
  18    CONTINUE
        GO TO 15
C
C                            SEPARATE THE NEXT COMMAND IN THE BUFFER
C
  20  CALL SEPCMD( CHARS, NFIELD, ISTAFL, IENDFL, NEXFLD , NEWNEX )
C
C                            DETERMINE WHICH COMMAND
C
      NX  = NEXFLD
      NFL = NEWNEX - NX
C
      CALL JADCMD( CHARS, NFL, ISTAFL(NX), IENDFL(NX), LSTCMD,ACMD,BCMD)
      IF( LSTCMD .NE. 112 .OR. .NOT.LCARET ) GO TO 23
          WRITE(6,22)
  22      FORMAT(' Command WRIT will not be repeated if only CR was ente
     &red!')
          GO TO 10
  23  IF( LSTCMD .GT. 0 ) GO TO 30
C
C                            UNRECOGNISED COMMAND OR ERROR
C
        WRITE(6,25) CHARS
  25    FORMAT(' Input was --> ',80A1/
     +         ' Invalid command string. MENU and HELP give options.')
        MACDEP = 1
        LERROR = .TRUE.
        GO TO 10
C
C                            SET POSITION OF NEXT COMMAND IN BUFFER
C
  30  NEXFLD = NEWNEX + 1
C
C                            IS THIS COMMAND A MACRO?   LSTCMD > MACSTA?
C
      IF( LSTCMD .LE. MACSTA ) RETURN
C
C                            IF SO, EXPAND THIS MACRO INTO CHARS BUFFER
C
        CALL EXPMAC( LSTCMD, CHARS, NEXFLD, IERR )
        IF( IERR .EQ. 0 ) GO TO 15
          LERROR = .TRUE.
          GO TO 10
C
      END
