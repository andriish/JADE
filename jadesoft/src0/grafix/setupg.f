C   08/06/86 606092157  MEMBER NAME  SETUPG   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE SETUPG
C-----------------------------------------------------------------------
C
C    AUTHOR:   C. BOWDERY   8/06/86 :  SETS UP DEFAULT GRAPHICS VALUES
C
C
C     SETS UP THE STANDARD VIEW, USER LEVEL STOP SETTING ETC.
C     DEFINES THE GRAPHICS COMMANDS USING DEFCMD. READS PROFILE.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cgraph.for"
C
C------------------  C O D E  -----------------------------------------
C
C                            DEFINE THE STANDARD COMMANDS
C
      CALL DEFCMD
C
C                            READ THE PROFILE IF EXISTING
C
      CALL PROFIL
C
C                            EXECUTE THE DEFAULT SETTINGS MACRO
C
      CALL SETDEF
C
C                            SCALE SETTING
C
      CALL SETSCL(ISTANV)
C
      SXIN   = XMIN
      SXAX   = XMAX
      SYIN   = YMIN
      SYAX   = YMAX
C
      DO  10  I = 1,10
        SSTPS(I) = .FALSE.
        LSTPS(I) = 0
  10  CONTINUE
C
C                            SET THE DEFAULT STOPPING LEVELS
C
      SSTPS(2) = .TRUE.
      SSTPS(6) = .TRUE.
C
      CALL TRMOUT(80,'Stops will be made at the  USER LEVELS  2 and  6.
     +      ^')
      CALL TRMOUT(80,'They can be changed at any time by the command  CS
     +TL or LEVELS.^')
      CALL TRMOUT(80,'Use command "C" for going from LEVEL 2 to LEVEL 6^
     +')
      CALL TRMOUT(80,' ^')
      CALL TRMOUT(80,'Please type in your command when the arrow appears
     +.^')
      CALL TRMOUT(80,'Commands MENU and HELP list the available commands
     +.^')
      CALL TRMOUT(80,' ^')
C
      RETURN
      END
