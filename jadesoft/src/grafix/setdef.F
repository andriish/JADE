C   08/06/86 606101931  MEMBER NAME  SETDEF   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE SETDF ! PMF 18/11/99: SETDEF changed to SETDF
C-----------------------------------------------------------------------
C
C    AUTHOR:   C. BOWDERY  10/06/86 :  SETS UP DEFAULT VIEW FROM PROFILE
C
C
C     READS THE MACRO "DEFVIEW" AND SETS UP THE DEFAULT VIEW IT CONTAINS
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      character*8    DCHARS, DVIEW
      character*1 CVIEW(8), CBLANK, CHARS(80)
C
#include "cgraph.for"
C
      COMMON / CSTANV / HDISPN, HCSTV2, HCSTV3
C
      DIMENSION  ISTAFL(80), IENDFL(80)
C
      EQUIVALENCE  ( DCHARS, CHARS(1) )
      EQUIVALENCE  ( DVIEW,  CVIEW(1) )
C
      DATA  DCHARS    / 'DEFVIEW ' /
      DATA  DVIEW     / 'RB      ' /
C
      DATA  CBLANK    / ' '  /
      DATA  ISTAFL(1) /  1   /
      DATA  IENDFL(1) /  7   /
      DATA  NFL       /  1   /
C
C------------------  C O D E  -----------------------------------------
C
C                            SET UP INTERNAL DEFAULT VIEW
C
      ISTANV = 2
C
C                            IS THERE A MACRO CALLED DEFVIEW?
C
      CALL JADCMD( CHARS, NFL, ISTAFL, IENDFL, LCOM, A, B )
C
      IF( LCOM .EQ. 0 ) GO TO 15
C
C                            EXPAND THIS MACRO INTO CHARS BUFFER
C
        NEXFLD = 8
        CALL EXPMAC( LCOM, CHARS, NEXFLD, IERR )
        IF( IERR .NE. 0 ) GO TO 10
C
C                            LOCATE THE FIELDS IN THE MACRO COMMAND
C
        CALL LOCFLD( CHARS, NFIELD, ISTAFL, IENDFL )
        IF( NFIELD .GT. 2 ) GO TO 10
C
C                            IF ONLY ONE OR TWO FIELDS, DECODE THEM
C
        CALL JADCMD( CHARS, NFIELD, ISTAFL, IENDFL, LCOM, A, B )
C
        IF( LCOM .LT. 1  .OR.  LCOM. GT. 30 ) GO TO 10
          ISTANV = LCOM
          IF( A .NE. 0.0 ) HCSTV3 = 1
C
C                            COPY THE NAME OF THE VIEW INTO CVIEW
C
          LEN = IENDFL(1) - ISTAFL(1) + 1
          DO  5  I = 1,8
            IF( I .LE. LEN ) GO TO 3
              CVIEW(I) = CBLANK
              GO TO 5
C
   3          CVIEW(I) = CHARS( ISTAFL(1) + I - 1 )
   5      CONTINUE
C
C                            RESTORE THE MACRO TO CLEAN THE STACK
C
  10  CALL RESMAC( CHARS, NEXFLD )
C
C                            SET LAST VIEW TO CURRENT VIEW
C
  15  LASTVW = ISTANV
C
      WRITE(6,20) CVIEW
  20  FORMAT(' Standard view is ',8A1/
     +       ' It can be changed by selecting a new view'/
     +       ' and making it the standard using the command CSTV 1'/)
C
      RETURN
      END
