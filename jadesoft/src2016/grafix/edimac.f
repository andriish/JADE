C   03/08/85 508032008 MEMBER NAME  EDIMAC   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE EDIMAC
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY   3/08/85 :  EDIT AN EXISTING MACRO
C
C
C     THIS ROUTINE PROMPTS FOR A MACRO NAME, DISPLAYS THE MACRO
C     DEFINITION AND ALLOWS THE USER TO REDEFINE THE MACRO.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL*1  CHARS
      LOGICAL*1  CDEF
C
      COMMON / CWORK  / NFIELD, CHARS(80), ISTAFL(80), IENDFL(80)
      COMMON / CGRMAC / MACNR, MACSTA, MACDEP, MACPNT(2,10), CDEF(80,31)
*** PMF 03/12/99
      CHARACTER*80 CHARS2
      EQUIVALENCE(CHARS2,CHARS(1))
*** PMF(end)
C
C------------------  C O D E  ------------------------------------------
C
   5  WRITE(6,10)
  10  FORMAT(' Please enter the NAME of the macro to be edited')
      READ(5,40) CHARS
  40  FORMAT(80A1)
      CALL CLTOU(CHARS2)
C
C                            FIND THE NAME
C
      CALL LOCFLD( CHARS, NFIELD, ISTAFL, IENDFL )
      IF( NFIELD .EQ. 0 ) RETURN
C
C                            CORRECT LENGTH AND ONLY ONE?
C
      LEN = IENDFL(1) - ISTAFL(1) + 1
      IF( NFIELD .EQ. 1  .AND.  LEN .LE. 8 ) GO TO 60
        WRITE(6,50)
  50    FORMAT(' Sorry. Only one name of 8 letters or less and no ";"',
     +         '. Please try again')
        GO TO 5
C
C                            IS THE NAME UNIQUE?
C
  60  CALL FINABR( CHARS(ISTAFL(1)), LEN, IREC, MAXARG, IS )
      IMAC = IREC - MACSTA
      IF( IMAC .GT. 0 ) GO TO 70
        WRITE(6,65)
  65    FORMAT(' No macro with that name exists.',
     +         ' Please try again or press ENTER to end EDITMAC')
        GO TO 5
C
C                            DISPLAY DEFINITION
C
  70  WRITE(6,80) (CDEF(K,IMAC),K=1,80)
  80  FORMAT(' The following definition currently exists for this',
     +       ' macro:'/
     +       1X,80A1/' Please enter new definition    or just press ',
     +               'ENTER to retain old definition')
C
      READ(5,90) CHARS
  90  FORMAT(80A1)
      CALL CLTOU(CHARS2)        ! PMF 03/12/99
C
C                            NULL LINE?
C
      CALL LOCFLD( CHARS, NFIELD, ISTAFL, IENDFL )
      IF( NFIELD .GT. 0 ) GO TO 100
        WRITE(6,95)
  95    FORMAT(' Old definition retained.')
        RETURN
C
C
C
 100  DO  110  I = 1,80
        CDEF(I,IMAC) = CHARS(I)
 110  CONTINUE
C
      WRITE(6,120)
 120  FORMAT(' New definition accepted.')
C
      RETURN
      END
