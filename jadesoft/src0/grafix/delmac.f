C   05/08/85 508051152  MEMBER NAME  DELMAC   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE DELMAC
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY   5/08/85 :  DELETE AN EXISTING MACRO
C
C
C     THIS ROUTINE PROMPTS FOR A MACRO NAME, WHICH IF EXISTING, IS
C     DELETED AFTER CONFIRMATION FROM THE USER.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL*1  CCMD
      LOGICAL*1  CHARS
      LOGICAL*1  CDEF
C
      COMMON / CWORK  / NFIELD, CHARS(80), ISTAFL(80), IENDFL(80)
      COMMON / CGRMAC / MACNR, MACSTA, MACDEP, MACPNT(2,10), CDEF(80,31)
      COMMON / CGRAP4 / IPOS4,CCMD(8,200),HCMD(5,200)
C
C------------------  C O D E  ------------------------------------------
C
   5  WRITE(6,10)
  10  FORMAT(' Please enter the NAME of the macro to be deleted:')
C
      READ(5,15) CHARS
  15  FORMAT(80A1)
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
  60  CALL FINABR( CHARS(ISTAFL(1)), LEN, ICMD, MAXARG, IS )
      IMAC = ICMD - MACSTA
      IF( IMAC .GT. 0 ) GO TO 70
        WRITE(6,65)
  65    FORMAT(' No macro with that name exists.',
     +         ' Try again or press ENTER to end DELMAC')
        GO TO 5
C
C                            DISPLAY DEFINITION
C
  70  WRITE(6,80) (CDEF(K,IMAC),K=1,80)
  80  FORMAT(' The following definition currently exists for this',
     +       ' macro:'/
     +       1X,80A1/' Do you really want to delete this macro? ',
     +               '  ( YES or <NO> )')
C
      CALL DECIDE( IANSW )
C
      IF( IANSW .EQ. 1 ) GO TO 100
        WRITE(6,95)
  95    FORMAT(' Macro not deleted.')
        RETURN
C
C                            DELETE MACRO ENTRY
C
 100  CALL LOCMAC( ICMD, LOCAT )
      IF( LOCAT .GT. 0 ) GO TO 110
        WRITE(6,105)
 105    FORMAT(' SYSTEM ERROR in DELMAC. Call the GRAPHICS EXPERT!')
        RETURN
C
 110  WRITE(6,120) ( CCMD(K,LOCAT),K=1,8)
 120  FORMAT(' Macro ',8A1,' deleted.')
C
C                            REMOVE ENTRY IN THE COMMAND LIST
C
      IPOS4 = IPOS4 - 1
      MACNR = MACNR - 1
      IF( LOCAT .GT. IPOS4 ) RETURN
      DO  130  I = LOCAT,IPOS4
C
        DO  123  J = 1,8
          CCMD(J,I) = CCMD(J,I+1)
 123    CONTINUE
C
        DO  127  J = 1,5
          HCMD(J,I) = HCMD(J,I+1)
 127    CONTINUE
C
 130  CONTINUE
C
C                            REMOVE ENTRY IN THE MACRO DEFINITION LIST
C
      DO  140  I = IMAC,MACNR
        DO  135  J = 1,80
          CDEF(J,I) = CDEF(J,I+1)
 135    CONTINUE
 140  CONTINUE
C
      RETURN
      END
