C   03/08/85 508052020  MEMBER NAME  RENMAC   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE RENMAC
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY   5/08/85 :  RENAME AN EXISTING MACRO
C
C
C     THIS ROUTINE PROMPTS FOR A MACRO NAME, DISPLAYS THE MACRO
C     DEFINITION AND ALLOWS THE USER TO RENAME THE MACRO.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      character*1  CHARS
      character*1  CBLNK
      character*1  CDEF
      character*1  CCMD
C
      COMMON / CGRAP4 / IPOS4,CCMD(8,200),HCMD(5,200)
      COMMON / CWORK  / NFIELD, CHARS(80), ISTAFL(80), IENDFL(80)
      COMMON / CGRMAC / MACNR, MACSTA, MACDEP, MACPNT(2,10), CDEF(80,31)
C
      DATA  CBLNK / ' ' /
*** PMF 03/12/99
      CHARACTER*80 CHARS2
      EQUIVALENCE(CHARS2,CHARS(1))
*** PMF(end)
C
C------------------  C O D E  ------------------------------------------
C
   5  WRITE(6,10)
  10  FORMAT(' Please enter the NAME of the macro to be renamed')
      READ(5,40) CHARS
  40  FORMAT(80A1)
      CALL CLTOU(CHARS2) ! PMF 03/12/99
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
C                            DOES THE MACRO EXIST?
C
  60  CALL FINABR( CHARS(ISTAFL(1)), LEN, ICMD, MAXARG, IS )
      IMAC = ICMD - MACSTA
      IF( IMAC .GT. 0 ) GO TO 70
        WRITE(6,65)
  65    FORMAT(' No macro with that name exists.',
     +         ' Try again or press ENTER to end RENAMAC')
        GO TO 5
C
C                            DISPLAY DEFINITION
C
  70  WRITE(6,80) (CDEF(K,IMAC),K=1,80)
  80  FORMAT(' The following definition currently exists for this',
     +       ' macro:'/
     +       1X,80A1/' Please enter NEW NAME    or just press ',
     +               'ENTER to retain old name')
C
  85  READ(5,90) CHARS
  90  FORMAT(80A1)
      CALL CLTOU(CHARS2) ! PMF 03/12/99
C
C                            NULL LINE?
C
      CALL LOCFLD( CHARS, NFIELD, ISTAFL, IENDFL )
      IF( NFIELD .GT. 0 ) GO TO 100
        WRITE(6,95)
  95    FORMAT(' Old name retained.')
        RETURN
C
C                            CORRECT LENGTH AND ONLY ONE?
C
 100  LEN = IENDFL(1) - ISTAFL(1) + 1
      IF( NFIELD .EQ. 1  .AND.  LEN .LE. 8 ) GO TO 160
        WRITE(6,50)
        GO TO 85
C
C                            IS THE NEW NAME UNIQUE?
C
 160  CALL FINABR( CHARS(ISTAFL(1)), LEN, ICMD1, MAXARG, IS )
      IMAC1 = ICMD1 - MACSTA
      IF( ICMD1 .LE. 0 ) GO TO 170
      IF( IMAC1 .NE. IMAC ) GO TO 164
        WRITE(6,95)
        RETURN
C
 164    WRITE(6,165)
 165    FORMAT(' This name is already in use.',
     +         ' Try again or press ENTER to end RENAMAC')
        GO TO 85
C
 170  CALL LOCMAC( ICMD, LOCAT )
      IF( LOCAT .GT. 0 ) GO TO 175
        WRITE(6,172)
 172    FORMAT(' SYSTEM ERROR in RENMAC. Call the GRAPHICS EXPERT!')
        RETURN
C
 175  KK1 = ISTAFL(1)
      KK2 = IENDFL(1)
      WRITE(6,180) (CCMD(K,LOCAT),K=1,8), (CHARS(KK),KK=KK1,KK2)
 180  FORMAT(' Macro ',8A1,' renamed to ',8A1)
C
      DO  190  I = 1,8
        IP = KK1 + I - 1
        IF( IP .GT. 80 ) GO TO 185
          CCMD(I,LOCAT) = CHARS( IP )
          GO TO 190
C
 185      CCMD(I,LOCAT) = CBLNK
C
 190  CONTINUE
C
      RETURN
      END
