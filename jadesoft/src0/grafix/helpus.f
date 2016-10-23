C   09/07/85 508061822 MEMBER NAME  HELPUS   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE HELPUS
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY   9/07/85 :  GIVE HELP IN GRAPHICS
C
C LAST MOD:   C. BOWDERY   3/08/85 :  GIVE HELP FOR MACROS NOW
C
C
C     DETERMINES THE RECORD NUMBER ON THE DIRECT ACCESS DATASET WHICH
C     CONTAINS THE HELP INFORMATION FOR THE REQUIRED ITEM. THIS IS
C     THEN READ AND THE CONTENTS SENT TO THE SCREEN. MACROS ARE
C     TREATED DIFFERENTLY (DISPLAY DEFINITION FROM COMMON).
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL*1  CHARS, CQUEST
      LOGICAL*1  CDEF
C
      COMMON / CGRMAC / MACNR, MACSTA, MACDEP, MACPNT(2,10), CDEF(80,31)
C
      COMMON / CWORK3 / CHARS(80), ISTAFL(80), IENDFL(80), IAR(240)
C
      DATA  CQUEST / '?' /
C
C-------------------  C O D E  -----------------------------------------
C
      DEFINE FILE 50 ( 200, 1024, E, I1 )
C
C                            CLRCON CLEARS COMMAND INPUT SCREEN
C
   7  CALL CLRCON
C
C                            WHAT HELP IS WANTED?
C
      CALL HELPLI
  20  WRITE(6,30)
  30  FORMAT(/' HELP:  Enter name of item you want help for ',
     + '    (or press ENTER to end HELP)')
C
C                            FURTHER HELP LOOP BEGINS HERE
C
  22  READ(5,40) CHARS
  40  FORMAT(80A1)
C
C                            WHICH RECORD NUMBER HAS THE INFO
C
      CALL LOCFLD( CHARS, NFIELD, ISTAFL, IENDFL )
      IF( NFIELD .EQ. 0 ) RETURN
      LEN = IENDFL(1) - ISTAFL(1) + 1
      CALL FINABR( CHARS(ISTAFL(1)), LEN, IREC, MAXARG, IS )
      IF( IREC .GT. 0 ) GO TO 50
      IF( CHARS(ISTAFL(1)) .EQ. CQUEST  .AND.  LEN .EQ. 1
     +    .AND.  NFIELD .EQ. 1                           ) GO TO 7
C
      WRITE(6,45)
  45  FORMAT(' Sorry. No information available about that.'/)
      WRITE(6,47)
  47  FORMAT(' HELP:  Enter name of next item   or   ? (= List)',
     + '   or press ENTER (= end HELP)')
      GO TO 22
C
C                            COMMAND OR MACRO? MACROS START AT MACSTA
C
  50  IF( IREC .LT. MACSTA ) GO TO 55
        CALL HLPMAC( IREC, CHARS(ISTAFL(1)), LEN )
        WRITE(6,47)
        GO TO 22
C                            GET RECORD FROM DIRECT ACCESS DATASET
C
  55  READ(50'IREC,60) LEN,(IAR(K),K=1,LEN)
  60  FORMAT(I4,240A4)
C
C                            CLEAR SCREEN AND WRITE OUT INFO
C
      CALL CLRCON
C
  80  WRITE(6,90) (IAR(K),K=1,LEN)
  90  FORMAT(/X,20A4//15(X,20A4/))
C
      WRITE(6,47)
      GO TO 22
      END
