C   09/07/85 508061822 MEMBER NAME  HELPUS   (S)           FORTRAN
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
*** PMF 21/10/99:  needed by INQUIRE
      LOGICAL OP
      CHARACTER*44 FNAM    
*** PMF (end)
*** PMF 03/12/99
      CHARACTER*80 CHARS2
      EQUIVALENCE(CHARS2,CHARS(1))
*** PMF(end)
CC
C-------------------  C O D E  -----------------------------------------
C
*** PMF 19/10/99
*      DEFINE FILE 50 ( 200, 1024, E, I1 )
      INQUIRE(UNIT=50,OPENED=OP,NAME=FNAM)
      IF(OP) THEN
         CLOSE(50)
         OPEN(UNIT=50,FILE=FNAM,ACCESS='DIRECT',RECL=1024,
     +        FORM='FORMATTED',STATUS='OLD')
         IREC=1
      ENDIF
* PMF (end)
C
C                            CLRCON CLEARS COMMAND INPUT SCREEN
C
   7  CONTINUE !CALL CLRCON PMF 03/12/99
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
      CALL CLTOU(CHARS2)
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
*** PMF 09/08/99
*     55  READ(50'IREC,60) LEN,(IAR(K),K=1,LEN)
 55   CONTINUE
      IF( OP ) THEN
         READ(50,REC=IREC,FMT=60) LEN,(IAR(K),K=1,LEN)
         IREC=IREC+1
      ENDIF
*** PMF (end)
  60  FORMAT(I4,240A4)
C
C                            CLEAR SCREEN AND WRITE OUT INFO
C
      CONTINUE ! CALL CLRCON PMF 03/12/99
C
  80  WRITE(6,90) (IAR(K),K=1,LEN)
  90  FORMAT(/X,20A4//15(X,20A4/))
C
      WRITE(6,47)
      GO TO 22
      END
