C   07/02/83 409202146 MEMBER NAME  DSERR    (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE DSERR(HERR,JERR,DDN,*)
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY    ??/??/81 : HANDLES GETPDD ERRORS
C
C   LAST MOD: C. BOWDERY    20/09/84 : NEW ERROR MESSAGE
C
C     HERR      LINKDS/GETPDD ERROR NUMBER (INTEGER*2)
C     JERR      ERROR COUNT
C     DDN       DDNAME (REAL*8) FROM LINKDS
C     *         RETURN FOR ANOTHER LINK ATTEMPT
C               NORMAL RETURN FOR TOO MANY ERRORS (3)
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      REAL*8 DDN
C
C------------------  C O D E  ------------------------------------------
C
C                  IF HERR IS NEGATIVE = ERROR FREEING A DDNAME
C
      JUSCRN = 6
      IF( HERR .GT. 0 ) GO TO 15
        HERROR = - HERR
        WRITE(JUSCRN,17) HERROR,DDN
 17     FORMAT(' ****  RUN TIME PROGRAM ERROR  ++++  FATAL  ****'/
     +  ' Error ',I5,' occurred while freeing ',A8,' in S/R LINKDS')
            RETURN
C
C                  HANDLE THE OTHER GETPDD ERRORS.
C
 15   JERR = JERR + 1
      IF( HERR .NE. 100 ) GO TO 20
        IF( JERR .GT. 1 ) RETURN
          WRITE(JUSCRN,21)
 21       FORMAT(' No name given. Enter a name or <ENTER> to terminate')
          RETURN 1
C
 20   IF( HERR .EQ. 860  .OR.
     +    HERR .GE. 5888 .AND. HERR .LE. 6143 ) HERR = 3
      IF( HERR .EQ. 3 ) WRITE(JUSCRN,5)
 5      FORMAT(' Dataset non-existent  or  name wrongly entered')
      IF( HERR .EQ. 536 ) WRITE(JUSCRN,18)
 18     FORMAT(' Dataset migrated to tape. "FAST" it or')
C
      IF( HERR .EQ. 528 ) WRITE(JUSCRN,26)
 26     FORMAT(' Dataset already in use.Try later')
C
      IF( HERR .EQ. 1   ) WRITE(JUSCRN,25)
 25     FORMAT(' All DDNAMEs between FT11F001 and FT60F001 have been use
     +d'/' Use the NEWLIB FREE command to release one and try again')
C
      IF( HERR .EQ. 584 ) WRITE(JUSCRN,30)
 30     FORMAT(' The dataset is on an MSS volume that cannot be',
     +  ' mounted at the moment.'/
     +  ' This happens occasionally. Don''t panic (yet)!')
C
      IF( HERR .GT. 3  .AND.  HERR .NE. 528  .AND.  HERR .NE. 536
     +    .AND.  HERR .NE. 584 ) WRITE(JUSCRN,19)HERR
 19     FORMAT(' Dataset Allocation Error ',I5,' in GETPDD called by',
     +         ' LINKDS'/' I don''t know what caused it.',
     +         ' Try the dataset again later is my advice')
      IF( JERR .GT. 2  .OR.  HERR .EQ. 1 ) RETURN
        WRITE(JUSCRN,22)
 22     FORMAT(' Enter another name or press <ENTER> to terminate ....')
        RETURN 1
        END
