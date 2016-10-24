C   05/08/85 508061824 MEMBER NAME  PROFIL   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE PROFIL
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY   5/08/85 :  RENAME AN EXISTING MACRO
C
C
C     THIS ROUTINE READS A USERS PROFILE (DATASET CONTAINING MACROS)
C     AND DEFINES THE MACROS FOR THE CURRENT SESSION.
C
C     PROFILE NAME MUST BE:  USERID.GRAPHICS.PROFILE.MACROS
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      CHARACTER*1  CHARS, CNAME(6), CID(16)
      CHARACTER*1  CDEF
      CHARACTER*8  DDN, DNAME(6)
C
      COMMON / CGRMAC / MACNR, MACSTA, MACDEP, MACPNT(2,10), CDEF(80,31)
C
      COMMON / CWORK3 / CHARS(80), ISTAFL(80), IENDFL(80)
C
      EQUIVALENCE  ( CNAME(1), DNAME(1) )
C

      DATA  DNAME / 'F11LHO.G', 'RAPHICS.', 'PROFILE.',
     +              'MACROS  ', '        ', '        ' /
*** PMF 20/11/99
      CHARACTER*80 CHARS2,CDEF2(31)
      EQUIVALENCE (CHARS2,CHARS(1)),(CDEF2(1),CDEF(1,1))
*** PMF (end)
C
C------------------  C O D E  ------------------------------------------
C
      CALL JOBNAM(CID)
C
C                            COPY FIRST 6 LETTERS OF ID INTO DNAME(1)
C
      DO  1  I = 1,6
        CNAME(I) = CID(I)
   1  CONTINUE
C
      LUPROF = 0
      CALL LINKDS(LUPROF,DNAME,HERR,DDN)
C
C                            ERROR RETURN ==> PROBABLY NO PROFILE
C
      IF( HERR .NE. 0 ) RETURN
C
C                            READ IN THE PROFILE MACROS
C
   5  READ(LUPROF,10,END=999) CHARS
  10  FORMAT(80A1)
      CALL CLTOU(CHARS2) ! PMF 20/11/99
C
      IF( MACNR .LT. 30 ) GO TO 20
       WRITE(6,12)
  12   FORMAT(' 30 PROFILE macros already defined. Processing aborted.')
       RETURN
C                            FIND THE NAME
C
  20  CALL LOCFLD( CHARS, NFIELD, ISTAFL, IENDFL )
      IF( NFIELD .EQ. 0 ) RETURN
C
C                            CORRECT LENGTH AND ONLY ONE?
C
      LEN = IENDFL(1) - ISTAFL(1) + 1
      IF( NFIELD .EQ. 1  .AND.  LEN .LE. 8 ) GO TO 60
        WRITE(6,50)
  50    FORMAT(' Bad MACRO NAME found while processing PROFILE.',
     +         ' Processing aborted.')
        RETURN
C
C                            IS THE NAME UNIQUE?
C
  60  CALL FINABR( CHARS(ISTAFL(1)), LEN, IREC, MAXARG, IS )
      IF( IREC .LE. 0 ) GO TO 70
        WRITE(6,65)
  65    FORMAT(' PROFILE MACRO NAME conflicts with a',
     +         ' COMMAND NAME. MACRO ignored.')
        READ(LUPROF,10,END=999)
        GO TO 5
C
C                            READ DEFINITION
C
  70  IMAC = MACNR + 1
C
      READ(LUPROF,10,END=999) (CDEF(K,IMAC),K=1,80)
      CALL CLTOU(CDEF2(IMAC)(1:80)) ! PMF 20/11/99
C
C                            NULL LINE?
C
      CALL LOCFLD( CDEF(1,IMAC), NFIELD, ISTAFL, IENDFL )
      IF( NFIELD .GT. 0 ) GO TO 100
        WRITE(6,95)
  95    FORMAT(' PROFILE macro definition aborted. No macro defined.')
        GO TO  5
C
C
C                            ADD NAME TO COMMAND LIST
C                            COMMAND NUMBER IS MACRO NUMBER + BASE NO.
C
 100  MACNR  = MACNR + 1
      NUMCMD = MACNR + MACSTA
      CALL SETCMD( NUMCMD, CHARS(ISTAFL(1)), 0, 0,0,0 )
C
      GO TO 5
C
 999  RETURN
      END
