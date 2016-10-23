C   18/12/87 802031945  MEMBER NAME  TPROPT   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPROPT( CTPOPT, COPT, END, IERROR )
C-----------------------------------------------------------------------
C
C     Author:  C. Bowdery       18/12/87:  Read one record with TP opts
C
C
C
C     Routine to read in one record of user options from unit 5
C     and decode them. COPT = 'SPECIAL ' means that a special record
C     such as Event Numbers and Smearing Date has been already processed
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     CTPOPT    Out  CHAR*20      Array of options
C     COPT      Out   CHAR*8      Option group name
C                                 = 'SPECIAL ' if special option record
C     END       Out      L*4      Flag = .TRUE. if no more records
C                                 This is signalled if an EOF is found
C                                 or if a record with TPEND is found.
C     IERROR    Out      I*4      Error code variable from TPDECO
C                                   1 : illegal option group name
C                                   2 : unrecognised record
C                                   3 : extra options found
C                                   4 : no room for new option
C                                   5 : unknown option
C                                   6 : illegal syntax
C                                   7 : bad value
C                                  10 : READ error in this routine
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      PARAMETER  ( NOPT = 20 )

      LOGICAL  END, READER

      INTEGER  IERROR

      CHARACTER   COPT*8, STRING*80, CTPOPT( NOPT )*80

      COMMON / CTPRER / READER

C------------------  C O D E  ------------------------------------------

C                            Read a record from unit 5

      READER = .FALSE.
      END    = .FALSE.
   1  READ(5,5,END=80,ERR=90) STRING
   5  FORMAT( A80 )
      IF( STRING .EQ. ' ' ) GO TO 1

C                            Decode the string

      CALL TPDECO( STRING, CTPOPT, COPT, END, IERROR )

      RETURN

C                            EOF encountered on unit 5

  80  END = .TRUE.
      RETURN

C                            Unexpected read error on unit 5

  90  IERROR =  10

      READER= .TRUE.

      RETURN
      END
