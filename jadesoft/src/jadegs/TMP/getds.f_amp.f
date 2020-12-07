C   13/03/84 403140007  MEMBER NAME  GETDS    (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
         SUBROUTINE  GETDS(LUNC,MESS,INAME,HERR)
C-----------------------------------------------------------------------
C
C   AUTHOR:  C. BOWDERY    ??/??/81 : OUTPUTS MESSAGE AND LINKS A DS
C
C        THIS SUBROUTINE OUTPUTS MESSAGE MESS AND READS IN A
C        DSNAME AND LINKS IT TO LUNC USING S/R LINKDS
C
C            INAME   -   ARRAY OF LENGTH 11 CONTAINING DSNAME
C            LUNC    -   VARIABLE HOLDING LOGICAL UNIT NO.
C            HERR    -   ERROR VARIABLE
C            MESS    -   MESSAGE ARRAY TERMINATED WITH ^
C
C        IF LUNC=0 ON ENTRY, THE VALUE OF LUNC ON EXIT WILL BE
C        DECIDED BY SUBROUTINE GETPDD (FROM R02SCH.TSOIPS.LOAD).
C        IF INAME HAS LEADING BLANKS, THEY WILL BE REMOVED.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      REAL*8 DDN
      LOGICAL*1 MESS(1),BAR
C
      DIMENSION INAME(11)
      DATA BAR/'^'/
C
C-------------------  C O D E  -----------------------------------------
C
      JERR = 0
C
 4    IF( MESS(1)   .EQ. BAR ) GO TO 5
      DO  10  M = 1,80
      IF( MESS(M+1) .EQ. BAR ) GO TO 11
 10   CONTINUE
C
 11   WRITE(6,1) (MESS(J),J=1,M)
 1    FORMAT(X,80A1)
 5    READ(5,2) INAME
 2    FORMAT(11A4)
C
C
C             CALL LINKDS TO ALLOCATE LOGICAL UNIT NUMBER LUNC
C             TO THE CALIBRATION DATASET. IF LUNC IS ZERO, GETPDD
C             CHOOSES A VALUE. ON RETURN, HERR SHOULD BE ZERO.
C             IF NOT, 3 ATTEMPTS ARE MADE TO ASK FOR A DSNAME.
C
      CALL LINKDS(LUNC,INAME,HERR,DDN)
      IF( HERR .EQ. 0 ) RETURN
        WRITE(6,3) INAME
 3      FORMAT(' DSNAME ---> ',11A4)
        CALL DSERR(HERR,JERR,DDN,*11)
        RETURN
      END
