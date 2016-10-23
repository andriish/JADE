C   08/06/86 606102122  MEMBER NAME  ENDMES   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE ENDMES( OK )
C-----------------------------------------------------------------------
C
C    AUTHOR:   C. BOWDERY   8/06/86 :  FINAL MESSAGES ABOUT GRAPHICS
C
C
C
C     OUTPUTS THE FINAL MESSAGES FROM THE GRAPHICS PROGRAM.
C     IF OK = .FALSE., SKIP MESSAGE ABOUT OUTPUT RECORDS.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      REAL*8    DATE,TIME
      INTEGER*4 BINTIM
      LOGICAL   OK
C
#include "cgraph.for"
C
      COMMON / CWORK  / DATE, TIME, DUMMY, ITIME, HWORK(40), HNAM(8)
      COMMON / CSVCW1 / NDDSVE,NRWR
C
      DIMENSION  HTM1(4), HTM2(4)
      DIMENSION  HNAME(4)
C
      EQUIVALENCE  ( DATE, HTM1(1) )
      EQUIVALENCE  ( TIME, HTM2(1) )
C
      EQUIVALENCE  ( HNAM(1), HNAME(1) )
C
      DATA  HBLANK /'  '/
C
C------------------  C O D E  -----------------------------------------
C
      IF( .NOT. OK      ) GO TO 170
      IF( NDDOUT .EQ. 0 ) GO TO 170
      WRITE(JUSCRN,165) NRWR
 165  FORMAT(' The newly written records are preceded by ',I5,' records
     $on the output file')
C
 170  CALL DAY(DATE,TIME)
      DO  180  I = 1,4
        HWORK(I) = HTM1(I)
 180  CONTINUE
      HWORK(5) = HBLANK
      DO  190  I = 1,4
        HWORK(I+5) = HTM2(I)
 190  CONTINUE
C
C                            CLEAR NAME STRING
C
      DO  195  I = 1,8
        HNAM(I) = HBLANK
 195  CONTINUE
C
C                            FIND USERID
C
      CALL IDFUN(HNAME,NBOK)
C
      WRITE(6,200) (HWORK(I),I=1,9)
 200  FORMAT(' Scan session ending at DATE and TIME: ',9A2)
C
      ITIME = BINTIM(DUMMY)/360000
C
      IF(ITIME.GT.4  .AND. ITIME.LE.9  ) WRITE(6,210) (HNAM(I),I=1,5)
      IF(ITIME.GT.9  .AND. ITIME.LE.16 ) WRITE(6,220) (HNAM(I),I=1,5)
      IF(ITIME.GT.16 .AND. ITIME.LE.21 ) WRITE(6,230) (HNAM(I),I=1,5)
      IF(ITIME.GT.21 .OR.  ITIME.LE.4  ) WRITE(6,240) (HNAM(I),I=1,5)
 210  FORMAT('         Have a pleasant morning, ',4A2,'  ',A2)
 220  FORMAT('         A very good day to you, ',4A2,'  ',A2)
 230  FORMAT('         Have an enjoyable evening, ',4A2,'  ',A2)
 240  FORMAT('         Good night and sleep well, ',4A2,'  ',A2)
C
      RETURN
      END
