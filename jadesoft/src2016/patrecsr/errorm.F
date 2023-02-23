C   06/12/79 102191154  MEMBER NAME  ERRORM   (PATRECSR)    FORTRAN
      SUBROUTINE ERRORM(SUBR,NERROR,NT)
      IMPLICIT INTEGER*2(H)
      REAL*8 SUBR
CAV      COMMON/ CADMIN /IEVTP,NREC,NRWRIT,NRERR
CAV   To assure all symbols have same size
#include "cadmin.for"
CAV

      COMMON/CHEADR/HEAD(17),HRUN,HEV
#include "cpatlm.for"
C
C
C-----------------------------------------------------------------------
C
C         -------------- SUBROUTINE ERRORM  ------------------
C         --- G.F.PEARCE .. LAST UPDATE : 1200 ON  6/12/80 ---
C
C     THIS SUBROUTINE PRINTS OUT A SINGLE LINE ERROR MESSAGE
C     REQUIRED :
C     SUBR   = ALPHANUMERIC SUBROUTINE NAME (8 BYTES) IN WHICH ERROR
C              OCCURRED (E.G. 'XYFIT   ')
C     NERROR = ERROR IDENTIFICATION NUMBER
C     NT     = TRACK NUMBER ON WHICH ERROR OCCURED
C
C-----------------------------------------------------------------------
C
C
      IXYF(11) = IXYF(11) - 1
      IF(IXYF(11).LT.0)RETURN
      PRINT1,SUBR,NERROR,NREC,HRUN,HEV,NT
 1    FORMAT(1X,20('#'),A8,' ERROR MESSAGE ',I3
     +  ,'  RECORD/RUN/EVENT/TRACK =',4I6,1X,20('#'))
      IF(IXYF(11).EQ.0)PRINT2
 2    FORMAT(1X,20('$'),' ERRORM ERROR MESSAGES NOW SUSPENDED')
      RETURN
      END
