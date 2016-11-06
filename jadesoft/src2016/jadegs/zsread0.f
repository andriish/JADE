C   21/11/81 305161658  MEMBER NAME  ZSREAD   (S)           FORTRAN
      SUBROUTINE ZSREAD(NRUN,LUN)
C*800817*DITTMANN******************************************************
C*                                                                    *
C*           R E A D   C A L I B R A T I O N   C O N S T A N T S      *
C*                                                                    *
C*       TO BE CALLED FOR EACH EVENT IF THE ZS-PACKAGE IS USED IN A   *
C*       SUPERVISOR CONTROLLED PROGRAM.                               *
C*       THE ROUTINE IS ACTIVE ONLY FOR THE FIRST EVENT AND IF        *
C*       THE RUN NUMBER CHANGES BEYOND THE FOLLOWING LIMITS:          *
C*                                                                    *
C*       79A:      0-1400     79B:    1400-2600                       *
C*       80A:   2600-3730     80B:    3730-4900   80C: 4900-6000      *
C*       81A:   6000-7600     81B:   7600-10000                       *
C*       82A: 10000-11800     82B:  11800-13000   (=NRUNE)            *
C*                                                                    *
C*       CALIBRATION CONSTANTS ARE READ FROM UNIT LUN.                *
C*                                                                    *
C*            ---    LAST UPDATE :  30.4.83  L. BECKER                *
C*                                                                    *
C**********************************************************************
      IMPLICIT INTEGER*2 (H)
      COMMON /CZSCAL/ IPVERS,ZALPDI,RESFAC,SECH(5),DUMMY(2),
     *                TCORR(4,96),XTALK(2,96),PAR(7,1536)
      REAL RESFX(2) / 4480., 7000. /
      REAL SECHX(5,2) /12.3, .183, .011 , .365, .106,
     *                  7.5, .045, .0015, .200, .055/
      DATA NRUNL /1/, NRUNH/0/, NRUNE /13000/
C
C        READ CALIBRATION CONSTANTS
      IF(NRUN.GE.NRUNL .AND. NRUN.LE.NRUNH) RETURN
      IF (NRUN.GT.NRUNE.AND.NRUNH.EQ.NRUNE) RETURN
      IF(NRUN.GT.NRUNH) GOTO 32
      REWIND LUN
   32 READ(LUN,END=33) IPVERS,NRUNL,NRUNH,TCORR,XTALK,PAR
C--   PRINT 10, NRUN, NRUNL,NRUNH
   10 FORMAT ('  NRUN, NRUNL, NRUNH= ',3I10,'         (ZSREAD 10)')
      IF(NRUN.GT.NRUNH) GOTO 32
C
      ZALPDI = 1400.
   33 J = 1
      IF(NRUN.GT.6000) J=2
      RESFAC = RESFX(J)
      DO 34 I=1,5
   34 SECH(I) = SECHX(I,J)
C
      RETURN
      END
