C   08/06/86 606080831  MEMBER NAME  CALLOC   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE CALLOC( OK )
C-----------------------------------------------------------------------
C
C    AUTHOR:   C. BOWDERY   8/06/86 :  ALLOCATES CALIBRATION DATASETS
C
C
C     ALLOCATES THE CALIBRATION DATASETS TO VARIABLE LUNS.
C     OK = .TRUE. IF THE ALLOCATION WAS SUCCESSFUL.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      REAL*8   DDN
      LOGICAL  OK
C
#include "cgraph.for"
#include "ciouni.for"
C
      COMMON / CNWCAL / IFILE
C
      DIMENSION LUNC(11,4)
C
      DATA  HBLANK/'  '/, HCAL/'CC'/
      DATA  LUNC /'F11L','HO.B','UPDA','T0  ','    ','    ',5*'    ',
     +            'F11L','HO.B','UPDA','T1  ','    ','    ',5*'    ',
     +            'F11L','HO.A','UPDA','T1  ','    ','    ',5*'    ',
     +            'F11L','HO.K','ALWR','K0  ','    ','    ',5*'    '/
C
C
C------------------  C O D E  -----------------------------------------
C
      CALL TRMOUT(80,'The default calibration dataset  F11LHO.AUPDAT1 wi
     $ll now be allocated.^')
      CALL TRMOUT(80,'If you want the BUPDAT0/BUPDAT1 files for LG calib
     $ration (REFORM Data),^')
      CALL TRMOUT(80,'then press any CHARACTER+RETURN, else press RETURN
     $ for the first event^')
      CALL TRMOUT(80,'  --->     Please wait a moment...^')
C
C                            ALLOCATE THE CALIBRATION DATA SETS
C
      IFILE = 0
      NCALI = 1
      HDUM  = HBLANK
      OK    = .TRUE.
C
      LUNNNN = 0
      CALL LINKDS(LUNNNN,LUNC(1,3),HERR,DDN)
      IF( HERR .NE. 0 ) GO TO 100
      LUNITA(1) = LUNNNN
      GO TO 150
C
C                            ERROR WHILE ALLOCATING AUPDAT1 FILE
C
 100  CALL TRMOUT(80,'Error in allocating F11LHO.AUPDAT1.  The BUPDAT fi
     $les will be tried..^')
      IF( NCALI .EQ. 1 ) GO TO 130
 105  CALL TRMOUT(80,'Allocation Failure for both A and B files. Prematu
     $re End.^')
C
      OK = .FALSE.
      RETURN
C
C                            ERROR WHILE ALLOCATING BUPDAT0/1 FILE
C
 110  CALL TRMOUT(80,'Error in allocating BUPDAT0,1 files.  The AUPDAT1
     $file will be tried..^')
      IFILE = 0
      IF( NCALI .EQ. 2 ) GO TO 150
      GO TO 105
C
 120  NCALI = 2
C
 130  IFILE = 1
C
      DO  140  II = 1,2
        IPDS = II
        IF( II .EQ. 2  .AND.  HDUM .EQ. HCAL ) IPDS = 4
        LUNNNN = 0
        CALL LINKDS(LUNNNN,LUNC(1,IPDS),HERR,DDN)
        IF( HERR .NE. 0 ) GO TO 110
        LUNITA(II) = LUNNNN
 140  CONTINUE
C
      CALL TRMOUT(80,'  ---> Allocation Ready....^')
      RETURN
C
 150  CALL TRMOUT(80,'  ---> Allocation Ready.  Enter ?^')
      CALL TRMIN(2,HDUM)
      IF( HDUM .NE. HBLANK ) GO TO 120
C
      RETURN
      END
