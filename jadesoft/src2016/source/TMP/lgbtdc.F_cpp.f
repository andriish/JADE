C   09/05/82 205271820  MEMBER NAME  LGBTDC   (SOURCE)      FORTRAN
      SUBROUTINE LGBTDC( CBTDC, JFLLGT )
C
C     S.YAMADA   09-05-82   12:00      COPIED FROM F22YAM.PHANAL.S
C                                      ON 27-05-82
C---- LG-BARREL TOF
C
C----------------------------------------------------------------------
C             MACRO CDATA .... BOS COMMON.
C
C             THIS MACRO ONLY DEFINES THE IDATA/HDATA/ADATA NAMES.
C             THE ACTUAL SIZE OF /BCS/ IS FIXED ON MACRO CBCSMX
C             OR BY OTHER MEANS. A DEFAULT SIZE OF 40000 IS GIVEN HERE.
C
C----------------------------------------------------------------------
C
      COMMON /BCS/ IDATA(40000)
      DIMENSION HDATA(80000),ADATA(40000),IPNT(50)
      EQUIVALENCE (HDATA(1),IDATA(1),ADATA(1)),(IPNT(1),IDATA(55))
      EQUIVALENCE (NWORD,IPNT(50))
C
C------------------------ END OF MACRO CDATA --------------------------
C
      INTEGER *4 NRUNBN(4)/2010241,2010263,2010361,2010973/
      DATA NMAX/4/
      REAL       TBASE(4)/ 804.40, 1195.07,988.52, 1198.86/
C
      CBTDC = 0.
      JFLLGT = 0
C
C---- RUN NUMBER
      NPHD2 = 2*IDATA( IBLN('HEAD') )
      IF( NPHD2.LE.0 ) GO TO 90
      NRUN = HDATA(NPHD2+9)*100000 + HDATA(NPHD2+10)
      IF( NRUN.LT.2010055 ) GO TO 90
C     GET THE RUN-BIN NUMBER
        DO 1 N=1,NMAX
        IF( NRUN.LE.NRUNBN(N) ) GO TO 2
    1   CONTINUE
C
C---- CALIBRATION IS NOT READY YET.
      IFLLGT = 1
      N = NMAX
C
    2 TBASEN = TBASE(N)
C
C---- 'ATST' BANK
      NPATST = IDATA( IBLN('ATST') )*2
      IF( NPATST.EQ.0 ) GO TO 90
      BADC = HDATA( NPATST+4 )
      BTDC = HDATA( NPATST+17 )
      CBTDC = (BTDC + 280.0-70.*(2.0-0.001*BADC)**2 - TBASEN)*0.1
      RETURN
C
   90 JFLLGT = 2
      RETURN
      END
