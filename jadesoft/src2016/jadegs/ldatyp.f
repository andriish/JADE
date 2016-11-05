C   18/03/88 803181307  MEMBER NAME  LDATYP   (JADEGS)      FORTRAN
      INTEGER FUNCTION LDATYP( DUMMY )
C-----------------------------------------------------------
C  Version of 18/03/88    last Mod 18/03/88   E Elsen
C  Determine event type form MC and data
C     LDATYP = 1   for DL8 Data
C            = 2   for DL300 Data
C-----------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
      COMMON / BCS / IW(1)
      DIMENSION HW(1),RW(1)
      EQUIVALENCE (HW(1),IW(1),RW(1))
      INTEGER IPHEAD / 0 /
C
      IF( IPHEAD.GT.0 ) GO TO 10
        IPHEAD = IBLN('HEAD')
        WRITE(6,9101)
 9101   FORMAT(' +++ LDATYP +++   Version of 18/03/88')
   10 CONTINUE
C
      NPHEAD = IW(IPHEAD)
      NRUN = HW(NPHEAD*2+10)
      IF( NRUN.LT.100 ) NRUN = IZT2RN( DUMMY )
C
      LDATYP = 1
      IF( NRUN.GE.24200 ) LDATYP = 2
      RETURN
      END
