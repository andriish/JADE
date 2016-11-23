      PROGRAM chartest
*
      CHARACTER*8  DUMM
      CHARACTER*8 FMT(11)
      CHARACTER*8 FMZ(8)
      EQUIVALENCE (FMZ(1),FMT(3))
*
      DO 100 I = 1,100
        DUMM = FMT(3)
        WRITE(6,1001) DUMM
 1001   FORMAT('  DUMM ',A8)
 100  CONTINUE
      STOP
      END
