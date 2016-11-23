      PROGRAM MAIN
*
*   Test of conversion routine  CNVIBM3E
*
      DATA ITEST1 /'C276A000'X/
*          ITEST1 corresponds to the IBM FP repr. of -118.625
*
*-------------  CODE  -------------
*
*
      WRITE(6,1001)
 1001 FORMAT('  TEST of CNVIBM3E ')
*
      INN = ITEST1
      WRITE(6,1002) INN
 1002 FORMAT(' Calling CNVIBM3E  with INN ',Z8)
*
      CALL CNVIBM3E(INN)
      WRITE(6,1003) INN
 1003 FORMAT(' After call to CNVIBM3E,  INN ',Z8)
*
      RETURN
      END
