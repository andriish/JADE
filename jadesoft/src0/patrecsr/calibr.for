C----------------------------------------------------------------------
C            MACRO CALIBR .... JADE CALIBRATION DATA COMMON
C----------------------------------------------------------------------
      COMMON/CALIBR/ ACALIB(1000)
                     DIMENSION HCALIB(100),ICALIB(100)
                     EQUIVALENCE(ACALIB(1),HCALIB(1),ICALIB(1))
C------------------------ END OF MACRO CALIBR -------------------------
