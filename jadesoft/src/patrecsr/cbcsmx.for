C-----------------------------------------------------------------------
C             MACRO CBCSMX.... BOS COMMON + SIZE DEFINITION
C
C             THIS MACRO DEFINES THE IDATA/HDATA/ADATA NAMES AND
C             FIXES THE ACTUAL SIZE OF /BCS/. IT IS THUS IDEAL FOR
C             MAIN PROGRAMS OR THE SUPERVISOR. LDATA CAN BE USED BY BINT
C
C-----------------------------------------------------------------------
C
      COMMON /BCS/ IDATA(60000)
      DIMENSION HDATA(120000),ADATA(60000)
      EQUIVALENCE (HDATA(1),IDATA(1),ADATA(1))
      DATA  LDATA / 60000 /
C
C------------------------ END OF MACRO CBCSMX --------------------------
