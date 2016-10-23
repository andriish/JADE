C-----------------------------------------------------------------------
C             MACRO CBCSMX.... BOS COMMON + SIZE DEFINITION
C
C             THIS MACRO DEFINES THE IDATA/HDATA/ADATA NAMES AND
C             FIXES THE ACTUAL SIZE OF /BCS/. IT IS THUS IDEAL FOR
C             MAIN PROGRAMS OR THE SUPERVISOR. LDATA CAN BE USED BY BINT
C
C-----------------------------------------------------------------------
C
      COMMON /BCS/ IDATA(45000)
      DIMENSION HDATA(90000),ADATA(45000)
      EQUIVALENCE (HDATA(1),IDATA(1),ADATA(1))
      DATA  LDATA / 45000 /
C
C------------------------ END OF MACRO CBCSMX --------------------------
