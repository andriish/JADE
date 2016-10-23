C--------------------------------------------------
C  MACRO CDATAMIN .... MINIMUM SIZE JADE BOS COMMON
C--------------------------------------------------
      COMMON /BCS/ IDATA(1)
      REAL*4 ADATA(1)
      INTEGER*2 HDATA(2)
      EQUIVALENCE (HDATA(1),IDATA(1),ADATA(1))
C------------- END OF MACRO CDATAMIN ----------
