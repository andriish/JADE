C   11/01/88 807231744  MEMBER NAME  BOSDATA  (S)           MACROFOR
C
      PARAMETER  ( LENBOS = 65000 )
C
      INTEGER    IDATA
      REAL       ADATA( LENBOS )
      INTEGER*2  HDATA( 2*LENBOS )
C
      COMMON  / BCS /  IDATA( LENBOS )
C
      EQUIVALENCE  ( IDATA(1), ADATA(1) ), ( HDATA(1), IDATA(1) )
C
