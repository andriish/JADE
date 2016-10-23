C   30/11/82 606091754  MEMBER NAME  JSTORE   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE JSTORE
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. HAGEMANN  16/11/82 :  ORDER VERTEX CHAMBER HITS OF
C             R. RAMCKE                ONE EVENT
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / CSTORE / MHITS, IPV, HSTORE(2000)
C
      DIMENSION HWV(4)
C
C------------------------  C O D E  ------------------------------------
C
C     -----   ORDER HITS OF ONE EVENT USING SHELLSORT ALGORITHM  -----
C
       IF( MHITS.EQ. 0 ) RETURN
C
       M = MIN0( MHITS, 30 )
C
   10 M = M/2
            IF( M .EQ. 0 ) GO TO 900
          K = ( MHITS - M )*4
       DO 50 J = 1, K, 4
             I = J
   20             IF( I .LT. 1 ) GO TO 50
                    IM = I + M*4
                     IF( HSTORE(IM).GT.HSTORE(I).OR.
     *                  (HSTORE(IM).EQ.HSTORE(I).AND.
     *                    HSTORE(IM+3).GE.HSTORE(I+3)) ) GO TO 50
                                ISH = ( I - 1 )*2
                                IMSH =( IM - 1 )*2
                                CALL MVC( HWV, 0, HSTORE, ISH, 8 )
                                CALL MVC( HSTORE, ISH, HSTORE, IMSH, 8 )
                                CALL MVC( HSTORE, IMSH, HWV, 0, 8 )
             I = I - 4*M
             GOTO 20
   50   CONTINUE
        GO TO 10
  900 CONTINUE
      RETURN
      END
