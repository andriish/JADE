C   29/07/79 004240754  MEMBER NAME  ORDLG    (S)           FORTRAN
      SUBROUTINE ORDLG
C-----------------------------------------------------------
C   VERSION OF 29/07/79      LAST MOD    24/04/80   BARTEL/ELSEN
C   PUT LEAD GLASS BLOCKS WITH AMPLITUDES .NE.0 INTO ARRAY HAMPL
C      HAMPL(1,I) = BLOCK NUMBER
C      HAMPL(2,I) = PULSEHEIGHT IN MEV
C   ERASE BLOCKS IN FIRST AND LAST RING ( THESE RINGS HAVE NOT
C   BEEN BUILT IN ).
C   ERROR CORRECTED IN ROW NUMBER FOR ERASED BLOCKS.
C-----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
      COMMON/CLGHIT/AMPLG(3000)
      COMMON/CLGAMP/LGLONG,HAMPL(2,1000)
      DATA NBLIM / 2688 /
C
      LGLONG=0
      I1=0
C
      DO 1 I=1,3000
      IF(AMPLG(I).EQ.0.) GO TO 1
C                                           END CAP CHECK
      IF( I .GT. NBLIM ) GO TO 2
C                                           ERASE EXTREME RINGS
      IF( MOD(I,32) .EQ. 0 ) GO TO 1
      IF( MOD(I+31,32) .EQ. 0 ) GO TO 1
C                                           STORE BLOCK AND ENERGY
    2 LGLONG=LGLONG+2
      I1=I1+1
      HAMPL(1,I1)=I
      HAMPL(2,I1)=AMPLG(I)
    1 CONTINUE
      RETURN
      END
