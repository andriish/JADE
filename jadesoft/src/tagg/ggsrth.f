C   25/06/78 C9060101   MEMBER NAME  GGSRTH   (S)           FORTRAN
      SUBROUTINE GGSRTH( IB, ND )
C
C     H.WRIEDT    17-11-78
C     LAST MODIFICATION  21.03.79  18:05
C
C---- PULSE HEIGHT SORT OF LEAD GLASS DATA
C---- SORT ACCORDING TO THE ALGORITHM 201 OF CACM,SEE P.66 DESY R-INFO
C
C---- THIS IS A SUBPROGRAM TO SORT LG-DATA IN THE TWO FORWARD DETECTORS
C     SEPARATELY.
C---- IB=BASE INDEX,  ND=NO. OF DATA TO BE SORTED.
C
      IMPLICIT INTEGER*4 (G), INTEGER*2 (H)
      COMMON /CWORK/ IWORK1(42),LNG,HPOINT(4),HGGADC(2,192)
C***  COMMON /CGGADC/ LNG,HPOINT(4),HGGADC(2,192)
      DIMENSION GGADC(192)
      EQUIVALENCE (GGADC(1),HGGADC(1,1))
C
      COMMON/ CGGVRN/ NVRSN(20)
      DATA NVCODE/178111717/
      NVRSN(17) = NVCODE
C
      IF( ND.LE.1) RETURN
      M = ND
   10 M = M/2
      IF(M.EQ.0) RETURN
      K = ND-M
        DO 50 J=1,K
        I = J+IB
   20   IF(I.LE.IB) GO TO 50
        IF( HGGADC(2,I+M).LE.HGGADC(2,I) ) GO TO 50
C----   REPLACING
    3   IW = GGADC(I)
        GGADC(I) = GGADC(I+M)
        GGADC(I+M) = IW
        I = I-M
        GO TO 20
   50   CONTINUE
      GO TO 10
      END
