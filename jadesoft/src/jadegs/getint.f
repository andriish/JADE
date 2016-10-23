C   21/06/85 506211630  MEMBER NAME  GETINT   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE GETINT( IZA, INT )
C-----------------------------------------------------------------------
C
C    AUTHOR:   R. RAMCKE     14/03/83 :  CONVERTS EBCDIC-CODE TO INTEGER
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      DIMENSION HNR(10)
C
      DATA IML / 255 /
      DATA HNR / ' 0',' 1',' 2',' 3',' 4',' 5',' 6',' 7',' 8',' 9' /
C
C-----------------  C O D E  -------------------------------------------
C
      IEXP = 1
      INT = 0
      IZB = IZA
      DO 300 I =1,4
         IHZB = LAND(IML, IZB)
         IZB = ISHFTR(IZB,8)
         HZB = IHZB
         DO 100 J = 1,10
            HNRM = hLAND(HNR(J), hint(IML) ) ! PMF 10/06/99: hlor,hint 
            IF(HZB .EQ. HNRM) GOTO 200
 100     CONTINUE
         GOTO 300
 200     INT = INT + (J-1)*IEXP
         IEXP = IEXP*10
 300  CONTINUE
      RETURN
      END
