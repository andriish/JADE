C   14/03/84 403211545  MEMBER NAME  GSCRUB   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE GSCRUB
C-----------------------------------------------------------------------
C
C     CLEAR COMMON CWORK FOR NEW REAL EVENT
C     FOR CONTENTS OF CWORK SEE H.WRIEDT'S LABORBUCH, 16.3.79
C
C     H.WRIEDT      09.03.79
C          MOD      20.07.79
C     LAST MOD      21.03.84 : RENAMED FROM GCLEAR
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON /CWORK/ IWORK(2000)
      DIMENSION HWORK(4000), RWORK(2000)
      EQUIVALENCE (HWORK(1),IWORK(1),RWORK(1))
C
        DO 1 I = 1,2000
    1   IWORK(I) = 0
        RWORK(528) = 6.2831853
C
      RETURN
      END
