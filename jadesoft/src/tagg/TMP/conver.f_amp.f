C   19/03/84            MEMBER NAME  CONVER   (S)           FORTRAN
C   16/09/79 C9091601   MEMBER NAME  CONVER   (JADPRIVS)    FORTRAN
      SUBROUTINE CONVER
C---  SET HPOINT(4) TO 0 (IT WAS NOT DONE FOR SOME DATA)
C
C     H.WRIEDT       16.09.79       18:45
C     LAST MODIFICATION       16.09.79      18:45
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON /CWORK/ HCOINC(16),HLUMON(16),HGGLAT(16),HSCALE(36),
     *               LNG,HPOINT(4)
C
      HPOINT(4) = 0
      RETURN
      END