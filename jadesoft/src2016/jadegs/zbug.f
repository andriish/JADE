C   19/03/84            MEMBER NAME  ZBUG     (ZS)          FORTRAN
      SUBROUTINE ZBUG
C ----------------------------------------------------------------------
C     DEBUG FOR DIVIDE CHECK
C ----------------------------------------------------------------------
      COMMON/CZEXT/XT,YT,ZT,XI1,YI1,ZI1,XI2,YI2,ZI2
      COMMON/CZEXT2/X1,Y1,X2,Y2,XA,YA,XB,YB
      COMMON/CZBUG/KRUN,KREC,II,JJ,X0,Y0,R0,RR
      COMMON/CZCOOR/IZ(5,100),RZ(3,100),EZ(2,100)
      WRITE(6,9000) KRUN,KREC,II,JJ
      IF(JJ.GT.0)   WRITE(6,9001) X0,Y0,R0,RR,
     *              XT,YT,ZT,XI1,YI1,ZI1,XI2,YI2,ZI2,
     *              X1,Y1,X2,Y2,XA,YA,XB,YB
      IF(JJ.LT.0.AND.II.NE.0)
     *              WRITE(6,9002) (IZ(I,II),I=1,5),(RZ(I,II),I=1,3)
 9000 FORMAT(/' ******** DIVIDE CHECK ******** RUN ',I6,', EVENT ',I6,
     *        ', TRACK ',I3,': ZEXTRA/ZCOORD ',
     *        'PART ',I2)
 9001 FORMAT(/'         TRACK ',
     *        'PARAMETERS X0,Y0,R0,RR WERE ',4E12.4,/'         ',
     *        'CONTENTS OF COMMON/CZEXT/ ARE  ',9F10.3,/'         ',
     *        'CONTENTS OF COMMON/CZEXT2/ ARE ',8F10.3)
 9002 FORMAT( '         IZ(1-5,II),RZ(1-3,II) WERE ',5I4,3F10.3)
      RETURN
      END
