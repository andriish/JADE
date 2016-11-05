C   21/11/86 702231849  MEMBER NAME  ZE4VVX   (S)           FORTRAN77
      SUBROUTINE ZE4VVX( R, NTRZ )
C-----------------------------------------------------------
C  VERSION OF 24 JAN 85         LAST MOD 23 FEB 87   M ZIMMER
C  LOAD EVENT VERTEX.
C  X,Y POSITION:  FROM CALIB IF IT IS LOADED OTHERWISE (0,0)
C  Z   POSITION:  CALCULATE FROM PATR, ACCEPTING TRACKS WITH
C                 MORE THAN NHZCT HITS ONLY AND
C                 LESS THAN ZMXCT MM FROM 0
C-----------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
C
      COMMON / BCS / IW(1)
      DIMENSION RW(1), HW(1)
      EQUIVALENCE (HW(1),RW(1),IW(1))
#include "calibr.for"
      REAL*4 R(3)
      INTEGER NHZCT / 7 /
      REAL*4 ZMXCT / 80. /
      INTEGER IPVXDF / 16389 /
      INTEGER IEV / 0 /
C
      IEV = IEV + 1
      DO 1 I=1,3
    1 R(I) = 0.
C
      ICX = ICALIB(10)
      IF( ICX.NE.IPVXDF ) GO TO 50
C                                           LOAD BHABHA VERTEX
         R(1) = ACALIB(ICX+1)
         R(2) = ACALIB(ICX+3)
      GO TO 100
   50 CONTINUE
         IF( IEV.GT.1 ) GO TO 100
            WRITE(6,9101) ICX,IPVXDF
 9101       FORMAT(' EAVXGL WARNING: ICALIB(10)=',I6,
     +             ' DOES NOT MATCH DEFAULT ',I6)
  100 CONTINUE
C
C
      NTRZ = 0
      ZV = 0.
      SZH = 0.
C
C
      NPPATR = IW(IBLN('PATR'))
      IF( NPPATR .EQ. 0 ) GO TO 1000
         NTR = IW( NPPATR + 2 )
         IF( NTR .LE. 0 ) GO TO 1000
            IP0 = IW( NPPATR + 1 ) + NPPATR
            LP = IW( NPPATR + 3 )
            IP9 = IP0 + ( NTR - 1) * LP
C                                           LOOP OVER TRACKS
            DO 300 J = IP0, IP9, LP
               IF( IW(J+33) .LT. NHZCT ) GO TO 300
                  AZV = ABS(RW(J+31))
               IF( AZV .GT. ZMXCT ) GO TO 300
                  ZV = ZV +  RW(J+31)*IW(J+33)
                  SZH = SZH + IW(J+33)
                  NTRZ = NTRZ + 1
  300       CONTINUE
            IF( NTRZ.LE.0 ) GO TO 1000
               ZV = ZV / SZH
 1000 CONTINUE
      R(3) = ZV
      RETURN
      END
