C   02/11/81 312061508  MEMBER NAME  JTRKGM0  (S)           FORTRAN
      SUBROUTINE JTRKGM( PV, R0, * )
C--------------------------------------------------------
C
C  VERSION OF 02/11/81   LAST MOD 29/03/82      E.ELSEN
C  TRACK PHOTONS AND COPE WITH CONVERTING ONES.
C--------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
      DIMENSION PV(10),R0(3),RR(3)
      DIMENSION P1(10),P2(10)
      COMMON / CJTRLE / TOTLEN, STPLEN, TRCOFS
C                                           TRACK PHOTONS
      CALL TRKGAM(PV,R0,RR,P1,P2,*34,*1000)
C
C                                           PHOTON CONVERTED, STORE ELEC
      TRCOFS = TRCOFS + SQRT(RR(1)**2 + RR(2)**2 + RR(3)**2)
      P1(8) = 2.
      P2(8) = 2.
      CALL SVECT1( P1 , RR )
      CALL SVECT1( P2 , RR )
      GO TO 1000
C
   34 CONTINUE
      DO 35 J=1,3
   35 R0(J) = RR(J)
      RETURN
C
 1000 CONTINUE
      RETURN 1
      END
