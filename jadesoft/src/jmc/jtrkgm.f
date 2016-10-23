C   02/11/81 606101503  MEMBER NAME  JTRKGM9  (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE JTRKGM( PV, R0, * )
C-----------------------------------------------------------------------
C
C   AUTHOR:   E. ELSEN    02/11/81 :  TRACK PHOTONS AND COPE WITH
C                                     CONVERTING ONES.
C
C        MOD  E. ELSEN    29/03/82 :
C   LAST MOD  J. HAGEMANN 21/09/84 :  VERTEX CHAMBER INCLUDED
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
      LOGICAL*1 LFLAG
C
      COMMON / CFLAG  / LFLAG(10)
C
      COMMON / CJTRLE / TOTLEN, STPLEN, TRCOFS
C
      DIMENSION PV(10),R0(3),RR(3)
      DIMENSION P1(10),P2(10)
C
C------------------  C O D E  ------------------------------------------
C
      IF ( LFLAG(5) )       CALL TRKGMV(PV,R0,RR,P1,P2,&34,&1000)
      IF ( .NOT. LFLAG(5) ) CALL TRKGAM(PV,R0,RR,P1,P2,&34,&1000)
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
