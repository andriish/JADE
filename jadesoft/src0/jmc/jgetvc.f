C   02/11/81 312061526  MEMBER NAME  JGETVC   (S)           FORTRAN
      SUBROUTINE JGETVC( P, R, ICODE, IPRINT )
C-----------------------------------------------------------
C
C  VERSION OF 02/11/81   LAST MOD 05/11/81      E.ELSEN
C  GET NEXT REASONABLE 4-VECTOR FROM BANK VECT.
C  P = MOMENTUM ARRAY, R = STARTING POINT.
C  ICODE > 0 IF NO MORE PARTICLES IN VECT BANKS.
C  ( STEERED VIA COMMON /CJTCDC / )
C  PRINT OUT IF PRINT > 0.
C-----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
      COMMON / BCS / IW(1)
      DIMENSION RW(1), HW(1)
      EQUIVALENCE (HW(1),RW(1),IW(1))
C
      COMMON / CJTCDC / IBANK, IPART
      COMMON / CJTRLE / TOTLEN, STPLEN, TRCOFS
C
      COMMON / CJPATR / KPATR, IPATR(1448)
      DIMENSION HPATR(1)
      EQUIVALENCE (HPATR(1),IPATR(1))
C
      DIMENSION P(10),R(3)
C
      DATA IUNPH / 0 /
      DATA ICALL / 0 /
      IF( ICALL .GT. 0 ) GO TO 1
      ICALL = 1
      IPVECT = IBLN('VECT')
    1 CONTINUE
C
      NPVECT = IPVECT + 1
      ICODE = 2
    2 NPVECT = IW(NPVECT-1)
      IF( NPVECT .LE. 0 ) GO TO 8000
      IF( IW(NPVECT-2) .LT. IBANK ) GO TO 2
C
      L0 = IW(NPVECT+1)
      L1 = IW(NPVECT+2)
      NR  = IW(NPVECT+3)
      N  = IW(NPVECT+4)
C
      IF( IPART.EQ.0.AND.IPRINT.GT.0 ) WRITE(6,9101) NR, IBANK
 9101 FORMAT(//' === EVENT ',I5,
     *        '  === TRACKED PARTICLES FROM VECT',I3,' FOLLOWING ===' )
C
 1000 IF( IPART .GE. N ) GO TO 7000
      IP = NPVECT + L0 + IPART*L1
      IPART = IPART + 1
      ITYPE = IW(IP+7)
C                                           CHECK PARTICLE TYPE
      IF( ITYPE .LE. 0 ) GO TO 1000
C                                           UNPHYSICAL PARTICLES
      IF(RW(IP+5).EQ.0. .AND. IW(IP+6).NE.0 ) GO TO 6500
C
C                                           INITIALISE PARTICLE VECTOR
C                                           AND ORIGIN
      DO 200 J=1,5
  200 P(J)=RW(IP+J)
      P(6)=SQRT(P(1)**2+P(2)**2+P(3)**2)
      P(7)=IW(IP+6)
      P(8)=IW(IP+7)
      P(9)=0.
      P(10)=0.
C
      R(1)=RW(IP+ 8)
      R(2)=RW(IP+ 9)
      R(3)=RW(IP+10)
      R(4)=RW(IP+11)
C                                           OFFSET FOR TOF
      IF( IBANK .EQ. 1 ) TRCOFS = RW(IP+12)
      IF( IBANK .EQ. 0 ) TRCOFS = SQRT( R(1)**2+R(2)**2+R(3)**2 )
C                                           PATR INFORMATION
      IPATR( KPATR + 4 ) = IBANK + 1
      HPATR( (KPATR+2)*2-1) = IPART
C
      IF( IPRINT .GT. 0 ) WRITE(6,9102) (P(I),I=1,8),R
 9102 FORMAT(8F8.3,3F9.3)
C
C
      ICODE = 0
      GO TO 8000
C
C                                           MESSAGES
 6500 IF( IUNPH .GT. 5 ) GO TO 1000
      IUNPH = IUNPH + 1
      WRITE(6,9103) IPART
 9103 FORMAT(' === UNPHYSICAL PARTICLE AT PARTICLE #',I3,' ===')
      CALL BPRS( 'VECT', IBANK )
      GO TO 1000
C                                           NEXT BANK
 7000 IBANK = IBANK + 1
      IPART = 0
      GO TO 2
C
C
 8000 RETURN
      END
