C   03/12/83 712202134  MEMBER NAME  BRVECT   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE BRVECT( IUNIT, *, * )
C-----------------------------------------------------------------------
C
C   AUTHOR    E. ELSEN             :  READ MC EVENT AND FILL VECT/0
C
C        MOD  C. BOWDERY  29/06/83 :  ALSO CREATE PALL/0 BANK
C        MOD  C. BOWDERY  20/10/83 :  FOR CPROD  MACRO
C        MOD  C. BOWDERY  17/11/83 :  CALL TO MCVALI TO CHECK INPUT
C        MOD  C. BOWDERY  22/11/83 :  IMPROVED ERROR MESSAGE CONTROL
C        MOD  C. BOWDERY   2/12/83 :  CHANGED MESSAG (13/14) TO (18/19)
C        MOD  J. OLSSON   1985     :  NEW AT ONCE, IF MCVALI FAILS
C   LAST MOD  J. OLSSON   20/5/87  :  BANK SF56 CREATED
C
C
C  READ EVENT AND FILL 4 VECTOR BANK VECT/0 AND PALL/0. THE LATTER HAS
C  INFORMATION ABOUT ALL THE PARTICLES THE GENERATOR MADE.
C
C  RETURN 1    FOR READ ERROR, NOT ENOUGH SPACE IN BCS OR BAD INPUT DATA
C  RETURN 2    FOR EOF
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL VALID
C
      COMMON / BCS /  IW(1)
C
      COMMON /CCSF56/ NFINAL,NTR56
C
      COMMON / CVERR / MESSAG(20)
C
#include "cprod.for"
C
C-----------------------  C O D E  -------------------------------------
C
C                                           READ EVENTS
C
1111  READ(IUNIT,ERR=8000,END=8100)  NEV, BEAM, PT, PHI, THETA, IFLAVR,
     *     NP, NC, NN, ( (PP(I4,N), I4=1,4), XM(N), JCH(N), JTP(N),
     *     (JP(N,I2), I2=1,2), N=1,NP),
     *     NF, NCF, NNF, ( (PF(I4,N2),I4=1,4), XMF(N2), ICF(N2),ITF(N2),
     *     (PSTRT(I3,N2),I3=1,3),N2=1,NF)
C
C                                           CHECK INPUT
C
      CALL MCVALI( VALID )
C     IF( .NOT. VALID ) RETURN 1
      IF( .NOT. VALID ) WRITE(6,5000)
5000  FORMAT(' BRVECT, JADE56 VERSION:   EVENT REJECTED, NOT TRACKED.')
      IF( .NOT. VALID ) GO TO 1111
C
C                                           CREATE VECT BANK
C
      CALL LHVECT( LENGTH )
      CALL BCRE( NPVECT, 'VECT', 0 , LENGTH, &8200, IER )
      CALL STVECT( LENGTH, IW(NPVECT+1) )
      CALL BSAW( 1, 'VECT' )
C
C                                           CREATE SF56 BANK
C
      NFINAL = NF
      LSF56 = NF + 1
      CALL BCRE( IPSF56, 'SF56', 0 , LSF56, &8200, IER )
      IW(IPSF56+1) = NF
      CALL BSAW( 1, 'SF56' )
C
C                                           CREATE PALL BANK
C
      CALL LHPALL( LENGTH)
      CALL BCRE( NPALL, 'PALL', 0, LENGTH, &8300, IER )
      CALL STPALL( LENGTH, IW(NPALL + 1) )
      CALL BSAW( 1, 'PALL' )
      RETURN
C                                           READ ERROR
 8000 MESSAG(18) = MESSAG(18) + 1
      IF( MESSAG(18) .GT. 20 ) RETURN 1
C
      WRITE(6,8005) IUNIT
 8005 FORMAT(/' *** READ ERROR ***  INPUT UNIT NUMBER = ',I4,'    ***'/)
      RETURN 1
C                                           BOS ERROR (VECT)
 8200 MESSAG(19) = MESSAG(19) + 1
      IF( MESSAG(19) .GT. 20 ) RETURN 1
C
      WRITE(6,8205) IER
 8205 FORMAT(/' *** BOS ERROR ***  BCRE RETURNED ERROR CODE ',I2,
     +        '   WHILE CREATING VECT/0 IN BRVECT    ***'/)
      RETURN 1
C                                           BOS ERROR (PALL)
 8300 MESSAG(19) = MESSAG(19) + 1
      IF( MESSAG(19) .GT. 20 ) RETURN 1
C
      WRITE(6,8305) IER
 8305 FORMAT(/' *** BOS ERROR ***  BCRE RETURNED ERROR CODE ',I2,
     +        '   WHILE CREATING PALL/0 IN BRVECT    ***'/)
      RETURN 1
C                                           EOF DETECTED
 8100 RETURN 2
      END
