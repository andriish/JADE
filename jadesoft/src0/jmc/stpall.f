C   27/08/83 310271210  MEMBER NAME  STPALL   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE STPALL( LENGTH, IPALL )
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY   29/06/83 :  FILL FOUR VECTOR BANK 'PALL'/0
C
C        MOD  C. BOWDERY   20/10/83 :  NEW CPROD MACRO
C   LAST MOD  C. BOWDERY   27/10/83 :  IFLAVR ==> IABS( IFLAVR )
C
C  FILL FOUR VECT BANK PALL  WHICH CONTAINS ALL THE PARTICLES.
C
C  LENGTH = TOTAL LENGTH OF IPALL ARRAY
C
C  IPALL( 1) = LGTH OF HEADER=L0   IPALL( 2) = LENGTH OF 1PARTICLE DATA
C  IPALL( 3) = EVT #               IPALL( 4) = TOTAL # OF PARTICLES
C  IPALL( 5) = # OF CHARGED PART.  IPALL( 6) =  # OF NEUTRAL PARTICLES
C   VECT( 7) = PHI OF JET AXIS      VECT( 8) = COS(THETA) OF JET AXIS
C  IVECT( 9) = FLAVOUR OF EVENT
C
C  IPALL( L0 + 1 - 4 ) = MOMENTUM FOUR VECTOR
C  IPALL(L0+5) = MASS OF PARTICLE  IPALL(L0+6) = CHARGE OF PARTICLE
C  IPALL(L0+7) = PARTICLE TYPE
C  IPALL(L0 + 8 ) = PARENT PARTICLE NUMBER IN LIST
C  IPALL(L0 + 9 ) = NO. OF PARTON WHICH GAVE RISE TO THIS PARTICLE
C                                           AT THE START.
C  EXTRA ENTRY LHPALL RETURNS LENGTH ONLY
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cprod.for"
C
      DIMENSION IPALL(1) , IPP(4,500) , IXM(500)
C
      EQUIVALENCE (IPP(1,1),PP(1,1)) , (IXM(1),XM(1))
      EQUIVALENCE (IPHI,PHI) , (ITHETA,THETA)
C
      DATA L0 / 9 /, L1 / 9 /
C
C--------------------  C O D E  ----------------------------------------
C
C                   BANK HEADER
C
      IPALL( 1) = L0
      IPALL( 2) = L1
      IPALL( 3) = NEV
      IPALL( 4) = NP
      IPALL( 5) = NC
      IPALL( 6) = NN
      IPALL( 7) = IPHI
      IPALL( 8) = ITHETA
      IPALL( 9) = IABS( IFLAVR )
      J = L0
C
C                  STORE DATA FOR EACH PARTICLE
C
      NJET = 0
      DO 10 K =1,NP
      IPALL(J+ 1) = IPP(1,K)
      IPALL(J+ 2) = IPP(2,K)
      IPALL(J+ 3) = IPP(3,K)
      IPALL(J+ 4) = IPP(4,K)
      IPALL(J+ 5) = IXM(K)
      IPALL(J+ 6) = JCH(K)
      IPALL(J+ 7) = JTP(K)
      IPALL(J+ 8) = JP(K,1)
      IPALL(J+ 9) = JP(K,2)
   10 J = J + L1
C
C----------------------------------------------------------
      ENTRY LHPALL( LENGTH )
C----------------------------------------------------------
C
C                                      LENGTH OF BANK
      LENGTH = NP*L1 + L0
      RETURN
      END
