C   27/08/83 310201409  MEMBER NAME  STVECT   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE STVECT( LENGTH, IVECT )
C-----------------------------------------------------------------------
C
C   AUTHOR:   E. ELSEN     07/04/81 :  FILL FOUR VECTOR BANK 'VECT'/0
C
C        MOD  E. ELSEN     03/11/81 :
C        MOD  C. BOWDERY   27/08/83 :  EXPANDED COMMON /CPROD/.
C   LAST MOD  C. BOWDERY   20/10/83 :  NEW CPROD MACRO
C
C                 FILL FOUR VECT BANK VECT.
C
C  LENGTH = TOTAL LENGTH OF IVECT ARRAY
C
C  IVECT( 1) = LGTH OF HEADER=L0   IVECT( 2) = LENGTH OF 1PARTICLE DATA
C  IVECT( 3) = EVT #               IVECT( 4) = TOTAL # OF PARTICLES
C  IVECT( 5) = # OF CHARGED PART.  IVECT( 6) =  # OF NEUTRAL PARTICLES
C   VECT( 7) = PHI OF JET AXIS      VECT( 8) = COS(THETA) OF JET AXIS
C
C  IVECT( 9) = FLAVOUR OF THE EVENT
C  IVECT(10) = 0
C  IVECT(11) = 0
C  IVECT(12) = 0
C  IVECT(13) = BEAM ENERGY IN MEV
C
C  IVECT( L0 + 1 - 4 ) = MOMENTUM FOUR VECTOR
C  IVECT(L0+5) = MASS OF PARTICLE  IVECT(L0+6) = CHARGE OF PARTICLE
C  IVECT(L0+7) = PARTICLE TYPE
C  IVECT(L0 + 8 -10 ) = ORIGIN OF PARTICLE ( USUALLY (0,0,0) )
C
C  EXTRA ENTRY LHVECT RETURNS LENGTH ONLY
C  FIXUP FOR EVENTS WITH ONLY 2 INITIAL STATE PARTICLES INTRODUCED
C  FOR THE JET POINTERS
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C   27/08/83 310201406  MEMBER NAME  CPROD    (S)           FORTRAN
C ------------------ MACRO CPROD -----------------
C                                       4-VECTOR FORMAT FOR JADE
C
      COMMON/CPROD/ NEV,BEAM,PT,PHI,THETA,IFLAVR,
     *        NP,NC,NN,PP(4,500),XM(500),JCH(500),JTP(500),JP(500,2),
     *        NF,NCF,NNF,PF(4,300),XMF(300),ICF(300),ITF(300),
     *        PSTRT(3,300)
C
C ---------------END MACRO CPROD -----------------
C
*** PMF 15/10/99   DIMENSION IVECT(1) , IPF(4,300) , IXMF(300) , IPSTRT(3,300)
      DIMENSION IVECT(*) , IPF(4,300) , IXMF(300) , IPSTRT(3,300)
*** PMF (end)
C
      EQUIVALENCE (IPF(1,1),PF(1,1)) , (IXMF(1),XMF(1))
      EQUIVALENCE (IPSTRT(1,1),PSTRT(1,1)) , (IPHI,PHI) , (ITHETA,THETA)
C
      DATA L0 / 13 /, L1 / 10 /
C
C---------------------  C O D E  ---------------------------------------
C
C                  BANK HEADER
C
      IVECT( 1) = L0
      IVECT( 2) = L1
      IVECT( 3) = NEV
      IVECT( 4) = NF
      IVECT( 5) = NCF
      IVECT( 6) = NNF
      IVECT( 7) = IPHI
      IVECT( 8) = ITHETA
      IVECT( 9) = IFLAVR
      IVECT(10) = 0
      IVECT(11) = 0
      IVECT(12) = 0
      IVECT(13) = BEAM*1000.
      J = L0
C
C                  STORE DATA FOR EACH PARTICLE
C
      NJET = 0
      DO 10 K =1,NF
      IVECT(J+ 1) = IPF(1,K)
      IVECT(J+ 2) = IPF(2,K)
      IVECT(J+ 3) = IPF(3,K)
      IVECT(J+ 4) = IPF(4,K)
      IVECT(J+ 5) = IXMF(K)
      IVECT(J+ 6) = ICF(K)
      IVECT(J+ 7) = ITF(K)
      IVECT(J+ 8) = IPSTRT(1,K)
      IVECT(J+ 9) = IPSTRT(2,K)
      IVECT(J+10) = IPSTRT(3,K)
      IF( ITF(K) .LE. -100 ) NJET = NJET + 1
   10 J = J + L1
C                                           FIX UP POINTERS
      IF( NJET .EQ. 2 ) IVECT(12) = IVECT(11)
C
C----------------------------------------------------------
      ENTRY LHVECT( LENGTH )
C----------------------------------------------------------
C
C                                           LENGTH OF BANK
      LENGTH = NF*L1 + L0
      RETURN
      END
