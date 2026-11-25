      SUBROUTINE HEADFX
C-----------------------------------------------------------------------
C Puts the two half-words of every word of the bank HEAD in the order
C that the programs expect (the order of the machine they run on).
C
C The half-words of a word of a BOS bank are in the order of the machine
C that wrote the bank. The real data come from big-endian machines, so
C that on a little-endian host the neighbouring half-word is found where
C the programs look for the run number, the date and time (half-words 3
C to 8), the beam energy (29) and the magnetic field (30). The Monte
C Carlo events of MCJADE are written on the same host and are in the right
C order. The beam energy (half-word 29, MeV) tells the order: if it is
C not plausible (500 to 30000) while half-word 30 would be, the two
C half-words of each word of the bank are exchanged. The routine is
C called after every BREAD of the events (EVREAD) and also swaps the banks
C MUEV, ALGL and ALGN of half-words; the output of SUPERV has the right
C order, so that it does nothing when it is called again.
C-----------------------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
#include "cdata.for"
      INTEGER IP, N, I, K
      INTEGER*2 HT
      INTEGER IPB, NJ, IB
C
      IP=IDATA(IBLN('HEAD'))
      IF(IP.LE.0) RETURN
      N=IDATA(IP)
      IF(N.LT.15) RETURN
C beam energy in the right place?
      IF(HDATA(2*IP+29).GE.500 .AND. HDATA(2*IP+29).LE.30000) GO TO 20
C or in the neighbouring half-word?
      IF(HDATA(2*IP+30).LT.500 .OR. HDATA(2*IP+30).GT.30000) GO TO 20
      DO 10 I=1,N
        K=2*(IP+I)-1
        HT=HDATA(K)
        HDATA(K)=HDATA(K+1)
        HDATA(K+1)=HT
   10 CONTINUE
C The banks of half-words of the tape data: the raw muon chambers MUEV,
C and the raw lead glass bank ALGL and the calibrated one ALGN (LGDCPY
C checks HDATA(NNP+3).EQ.1 and the pointers in the half-words 4 to 6 and
C gives up on a mismatch: no lead glass clusters at all).
      DO 18 IB=1,3
        IF(IB.EQ.1) IPB=IDATA(IBLN('MUEV'))
        IF(IB.EQ.2) IPB=IDATA(IBLN('ALGL'))
        IF(IB.EQ.3) IPB=IDATA(IBLN('ALGN'))
        IF(IPB.GT.0) THEN
          NJ=IDATA(IPB)
          DO 16 I=1,NJ
            K=2*(IPB+I)-1
            HT=HDATA(K)
            HDATA(K)=HDATA(K+1)
            HDATA(K+1)=HT
   16     CONTINUE
        ENDIF
   18 CONTINUE
   20 CONTINUE
      RETURN
      END
