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
C called after every BREAD of the events (EVREAD); the output of SUPERV
C has the right order, so that it does nothing when it is called again.
C-----------------------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
#include "cdata.for"
      INTEGER IP, N, I, K
      INTEGER*2 HT
      INTEGER IPB, LVAL, ISTAT, NJ
      CHARACTER*8 CVAL
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
C The jet chamber bank JETC has the same order of the half-words (all its
C data are half-words), which the pattern recognition and the hit
C analysis need to read it (otherwise: WRONG POINTER IN JETC-BANK).
      IPB=IDATA(IBLN('JETC'))
      IF(IPB.GT.0) THEN
        NJ=IDATA(IPB)
        DO 15 I=1,NJ
          K=2*(IPB+I)-1
          HT=HDATA(K)
          HDATA(K)=HDATA(K+1)
          HDATA(K+1)=HT
   15   CONTINUE
      ENDIF
C The raw muon chamber bank MUEV is a stream of half-words as well.
      IPB=IDATA(IBLN('MUEV'))
      IF(IPB.GT.0) THEN
        NJ=IDATA(IPB)
        DO 16 I=1,NJ
          K=2*(IPB+I)-1
          HT=HDATA(K)
          HDATA(K)=HDATA(K+1)
          HDATA(K+1)=HT
   16   CONTINUE
      ENDIF
C
C Optional (environment variable JADE_REDO_PATR=1, real data only): the
C inner detector pattern recognition is done again. SUPERV does it only if
C the event has no bank PATR (and JHTL) and the activity flag (half-word 27
C of HEAD, nonzero for the events that were processed by REDUC1) is zero,
C so that the banks PATR and JHTL of the tape are deleted and the flag is
C cleared.
   20 CONTINUE
      IF(HDATA(2*IP+10).LE.100) RETURN
      CALL GET_ENVIRONMENT_VARIABLE('JADE_REDO_PATR',CVAL,LVAL,ISTAT)
      IF(ISTAT.NE.0 .OR. CVAL(1:1).NE.'1') RETURN
      HDATA(2*IP+27)=0
   30 IPB=IDATA(IBLN('PATR'))
      IF(IPB.GT.0) THEN
        CALL BDLS('PATR',IDATA(IPB-2))
        GO TO 30
      ENDIF
   40 IPB=IDATA(IBLN('JHTL'))
      IF(IPB.GT.0) THEN
        CALL BDLS('JHTL',IDATA(IPB-2))
        GO TO 40
      ENDIF
      RETURN
      END
