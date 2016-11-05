C====  MACRO CWORKZV  ==================================
C-------------------------------------------
C   RESULTS + INTERM. STORAGE OF ZVERTF
C   P. STEFFEN (79/01/21)
C---------------------------------------------
      COMMON /CWORK/ FZRSLT(12)
     ,             , HUFLO,HOFLO,MAXZ,HIST(100)
     ,             , HPTSEC(98)
     ,             , NZ1(16),NZ2(16), HLB1(8),HLB2(8)
     ,        , HZ1(8,16),HZ2(16,16), FI1(8,16),FI2(8,16),HTMP(100)
                     INTEGER*4 HPTSEC
                     INTEGER IZRSLT(12)
                     EQUIVALENCE (IZRSLT(1),FZRSLT(1))
C
C==  ENDMACRO CWORKZV  ========================================
