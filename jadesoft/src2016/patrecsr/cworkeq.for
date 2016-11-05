C-------------------------------------------------------
C  MACRO CWORKEQ .... PATTERN RECOGNITION CWORK POINTERS
C-------------------------------------------------------
      EQUIVALENCE
C                POINTERS FOR FXYZ HIT ARRAY .. PRIMARY L/R SOLUTION
     +          (HPHT0,HPWRK( 1)),(HPHT9,HPWRK( 2)),(HLDHT,HPWRK( 3))
C                POINTERS FOR CWORK SINGLE TRACK PATR BANK
     +         ,(HPTR0,HPWRK( 4)),(HPTR9,HPWRK( 5)),(HLDTR,HPWRK( 6))
C                POINTERS FOR TRACK ELEMENT HIT LABEL ARRAY
     +         ,(HPHL0,HPWRK( 7)),(HPHL9,HPWRK( 8)),(HLDHL,HPWRK( 9))
C                POINTERS FOR FXYZ HIT ARRAY .. OPPOSITE L/R SOLUTION
     +         ,(HPHT0A,HPWRK(10)),(HPHT9A,HPWRK(11)),(HLDHTA,HPWRK(12))
C               POINTER LIMIT ON FXYZ HIT ARRAY
     +         ,(HPHTLM,HPWRK(13))
C               POINTERS FOR
     +         ,(HPTE0,HPWRK(14)),(HPTE9,HPWRK(15)),(HLDTE,HPWRK(16))
C-------------- END OF MACRO CWORKEQ ------------------
