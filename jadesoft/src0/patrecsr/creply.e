C--------------------------------------------
C  MACRO CREPLY .... OUTPUT COMMON FOR ALPHAN
C--------------------------------------------
      LOGICAL*1 QREPLY
      COMMON/CREPLY/NREPLY,REPLY(20),QREPLY(20)
      INTEGER*4 IREPLY(20)
      EQUIVALENCE (IREPLY(1),REPLY(1))
C---------- END OF MACRO CREPLY -------------
