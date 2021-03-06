      COMMON/CWORK/MARK,SADC(32,2),CMAP(10,9),
     1CAND(3),SIGX,SIGY,SIGEN,
     1CLUS(9,2),NCLST,NNEI,ISTMZ,ISTPZ,IENDMZ,IENDPZ,IMC,
     1CATAG(192)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C CWORK - WORKSPACE USED ONLY ONCE PER EVENT FOR INTERNAL PROCESSING
C ==================================================================
C
C
C MARK -> WHICH 'MARK' OF TAGGER - 1 = 1981,2
C                                - 2 = 1983 ONWARDS
C
C CATAG  -> CONTAINS THE ADC CONTENTS UNPACKED FROM ATAG
C
C SADC ->   COMMON FOR ADC'S AFTER SORTING  (SORT 1)
C
C CMAP(I,1....9) -> ADDRESS OF ADC'S IN CLUSTER I,SORT23 PUTS THESE IN
C                   ORDER OF ENERGY.
C
C CAND(3) -> X,Y,AND ENERGY OF A FOUND CLUSTER IN AFTER CLSPS
C
C SIGX,SIGY,SIGEN -> ERROR ON X,Y,ENERGY AFTER CLSPS
C
C FLIST(48) ->  NUMBER OF TIMES ADC(I) HAS BEEN FOUND IN A CLUSTER
C                   CLUSTER MAP FROM SORT23 WITH ENERGY IN CLUS(I,2)
C
C CLUS(9,2) -> ADC ADDRESS AND CONTENTS OF CLUSTERS - SORTED BY ENERGY
C
C
C NCLST -> NUMBER OF CLUSTERS THIS END
C ISTMZ -> POINTER TO START OF -Z DATA IN CATAG (ALWAYS 1)
C ISTPZ -> POINTER TO START OF +Z DATA IN CATAG (EITHER 33 OR 25)
C IENDMZ -> POINTER TO END OF -Z DATA IN CATAG (EITHER 32 OR 24)
C IENDPZ -> POINTER TO END OF +Z DATA IN CATAG (EITHER 64 OR 48)
C
C A.J.FINCH 24/2/84
C MODIFIED 12/3/84 CATAG PUT TO END AND INCREASED TO 192
C  TO ALLOW IT TO BE USED FOR 1979,80 TAGGER IN GRAPHICS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
