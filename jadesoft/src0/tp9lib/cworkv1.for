C   11/01/88 801112021  MEMBER NAME  CWORKV1  (S)        M  MACROFOR
C
C                            Common for vertex fit in TP

      COMMON /CWORK/ NTRK, MARK(128),MRKSUM,
     1               CS(128),SN(128),X0(128),Y0(128), C(128),
     2               XX(128),YY(128),  XQ(128),
     3               A(128),B(128), A2B2(128), DIS2(128), SIG2(128),
     4               NL, AL(50),BL(50),SIG2L(50), COST,
     5               XV,YV,  XYFIT(6), MODE,NTRY, IER,
     6               DIS2FT(128), SQA2B2(128),
     7               LCOUNT(128),LINKMP(6,128),
     8               LINKPC(128),MAPPRT(128),DISPRT(128)

