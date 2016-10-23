C   07/06/96 606071852  MEMBER NAME  INLINE   (S4)          FORTG1
C
C
C     ------------------------

C     FIRST PART (NAME)
      IW(INAMV)=NAME
      LFDI=MOD(IABS(NAMEV),NPRIM)+NAMAX1
    1 LFDI=IW(LFDI+IPLST)
      IF(IW(LFDI+INAMV).NE.IW(INAMV)) GOTO 1
      IF(LFDI.EQ.0) LFDI=IBLN(IW(INAMV))
C     LFDI = INDEX TO FIRST BANK (NAME,...)
C     -------------------------
C
C     -------------------------
C     SECOND PART (NR)
      LFDK=LFDI+1
      LFDI=IW(LFDI)
      GOTO 3
    2 LFDK=LFDI
      LFDI=IW(LFDI-1)
    3 IF(LFDI.EQ.0)        GOTO    XXX
      IF(IW(LFDI-2).LT.NR) GOTO 2
      IF(IW(LFDI-2).GT.NR) GOTO    XXX
C     LFDI = INDEX TO BANK (...,NR)
C     LFDK = POINTER TO THIS BANK
C     ------------------------
      END
