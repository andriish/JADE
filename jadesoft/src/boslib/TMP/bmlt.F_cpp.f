C   07/06/96 606071824  MEMBER NAME  BMLT     (S4)          FORTG1
      SUBROUTINE BMLT(NL,LIST)
C     BOS SUBPROGRAM =0.4=
C   18/10/82            MEMBER NAME  ACS      (S)           FORTRAN
      COMMON/ACS/
     1   ICOND,NLAST,NDUMP,NSPL,NAMAX,NAMAX1,NLIST,NPRIM,NFRS,NLST,
     2   NEXT,NCI,NFRE,IN,KPOS,LIND,ILOW,LFDI,LFDK,NCPL,
     3   NHISTH(11),
     4        NCOL,NZT,IBFI,NLAST1,ISLST,IMLST,INAMV,IOLST,IPLST,
     5   NER(10),
     6   NRECL,NERRL,NRIN,NROUT,NS,NEXTI,NEXTA,NEXTB,ISAVB,NSAVB,
     7   NHISTL(11),
     8        MARKWR,NOUT(3),IASW,NPRE,IDUMMY(1),NEOTP,NDUMP1
      COMMON/BCS/IW(1)
C
      INTEGER LIST(1)
C
      IF(NL.LT.0) GOTO 20
      IN=NL
      IF(NL.EQ.0) GOTO 100
      DO 10 I=1,IN
   10 IW(NLST+I)=IBLN(LIST(I))
      GOTO 100
C
   20 IN=0
      IF(NS.EQ.0) GOTO 100
      MARK=LIST(1)
      DO 30 I=1,NS
      IF(MARK.EQ.0) GOTO 25
      IF(IW(IMLST+I).EQ.0) GOTO 30
   25 IN=IN+1
      IW(NLST+IN)=IW(ISLST+I)
   30 CONTINUE
  100 RETURN
      END
