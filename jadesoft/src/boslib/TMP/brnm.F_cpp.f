C 07/06/96 606071832 MEMBER NAME BRNM (S4) FORTG1
      SUBROUTINE BRNM(NA,NB,NC,ND)
C BOS SUBPROGRAM =1.1=
C 18/10/82 MEMBER NAME ACS (S) FORTRAN
      COMMON/ACS/
     1 ICOND,NLAST,NDUMP,NSPL,NAMAX,NAMAX1,NLIST,NPRIM,NFRS,NLST,
     2 NEXT,NCI,NFRE,IN,KPOS,LIND,ILOW,LFDI,LFDK,NCPL,
     3 NHISTH(11),
     4 NCOL,NZT,IBFI,NLAST1,ISLST,IMLST,INAMV,IOLST,IPLST,
     5 NER(10),
     6 NRECL,NERRL,NRIN,NROUT,NS,NEXTI,NEXTA,NEXTB,ISAVB,NSAVB,
     7 NHISTL(11),
     8 MARKWR,NOUT(3),IASW,NPRE,IDUMMY(1),NEOTP,NDUMP1
      COMMON/BCS/IW(1)
      REAL RW(1)
      EQUIVALENCE (RW(1),IW(1))
C 30/04/96 604301841 MEMBER NAME STAR (BOSLIB.S) FORTRAN
      REAL STAR/Z0000005C/,FF/Z000000FF/
      EQUIVALENCE (XNAME,INAME)
      LOGICAL NOTLOW
      REAL AND
      EXTERNAL AND
      NOTLOW(XNAME)=AND(XNAME,FF).NE.STAR
      CALL BLOC(IND,NA,NB,*100)
      KK=LFDK
      IS=IW(KK-1)
      IW(KK-1)=IW(LFDI-1)
      CALL BLOC(JND,NC,ND,*20)
      IW(KK-1)=IS
      GOTO 100
   20 IW(IND-1)=IW(LFDK-1)
      IW(LFDK-1)=IND
      IW(IND-3)=NC
      IW(IND-2)=ND
      NN=0
      INAME=NA
      IF(NOTLOW(XNAME)) GOTO 22
      NN=NN-IW(IND)-4
   22 INAME=NC
      IF(NOTLOW(XNAME)) GOTO 24
      NN=NN+IW(IND)+4
   24 NCPL=NCPL+NN
      GOTO 100
C
      ENTRY BSPC(NA,NB,NC,ND)
      NA=NEXT
      NB=NLST-NEXT+1
      NC=NFRE
      ND=NS
  100 RETURN
C
      END
