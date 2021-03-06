C   07/06/96 606071805  MEMBER NAME  BDLMY    (S4)          FORTG1
      SUBROUTINE BDLM
C     BOS SUBPROGRAM =1.14=
#include "acs.for"
      COMMON/BCS/IW(1)
#include "star.for"
C
      IDLM=0
    1 IF(IN.LE.0) GOTO 24
      ILS=0
      JLOW=NLST
      NDEL=0
      DO 20 J=1,IN
      K=IW(NLST+J)
      I=IW(K)
      IF(I.EQ.0) GOTO 20
      IW(K)=0
      NAMDEL=NDEL
      IF(ILS.EQ.0) ILS=I
      GOTO 15
   10 I=IW(I-1)
   15 IF(I.EQ.0) GOTO 18
      NN=4+IW(I)
      JLOW=MIN0(I,JLOW)
      NDEL=NDEL+NN
      IW(I)=-NN
      IF(ILS-IW(ILS).EQ.I) GOTO 16
      ILS=I
      GOTO 10
   16 IW(ILS)=IW(ILS)-NN
      GOTO 10
   18 INAME=IW(INAMV+K)
      IF(NOTLOW(XNAME)) GOTO 20
      NCPL=NCPL-NDEL+NAMDEL
   20 CONTINUE
      IF(JLOW+NDEL-3.NE.NEXT) GOTO 22
C     LAST BANKS
      NK=1+((10*(NEXT-2))/NLAST)
      NHISTH(NK)=NHISTH(NK)+1
      NK=1+((10*NCPL)/NLAST)
      NK=MIN0(10,NK)
      NHISTL(NK)=NHISTL(NK)+1
      NCOL=NCOL+1
      NEXT=NEXT-NDEL
      GOTO 24
C     NOT LAST BANKS
   22 NFRE=NFRE+NDEL
      ILOW=MIN0(ILOW,JLOW)
C
   24 IF(IDLM.EQ.1.AND.NFRE.NE.0) CALL BGAR(IGA)
      GOTO 100
C
C     BDLG = BDLM + BGAR, BUT FASTER IN MOST CASES
      ENTRY BDLG
      IDLM=1
      GOTO 1
C
      ENTRY BPRM
      IF(IN.LE.0) GOTO 100
      DO 40 J=1,IN
      K=IW(NLST+J)
      I=IW(K)
      GOTO 35
   30 I=IW(I-1)
   35 IF(I.EQ.0) GOTO 40
      CALL BPRS(IW(I-3),IW(I-2))
      GOTO 30
   40 CONTINUE
      GOTO 100
C
      ENTRY BSLW
      IN=0
      IF(NS.EQ.0) GOTO 100
      DO 80 I=1,NS
      IF(IW(IMLST+I).EQ.0) GOTO 80
      IN=IN+1
      IW(NLST+IN)=IW(ISLST+I)
   80 CONTINUE
      GOTO 100
C
      ENTRY BSLT
      IN=NS
      IF(NS.EQ.0) GOTO 100
      DO 90 I=1,NS
   90 IW(NLST+I)=IW(ISLST+I)
      GOTO 100
C
      ENTRY BSLC
      NS=0
  100 RETURN
      END
