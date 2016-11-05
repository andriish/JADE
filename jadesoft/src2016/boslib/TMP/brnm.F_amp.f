C   07/06/96 606071832  MEMBER NAME  BRNM     (S4)          FORTG1
      SUBROUTINE BRNM(NA,NB,NC,ND)
C     BOS SUBPROGRAM =1.1=
#include "acs.for"
      COMMON/BCS/IW(1)
      REAL RW(1)
      EQUIVALENCE (RW(1),IW(1))
#include "star.for"
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
