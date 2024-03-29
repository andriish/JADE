C   07/06/96 606071819  MEMBER NAME  BINP1OLD (S4)          FORTG1
      SUBROUTINE BINP(NRW,AR)
C     BOS SUBPROGRAM =3.7=
#include "acs.for"
      COMMON/BCS/IW(1)
      REAL RW(1)
      EQUIVALENCE (RW(1),IW(1))
#include "star.for"
      INTEGER IONE/1/
      EQUIVALENCE (IONE,ONE)
      INTEGER AR(1)
C     CREATE POINTER FOR NEW BANKS
      IF(NRW.LT.4) GOTO 100
      IF(NEXT+NRW.GT.NLST) GOTO 100
      CALL UCOPY2(AR,IW(NEXT),NRW)
      GOTO 2
      ENTRY BFRR(NRW,AR)
    2 ICD=0
      NRIN=NRIN+1
      IEB=0
      NEXTS=NEXT
      IF(NRW.LT.4) GOTO 81
      NS=0
      NTOT=NRW
C     CHECK BANK LENGTH AND GET INDEX FOR NAME
   10 LENG=IW(NEXT+3)
      IF(LENG.LT.0) GOTO 83
      LENG=LENG+4
      IF(LENG.GT.NTOT) GOTO 83
      LNAM=IW(NEXT)
      IW(INAMV)=LNAM
      LFDI=MOD(IABS(LNAM),NPRIM)+NAMAX1
    1 LFDI=IW(LFDI+IPLST)
      IF(IW(LFDI+INAMV).NE.IW(INAMV)) GOTO 1
      IF(LFDI.EQ.0) LFDI=IBLN(IW(INAMV))
      KL  =LFDI
      IF(NS.GE.NLIST) CALL BDMPA(35)
      NS=NS+1
      RW(IOLST+KL)=OR(RW(IOLST+KL),ONE)
      IW(ISLST+NS)=KL
      IW(IMLST+NS)=1
      IF(IW(KL).EQ.0) GOTO 50
C     BANK ALREADY THERE, CHECK THIS RECORD
      DO 4 IS=1,NS
      IF(IW(ISLST+NS).EQ.IW(ISLST+IS)) GOTO 6
    4 CONTINUE
      IS=NS
    6 IF(IS.EQ.NS) GOTO 12
C     IN THIS RECORD
      NS=NS-1
      I=KL+1
    8 I=IW(I-1)
      IF(IW(I-1).NE.0) GOTO 8
      IF(IW(I-2).GE.IW(NEXT+1)) GOTO 84
      K=I
      INAME=LNAM
      GOTO 60
C     DELETE BANKS OF THE SAME NAME
   12 IEB=1
      IF(NEOTP.LE.1) GOTO 15
      NEOTP=NEOTP-1
      WRITE(6,102) LNAM
   15 INAME=IW(INAMV+KL)
      LENGS=0
      I=IW(KL)
      IW(KL)=0
   20 LENG=4+IW(I)
      LENGS=LENGS+LENG
      IW(I)=-LENG
      NFRE=NFRE+LENG
      I=IW(I-1)
      IF(I.NE.0) GOTO 20
      LENG=IW(NEXT+3)+4
      IF(NOTLOW(XNAME)) GOTO 50
      NCPL=NCPL-LENGS
      GOTO 50
C     CHECK LENGTH OF BANK
   30 LENG=IW(NEXT+3)
      IF(LENG.LT.0) GOTO 82
      LENG=LENG+4
      IF(LENG.GT.NTOT) GOTO 83
C     CHECK BANK NR
      IF(IW(NEXT+1)-IW(K-2)) 84,40,60
C     SAME, APPEND
   40 IW(K)=IW(K)+LENG-4
      CALL UCOPY2(IW(NEXT+4),IW(NEXT),NTOT-4)
      NEXT=NEXT+LENG-4
      IF(NOTLOW(XNAME)) GOTO 70
      NCPL=NCPL+LENG-4
      GOTO 70
C     NEW NAME
   50 K=KL+1
      INAME=IW(INAMV+KL)
C     LARGER NR
   60 IW(K-1)=NEXT+3
      K=NEXT+3
      IW(K-1)=0
      NEXT=NEXT+LENG
      IF(NOTLOW(XNAME)) GOTO 70
      NCPL=NCPL+LENG
C     CHECK COMPLETE RECORD
   70 NTOT=NTOT-LENG
      IF(NTOT.EQ.0) GOTO 90
      IF(NTOT.LT.4) GOTO 85
      IF(IW(NEXT).EQ.LNAM) GOTO 30
      GOTO 10
C     ERRORS
C     RECORD END WRONG
   85 ICD=ICD+1
C     WRONG ORDER IN BANK NR
   84 ICD=ICD+1
C     BANK LENGTH TO LARGE
   83 ICD=ICD+1
C     BANK LENGTH NEGATIVE
   82 ICD=ICD+1
C     RECORD LENGTH LESS THAN 4
   81 ICD=ICD+1
      NER(ICD)=NER(ICD)+1
      IF(NEOTP.LE.0) GOTO 89
      WRITE(6,101) ICD
      CALL UWP(IW(NEXTS),1,NRW)
      NEOTP=NEOTP-1
   89 CALL BSLT
      CALL BDLM
      NEXT=NEXTS
      NS=0
      GOTO 100
   90 IF(IEB.EQ.0) GOTO 100
      NER(10)=NER(10)+1
C
  100 RETURN
  101 FORMAT('0LISTING OF RECORD WITH INPUT-BANK ERROR OF CODE',I2/)
  102 FORMAT('0READ BANK NAME',1X,A4,' ALREADY PRESENT'/)
      END
