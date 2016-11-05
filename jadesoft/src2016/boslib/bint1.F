C   07/06/96 606071822  MEMBER NAME  BINT1    (S4)          FORTG1
      SUBROUTINE BINT(NA,NB,NC,ND)
C     BOS SUBPROGRAM =1.1=
#include "acs.for"
      COMMON/BCS/IW(1)
      REAL RW(1)
      EQUIVALENCE (RW(1),IW(1))
#include "ccs.for"
      REAL*8 DATE(2)
      EXTERNAL UCOPY2,BCHM,BPOS,BMLT,BDLS,BDLM,BDMP,BCHL,CCHL
      LOGICAL INIT/.FALSE./
      INTEGER JW(1)
#include "star.for"
C
C     LAYOUT OF BANK COMMON/BCS/
C
C     INDEX    IW( )             I   MEANING
C     ------------------------------------------------------
C         1    IW(1)             I+  POINTER TO FIRST BANKS
C         .    ...               I
C     NAMAX    IW(NAMAX)         I
C      NFRS    SPACE FOR BANKS   I+  BANK SPACE
C              ...               I
C      NLST    SPACE FOR BANKS   I
C              NALIST(1)         I+  LIST OF NAMES
C              ...               I
C     ISLST    NALIST(NLIST)     I
C              NSLIST(1)         I+  SPECIAL LIST OF NAMES
C              ...               I
C     IMLST    NSLIST(NLIST)     I
C              NMLIST(1)         I+  MARKER FOR SPECIAL LIST
C              ...               I
C              NMLIST(NLIST)     I
C     INAMV    NAME              I+  NAME
C              NAMES(1)          I+  NAMES
C              ...               I
C     IOLST    NAMES(NAMAX)      I
C              IOLIST(1)         I+  IO-MARKER
C              ...               I
C     IPLST    IOLIST(NAMAX)     I
C              IK(1)             I+  POINTER FOR NAMES
C              ...               I
C              IK(NAMAX)         I
C              IK(NAMAX+1)       I
C              ...               I
C     NLAST    IK(NAMAX+NPRIM)   I
C
C
      IF(INIT) GOTO 100
      INIT=.TRUE.
      CALL VZERO(ICOND,80)
      CALL VZERO(IUND,11)
      NLAST=NA
      NLAST1=NLAST
      CALL VZERO(IW,NLAST)
      NRECL=4000
      IF(NB.GT.0) NRECL=NB
      NDUMP=500
      IF(NC.GT.0) NDUMP=NC
      NDUMP1=NDUMP
      NSPL=NLAST/10
      IF(ND.GT.0) NSPL=ND
      CALL DAY(DATE(1),DATE(2))
      NZT=NTIME(DUMMY)
      WRITE(6,101) DATE,NLAST,NRECL,NDUMP,NSPL
      NAMAX=200
      NLIST=100
      NERRL=10
      NEOTP=2
      MARKWR=-1
      GOTO 10
C
      ENTRY BINT1(NA,NB,NC,ND)
C
      NAMAX=MAX0(10,NA)
      NLIST=MAX0(10,NB)
      NERRL=MAX0( 0,NC)
      NEOTP=ND
      WRITE(6,102) NAMAX,NLIST,NERRL,NEOTP
C
   10 NPRIM=(NAMAX/2)*2-1
   12 NPRIM=NPRIM+2
      J    =SQRT(FLOAT(NPRIM))
      DO 14 K=3,J,2
      L    =NPRIM/K
      IF(L*K.EQ.NPRIM) GOTO 12
   14 CONTINUE
C
      NAMAX1=NAMAX+1
      IPLST=NLAST-NAMAX-NPRIM
      IOLST=IPLST-NAMAX
      INAMV=IOLST-NAMAX
      IMLST=INAMV-NLIST-1
      ISLST=IMLST-NLIST
      NFRS =NAMAX1
      NEXT =NFRS
      NLST =ISLST-NLIST
      ILOW=NLST
      GOTO 100
C
      ENTRY BSPC(NA,NB,NC,ND)
      NA=NEXT
      NB=NLST-NEXT+1
      NC=NFRE
      ND=NS
      GOTO 100
C
      ENTRY BRNM(NA,NB,NC,ND)
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
  100 RETURN
C
  101 FORMAT(/'0START/BANK ORGANISATION PROGRAM   -   JOB DATE ',A8,
     1   ' TIME ',A8/'0',6X,'CALL BINT(',I6,',',5X,'NR OF WORDS IN'
     2 ,' COMMON/BCS/'/17X,I6,',',5X,'MAX NR OF WORDS ALLOWED IN A ',
     3   'BANK-RECORD'/17X,I6,',',5X,'NR OF WORDS PRINTED IN A DUMP'/
     4   17X,I6,')',5X,'NR OF WORDS FOR LOW PRIORITY BANK SPACE'/)
  102 FORMAT('0',5X,'CALL BINT1(',I6,',',5X,'MAX NR OF NAMES'/
     1  17X,I6,',',5X,'MAX LENGTH OF LIST OF NAMES'/
     2  17X,I6,',',5X,'MAX NR OF READ ERRORS'/
     3  17X,I6,')',5X,'MAX NR OF LISTINGS FOR INPUT-BANK ERRORS'/)
      END
