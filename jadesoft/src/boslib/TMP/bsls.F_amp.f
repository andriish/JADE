C   07/06/96 606071835  MEMBER NAME  BSLS     (S4)          FORTG1
      SUBROUTINE BSLS(ISA)
C     BOS SUBPROGRAM =1.21=
#include "acs.for"
      COMMON/BCS/IW(1)
      CALL BDLS('+SPL',ISA)
      IF(NS.EQ.0) GOTO 100
      CALL BCRE(IND,'+SPL',ISA,2*NS,*90,IER)
      CALL BSTR(IND,IW(ISLST+1),NS)
      CALL BSTR(IND+NS,IW(IMLST+1),NS)
      GOTO 100
C
      ENTRY BSLR(ISA)
      NS=0
      CALL BLOC(IND,'+SPL',ISA,*100)
      NS=IW(IND)/2
      CALL BSTR(ISLST,IW(IND+1),NS)
      CALL BSTR(IMLST,IW(IND+1+NS),NS)
      CALL BDLS('+SPL',ISA)
      GOTO 100
C
   90 CALL BDMPA(90)
  100 RETURN
      END