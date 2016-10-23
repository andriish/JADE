C   20/09/79 102161800  MEMBER NAME  PRPATR   (PATRECSR)    FORTRAN
      SUBROUTINE PRPATR
C
C   ****************************
C   *    PRINT BANK 'PATR'     *
C   ****************************
C
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
C
C                              STOP EVENT ANALYSIS AFTER PATREC
C
C     PRINT OUT OF TRACK BANKS
      IP = IDATA(IBLN('PATR'))
   51 IF(IP.LE.0) GOTO 52
        PRINT 2101,IDATA(IP-3),IDATA(IP-2),IDATA(IP-1),IDATA(IP),
     ,             IDATA(IP+1),IDATA(IP+2),IDATA(IP+3),IP
 2101   FORMAT(1H0,A4,8I12)
        IP0 = IP-3
        IP9 = IP+IDATA(IP+1)
        PRINT 2201,(IDATA(I1),I1=IP0,IP9)
 2201   FORMAT(1H0,A4,20I6)
        NTR = IDATA(IP+2)
        LDTR = IDATA(IP+3)
        IF(NTR.LE.0) GOTO 59
          JP0 = IP+IDATA(IP+1) + 1
          JP9 = JP0 + LDTR*NTR - 1
          DO 56 JP1 = JP0,JP9,LDTR
            I0 = JP1
            I9 = I0 + LDTR - 1
            PRINT 2202,(IDATA(I1),I1=I0,I9)
 2202 FORMAT(1H0,2I3,I8,2(I4,3F6.1,3F6.3),
     ,     /,14X,I3,4E13.5,F6.2,I3,4E13.5,
     ,     /,14X,I3,2F8.3,F6.1,I3,10X,6I3,8I6,2X,Z4)
  56      CONTINUE
  59    CONTINUE
        IP = IDATA(IP-1)
      GOTO 51
  52  CONTINUE
C
      RETURN
C
      END
