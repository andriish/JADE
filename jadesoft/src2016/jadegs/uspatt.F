C   10/05/79 105081149  MEMBER NAME  USPATT   (JADEGS)      FORTRAN
      SUBROUTINE USER(INDEX)
C---
C---     USER ROUTINE FOR CUTS AND INTERACTIVE DECISION MAKING.
C---
      IMPLICIT INTEGER*2 (H)
C
C PMF 05/11/98     LOGICAL DSPDTL,SSTPS,PSTPS,FREEZE
C This declaration is just contained in macro cgraph.for 
      DIMENSION KU(2)
C
      COMMON /CTLIM/ ISECLF
C
#include "cgraph.for"
#include "cpatlm.for"
C
#include "cheadr.for"
#include "cdata.for"
#include "ccycp.for"
C
#include "cworkpr.for"
#include "cworkeq.for"
C
#include "cjdrch.for"
C
CAV      COMMON/ CADMIN / IEVTP,NRREAD,NRWRIT,LBREAD(4)
CAV   To assure all symbols have same size
#include "cadmin.for"
CAV

CAV      LOGICAL*1 LBREAD
C
#include "cbin.for"
C---
C        INDEX=0   INITIAL CALL, BEFORE FIRST EVENT READ.
C              1   CALLED AT THE BEGINNING OF EACH NEW RUN.
C              2   CALLED IMMEDIATELY AFTER EVENT IS READ INTO CDATA.
C              3   LEAD GLASS ENERGIES HAVE BEEN COMPUTED.
C              4   FAST Z VERTEX RECONSTRUCTION HAS BEEN DONE.
C              5   INNER DETECTOR PATTERN RECOGNITION HAS BEEN RUN.
C              6   ENERGIES CLUSTERS IN THE LEAD GLASS HAVE BEEN FOUND.
C              7   TRACKS AND CLUSTERS HAVE BEEN ASSOCIATED.
C              8   MUON CHAMBER TRACKING HAS BEEN DONE.
C              9   MUON AND INNER DETECTOR TRACKS HAVE BEEN ASSOCIATED.
C             10   UNUSED
C---
C---     CHECK IF CALL AT END OF JOB
      IF(INDEX.EQ.100) GOTO 1100
C
      GO TO (1001,1002,1003,1004,1005,1006,1007,1008,1009,1010),INDEX
C---
C---     INDEX=0 INITIALIZATION.
C---
C
      ISECLF= 0
      INDEX=INDEX+1
      RETURN
C
 1001 CONTINUE
C        TIMDEL(1,1) = .005900
C        TIMDEL(2,1) = .005900
C        TIMDEL(1,2) = .005900
C        TIMDEL(2,2) = .005900
C        TIMDEL(1,3) = .005900
C        TIMDEL(2,3) = .005900
C        TZERO(1) = -3.
C        TZERO(2) = -3.
C        TZERO(3) =  0.
C        DRIDEV = 0.
C        DRISIN = 0.
C        DRICOS = 1.
C        XYF(1) = 0.4 / SQRT(12.)
C        XYF(3) = 1.0
C        XYF(8) = 1.0
C        XYF(9) = 1.0
C
         IEVTP = 0
C        CALL INPATR
C        CALL INPATC
C     CALL INBTLM
      INDEX=INDEX+1
      RETURN
C
 1002 CONTINUE
C     DATA JHTL,PATR /'JHTL','PATR'/
C     CALL JBDLM(PATR,IER)
C     CALL JBDLM(JHTL,IER)
C                                                 SELECT EVENT
      DATA NREC/ 0/
      NREC = NREC + 1
      IF(NREC.LT. 1) GOTO 1
      PRINT 2991, HHEADR(17),HHEADR(18),HHEADR(19),NREC
 2991 FORMAT(' **********************   EVENT:',4I6,'   *************')
C                                       CHECK IF HITS IN ID OR LG-ENERGY
      IF(IDATA(IBLN('JETC')).LE.0) GOTO 1
        NWRD2 = NWORD*2
C     PRINT 2901, NREC, NWORD,(HDATA(I1), I1=1,NWRD2)
2901    FORMAT(1H1,'EVENT =',2I6,/,(1X,20I6))
      INDEX=INDEX+1
      RETURN
C
 1003 CONTINUE
      INDEX=INDEX+1
      RETURN
C
 1004 CONTINUE
C                                                 CHECK IF GOOD VERTEX
      INDEX=INDEX+1
      RETURN
C
 1005 CONTINUE
C                              STOP EVENT ANALYSIS AFTER PATREC
C
      CALL USTART(KU,1,0,1)
      CALL PATREC(0)
      CALL USTOP(KU,1,0,1)
      DTM  = KU(2) * .026
      IPPATR = IDATA(IBLN('PATR'))
      NTR = IDATA(IPPATR+2)
      DTMT = 0.
      IF(NTR.NE.0) DTMT = DTM / NTR
      PRINT 2904, NREC,DTM,DTMT,NTR
 2904 FORMAT('0NREC:',I6,',    TIME(PATREC):',F8.3,' :',F7.3,' (',I2,
     ,       ' TRACKS)')
C     PRINT OUT OF TRACK BANKS
      DATA NPR /0/
      NPR = NPR + 1
      IF(NPR.LE. 4)
     ,CALL PRPATR
C
51    GOTO 11
C                                       CHANGE LIMITS + REPEAT PATREC
      INDEX=INDEX+1
      RETURN
C
 1006 CONTINUE
      INDEX=INDEX+1
      RETURN
C
 1007 CONTINUE
      INDEX=INDEX+1
      RETURN
C
 1008 CONTINUE
      INDEX=INDEX+1
      RETURN
C
 1009 CONTINUE
      INDEX=INDEX+1
      RETURN
C
 1010 CONTINUE
      INDEX=INDEX+1
      RETURN
C---     END OF JOB: FINAL CALCULATIONS + PRINTOUT
 1100 CONTINUE
      NPR = NPR - 1
      PRINT 2999, NREC,NPR
 2999 FORMAT('0END OF JOB:',I6,' EVENTS READ,',I6,' EVENTS WRITTEN')
      STOP
      RETURN
C---
C---     RETURNS FOR STEERING ANALYSIS TO DESIRED NEXT STEP.
C---     'GO TO 1' MEANS REJECT EVENT AND GO TO NEXT EVENT.
C---
    1 CONTINUE
      INDEX=1
      RETURN
    2 CONTINUE
      INDEX=2
      RETURN
    3 CONTINUE
      INDEX=3
      RETURN
    4 CONTINUE
      INDEX=4
      RETURN
    5 CONTINUE
      INDEX=5
      RETURN
    6 CONTINUE
      INDEX=6
      RETURN
    7 CONTINUE
      INDEX=7
      RETURN
    8 CONTINUE
      INDEX=8
      RETURN
    9 CONTINUE
      INDEX=9
      RETURN
   10 CONTINUE
      INDEX=10
      RETURN
   11 CONTINUE
      INDEX=11
      RETURN
      END
