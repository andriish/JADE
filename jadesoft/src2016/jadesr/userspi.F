C   24/11/78 408031344  MEMBER NAME  USERSPI  (JADESR)      FORTRAN
      SUBROUTINE USER(INDEX)
C---
C---     USER ROUTINE FOR CUTS AND INTERACTIVE DECISION MAKING.
C---
      IMPLICIT INTEGER*2 (H)
C
      COMMON // BLCOMM(15000)
#include "cgraph.for"
C
#include "cdata.for"
      COMMON /CHEADR/ HEAD(108)
CAV      COMMON / CADMIN / IEVTP,NRREAD,NRWRIT,NRERR
CAV   To assure all symbols have same size
#include "cadmin.for"
CAV

      DIMENSION IGG(30),JIGG(12)
CAV      DIMENSION IGG(30),JIGG(10)
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
C---     CHECK WHETHER CALL AT END OF JOB
      IF(INDEX.EQ.100) GOTO 9900
C
      GO TO (100,200,300,400,500,600,700,800,900,1000),INDEX
C---
C---     INDEX=0 INITIALIZATION.
C---
C
      DO 99 I = 1,12
99    JIGG(I) = 0
      DO 98 I = 1,30
98    IGG(I) = 0
      WRITE(6,112)
112   FORMAT(' REPAIR ROUTINE FOR DL8 ERROR; ',/,
     $ '   ALL JETC BANKS ARE CORRECTED; ',/,
     $ '   ALL ZVTX,PATR AND JHTL BANKS ARE DELETED',/,
     $ '   Z-VERTEX AND PATREC (SLOW VERSION)  ARE BOTH REDONE')
C
C     INITIALIZE INPUT OUTPUT UNITE
C
      CALL JBWDL8(2,3)
C
C    UNIT 2 = INPUT,    UNIT 3 = OUTPUT
C
      INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
100   CONTINUE
      JIGG(1) = JIGG(1) + 1
      INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
200   CONTINUE
C                                    SELECT EVENT
      JIGG(2) = JIGG(2) + 1
      CALL SPITZ(*11)
C
C RETURN1 = GO TO 11, WRITE EVENT
C   IF OUTSIDE RUN LIMITS 16803,17326   OR IF NO HEAD OR JETC BANK.
C
3010  CONTINUE
      IPPATR = IDATA(IBLN('PATR'))
      IF(IPPATR.LE.0) GO TO 3011
      NRPATR = IDATA(IPPATR - 2)
      CALL BDLS('PATR',NRPATR)
      GO TO 3010
3011  IPJHTL = IDATA(IBLN('JHTL'))
      IF(IPJHTL.LE.0) GO TO 3012
      NRJHTL = IDATA(IPJHTL - 2)
      CALL BDLS('JHTL',NRJHTL)
      GO TO 3011
3012  IPZVTX = IDATA(IBLN('ZVTX'))
      IF(IPZVTX.LE.0) GO TO 210
      NRZVTX = IDATA(IPZVTX - 2)
      CALL BDLS('ZVTX',NRZVTX)
      GO TO 3012
C
210   INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
300   CONTINUE
C                                    LEAD GLASS CALIBRATION DONE
      JIGG(3) = JIGG(3) + 1
      INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
400   CONTINUE
C                                    JETC CALIBRATED
C                                    ZVERTEX CALCULATED
      JIGG(4) = JIGG(4) + 1
      INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
500   CONTINUE
C                                    PATREC PERFORMED
      JIGG(5) = JIGG(5) + 1
      GO TO 11
C                                     WRITE EVENT
C -------------------------------------------------------------------
600   CONTINUE
C                                    CLUSTER ANALYSIS PERFORMED
      JIGG(6) = JIGG(6) + 1
      INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
700   CONTINUE
      JIGG(7) = JIGG(7) + 1
      INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
800   CONTINUE
      JIGG(8) = JIGG(8) + 1
      INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
900   CONTINUE
      JIGG(9) = JIGG(9) + 1
      INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
1000  CONTINUE
      JIGG(10) = JIGG(10) + 1
      INDEX=INDEX+1
      RETURN
C -------------------------------------------------------------------
C---     END OF JOB: FINAL CALCULATIONS + PRINTOUT
9900  CONTINUE
      WRITE(JUSCRN,9901) JIGG
      WRITE(JUSCRN,9902) IGG
9901  FORMAT(' JIGG ',12I10)
9902  FORMAT(' IGG ',10I10)
      RETURN
C---
C---     RETURNS FOR STEERING ANALYSIS TO DESIRED NEXT STEP.
C---     'GO TO 1' MEANS REJECT EVENT AND GO TO NEXT EVENT.
C---     'GO TO 11' MEANS ACCEPT EVENT, WRITE IT AND GO TO NEXT EVENT
C---     'GO TO 12' MEANS END THE JOB, WRITE FINAL RESULTS AND PLOTS
C---
1     INDEX = 1
      RETURN
2     INDEX = 2
      RETURN
3     INDEX = 3
      RETURN
4     INDEX = 4
      RETURN
5     INDEX = 5
      RETURN
6     INDEX = 6
      RETURN
7     INDEX = 7
      RETURN
8     INDEX = 8
      RETURN
9     INDEX = 9
      RETURN
10    INDEX = 10
      RETURN
11    INDEX = 11
      RETURN
12    INDEX = 12
      RETURN
      END
      SUBROUTINE JBWDL8(IDUM1,IDUM2)
C
      WRITE (6,111)
111   FORMAT(' DUMMY JBWDL8 WAS CALLED ')
      RETURN
      END
