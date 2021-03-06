C   09/10/79 203031804  MEMBER NAME  USCOPY   (JADESR)      FORTRAN
      SUBROUTINE USER(INDEX)
C---
C---     USER ROUTINE FOR CUTS AND INTERACTIVE DECISION MAKING.
C---
      IMPLICIT INTEGER*2 (H)
C
      COMMON /CTLIM/ ISECLF
C
#include "cdata.for"
#include "cheadr.for"
      EQUIVALENCE (HHEADR(18),HRUN) , (HHEADR(19),HEV)
      EQUIVALENCE (HHEADR(35),HWRT)
C
      DIMENSION NCOUNT(20)
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
      IF(INDEX.EQ.2) GOTO 1002
C
      IF(INDEX.EQ.0) CALL SETSL(NCOUNT(1),0,80,0)
      ISECLF= 0
      INDEX=INDEX+1
      IF(INDEX.LT.100) RETURN
        PRINT 2002, NREC,NRUN,NEV
 2002   FORMAT(' LAST EVENT WAS:',3I6)
        PRINT 2003, NCOUNT
 2003   FORMAT('0NCOUNT =',10I8)
      RETURN
C
 1002 CONTINUE
      DATA NREC/ 0/
      NREC = NREC + 1
      NRUN = HRUN
      NEV  = HEV
      IF(NREC.LT. 1) GOTO 1
      IF(HWRT.NE. 1 .AND. HWRT.NE. 9) GOTO 1
      PRINT 2901, NREC, HRUN,HEV,HWRT
 2901 FORMAT(1H ,'EVENT =',6I6)
C
      INDEX=11
      RETURN
    1 INDEX=1
      RETURN
C
      END
