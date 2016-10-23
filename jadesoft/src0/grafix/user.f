C   18/01/84 401181641  MEMBER NAME  USER     (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE USER( INDEX )
C-----------------------------------------------------------------------
C
C   AUTHOR:   L. O'NEILL   ?/??/78 :  USER ROUTINE FOR JADEZ
C
C   LAST MOD: C. BOWDERY  13/12/83 :  SCRATCH MUON BANKS BEFORE LEVEL 9
C
C
C        USER ROUTINE FOR CUTS AND INTERACTIVE DECISION MAKING.
C        THIS IS THE INTERACTIVE VERSION FOR THE GRAPHICS PROGRAM.
C
C-----------------------------------------------------------------------
C
C        INDEX IS PASSED FROM SUPERV TO INDICATE WHICH LEVEL HAS BEEN
C        COMPLETED. THE MEANING OF THE VALUES IS EXPLAINED BELOW.
C
C        INDEX=0   INITIAL CALL, BEFORE FIRST EVENT READ.
C              1   CALLED AT THE BEGINNING OF EACH NEW RUN.
C              2   CALLED IMMEDIATELY AFTER EVENT IS READ INTO /BCS/.
C              3   LEAD GLASS ENERGIES HAVE BEEN COMPUTED.
C              4   FAST Z VERTEX RECONSTRUCTION HAS BEEN DONE + JETCAL
C              5   INNER DETECTOR PATTERN RECOGNITION HAS BEEN RUN.
C              6   ENERGY CLUSTERS IN THE LEAD GLASS HAVE BEEN FOUND.
C              7   TRACKS AND CLUSTERS HAVE BEEN ASSOCIATED.
C              8   UNUSED LEVEL
C              9   MUON ANALYSIS HAS BEEN DONE
C             10   UNUSED LEVEL
C            100   END OF JOB CALL
C
C   ON RETURN, IF INDEX =  1 : THE SUPERVISOR WILL DROP CURRENT EVENT
C                              AND READ THE NEXT ONE.
C                         11 : EVENT WILL BE WRITTEN OUT AND NEW EVENT
C                              WILL BE READ
C                         12 : JOB WILL BE TERMINATED NORMALLY
C
C   OTHERWISE THE SUPERVISOR WILL GO TO THE LEVEL GIVEN BY "INDEX" ON
C   RETURN FROM "USER". THUS, TO CONTINUE WITH THE ANALYSIS, THIS
C   ROUTINE MUST INCREMENT "INDEX" BEFORE PASSING CONTROL BACK.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cgraph.for"
C
#include "cdata.for"
C
      DATA NREC / 0 /
C
C------------------  C O D E  ------------------------------------------
C
C                            CHECK WHETHER CALL AT END OF JOB
C
      IF( INDEX .EQ. 100 ) GO TO 1100
C
C                            CHECK FOR INDEX = 0
C
      IF( INDEX .GT. 0 ) GO TO 1
C
C                - 0 -       INITIALISATION.
C
C                            CLEAR FLAGS BY WHICH THE ANALYSIS  PROGRAM
C                            REQUESTS SCANNER HELP.
C
      DO   1000  I = 1,10
        PSTPS(I)=.FALSE.
 1000 CONTINUE
C
C                            DUMMY, TEMPORARY EVENT TYPE LABEL.
C
      LABEL = -1
C
      INDEX = INDEX + 1
      RETURN
C
C                            HANDLE POSITIVE INDEX VALUES.
C                            JUMP TO APPROPRIATE SECTION USING INDEX
C                            E.G. IF LEVEL 7 HAS JUST BEEN DONE --> 100700000810
C                            CARRY OUT TASKS FOR THE LEVEL THEN
C                            GO AND CALL SCANNR (IF A "STOP" REQUESTED)
C
  1   GO TO (1001,1002,1003,1004,1005,1006,1007,1008,1009,1010),INDEX
C
C                - 1 -       NEW RUN
C
 1001 ILSTOP = 0
      GO TO 3000
C
C                - 2 -       EVENT HAS BEEN READ IN
C
C                            CLEAR FLAGS BY WHICH THE ANALYSIS  PROGRAM
C                            REQUESTS SCANNER HELP.
C
 1002 DO   2000  I = 1,10
        PSTPS(I) = .FALSE.
 2000 CONTINUE
      ILSTOP = 1
C
      NREC = NREC + 1
      GO TO 3000
C
C                - 3 -       LG ENERGIES HAVE BEEN COMPUTED
C
 1003 CONTINUE
      GO TO 3000
C
C                - 4 -       JET CHAMBER CALIBRATED & FAST Z VERTEX DONE
C
 1004 CONTINUE
      GO TO 3000
C
C                - 5 -       PATREC HAS BEEN DONE
C
 1005 CONTINUE
      GO TO 3000
C
C                - 6 -       LG CLUSTERS FOUND
C
 1006 CONTINUE
      GO TO 3000
C
C                - 7 -       LG CLUSTERS JOINED TO ID TRACKS
C
 1007 CONTINUE
      GO TO 3000
C
C                - 8 -       NOTHING FURTHER HAS BEEN DONE
C
 1008 CONTINUE
      GO TO 3000
C
C                - 9 -       MUON ANALYSIS HAS BEEN DONE
C
 1009 CONTINUE
      GO TO 3000
C
C               - 10 -       NOTHING FURTHER HAS BEEN DONE
C
 1010 CONTINUE
      GO TO 3000
C
C                            END OF JOB: FINAL CALCULATIONS +  PRINTOUT
C
 1100 CONTINUE
      RETURN
C
C                            IF A "STOP" HAS BEEN REQUESTED, THEN CALL
C                            S/R SCANNR TO HANDLE THE INTERACTIVE
C                            DIALOGUE WITH THE "HUMAN USER" !
C
 3000 IF( SSTPS(INDEX) .OR. PSTPS(INDEX) ) CALL SCANNR( INDEX,IRFLG )
      IF( ILSTOP .EQ. 0 ) GO TO 3001
      KNTR = 0
C
 3002 KNTR = KNTR + 1
      NSET = LSTPS(KNTR)
      IF( NSET .EQ. 0 ) GO TO 3001
      IF( NSET .NE. LABEL ) GO TO 3003
C
      WRITE(JUSCRN,101) LABEL,INDEX
  101 FORMAT('PROGRAM HALTED BY LABEL=',I5,'   USER INDEX=',I5)
C
      CALL TRMOUT(80,'DO YOU WISH TO STOP AT SUBSEQUENT USER ENTRIES^')
      CALL TRMOUT(80,'IN THIS EVENT?^')
      CALL DECIDE(IANSWR)
      IF( IANSWR .EQ. 2 ) ILSTOP = 0
C
      CALL SCANNR( INDEX,IRFLG )
C
 3003 IF( KNTR .EQ. 10 ) GO TO 3001
      GO TO 3002
C
C                            INCREMENT INDEX TO GO TO NEXT LEVEL
C
 3001 IF( INDEX .NE. 8 ) GO TO 4000
C
C                            SCRATCH EXISTING MUON RESULTS BEFORE L9
C
      CALL BMLT(2,'MUR1MUR2')
      CALL BDLM
 4000 INDEX = INDEX + 1
      RETURN
      END
