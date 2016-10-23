C   11/10/83            MEMBER NAME  USERSP   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE USER(INDEX)
C-----------------------------------------------------------------------
C      *** SPECIAL VERSION ***
C
C  BASED ON THE STANDARD PROGRAM 'USER'
C  MODIFIED FROM A VERSION BY HARRISON PROSPER, MANCHESTER/JADE
C
C  THIS SUBROUTINE IS CALLED BY THE JADE SUPERVISOR WITH 'INDEX'
C  SET TO INDICATE THE LEVEL OF ANALYSIS REACHED.
C
C  INPUT INDEX=0   INITIAL CALL, BEFORE FIRST EVENT READ.
C              1   CALLED AT THE BEGINNING OF EACH NEW RUN.
C              2   CALLED IMMEDIATELY AFTER EVENT IS READ INTO CDATA.
C              3   LEAD GLASS ENERGIES HAVE BEEN COMPUTED.
C              4   FAST Z VERTEX RECONSTRUCTION HAS BEEN DONE.
C              5   INNER DETECTOR PATTERN RECOGNITION HAS BEEN RUN.
C              6   ENERGIES CLUSTERS IN THE LEAD GLASS HAVE BEEN FOUND.
C              7   TRACKS AND CLUSTERS HAVE BEEN ASSOCIATED.
C              8   UNUSED
C              9   MUON ANALYSIS HAS BEEN DONE (PHILOSPHY 2)
C             10   UNUSED
C            100   JUST BEFORE END OF JOB
C
C  ON RETURN, IF INDEX =  1 : THE SUPERVISOR WILL DROP CURRENT EVENT
C                             AND READ THE NEXT ONE.
C                        11 : EVENT WILL BE WRITTEN OUT AND NEW EVENT
C                             WILL BE READ
C                        12 : JOB WILL BE TERMINATED ON RETURN
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL DSPDTL,SSTPS,PSTPS,FREEZE
C
C           COMMON DECLARATIONS
C
#include "cgraph.for"
#include "cadmin.for"
#include "cdata.for"
      COMMON /CTPCNS/ BENGEV,BKGAUS
      COMMON /CTLIM / ISECLF
      COMMON /CBSTR / MODE
      COMMON /CGCALB/ NBADCR,NAVALL
      COMMON /CWORK / IDUMMY
C
      DATA  ICOUNT / 0 /
C
C=======================================================================
C             START OF CODE SECTION
C=======================================================================
C             CHECK WHETHER CALL AT END OF JOB
      IF(INDEX.EQ.100) GOTO 1100
C
      GO TO (1001,1002,1003,1004,1005,1006,1007,1008,1009,1010),INDEX
C
C=======================================================================
C             INDEX = 0  INITIALIZATION.
C=======================================================================
      INDEX=INDEX+1
      RETURN
C=======================================================================
C             INDEX = 1    CALLED AT THE BEGINNING OF EACH NEW RUN.
C=======================================================================
C             FILL COMMON CTPCNS WITH VALUES OF ENERGY & FIELD
 1001 IHEAD  = IDATA( IBLN( 'HEAD' ) )
      BENGEV = HDATA(2*IHEAD+29)/1000.
      BKGAUS = HDATA(2*IHEAD+30)/1000.
C     WRITE(6,37)
C37   FORMAT('USERJ CALLED AT INDEX=1')
      INDEX=INDEX+1
      RETURN
C=======================================================================
C             INDEX = 2    CALLED IMMEDIATELY AFTER EVENT IS READ IN
C=======================================================================
 1002 INDEX=INDEX+1
      ICOUNT = ICOUNT + 1
      CALL PRTPEV(12)
      CALL PRTPEV(13)
      CALL PRTPEV(14)
      CALL PRTPEV(15)
      CALL PRTPEV(16)
C
C                            PRINTS ON UNITS 12,13,14,15,16
C
      CALL PRTPTR(0,12,13,14,15,16)
C
      CALL PRTPVX(0,12)
      CALL PRTPVX(0,13)
      CALL PRTPVX(0,14)
      CALL PRTPVX(0,15)
      CALL PRTPVX(0,16)
      INDEX = 1
      IF( ICOUNT .GT. 5 ) INDEX = 12
      RETURN
C=======================================================================
C             INDEX = 3    LEAD GLASS ENERGIES HAVE BEEN COMPUTED.
C=======================================================================
 1003 INDEX=INDEX+1
      RETURN
C=======================================================================
C             INDEX = 4    FAST Z VERTEX RECONSTRUCTION HAS BEEN DONE.
C=======================================================================
 1004 INDEX=INDEX+1
      RETURN
C=======================================================================
C             INDEX = 5    JET CHAMBER PATTERN RECOGNITION HAS BEEN RUN.
C=======================================================================
 1005 INDEX=INDEX+1
      RETURN
C=======================================================================
C             INDEX = 6    LEAD GLASS ENERGY CLUSTERS HAVE BEEN FOUND.
C=======================================================================
 1006 CONTINUE
      INDEX=INDEX+1
      RETURN
C=======================================================================
C             INDEX = 7    TRACKS AND CLUSTERS HAVE BEEN ASSOCIATED.
C=======================================================================
 1007 INDEX=INDEX+1
      RETURN
C=======================================================================
C             INDEX = 8    NO FURTHER ANALYSIS DONE SINCE LEVEL 7
C=======================================================================
 1008 INDEX=INDEX+1
      RETURN
C=======================================================================
C             INDEX = 9    MUON ANALYSIS HAS BEEN DONE.
C=======================================================================
 1009 INDEX=INDEX+1
      RETURN
C=======================================================================
C             INDEX = 10   UNUSED AT THE MOMENT
C=======================================================================
 1010 INDEX=INDEX+1
      RETURN
C=======================================================================
C             INDEX = 100  END OF JOB: FINAL CALCULATIONS + PRINTOUT
C=======================================================================
 1100 CALL BSTA
      RETURN
C=======================================================================
      END
