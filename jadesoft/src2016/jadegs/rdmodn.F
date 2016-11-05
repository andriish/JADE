C   19/02/84 402192318  MEMBER NAME  RDMODN   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE RDMODN( IRN, EPS, IDOUBL )
C-----------------------------------------------------------------------
C
C   AUTHOR    E. ELSEN    26/04/79 :  SMEAR DRIFT DATA
C
C        MOD  E. ELSEN    29/05/79 :
C        MOD  C. BOWDERY  30/08/83 :  POINTERS FROM OLD TO NEW HITS
C   LAST MOD  C. BOWDERY   6/10/83 :  REMOVE CREATION OF 'JETC'/9 BANK
C   LAST MOD  J. OLSSON   18/01/84 :  CORRECT LOGICAL ERROR IN LOOP 2
C
C                  MODIFY  DRIFT  DATA  TO  ACCOUNT  FOR  RANDOM   HITS,
C                  INEFFICIENCIES AND DOUBLE PULSE RESOLUTION.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cmubcs.for"
C
      COMMON / CWORK  / HWORK(20000)
      COMMON / CWORK1 / JHITS, H4VJH(4000), H4VMRG(5000)
C
      DIMENSION IRN(3), IRAN(3), IDOUBL(3), EPS(3)
C
C
C------------------  C O D E  ------------------------------------------
C
      IPJ = IDATA( IBLN( 'JETC' ) )
      IPH = IDATA( IBLN( 'HITL' ) )
C
C                       IF NO 'JETC' BANK THEN RETURN.
C
      IF( IPJ .EQ. 0 ) RETURN
C                                     CHECK MAXIMUM LENGTH
           LENGTH = HDATA(IPJ*2)*2
           IF(LENGTH.LT.20000) GO TO 4
      WRITE(6,1001) LENGTH
1001  FORMAT(' TOO MANY HITS, LENGTH = ',I10,'  RDMODN PROGRAMMED STOP')
      STOP
C
 4    NHITS = ( HDATA(IPJ*2+99) - 1 ) / 4
C
      DO  1  I = 1,3
        IRAN(I) = IRN(I)
    1 CONTINUE
C
      SCL = -1.
    2 IRNTOT = IRAN(1) + IRAN(2) + IRAN(3)
C
C        SCALE DOWN NUMBER OF RANDOM HITS, IF TOO MANY. MAX IS 800.
C
      IF( IRNTOT .LT. 800 ) GO TO 3
           SC =  800./ FLOAT(IRNTOT)
           GO TO 5
    3 IF( LENGTH + IRNTOT*4 .LT.20000 ) GO TO 6
      SC = (20000. - FLOAT(LENGTH) ) / FLOAT(IRNTOT*4)
    5 DO 7 I=1,3
    7 IRAN(I) = IFIX(FLOAT(IRAN(I)) * SC)
C
      IF(SCL.EQ.SC) GO TO 6
      SCL = SC
      GO TO 2
C
C                       GENERATE RANDOM HITS IN END SECTION OF HWORK
  6   CONTINUE
      write(*,*)' RDM 1', 'NHITS= ', NHITS
C
      CALL RDRDMH( IRAN, HWORK(NHITS*4+1) )
C
C                       MERGE ALL HITS IN HWORK
C                       'JETC' HAS  100 HEADER WORDS.
C
      CALL RDMERG( NHITS, HDATA(IPJ*2+101), IRNTOT, HWORK )
      NHNEW = NHITS + IRNTOT
C
C                       INEFFICIENCIES
C
      CALL RDINEF( EPS, NHNEW, HWORK, INEFLO )
      NHD = NHNEW - INEFLO
      write(*,*)' RDM 2 ',NHNEW, NHD
C
C                       DOUBLE PULSE RESOLUTION
C
      IDOULO = 0
      IF( NHD .GT. 1 ) CALL RDDOUB( IDOUBL, NHNEW, HWORK, IDOULO )
      NHD = NHD - IDOULO
C
CCC  DEAD CELLS
      IDEAD = 0
      IF( NHD .GT. 0 ) CALL RDDDCL( NHNEW, HWORK, IDEAD )
      NHD = NHD - IDEAD
      write(*,*)' RDM 3'
C
C                  ADJUST LENGTH OF BANK, CONSTRUCT  CELL  POINTERS  AND
C                  ERASE ZERO WIRE NUMBERS
C
      IF( NHD .LE. 0 ) GO TO 1000
      LD = ( NHD - NHITS ) * 2
      write(*,*)' RDM 3a  ',IPJ, LD, IER 
      IF( LD .NE. 0 ) CALL  BCHM( IPJ, LD, IER )
      CALL RDPOIN( HDATA(IPJ*2+3), NHNEW, HWORK )
C
C                  IF THERE WAS A 'HITL' BANK, CREATE A 'HTSL' BANK
C                  TO STORE THE ASSOCIATION BETWEEN THE ORIGINAL HITS
C                  AND THE SMEARED ONES.
C
      IF(IPH .LE. 0 ) RETURN
      LENGTH = ( JHITS + 2 ) / 2
      CALL BCRE( IND, 'HTSL', 8, LENGTH, *1500, IER )
      CALL BSAW( 1, 'HTSL' )
      HDATA(IND*2 + 1) = JHITS
      write(*,*)' RDM 88'
      write(*,*)4*IND + 2,  JHITS * 2 
      CALL MVCL( HDATA, 4*IND + 2, H4VJH, 0, JHITS * 2 )
      write(*,*)' RDM 89'
      RETURN
C
C                  DELETE BANK AND HEAD POINTER BECAUSE ALL POINTS  WERE
C                  ERASED
C
      write(*,*)' RDM 99'
 1000 CALL BDLS( 'JETC', IDATA(IPJ-2) )
      CALL BDLS( 'PATR', 12 )
      RETURN
C
 1500 WRITE(6,1501) IER
 1501 FORMAT(' BCRE ENDED WITH ERROR ',I3,' IN RDMODN. ')
      RETURN
      END
