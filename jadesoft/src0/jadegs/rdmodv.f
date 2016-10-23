C   01/11/84 411011903  MEMBER NAME  RDMODV   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE RDMODV( IRNV, EPV, DOUBV )
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. HAGEMANN 27/06/83 :  SMEAR VERTEX CHAMBER DRIFT DATA
C             R. RAMCKE
C
C   LAST MOD  J. HAGEMANN 01/10/84 :  SOME VARIABLE NAMES CHANGED
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cmubcs.for"
C
      COMMON / CWORKV / HWORKV(2000)
C
      DIMENSION EPV(2), DOUBV(3)
C
C------------------  C O D E  ------------------------------------------
C
C                                      POINTER OF VTXC-BANK
      IPVTXC = IDATA( IBLN( 'VTXC' ) )
      IF( IPVTXC .EQ. 0 ) RETURN
C                                      HEADER LENGTHOF VTXC-BANK
      ILHDR = IDATA(IPVTXC+1)
C                                      NUMBER OF HITS
      NHITV = ( IDATA(IPVTXC) - ILHDR ) / 2
C
C                       SCALE DOWN NUMBER OF RANDOM HITS, IF TOO MANY
      IRANV = IRNV
      IF (NHITV .GT. 450) IRANV = 0
C
C                       GENERATE RANDOM HITS IN END SECTION OF HWORK
      CALL RDRNHV( IRANV, HWORKV(NHITV*4+1) )
C
C                       MERGE ALL HITS IN HWORK
      CALL RDMRGV( NHITV, HDATA(IPVTXC*2 + ILHDR*2 + 1), IRANV, HWORKV )
      NHNEWV = NHITV + IRANV
C
C                       INEFFICIENCIES
      CALL RDIEFV( EPV, NHNEWV, HWORKV, INEFLO )
      NHDV = NHNEWV - INEFLO
C
C                       DOUBLE PULSE RESOLUTION
      IDOULO = 0
      IF( NHDV .GT. 1 ) CALL RDDOUV( DOUBV, NHNEWV, HWORKV, IDOULO )
      NHDV = NHDV - IDOULO
C
C                       ADJUST LENGTH OF BANK
      IF( NHDV .LE. 0 ) GO TO 1000
      LDV = ( NHDV - NHITV ) * 2
      IF( LDV .NE. 0 ) CALL  BCHM( IPVTXC, LDV, IER )
      CALL RDPOIV( HDATA(IPVTXC*2 + ILHDR*2 + 1), NHNEWV, HWORKV )
      RETURN
C
C
 1000 CALL BDLS( 'VTXC', IDATA(IPVTXC - 2) )
      RETURN
      END
