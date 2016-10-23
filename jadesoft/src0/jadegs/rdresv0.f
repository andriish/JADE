C   01/11/84            MEMBER NAME  RDRESV0  (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE RDRESV( DTRSVV )
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. HAGEMANN 07/12/82 :  SIMULATE SPACE RESOLUTION OF
C             R. RAMCKE               VERTEX CHAMBER
C
C   LAST MOD  J. HAGEMANN 11/10/84 :  IMPROVED CODE
C
C      SMEAR DRIFT TIMES AND Z-AMPLITUDES.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cmubcs.for"
C
#include "cjvtxc.for"
C
      DATA PI / 3.14159 /
C
C------------------  C O D E  ------------------------------------------
C
C     STATEMENT FUNCTION TO COMPUTE GAUSSIAN DISTRIBUTION
      G1(Z1,SQLOG)=SIN(PI*2.*Z1)*SQLOG
C
C
      IPVTXC = IDATA( IBLN('VTXC') )
      IF( IPVTXC .EQ. 0 ) RETURN
C
C                          FIRST AND LAST ADD. OF WIRE
      ILHD = IDATA(IPVTXC+1)
      ILV  = IPVTXC*2 + ILHD*2 + 1
      IHV  = ILV + (IDATA(IPVTXC) - ILHD)*2 - 4
C
C                          LOOP OVER ALL WIRES AND SET RESOLUTIONS
C
      DO  100  J = ILV, IHV, 4
C                          DRIFT TIME
         IDT   = HDATA(J + 3)
         DT    = FLOAT(IDT)*TIMEV
         Z1    = RN(DUM)
         SQLOG = SQRT(-2.*ALOG(RN(DUM)))
         DTREL = ABS(DT + G1(Z1,SQLOG)*DTRSVV)/TIMEV
         HDATA(J + 3) = HFIX(AMIN1( DTREL, 32767. ))
C                          LEFT AND RIGHT AMPLITUDES
         IAL = HDATA(J + 1)
         ZLA = FLOAT(IAL)
         CALL NVERT ( ZRESV, ZLA, ZSLA )
         HDATA(J + 1) = HFIX(ZNAMP*(1 - ZSLA/ZMAXV))
         HDATA(J + 2) = HFIX(ZNAMP*(1 + ZSLA/ZMAXV))
  100 CONTINUE
C
      RETURN
      END
