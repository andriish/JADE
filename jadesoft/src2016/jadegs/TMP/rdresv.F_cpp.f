C   01/11/84 712141947  MEMBER NAME  RDRESV   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE RDRESV
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. HAGEMANN 07/12/82 :  SIMULATE SPACE RESOLUTION OF
C             R. RAMCKE               VERTEX CHAMBER
C
C        MOD  J. HAGEMANN 11/10/84 :  IMPROVED CODE
C        MOD  J. HAGEMANN 05/09/86 :  FOR FASTER SIMULATION OF GAUSSIAN
C                                     DISTRIBUTION, GENERAL UPDATE
C   LAST MOD  J. HAGEMANN 11/12/87 :  R-PHI RESOLUTION (2 GAUSS SCHEME)
C
C      SMEAR DRIFT TIMES AND Z-AMPLITUDES.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C----------START OF MACRO CMUBCS----------------------------------------
      COMMON /BCS/IDATA(1)
      DIMENSION HDATA(1),ADATA(1)
      EQUIVALENCE (HDATA(1),ADATA(1),IDATA(1))
C----------END OF MACRO CMUBCS------------------------------------------
C-----------------------------------------------------------------------
C                            MACRO CJVTXC     VERTEX CHAMBER
C-----------------------------------------------------------------------
C
      COMMON / CJVTXC / RVEC, ANG1, ANG2, DISTPW, FIRSTP, DISTW1,
     +                  ANGL, COSLOR, SINLOR,
     +                  ZRESV, ZMAXV, ZOFFV, ZNAMP, ZALV, TIMEV,
     +                  DRILOR(24), SNLORA(24), CSLORA(24),
     +                  DRVELO(24)
C
C--------------------------- END OF MACRO CJVTXC -----------------------
C
C
      COMMON / CBINV /  DTRSVV, ZRSVV, EFFVV(2), DOUBVV(3), IRNHVV,
     +                  SMPRMV(3)
C
C------------------  C O D E  ------------------------------------------
C
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
         IDT = HDATA(J + 3)
         DT  = FLOAT(IDT)*TIMEV
C
         CALL NVERT( 1.0, 0.0, G1R )
C
         IF( RN(DUMMY) .GT. SMPRMV(1) ) GO TO 10
            DTSMR = G1R*DTRSVV
            GO TO 20
   10    CONTINUE
            DTSMR = (G1R*SMPRMV(3) - SMPRMV(2))*DTRSVV
   20    CONTINUE
C
         DTREL = ABS(DT + DTSMR)/TIMEV
         HDATA(J + 3) = HFIX(AMIN1( DTREL, 32767. ))
C                          LEFT AND RIGHT AMPLITUDES
         IAL  = HDATA(J + 1)
         ZLA = FLOAT(IAL)
         CALL NVERT ( ZRESV, ZLA, ZSLA )
         HDATA(J + 1) = HFIX(ZNAMP*(1.0 + ZSLA/ZMAXV))
         HDATA(J + 2) = HFIX(ZNAMP*(1.0 - ZSLA/ZMAXV))
  100 CONTINUE
C
      RETURN
      END
