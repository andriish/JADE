      SUBROUTINE VTXEER(IT1,IT2,IPOINT,ITEST) 
C*860529*KLEINWORT***************************************************
C*  COPIED FROM F22KLE.VERTEX.S(VTXEER)   18.9.86
C*                                                                  *
C*   PRINT ERROR-TEXT FOR   V T X E E                               *
C*                                                                  *
C********************************************************************
C
      IMPLICIT INTEGER*2(H)
C PMF 03.11.98 
      LOGICAL TBIT
C
C----------------------------------------------------------------------
C             MACRO CDATA .... BOS COMMON.
C
C             THIS MACRO ONLY DEFINES THE IDATA/HDATA/ADATA NAMES.
C             THE ACTUAL SIZE OF /BCS/ IS FIXED ON MACRO CBCSMX
C             OR BY OTHER MEANS. A DEFAULT SIZE OF 40000 IS GIVEN HERE.
C
C----------------------------------------------------------------------
C
      COMMON /BCS/ IDATA(40000)
      DIMENSION HDATA(80000),ADATA(40000),IPNT(50)
      EQUIVALENCE (HDATA(1),IDATA(1),ADATA(1)),(IPNT(1),IDATA(55))
      EQUIVALENCE (NWORD,IPNT(50))
C
C------------------------ END OF MACRO CDATA --------------------------
C
      REAL*8 DTEXT(14)
C
      DATA DTEXT / 'PRETEST ', 'TANKWALL', 'BEAMPIPE', 'PAR.TEST',

     * 'CHAMBER ', 'CHARGE  ', 'D(PHI)  ', 'D(THETA)', 'D(XY)   ',
     * 'D(Z)    ', 'INTRSECT', 'EXTRAP.S', 'INTERP.S', 'RADIUS  ' /
C
      IF (ITEST.EQ.0) GOTO 600
C
      IF ((IPOINT.LT.1).OR.(IPOINT.GT.5)) GOTO 500
      IF ((ITEST.LT.0).OR.(ITEST.GT.9)) GOTO 500
C
      WRITE(6,9000) DTEXT(IPOINT),DTEXT(ITEST+5),IT1,IT2
 9000 FORMAT(' VTXEE FAILED IN ',A8,' DUE TO INVALID ',A8,
     *' WITH TRACKS :',2I3)
C
      IH = IDATA(IBLN('HEAD'))
      IF (IH.LE.0) GOTO 600
C
      NRUN  = HDATA( 2*IH+ 10 )
      NREC  = HDATA( 2*IH+ 11 )
      WRITE(6,9010) NRUN,NREC
 9010 FORMAT(' RUN,REC : ',2I6)
C
      GOTO 600
C
  500 CONTINUE
      WRITE(6,9020) IPOINT,ITEST
 9020 FORMAT(' ??? STRANGE ENTRY TO VTXEER : ',2I5)
C
  600 CONTINUE
      RETURN
      END
