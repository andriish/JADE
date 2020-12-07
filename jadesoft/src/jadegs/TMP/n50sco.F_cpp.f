C   14/03/84 403202258  MEMBER NAME  N50SCO   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE N50SCO( IPLT, X1, Y1, Z1, X2, Y2, Z2, RR )
C-----------------------------------------------------------------------
C
C   AUTHOR:   C. BOWDERY  14/03/84 :  EXTRACT COORDINATES FROM N50S BANK
C
C
C        THIS ROUTINE EXTRACTS COORDINATES FROM THE IPJC SUB-BANK OF THE
C        N50S BANK WHICH USES CARTESIAN COORDINATES LIKE PATR
C
C        INPUT:  IPLT        POINTER TO HDATA WHERE TRACK STARTS
C        OUTPUT: X1,Y1,Z1    COORDINATES OF FIRST POINT OF TRACK (MM)
C                X2,Y2,Z2         "      "  LAST    "   "    "   (MM)
C                RR          RADIUS OF CURVATURE                 (MM)
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
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
      COMMON / CJTRIG / PI,TWOPI
C
      DIMENSION HRAD(2)
      EQUIVALENCE (IRAD,HRAD(1))
C
C------------------  C O D E  ------------------------------------------
C
      IFTR    = HDATA(IPLT)
      IF( IFTR .NE. 0 ) RETURN
C
C                            HALF WORDS 1,2,3 ARE X1,Y1,Z1
C                            1ST POINT ON TRACK, UNITS: 1/10  MM
C
      X1      = HDATA(IPLT + 1) * 0.1
      Y1      = HDATA(IPLT + 2) * 0.1
      Z1      = HDATA(IPLT + 3) * 0.1
C
C                            HALF WORDS 4,5,6 ARE X2,Y2,Z2
C                            LAST POINT ON TRACK, UNITS: 1/10  MM
C
      X2      = HDATA(IPLT + 4) * 0.1
      Y2      = HDATA(IPLT + 5) * 0.1
      Z2      = HDATA(IPLT + 6) * 0.1
C
C                            HALF WORDS 8,9   ARE R RADIUS OF TRACK
C                            (32 BIT WORD IN 2 PARTS, UNITS = 1/10 MM )
C
      HRAD(1) = HDATA(IPLT + 8)
      HRAD(2) = HDATA(IPLT + 9)
      RR      = FLOAT( IRAD )  * 0.1
C
      RETURN
      END
