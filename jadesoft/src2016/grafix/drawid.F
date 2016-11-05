C   01/11/84 807241257  MEMBER NAME  DRAWID   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE DRAWID( IVIEW )
C-----------------------------------------------------------------------
C
C   AUTHOR:    C. BOWDERY    26/04/84 :  DRAW INNER DETECTOR
C
C       MOD:   J. HAGEMANN   09/10/84 :  HATCH TANK WALL ON SCREEN
C  LAST MOD:   J. HAGEMANN   11/04/88 :  BIG DOTS FOR SIGNAL WIRES
C                                        WHEN REQUESTED
C
C     DRAW THE INNER DETECTOR FOR THE VIEW IVIEW.
C       IVIEW = 1   : R PHI VIEW
C       IVIEW = 2   : RZ VIEWS
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL  DSPDTM, FLVCDO
C
      COMMON / CJTRIG / PI,TWOPI
      COMMON / CJCELL / NCELL(3),NWIRES(3)
      COMMON / CGRAP2 / BCMD,DSPDTM(30),ISTVW,JTVW
      COMMON / CGVCDO / FLVCDO(20)
C
#include "cjdrch.for"
#include "cgeo1.for"
#include "cgeo2.for"
#include "cgraph.for"
#include "cgrscl.for"
C
C------------------  C O D E  ------------------------------------------
C
C                            CDTL 4   DRAW JET CHAMBER WALLS
C                            CDTL 5   DRAW JET CHAMBER WIRES
C                            CDTL 6   DRAW CROSSES FOR JET CHAMBER HITS
C
      GO TO ( 10 , 20 ) , IVIEW
C
C                            R PHI VIEW
C
  10  IF( ( .NOT. DSPDTL(4) )  .AND.  ( .NOT. DSPDTL(5) ) ) GO TO 9
C
      RDT = 0.5/ABS(XMAX-XMIN)*ABS(XMAXST(20)-XMINST(20))
      NRING = 3
      IF( LASTVW .EQ. 20 ) NRING = 1
      DO  2  J = 1,NRING
        RADD   = RINCR(J)
        RR1    = FSENSW(J)
        NN     = NCELL(J)
        NWE    = NWIRES(J)
        RR2    = RR1 + RADD*(NWE - 1)
        DELPHI = TWOPI/NN
C
        DO  4  K = 1,NN
          DO  6  L = 4,5
            IF( .NOT. DSPDTL(L) ) GO TO 6
            RR11 = RR1
            IF( L .EQ. 4 ) RR11 = RR11 - 15.
            RR22 = RR2
            IF( L .EQ. 4 ) RR22 = RR22 + 15.
C
            PHI  = DELPHI * (K - 1) + 0.5 * DELPHI * (L - 4)
            CSN  = COS(PHI)
            SSN  = SIN(PHI)
            XL1  = RR11 * CSN
            YL1  = RR11 * SSN
            IF( L .EQ. 5 ) GO TO 7
C
            CALL DRAMOV(XL1,YL1,RR22*CSN,RR22*SSN,0)
            GO TO 6
    7       CONTINUE
            STEPX = RADD * CSN
            STEPY = RADD * SSN
            DO  8  M = 1,NWE
              CALL POINTA(XL1,YL1)
              IF( LASTVW .NE. 20 .OR. .NOT. FLVCDO(19) )  GO TO 1008
                 CALL PLYGON( 11, RDT, XL1, YL1, 0 )
1008          XL1 = XL1 + STEPX
              YL1 = YL1 + STEPY
   8        CONTINUE
   6      CONTINUE
   4    CONTINUE
   2  CONTINUE
C
   9  DDD   = 10.24 * 25.4 / ( XMAX - XMIN )
      NN = 525.0 * DDD
      IF( NN .LT. 40 ) NN = 40
      CALL PLYGON(NN,RITNK,0.,0.,0)
      CALL PLYGON(NN,RITNK+DRITNK,0.,0.,0)
      IF( DSPDTM(15) )
     +              CALL DRHATC(IVIEW, 200, RITNK, RITNK+DRITNK, 0., 0.)
C
      NN = 750.0 * DDD
      IF( NN .LT. 40 ) NN = 40
      CALL PLYGON(NN,ROTNK,0.,0.,0)
      CALL PLYGON(NN,ROTNK+DROTNK,0.,0.,0)
      IF( DSPDTM(15) )
     +              CALL DRHATC(IVIEW, 500, ROTNK, ROTNK+DROTNK, 0., 0.)
C
      RETURN
C
C                            Z VIEWS
C
C                            FIRST DRAW BOX
C
  20  CALL DRAMOV(ZTKM, RITNK,ZTKM, ROTNK,0)
      CALL DRAWA( ZTKP, ROTNK)
      CALL DRAWA( ZTKP, RITNK)
      CALL DRAWA( ZTKM, RITNK)
      CALL DRAMOV(ZTKM,-RITNK,ZTKM,-ROTNK,0)
      CALL DRAWA( ZTKP,-ROTNK)
      CALL DRAWA( ZTKP,-RITNK)
      CALL DRAWA( ZTKM,-RITNK)
      X1 = -ZMAX + ZOFFS
      X2 =  ZMAX + ZOFFS
C
C                            DRAW WIRES OF INNER DETECTOR
C
      IF( .NOT. DSPDTL(5) ) RETURN
C
      DO  21  J = 1,3
        RADD = RINCR(J)
        R    = FSENSW(J)
        NWE  = NWIRES(J) - 1
        DO  22  K = 1,2
          R = R + (K-1)*NWE*RADD
          CALL DRAMOV(X1, R,X2, R,0)
          CALL DRAMOV(X1,-R,X2,-R,0)
  22    CONTINUE
  21  CONTINUE
C
      RETURN
      END
