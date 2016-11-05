C   01/11/84 807241258  MEMBER NAME  DRAWVC   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE DRAWVC( IVIEW , JNDEX )
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. HAGEMANN   26/04/84 :  DRAW VERTEX CHAMBER
C
C      MOD:   J. HAGEMANN   19/10/84 :  DRAW CELL NUMBERS  (JNDEX=16)
C      MOD:   J. HAGEMANN   22/05/85 :  SKIP CELL NUMBERS IN A
C                                       DEMAGNIFICATION OF VC-VIEW
C      MOD:   J. HAGEMANN   14/02/86 :  DRAW CHAMBER IN TRUE POSITION
C                                       RELATIVE TO THE ORIGIN
C LAST MOD:   J. HAGEMANN   24/04/86 :  DRAW POTENTIAL WIRE PLANE
C                                       BETWEEN BAD CELLS FOR REAL DATA
C
C     DRAW THE VERTEX CHAMBER FOR THE VIEW IVIEW.
C       IVIEW = 1   : R PHI VIEW
C       IVIEW = 2   : RZ VIEWS
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL FL18, FL22, FL24
      LOGICAL FLVCDO, DSPDTM
C
#include "cgeov.for"
#include "cjvcel.for"
#include "cjvtxc.for"
#include "cgraph.for"
#include "cgrscl.for"
#include "mvccal.for"
C
      COMMON / CHEADR / HEAD(108)
      COMMON / CPROJ  / XMINR,XMAXR,YMINR,YMAXR,IPRJC,FL18,FL22,FL24
      COMMON / CGRAP2 / BCMD,DSPDTM(30),ISTVW,JTVW
      COMMON / CGVCDO / FLVCDO(20)
      COMMON / CJTRIG / PI,TWOPI
C
      DIMENSION HTX(24)
C
      DATA HTX / 2H 1, 2H 2, 2H 3, 2H 4, 2H 5, 2H 6, 2H 7, 2H 8, 2H 9,
     +           2H10, 2H11, 2H12, 2H13, 2H14, 2H15, 2H16, 2H17, 2H18,
     +           2H19, 2H20, 2H21, 2H22, 2H23, 2H24 /
C
C------------------  C O D E  ------------------------------------------
C
C                            CDTL 4   DRAW VERTEX CHAMBER FIELD WIRES
C                            CDTL 5   DRAW VERTEX CHAMBER SENSE WIRES
C                            CDTL 6   DRAW CROSSES FOR HITS
C
      GO TO ( 100, 2000 ) , IVIEW
C
C                            R PHI VIEW
C
  100 IF( (.NOT. DSPDTL(4))  .AND.  (.NOT. DSPDTL(5)) ) GO TO 1800
C
C                            LOAD CONSTANTS FOR RELATIVE POSITION
      XDEV = 0.0
      YDEV = 0.0
      ADEV = 0.0
      IF( HEAD(18) .LE. 100 ) GO TO 200
         XDEV = VDX
         YDEV = VDY
         ADEV = VROT
C
C                            DRAW CELL NUMBERS ON SCREEN
C                            UNLESS DEMAGNIFIED VIEW
C
  200 IF( JNDEX .NE. 16 .OR. ( FL22 .AND.
     &     ABS(XMAX-XMIN) .GT. ABS(XMAXST(20) - XMINST(20))) .OR.
     &     DSPDTM(16) ) GO TO 600
         CALL CHRSIZ( 4 )
         XSCAL = (XMAX - XMIN)/690.
         IF( FL22 ) XSCAL = (XMAXR - XMINR)/690.
C
         DO 500 I = 1, 24
            PHI = ANG1 + ANG2*FLOAT(I-1) + ADEV
            XN  = (RPIPV - 10.*XSCAL)*COS(PHI) + 4.0*XSCAL + XDEV
            YN  = (RPIPV - 10.*XSCAL)*SIN(PHI) - 3.0*XSCAL + YDEV
C
            IF( .NOT. FL22 ) GO TO 400
C
            IF( -XN .LT. XMINR .OR. -XN .GT. XMAXR .OR.
     *           YN .LT. YMINR .OR.  YN .GT. YMAXR ) GO TO 500
C
  400       CALL MOVEA( -XN, YN )
            CALL EOUTST( 2, HTX(I) )
  500    CONTINUE
C
  600 DDD  = 10.24 * 25.4 / ( XMAX - XMIN )
      RDT = 0.5/ABS(XMAX-XMIN)*ABS(XMAXST(20)-XMINST(20))
      NCOL = IFIX(10.0 * DDD)
      IF( NCOL .LT. 10 ) NCOL = 10
      RR11V = DISTW1
      RR22V = DISTW1 + DISTPW*(MWIRE-1)
      DO  1700  KV = 1, MCELL
        DO  1600  L = 4, 5
          IF (.NOT. DSPDTL(L)) GO TO 1600
C
             PHIV = ANG2*(KV-1) + ANG1*(L*2 - 9) + ADEV
             CSNV = COS(PHIV)
             SSNV = SIN(PHIV)
             XL1V = RR11V*CSNV + XDEV
             YL1V = RR11V*SSNV + YDEV
             IF (L .EQ. 5) GO TO 1100
               STEPXV = DISTPW * CSNV *.5
               STEPYV = DISTPW * SSNV *.5
C
C                            DRAW POTENTIAL WIRE PLANE BETWEEN CELLS
C
               DO 1000 IJ = 1, 13
                  XP1V   = XL1V + STEPXV*FLOAT(IJ-1) - 1.0*CSNV
                  YP1V   = YL1V + STEPYV*FLOAT(IJ-1) - 1.0*SSNV
                  XP2V   = XL1V + STEPXV*FLOAT(IJ-1) + 1.0*CSNV
                  YP2V   = YL1V + STEPYV*FLOAT(IJ-1) + 1.0*SSNV
                  CALL DRAMOV( -XP1V, YP1V, -XP2V, YP2V, 1 )
 1000          CONTINUE
               GO TO 1600
C
 1100        STEPXV = DISTPW * CSNV
             STEPYV = DISTPW * SSNV
C
C                            DRAW SIGNAL WIRES
C
             DO  1500  MV = 1, MWIRE
               IF(MV .NE. 1) GO TO 1300
               IF(KV .NE. 1) GO TO 1200
C
C                            DRAW COOLING PIPE IN CELL 1
C
                 XCOL = 96.5*COS(ANG1+ADEV) + XDEV
                 YCOL = 96.5*SIN(ANG1+ADEV) + YDEV
                 CALL PLYGON( NCOL, 3.0, -XCOL, YCOL, 0 )
                 CALL PLYGON( NCOL, 2.5, -XCOL, YCOL, 0 )
                 GO TO 1400
 1200          IF(KV .NE. 13) GO TO 1300
C
C                            DRAW COOLING PIPE IN CELL 13
C
                 XCOL = -96.5*COS(ANG1+ADEV) + XDEV
                 YCOL = -96.5*SIN(ANG1+ADEV) + YDEV
                 CALL PLYGON( NCOL, 3.0, -XCOL, YCOL, 0 )
                 CALL PLYGON( NCOL, 2.5, -XCOL, YCOL, 0 )
                 GO TO 1400
C
 1300          CALL POINTA( -XL1V, YL1V )
               IF( JNDEX .NE. 16 .OR. .NOT. FLVCDO(19) )  GO TO 1400
                  CALL PLYGON( 11, RDT, -XL1V, YL1V, 0 )
 1400          XL1V = XL1V + STEPXV
               YL1V = YL1V + STEPYV
 1500        CONTINUE
 1600     CONTINUE
 1700 CONTINUE
C
C                            MARK BAD CELL BORDER
      IF( (JNDEX .NE. 16) .OR. (.NOT. DSPDTL(4)) .OR.
     &    (HEAD(18) .LE. 100) ) GO TO 1800
         CS1213 = COS(13.0*ANG2 - ANG1 + ADEV)
         SN1213 = SIN(13.0*ANG2 - ANG1 + ADEV)
         XF1213 = (RR11V-1.0)*CS1213 + XDEV
         YF1213 = (RR11V-1.0)*SN1213 + YDEV
         XL1213 = (RR22V+1.0)*CS1213 + XDEV
         YL1213 = (RR22V+1.0)*SN1213 + YDEV
         CALL DRAMOV( -XF1213, YF1213, -XL1213, YL1213, 1 )
 1800 NN = IFIX(500.*DDD)
      IF( NN .LT. 40 )    NN = 40
      CALL PLYGON( NN, RVXC, -XDEV, YDEV, 0 )
      CALL PLYGON( NN, RVXC+DRVXC, -XDEV, YDEV, 0 )
      RETURN
C
C                            Z VIEWS
C
 2000 CALL DRAMOV( ZVXCM, RPIPV+DRPIPV, ZVXCM, RVXC, 0 )
      CALL DRAWA( ZVXCP, RVXC )
      CALL DRAWA( ZVXCP, RPIPV+DRPIPV )
      CALL DRAWA( ZVXCM, RPIPV+DRPIPV )
C
      CALL DRAMOV( ZVXCM, -RPIPV-DRPIPV, ZVXCM, -RVXC, 0 )
      CALL DRAWA( ZVXCP, -RVXC )
      CALL DRAWA( ZVXCP, -RPIPV-DRPIPV )
      CALL DRAWA( ZVXCM, -RPIPV-DRPIPV )
      X1V = ZVXCM
      X2V = ZVXCP
C
C                           DRAW WIRES OF VERTEX CHAMBER
C
      IF( .NOT. DSPDTL(5) ) RETURN
      DO  2020 KV = 1,2
        RV = DISTW1 + (KV - 1) * (MWIRE - 1) * DISTPW
        CALL DRAMOV( X1V,  RV, X2V,  RV, 1 )
        CALL DRAMOV( X1V, -RV, X2V, -RV, 1 )
 2020 CONTINUE
      RETURN
C
      END
