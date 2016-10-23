C   10/10/84 807241316  MEMBER NAME  DISPVC   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE DISPVC(INDEX,MODVW)
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. HAGEMANN   28/10/85 :  DISPLAY VERTEX CHAMBER HITS
C                                        IN R-FI VIEWS
C
C         MOD: J. HAGEMANN  13/03/86 : EXTENDED COMMON CVX AND SWITCHING
C                                      OF VTXC BANKS POSSIBLE
C         MOD: J. HAGEMANN  09/04/86 : FOR PRE-FILTER BANK VPAT 19/20
C         MOD: J. HAGEMANN  26/05/86 : SUPPRESS PRINTOUT FOR DATA
C                                      WITHOUT VTXC-BANK
C         MOD: J. HAGEMANN  14/11/86 : PRINT ONCE CALIBRATION CONSTANTS
C                                      FOR REAL DATA
C         MOD: J. HAGEMANN  16/12/86 : STRUCTURE CHANGE FOR Z-COORDINATE
C                                      DISPLAY
C    LAST MOD: J. HAGEMANN  26/01/88 : GENERAL UPDATE
C
C       JNDEX: VIEW INDEX
C       MODVW: 1= R-FI-VIEWS  , 2= Z-VIEWS
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL FLVCDO
      LOGICAL FL18,FL22,FL24
      LOGICAL LVTXC,LNHARD,NEWDET
      LOGICAL LZDSP,LCOMFT
C
#include "cgraph.for"
#include "cadmin.for"
#include "cgrscl.for"
#include "cdata.for"
#include "mvccal.for"
#include "mvtrce.for"
C
      COMMON / CWORK1 / R,FI,R1,FI1,X1,Y1,R2,FI2,X2,Y2,ZET,X3,Y3,X4,Y4,
     +                  KZAMP,IMW(132)
      COMMON / CPROJ  / XMINR,XMAXR,YMINR,YMAXR,IPRJC,FL18,FL22,FL24
C
      COMMON / CGVCDO / FLVCDO(20)
      COMMON / CVX    / NNPATR,NDUMM,NNJETC,NNVTXC
      COMMON / CVCEX  / LVTXC
      COMMON / CHEADR / HEAD(108)
C
      DIMENSION HVTHT(500), IVPAT(2,500), HTUSED(500), IVPRE(2,500)
C
C
C-----------------  C O D E  -------------------------------------------
C
      JNDEX  = INDEX
C------------------------    TEMPORARY
      CSLOR =  0.961248
      SNLOR = -0.275684
C------------------------    TEMPORARY
      IF( INDEX .GT. 7 ) JNDEX = INDEX - 4
C
      IF( NNVTXC .LT. 0 .OR. NNVTXC .GT. 99 ) GO TO 50
         CALL CLOC( IPVTXC, 'VTXC', NNVTXC )
         NBANK = NNVTXC
         IF( IPVTXC .GT. 0 ) GO TO 80
            LNHARD = (HEAD(15) .GE. 5  .AND.  HEAD(16) .EQ. 1984)
     &                                .OR.   HEAD(16) .GE. 1985
            IPBPCH = IDATA(IBLN('BPCH'))
            NEWDET = LVTXC .OR. (IEVTP.EQ.0 .AND. LNHARD)
            IF( NEWDET .AND. IPBPCH .LE. 0 ) WRITE(6,40) NNVTXC
   40       FORMAT(' VTXC BANK NR',I3,' (GIVEN BY COMMAND VTXC) NOT EXIS
     &TING')
   50 NBANK = 10
      CALL CLOC( IPVTXC, 'VTXC', NBANK )
      IF( IPVTXC .LE. 0 )  GO TO 2000
   80    CONTINUE
         IPVTX2 = IPVTXC*2
         NNVTXC = NBANK
         NHDLV = IDATA(IPVTXC+1)
         NWOV  = IDATA(IPVTXC) - NHDLV
         IF( NWOV .LE. 0 ) GO TO 2000
C
         DO 100 I = 1, 500
            HVTHT(I) = 0
  100    CONTINUE
C
         IF( MODVW .NE. 1 ) GO TO 800
C
         CALL CLOC( IPVTHT, 'VTHT', NBANK )
         IF( IPVTHT .LE. 0 ) GO TO 800
C
  200       ILVTHT = IDATA(IPVTHT)*2
            IPVTH2 = IPVTHT*2
            ILHDVT = HDATA(IPVTH2+1)
            ILTLVT = HDATA(IPVTH2+4)
            ISMTRA = HDATA(IPVTH2+5)
            ISMTR1 = HDATA(IPVTH2+6)
C
            DO 700 I = 1, ISMTRA
               IPAC = IPVTH2 + ILHDVT + (I-1)*ILTLVT + 1
               IF( IPAC .LE. IPVTH2+ILVTHT ) GO TO 300
      CALL TRMOUT(80,'DISPVC : VTHT-BANK --> NUMBER OF TRACKS IN HEADER
     &IS WRONG !^')
               GO TO 800
  300          NHTS = HDATA(IPAC)
C
               IPFL = 2
               IF( I .GT. ISMTR1 ) IPFL = 0
               IF( NHTS .LT. 0 ) IPFL = -10
               NHTS = IABS(NHTS)
C
               DO 600 J = 1, NHTS
                  IPHT = HDATA(IPAC+J)
                  IF( IPFL .EQ. -10 ) GO TO 500
                     IF( IPFL .GT. 0 ) GO TO 400
                        IF( IPHT .LT. 0 ) HVTHT(-IPHT) = HVTHT(-IPHT) -3
                        IF( IPHT .GT. 0 ) HVTHT( IPHT) = HVTHT(IPHT) + 5
                        GO TO 600
  400                HVTHT(IPHT) = IPFL
                     GO TO 600
  500             HVTHT(IPHT) = 0
  600          CONTINUE
C
  700       CONTINUE
C
C
  800    DO 900 I = 1, 500
            IVPAT(1,I) = 0
            IVPAT(2,I) = 0
            HTUSED(I)  = 0
  900    CONTINUE
C
         CALL CLOC( IPVPAT, 'VPAT', NBANK )
         IF( IPVPAT .LE. 0 ) GO TO 1350
C
            ILVPAT = IDATA(IPVPAT)*2
            IPVPA2 = IPVPAT*2
            ILHDVP = HDATA(IPVPA2+1)
            ILTLVP = HDATA(IPVPA2+4)
            ISMTRL = HDATA(IPVPA2+5)
C
            DO 1300 I = 1, ISMTRL
               IPAC = IPVPA2 + ILHDVP + (I-1)*ILTLVP + 5
               IF( IPAC .LE. IPVPA2+ILVPAT ) GO TO 1100
      CALL TRMOUT(80,'DISPVC : VPAT-BANK --> NUMBER OF TRACKS IN HEADER
     &IS WRONG !^')
               GO TO 1350
 1100          NHTL   = HDATA(IPAC)
               NIDT   = HDATA(IPAC-4)
               IPPATR = IDATA(IBLN('PATR'))
               IPHWDS = IDATA(IBLN('HWDS'))
               IF( FLVCDO(15) .AND. IPHWDS .GT. 0 ) IPPATR = IPHWDS
               IPTRK  = IPPATR + IDATA(IPPATR+1)
     &                                 + (NIDT-1)*IDATA(IPPATR+3)
C
               LCOMFT = .FALSE.
               IF( IDATA(IPPATR+3).LT.64 ) GOTO 1190
                  MASK1 = IDATA(IPTRK+60)
                  MASK2 = IDATA(IPTRK+61)
                  CALL VHITLD(MASK1,MASK2)
                  LCOMFT = .TRUE.
 1190          CONTINUE
               DO 1200 J = 1, NHTL
                  IPHL = HDATA(IPAC+J)
                  IF( IPHL .GT. 0 ) IVPAT(1, IPHL) = IVPAT(1,IPHL) + 5
                  IF( IPHL .LT. 0 ) IVPAT(1,-IPHL) = IVPAT(1,-IPHL) - 3
                  IVPAT(2, IABS(IPHL)) = IPTRK
                  IF( .NOT.LCOMFT ) GOTO 1200
                     IPHTVC = IPVTX2 + 6 + IABS(IPHL)*4
                     IWT = HDATA(IPHTVC+1)
                     ILY = MOD(IWT-1,7) + 1
                     IF( HVMSK(ILY) .EQ. 2 ) HTUSED(IABS(IPHL)) = 1
 1200          CONTINUE
C
 1300       CONTINUE
C
 1350    DO 1360 I = 1, 500
            IVPRE(1,I) = 0
            IVPRE(2,I) = 0
 1360    CONTINUE
C
         CALL CLOC( IPVPRE, 'VPAT', NBANK+10 )
         IF( IPVPRE .LE. 0 ) GO TO 1400
C
            ILVPRE = IDATA(IPVPRE)*2
            IPVPR2 = IPVPRE*2
            ILHDVR = HDATA(IPVPR2+1)
            ILTLVR = HDATA(IPVPR2+4)
            ISMTRR = HDATA(IPVPR2+5)
C
            DO 1390 I = 1, ISMTRR
               IPACR = IPVPR2 + ILHDVR + (I-1)*ILTLVR + 5
               IF( IPACR .LE. IPVPR2+ILVPRE ) GO TO 1370
      CALL TRMOUT(80,'DISPVC : VPAT-BANK --> NUMBER OF TRACKS IN HEADER
     &IS WRONG !^')
               GO TO 1400
 1370          NHTLR  = HDATA(IPACR)
               NIDTR  = HDATA(IPACR-4)
               IPPATR = IDATA(IBLN('PATR'))
               IPHWDS = IDATA(IBLN('HWDS'))
               IF( FLVCDO(15) .AND. IPHWDS .GT. 0 ) IPPATR = IPHWDS
               IPTRK  = IPPATR + IDATA(IPPATR+1)
     &                                 + (NIDTR-1)*IDATA(IPPATR+3)
C
               DO 1380 J = 1, NHTLR
                  IPHLR = HDATA(IPACR+J)
                  IF(IPHLR .GT. 0) IVPRE(1, IPHLR) = IVPRE(1, IPHLR) + 5
                  IF(IPHLR .LT. 0) IVPRE(1,-IPHLR) = IVPRE(1,-IPHLR) - 3
                  IVPRE(2, IABS(IPHLR)) = IPTRK
 1380          CONTINUE
C
 1390       CONTINUE
C
 1400    CONTINUE
         IPACTU = IPVTX2 + NHDLV*2 + 1
         NEV = IPACTU
         IIA = 0
 1500       CONTINUE
              IIA = IIA + 1
              IF( IIA .GT. 500 ) GO TO 1530
                 IFLH  = HVTHT(IIA)
                 IFLP  = IVPAT(1,IIA)
                 IPTRK = IVPAT(2,IIA)
                 IFLPR = IVPRE(1,IIA)
                 IPTRR = IVPRE(2,IIA)
                 GO TO 1570
 1530         IFLH  = 0
              IFLP  = 0
              IPTRK = 0
              IFLPR = 0
              IPTRR = 0
 1570         IM = 1
              IF( IFLP .EQ. 0 ) IM = -1
              IF( IFLP .NE. 0 .OR. IFLPR .EQ. 0 ) GO TO 1580
                 IM    = 1
                 IPTRK = IPTRR
 1580         CALL FICOOV( MODVW-1, IPACTU, NWEV, IM, IPTRK )
              IF( NWEV .LT. -500 ) GO TO 2000
              IF( NWEV .EQ. 0 .OR. IPRJC .NE. 0 ) GO TO 1900
              IF( MODVW .EQ. 2 ) GO TO 1810
              IF( .NOT. FLVCDO(1) ) GO TO 1600
                    CALL HITMRK( JNDEX, -X1, Y1, 1.5, 0 )
                    CALL HITMRK( JNDEX, -X2, Y2, 1.5, 0 )
 1600         IF( .NOT. FLVCDO(2) ) GO TO 1700
                    IF( IFLH .LT. 0 .OR. IFLH .EQ. 2 )
     &                         CALL PLYGON( 8, 1.75, -X1, Y1, 0 )
                    IF( IFLH .GT. 0 )
     &                         CALL PLYGON( 8, 1.75, -X2, Y2, 0 )
 1700         IF( .NOT. FLVCDO(3) ) GO TO 1750
                    IDSC = -1
                    IF( HTUSED(IIA) .EQ. 1 ) IDSC = 0
                    IF( IFLP .LT. 0 .OR. IFLP .EQ. 2 )
     &                         CALL PLYGON( 4, 2.75, -X1, Y1, IDSC )
                    IF( IFLP .GT. 0 )
     &                         CALL PLYGON( 4, 2.75, -X2, Y2, IDSC )
 1750         IF( .NOT. FLVCDO(7) ) GO TO 1800
                    IF( IFLPR .LT. 0 .OR. IFLPR .EQ. 2 )
     &                         CALL PLYGON( 3, 2.75, -X1, Y1, 0 )
                    IF( IFLPR .GT. 0 )
     &                         CALL PLYGON( 3, 2.75, -X2, Y2, 0 )
 1800         IF( INDEX .NE. 20 .OR.
     &            ABS(XMAX-XMIN) .GT. 0.5*ABS(XMAXST(20) - XMINST(20))
     &            .OR. .NOT. FLVCDO(1) )
     &                                                GO TO 1900
                    CALL HITSCL( -X1, Y1, 1.5 )
                    CALL HITSCL( -X2, Y2, 1.5 )
C
                    IF( .NOT. FLVCDO(5) ) GO TO 1900
                       IF( ABS(XMAX-XMIN) .GT.
     &                                0.2*ABS(XMAXST(20) - XMINST(20)) )
     &                                                GO TO 1900
                          CALL HITPNT( -X1, Y1, -X2, Y2, IIA )
                          GO TO 1900
C
 1810          IF( NBANK .EQ. 9 ) GO TO 1900
               LZDSP = .FALSE.
               IF( FLVCDO(1) ) LZDSP = .TRUE.
               IF( FLVCDO(1) .OR. FLVCDO(2) .OR. .NOT.FLVCDO(3) )
     &                                             GO TO 1815
                  IF( IFLP .NE. 0 ) LZDSP = .TRUE.
 1815          IF( (.NOT.DSPDTL(9)) .OR. DSPDTL(10) ) GO TO 1820
                  IF( LZDSP ) CALL HITMRK( JNDEX, ZET, R, 1.5, 0 )
                  GO TO 1860
 1820          CONTINUE
               DO 1850  LRHIT = 1, 2
                  IF( LRHIT .EQ. 2 ) GO TO 1830
                     RLRHI = R1
                     FILRH = FI1
                     GO TO 1840
 1830             CONTINUE
                     RLRHI = R2
                     FILRH = FI2
 1840             CONTINUE
                  Y0 = RLRHI
C                                             GET PROJECTED Y-COORDINATE
                  IF( INDEX .LE. 7 ) YPRO =  RLRHI*COS(FILRH)
                  IF( INDEX .GT. 7 ) YPRO =  RLRHI*SIN(FILRH)
                  IF(.NOT.DSPDTL(9)) Y0 = YPRO
                  IF( LZDSP ) CALL HITMRK( JNDEX, ZET, Y0, 1.5, 0 )
C
 1850          CONTINUE
 1860          CONTINUE
C
 1900       IF( IPACTU .LE. (NWOV*2 - 4 + NEV) ) GO TO 1500
C
 2000 CONTINUE
C
      RETURN
      END
