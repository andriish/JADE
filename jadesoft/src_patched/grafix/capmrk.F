C   01/11/84 807241307  MEMBER NAME  CAPMRK   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE CAPMRK(INDEX,IESUM)
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. OLSSON        ?     :  DRAW BEAM CROSS AND EVENT INFO
C
C        MOD: J. OLSSON    20/12/83  :
C        MOD: C. BOWDERY   26/04/83  :  SMALLER ARROW INSIDE BEAM PIPE
C        MOD: J. HAGEMANN  19/10/84  :  FOR SPECIAL VERTEX CHAMBER VIEW
C        MOD: J. OLSSON    15/04/85  :  FOR TRIG1 WITH BANKDESCR.4
C        MOD: C. BOWDERY   17/04/85  :  VERTEX CHAMBER CAPTION ADDED
C        MOD: J. OLSSON    26/04/85  :  T2 COINC. PRINT FOR 1985 DATA..
C        MOD: C. BOWDERY   20/12/85  :  OPTION TO SUPPRESS CAPTION, ETC
C   LAST MOD: J. HAGEMANN  18/02/86  :  FOR VERTEX CHAMBER RUN VERTEX
C
C     INDEX GIVES THE VIEW NUMBER, FOR POSITIVE INDEX
C     NEGATIVE INDEX ARE FOR SPECIAL VIEWS
C       INDEX = -1, LSTCMD = 119   DEDX
C       INDEX = -2, LSTCMD = 121   TOF
C       INDEX = -3, LSTCMD = 124   FADC
C       INDEX = -4, LSTCMD = 125   ZTRG
C
C     IESUM IS A DUMMY, HISTORICAL ARGUMENT
C
C-----------------------------------------------------------------------
C
      IMPLICIT integer*2 (H)
C
C       character*2 HBLANK, HPLUS, HMINUS, HSIGN
      REAL*8      DATE,TIME
      LOGICAL     FL18,FL22,FL24
      LOGICAL     DSPDTM
      LOGICAL*1   TRGW1(2)
C
#include "cgraph.for"
#include "cdata.for"
#include "cgeo1.for"
#include "chsym.for"
C
       CHARACTER*2  HBLANK,HMINUS, HTX1,HTX, HSYM, HBUF,HDATSV
        CHARACTER*2   HTX2, HTX3,HTEXT, HMW,HTM2,HTM1

      COMMON / CHEADR / HEAD(108)
      COMMON / CGRAP2 / BCMD,DSPDTM(30),ISTVW,JTVW
      COMMON / CWORK1 / R,FI,R1,FI1,X1,Y1,R2,FI2,X2,Y2,ZET,X3,Y3,X4,Y4,
     +                  IMW(200)
      COMMON / CPROJ  / XMINR,XMAXR,YMINR,YMAXR,IPRJC,FL18,FL22,FL24
C
      DIMENSION HTEXT(6,6),HBUF(6),JSUM(6),HDATSV(22),HTX(136)
      DIMENSION HTM1(4),HTM2(4),HMW(400),HTX1(32),HTX2(42),HTX3(62)
C
      EQUIVALENCE (HBUF(1),HMW(69)),(HRGW,TRGW1(1)),(HMW(1),IMW(1))
      EQUIVALENCE (HDATSV(1),IDATSV(1)),(DATE,HTM1(1)),(TIME,HTM2(1))
      EQUIVALENCE (HTX(1),HTX1(1)),(HTX(33),HTX2(1)),(HTX(75),HTX3(1))
C
      DATA HBLANK/'  '/,HMINUS/'- '/, MASK2/Z'20000'/, MASK4/Z'40000'/
      DATA HTX1/'DS','N ','DA','TE','TI','ME','JA','DE','  ','DE','TE',
     $'CT','OR','R-','FI',' S','EC','TI','ON','Z-','X ',' S','EC','TI',
     $'ON','Z-','Y ',' S','EC','TI','ON','FO'/
      DATA HTX2/'RW','AR','D ','DE','TE',
     $'CT','OR','SH','OW','ER',' C','OU','NT','ER','S ','R-','FI',' P',
     $'ER','SP','EC','TI','VE','MO','NT','E ','CA','RL','O ',
     $'PR','OJ','EC','T ','MO','DE','RO','TA','TE',' M','OD','E ','  '/
      DATA HTX3/'FW','-M','UO','N ','CO','UN','TE','RS','DE','DX','TO',
     $ 'F ','FL','AS','H ',' A','DC',' Z','-C','HA','MB','ER','  ','RZ',
     $ '-T','RI','GG','ER','VE','RT','EX',' R','EG','IO','N ','RF','I ',
     $ 'VE','RT','EX',' R','EG','IO','N ','ZX','VE','RT','EX',' R','EG',
     $ 'IO','N ','ZY','VE','RT','EX',' C','HA','MB','ER',' R','FI'/
      DATA HTEXT /'I ','D ','H ','I ','T ','S ','E ','L ','G ','T ','O '
     $,'T ','M ','U ','H ','I ','T ','S ','L ','G ','C ','Y ','L ','  ',
     $ 'L ','G ','C ','A ','P ','S ','F ','W ','C ','A ','P ','S '/
*** PMF 17/11/99: add variables needed for emulation of DESYLIB routine 'CORE'  
      CHARACTER cHMW*800
      EQUIVALENCE (cHMW,HMW(1))
*** PMF(end)
C
C------------------  C O D E  ------------------------------------------
C
C                            FOR THE STANDARD VIEWS, DRAW BEAM
C                            CROSS IN INTERACTION POINT
C
      call setcol('TEXT') ! PMF 24/11/99: set colour
      IF( INDEX .LE. 0 ) GO TO 30
C
C                            VIEWS RU,FW, RZ, FWMU HAVE NO BEAM CROSS
C
      IF( INDEX .EQ. 12  .OR.  INDEX .EQ. 13 ) GO TO 20
      IF( INDEX .EQ. 15  .OR.  INDEX .EQ. 16 ) GO TO 20
C
C                            SET DIMENSION OF BEAM CROSS
C
      AW = 10.
      BW = 43.
      CW = 20.
C
      IF( INDEX .LT. 17  .OR.  INDEX .GT. 19 ) GO TO 10
C
        AW = 0.1
        BW = 0.2
        CW = 0.12
C
  10  DW = BW + CW
C
C                            DRAW ARROW TO MARK BEAM AND AXIS DIRECTION
C                            UNLESS OPTION 47 IS ON
C
      IF( DSPDTM(17) ) GO TO 15
      SIGN = 1.0
      IF( INDEX .LE. 3  .OR.  INDEX .EQ. 14  .OR.  INDEX .EQ. 17  .OR.
     +    INDEX .EQ. 20 )  SIGN = -1.0
C
      CALL MOVEA(-SIGN*CW, 0.0)
      CALL DRAWA( SIGN*BW, 0.0)
      CALL DRAWA( SIGN*BW, AW )
      CALL DRAWA( SIGN*DW, 0.0)
      CALL DRAWA( SIGN*BW, -AW)
      CALL DRAWA( SIGN*BW, 0.0)
C
      CALL MOVEA(0.0, -CW)
      CALL DRAWA(0.0,  CW)
  15  IF( INDEX .GE. 17  .AND.  INDEX .LE. 19 ) CALL VRXCIR
C
C                                 BEAM SPOT FOR REAL DATA
      IF( HEAD(18) .LE. 100 )  GO TO 20
      IF( INDEX .NE. 17  .AND.  INDEX .NE. 20 ) GO TO 20
         CALL VTXCRV( INT(HEAD(18)), XRV, YRV, DXR, DYR ) ! PMF 08/11/99: add run argument HEAD(18)
         CALL ELLIPS( 1, INDEX, 0, XRV, YRV, SQRT(DXR), SQRT(DYR), 0.0 )
C
C                            LSTCMD=52: VRES,  FL18  PROJECTIONS
C                            THESE NEED NO FURTHER PROCESSING HERE
C
  20  IF( LSTCMD .EQ. 52  .OR.  FL18 ) goto 999! RETURN
C
C                            DRAW COORDINATE AXES
C
      call setcol('JADE')
      CALL CROAX(INDEX)
      call setcol('TEXT')
C
C                            IF PUBLICATIONS OPTION SET, RETURN
C
  30  IF( DSPDTM(16) ) goto 999!RETURN
C
C                            WRITE OUT EVENT CAPTION.
C
      XXX = XMIN + 0.01*(XMAX - XMIN)
      YYY = YMIN + 0.76*(YMAX - YMIN)
      CALL CHRSIZ(4)
      CALL CSIZE(IHOR,IVER)
      SSS = IVER*(YMAX - YMIN)*1.0/6240.0*.75 ! PMF20/11/99: add factor .75
C
C                            WRITE DSN ON PICTURE
C
      HMW(1) = HTX(1)
      HMW(2) = HTX(2)
C
      DO  40  I = 1,22
        HMW(I+2) = HDATSV(I)
  40  CONTINUE
C
      CALL MOVEA(XXX,YYY)
      CALL EOUTST(48,HMW)
C
C                            WRITE RUN,EVENT,RECORD ON PICTURE
C
      KCNT = 0
      DO  60  III = 1,3
        IF( III    .EQ. 1 ) NUMCAP    = HEAD(18)
        IF( III    .EQ. 2 ) NUMCAP    = HEAD(19)
        IF( III    .EQ. 3 ) NUMCAP    = ICREC
        IF( NUMCAP .LT. 0 ) KCNT      = KCNT + 1
        IF( NUMCAP .LT. 0 ) HMW(KCNT) = HMINUS
        NUMCAP = IABS(NUMCAP)
        NDIG   = 1
        ACAP   = NUMCAP
        IF( NUMCAP .GT. 0 ) NDIG = 1 + ALOG10( ACAP + 0.001 )
        IF( NDIG   .GT. 8 ) NDIG = 8
C
        DO  50  JJJ = 1,NDIG
          KKK      = KCNT + NDIG - JJJ + 1
          NTEN     = NUMCAP/10
          IDIG     = NUMCAP - 10*NTEN
          HMW(KKK) = HSYM(IDIG + 1)
          NUMCAP   = NTEN
  50    CONTINUE
C
        KCNT = KCNT + NDIG
        IF( III .LT. 3 ) KCNT      = KCNT + 1
        IF( III .LT. 3 ) HMW(KCNT) = HBLANK
  60  CONTINUE
C
      YYY = YYY - 3.5*SSS
      CALL HEMSYM(XXX,YYY,SSS,HMW,KCNT,0.)
      XX1 = XMIN + 0.12*(XMAX - XMIN)
      IF( HEAD(18) .GT. 100 ) GO TO 80
C
C                            WRITE   JADE   ON PICTURE
C                            IF RUN < 100, WRITE MONTE CARLO ON PICTURE
C
      DO  70  I = 1,6
        HMW(I) = HTX(55+I)
  70  CONTINUE
C
      CALL MOVEA(XX1,YYY)
      CALL EOUTST(12,HMW)
C
  80  YYY = YYY - 4.0*SSS
      CALL MOVEA(XX1,YYY)
C
      DO  90  I = 1,2
        HMW(I) = HTX(6+I)
  90  CONTINUE
C
      CALL CHRSIZ(3)
      CALL EOUTST(4,HMW)
      CALL CHRSIZ(4)
      YYY = YYY + 4.0*SSS
C
C                            COMPUTE LEADGLASS ENERGY SUMS,NUMBER OF
C                            HITS IN ID AND MUON FILTER
C
      DO  100 IJK = 1,6
        JSUM(IJK) = 0
 100  CONTINUE
C
      CALL ERGTOT(ECYL,ECAMI,ECAPL)
      JSUM(4) = ECYL
      JSUM(5) = ECAMI
      JSUM(6) = ECAPL
C
      IPJ = IDATA(IBLN('JETC'))
      IF( IPJ .GT. 0 ) IPJ     = (IPJ + 1)*2
      IF( IPJ .NE. 2 ) JSUM(1) = (HDATA(IPJ+97)-HDATA(IPJ+1))/4
      JSUM(2) = ECYL + ECAMI + ECAPL
      CALL MUHITS(NMU)
      JSUM(3) = NMU
C
C                            WRITE EVENT INFORMATION INTO CAPTION
C
      DO  130  IJK = 1,5
        YYY = YYY - 1.5*SSS
C
        DO  110  JJK = 1,6
          HMW(JJK) = HTEXT(JJK,IJK)
 110    CONTINUE
C
        HMW(7) = HBLANK
        JOLLA  = JSUM(IJK)
        CALL NUMCOD(JOLLA,HBUF,KKK)
C
        DO  120  JJK = 1,6
          HMW(JJK+7) = HBUF(JJK)
 120    CONTINUE
C
        IF( IJK .NE. 5 ) CALL HEMSYM(XXX,YYY,SSS,HMW,13,0.)
 130  CONTINUE
C
      HMW(14) = HBLANK
      JOLLA   = JSUM(6)
      CALL NUMCOD(JOLLA,HBUF,KKK)
C
      DO  140  JJK = 1,6
        HMW(JJK+14) = HBUF(JJK)
 140  CONTINUE
C
      CALL HEMSYM(XXX,YYY,SSS,HMW,20,0.)
      CALL TAGCHK(IPHTGM,IPHTGP)
      YYY = YYY - 1.5*SSS
      DO  150  JJK = 1,6
        HMW(JJK) = HTEXT(JJK,6)
 150  CONTINUE
C
      HMW(7) = HBLANK
      JOLLA  = IPHTGM
      CALL NUMCOD(JOLLA,HBUF,KKK)
C
      DO  160  JJK = 1,6
        HMW(JJK + 7) = HBUF(JJK)
 160  CONTINUE
C
      HMW(14) = HBLANK
      JOLLA   = IPHTGP
      CALL NUMCOD(JOLLA,HBUF,KKK)
C
      DO  170  JJK = 1,6
        HMW(JJK + 14) = HBUF(JJK)
 170  CONTINUE
C
      CALL HEMSYM(XXX,YYY,SSS,HMW,20,0.)
C
C                            WRITE CAPTION "VIEW INFORMATION"
C
      IF( INDEX .GT.  0 ) GO TO 190
C
C                            SPECIAL VIEWS HERE
C
      IF( INDEX .LT. -2 ) GO TO 180
      LODO = 2
      IADR = 82
      IF( INDEX .EQ. -2 ) IADR = 84
      GO TO 220
 180  IF( INDEX .LT. -4 ) GO TO 190
      LODO = 5
      IADR = 86
      IF( INDEX .EQ. -4 ) IADR = 97
      GO TO 220
C
C                            STANDARD VIEWS HERE
C
 190  YYY=YMIN + 0.75*(YMAX-YMIN) - 2.*SSS
C
      LODO = 9
      IADR = 127
      IF( INDEX .EQ. 20 ) GO TO 220
C
      LODO = 6
      JNDX = 1
      IF(INDEX.GT. 3) JNDX = 2
      IF(INDEX.GT. 7) JNDX = 3
      IADR = 7+JNDX*6
      IF( INDEX .LT. 12 ) GO TO 220
      LODO = 8
      JNDX = INDEX - 11
      IADR = 23 + JNDX*8
      IF( INDEX .LT. 15 ) GO TO 220
      IF(INDEX.GT.15) GO TO 200
      LODO = 8
      IADR = 74
      GO TO 220
 200  IF(INDEX.GT.16) GO TO 210
      LODO = 6
      IADR = 91
      GO TO 220
 210  IF( INDEX .LT. 17  .OR.  INDEX .GT. 19 ) GO TO 220
      LODO = 9
      IF(INDEX.NE.17) LODO = 8
      IADR = 102
      IF(INDEX.EQ.18) IADR = 111
      IF(INDEX.EQ.19) IADR = 119
C
 220  XXX = XMAX - .77*(XMAX-XMIN)
      CALL MOVEA(XXX,YYY)
C
      DO  250  I = 1,LODO
        HMW(I) = HTX(IADR + I)
 250  CONTINUE
C
      LODO = 2*LODO
      CALL EOUTST(LODO,HMW)
      IF( INDEX .LE. 0 ) GO TO 295
C
C                            WRITE INFORMATION FOR ZX OR ZY VIEW,
C                            ROTATED OR PROJECTED MODE
C
      IF( INDEX .GT.  3  .AND.  INDEX .LT. 12 ) GO TO 280
      IF( INDEX .EQ. 18  .OR.   INDEX .EQ. 19 ) GO TO 280
      GO TO 295
 280  YYY  = YMIN + 0.735*(YMAX - YMIN) - 2.0*SSS
      JNDX = 1
      IF( DSPDTL(9) ) JNDX = 2
      LODO = 6
      IADR = 55 + JNDX*LODO
      CALL MOVEA(XXX,YYY)
C
      DO  290  I = 1,LODO
        HMW(I) = HTX(IADR + I)
 290  CONTINUE
C
      LODO = 2*LODO
      CALL EOUTST(LODO,HMW)
C
C                            WRITE BEAM ENERGY AND MAGNETIC FIELD
C
 295  XXX  = XMAX - 0.57*(XMAX - XMIN)
      YYY  = YMIN + 0.75*(YMAX - YMIN)
      CALL MOVEA(XXX,YYY)
      EBEAM = HEAD(37)*0.001
      BFLD  = HEAD(38)*0.001
      CALL CORE(HMW,44)
      WRITE(cHMW,300) EBEAM,BFLD,HEAD(30)  ! PMF 17/11/99: JUSCRN changed to cHMW
 300  FORMAT(' BEAM',F7.3,' GEV  FIELD ',F6.3,' KG  TALC ',Z4)
      CALL EOUTST(44,HMW)
C
C                            WRITE TRIGGER INFORMATION, FROM BANK TRIG 1
C
      ITRG = IDATA(IBLN('TRIG'))
      IF( ITRG .GT. 0 ) GO TO 350
      CALL TRMOUT(80,'TRIGGER BANK NOT PRESENT^')
      GO TO 400
 350  IF( IDATA(ITRG - 2) .EQ. 1 ) GO TO 370
      CALL TRMOUT(80,'FIRST TRIGGER BANK NUMBER NE.1^')
      GO TO 400
 370  IBWSC  = LAND(IDATA(ITRG+1),MASK2)
      IBWSC4 = LAND(IDATA(ITRG+1),MASK4)
      ITRG2  = 2*ITRG
      ITRGL  = IDATA(ITRG)*2
      HRGW   = HDATA(ITRG2+ITRGL)
      HRGWA  = HDATA(ITRG2+ITRGL-2)
      HRGWP  = HDATA(ITRG2+ITRGL-1)
      HRGWA1 = HDATA(ITRG2+ITRGL-4)
      HRGWP1 = HDATA(ITRG2+ITRGL-3)
      HRGWT2 = HDATA(ITRG2+ITRGL)
      XXX = XMAX - 0.53*(XMAX - XMIN)
      YYY = YYY  - 2.0*SSS
      CALL MOVEA(XXX,YYY)
      CALL CORE(HMW,39)
      IF( IBWSC  .EQ. 0  .AND.  IBWSC4 .EQ. 0 )
     +     WRITE(cHMW,372) HRGW ! PMF 17/11/99: JUSCRN changed to cHMW
      IF( IBWSC  .NE. 0  .AND.  IBWSC4 .EQ. 0 )
     +     WRITE(cHMW,374) HRGWA,HRGWP ! PMF 17/11/99: JUSCRN changed to cHMW
      IF( IBWSC4 .NE. 0 )  WRITE(cHMW,376) HRGWA1,HRGWP1,HRGWT2 ! PMF 17/11/99: JUSCRN changed to cHMW
 372  FORMAT(16X,'TRIG ',Z4,14X)
 374  FORMAT(16X,' T1A ',Z4,'  T1P ',Z4,4X)
 376  FORMAT(12X,' T1A ',Z4,' T1P ',Z4,' T2C ',Z4)
      CALL EOUTST(39,HMW)
      YYY = YYY + 2.0*SSS
C
C                            WRITE DATE AND TIME
C
 400  XXX = XMAX - 0.23*(XMAX - XMIN)
      CALL DAY(DATE,TIME)
      HMW(1) = HTX(3)
      HMW(2) = HTX(4)
      HMW(3) = HBLANK
C
      DO  450  I = 1,4
        HMW(I+3) = HTM1(I)
 450  CONTINUE
C
      CALL MOVEA(XXX,YYY)
      CALL EOUTST(14,HMW)
      HMW(1) = HTX(5)
      HMW(2) = HTX(6)
      HMW(3) = HBLANK
C
      DO  480  I = 1,4
        HMW(I+3) = HTM2(I)
 480  CONTINUE
C
      XXX = XMAX - 0.115*(XMAX-XMIN)
      CALL MOVEA(XXX,YYY)
      CALL EOUTST(14,HMW)
C
C                            WRITE CAMAC TIME
C
      XXX = XMAX - 0.23*(XMAX-XMIN)
      YYY = YYY  - 2.0*SSS
      CALL MOVEA(XXX,YYY)
      CALL CORE(HMW,32)
      WRITE(cHMW,500) (HEAD(I),I=11,16) ! PMF 17/11/99: JUSCRN changed to cHMW
 500  FORMAT('CAMAC TIME ',I2,'.',I2,'.',I2,2X,I2,'/',I2,'/',I4)
      CALL EOUTST(32,HMW)
 999  continue
      call setcol(' ') ! PMF 24/11/99: reset colour
      RETURN
      END
