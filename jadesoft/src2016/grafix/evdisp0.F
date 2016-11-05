C   01/11/84 807251536  MEMBER NAME  EVDISP0  (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE EVDISP(INDEX) 
C-----------------------------------------------------------------------
C
C   AUTHOR: J. OLSSON      ?    : EVENT DISPLAY
C
C      MOD: C. BOWDERY   13/03/84 : CDTL 20/21 REVERSED TRUE/FALSE
C      MOD: C. BOWDERY   21/03/84 : FWCHEV ALWAYS CALLED FOR RU VIEW NOW
C      MOD: C. BOWDERY   23/03/84 : FWCHEV NOT CALLED IN CYL ANYMORE
C      MOD: C. BOWDERY   28/04/84 : VERTEX CHAMBER CODE FROM HAGEMANN
C      MOD: C. BOWDERY   10/06/84 : NEW COMMAND NUMBERS.
C      MOD: C. BOWDERY   20/06/84 : CLEAR DETDRW FLAG WHEN CALLED
C      MOD: C. BOWDERY   27/06/84 : CDTL 1 OFF ONLY NOT DRAWS ID NOW
C      MOD: J. HAGEMANN  10/10/84 : INCLUDE DRAWING OF VERTEX CHAMBER
C                                   REAL DATA / OTHER SUBROUTINES
C                                   REMOVED (NOW OWN MEMBER)
C      MOD: J. HAGEMANN  19/10/84 : SPECIAL VERTEX CHAMBER VIEW ADDED
C      MOD: J. HAGEMANN  29/11/84 : UPDATED VERTEX CHAMBER CODE
C      MOD: J. HAGEMANN  30/09/85 : CHANGES FOR VTHT- VPAT -BANKS
C      MOD: J. HAGEMANN  13/03/86 : EXTENDED COMMON CVX
C      MOD: J. HAGEMANN  20/05/86 : DRAW BP-COUNTER FOR INDEX=20 IF SET
C      MOD: J. HAGEMANN  16/12/86 : FOR VTXC Z-COORDINATE DISPLAY
C LAST MOD: J. HAGEMANN  27/02/87 : FOR 1986 ID-DATA SMALLER CROSSES IN
C                                   R-FI-VIEWS
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL TBIT
      LOGICAL DSPDTM
      LOGICAL INNERC,LEADGL,MUONFL,PHNUMS,PHHIST
      LOGICAL FL18,FL22,FL24
      LOGICAL DETDRW,REDRDT
      LOGICAL LVTXC
      LOGICAL FLVCDO
C
#include "cgraph.for"
#include "cjdrch.for"
#include "cgeov.for"
#include "cjvcel.for"
#include "cjvtxc.for"
C
#include "cgeo1.for"
#include "cgeo2.for"
#include "cdata.for"
C
      COMMON / CHEADR / HEAD(108)
      COMMON / CWORK1 / R,FI,R1,FI1,X1,Y1,R2,FI2,X2,Y2,ZET,X3,Y3,X4,Y4,
     +                  KZAMP,IMW(132)
      COMMON / CPROJ  / XMINR,XMAXR,YMINR,YMAXR,IPRJC,FL18,FL22,FL24
C
      COMMON / CJTRIG / PI,TWOPI
      COMMON / CVCEX  / LVTXC
      COMMON / CVX    / NNPATR,NDUMM,NNJETC,NNVTXC
      COMMON / CADC   / CATAG(190)
      COMMON / CGRAP2 / BCMD,DSPDTM(30),ISTVW,JTVW
      COMMON / CGRAP6 / DETDRW
      COMMON / CGVCDO / FLVCDO(20)
C
      DIMENSION HMW(264)
C
      EQUIVALENCE (IMW(1),HMW(1))
C
      DATA ICCO  / 0 /
C
C------------------  C O D E  ------------------------------------------
C
      CRLEN = 4.0
      IF( HEAD(16) .EQ. 1986 ) CRLEN = 2.75
C
      IF( ICCO .EQ. 1 ) GO TO 1
      ICCO   = 1
      DEFIX  = TWOPI / 84.0
      SH1    = BLDEP *  0.10
      SH2    = BLFI  *  0.16667
C
      ADX    =    0.0
      ADY    = 2800.0
      YADD   =  900.0
      ADDEND =   84.0     * BLFI
      BLSEX  =    0.16667 * BLXY
C
   1  JNDEX  = INDEX
      IF( INDEX .GT. 7 ) JNDEX = INDEX - 4
      IF( INDEX .LT. 1  .OR.  INDEX .GT. 20 ) RETURN
C
      LASTVW = INDEX
      DETDRW = .FALSE.
C
C                            SET DISPLAY FLAGS
C
      INNERC = JNDEX.LE.7  .AND.  DSPDTL(1)  .AND.  DSPDTL(21)
      LEADGL = (JNDEX.EQ.2.OR.JNDEX.EQ.3.OR.JNDEX.EQ.5.OR.JNDEX.EQ.6.OR.
     +          JNDEX.EQ.7)  .AND.  DSPDTL(2)
      MUONFL = (JNDEX.EQ.3.OR.JNDEX.EQ.6.OR.JNDEX.EQ.7).AND.DSPDTL(3)
      PHNUMS = LEADGL  .AND.  ( .NOT. DSPDTL(7))
      PHHIST = LEADGL  .AND.  DSPDTL(7)
C
C                            GET COMMAND ARGUMENT. MAY BE ZERO.
C
      NECC   = ACMD
C
C                            MARK INTERACTION POINT AND
C                            DRAW ARROW TOWARD CENTER OF PETRA.
C
      CALL CAPMRK(INDEX,IESUM)
C
C                            NOTHING MORE TO DO FOR VERTEX REGION VIEWS
C
      IF( INDEX .GE. 17  .AND.  INDEX .LE. 19 ) RETURN
C
C                            SPECIAL VERTEX CHAMBER VIEW
C
      IF( INDEX .EQ. 20 ) GO TO 7050
C
C                            SPECIAL STEFFEN LG HISTOGRAM REQUIRED?
C
      IF( .NOT. PHHIST ) GO TO 5003
C     CALL TRMOUT(80,'ENERGY HISTOGRAMS NOT AVAILABLE.^')
      IPJ=IDATA(IBLN('ALGN'))
      IF(IPJ.LE.0) GO TO 5003
      NWO=IDATA(IPJ)
      IF(NWO.LE.3) GO TO 5003
      IPJ=2*IPJ
      LIM1=IPJ+8
      LIM2=IPJ+2*NWO
      DO 5005 IJK=1,84
      IMW(IJK)=0
 5005 CONTINUE
      DO 5006 IJK=LIM1,LIM2,2
      IENER=HDATA(IJK)
      IF(IENER.LE.0) GO TO 5006
      IADDR=HDATA(IJK-1)
      IF(IADDR.GT.2687) GO TO 5006
      NROW=IADDR/32+1
      IHINDX=NROW
      IF(JNDEX.LT.4) GO TO 5007
      IHINDX=IADDR-32*(NROW-1)
      IF((INDEX.LE.7).AND.(NROW.GT.21).AND.(NROW.LT.64))
     1IHINDX=IHINDX+32
      IF((INDEX.GT.7).AND.(NROW.GT.42)) IHINDX=IHINDX+32
 5007 CONTINUE
      IMW(IHINDX)=IMW(IHINDX)+IENER
 5006 CONTINUE
      IF(JNDEX.GT.3) GO TO 5008
      LDRAW=0
      DO 5014 IJK=1,84
      LAST=IJK-1
      IF(LAST.EQ.0) LAST=84
      NEXT=IJK+1
      IF(NEXT.EQ.85) NEXT=1
      IF(IMW(IJK).GT.1) GO TO 5015
      IF(IMW(LAST).GT.1) GO TO 5015
      IF(IMW(NEXT).GT.1) GO TO 5015
      GO TO 5014
 5015 CONTINUE
      PHI=DEFIX*IJK
      X1=-RLG*COS(PHI)
      Y1=RLG*SIN(PHI)
      IF(LDRAW.EQ.LAST) GO TO 5016
      X0=-RLG*COS(PHI-DEFIX)
      Y0=RLG*SIN(PHI-DEFIX)
      CALL MOVEA(X0,Y0)
 5016 CONTINUE
      CALL DRAWA(X1,Y1)
      LDRAW=IJK
 5014 CONTINUE
      AMP=IMW(84)
      IF(IMW(84).LE.1) AMP=0.
      IF(IMW(84).GT.1) AMP=60.*ALOG10(AMP)
      X0=-RLG-AMP
      Y0=0.
      CSSP=1.
      SSSP=0.
      IF(AMP.GT.0.) CALL MOVEA(X0,Y0)
      DO 5009 IJK=1,84
      AMP=IMW(IJK)
      IF(AMP.LE.1.) GO TO 5009
      AMP=60.*ALOG10(AMP)
      RAD=RLG+AMP
      LAST=IJK-1
      IF(LAST.EQ.0) LAST=84
      IF(IMW(LAST).GT.1) GO TO 5010
      PHI=DEFIX*LAST
      CSSP=COS(PHI)
      SSSP=SIN(PHI)
      X0=-RLG*CSSP
      Y0=RLG*SSSP
      CALL MOVEA(X0,Y0)
 5010 CONTINUE
      X0=-RAD*CSSP
      Y0=RAD*SSSP
      CALL DRAWA(X0,Y0)
      PHI=PHI+DEFIX
      CSSP=COS(PHI)
      SSSP=SIN(PHI)
      X0=-RAD*CSSP
      Y0=RAD*SSSP
      CALL DRAWA(X0,Y0)
      NEXT=IJK+1
      IF(NEXT.EQ.85) NEXT=1
      IF(IMW(NEXT).GT.1) GO TO 5009
      X0=-RLG*CSSP
      Y0=RLG*SSSP
      CALL DRAWA(X0,Y0)
 5009 CONTINUE
      GO TO 5003
 5008 CONTINUE
      CALL MOVEA((ZLGMI    -BLZ), RLG)
      CALL DRAWA((ZLGMI+33.*BLZ), RLG)
      CALL MOVEA((ZLGMI    -BLZ),-RLG)
      CALL DRAWA((ZLGMI+33.*BLZ),-RLG)
      DO 5011 IKJ=1,2
      IOFF=32*(IKJ-1)
      ISIGN=3-2*IKJ
      X0=ZLGMI
      Y0=ISIGN*RLG
      CALL MOVEA(X0,Y0)
      DO 5012 IJK=1,32
      AMP=IMW(IJK+IOFF)
      IF(AMP.LE.1.) GO TO 5012
      AMP=60.*ALOG10(AMP)
      IF(IJK.EQ.1) GO TO 5013
      LAST=IJK-1
      IF(IMW(LAST+IOFF).GT.1) GO TO 5013
      X0=ZLGMI+BLZ*LAST
      CALL MOVEA(X0,Y0)
 5013 CONTINUE
      Y0=ISIGN*(RLG+AMP)
      CALL DRAWA(X0,Y0)
      X0=X0+BLZ
      CALL DRAWA(X0,Y0)
      NEXT=IJK+1
      IF(NEXT.EQ.33) NEXT=84-IOFF
      IF(IMW(NEXT+IOFF).GT.1) GO TO 5012
      Y0=ISIGN*RLG
      CALL DRAWA(X0,Y0)
 5012 CONTINUE
 5011 CONTINUE
C
C        DRAW IN END CAPS. ENERGIES COMPUTED IN THE CAPTION ROUTINE
C        AND TUCKED AWAY INTO THE LAST 24 LOCATIONS IN THE SCRATCH
C        ARRAY "HMW" IN COMMON "CWORK1".
C
      DO 5017 IKJ=1,2
      X0=ZENDMI
      IF(IKJ.EQ.2) X0=ZENDPL
      Y0=-6*BLXY
      CALL MOVEA(X0,Y0)
      DO 5018 KJI=1,12
      IRETRV=84+12*(IKJ-1)+KJI
      AMP=IMW(IRETRV)
      IF(AMP.GT.1.) AMP=60.*ALOG10(AMP)
      IF(AMP.LE.1.) AMP=0.
      X=X0+(2*IKJ-3)*AMP
      Y=Y0+(KJI-1)*BLXY
      CALL DRAWA(X,Y)
      Y=Y+BLXY
      CALL DRAWA(X,Y)
 5018 CONTINUE
      X=X0
      CALL DRAWA(X,Y)
 5017 CONTINUE
 5003 CONTINUE
C
      IF( JNDEX .GT. 7 ) GO TO 3
      IF( JNDEX .GT. 3 ) GO TO 5019
C
C
C                            ***************
C                            *  RFI-VIEWS  *
C                            ***************
C
 7050 IF( .NOT. DSPDTM(13) ) GO TO 4
C
C                                      VERTEX CHAMBER
C
      IF( HEAD(18) .LE. 100 ) GO TO 7070
C                            BRANCH FOR REAL DATA
C                                      TEST IF RAW DATA BANK  B P C H
C                                      IS EXISTING
         IPBPCH = IDATA(IBLN('BPCH'))
         IF( IPBPCH .LE. 0 )  GO TO 4
C
            IF( .NOT. FLVCDO(4) ) GO TO 7070
C
C                                      FORCED DISPLAY OF  B P C H
               ILBPCH = IDATA(IPBPCH)
C
               IF( ILBPCH .LE. 1 ) GO TO 4
                  CALL DRVCRD( JNDEX, IPBPCH, ILBPCH )
                  GO TO 4
C
C                            BRANCH FOR REAL DATA AND MC-DATA
C
 7070 CALL DISPVC(INDEX,1)
C
C                                      INNER DETECTOR
C
  4   IF( INDEX .EQ. 20 ) INNERC = .TRUE.
C
      IF( .NOT. INNERC ) GO TO 5
      KZAMP = 0
      IF(NNJETC.LE.0) GO TO 2701
      CALL CLOC(IPJ,'JETC',NNJETC)
      IF(IPJ.GT.0) GO TO 2702
      WRITE(6,2703) NNJETC
2703  FORMAT(' JETC BANK NR',I3,' (GIVEN BY COMMAND JETC) NOT EXISTING')
2701  IPJ = IDATA(IBLN('JETC'))
2702  IF(IPJ.LE.0) GO TO 5
      NWO = (HDATA(2*IPJ+99)-HDATA(2*IPJ+3)+100)/2
C--  LOOP OVER NR OF WORDS IN EVENT
      IF(NWO.LE.2) GO TO 5
      IPJ = 2*IPJ
      NI = IPJ + 101
   91 CONTINUE
      CALL FICOOR(0,NI,NWE)
      IF(NWE.LT.-500) GO TO 5
      IF(NWE.EQ.0) GO TO 9111
C     IF(DSPDTL(26).AND.TBIT(NWE,31)) GO TO 9111
      CALL HITMRK( JNDEX, -X1, Y1, CRLEN, 0 )
      CALL HITMRK( JNDEX, -X2, Y2, CRLEN, 0 )
C--
9111  IF(NI.LE.(IPJ+2*NWO)) GO TO 91
C
      IF( INDEX .EQ. 20 .AND. LVTXC ) RETURN
      IF( INDEX .EQ. 20 .AND. .NOT. LVTXC ) GO TO 51
C
C     IF(KZAMP.GT.0.AND..NOT.FL18)
C    $ CALL DIAGIN('Z-AMP SUM=0, TIMES: ',1,KZAMP,NI,NI,NI,NI,NI)
C
C                                      Z CHAMBER
C
  5   IF( .NOT. DSPDTM(11) ) GO TO 51
      IPZETC = IDATA(IBLN('ZETC'))
      IF( IPZETC .LE. 0 )    GO TO 51
      LENZC  = IDATA(IPZETC)
      IF( LENZC  .LT. 3 )    GO TO 51
      IZHITS = (LENZC-1)/2
      NWO    = IPZETC + LENZC -2
      IPZETC = IPZETC + 1
52    IF( IPZETC .GT. NWO )  GO TO 51
C
      NWIR   = HDATA(2*IPZETC + 1)/8
      CALL RZHITS(IPZETC,XZ,YZ,XZ1,YZ1,XZ2,YZ2,ZZ1,ZZ2,IERR)
      CALL RZDRAW(NWIR,XZ,YZ,XZ1,YZ1,XZ2,YZ2,ZZ1,ZZ2,1)
C
      IPZETC = IPZETC + 2
      GO TO 52
C
C                                      TOF AND BP HODOSCOPE DISPLAY
C
  51  CALL HODRFI
C
C                                      LEAD GLASS DISPLAY (NUMBERS)
C
      IF( PHNUMS ) CALL PBGRFI( SH1, SH2, 4.0 * SH2, DEFIX )
C
C                                      MUON FILTER
C
      IF( MUONFL ) CALL MUHDSP
      IF( MUONFL  .AND.  DSPDTL(18) ) CALL TR3DSP
      RETURN
C
C
C                            ************************
C                            *  ZX-VIEWS, ZY-VIEWS  *
C                            ************************
C
C                                      VERTEX CHAMBER
C
 5019 IF( .NOT. DSPDTM(13) ) GO TO 715
         CALL DISPVC(INDEX,2)
C
C                                      INNER DETECTOR
C
 715  KZAMP = 0
      IF( .NOT. INNERC ) GO TO 11
      IF( FL18  .AND.  .NOT. DSPDTL(20) ) GO TO 11
      IF(NNJETC.LE.0) GO TO 2801
      CALL CLOC(IPJ,'JETC',NNJETC)
      IF(IPJ.GT.0) GO TO 2802
      WRITE(6,2703) NNJETC
2801  IPJ = IDATA(IBLN('JETC'))
2802  IF(IPJ.LE.0) GO TO 11
      NWO = (HDATA(2*IPJ+99)-HDATA(2*IPJ+3)+100)/2
      IF(NWO.LE.2) GO TO 11
      IPJH = 2*IPJ
      NI = IPJH + 101
   93 CONTINUE
C     LOOP OVER WIRES
      CALL FICOOR(1,NI,NWE)
      IF(NWE.LT.-500) GO TO 11
      IF(NWE.EQ.0) GO TO 9333
      X0 = ZET
      IF((.NOT.DSPDTL(9)).OR.DSPDTL(10)) GO TO 4001
      Y0 = R
      CALL HITMRK( JNDEX, ZET, Y0, 4.0, 0 )
      GO TO 4002
 4001 CONTINUE
      DO 4003 LRHIT=1,2
      IF(LRHIT.EQ.2) GO TO 4004
      RLRHI=R1
      FILRH=FI1
      GO TO 4005
 4004 CONTINUE
      RLRHI=R2
      FILRH=FI2
 4005 CONTINUE
      Y0 = RLRHI
C      GET PROJECTED Y-COORDINATE
       IF(INDEX.LE.7) YPRO =  RLRHI*COS(FILRH)
       IF(INDEX.GT.7) YPRO =  RLRHI*SIN(FILRH)
      IF(.NOT.DSPDTL(9)) Y0 = YPRO
      CALL HITMRK( JNDEX, ZET, Y0, 4.0, 0 )
 4003 CONTINUE
 4002 CONTINUE
 9333 IF(NI.LE.(IPJH + 2*NWO)) GO TO 93
C     IF(KZAMP.GT.0.AND..NOT.FL18)
C    $ CALL DIAGIN('Z-AMP SUM=0, TIMES: ',1,KZAMP,NI,NI,NI,NI,NI)
C
C                                      Z CHAMBER
C
  11  IF( .NOT. DSPDTM(11) ) GO TO 81
      IPZETC = IDATA(IBLN('ZETC'))
      IF(IPZETC.LE.0) GO TO 81
      LENZC = IDATA(IPZETC)
      IF(LENZC.LT.3) GO TO 81
      IZHITS = (LENZC-1)/2
      NWO = IPZETC + LENZC -2
      IPZETC = IPZETC + 1
82    IF(IPZETC.GT.NWO) GO TO 81
C
      NWIR   = HDATA(2*IPZETC + 1)/8
      CALL RZHITS(IPZETC,XZ,YZ,XZ1,YZ1,XZ2,YZ2,ZZ1,ZZ2,IERR)
      CALL RZDRAW(NWIR,XZ,YZ,XZ1,YZ1,XZ2,YZ2,ZZ1,ZZ2,2)
C
      IPZETC = IPZETC + 2
      GO TO 82
C
C                                      LEAD GLASS
C
  81  IF( PHNUMS ) CALL PBGRZ( INDEX, SH1, SH2, 4.0 * SH2 )
C
C                                      FORWARD MUON COUNTERS
C
      IF( JNDEX .GT. 5 ) CALL FWMUHT
C
C                                      MUON FILTER
C
      IF( MUONFL ) CALL MUHDSP
      IF( MUONFL  .AND.  DSPDTL(18) ) CALL TR3DSP
C
      IF( JNDEX .NE. 7 ) GO TO 303
C
C                                      DISPLAY FORWARD DETECTOR HITS
C                                      IN ZXD AND ZYD VIEWS
C
C ------- NOT YET CODED ------
C
303   RETURN
C
C                            ***************************
C                            *  FORWARD DETECTOR VIEW *
C                            ***************************
C
  3   IF( JNDEX .EQ. 8 ) CALL FWCHEV
C
      IF(JNDEX.LT.9) RETURN
C
      IF(JNDEX.GT.9) GO TO 310
C
C                            *******************************
C                            *  ROLLED OUT LEADGLASS VIEW  *
C                            *******************************
C
      IPJ = IDATA(IBLN('ALGN'))
      IF(IPJ.LE.0) GO TO 300
      SH3 = 0.6667*BLZ
      NWO = IDATA(IPJ)
      IF(NWO.LE.3) GO TO 300
      IPJ = 2*IPJ + 8
      NWO = IPJ + 2*NWO - 8
C
      DO 12  NI = IPJ,NWO,2
      NO = HDATA(NI-1)
      IF(NO.GT.2687) SH3 = .6667*BLXY
      CALL XYBLK(NO,X1,Y1,DXEB,DYEB)
      X1=X1+10.
      Y1=Y1+10.
      IE = HDATA(NI)
      CALL PHNUMB(IE,SH3,0.)
   12 CONTINUE
C
C                                      FORWARD TAGGING APPARATUS
C
  300 CALL FWCHEV
C
  310 IF(JNDEX.EQ.9) RETURN
C
      IF(JNDEX.NE.10) GO TO 4100
C
C                           **************************************
C                           *  PERSPECTIVE VIEW OF PBG CYL + ID  *
C                           **************************************
C
      IF(NNJETC.LE.0) GO TO 2901
      CALL CLOC(IPJ,'JETC',NNJETC)
      IF(IPJ.GT.0) GO TO 2902
      WRITE(6,2703) NNJETC
2901  IPJ = IDATA(IBLN('JETC'))
2902  IF(IPJ.LE.0) GO TO 8999
      KZAMP = 0
      IF( .NOT. DSPDTL(21) ) GO TO 8999
      NWO = (HDATA(2*IPJ+99)-HDATA(2*IPJ+3)+100)/2
C     REGISTER WIRE HITS
C--  LOOP OVER NR OF WORDS IN EVENT
      IF(NWO.LE.2) RETURN
      IPJH = 2*IPJ
      NI = IPJH + 101
   96 CONTINUE
      CALL FICOOR(2,NI,NWE)
      IF(NWE.LT.-500) GO TO 8999
      IF(NWE.EQ.0) GO TO 9666
C     IF(DSPDTL(26).AND.TBIT(NWE,31)) GO TO 9666
      CALL HITMRK( JNDEX, -X1, Y1, 4.0, 0 )
      CALL HITMRK( JNDEX, -X2, Y2, 4.0, 0 )
9666  IF(NI.LE.(IPJH + 2*NWO)) GO TO 96
C     IF(KZAMP.GT.0.AND..NOT.FL18)
C    $ CALL DIAGIN('Z-AMP SUM=0, TIMES: ',1,KZAMP,NI,NI,NI,NI,NI)
8999  CALL PBGCYL(DEFIX)
      RETURN
C
4100  CONTINUE
      IF(JNDEX.NE.11) GO TO 4201
C
C                            *******************************
C                            *  FORWARD MUON COUNTER VIEW  *
C                            *******************************
C
      IF(HEAD(18).GT.3730) CALL FWMUHT
      IF(HEAD(18).LT.3730.AND.HEAD(18).GT.100) WRITE(6,4101)
4101  FORMAT(' FORWARD MUON COUNTERS NOT YET INSTALLED FOR THIS RUN.')
C
4201  IF(JNDEX.NE.12) RETURN
C
C                            *******************************
C                            *  ROLLED OUT Z CHAMBER VIEW  *
C                            *******************************
C
      IPZETC = IDATA(IBLN('ZETC'))
      IF( IPZETC .LE. 0 )   GO TO 939
      LENZC  = IDATA(IPZETC)
      IF( LENZC  .LT. 3 )   GO TO 939
      IZHITS = (LENZC-1)/2
      NWO    = IPZETC + LENZC -2
      IPZETC = IPZETC + 1
92    IF( IPZETC .GT. NWO ) GO TO 939
C
      CALL RZHITS(IPZETC,XZ,YZ,XZ1,YZ1,XZ2,YZ2,ZZ1,ZZ2,IERR)
      IF( IERR .EQ. 0 ) CALL RZDRAW(XZ,YZ,XZ1,YZ1,XZ2,YZ2,ZZ1,ZZ2,3)
C
      IPZETC = IPZETC + 2
      GO TO 92
C
939   CONTINUE
C
      RETURN
      END
