C   13/02/80 711201806  MEMBER NAME  DEDXDS   (S)           FORTRAN
      SUBROUTINE DEDXDS
C--
C--     DEDX DISPLAY FOR JADE, USING DEDX PROGRAM FROM R.EICHLER
C--     13.02.80       LAST CHANGE 14.06.83          J.OLSSON
C--                    LAST CHANGE 17.05.85          J.A.J.SKARD
C--                    LAST CHANGE  9.06.86          J.OLSSON
C--                    LAST CHANGE 20.08.86          J.OLSSON
C--                    LAST CHANGE 13.11.87          E ELSEN
C---
C---   OPTIONS IN COMMAND DEDX
C---     DEDX I, I>0   SINGLE LINE PRINT FOR TRACK I
C---     DEDX 0        SINGLE LINE PRINT FOR EACH TRACK
C---     DEDX -1       CREATE BANK DEDX
C---     DEDX -2       LARGE DEDX VS. MOMENTUM DISPLAY
C---     DEDX -3       NOT USED
C---     DEDX -4       SMALL DEDX VS. MOMENTUM DISPLAY
C---     DEDX -5       LARGE DEDX VS. MOMENTUM DISPLAY, SCALE EXTRALARGE
C---     DEDX -6       CHANGE VARIABLE NHFCUT: MIN. NR OF HITS ON TRACK.
C---                     ALSO VARIABLE PMCUT: MIN. MOMENTUM OF TRACK.
C---
      IMPLICIT INTEGER*2 (H)
      character*2 HTYP ,HBLANK, HMW
      LOGICAL FL18,FL22,FL24
      COMMON/CPROJ/XMINR,XMAXR,YMINR,YMAXR,IPRJC,FL18,FL22,FL24
      COMMON/CWORK1/IER,NTR,TRES(10,60)
      DIMENSION ITRES(10,60),HTYP(3,8),HMW(4)
      EQUIVALENCE (TRES(1,1),ITRES(1,1))
      DIMENSION SAV(6)
#include "cgeo1.for"
#include "cgraph.for"
#include "cdata.for"

      COMMON / CDEDXU / NHFCUT,PMCUT
      COMMON / CGRAP2 / BCMD,DSPDTM(30),ISTVW,JTVW
      LOGICAL DSPDTM
C     DATA HTYP /'  ','PR','OT','ON','  ','K ','PL','US',
C    $           'PI',' P','LU','S ','PO','SI','TR','ON',
C    $           'A-','PR','OT','ON',' K',' M','IN','US',
C    $           'PI',' M','IN','US','EL','EC','TR','ON'/
      DATA HTYP /'PR','OT','ON','  ',' K',' +',
     $           '  ','PI',' +','  ',' E',' +',
     $           'AN','TI','-P','  ',' K',' -',
     $           '  ','PI',' -','  ',' E',' -'/
      DATA HBLANK /'  '/
C                                           FLAG TO REQUEST DATA FOR
C                                           SINGLE TRACK
      COMMON /CDXSIN/ ISNGTR
C----------------------------------------
      NN = ACMD
C---   CALL DEDX PROGRAM HERE
      IF( NN.GT.0 ) ISNGTR = NN
      NPDEDX = IDATA(IBLN('DEDX'))
C                                           PERFORM DEDX CALC IF
C                                           REQUESTED (NN=-1)
C                                           BANK NOT YET THERE
C                                           OR SINGLE TRACK
      IF( .NOT. ( NN.GT.0 .OR. NN.EQ.-1 .OR. NPDEDX.LE.0 ) )
     *                            GO TO 2213
        CALL DEDXBN
C                                           DO NOT RECREATE BANK
C                                           FOR SINGLE TRACK MODE
        IF( .NOT. ( NN.EQ.-1 .OR. NPDEDX.LE.0 ) ) GO TO 2213
          IPPATR = IDATA(IBLN('PATR'))
          IF(IPPATR.GT.0) GO TO 98
            WRITE(6,97)
97          FORMAT(' ERROR ----->>   PATR BANK DOES NOT EXIST')
            RETURN
98        CALL BDLS( 'DEDX', IDATA(IPPATR-2) )
          CALL DEDXBK(IPPATR)
2213  CONTINUE
C                                           REINSTALL DEDX BANK
      NPDEDX = IDATA(IBLN('DEDX'))
      IF( NPDEDX.GT.0 ) CALL UCOPY(IDATA(NPDEDX+1),IER,IDATA(NPDEDX) )
C
      IF(NN.GE.0) GO TO 99
      IF(NN.EQ.-1) GO TO 91
      IF(NN.EQ.-6) GO TO 121
C SHOW DIAGRAM DEDX VS MOMENTUM
      CALL DEDXVW
      RETURN
91    CONTINUE
      RETURN
C---       CHANGE NHFCUT AND PMCUT    OPTION DEDX -6
121   CONTINUE
      WRITE(6,1211) NHFCUT,PMCUT
1211  FORMAT(' PRESENT VALUES OF NHFCUT AND PMCUT ARE:',I3,F6.3,
     $  ',  ENTER NEW VALUES:')
      CALL TONUM(INT1,FLP1)
      NHFCUT = INT1
      IF(NHFCUT.LT.5.OR.NHFCUT.GT.45) NHFCUT = 15
      PMCUT = FLP1
      IF(PMCUT.LT..02) PMCUT = 0.05
      WRITE(6,995) NHFCUT,PMCUT
995   FORMAT(' NEW VALUES FOR NHFCUT AND PMCUT ARE:',I4,F7.3)
      CALL BMLT( 1, 'DEDX' )
      CALL BDLM
      RETURN
C---
99    CONTINUE
C--   LOOP OVER TRACKS
      IPO = IDATA(IBLN('PATR'))
      LO = IDATA(IPO+1)
C     NTR = IDATA(IPO+2)
      LTR = IDATA(IPO+3)
      IPO = IPO + LO - LTR
      NTRR = MIN0(NTR,60)
      DO 111  ITR = 1,NTRR
      IPO = IPO + LTR
      IF(NN.GT.0.AND.NN.NE.ITR) GO TO 111
      RAD = ADATA(IPO+25)
      HSIGN = 1
      IF(RAD*BKGAUS.LT.0.) HSIGN = -1
      JMIN = ITRES(8,ITR)
      DO 112  I = 1,3
      IF(JMIN.LT.1.OR.JMIN.GT.4) HMW(I) = HBLANK
      IF(JMIN.LT.1.OR.JMIN.GT.4) GO TO 112
      K = JMIN
      IF(HSIGN.LT.0) K = K + 4
      HMW(I) = HTYP(I,K)
112   CONTINUE
      NHT = ITRES(1,ITR)
      DEDX = TRES(2,ITR)
      SIDEDX = TRES(3,ITR)
      SAV(1)=TRES(4,ITR)
      SAV(2)=TRES(5,ITR)
      SAV(3)=TRES(6,ITR)
      SAV(4)=TRES(7,ITR)
      SAV(5)=TRES(9,ITR)
      SAV(6)=TRES(10,ITR)
C
C  IF SINGLE TRACK REQUIRED, DISPLAY DEDX VS RADIUS HISTOGRAM
C
      IF(DSPDTM(19).AND.NN.GT.0.AND.ITRES(1,NN).GT.0)   CALL DEDXV1
C
C---  WRITE DEDX INFORMATION FOR TRACK ITR
C     WRITE(6,1379) ITR,(HMW(I),I=1,3),DEDX,SIDEDX,(TRES(I,ITR),I=4,7)
C    ,, TRES(9,ITR), TRES(10,ITR)
      WRITE(6,1379) ITR,(HMW(I),I=1,3),DEDX,SIDEDX,(SAV(I),I=1,6)
1379  FORMAT(I2,1X,3A2,' DEDX=',F5.2,'+-',F5.2,' CHI(E P K P) ',
     $ 4F6.2,' P',F6.3,'+-',F5.3)
111   CONTINUE
      RETURN
      END
