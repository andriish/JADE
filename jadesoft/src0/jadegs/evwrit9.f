C   19/02/84 808112202  MEMBER NAME  EVWRIT9  (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE EVWRIT(NUNIT)
C-----------------------------------------------------------------------
C
C   AUTHOR:   L. O'NEILL  30/08/79 :  WRITE JADE EVENTS
C
C        MOD: J. OLSSON   10/09/79 :  CORRECTED
C        MOD: L. O'NEILL  14/11/79 :  USE MUCONW
C        MOD: J. OLSSON   12/11/81 :  NEW MUCONW
C        MOD: J. OLSSON    4/08/83 :  CHANGES REGARDING MC CONSTANTS
C        MOD: C. BOWDERY  19/02/84 :  CHECK HEAD BANK EXISTS
C        MOD: C. BOWDERY  20/02/84 :  WRITE CONSTANTS WHEN NFLAGS(8)=1
C        MOD: J. HAGEMANN 08/11/84 :  ADD VERTEX CHAMBER CONSTANTS
C        MOD: E ELSEN     10/12/87 :  ADD DLRSLN
C        MOD: J. HAGEMANN 11/12/87 :  ADD SMPRMV TO /CBINV/
C   LAST MOD: E ELSEN     29/03/88 :  ADD DLZSLN
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL LVTXC
C
C                            NFLAGS(7) IS USED TO COUNT HEADLESS EVENTS
C                            NFLAGS(8) = 1 MEANS WRITE NEW CONSTANTS
C
#include "cadmin.for"
#include "cdata.for"
#include "cgraph.for"
C
      COMMON / CGEO1  / GEO(66)
      COMMON / CJDRCH / DRCH(34)
C
C                                          VERTEX CHAMBER
      COMMON / CVCEX  / LVTXC
      COMMON / CGEOV  / GEOV(13)
      COMMON / CJVTXC / VXCC(15)
      COMMON / CBINV /  DTRSVV, ZRSVV, EFFVV(2), DOUBVV(3), IRNHVV,
     +                  SMPRMV(3)
C
      COMMON / CRDSTA / NDEAD, NCDEAD, HITD(10), HCELLD(10), IPHALG
      COMMON / CBIN   / TIME(6),ZOF,ZRS,ZL,ZSC,EPSI(3),DOUB(3),IRN(3),
     +                  BINDL8(6),RJITT, DLRSLN(3), DLZSLN(3)
C
      COMMON / CTRIGG / IHIST(3),NBPTFW,IDUM1(5),HDUM1,HLGBRT,HLGBST,
     +                  HLGQT,HLGTOT(4,2),HECAPT(4),HLGTL,HLGTH,
     +                  HDUM2(10),IWIDBS,NRNBSL,NRNBSH,NTOFBS,IDUM2(10),
     +                  NTOFC,NTOFC1,NTOFC2,NTBGC,NTBGC1,NTBGC2,
     +                  IWCOLL,IWCOLN,IWMPRG,HFMPRL,HFMPRH,HWCOLN,
     +                  HMPRON,IWCTBG,NTFCOL,NTFCLN,IDUM3(10),
     +                  HITCLL(3),HITWLL(3),HITSUM(3),HCHAMB(3),
     +                  HMASK(16,3),HDEADC(10),HACC1,HACC2,HACC3,
     +                  HACC4,HACCM,HDUM6,IWIDT2,IDUM4(10)
C
      DIMENSION NAM1(3), NAMV(2)
C
C
      DIMENSION TSTAT1(110), TSTAT2(13), ABIN(20)
      EQUIVALENCE (TSTAT1(1),IHIST(1)), (TSTAT2(1), NDEAD)
      EQUIVALENCE ( ABIN(1), TIME(1) )
C
      DATA NAM1  / 'MTCO', 'MJET', 'MGEO' /
      DATA NAMV  / 'MVCC', 'MGEV' /
      DATA LMTCO,LGEO,LJET / 159, 66, 34 /
      DATA LVCC, LGEV  / 15, 13 /
      DATA IENTW /0/
C
C------------------  C O D E  ------------------------------------------
C
      IENTW = IENTW + 1
      IF(IENTW.EQ.1) WRITE(6,692) NFLAGS(9)
692   FORMAT(' EVWRIT9: NFLAGS(9) ON FIRST ENTRY ',I4)
C
      IF( JUSCRN .LE. 0  .OR. JUSCRN .GT. 99 ) JUSCRN = 6
C
      IF( NFLAGS(9) .EQ. 0  .OR. IEVTP .EQ. 0 ) GO TO 10
C                                                    *****************
C                                                    *  MONTE CARLO  *
C                                                    *****************
C                                   ***************
C                                   *  BANK MTCO  *
C                                   ***************
C
C                  WRITE OUT CONSTANTS RECORDS FIRST ON FILE
C
      CALL CLOC( IPMTCO, NAM1(1), 1 )
      IF( IPMTCO .GT. 0 .AND. IDATA(IPMTCO).GE.LMTCO ) GO TO 1
C
      IF( IPMTCO .LE. 0 )
     *              CALL BCRE( IPMTCO, NAM1(1), 1,LMTCO, &1000, IER )
      IF( IDATA(IPMTCO).LT.LMTCO )
     *              CALL BCHM( IPMTCO, LMTCO - IDATA(IPMTCO), IER )
C
C                  COPY DETECTOR STATUS INTO MTCO BANK
C
    1 CALL MVCL( IDATA, (IPMTCO+10)*4, TSTAT1, 0, 110*4 )
      CALL MVCL( IDATA, (IPMTCO+120)*4, TSTAT2, 0, 13*4 )
C
C                  COPY DOUB, EPSI AND IRN INTO MTCO
C
      DO  1001  I = 1,3
          ADATA(IPMTCO+133+I) = DOUB(I)
          ADATA(IPMTCO+136+I) = EPSI(I)
          IDATA(IPMTCO+139+I) = IRN(I)
 1001 CONTINUE
      IF( .NOT. LVTXC )  GO TO 1003
         ADATA(IPMTCO+143) = DTRSVV
         ADATA(IPMTCO+144) = ZRSVV
         ADATA(IPMTCO+145) = EFFVV(1)
         ADATA(IPMTCO+146) = EFFVV(2)
         ADATA(IPMTCO+147) = DOUBVV(1)
         ADATA(IPMTCO+148) = DOUBVV(2)
         ADATA(IPMTCO+149) = DOUBVV(3)
         IDATA(IPMTCO+150) = IRNHVV
         ADATA(IPMTCO+154) = SMPRMV(1)
         ADATA(IPMTCO+155) = SMPRMV(2)
         ADATA(IPMTCO+156) = SMPRMV(3)
C
C  WORD2 :   DETECTOR STATUS AND SOME CBIN VARIABLES ARE STORED
C
 1003 IDATA(IPMTCO+2) = 1
C
C  WORD3 :   BINDL8 AND RJITT SMEARING VARIABLES ARE STORED, IF ISMEAR=100012500
C
      IDATA(IPMTCO+3) = ISMEAR
      IF(ISMEAR.EQ.0) GO TO 1111
C
C                  COPY BINDL8 AND RJITT INTO MTCO
C
      DO 1011 I=1,6
 1011 ADATA(IPMTCO+3+I) = BINDL8(I)
      ADATA(IPMTCO+10) = RJITT
      ADATA(IPMTCO+151) = DLRSLN(1)
      ADATA(IPMTCO+152) = DLRSLN(2)
      ADATA(IPMTCO+153) = DLRSLN(3)
      ADATA(IPMTCO+157) = DLZSLN(1)
      ADATA(IPMTCO+158) = DLZSLN(2)
      ADATA(IPMTCO+159) = DLZSLN(3)
 1111 CONTINUE
C
C  WORD1 :   THIS IS SMEARED MONTE CARLO DATA
C
      IDATA(IPMTCO+1) = 1
C                                   ***************
C                                   *  BANK MJET  *
C                                   ***************
C
C                  JET CHAMBER CONST
C
      CALL CLOC( IND, NAM1(2), 2 )
      IF( IND .GT. 0 ) GO TO 2
C
      CALL BCRE( IND, NAM1(2), 2, LJET, &1000, IER )
    2 CALL BSTR( IND, DRCH, LJET )
C
C                                   ***************
C                                   *  BANK MGEO  *
C                                   ***************
C
C                  GEOMETRICAL CONST
C
C
      CALL CLOC( IND, NAM1(3), 3 )
      IF( IND .GT. 0 ) GO TO 3
C
      CALL BCRE( IND, NAM1(3), 3, LGEO, &1000, IER )
    3 CALL BSTR( IND, GEO, LGEO )
C
      IF( .NOT. LVTXC ) GO TO 6
C
C                                   ***************
C                                   *  BANK MVCC  *
C                                   ***************
C
C                  CONSTANTS FOR VERTEX CHAMBER
C
C
      CALL CLOC( IND, NAMV(1), 4 )
      IF( IND .GT. 0 ) GO TO 4
C
      CALL BCRE( IND, NAMV(1), 4, LVCC, &1000, IER )
    4 CALL BSTR( IND, VXCC, LVCC )
C
C                                   ***************
C                                   *  BANK MGEV  *
C                                   ***************
C
C                  GEOMETRICAL CONSTANTS OF VERTEX CHAMBER
C
C
      CALL CLOC( IND, NAMV(2), 5 )
      IF( IND .GT. 0 ) GO TO 5
C
      CALL BCRE( IND, NAMV(2), 5, LGEV, &1000, IER )
    5 CALL BSTR( IND, GEOV, LGEV )
C
C                  WRITE OUT
C
    6 CALL BSLS(1)
      CALL BSLC
      CALL BSAW( 3, NAM1 )
      IF( LVTXC ) CALL BSAW( 2, NAMV)
      CALL BSLW
      CALL BWRITE( NUNIT )
      CALL BSLT
      CALL BDLG
C
C
C                  MU CHAMBER CONSTANTS
C
      CALL MUCONW(1,NUNIT,0,HERR)
      CALL BSLR(1)
C
C                            CLEAR FLAG TO INDICATE CONSTANTS WRITTEN
C
      NFLAGS(9) = 0
C
C                                                    *****************
C                                                    * REAL DATA AND *
C                                                    *  MONTE CARLO  *
C                                                    *    EVENTS     *
C                                                    *****************
10    NRWRIT = NRWRIT + 1
      IPHEAD = IDATA(IBLN('HEAD'))
      IF( IPHEAD .GT. 0 ) GO TO 12
        NFLAGS(7) = NFLAGS(7) + 1
        IF( NFLAGS(7) .GT. 50 ) GO TO 12
          WRITE(JUSCRN,11) NRWRIT
11        FORMAT(/' ****  WARNING  ****  EVWRIT DETECTED ''HEAD'' BANK',
     +            ' MISSING WHILE WRITING OUTPUT EVENT ',I6/)
C
12    CALL BSLW
      CALL BWRITE(NUNIT)
      RETURN
C
C                  ERROR IN RECORD WRITING
C
 1000 WRITE(JUSCRN,9101) IER
 9101 FORMAT(' BCRE ENDED WITH ERROR IER=',I4,' IN EVWRIT')
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE FWRITE(NUNIT,NN,DATA)
C-----------------------------------------------------------------------
C
C      WRITE THE ARRAY DATA(NN) ONTO UNIT NUNIT
C
      DIMENSION DATA(NN)
      WRITE(NUNIT) NN,DATA
      RETURN
      END
