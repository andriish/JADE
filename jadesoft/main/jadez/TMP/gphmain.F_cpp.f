










C   08/06/86 807251549  MEMBER NAME  GPHMAIN  (S)           FORTRAN
C
C----------------------------------------------------------------------
C     MAIN PROGRAM FOR JADE GRAPHICS           JUNE       1986
      PROGRAM GPHMAIN
C----------------------------------------------------------------------
C
C   AUTHOR:   L. O'NEILL  17/05/78 :  MAIN PROGRAM FOR GRAPHICS
C
C        MOD  C. BOWDERY  13/03/84 :  MAJOR OVERHAUL
C        MOD  C. BOWDERY  25/04/84 :  CDTL DEFAULTS CHANGED
C        MOD  C. BOWDERY  14/06/84 :  NEW COMMONS BLOCK DATA SET
C        MOD  C. BOWDERY  12/10/84 :  BLOCK DATA CHANGE
C        MOD  J. HAGEMANN 24/10/84 :  UPDATE PRINTOUT
C        MOD  C. BOWDERY  24/04/85 :  BLOCK DATA COMES FROM MACRO NOW
C        MOD  C. BOWDERY   9/07/85 :  LINK HELP DATASET (DIRECT ACCESS)
C        MOD  C. BOWDERY   3/08/85 :  TIDY UP SCREEN CLEARING
C        MOD  C. BOWDERY   8/06/86 :  CODE PUT INTO SUBROUTINES
C   LAST MOD  J. HAGEMANN 05/03/88 :  FOR OWN BLOCKDATA
C
C         THIS PROGRAM STARTS A GRAPHICS SESSION AND CALLS SUPERV
C   ==>   It contains lower case letters. Check ASIS mode!
C
C----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      REAL*8   DDN,VOLSR
      LOGICAL  OK
C
C                            MACRO CBCSMX.... BOS COMMON + SIZE DEFINED
C
C-----------------------------------------------------------------------
C             MACRO CBCSMX.... BOS COMMON + SIZE DEFINITION
C
C             THIS MACRO DEFINES THE IDATA/HDATA/ADATA NAMES AND
C             FIXES THE ACTUAL SIZE OF /BCS/. IT IS THUS IDEAL FOR
C             MAIN PROGRAMS OR THE SUPERVISOR. LDATA CAN BE USED BY BINT
C
C-----------------------------------------------------------------------
C
      COMMON /BCS/ IDATA(60000)
      DIMENSION HDATA(120000),ADATA(60000)
      EQUIVALENCE (HDATA(1),IDATA(1),ADATA(1))
      DATA  LDATA / 60000 /
C
C------------------------ END OF MACRO CBCSMX --------------------------
C-----------------------------------------------------------------------
C                            MACRO CGRAPH .... GRAPHICS COMMON
C-----------------------------------------------------------------------
C
      LOGICAL DSPDTL,SSTPS,PSTPS,FREEZE
C
      character*2 IDATSV
      COMMON / CGRAPH / JUSCRN,NDDINN,NDDOUT,IDATSV(11),ICREC,MAXREC,
     +                  LSTCMD,ACMD,LASTVW,ISTANV,
     +                  SXIN,SXAX,SYIN,SYAX,XMIN,XMAX,YMIN,YMAX,
     +                  DSPDTL(30),SSTPS(10),PSTPS(10),FREEZE(30),
     +                  IREADM,LABEL,LSTPS(10),IPSVAR
C
C------- END OF MACRO CGRAPH -------------------------------------------
C
C
      COMMON / CGAMOD / MODEGA, GAHEX
C
      DIMENSION IHLP(11)
C
      DATA  VOLSR/'        '/
C
      DATA  IHLP /'F22B','OW.H','ELP.','DATA','SET ','    ',5*'    '/
C
C------------------  C O D E  -----------------------------------------
C
C                            INITIALISE PLOT10
C
      CALL INITT(DUMMY)
      CALL TWINDO(0,4095,0,4095)
C
C                            ARE WE RUNNING IN GA MODE? MODEGA = 1 ?
C                            IF YES, CLEAR THE TSO SCREEN
C
      CALL TESTGA( MODEGA, GAHEX )
C
      IF( MODEGA .EQ. 1 ) CALL CLEAR
C
C                            WELCOME THE USER
C
      CALL WELCME
C
C                            ALLOCATE FORTRAN LUN TO INPUT DATA SET.
C
      CALL RALLOC( OK )
      IF( .NOT. OK ) GO TO 10
C
C                            ALLOCATE THE DIRECT ACCESS HELP DATASET
C
      CALL GETPDD(IHLP,VOLSR,DDN,50,HERR)
      IF( HERR .NE. 0  .AND.  HERR .NE. 1040 )
     +                  CALL TRMOUT(80,'Warning: The HELP dataset could
     +not be allocated.^')
C
C                            ALLOCATE THE CALIBRATION DATASETS
C
      CALL CALLOC ( OK )
      IF( .NOT. OK ) GO TO 10
C
C                            CLEAR SCREEN AND INITIALISE BOS
C
      CALL CLRCON
      CALL BINT(LDATA,LDATA,200,1)
C
C                            SET UP DEFAULT GRAPHICS VALUES
C
      CALL SETUPG
C
C                            CALL MAIN ANALYSIS SUPERVISOR ROUTINE.
C
      CALL SUPERV
C
C                            END SCAN SESSION AND TERMINATE PLOT10.
C                            OK IS INPUT ARGUMENT.
C
  10  CALL ENDMES( OK )
C
C                            CLEAR GRAPHICS SCREEN IF IN GA MODE
C                            THEN TERMINATE PLOT10.
C
      IF( MODEGA .EQ. 1 ) CALL ERASE
      CALL FINITT(0,0)
C
      STOP
      END
C
C                            BLOCK DATA FOR GRAPHICS PROGRAM
C
C #include "grblock.for" PMF 5.11.98
C   24/04/85 807251550  MEMBER NAME  GRBLOCK  (S)           FORTRAN
C
C-----------------------------------------------------------------------
      BLOCK DATA BLCKG2 !PMF 03/12/99: add name
C-----------------------------------------------------------------------
C
C                  THIS MACRO IS EXPANDED IN GPHMAIN
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL  FL18,FL22,FL24,DSPDTM
      LOGICAL  LERROR
      LOGICAL*1  CCMD,CDICTY
C
      COMMON / CPROJ  / XMINR,XMAXR,YMINR,YMAXR,
     +                  IPRJC,FL18,FL22,FL24,KPROJ
      COMMON / CZKON  / ZCVDR,ZCXCH,ZCTZER,ZCAPED,XL1,XL2
      COMMON / CVX    / NNPATR,ICRSTR,NNJETC,NNVTXC
C
      COMMON / CZGEO  / RZCHI,RZCHA,NZRPSI,NZZ,Z1ZCH,Z2ZCH,ZCHA,ZCHB,
     +                  ZCHSS,ZCHDL,ZCHDLL,DLZZ,DLZPHI,DLZW1,DLZW2
      COMMON / CGRAP2 / BCMD,DSPDTM(30),ISTVW,JTVW
      COMMON / CGRAP3 / IDEFWP, CDICTY(700)
C
      COMMON / CGRAP4 / IPOS4,CCMD(8,200),HCMD(5,200)
      COMMON / CGRAP5 / KOMCAL,LERROR
      COMMON / CGRMAC / MACNR, MACSTA, MACDEP, MACPNT(2,10), CDEF(80,31)
      COMMON / CSTANV / HDISPN, HCSTV2, HCSTV3
C
C---------  MACRO CHSYM    SYMBOLS USED IN GRAPHICS WRITING -----
      COMMON /CHSYM/ HSYM(36)
C------ END MACRO CHSYM
C
C-----------------------------------------------------------------------
C                            MACRO CGRSCL .... GRAPHICS VIEW SCALES
C-----------------------------------------------------------------------
C
      COMMON  / CGRSCL / XMINST(30), XMAXST(30), YMINST(30)
C
C------- END OF MACRO CGRSCL -------------------------------------------
C
C
C------------------  D A T A  ------------------------------------------
C
      DATA HSYM /'0 ','1 ','2 ','3 ','4 ','5 ','6 ','7 ','8 ','9 ','A ',
     + 'B ','C ','D ','E ','F ','G ','H ','I ','J ','K ','L ','M ','N ',
     + 'O ','P ','Q ','R ','S ','T ','U ','V ','W ','X ','Y ','Z '/
C
      DATA  FL18,FL22,FL24  / 3*.FALSE. /, KPROJ / 0 /
      DATA  DSPDTM / .TRUE. ,.TRUE. ,.FALSE.,.FALSE.,.FALSE.,
     +               .FALSE.,.FALSE.,.TRUE. ,.FALSE.,.FALSE.,
     +               .TRUE. ,.FALSE.,.TRUE. ,.FALSE.,.TRUE. ,
     +               .FALSE.,.FALSE.,.FALSE.,.TRUE. ,.TRUE. ,
     +               .TRUE. ,.FALSE.,.FALSE.,.FALSE.,.TRUE. ,
     +               .TRUE. ,.FALSE.,.FALSE.,.FALSE.,.FALSE. /
      DATA  ISTVW  / 0 /, JTVW   / 0 /
      DATA  NNPATR / 0 /, NNJETC / 0 /, NNVTXC / 9 /
C
      DATA  MACNR, MACSTA, MACDEP / 0, 169, 1 /
C
      DATA  RZCHI,RZCHA /864.5, 884.5/
      DATA  NZRPSI,NZZ /24,16/
      DATA  Z1ZCH,Z2ZCH /-1200.,1200./
      DATA  ZCHA,ZCHB,ZCHSS /5.,15.,1./
      DATA  ZCHDL,ZCHDLL /100.,20./, DLZZ,DLZPHI /150.,227.63/
      DATA  DLZW1,DLZW2 /228.94,231.58/
C
      DATA  ZCVDR/0.34/, ZCXCH/1.0/, ZCTZER/0.0/, ZCAPED/0.0/
      DATA  XL1,XL2 /2547.28, 2578.96/
      DATA  IDEFWP, IPOS4 / 0, 0 /
      DATA  KOMCAL / 0 /
C
      DATA  HDISPN/ 1 /, HCSTV2/ 1 /, HCSTV3/ -1 /
C
C                            VIEW RA
C
      DATA  XMAXST(1)  /  1400.0 /
      DATA  XMINST(1)  / -1400.0 /
      DATA  YMINST(1)  / -1100.0 /
C
C                            VIEW RB
C
      DATA  XMAXST(2)  /  1950.0 /
      DATA  XMINST(2)  / -2050.0 /
      DATA  YMINST(2)  / -1550.0 /
C
C                            VIEW RC
C
      DATA  XMAXST(3)  /  4900.0 /
      DATA  XMINST(3)  / -4900.0 /
      DATA  YMINST(3)  / -3350.0 /
C
C                            VIEW ZXA
C
      DATA  XMAXST(4)  /  1500.0 /
      DATA  XMINST(4)  / -1800.0 /
      DATA  YMINST(4)  / -1300.0 /
C
C                            VIEW ZXB
C
      DATA  XMAXST(5)  /  2000.0 /
      DATA  XMINST(5)  / -2400.0 /
      DATA  YMINST(5)  / -1700.0 /
C
C                            VIEW ZXC
C
      DATA  XMAXST(6)  /  4400.0 /
      DATA  XMINST(6)  / -5600.0 /
      DATA  YMINST(6)  / -3900.0 /
C
C                            VIEW ZXD
C
      DATA  XMAXST(7)  /  5500.0 /
      DATA  XMINST(7)  / -5500.0 /
      DATA  YMINST(7)  / -3900.0 /
C
C                            VIEW ZYA
C
      DATA  XMAXST(8)  /  1500.0 /
      DATA  XMINST(8)  / -1800.0 /
      DATA  YMINST(8)  / -1300.0 /
C
C                            VIEW ZYB
C
      DATA  XMAXST(9)  /  2000.0 /
      DATA  XMINST(9)  / -2400.0 /
      DATA  YMINST(9)  / -1700.0 /
C
C                            VIEW ZYC
C
      DATA  XMAXST(10) /  4400.0 /
      DATA  XMINST(10) / -5600.0 /
      DATA  YMINST(10) / -3900.0 /
C
C                            VIEW ZYD
C
      DATA  XMAXST(11) /  5500.0 /
      DATA  XMINST(11) / -5500.0 /
      DATA  YMINST(11) / -3900.0 /
C
C                            VIEW FW
C
      DATA  XMAXST(12) /  1200.0 /
      DATA  XMINST(12) / -1200.0 /
      DATA  YMINST(12) / - 800.0 /
C
C                            VIEW RU
C
      DATA  XMAXST(13) /  7890.0 /
      DATA  XMINST(13) / -1610.0 /
      DATA  YMINST(13) / -  25.0 /
C
C                            VIEW CYL
C
      DATA  XMAXST(14) /  1800.0 /
      DATA  XMINST(14) / -1800.0 /
      DATA  YMINST(14) / -1300.0 /
C
C                            VIEW FWMU
C
      DATA  XMAXST(15) /  7890.0 /
      DATA  XMINST(15) / -1610.0 /
      DATA  YMINST(15) / -  25.0 /
C
C                            VIEW Z-CHAMBER (ALIAS  RZ )
C
      DATA  XMAXST(16) /  5530.0 /
      DATA  XMINST(16) / -1250.0 /
      DATA  YMINST(16) /     0.0 /
C
C                            VIEW VRX  (RFI VERTEX REGION)
C
      DATA  XMAXST(17) /     7.0 /
      DATA  XMINST(17) /   - 8.5 /
      DATA  YMINST(17) /   - 6.0 /
C
C                            VIEW VRZX (ZX  VERTEX REGION)
C
      DATA  XMAXST(18) /     7.0 /
      DATA  XMINST(18) /   - 8.5 /
      DATA  YMINST(18) /   - 6.0 /
C
C                            VIEW VRZY (ZY  VERTEX REGION)
C
      DATA  XMAXST(19) /     7.0 /
      DATA  XMINST(19) /   - 8.5 /
      DATA  YMINST(19) /   - 6.0 /
C
C                            VIEW VC   (VERTEX CHAMBER)
C
      DATA  XMAXST(20) /   345.0 /
      DATA  XMINST(20) / - 345.0 /
      DATA  YMINST(20) / - 260.0 /
C
C                            SPARE VIEWS
C
      DATA  XMAXST(21) /   11.0 /
      DATA  XMINST(21) /    7.0 /
      DATA  YMINST(21) /    7.0 /
C
      DATA  XMAXST(22) /   11.0 /
      DATA  XMINST(22) /    7.0 /
      DATA  YMINST(22) /    7.0 /
C
      DATA  XMAXST(23) /   11.0 /
      DATA  XMINST(23) /    7.0 /
      DATA  YMINST(23) /    7.0 /
C
      DATA  XMAXST(24) /   11.0 /
      DATA  XMINST(24) /    7.0 /
      DATA  YMINST(24) /    7.0 /
C
      DATA  XMAXST(25) /   11.0 /
      DATA  XMINST(25) /    7.0 /
      DATA  YMINST(25) /    7.0 /
C
      DATA  XMAXST(26) /   11.0 /
      DATA  XMINST(26) /    7.0 /
      DATA  YMINST(26) /    7.0 /
C
      DATA  XMAXST(27) /   11.0 /
      DATA  XMINST(27) /    7.0 /
      DATA  YMINST(27) /    7.0 /
C
      DATA  XMAXST(28) /   11.0 /
      DATA  XMINST(28) /    7.0 /
      DATA  YMINST(28) /    7.0 /
C
      DATA  XMAXST(29) /   11.0 /
      DATA  XMINST(29) /    7.0 /
      DATA  YMINST(29) /    7.0 /
C
      DATA  XMAXST(30) /   11.0 /
      DATA  XMINST(30) /    7.0 /
      DATA  YMINST(30) /    7.0 /
C
      END
