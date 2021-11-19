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

CAV      COMMON / CZGEO  / RZCHI,RZCHA,NZRPSI,NZZ,Z1ZCH,Z2ZCH,ZCHA,ZCHB,
CAV     +                  ZCHSS,ZCHDL,ZCHDLL,DLZZ,DLZPHI,DLZW1,DLZW2
      COMMON / CGRAP2 / BCMD,DSPDTM(30),ISTVW,JTVW
      COMMON / CGRAP3 / IDEFWP, CDICTY(700)
C
      COMMON / CGRAP4 / IPOS4,CCMD(8,200),HCMD(5,200)
      COMMON / CGRAP5 / KOMCAL,LERROR
      COMMON / CGRMAC / MACNR, MACSTA, MACDEP, MACPNT(2,10), CDEF(80,31)
      COMMON / CSTANV / HDISPN, HCSTV2, HCSTV3
C
#include "chsym.for"
#include "cgrscl.for"
C
C------------------  D A T A  ------------------------------------------
C
      DATA HSYM /2H0 ,2H1 ,2H2 ,2H3 ,2H4 ,2H5 ,2H6 ,2H7 ,2H8 ,2H9 ,2HA ,
     + 2HB ,2HC ,2HD ,2HE ,2HF ,2HG ,2HH ,2HI ,2HJ ,2HK ,2HL ,2HM ,2HN ,
     + 2HO ,2HP ,2HQ ,2HR ,2HS ,2HT ,2HU ,2HV ,2HW ,2HX ,2HY ,2HZ /
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
CAV      DATA  RZCHI,RZCHA /864.5, 884.5/
CAV      DATA  NZRPSI,NZZ /24,16/
CAV      DATA  Z1ZCH,Z2ZCH /-1200.,1200./
CAV      DATA  ZCHA,ZCHB,ZCHSS /5.,15.,1./
CAV      DATA  ZCHDL,ZCHDLL /100.,20./, DLZZ,DLZPHI /150.,227.63/
CAV      DATA  DLZW1,DLZW2 /228.94,231.58/
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
