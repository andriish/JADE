C   17/04/85 504181858  MEMBER NAME  RZHITS   (ZS)          FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE RZHITS(IPZETC,XZ,YZ,XZ1,YZ1,XZ2,YZ2,ZZ1,ZZ2,IERR)
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. OLSSON        ?     :  CALCULATE COORDINATES OF
C                                        FOR RZ CHAMBERS
C
C       MOD:   J. HAGEMANN   09/10/84 :  NOW OWN MEMBER (FROM EVDISP)
C       MOD:   C. BOWDERY    17/04/85 :  COMMENT OUT ERROR PRINTOUT
C  LAST MOD:   S. CARTWRIGHT 17/04/85 :  COMPLETELY REWRITTEN
C
C        IPZETC POINTS TO WORD BEFORE THE  QUADRUPLE OF HALFWORDS IN BCS
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL TBIT
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
C-----------------------------------------------------------------------
C                            MACRO CGRAPH .... GRAPHICS COMMON
C-----------------------------------------------------------------------
C
      LOGICAL DSPDTL,SSTPS,PSTPS,FREEZE
C
      COMMON / CGRAPH / JUSCRN,NDDINN,NDDOUT,IDATSV(11),ICREC,MAXREC,
     +                  LSTCMD,ACMD,LASTVW,ISTANV,
     +                  SXIN,SXAX,SYIN,SYAX,XMIN,XMAX,YMIN,YMAX,
     +                  DSPDTL(30),SSTPS(10),PSTPS(10),FREEZE(30),
     +                  IREADM,LABEL,LSTPS(10),IPSVAR
C
C------- END OF MACRO CGRAPH -------------------------------------------
C
C
      COMMON /CZGEO/ RZCHI,RZCHA,NZRPSI,NZZ,Z1ZCH,Z2ZCH,ZCHA,ZCHB,ZCHSS,
     $               ZCHDL,ZCHDLL,DLZZ,DLZPHI,DLZW1,DLZW2
      COMMON /CZKON/ ZCVDR,ZCXCH,ZCTZER,ZCAPED,XL1,XL2
C
      COMMON /CJTRIG/ PI,TWOPI,PIHALF,PI3HAF
C
      INTEGER IZPED(2,64)/58,62, 47,45, 58,38, 47,37, 45,39, 52,54,
     *                    46,54, 54,36, 37,27, 41,30, 53,41, 45,39,
     *                    46,38, 43,43, 54,43, 48,46, 40,44, 43,38,
     *                    41,38, 44,38, 33,36, 42,43, 44,50, 42,35,
     *                    40,29, 34,33, 30,36, 41,39, 46,30, 39,42,
     *                    39,36, 45,40, 44,40, 31,43, 45,45, 40,39,
     *                    45,41, 45,42, 42,48, 20,28, 26,24, 22,28,
     *                    27,30, 32,24, 31,40, 29,29, 34,25, 32,28,
     *                    34,39, 52,45, 52,43, 50,42, 54,46, 50,48,
     *                    43,49, 48,43, 41,39, 42,42, 49,50, 40,41,
     *                    42,24, 51,47, 55,51, 47,41/
C
      REAL ZSTA(64)      /-1., 1., -1., 1., -1., 1., -1., 1.,
     *                    -1., 1., -1., 1., -1., 1., -1., 1.,
     *                     1.,-1.,  1.,-1.,  1.,-1.,  1.,-1.,
     *                     1.,-1.,  1.,-1.,  1.,-1.,  1.,-1.,
     *                    -1., 1., -1., 1., -1., 1., -1., 1.,
     *                    -1., 1., -1., 1., -1., 1., -1., 1.,
     *                     1.,-1.,  1.,-1.,  1.,-1.,  1.,-1.,
     *                     1.,-1.,  1.,-1.,  1.,-1.,  1.,-1./
C
      DIMENSION WLEN(2)
C
      DATA ICAL /0/,FIRES/0.01/
C
C-----------------  C O D E  -------------------------------------------
C
      IERR = 0
      ICAL = ICAL + 1
      IF(ICAL.GT.1.)         GOTO 10
         PIBY12 = PI/12.
         PIBY24 = PI/24.
         WLEN(1)= 12.*DLZW1 - 200.
         WLEN(2)= 12.*DLZW2 - 200.
         KHEAD  = IBLN('HEAD')
 10   CONTINUE
      JZETC = 2*IPZETC
C ----------------------------------------------------------------------
C                             CALIBRATION
C                             THIS WILL BE ORGANISED BETTER IN THE
C                             FUTURE,  BUT  FOR THE TIME BEING YOU
C                             HAVE TO SETTLE FOR THIS MESS........
      IHEAD = IDATA(KHEAD)
      KRUN  = HDATA(2*IHEAD + 10)
C --------------------------- DRIFT VELOCITY
      IF(KRUN.GT.0)           ZCVDR = 0.28
      IF(KRUN.GT.19058)       ZCVDR = 0.39
      IF(KRUN.GT.19079)       ZCVDR = 0.35
C --------------------------- CHARGE DIVISION FOR REAL DATA
      IF(KRUN.NE.0)           ZCXCH = 0.89
C ----------------------------------------------------------------------
      IWIR = HDATA(JZETC+1)
      IWIR = ISHFTR(IWIR,3)
      IA1  = HDATA(JZETC+2)
      IA2  = HDATA(JZETC+3)
      ITDC = HDATA(JZETC+4)
C --------------------------- I SUPPOSE A SILLY WIRENO IS POSSIBLE...
      IF(IWIR.LT.0 .OR. IWIR.GT.63)    GOTO 99
C --------------------------- PEDESTALS .... AND T0S WHEN I GET THEM ...
      IF(KRUN.EQ.0)           GOTO 20
         IA1 = MAX(IA1-IZPED(1,IWIR+1),0)
         IA2 = MAX(IA2-IZPED(2,IWIR+1),0)
 20   CONTINUE
C --------------------------- PHI FROM CHARGE DIVISION
      ADIF = 0.
      IF(IA1.GT.0 .OR. IA2.GT.0)   ADIF = FLOAT(IA2-IA1)/FLOAT(IA1+IA2)
      ILAY = MOD(IWIR,2) + 1
      WL   = WLEN(ILAY)
      WL1  = 0.5 * WL * (1. + ADIF/ZCXCH)
C --------------------------- FORCE 0<WL1<WL
      IF(WL1.LT.0.)           WL1 = 0.
      IF(WL1.GT.WL)           WL1 = WL
C --------------------------- GET ANGLE (CHI=PHI BUT WRT Y-AXIS)
      CHI  = (WL1+100.)/(WL+200.) * PI
      IF(IWIR.LT.32)          CHI = CHI+PI
C --------------------------- GET Z
      NCELL= MOD(IWIR/2,16)
      ZWIR = -1125. + 150.*NCELL + ZSTA(IWIR+1)
      ZTDC = (0.5+ITDC) * ZCVDR
      ZZ1  = ZWIR - ZTDC
      ZZ2  = ZWIR + ZTDC
C --------------------------- GET X AND Y FOR PLOTTING
      RZIN = 869.5 + 10.*(ILAY-1)
      PSI  = AMOD(CHI,PIBY12)
      PSI  = ABS(PSI-PIBY24)
      RHO  = RZIN/COS(PSI)
      XZ   = -RHO*SIN(CHI)
      YZ   =  RHO*COS(CHI)
C -------------------------- UPPER AND LOWER BOUNDS
      RES  = PI*FIRES
      PSI  = AMOD(CHI-RES,PIBY12)
      PSI  = ABS(PSI-PIBY24)
      RHO  = RZIN/COS(PSI)
      XZ1  = -RHO*SIN(CHI-RES)
      YZ1  =  RHO*COS(CHI-RES)
      PSI  = AMOD(CHI+RES,PIBY12)
      PSI  = ABS(PSI-PIBY24)
      RHO  = RZIN/COS(PSI)
      XZ2  = -RHO*SIN(CHI+RES)
      YZ2  =  RHO*COS(CHI+RES)
C -------------------------- ALL DONE
      RETURN
C -------------------------- NONEXISTENT WIRE NUMBER
 99   CONTINUE
      IERR = 1
      RETURN
      END
