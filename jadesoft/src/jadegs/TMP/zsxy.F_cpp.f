C   24/09/81 111210224  MEMBER NAME  ZSXY     (HELIX)       FORTRAN
      SUBROUTINE ZSXY(NH1,LRFLAG,XJET1,YJET1,XJET2,YJET2)
C*811119*DITTMANN*************************************************
C*                                                               *
C*       X - Y   C O O R D I N A T E   O F   O N E   H I T       *
C*                                                               *
C*       THIS ROUTINE CONTAINS THE ESSENTIALS OF G. PEARCE'S OLD *
C*       JETXYZ. THE ACCURACY NEEDED FOR X-Y IS ONLY 5 MM,       *
C*       THEREFORE SMALL CORRECTIONS ARE OMITTED.                *
C*       COMMONS /CJDRCH/ AND /CDSMAX/ ARE NEEDED PARTLY.        *
C*       IF THIS SUBROUTINE SHOULD RUN IN AN SUPERVISOR INDEPEN- *
C*       DENT PROGRAM AND THOSE COMMONS ARE NOT FILLED, THE      *
C*       ROUTINE ZSSPEC HAS TO BE CALLED, WHICH FILLS THE        *
C*       NEEDED PARTS OF THE COMMONS.                            *
C*****************************************************************
          IMPLICITINTEGER*2(H)
          LOGICAL TBIT
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
C                            MACRO CJDRCH .... JET CHAMBER CONSTANTS.
C-----------------------------------------------------------------------
C
      COMMON / CJDRCH / RDEC(4),PSIIN(3),RINCR(3),FIRSTW(3),FSENSW(3),
     +                  RDEPTH,SWDEPL,YSUSPN,TIMDEL(2,3),ZMAX,ZOFFS,
     +                  ZRESOL,ZNORM,ZAL,ZSCAL,DRIDEV,DRICOS,DRISIN,
     +                  PEDES,TZERO(3),DRIROT(96,2),SINDRI(96,2),
     +                  COSDRI(96,2),DRIVEL(96,2),T0FIX(3),
     +                  ABERR(8), DUMJDC(20)
C
C      BLOCK DATA SET TO MC VALUES, KALIBR WILL SET REAL DATA VALUES
C--->  A CHANGE OF THIS COMMON MUST BE DONE SIMULTANEOUSLY WITH  <----
C--->  A CHANGE OF THE BLOCK DATA                                <----
C
C--------------------------- END OF MACRO CJDRCH -----------------------
C
C----------------------------------------------------------------------
C           MACRO CDSMAX .... PATTERN RECOGNITION CONSTANTS.
C----------------------------------------------------------------------
      COMMON/CDSMAX/DSMAX(16,3,2),DIRWR1(24,2),DIRWR3(48,2)
     *             ,DHALF(16,3,2),DTWICE(16,3,2),HMCH(16,3,2)
     *             ,IBCK(9),DBCK(30),TRMATS(96,2),TRMATC(96,2)
C------------------------ END OF MACRO CDSMAX -------------------------
C         ------------------
C         DATA AND CONSTANTS
C         ------------------
          INWIRE=HDATA(NH1)
          INWIRE=ISHFTR(INWIRE,3)
          LAYER=LAND(INWIRE,15)
          INCELL=LAND(ISHFTR(INWIRE,4),127)+ 1
          INRING=1
          IF(INCELL.GT.24)INRING=2
          IF(INCELL.GT.48)INRING=3
          DR0=RINCR(INRING)
          IW1=INCELL-24*(INRING-1)
          IF(INRING.EQ.3)GOTO 10
          DXWR=DIRWR1(IW1,1)
          DYWR=DIRWR1(IW1,2)
          GOTO20
 10       DXWR=DIRWR3(IW1,1)
          DYWR=DIRWR3(IW1,2)
C         -------------------------
C         COMPUTE (X,Y,R) OF LAYER
C         INCLUDING WIRE STAGGERING
C         -------------------------
 20       R1=FSENSW(INRING)+LAYER*DR0
          IW1=-1
          IF(TBIT(LAYER,31))IW1=+1
          X1=R1*DXWR+IW1*SWDEPL*DYWR
          Y1=R1*DYWR-IW1*SWDEPL*DXWR
C         -----------------------
C         SET LEFT/RIGHT SOLUTION
C         -----------------------
          NEXTLR=0
          LR1M1=ISIGN(1,LRFLAG)
 30       LR12=1
          IF(LR1M1.GT.0)LR12=2
C         --------------------------------
C         CONVERT DRIFT TIME INTO DISTANCE
C         --------------------------------
          DRIFT=HDATA(NH1+3)*DRIVEL(INCELL,LR12)
C         -------------------------
C         ADD DRIFT TIME TO (X,Y,R)
C         -------------------------
          TRLORX=TRMATS(INCELL,LR12)
          TRLORY=TRMATC(INCELL,LR12)
          XDRFT=LR1M1*TRLORX*DRIFT
          YDRFT=LR1M1*TRLORY*DRIFT
          IF(NEXTLR.EQ.1)GOTO70
          XJET1=X1+XDRFT
          YJET1=Y1+YDRFT
          NEXTLR=1
          LR1M1=-LR1M1
          GOTO30
 70       XJET2=X1+XDRFT
          YJET2=Y1+YDRFT
          RETURN
          END
