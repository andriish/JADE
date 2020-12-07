C   25/11/80 011262001  MEMBER NAME  PREDIT   (JADEGS)      FORTRAN
      SUBROUTINE PREDIT
      IMPLICIT INTEGER*2 (H)
C
C     MAIN SUBROUTINE FOR PATREC IN JET CHAMBERS
C     PETER STEFFEN 15/ 9/79
C *****  GRAPHICS EDITING VERSION ** LAST CHANGE 21.02.80   J.OLSSON ***
C *****  CHANGED TO NEW FORMATS FOR HITLABEL ARRAY AND MIDOUT        ***
C----------------------------------------------
C  MACRO CWORKPR .... PATTERN RECOGNITION CWORK
C----------------------------------------------
      COMMON /CWORK/ HPLAST,HPFREE,HPWRK(30),ADWRK(600),
     ,               HPRO,HNTR,HNTCEL(98),IPCL(200),NRHT(200),
     ,               NWR1(200),DS1(200),SL1(200),
     ,               NWR2(200),DS2(200),SL2(200),
     ,               LBL(200),NTREL(200),ICRO(200),
     ,               NTR,HNREL(100),HISTR(9,100),HRES(168),
     ,               NTRLM,RLMTR(3,5),
     ,               WRK(7000)
                     DIMENSION TRKAR(200,11),ITRKAR(200,11),
     ,                         LMRTR(3,5)
                     EQUIVALENCE (IPCL(1),TRKAR(1,1),ITRKAR(1,1))
                     EQUIVALENCE (LMRTR(1,1),RLMTR(1,1))
         DIMENSION IWRK(7000),HWRK(14000),IDWRK(600),HDWRK(1200)
                     EQUIVALENCE (IWRK(1),WRK(1),HWRK(1))
                     EQUIVALENCE (IDWRK(1),ADWRK(1),HDWRK(1))
C---------- END OF MACRO CWORKPR --------------
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
C----------------------------------------------
C  MACRO CPATLM .... PATTERN RECOGNITION LIMITS
C----------------------------------------------
      COMMON /CPATLM/ PATRLM(5),FLINLM(10),TRELLM(20),ZFITLM(10),BKK(20)
     *               ,XYF(20),IGFP(20),XBKK(40),IADMIN(5),YBKK(20)
      INTEGER IXYF(20),LMPATR(5),LMFLIN(10)
      INTEGER LMTREL(20),LMZFIT(10),IBKK(20)
      DIMENSION GFP(20),IXBKK(40),IYBKK(20)
      EQUIVALENCE (PATRLM(1),LMPATR(1)),(IXBKK(1),XBKK(1)),(IYBKK(1),
     *YBKK(1))   ,(FLINLM(1),LMFLIN(1)),(TRELLM(1),LMTREL(1))
     *           ,(ZFITLM(1),LMZFIT(1)),(BKK(1),IBKK(1))
     *           ,(XYF(1),IXYF(1)),(GFP(1),IGFP(1)),(IADMIN(1),IMCERT)
     *           ,(IYBKK(20),IPPASS),(IADMIN(2),IPFAST)
C----------- END OF MACRO CPATLM --------------
      IF(ACMD.EQ.0.) GO TO 92
      CALL PATRC1(0)
      CALL TRMOUT(80,' EDIT ENTERED AFTER BACKTRACE, NO CALL TO FIT AND
     +ADMINISTRATION PROGRAM DONE^')
      CALL TRMOUT(80,' TO STOP AUTOMATIC DELETION OF LOW ENERGY TRACKS,
     +USE COMMAND NR 33^')
      GO TO 93
92    CALL PATREC(0)
93    NTRLM = 0
      ACMD = 0.
      CALL CREDIT
      RETURN
      END
