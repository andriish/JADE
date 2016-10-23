C   01/11/84 605221021  MEMBER NAME  JADISP   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE JADISP(INDEX)
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. OLSSON   PREHISTORY :  DRAW JADE DETECTOR
C
C        MOD: C. BOWDERY     9/04/84 :  IMPROVED Z CHAMBER DISPLAY
C        MOD: S. CARTWRIGHT 11/04/84 :  STILL BETTER Z-CH DISPLAY
C        MOD: C. BOWDERY    26/04/84 :  RADICAL CHANGES TO STRUCTURE
C        MOD: C. BOWDERY    24/05/84 :  FWCAP CALL CHANGED TO DRAWFD(1)
C        MOD: C. BOWDERY     7/06/84 :  NEW COMMAND NUMBERS
C        MOD: C. BOWDERY    18/06/84 :  VRXCIR NO LONGER CALLED HERE
C        MOD: C. BOWDERY    20/06/84 :  SET DETDRW FLAG WHEN CALLED
C        MOD: C. BOWDERY    27/06/84 :  CHANGES FOR VERTEX CHAM/BP CNTRS
C        MOD: J. HAGEMANN   19/10/84 :  SPECIAL VERTEX CHAMBER VIEW
C                                       ADDED (INDEX = 20)
C   LAST MOD: J. HAGEMANN   22/05/86 :  FLAG NEWDET NOW SWITCHED BY FLAG
C                                       LVTXC IN COMMON / CVCEX / FOR
C                                       MC DATA AND BY FLAG LNHARD FOR
C                                       REAL DATA
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL INNERC,LEADGL,MUONFL,PHNUMS,PHHIST
      LOGICAL DSPDTM,ZCHAMR,NEWDET,ZCBK,VXBK,VTXCHR,DRVTXC
      LOGICAL DETDRW,REDRDT,LVTXC,LNHARD
C
      COMMON / CHEADR / HEAD(108)
      COMMON / CVCEX  / LVTXC
      COMMON / CJTRIG / PI,TWOPI
      COMMON / CGRAP2 / BCMD,DSPDTM(30),ISTVW,JTVW
      COMMON / CGRAP6 / DETDRW
C
      COMMON / CWORK1 / VTXCHR,JNDEX,INNERC,LEADGL,MUONFL,
     +                  PHNUMS,PHHIST,ZCHAMR,NEWDET,ZCBK,VXBK,DRVTXC
C
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
C                        --- MACRO CADMIN ---
C
      LOGICAL*1 LBREAD
C
      COMMON / CADMIN / IEVTP,NRREAD,NRWRIT,NRERR,LBREAD(4),IFLG,IERCAL,
     +                  ISMEAR,IJETCI,NFLAGS(10)
C
C                                 NFLAGS IS AN ARRAY OF GENERAL FLAGS
C                                   (1) : USED BY RDDATE
C                                   (2) : USED BY RDTRIG
C                                   (3) : USED BY RDTRIG
C                                   (4) : USED BY RDTRIG / PRSTAT
C                                   (5) : USED BY EVREAD -COUNTS RECORDS
C                                   (6) : USED BY SUPERV -COUNTS ERRORS
C                                   (7) : USED BY EVWRIT -'HEAD'LESS EVS
C                                   (8) : USED BY EVREAD/RDMTCO (EVWRIT)
C                                   (9) : USED BY RDMTCO/EVWRIT
C                                  (10) : FREE
C
C                                  BLOCK DATA SET IN MEMBER JADEBD
C
C-----------------------------------------------------------------------
C                            MACRO CGEO1 .... JADE GEOMETRY
C-----------------------------------------------------------------------
C
      COMMON / CGEO1 / BKGAUS,
     +                 RPIP,DRPIP,XRLPIP,   RBPC,DRBPC,XRLBPC,
     +                 RITNK,DRITNK,XRLTKI, R0ROH,DR0ROH,XR0ROH,
     +                 R1ROH,DR1ROH,XR1ROH, R2ROH,DR2ROH,XR2ROH,
     +                 R3ROH,DR3ROH,XR3ROH, ROTNK,DROTNK,XRLTKO,
     +                 RTOF,DRTOF,XRTOF,    RCOIL,DRCOIL,XRCOIL,
     +                 ZJM,DZJM,XRZJM,ZJP,DZJP,XRZJP,ZTKM,DZTKM,XRZTKM,
     +                 ZTKP,DZTKP,XRZTKP,ZBPPL,ZBPMI,ZTOFPL,ZTOFMI,
     +                 XRJETC,RLG,ZLGPL,ZLGMI,OUTR2,CTLIMP,
     +                 CTLIMM,DELFI,BLXY,BLZ,BLDEP,ZENDPL,ZENDMI,DEPEND,
     +                 XHOL1,XHOL2,YHOL1,YHOL2,BLFI
C
C------------------------- END OF MACRO CGEO1 --------------------------
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
C
C------------------  C O D E  ------------------------------------------
C
      JNDEX  = INDEX
      IF( INDEX .GT. 7 ) JNDEX = INDEX - 4
C
      IF( INDEX .GT.  0  .AND.  INDEX .LT. 21 ) GO TO 1
      RETURN
C
   1  LASTVW = INDEX
      DETDRW = .TRUE.
C
C                            NEW DETECTOR HARDWARE IN MONTE-CARLO DATA?
C                            THEN LVTXC IS TRUE (FLAG IS CHECKED AND
C                                                SET IN RDMTCO)
C
      LNHARD = (HEAD(15) .GE. 5  .AND.  HEAD(16) .EQ. 1984)
     +                          .OR.   HEAD(16) .GE. 1985
      NEWDET = LVTXC .OR. (IEVTP.EQ.0 .AND. LNHARD)
     +
      INNERC = JNDEX .LE. 7  .AND.  DSPDTL(1)
C
      LEADGL = ( JNDEX .EQ. 2  .OR.  JNDEX .EQ. 3 .OR.
     +           JNDEX .EQ. 5  .OR.  JNDEX .EQ. 6 .OR.
     +           JNDEX .EQ. 7                        )  .AND.  DSPDTL(2)
C
      MUONFL = ( JNDEX .EQ. 3  .OR.  JNDEX .EQ. 6  .OR.  JNDEX .EQ. 7 )
     +                        .AND.  DSPDTL(3)
      PHNUMS = LEADGL  .AND.  ( .NOT. DSPDTL(7) )
      PHHIST = LEADGL  .AND.  DSPDTL(7)
C
C                            SET Z CHAMBER DRAWING FLAG
C                            IF CDTL 41 SET  AND 1 OF THE FOLLOWING:
C                             - CDTL 42 SET (DRAWING FORCED)
C                             - DATE OF EVENT IS MAY 1984 OR AFTER
C                             - Z CHAMBER BANK PRESENT
C
      ZCBK   = IDATA( IBLN( 'ZETC' ) ) .GT. 0
      ZCHAMR = DSPDTM(11)  .AND.  (DSPDTM(12)  .OR.  NEWDET  .OR.  ZCBK)
C
C                            SET VERTEX CHAMBER AND NEW SMALL BEAM PIPE
C                            FLAG IF ONE OF THE FOLLOWING IS TRUE
C                             - CDTL 44 SET (DRAWING FORCED)
C                             - DATE OF EVENT IS MAY 1984 OR AFTER
C                             - VERTEX CHAMBER BANK PRESENT
C                            DRAW VERTEX CHAMBER IF ABOVE FLAG SET AND
C                            IF CDTL 43 SET.
C
      VXBK   = IDATA( IBLN( 'VTXC' ) ) .GT. 0
      VTXCHR = DSPDTM(14)  .OR.  NEWDET  .OR.  VXBK
      DRVTXC = DSPDTM(13)  .AND.  VTXCHR
C
      NECC   = ACMD
      IF(  INDEX .EQ. 20 ) GO TO 10
      IF(  JNDEX .GT.  3  .AND.  INDEX .NE. 17 ) GO TO 3
C
C                            LSTCMD = 38   IS DET
C
      IF( LSTCMD .EQ. 38  .AND.   NECC .NE.  0 ) GO TO 2
C
C
C                            ***************
C                            *  RFI-VIEWS  *
C                            ***************
C
C                                      BEAM PIPE
C
   10 CALL DRAWBP( 1 , VTXCHR )
C
C                                      BEAM PIPE COUNTERS
C
      IF( .NOT. VTXCHR ) CALL BARSEC(PI/24.0,0.,0.,RBPC,
     +                               RBPC+DRBPC,0.,0.,24,PI/12.0,0.)
C
C                                      VERTEX CHAMBER
C
      IF( DRVTXC ) CALL DRAWVC( 1 , JNDEX )
C
C                                      INNER DETECTOR
C
      IF( INDEX .EQ. 20 ) INNERC = .TRUE.
C
      IF( INNERC ) CALL DRAWID( 1 )
C
      IF( INDEX .EQ. 20 ) RETURN
C
C                                      Z CHAMBER
C
      IF( ZCHAMR ) CALL DRAWZC( 1 )
C
C                                      TOF COUNTERS
C
      CALL BARSEC(PI/42.0,0.,0.,RTOF,RTOF+DRTOF,0.,0.,42,PI/21.0,0.)
C
C                                      LEAD GLASS BARREL
C
      IF( LEADGL ) CALL BARSEC(0.,0.,0.,RLG,OUTR2,0.,0.,84,TWOPI/84.,0.)
C
C
C
C                                      MUON FILTER
C
      IF( (JNDEX .LE. 3)  .AND.  MUONFL ) CALL MUPIC
C
      IF( JNDEX .LT. 4  .OR.  INDEX .EQ. 17 ) RETURN
C
C                            IF DET 1/2/3 WAS REQUESTED, DRAW ENDCAP
C                            AND/OR FORWARD DETECTOR HARDWARE.
C
  2   IF( NECC  .EQ. 1  .OR.  NECC  .EQ.  3 ) CALL ECAPCY(0.,0.,1.0)
      IF( NECC  .EQ. 2  .OR.  NECC  .EQ.  3 ) CALL DRAWFD(1)
C
      RETURN
C
C
C
  3   IF( JNDEX .GT. 7 ) GO TO 5
C
C                            ***************************
C                            *  ZX-VIEWS AND ZY-VIEWS  *
C                            ***************************
C
C                                      BEAM PIPE
C
      CALL DRAWBP( 2 , VTXCHR )
C
C                                      VERTEX CHAMBER
C
      IF( DRVTXC ) CALL DRAWVC( 2 , IDUMMY ) ! PMF 19/10/99: add dummy argument IDUMMY in parameter list
C
C                                      INNER DETECTOR
C
      IF( INNERC ) CALL DRAWID( 2 )
C
C                                      Z CHAMBER
C
      IF( ZCHAMR ) CALL DRAWZC( 2 )
C
C                                      TOF COUNTERS IN RZ VIEW
C
      IF( JNDEX .EQ. 4 ) GO TO 4
      CALL RECTAN(ZTOFMI,-RTOF-DRTOF,ZTOFPL,-RTOF,      0)
      CALL RECTAN(ZTOFMI, RTOF,      ZTOFPL, RTOF+DRTOF,0)
C
C                                      LEAD GLASS CYLINDER AND ENDCAPS
C
  4   IF( LEADGL ) CALL DRAWLG( 2 )
C
C                                      MUON FILTER
C
      IF( JNDEX .GE. 4  .AND.  JNDEX .LE. 7  .AND.  MUONFL) CALL MUPIC
      IF( JNDEX .EQ. 6  .OR.   JNDEX .EQ. 7 ) CALL FWMUCN
      IF( JNDEX .EQ. 7                      ) CALL DRAWFD( 2 )
  5   IF( JNDEX .LT. 8 ) RETURN
C
C                            ******************************
C                            *  FORWARD DETECTOR DISPLAY  *
C                            ******************************
C
      IF( INDEX .EQ. 12 ) CALL DRAWFD( 3 )
C
C                            ***************************************
C                            *  ROLLED OUT VIEW OF LG AND TAGGING  *
C                            ***************************************
C
      IF( INDEX .EQ. 13 ) CALL DRAWLG( 3 )
C
C                            *************************************
C                            *  PERSPECTIVE VIEW OF LG CYL + ID  *
C                            *************************************
C
      IF( INDEX .EQ. 14 ) CALL DRAWLG( 4 )
C
C                            ***************************
C                            *  FORWARD MUON COUNTERS  *
C                            ***************************
C
      IF( INDEX .EQ. 15      .AND.
     +  ( HEAD(18) .GT. 3730  .OR.  HEAD(18) .LT. 100 ) ) CALL FWMUCN
C
C                            *******************************
C                            *  Z CHAMBER ROLLED OUT VIEW  *
C                            *******************************
C
      IF( INDEX .EQ. 16 ) CALL DRAWZC( 3 )
      RETURN
      END
