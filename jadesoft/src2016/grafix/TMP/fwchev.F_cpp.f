C   20/03/84 711172327  MEMBER NAME  FWCHEV   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE FWCHEV
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. OLSSON PREHISTORY :  DISPLAY FORWARD (TAGGING) SYSTEM
C
C        MOD: J. OLSSON   10/06/79 :  CHAMBER DISPLAY ADDED
C        MOD: J. OLSSON    4/10/83 :  CHAMBER DISPLAY REMOVED. (FWCHE0)
C        MOD: J. OLSSON    8/02/84 :
C        MOD: A. FINCH    12/03/84 :  UPDATED. SEE TAGG.S(TAGNOTE)
C        MOD: J. NYE      30/05/84 :  USES TAGMRK RATHER THAN TAGINT
C        MOD: C. BOWDERY   8/06/84 :  NEW COMMAND NUMBERS.
C   LAST MOD: C. BOWDERY   7/08/85 :  NEW TAGGING NAMES + TAGINT BACK
C   LAST MOD: J. OLSSON   23/10/85 :  PROPER HANDLING OF MC PEDESTALS
C
C
C        DISPLAY JADE FORWARD DETECTOR EVENT.
C        USED IN VIEWS NR 12(FW) 13(RU) AND COMMAND 58(FC)
C
C        1979-80 TAGGING APPARATUS
C        -------------------------
C
C        SUBTRACT A STANDARD PEDESTAL FROM LEAD GLASS, CONVERT TO MEV
C        WITH A STANDARD FACTOR 5.0      STANDARD PEDESTAL = 500 COUNTS
C        WITH A STANDARD FACTOR 7.5, PEDESTAL = 50 COUNTS AFTER RUN 278200002600
C
C        1981-82,83.. TAGGING APPARATUS
C        ------------------------------
C
C        CHANGED TO USE THE CALIBRATION OF A.FINCH, FOR 1981-82 AND 198300003100
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
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
C---------  MACRO CHSYM    SYMBOLS USED IN GRAPHICS WRITING -----
      COMMON /CHSYM/ HSYM(36)
C------ END MACRO CHSYM
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
C-----------------------------------------------------------------------
C                            MACRO CGEO2 .... JADE TAGGING GEOMETRY
C-----------------------------------------------------------------------
C
      COMMON / CGEO2 / FENDC,XYHOL1,XYHOL2,BLDPFW,ZMINBL,ZPLUBL,
     +                 XSC(2),YSC(2),RSC(2),ZMISC(2),ZPLSC(2),DZSC,
     +                 CHX(3,4),CHY(3,4),CHZ(3,4),WLEN,PITCH,WZDIS
C
C--------------------------- END OF MACRO CGEO2 ------------------------
C
C----------------------------------------------------------------------
C      MACRO CGEO3 .... JADE FORWARD DETECTOR GEOMETRY, 1981-3 VERSION
C----------------------------------------------------------------------
C
      COMMON / CGEO3 / ZPLUM2,ZMINM2,NRPBSC,PBSCR(4),PBSCZ(4)
C
C------------------------ END OF MACRO CGEO3 --------------------------
C
C
C
C------------ C O M M O N    C W O R K   F O R   T A G A N -------------
C
C
       COMMON/CWORK/MARK,IFLMRK,IMC,NCLST,NNEI,
     *              ISTMZ,ISTPZ,IENDMZ,IENDPZ,
     *              SIGX,SIGY,SIGEN,
     *              CAND(3),CLUS(9,2),CMAP(10,9),
     *              SADC(32,2),CATAG(192)
C
C
C CWORK - WORKSPACE USED ONLY ONCE PER EVENT FOR INTERNAL PROCESSING
C ==================================================================
C
C MARK   ->  WHICH 'MARK' OF TAGGER - 1 = 1981,2
C                                   - 2 = 1983 ONWARDS
C
C IFLMRK ->  SET TO '1' BY TAGMRK
C
C IMC    ->  SET TO '1' BY TAGMRK IF MC DATA
C
C CATAG  ->  CONTAINS THE ADC CONTENTS UNPACKED FROM ATAG
C
C SADC   ->  COMMON FOR ADC'S AFTER SORTING  (SORT 1)
C
C CMAP(I,1...9) ->  ADDRESS OF ADC'S IN CLUSTER I,SORT23 PUTS THESE IN
C                   ORDER OF ENERGY.
C
C CAND(3) ->  X, Y, AND ENERGY OF A FOUND CLUSTER IN AFTER CLSPS
C
C SIGX,SIGY,SIGEN ->  ERROR ON X, Y, AND ENERGY AFTER CLSPS
C
C CLUS(9,2) ->  ADC ADDRESS AND CONTENTS OF CLUSTERS - SORTED BY ENERGY
C
C NCLST   ->  NUMBER OF CLUSTERS THIS END
C ISTMZ   ->  POINTER TO START OF -Z DATA IN CATAG ( ALWAYS  1       )
C ISTPZ   ->  POINTER TO START OF +Z DATA IN CATAG ( EITHER 33 OR 25 )
C IENDMZ  ->  POINTER TO   END OF -Z DATA IN CATAG ( EITHER 32 OR 24 )
C IENDPZ  ->  POINTER TO   END OF +Z DATA IN CATAG ( EITHER 64 OR 48 )
C
C A.J.FINCH 24/2/84
C MODIFIED 12/3/84 CATAG PUT TO END AND INCREASED TO 192
C  TO ALLOW IT TO BE USED FOR 1979,80 TAGGER IN GRAPHICS
C LAST MOD : J. NYE  30/05/84  RE-ORGANIZED INCLUDING IFLMRK
C
C
C-----------------------------------------------------------------------
C
C
      COMMON / CJTRIG / PI,TWOPI
      COMMON / CHEADR / HEAD(108)
      COMMON / CWORK1 / R,FI,R1,FI1,X1,Y1,R2,FI2,X2,Y2,ZET,X3,Y3,X4,Y4,
     +                  HMW(132)
C
      DIMENSION  NPBPED(3),PBGCAL(3)
C
      DATA NPBPED / 500, 50 ,0/, PBGCAL / 5.0, 7.5 ,5.5/
C
C------------------  C O D E  ------------------------------------------
C
C
      IPNTR = IDATA(IBLN('ATAG'))
      IF( IPNTR .LT. 1 ) GO TO 300
C
CCC                        USE TAGMRK SO THAT WE CAN ALSO HANDLE MC DATA
CCC
CCC   CALL TAGMRK(*300)
C
C                          USE TAGINT ONCE TO INITIALISE CWKTAG
C
      CALL TAGINT(*300)
C
      ADDEND = 84.0 * BLFI
      FDX    = 0.0
      ADY    = 0.0
      FACTFW = 1.0
      SH3    = 0.75 * FENDC
      IF( LASTVW .NE. 12 ) GO TO 2
C
C                           FW VIEW (12)
C
      FDX    = -(XMAX - XMIN) * 0.25
      FACTFW = 1.5
      SH3    = 0.70 * FENDC
   2  CONTINUE
      IF( LASTVW .NE. 13 ) GO TO 3
C
C                            RU VIEW (13)
C
      IF ( MARK .EQ. 2 ) SH3 = FENDC
      FACTFW = 2.0
      ADY    = 900.0
      FDX    = 0.5 * ADDEND
C
   3  IF( MARK .EQ. 1 ) GO TO 1111
      IF( MARK .NE. 0 ) GO TO 2222
C
C                            ENTER HERE FOR 1979-80 APPARATUS
C
      NBLIM = 190
      IRUN  = 1
C
C                            NO PROVISION FOR THIS IN TAGMRK SCHEME
C
      IF ( HEAD(18) .GT. 2781 ) IRUN = 2
      IF ( IMC .EQ. 1 ) IRUN = 3
C--
C  LOOP OVER BLOCK HITS, GET BLOCK NUMBER NB AND PULSEHEIGHT HPH
C     SUBTRACT PEDESTAL AND MULTIPLY WITH GAIN, STORE IN CATAG ARRAY
C--
      IADD   = 2 * IPNTR + 6
      IFLUMI = IADD + HDATA( IADD - 1 )
  200 CONTINUE
      IADD   = IADD + 1
      IF( IADD .GE. IFLUMI ) GO TO 210
C
      NB   = HDATA(IADD)
      IADD = IADD + 1
      IF( NB .EQ. 0 ) GO TO 200
      IF( NB .GT. 190 ) GO TO 200
      CATAG(NB) = 0.0
      IF( NB .GT.  46 .AND. NB .LT.  49 ) GO TO 200
      IF( NB .GT.  94 .AND. NB .LT.  97 ) GO TO 200
      IF( NB .GT. 142 .AND. NB .LT. 145 ) GO TO 200
      HPH = HDATA(IADD)
      HPH = HPH - NPBPED(IRUN)
      IF ( HPH .LT. 0 ) HPH = 0
      CATAG(NB) = HPH * PBGCAL(IRUN)
      GO TO 200
C
C
C
C
C----------------------------------------1981/82 APPARATUS ONLY---------
C
C
C                            ENTER HERE FOR 1981-82 APPARATUS
C
 1111 CONTINUE
C     IF( MARK .NE. 1 ) GO TO 2222
C
      NBLIM = 64
      CALL TAGADC(0,*300)
      CALL TAGPED
      CALL TAGKAL(0)
      GO TO 210
C
C
C
C----------------------------------------1983    APPARATUS ONLY---------
C
C
C                            ENTER HERE FOR 1983-.. APPARATUS
C
2222  CONTINUE
      IF( MARK .NE. 2 ) GO TO 999
C
      CALL TAGADC(0,*300)
      CALL TAGPED
      CALL TAGKAL(0)
      NBLIM = IENDPZ
C
C   DISPLAY LOOP FOR 1983-..  LEAD SCINTILLATOR ELEMENTS
C
      NTAGK=IENDMZ
C
C CHECK THE MAGNIFICATION, TO CHOOSE BETWEEN CRICRO AND PHNUMB
C
      IMAGOK = 0
      DELTX = ABS(XMAX-XMIN)
      IF ( DELTX .LT. 5000.0 .AND. LSTCMD .NE. 58 ) IMAGOK = 1
      IF ( DELTX .LT. 3000.0 .AND. LSTCMD .EQ. 58 ) IMAGOK = 1
C
      DO 400  NB = 1,NBLIM
      IF( CATAG(NB) .LT. 1.0 ) GO TO 400
C GET CORNER COORDINATES OF UNIT
      CALL XYTAG(NB,XTAG,YTAG,SLOTAG)
      ADX = FDX
      IF ( NB .GT. NTAGK .AND. LASTVW .EQ. 12 ) ADX = -FDX
      IF ( LASTVW .EQ. 13 ) ADX = FDX - 750.0
      IF ( NB .GT. NTAGK .AND. LASTVW .EQ. 13 ) ADX = FDX + 750.0
      IF( IMAGOK .EQ. 0 ) GO TO 399
      X1 = -XTAG * FACTFW + ADX
      Y1 =  YTAG * FACTFW + ADY
      IPH = CATAG(NB)
      CALL PHNUMB(IPH,SH3,SLOTAG)
      GO TO 400
399   X1 = -X1 * FACTFW
      X2 = -X2 * FACTFW
      X3 = -X3 * FACTFW
      X4 = -X4 * FACTFW
      Y1 =  Y1 * FACTFW
      Y2 =  Y2 * FACTFW
      Y3 =  Y3 * FACTFW
      Y4 =  Y4 * FACTFW
      CALL CRICRO(ADX,ADY)
400   CONTINUE
      GO TO 300
C
C
C
C----------------------------------------1979-82 DETECTORS ONLY---------
C
C
210   CONTINUE
C
C   DISPLAY LOOP FOR 1979-82  LEAD GLASS CAPS
C
      NTAGK=96
      IF ( MARK .EQ. 1 ) NTAGK = 32
C
      DO 333  NB = 1,NBLIM
      IF( CATAG(NB) .LT. 1.0 ) GO TO 333
      INB = NB
      IF ( MARK .EQ. 1 ) INB = NB - 1
C GET CENTER COORDINATES OF BLOCK
      CALL XYTAG(INB,XTAG,YTAG,SLOTAG)
      ADX = FDX
      IF ( NB .GT. NTAGK .AND. LASTVW .EQ. 12 ) ADX = -FDX
      IF ( LASTVW .EQ. 13 ) ADX = FDX - 1100.0
      IF ( NB .GT. NTAGK .AND. LASTVW .EQ. 13 ) ADX = FDX + 1100.0
C  OBS THAT COORDINATES FOR X IN 1979-82 ALREADY HAVE REVERSED SIGN, FOR
C   DISPLAY IN POSITIVE SYSTEM
      X1 = -XTAG + ADX - 0.40 * FENDC
      Y1 =  YTAG + ADY - 0.40 * FENDC
      IPH = CATAG(NB)
      CALL PHNUMB(IPH,SH3,SLOTAG)
333   CONTINUE
C
  300 CONTINUE
      RETURN
 999  CONTINUE
      WRITE(6,6712)
6712  FORMAT('   ERROR RETURN FROM FWCHEV ')
      RETURN
      END
