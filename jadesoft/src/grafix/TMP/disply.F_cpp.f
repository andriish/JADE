C   16/03/84 810171936  MEMBER NAME  DISPLY   (S)        M  FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE DISPLY
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. OLSSON  PREHISTORY:  DISPLAY ROUTINE. GRAPHICS ONLY
C
C        MOD: J. OLSSON   15/12/83 :  COMMANDS VRX,VRZX,VRZY
C        MOD: P. HILL     17/02/84 :  COMMAND CDST IN. SOME CODE OUT.
C        MOD: C. BOWDERY  16/03/84 :  COMMAND ND50 ADDED TO LIST.
C        MOD: C. BOWDERY  15/06/84 :  PARTIAL RE-WRITE DONE.
C        MOD: C. BOWDERY  20/06/84 :  CHANGES TO REDRAW DET AFTER JOYS
C        MOD: C. BOWDERY  20/12/85 :  LASER PRINTER HARDCOPY
C        MOD: J. HAGEMANN 06/03/86 :  SWITCH VTXC BANKS, PICK COORDI-
C                                     NATES FROM SCREEN .
C                                     DSPDTL(9) NOW SET CORRECTLY
C                                     FOR COMMAND TRUE
C        MOD: J. OLSSON   02/08/86 :  REPAIR LOGIC ERROR IN NNJETC
C        MOD: J. OLSSON   16/04/87 :  VARIOUS DESTINATIONS IN HLP/LASER
C        MOD: J. HAGEMANN 25/07/88 :  NEW COMMANDS ADDED
C                                     (EDVP,DAX,VCDO,DIST,MUTYP)
C   LAST MOD: J. HAGEMANN 17/10/88 :  For TR command
C
C     DISPLAY ROUTINE FOR GRAPHICS COMMAND THAT DO NOT CHANGE CWORK
C
C
C     ISTVW      = 1  FOR STANDARD VIEWS,  0 OTHERWISE
C     ISTVW      IS SET TO 1 BY COMMAND STVW AND PASSING SCANNR DISPLAY
C     JTVW       HOLDS LAST LSTCMD VALUE
C     IVIEW      IS CURRENT VIEW, 1-20
C
C      FL18      IS TRUE IF PROJECTION
C      FL22      IS TRUE IF MAGNIFICATION
C      FL24      IS TRUE IF STANDARD VIEW IS MAGNIFIED
C
C      NNPATR IS USED TO GIVE PATR BANK NR, FOR VARIOUS VERTEX COMMANDS
C      NNJETC IS USED TO GIVE JETC BANK NR, FOR VARIOUS DISPLAY COMMANDS
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL DETDRW,REDRDT,DSPREM
      LOGICAL FL18,FL22,FL24,DSPDTM
      LOGICAL LPRINT,FLVCDO
C
      REAL*8 DESTLP
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
C
      COMMON / CGAMOD / MODEGA, GAHEX
      COMMON / CPROJ  / XMINR,XMAXR,YMINR,YMAXR,IPRJC,FL18,FL22,FL24
      COMMON / CGRAP2 / BCMD,DSPDTM(30),ISTVW,JTVW
      COMMON / CGRAP6 / DETDRW
      COMMON / CHEADR / HEAD(108)
      COMMON / CVX    / NNPATR,NDUMM,NNJETC,NNVTXC
      COMMON / CGVCDO / FLVCDO(20)
C
      DIMENSION DESTLP(5), IARRAY(2,10)
C
      DATA DESTLP  / 'L1      ','L2      ','L3      ','L4      ',
     *               'L5      '/
      REAL*8 DESTIN  / 'PLOTTER ' /
      REAL*8 DESTEX  / 'EXTPLOTT' /
C
      DATA  HDCL /0/
C
C------------------  C O D E  ------------------------------------------
C
      LAST   = LASTVW
      REDRDT = .FALSE.
C
C                            GET ARGS (INTEGER AND FLOATING POINT FORMS)
C
      A  = ACMD
      AB = BCMD
      N  = A
      NB = AB
C
C                            ICOMD1 IS THE '10' DIGIT OF THE COMMAND
C                            ICOMD2 IS THE '1' DIGIT OF THE COMMAND
C
      ICOMD1 = ( LSTCMD - 1 ) / 10  +  1
      ICOMD2 = MOD( LSTCMD, 10 )
      IF( ICOMD2 .EQ. 0 ) ICOMD2 = 10
C
C                            BRANCH TO APPROPRIATE COMMAND
C                            N.B. COMMANDS 1 - 30 ARE VIEWS
C
      GO TO ( 1, 1, 1, 3140, 4150, 5160, 6170, 7180 ) , ICOMD1
C
  99  CALL TRMOUT(80,'Software Error!  Please contact graphics expert^')
      CALL TRMOUT(80,'Command interpreter overflow in DISPLY^')
      RETURN
C
 3140 GO TO (31,32,33,34,35,36,37,38,39,40) , ICOMD2
 4150 GO TO (41,42,43,44,45,46,47,48,49,50) , ICOMD2
 5160 GO TO (51,52,53,54,55,56,57,58,59,60) , ICOMD2
 6170 GO TO (61,62,63,64,65,66,67,68,99,99) , ICOMD2
 7180 GO TO (99,99,99,99,99,99,99,99,99,99) , ICOMD2
C
C
C                            COMMANDS FOR ALL VIEWS
C                            ( COMMANDS 1 - 30 BUT NOT ALL USED.)
C
   1  IVIEW  = LSTCMD
      IF( IVIEW .NE. 30 ) GO TO 2
C
C                            DRAW STANDARD VIEW
C
        ISTVW = 1
        IVIEW = ISTANV
        CALL SETSCL(0)
        FL22  = FL24
        GO TO 4
C
   2  IF( (LAST.EQ.IVIEW)  .AND.  (JTVW.NE.30) .AND.
     *    (IVIEW.NE.17.OR.FL22) ) GO TO 3
        CALL SETSCL(IVIEW)
        FL22 = .FALSE.
   3  ISTVW  = 0
C
   4  LAST   = IVIEW
      JTVW   = LSTCMD
      DSPREM = DSPDTL(9)
      IF( IVIEW .EQ.  6  .OR.  IVIEW .EQ.  7  .OR.
     +    IVIEW .EQ. 10  .OR.  IVIEW .EQ. 11     ) DSPDTL(9) = .FALSE.
C
      CALL ERASE
      CALL EVDISP(IVIEW)
C
C                                      DRAW DETECTOR IF REQUESTED
C
      IF(  N .NE. 0  .OR.  REDRDT ) then
         call setcol('JADE')    ! PMF 23/11/99: set colour
         CALL JADISP(IVIEW)
         call setcol(' ')    ! PMF 23/11/99: reset colour
      endif
C
C                                      DISPLAY RESULTS IF CDTL 16 SET
C
      IF( DSPDTL(16) ) CALL RSDISP(IVIEW)
      DSPDTL(9) = DSPREM
      IF( DSPDTL(17) ) CALL PROJEC(LAST)
      IF( IVIEW.EQ.17.AND.FL22 ) FL22 = .FALSE.
      RETURN
C
C                            OTHER GRAPHICS ONLY COMMANDS
C
C
C             COMMAND TRUE   (DRAW TRUE TRACKS FOR MC EVENTS)
C
  31  DSPREM = DSPDTL(9)
      IF( LASTVW .EQ.  6  .OR.  LASTVW .EQ.  7  .OR.
     +    LASTVW .EQ. 10  .OR.  LASTVW .EQ. 11     ) DSPDTL(9) = .FALSE.
      call setcol('TRUE') ! PMF 22/11/99: set colour
      CALL TRUTH(N)
      call setcol(' ') ! PMF 22/11/99: reset colour
      DSPDTL(9) = DSPREM
      RETURN
C
C             COMMAND   BW   (PRINT OUT BANK CONTENTS)
C
  32  CALL BANKPR
      RETURN
C
C             COMMAND    H   (INTERNAL PLOTTER HARDCOPY)
C
  33  HDCL = HDCL + 1
      IF( (HDCL/5)*5 .EQ. HDCL) CALL
     +  TRMOUT(80,'Do you know that hardcopies cost  DM 1 /metre?^')
      IF( N .LT. 1) N = 1
      IF( N .GT. 4) N = 4
C
      DO  331  I = 1,N
        CALL HDCDST(DESTIN)
 331  CONTINUE
      RETURN
C
C             COMMAND HX     (EXTERNAL PLOTTER HARDCOPY)
C
  34  CALL HDCDST(DESTEX)
      RETURN
C
C             COMMAND MENU   (DISPLAY LIST OF COMMANDS)
C
  35  CALL MENU
      RETURN
C
C             COMMAND   RS   (RESET PICTURE SCALE)
C
  36  IF( .NOT. FL22 ) RETURN
      CALL SETSCL(LAST)
      FL22 = .FALSE.
      IF( DETDRW ) REDRDT = .TRUE.
C
C                            REDRAW EVENT (DET AS WELL IF DRAWN ALREADY)
C
      LSTCMD = LASTVW
      LAST   = LASTVW
      N      = 0
      ACMD   = 0.0
      JTVW   = 0
      GO TO 1
C
C             COMMAND JOYS   (JOYSTICK INPUT FOR MAGNIFICATION ETC)
C
  37  CALL JOYS(N,A)
      IF( DETDRW ) REDRDT = .TRUE.
C
C                            REDRAW EVENT (DET AS WELL IF DRAWN ALREADY)
C
      LSTCMD = LASTVW
      LAST   = LASTVW
      ACMD   = 0.0
      N      = 0
      JTVW   = 0
      GO TO 1
C
C             COMMAND  DET   (DRAW DETECTOR HARDWARE)
C
  38  continue
      call setcol('JADE') ! PMF 23/11/99: set colour
      CALL JADISP(LAST)
      call setcol(' ') ! PMF 23/11/99: reset colour
      RETURN
C
C             COMMAND  COM   (ADD COMMENT TO THE PICTURE)
C
  39  CALL COMENT
      RETURN
C
C             COMMAND  PRO     (PROJECTIONS OF Z-X AND Z-Y SECTIONS)
C
  40  CALL PROJEC(LAST)
      RETURN
C
C             COMMAND CDTL   (CHANGE DISPLAY DETAILS)
C                            FIRST 30 FLAGS IN DSPDTL, THEN IN DSPDTM
C
  41  IF( N .GE. 1  .AND.  N .LE. 60 ) GO TO 412
 411  CALL CDST(0)
      CALL TRMOUT(80,'Enter option number to be changed:^')
C
      N = TERNUM(DUMMY)
      IF( N .GE. 1  .AND.  N .LE. 60 ) GO TO 412
      CALL TRMOUT(80,'No such option exists. Please try again.^')
      GO TO 411
C
 412  IF( N .LE. 30 ) DSPDTL(N)    = .NOT. DSPDTL(N)
      IF( N .GT. 30 ) DSPDTM(N-30) = .NOT. DSPDTM(N-30)
C
C                            DISPLAY NEW STATUS OF CONTROL-DETAIL N
C                            (SAME AS IF CDST COMMAND WAS GIVEN)
C
C
C             COMMAND  CDST  (DISPLAY CONTROL-DETAIL-FLAGS STATUS)
C                            THE CURRENT (MAY 84) TOTAL IS 44.
C                            IF NO TRAILING INDEX THEN
C                            DISPLAY STATUS OF ALL OPTIONS
C
  42  CALL CDST(N)
      RETURN
C
C             COMMAND TRLG   (DISPLAY LEAD GLASS TRIGGER)
C                            ONLY ACTIVE FOR NEW DATA (1982 - ...)
C
  43  CALL TBGDIS
      RETURN
C
C             COMMAND NEWS   (DISPLAY GRAPHICS NEWS)
C
  44  CALL NEWS
      RETURN
C
C             COMMAND  RES   (DISPLAY ANALYSIS RESULTS IN THIS VIEW)
C
  45  DSPREM = DSPDTL(9)
      IF( LASTVW .EQ.  6  .OR.  LASTVW .EQ.  7  .OR.
     +    LASTVW .EQ. 10  .OR.  LASTVW .EQ. 11     ) DSPDTL(9) = .FALSE.
      CALL RSDISP(LASTVW)
      call setcol(' ') ! PMF 23/11/99: reset colour set in RSDISP
      DSPDTL(9) = DSPREM
      IF( DSPDTL(17) ) CALL PROJEC(LASTVW)
      RETURN
C
C             COMMAND HELP   (DISPLAY DETAILED INFORMATION ON COMMANDS.)
C
  46  CALL HELPUS
      RETURN
C
C             COMMAND TR     (SPECIAL DISPLAY OF TRACK HITS)
C
  47  DSPREM = DSPDTL(9)
      IF( LASTVW .EQ.  6  .OR.  LASTVW .EQ.  7  .OR.
     +    LASTVW .EQ. 10  .OR.  LASTVW .EQ. 11     ) DSPDTL(9) = .FALSE.
      CALL TRKHIT
      DSPDTL(9) = DSPREM
      RETURN
C
C             COMMAND  RET   (RETURN TO EDIT SUBSYTEM)
C
  48  FL22 = FL24
      RETURN
C
C             COMMAND MASS   (PRINT EFFECTIVE MASS OF UP TO 6
C                             GIVEN PARTICLES ON SCREEN)
C
  49  CALL EFMASS
      RETURN
C
C             COMMAND TRG2   (T2 TRIGGER DISPLAY)
C
  50  CALL TRIG2
      RETURN
C
C             COMMAND SAVE   (SAVE /CWORK/ ON DISK)
C
  51  IF( N .NE. 0 ) GO TO 511
      CALL TRMOUT(80,'Please type  1  to save CWORK onto disk^')
      CALL TRMOUT(80,'or  2  to retrieve from disk and reset CWORK.^')
      N = TERNUM(DUMMY)
 511  IF( N .EQ. 1  .OR.  N .EQ. 2 ) GO TO 512
      CALL TRMOUT(80,'No such option. Command ignored.^')
      RETURN
 512  CALL RETTEN(N)
      RETURN
C
C             COMMAND  VRES  (TRACK+VERTEX RESULT DISPLAY)
C                            VRES IS IDENTICAL TO RES IN RU AND RZ VIEWS
C                            OTHER VIEWS, START WITH VX COMMAND
C
  52  IF( LASTVW .EQ. 13  .OR.  LASTVW .EQ. 16 ) GO TO 45
C
C             COMMAND   VX   (VERTEX RESULTS DISPLAY)
C
  53  DSPREM = DSPDTL(9)
      IF( LASTVW .EQ.  6  .OR.  LASTVW .EQ.  7  .OR.
     +    LASTVW .EQ. 10  .OR.  LASTVW .EQ. 11     ) DSPDTL(9) = .FALSE.
      CALL VXDISP(LASTVW)
      DSPDTL(9) = DSPREM
      IF( LSTCMD .EQ. 52  .AND.  DSPDTL(17) ) CALL PROJEC(LASTVW)
      RETURN
C
C             COMMAND   BL   (LG DISPLAY OPTIONS)
C
  54  CALL BLSHOW
      RETURN
C
C             COMMAND MUR2   (MUON TRACK INFORMATION)
C
  55  DSPREM = DSPDTL(9)
      IF( LASTVW .EQ.  6  .OR.  LASTVW .EQ.  7  .OR.
     +    LASTVW .EQ. 10  .OR.  LASTVW .EQ. 11     ) DSPDTL(9) = .FALSE.
      CALL MUR2DS
      DSPDTL(9) = DSPREM
      RETURN
C
C             COMMAND DRAW   (DRAW LINES ETC.)
C
  56  CALL DRAWER
      RETURN
C
C             COMMAND EC     (ADD ENDCAP)
C
  57  IF( LASTVW .LE. 3 ) CALL ECAPS(N)
      RETURN
C
C             COMMAND FC     (ADD TAGGING SYSTEM)
C
  58  IF( LASTVW .LE. 3 ) CALL FWCHEV
      RETURN
C
C             COMMAND PATR   (SWITCH PATR BANKS)
C
  59  NNPATR = N
      IF( N .GE. 0 ) GO TO 592
 591  CALL TRMOUT(80,'Enter PATR bank number:^')
      CALL FYRINT(NNPATR,ID2,ID3,ID4)
 592  IF( NNPATR .LT. 0 ) GO TO 591
      RETURN
C
C             COMMAND JETC   (SWITCH JETC BANKS)
C
  60  NNJETC = N
      IF( N .NE. 0 ) GO TO 602
         CALL TRMOUT(80,'Enter JETC bank number:^')
         CALL FYRINT(NNJETC,ID2,ID3,ID4)
 602  CONTINUE
      RETURN
C
C             COMMAND HLP / LASER   (LASER PRINTER HARDCOPY)
C
  61  CALL PTRANS(335,100)
      NDEST = N
      IF( N .GT. 0 .AND. N .LT. 6 ) GO TO 612
 611  CALL TRMOUT(80,'Enter destination for hardcopy on laser printer (L
     *1 .. L5) : (1 .. 5)^')
      CALL FYRINT(NDEST,ID2,ID3,ID4)
 612  IF( NDEST .LT. 1 .OR. NDEST .GT. 5 ) GO TO 611
      CALL HDCDST(DESTLP(NDEST))
      RETURN
C
C             COMMAND VTXC   (SWITCH VTXC BANKS)
C
  62  NNVTXC = N
      IF( N .GE. 0 .AND. N .LT. 100 ) GO TO 622
 621  CALL TRMOUT(80,'Enter VTXC bank number:^')
      CALL FYRINT(NNVTXC,ID2,ID3,ID4)
 622  IF( NNVTXC .LT. 0 .OR. NNVTXC .GT. 99 ) GO TO 621
      RETURN
C
C             COMMAND PICK   (PICK COORDINATES FROM SCREEN)
C
  63  CALL PICK
      RETURN
C
C             COMMAND EDVP   (EDIT VPAT-BANK)
C
  64  CALL EDVP
      RETURN
C
C             COMMAND DAX   (CALCUTATE AXIS AND DISPLAY IT)
C
  65  CALL DISAXI
      RETURN
C
C             COMMAND VCDO  (VERTEX CHAMBER DISPLAY OPTIONS)
C
  66  IF( N .GE. 1  .AND.  N .LE. 20 ) GO TO 662
 661  CALL VCDO(0)
      CALL TRMOUT(80,'Enter option number to be changed:^')
C
      N = TERNUM(DUMMY)
      IF( N .EQ. 0 ) GO TO 663
      IF( N .GE. 1  .AND.  N .LE. 20 ) GO TO 662
      CALL TRMOUT(80,'No such option exists. Please try again.^')
      GO TO 661
C
 662  FLVCDO(N)    = .NOT. FLVCDO(N)
C
C                            DISPLAY NEW STATUS OF CONTROL-DETAIL N
C
      CALL VCDO(N)
 663  CONTINUE
      RETURN
C
C             COMMAND DIST   (CALCULATE DISTANCE BETWEEN TWO POINTS
C                             GIVEN BY JOYSSTICK INPUT OR ONE POINT
C                             AND A TRACK)
  67  CALL CTDIST
      RETURN
C
C             COMMAND MUTYP (SHOW MUON TYPES)
C
  68  CALL MUONS( NUMBER, IARRAY, IERROR )
      IF( IERROR.NE.0 .OR. NUMBER.LE.0 ) GOTO 685
      DO 684 J = 1, NUMBER
         WRITE(6,683) IARRAY(1,J),IARRAY(2,J)
 683     FORMAT(' TRACK ',I2,' IS A TYPE ',I2,' GOOD MUON')
 684  CONTINUE
 685  CONTINUE
      RETURN
C
      END
