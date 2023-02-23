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
#include "cdata.for"
#include "cgraph.for"
#include "chsym.for"
#include "cgeo1.for"
#include "cgeo2.for"
#include "cgeo3.for"
#include "cwktag.for"
C
      COMMON / CJTRIG / PI,TWOPI,PIHALF,PI3HALF
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
