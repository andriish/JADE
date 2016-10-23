C   07/12/83 403161118  MEMBER NAME  MULDSP   (JADEMUS)     FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE MULDSP
C-----------------------------------------------------------------------
C
C LAST CHANGE 22.15 15/03/84 CHRIS BOWDERY- TIDYING UP
C      CHANGE 14.00 15/03/84 PAUL HILL    - DRAW TYPE 9 MUONS ONLY
C      CHANGE 14.00 26/01/84 PAUL HILL    - DRAW TYPE 5 MUONS ONLY
C      CHANGE 14.00  7/12/83 CHRIS BOWDERY- NEW MUCUTS CRITERIA
C      CHANGE 14.00  8/11/83 CHRIS BOWDERY- TO CONFORM WITH MUCUTS
C      CHANGE 19.50 20/10/81 CHRIS BOWDERY- TO CORRECT A CDTL 36 BUG
C      CHANGE 17.41 28/09/81 CHRIS BOWDERY- COMMENT OUT PHIL 1 DISPLAY
C      CHANGE 13.13 24/09/81 CHRIS BOWDERY- MAJOR CHANGES(HITS MARKED..)
C NEW VERSION 20.30  7/07/81 CHRIS BOWDERY- SINGLE TRACK DISPLAY
C
C        THIS ROUTINE DISPLAYS MUON TRACKS AND 'ELLIPSES' ON SCREEN.
C        'INDEX'= TRAILING NUMBER WITH 'MUPT' COMMAND
C
C        IF 'INDEX' = 0    ALL TRACKS ARE DRAWN.
C        IF 'INDEX' < 0    A RE-ANALYSIS OCCURS AND ALL TRACKS ARE DRAWN
C        IF 'INDEX' > 0    ONLY TRACK NUMBER 'INDEX' IS DRAWN UNLESS ...
C
C        IF 'INDEX' = 100  ALL TRACKS WITH MUQUALITY > 0 AND < 100 DRAWN
C        IF 'INDEX' = 200  ALL TRACKS WITH MUQUALITY > 100  DRAWN
C        IF 'INDEX' = 300  ALL TRACKS WITH MUQUALITY = 0  DRAWN IF THEY
C                          ARE ASSOCIATED WITH A HIT OUTSIDE THE YOKE.
C        IF 'INDEX' = 1000 TRACKS WITH MUCUTS FLAG GT 0  ARE DRAWN
C        IF 'INDEX' = 5000 TRACKS WITH MUCUTS FLAG GE 5  ARE DRAWN
C        IF 'INDEX' = 9000 TRACKS WITH MUCUTS FLAG GE 9  ARE DRAWN
C
C        INDEX IS FOUND FROM ACMD IN THE COMMON CGRAPH
C        FL18 = .TRUE. IF THE SIDE PROJECTIONS ARE BEING DONE. MESSAGES
C                          WILL BE INHIBITED IN THIS CASE. (CDTL 17 ON)
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL VALID,VALIL,LOST1,END,DSPDTM,M100,M200,M300,M1000,NODRAW
      LOGICAL M5000,M9000
      LOGICAL XYVIEW,ZXVIEW,ZYVIEW,GOODMU,MUOK
      LOGICAL FL18,FL22,FL24
C
#include "cgraph.for"
#include "cdata.for"
C
      COMMON / CPROJ  / XMINR,XMAXR,YMINR,YMAXR,IPRJC,FL18,FL22,FL24
      COMMON / CGRAP2 / BCMD,DSPDTM(30)
      COMMON / CMUGRA / IMR112,IMR122,IMUR20,IMUR21,NTRKS,NWHIT,
     +                  NTPH,NWMUR,IMR222,IMR232,NHITS,MUOK
C
      DIMENSION HC(9),HAMB(2),HAMBA(2),XX(2),YY(2)
C
C------------------  C O D E  ------------------------------------------
C
      INDEX = ACMD
      MUOK  = .TRUE.
C
C                       PICK  UP  POINTERS  TO  THE  BANKS  USING  BOS
C                       FUNCTION IBLN
C
      IPMU  = IBLN('MUEV')
      IPATR = IBLN('PATR')
C
C                       RETURN IF NO RAW MUON DATA BANK 'MUEV'  EXISTS
C                       ELSE  RE-ANALYSE  THE  MUON  DATA  IF   (MUPT)
C                      'INDEX'  NEGATIVE.    THIS   ENTAILS   DELETING
C                       EXISTING MUON BANKS (IF ANY).
C
      IF( IDATA(IPMU) .LT. 1 ) GO TO 40
      IF( INDEX .GE. 0  .OR.  FL18 ) GO TO 2
C
C                       IF ANY MU BANKS EXIST, DELETE THEM
C
      CALL BMLT(2,'MUR1MUR2')
      CALL BDLM
C
C                       IF NO 'PATR' BANK, SET  FLAG  ==>  NO  PHIL  2
C                       ANALYSIS
C
 3    IMUARG = 1
      IF( IDATA(IPATR) .LT. 1 ) IMUARG = 0
      CALL MUANA(IMUARG)
C
C                       IF NOT  APPROPRIATE  VIEW  OR  IF  CDTL  9  IS
C                       OFF,  DRAW NOTHING
C
 2    IF( LASTVW .GT. 11 ) RETURN
      IF( LASTVW .LE.  3  .OR.  .NOT. DSPDTL(9) ) GO TO 5
        IF( FL18 ) RETURN
        CALL TRMOUT(80,'PLEASE REDRAW WITH PROJECT MODE - CDTL 9^')
        RETURN
C
C                       IF NO 'MUR1' BANKS , RETURN
C                       IF NO PHIL 1 RESULTS IN 'MUR1'  BANKS,  GO  TO
C                       PHIL 2 RESULTS
C
 5    CALL CLOC( IMUR10, 'MUR1', 0 )
      IF( IMUR10 .GT. 0 ) GO TO 7
 6      IF( INDEX .LT. 0 ) GO TO 40
        IF(.NOT. FL18) CALL TRMOUT(80,'NO RESULTS. PLEASE USE MUPT -1^')
        RETURN
 40     IF(.NOT. FL18) CALL TRMOUT(80,'NO MUON HITS^')
        RETURN
C
 7    CALL CLOC( IMUR11, 'MUR1', 1 )
      IF( IMUR11 .LT. 1 ) GO TO 40
      IMR112 = IMUR11*2
      CALL CLOC( IMUR12, 'MUR1', 2 )
      IF( IMUR12 .LT. 1 ) GO TO 6
      IMR122 = IMUR12 * 2
C
C                       DEFINE NWHIT AND NHITS FOR PHIL 2
C
      NHITS  = IDATA( IMUR10 + 1 )
      NWHIT  = IDATA( IMUR10 + 3 )
C
C-----------------------------------------------------------------------
C             IF NO 'MUR2' BANKS OR IF THEY ARE EMPTY, RETURN
C-----------------------------------------------------------------------
C
      CALL CLOC( IMUR20, 'MUR2', 0 )
      IF(IMUR20 .LE. 0 ) GO TO 6
      NTRKS  = IDATA( IMUR20 + 1 )
      IF(NTRKS .LE. 0 )  RETURN
C
C                       GET NO. OF WORDS PER TRACK/ NO. OF  WORDS  PER
C                       HIT ETC.
C
      NWMUR  = IDATA( IMUR20 + 2 )
      NTPH   = IDATA( IMUR20 + 3 )
      NPL    = IDATA( IMUR20 + 4 )
C
C                       GET POINTERS TO 'MUR2' BANKS 1,2,3,4 AND 5
C
      CALL CLOC( IMUR21, 'MUR2', 1 )
      CALL CLOC( IMUR22, 'MUR2', 2 )
      CALL CLOC( IMUR23, 'MUR2', 3 )
      CALL CLOC( IMUR24, 'MUR2', 4 )
      CALL CLOC( IMUR25, 'MUR2', 5 )
C
      IF( IMUR21 .LE. 0     .OR.
     +    IMUR22 .LE. 0     .OR.
     +    IMUR23 .LE. 0     .OR.
     +    IMUR24 .LE. 0     .OR.
     +    IMUR25 .LE. 0            )  GO TO 6
C
C                       HALF WORD POINTERS FOR S/R MUHIT (SEE  COMMON)
C
      IMR222 = IMUR22 * 2
      IMR232 = IMUR23 * 2
C
C-----------------------------------------------------------------------
C             ***  PHILOSOPHY 2 MUON ANALYSIS RESULTS DISPLAY  **
C-----------------------------------------------------------------------
C
      XYVIEW = LASTVW .EQ.  3
      ZXVIEW = LASTVW .EQ.  6  .OR.  LASTVW .EQ.  7
      ZYVIEW = LASTVW .EQ. 10  .OR.  LASTVW .EQ. 11
C
C                       REMEMBER SPECIAL INDEX VALUES
C
      M100   = INDEX .EQ. 100
      M200   = INDEX .EQ. 200
      M300   = INDEX .EQ. 300
      M1000  = INDEX .EQ. 1000
      M5000  = INDEX .EQ. 5000
      M9000  = INDEX .EQ. 9000
C
C                       DEFINE RANGE OF DO LOOP (ALL TRACKS)
C
      ITRKL  = 1
      ITRKH  = NTRKS
C
C                       SET DO LOOP TO DRAW ONLY TRACK #INDEX IF INDEX>000021400
C                       OR ALLOW THE SPECIAL VALUES OF INDEX
C
      IF( M100 .OR. M200 .OR. M300 .OR. M1000 .OR. M5000 .OR. M9000
     + .OR.  INDEX .LE. 0 ) GO TO 20
C
C                       IF NOT SPECIAL INDEX , RETURN IF TOO HIGH
C
        IF( INDEX .GT. NTRKS ) RETURN
        ITRKL  = INDEX
        ITRKH  = INDEX
C
C                       LOOP OVER ALL/REQUESTED TRACKS
C
 20   DO  200  ITRK = ITRKL,ITRKH
        IPL    = 3*NPL*(ITRK-1)
        IPMU   = NWMUR*(ITRK-1)
C
C                            GET TRACK MU QUALITY AND MUCUTS FLAG
C
        MUQUAL = IDATA(IMUR21+IPMU+6)
        MUFLAG = HDATA(IMUR21*2+IPMU*2+53)
        GOODMU = MUQUAL .GT. 0  .AND.  MUQUAL .LT. 100
C
C                            INFORM SCANNER OF GOOD MUONS
C                            BUT NOT FOR SINGLE TRACK DISPLAY
C
        IF( MUFLAG .LE.   0                        ) GO TO 22
        IF( INDEX  .EQ. 200  .OR.   INDEX .EQ. 300 ) GO TO 22
        IF( MUFLAG .LT. INDEX/1000  .OR.  FL18     ) GO TO 22
          WRITE(JUSCRN,21) ITRK,MUFLAG
  21      FORMAT(' TRACK ',I2,' IS A TYPE ',I2,' GOOD MUON')
C
C                            TEST MU QUALITY IF SPECIAL INDEX
C
  22    NODRAW = M100 .AND. .NOT.( GOODMU )
     +           .OR.            ( M200   .AND. MUQUAL .LT. 100 )
     +           .OR.            ( M300   .AND. MUQUAL .NE. 0   )
     +           .OR.            ( M1000  .AND. MUFLAG .LE. 0   )
     +           .OR.            ( M5000  .AND. MUFLAG .LE. 4   )
     +           .OR.            ( M9000  .AND. MUFLAG .LE. 8   )
        IF( MUQUAL .LT. 0  .OR.  NODRAW ) GO TO 200
C
C                       GET TRACK INFORMATION
C                            NAHITS=NUMBER OF ASSOC. HITS
C                            NLOYWH=NO. OF LAYERS OUTSIDE YOKE WITH
C                                              WITH ASSOCIATED HITS
C
          INFO   = IDATA(IMUR21 + 4 +IPMU)
          NAHITS = MOD(INFO,100)
          NLOYWH = MOD(INFO,10000) / 200
          IF( NLOYWH .EQ. 0  .AND.  M300 ) GO TO 200
          IF( NAHITS .EQ. 0 ) GO TO 10
C
C                       ICON IS A RUNNING  VARIABLE  TO  COMPARE  WITH
C                       NAHITS
C
          ICON   = 1
C
C                       LOOK THROUGH MUR2/2 LOOKING FOR A  MENTION  OF
C                       TRACK ITRK
C
          DO  8  IHIT = 1,NHITS
            DO  9  I = 1,NTPH
              JTRK = HDATA(IMR222 + (IHIT-1)*NTPH + I)
              JTRK = IABS(JTRK)
C
C                       IF THERE  WERE  NO  TRACKS  ASSOC.  WITH  THIS
C                       HIT, SKIP TO THE END OF THE LOOP.
C
              IF( JTRK .EQ. 0 ) GO TO 8
C
C                       IS THIS TRACK THE ONE WE WANT ? SKIP IF NOT
C
              IF( JTRK .NE. ITRK ) GO TO 9
C
C                       GET HIT INFORMATION USING S/R MUHIT
C                       (POSITION,HIT VALIDITY,ASSOCIATION CODES)
C
              CALL MUHIT(HC,IHIT,ITRK,LOST1,VALID,VALIL,HAMB,END)
C
C
C-----------------------------------------------------------------------
C  NOTE ORIENTATION OF THIS CHAMBER (FROM HC(2) = 10*LAYER+ORIENTATION)
C      IOR          ORIENTATION
C      ---     ----------------------------
C       1      NORMAL TO DRIFT PLANE IS PARALLEL TO THE X-AXIS
C       2        "    "   "     "    "     "     "   "  Y-AXIS
C       3        "    "   "     "    "     "     "   "  Z-AXIS
C-----------------------------------------------------------------------
C
              IC  = HC(2)
              IOR = MOD(IC,10)
C
C                       IS CDTL 24 SET ?
C
              IF( DSPDTL(24) )GO TO 11
C
C-----------------------------------------------------------------------
C  CDTL 24 IS NOT SET. THEREFORE , ONLY DRAW HITS AS FOLLOWS :
C    LASTVW=3    (XY) DRAW ONLY HITS ON CHAMBERS WHOSE IOR=1 OR 2
C      "    6,7  (XZ)   "   "    "    "    "       "    "  1 OR 3
C      "    10,11(YZ)   "   "    "    "    "       "    "  2 OR 3
C-----------------------------------------------------------------------
C
              IF( XYVIEW  .AND.  IOR .NE. 3 ) GO TO 11
              IF( ZXVIEW  .AND.  IOR .NE. 2 ) GO TO 11
              IF( ZYVIEW  .AND.  IOR .NE. 1 ) GO TO 11
              GO TO 12
C
C                       GET BOTH AMBIGUITIES
C
  11          DO  30  J = 1,2
                IX   = 3 * (J - 1) + 3
                IY   = IX + 1
                IZ   = IX + 2
                XCOR = HC(IX)
                YCOR = HC(IY)
                ZCOR = HC(IZ)
C
C                       CONVERT 3 D SPACE POINT TO 2 D DISPLAY
C                       COORDINATES IN THE X,Y VIRTUAL SPACE.
C
                CALL XYMUD(XCOR,YCOR,ZCOR,XX(J),YY(J))
  30          CONTINUE
C
C                       DRAW BOTH AMBIGUITIES
C
              DO  13  J = 1,2
C
C                       IF CDTL 36 IS ON, DON'T DRAW THE UNPHYSICAL
C                            AMBIGUITY WHERE THIS HAS BEEN RESOLVED.
C
                IF( .NOT. DSPDTM(6) ) GO TO 31
C
C                            IF AMBIGUITY HAS BEEN REJECTED IGNORE IT
C
                IF( HAMB(3-J) .GE. 2  .AND.  HAMB(J) .LT. HAMB(3-J)
     +                                .AND.  HAMB(J) .LT. 3 ) GO TO 13
C
C                            HAS THIS AMBIGUITY BEEN DRAWN IN MUHDSP ?
C
                IF( HAMB(J) .GT. 2 ) GO TO 31
                CALL MUHIT(HC,IHIT,0,LOST1,VALID,VALIL,HAMBA,END)
C
C                            IF NOT DO NOT DRAW A HIT SYMBOL
C
                IF( HAMBA(J) .LT. HAMBA(3-J)  .AND.  HAMBA(J) .LT. 3
     +                   .AND.  HAMBA(3-J) .GE. 2 ) GO TO 13
C
C                            IF CDTL 37 IS ON , DO NOT DRAW ANY SYMBOL
C                            FOR AMBIGUITY CODES LESS THAN 3
C
 31             IF( DSPDTM(7)  .AND.  HAMB(J) .LT. 3 ) GO TO 13
C
C                            WHEN BOTH AMBIGUITIES ARE AT SAME PLACE
C                            DON'T DRAW 2 DIFFERENT SYMBOLS
C
                X = XX(J)
                Y = YY(J)
                IF( X .EQ. XX(3-J)  .AND.  Y .EQ. YY(3-J)  .AND.
     +                HAMB(J) .LT. HAMB(3-J) ) GO TO 13
C
C                       DRAW NOTHING FOR HAMB CODE = 0 NOT  ASSOCIATED
C                       DRAW A  BOX  FOR HAMB CODE = 1  ONLY ASSOCIATED
C                       DRAW A   8   FOR HAMB CODE = 2  MUQUAL = 0
C                       DRAW A  <>   FOR HAMB CODE = 3  MUQUAL > 99
C                       DRAW A   +   FOR HAMB CODE = 4  GOOD MUON HIT
C
                IAMB = HAMB(J)
                IF( IAMB .EQ. 0 ) GO TO 13
                SIZE = 10.
                IF( VALID ) SIZE = 20.
C
C                       DRAW THE ASSOCIATION SYMBOL FOR CODES  1,2,3,4
C                       UNLESS CDTL 8 IS OFF
C
                IF( .NOT. DSPDTL(8)  .AND.  IAMB .NE. 4 ) GO TO 13
                CALL MOVEA(X+SIZE,Y-SIZE)
                GO TO (15,16,17,14) , IAMB
C
C                       A BOX
C
 15               CALL DRAWA(X+SIZE,Y+SIZE)
                  CALL DRAWA(X-SIZE,Y+SIZE)
                  CALL DRAWA(X-SIZE,Y-SIZE)
                  CALL DRAWA(X+SIZE,Y-SIZE)
                  GO TO 13
C
C                       A VERTICAL DIAMOND PAIR 8
C
 16               CALL DRAWA(X     ,Y - 1.5*SIZE)
                  CALL DRAWA(X-SIZE,Y -     SIZE)
                  CALL MOVEA(X+SIZE,Y +     SIZE)
                  CALL DRAWA(X     ,Y + 1.5*SIZE)
                  CALL DRAWA(X-SIZE,Y +     SIZE)
                  GO TO 13
C
C                       A HORIZONTAL DIAMOND PAIR <X>
C
 17               CALL DRAWA(X + 1.5*SIZE,Y     )
                  CALL DRAWA(X +     SIZE,Y+SIZE)
                  CALL MOVEA(X -     SIZE,Y-SIZE)
                  CALL DRAWA(X - 1.5*SIZE,Y     )
                  CALL DRAWA(X -     SIZE,Y+SIZE)
                  GO TO 13
C
C                       A + ==> STAR (*)
C
 14               CALL MOVEA(X,Y-SIZE)
                  CALL DRAWA(X,Y+SIZE)
                  CALL MOVEA(X-SIZE,Y)
                  CALL DRAWA(X+SIZE,Y)
 13             CONTINUE
C
C                       INCREMENT NUMBER OF FOUND HITS
C
 12             ICON = ICON + 1
                IF( ICON .GT. NAHITS ) GO TO 10
                GO TO 8
  9           CONTINUE
 8          CONTINUE
C
C                       NOW DRAW THE  TRACK  LINES  USING  INFORMATION
C                       FROM MUR2/4
C
 10       JPL = IPL
          DO  201  J = 1,NPL
            XCOR = ADATA(IMUR24+JPL+1)
            YCOR = ADATA(IMUR24+JPL+2)
            IF( XCOR .EQ. 0.0  .AND.  YCOR .EQ. 0.0 )GO TO 210
              ZCOR = ADATA(IMUR24+JPL+3)
              CALL XYMUD(XCOR,YCOR,ZCOR,X,Y)
              IF( J .EQ. 1                  ) CALL MOVEA(X,Y)
              IF( J .NE. 1  .AND.  J .NE. 5 ) CALL DRAWA(X,Y)
              IF( J .EQ. 5                  ) CALL DASHA(X,Y,45)
 210        JPL  = JPL + 3
 201      CONTINUE
C
C                       DRAW TRACK NUMBER IF CDTL 38 IS ON
C                       TYPE 5 MUONS ARE UNDERLINED
C                       TYPE 9 MUONS ARE OVERLINED
C
      IF( DSPDTM(8) ) CALL DNUM(ITRK,X+SIGN(80.,X),Y+SIGN(80.,Y),80.,0.)
      IF( .NOT. DSPDTM(8) ) GO TO 200
      IF( (MUFLAG .NE. 5) .AND. (MUFLAG .NE. 9) )   GO TO 200
      IF( MUFLAG .EQ. 5) TYPSIZ = -20.0
      IF( MUFLAG .EQ. 9) TYPSIZ = 100.0
C
      CALL MOVEA(X+SIGN(80.,X),Y+SIGN(80.,Y)+TYPSIZ)
C
C                        FOR ITRK > 9 UNDERLINING IS TWICE AS LONG
C
      IF( ITRK .GT. 9 ) GO TO 112
      CALL DRAWA(X+SIGN(80.,X)+50.,Y+SIGN(80.,Y)+TYPSIZ)
      GO TO 200
 112  CALL DRAWA(X+SIGN(80.,X)+120.,Y+SIGN(80.,Y)+TYPSIZ)
C
 200  CONTINUE
C
C                       NOW FOR MULTIPLE SCATTERING ELLIPSES
C
      NELIPS = IDATA(IMUR25)
      IF( NELIPS .EQ. 0 ) RETURN
C
C                       CONVERT TO 'INTEGER*2 POINTER'
C
      IMR252 = 2*IMUR25
      I2     = 0
C
      DO  300  I = 1,NELIPS,7
        IOR  = HDATA(IMR252+I2+8)
C
C                       FIND THE 7 TH HALF  WORD  (TRACK  NUMBER)  FOR
C                       THIS ELLIPSE
C
        ITRK = HDATA(IMR252+(I-1)*2+7)
C
C                       DETERMINE IF THIS TRACK  WAS  DRAWN.  IF  NOT,
C                       FORGET ELLIPSES
C
        IF(INDEX.GT.0 .AND. ITRK.NE.INDEX .AND. INDEX.LT.100) GO TO 399
        IPMU   = NWMUR*(ITRK-1)
        MUQUAL = IDATA(IMUR21+IPMU+6)
        MUFLAG = HDATA(IMUR21*2+IPMU*2+53)
        GOODMU = MUQUAL.GT.0 .AND. MUQUAL.LT.100
C
        NODRAW = M100 .AND. .NOT.( GOODMU )
     +           .OR.            ( M200   .AND. MUQUAL .LT. 100 )
     +           .OR.            ( M300   .AND. MUQUAL .NE. 0   )
     +           .OR.            ( M1000  .AND. MUFLAG .LE. 0   )
     +           .OR.            ( M5000  .AND. MUFLAG .LE. 4   )
     +           .OR.            ( M9000  .AND. MUFLAG .LE. 8   )
C
C                  GET TRACK INFORMATION : NLOYWH=NO. OF LAYERS OUTSIDE
C                                            YOKE WITH ASSOCIATED HITS
C
          INFO   = IDATA(IMUR21 + 4 +IPMU)
          NLOYWH = MOD(INFO,10000) / 200
          IF(NLOYWH.EQ.0 .AND. M300 .OR. NODRAW) GO TO 399
C
C                       IF CDTL 24 SET, THEN DRAW ELLIPSES
C
        IF(DSPDTL(24))GO TO 301
C
C-----------------------------------------------------------------------
C            IF NOT DRAW ELLIPSE PROJECTIONS ONLY
C IF LASTVW=3    (XY) DRAW ONLY  ON CHAMBERS WHOSE IOR=1 OR 2
C      "    6,7  (XZ)   "   "     "    "       "    "  1 OR 3
C      "    10,11(YZ)   "   "     "    "       "    "  2 OR 3
C-----------------------------------------------------------------------
C
        IF( XYVIEW  .AND.  IOR .NE. 3 ) GO TO 301
        IF( ZXVIEW  .AND.  IOR .NE. 2 ) GO TO 301
        IF( ZYVIEW  .AND.  IOR .NE. 1 ) GO TO 301
        GO TO 399
C
 301    XC  = ADATA(IMUR25+I  )
        YC  = ADATA(IMUR25+I+1)
        ZC  = ADATA(IMUR25+I+2)
        VT  = ADATA(IMUR25+I+4)
        VL  = ADATA(IMUR25+I+5)
        CTL = ADATA(IMUR25+I+6)
        IF( ABS(VT-VL) .LT. 1E-6 ) GO TO 305
          PHI = 0.5 * ATAN( 2 * CTL / (VT - VL) )
          GO TO 306
 305    PHI    = 3.14159/4.
 306    COSPHI = COS(PHI)
        SINPHI = SIN(PHI)
        V1     = VT*COSPHI**2 + VL*SINPHI**2 + 2*CTL*SINPHI*COSPHI
        V2     = VT*SINPHI**2 + VL*COSPHI**2 - 2*CTL*SINPHI*COSPHI
        SD1    = SQRT(V1)
        SD2    = SQRT(V2)
C
        DO  307  J = 1,37
          T  = 3.14159*J/18.
          C1 = SD1*COS(T)
          C2 = SD2*SIN(T)
          CT = C1*COSPHI - C2*SINPHI
          CL = C1*SINPHI + C2*COSPHI
          IF( IOR .EQ. 3 ) GO TO 303
            ZCOR = ZC + CL
            IF( IOR .EQ. 2 ) GO TO 302
              XCOR = XC
              YCOR = YC + CT
              GO TO 308
 302        XCOR = XC + CT
            YCOR = YC
            GO TO 308
 303      XCOR = XC + CT
          YCOR = YC + CL
          ZCOR = ZC
 308      CALL XYMUD(XCOR,YCOR,ZCOR,X,Y)
          IF( J .EQ. 1 ) CALL MOVEA(X,Y)
          IF( J .NE. 1 ) CALL DRAWA(X,Y)
 307    CONTINUE
C
 399  I2 = I2 + 14
C
 300  CONTINUE
C-----------------------------------------------------------------------
      IF( INDEX .GE. 0  .OR.  FL18 ) RETURN
        CALL TRMOUT(80,'YOU MAY HAVE TO REDRAW^')
      RETURN
      END
