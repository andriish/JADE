C   20/10/81 110201215  MEMBER NAME  MUHDSP   (JADEMUS)     FORTRAN
C   29/03/80 110180058  MEMBER NAME  MUHDSP   (S)           FORTRAN
C   12/02/80            MEMBER NAME  MUHDSP   (JADEMUS)     FORTRAN
C
C NEW VERSION 17.24 18/09/81 CHRIS BOWDERY- TO ONLY DRAW BASIC HITS
C
C-----------------------------------------------------------------------
      SUBROUTINE MUHDSP
C-----------------------------------------------------------------------
C
C        THIS ROUTINE DISPLAYS MUON CHAMBER HITS ON SCREEN AS FOLLOWS:
C
C          LARGE CROSS (X) ==> VALID   DRIFT DIGITISING
C          SMALL CROSS (X) ==> INVALID DRIFT DIGITISING
C
C    CDTL     STATE                  ACTION
C    ----     -----                  ------
C
C      8       ON       BAD CHAMBER Z COORDINATES MARKED WITH A CIRCLE
C  **  9  **       ==>  HITS ARE ALWAYS DRAWN IN PROJECT MODE
C     22       ON       HIT NUMBERS ARE DRAWN AND SCALE AS SQRT(MAGNIF)
C     24       OFF      ONLY 'IN PLANE' HITS ARE DRAWN
C     36       ON       UNPHYSICAL MIRROR HITS NOT DRAWN (WHERE KNOWN)
C     37       ON       DELETE HITS NOT ASSOCIATED WITH QUALITY MUONS
C     38       ON       EXTRAPOLATED TRACK NUMBERS DRAWN
C     39       ON       CHAMBER NUMBER AND RAW HIT NUMBERS DRAWN
C
C-----------------------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
      LOGICAL FL18,FL22,FL24,DSPDTM,VALID,VALIL,LOST1,END,MUDONE,MUOK
      LOGICAL XYVIEW,ZXVIEW,ZYVIEW
C-----------------------------------------------------------------------
C                  COMMONS
C                  -------
C ( XMIN,YMIN ) * ( XMAX,YMAX ) IN COMMON CGRAPH,ARE THE COORDINATES OF
C                               THE CURRENT VIEW WINDOW IN VIRTUAL SPACE
C
C FL22 = .TRUE.                 IF THE CURRENT WINDOW IS DIFFERENT FROM
C                               THE STANDARD WINDOW
C FL18 = .TRUE.                 IF WE ARE DRAWING SIDE PROJECTIONS
C DSPDTL                        LOGICAL ARRAY CONTAINING CDTL'S  1 - 30
C DSPDTM                        LOGICAL ARRAY CONTAINING CDTL'S 31 - 60
C
C-----------------------------------------------------------------------
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
C----------START OF MACRO CMUBCS----------------------------------------
      COMMON /BCS/IDATA(1)
      DIMENSION HDATA(1),ADATA(1)
      EQUIVALENCE (HDATA(1),ADATA(1),IDATA(1))
C----------END OF MACRO CMUBCS------------------------------------------
      COMMON /CPROJ / XMINR,XMAXR,YMINR,YMAXR,IPRJC,FL18,FL22,FL24
      COMMON /CGRAP2/ BCMD,DSPDTM(30)
      COMMON /CMUGRA/ IMR112,IMR122,IMUR20,IMUR21,NIDS,NWHIT,NTPH,
     +                NWTR,IMR222,IMR232,NHITS,MUOK
      DIMENSION HC(9),HAMB(2),OFFSET(8)
      DATA OFFSET/60.,-60.,30.,-30.,50.,-50.,40.,-40./
C-----------------------------------------------------------------------
C                    INITIALISATION
C                    --------------
C
C        CHARSZ = HIT NUMBER CHARACTER SIZE , K ='OFFSET' POINTER
C        LCHAM  = LAST CHAMBER NUMBER , MUDONE = TRUE IF ANALYSIS DONE
C        MUOK   = TRUE IF MUR1(0/1/2) AND MUR2(0/1/2/3) BANKS PRESENT
C-----------------------------------------------------------------------
      MUDONE = .FALSE.
      MUOK   = .FALSE.
      K = 0
      LCHAM  = 0
      CHARSZ = 80.
      IF(.NOT. FL22 .OR. FL18) GO TO 4
C
C                  FIND PRESENT MAGNIFICATION AND SCALE HIT NUMBER SIZE
C
         CURMAG = 10000./(XMAX - XMIN)
         CHARSZ = CHARSZ/(CURMAG**0.25)
         IF(CHARSZ.LT.30.) CHARSZ = 30.

C-----------------------------------------------------------------------
C
C FIND NO. OF HITS AND NO. OF WORDS PER HIT IN COORDINATE BANK.
C                  FIND POINTER TO MUR1 BANKS AFTER INITIALISATION.
C
  4   IHIT = 0
      ITRK = 0
      CALL CLOC(IMUR10,'MUR1',0)
      IF(IMUR10.GT.0) GO TO 11
C
C                  MUR1/0 NOT FOUND - CALL MUANAC TO CREATE IT.
C
      CALL MUANAC
      CALL CLOC(IMUR10,'MUR1',0)
        IF(IMUR10.GT.0) GO TO 11
        IF(FL18) RETURN
        CALL TRMOUT(80,'MUON ANALYSIS ERROR. MUR1 NOT CREATED^')
        RETURN
C
C                  MUR1/0 FOUND. PICK UP NUMBER OF HITS IN BANK
C
 11   NHITS=IDATA(IMUR10+1)
      IF(NHITS.LE.0) RETURN
C
C                  FIND NUMBER OF WORDS PER HIT.
C
      NWHIT=IDATA(IMUR10+3)
C
C                  FIND POINTER TO MU COORDINATE BANK MUR1/1
C                  AND MU HIT STATUS BANK MUR1/2.
C
      IMUR11  = IDATA(IMUR10-1)
      IF(IMUR11.LE.0) GO TO 200
      IMR112  = 2*IMUR11
      IMUR12  = IDATA(IMUR11-1)
      IF(IMUR12.LE.0) GO TO 200
      IMR122  = 2*IMUR12
C
C                  FIND POINTER TO AMBIGUITY BANK MUR2/3
C                  FIRST FIND MUR2 BANKS POINTER
C
      CALL CLOC(IMUR20,'MUR2',0)
C                  IF NO MUR2 BANKS JUMP
      IF(IMUR20.LE.0) GO TO 2
C                  FIND NUMBER OF INNER DETECTOR TRACKS.
      NIDS  = IDATA(IMUR20+1)
C                  FIND NUMBER OF I*4 WORDS PER TRACK IN MUR2/1
      NWTR  = IDATA(IMUR20+2)
C                  FIND NUMBER OF I*4 WORDS PER HIT IN MUR2/2 AND MUR2/300012100
      NTPH  = IDATA(IMUR20+3)
C                  FIND MUR2/1 , MUR2/2  AND  MUR2/3 POINTERS
      IMUR21  = IDATA(IMUR20-1)
      IF(IMUR21.LE.0) GO TO 2
      IMUR22  = IDATA(IMUR21-1)
      IF(IMUR22.LE.0) GO TO 2
      IMUR23  = IDATA(IMUR22-1)
      IF(IMUR23.LE.0) GO TO 2
      MUOK    = .TRUE.
C                  FIND THE OTHER BANKS TO SEE IF PHIL 2 ANALYSIS DONE
      IMUR24  = IDATA(IMUR23-1)
      IF(IMUR24.LE.0) GO TO 25
      IMUR25  = IDATA(IMUR24-1)
      IF(IMUR25.LE.0) GO TO 25
      MUDONE  = .TRUE.
C                  CALCULATE POINTERS TO HALF WORDS IN MUR2/2 AND MUR2/300013700
 25   IMR222 = 2*IMUR22
      IMR232 = 2*IMUR23
C-----------------------------------------------------------------------
C
C             ACCESS THE HIT INFORMATION USING S/R MUHIT
C
C INPUT VARIABLES
C
C IHIT        HIT NUMBER (AS DEFINED BY MUR1/1, THE COORDINATE BANK).
C ITRK        SET TO ZERO HERE.
C
C OUTPUT VARIABLES...
C
C HC          THE OUTPUT - 9 WORDS   (AS IN MUR1/1)
C               WORD  1     -  4*CHAMBER NUMBER + (RAW HIT NUMBER -1)
C               WORD  2     -  10*LAYER NUMBER + ORIENTATION
C               WORDS 3 - 8 -  X,Y,Z(LEFT),X,Y,Z(RIGHT) (MM).
C               WORD  9     -  POINTER TO HIT IN MUEV.
C LOST1       = .TRUE. IF THIS HIT HAS BEEN RECOVERED (HIT 4 PROBLEM)
C VALID       = .TRUE. IF HIT HAS VALID DRIFT COORDINATE
C VALIL       = .TRUE. IF HIT HAS VALID LONG. COORDINATE
C HAMB        AMBIGUITY INFORMATION FLAGS :HAMB(1) ==> LEFT  AMBIGUITY
C                                          HAMB(2) ==> RIGHT AMBIGUITY
C
C             = 4 "AMBIGUITY" ASSOCIATED WITH A MUON OF QUALITY > 0
C                  AND < 100 AND USED IN THE CHI-SQUARED FIT.
C             = 3 "AMBIGUITY" ASSOCIATED WITH A MUON OF QUALITY > 99
C                  AND USED IN THE CHI-SQUARED FIT.
C             = 2 AS 3 BUT THE MUON QUALITY WAS NOT > 0  (PHIL 2.1)
C             = 1 "AMBIGUITY" ASSOCIATED TO A TRACK BUT NOT IN CHI**2
C             = 0 "AMBIGUITY" NOT ASSOCIATED TO ANY TRACK
C
C END         = .TRUE. IF THIS IS LAST HIT , .FALSE. OTHERWISE
C
C-----------------------------------------------------------------------
C
  2   IHIT = IHIT + 1
      CALL MUHIT(HC,IHIT,ITRK,LOST1,VALID,VALIL,HAMB,END)
C
C-----------------------------------------------------------------------
C  FIND ORIENTATION OF THIS CHAMBER , IT'S NUMBER AND THE RAW HIT NUMBER
C      IOR          ORIENTATION
C      ---     ----------------------------
C       1      NORMAL TO DRIFT PLANE IS PARALLEL TO THE X-AXIS
C       2        "    "   "     "    "     "     "   "  Y-AXIS
C       3        "    "   "     "    "     "     "   "  Z-AXIS
C-----------------------------------------------------------------------
      IC    = HC(1)
      ICHAM = IC/4
      JHIT  = MOD(IC,4)  + 1
      IC    = HC(2)
      IOR   = MOD(IC,10)
C-----------------------------------------------------------------------
C                  IS CDTL 24 SET ? (TO SHOW OUT OF PLANE HITS)
C-----------------------------------------------------------------------
      XYVIEW = LASTVW.EQ.3
      ZXVIEW = LASTVW.EQ.6  .OR. LASTVW.EQ.7
      ZYVIEW = LASTVW.EQ.10 .OR. LASTVW.EQ.11
C
      IF(DSPDTL(24)) GO TO 10
C-----------------------------------------------------------------------
C
C  CDTL 24 IS NOT SET. THEREFORE , ONLY DRAW HITS AS FOLLOWS :
C    LASTVW=3    (XY) DRAW ONLY HITS ON CHAMBERS WHOSE IOR=1 OR 2
C      "    6,7  (XZ)   "   "    "    "    "       "    "  1 OR 3
C      "    10,11(YZ)   "   "    "    "    "       "    "  2 OR 3
C
C-----------------------------------------------------------------------
      IF( XYVIEW .AND. IOR.NE.3) GO TO 10
      IF( ZXVIEW .AND. IOR.NE.2) GO TO 10
      IF( ZYVIEW .AND. IOR.NE.1) GO TO 10
C                  IF WE ARE HERE , DRAW NOTHING FOR THIS HIT
      GO TO 3
C                  DEFINE SIZE OF THE CROSS   (SMALL IF BAD HIT)
 10   SIZE=20.
      IF(.NOT.VALID)SIZE=10.
C                  SKIP THIS HIT IF CDTL 37 IS ON AND BOTH AMBIGUITIES
C                  ARE BELOW CODE 3 BUT ONLY IF PHIL 2 HAS BEEN DONE
      IF( DSPDTM(7) .AND. HAMB(1).LT.3 .AND. HAMB(2).LT.3
     + .AND. MUDONE ) GO TO 3
C                  JUMP IF CDTL 39 OFF OR IF DRAWING SIDE PROJECTIONS
      IF(.NOT. DSPDTM(9) .OR. FL18 ) GO TO 5
C
C                  FIND THE NUMBER OF CHARACTERS I TO DRAW CHAMBER NO.
C
         ICH = ICHAM
         DO 6 I=1,5
           ICH = ICH/10
           IF(ICH.EQ.0) GO TO 7
   6     CONTINUE
C
C                  CALCULATE CENTRE OF CHAMBER BY AVERAGING HIT COORDS.
C
   7     XCOR  = ( HC(3) + HC(6) )/2
         YCOR  = ( HC(4) + HC(7) )/2
         ZCOR  = ( HC(5) + HC(8) )/2
C
C                  CONVERT TO 2 D VIRTUAL COORDINATE SPACE
C
         CALL XYMUD(XCOR,YCOR,ZCOR,X,Y)
C
C                  SET X TO LEFT OF CHAMBER CENTRE ALLOWING FOR WIDTH
C                  OF CHAMBER NUMBER. SELECT 1 OF 8 Y POSITIONS
C                  IF THIS HIT IS NOT IN A NEW CHAMBER , SELECT SAME
C                  Y POSITION. N.B. IN Z VIEWS THE HITS MAY BE FAR APART
C
         X = X - FLOAT(I)*CHARSZ/3.5 - 40.
         IF(ICHAM.NE.LCHAM) K = K + 1
         IF(K.GT.8) K = 1
         LCHAM = ICHAM
         Y = Y + OFFSET(K)
         CALL DNUM(ICHAM,X,Y,CHARSZ/2.0,0.)
C
C                  FIND THE NUMBER OF CHARACTERS I TO DRAW RAW HIT NO.
C
      KNUM = IHIT
      DO 8 I=1,5
        KNUM = KNUM/10
        IF(KNUM.EQ.0) GO TO 9
  8   CONTINUE
  9   XOFF = FLOAT(I)*CHARSZ*0.80 + 20.
C                  IF HIT WAS CORRUPTED AND RECOVERED , INDICATE THIS
  5   IF(LOST1) JHIT = 0
C
C                  DRAW BOTH AMBIGUITIES
C
      DO 1 J=1,2
C
C                  IF CDTL 36 IS ON , DO NOT DRAW THE UNPHYSICAL
C                  AMBIGUITY WHERE THIS HAS BEEN RESOLVED.
C
        IF( DSPDTM(6) .AND. HAMB(3-J).GE.2 .AND. HAMB(J).LT.HAMB(3-J)
     +                .AND. HAMB(J).LT.3 ) GO TO 1
C
C                  IF CDTL 37 ON DO NOT DRAW ANY AMBIGUITY WITH CODE
C                  LESS THAN 3 IF PHIL 2 HAS BEEN DONE.
C
        IF( DSPDTM(7) .AND. MUDONE .AND. HAMB(J).LT.3) GO TO 1
        IX=3*(J-1)+3
        IY=IX+1
        IZ=IX+2
        XCOR=HC(IX)
        YCOR=HC(IY)
        ZCOR=HC(IZ)
C
C                  CONVERT 3 D SPACE POINT TO 2 D DISPLAY COORDINATES IN
C                  THE X,Y VIRTUAL SPACE.
C
        CALL XYMUD(XCOR,YCOR,ZCOR,X,Y)
C
C                  DRAW MUR2/1 HIT NOS. IF 'CDTL 22' IS SET
C
        IF(DSPDTL(22) .AND..NOT. FL18)
     +    CALL DNUM(IHIT,X+20.,Y+20.,CHARSZ,0.)
C
C                  DRAW RAW HIT NOS. IF 'CDTL 39 * 22' ARE BOTH SET
C
        IF(DSPDTM(9) .AND. DSPDTL(22) .AND..NOT.FL18)
     +    CALL DNUM(JHIT,X+XOFF,Y+10.,CHARSZ/2.,0.)
C
C                  DRAW CROSS ( X ) TO REPRESENT HIT.
C
        CALL MOVEA(X-SIZE,Y-SIZE)
        CALL DRAWA(X+SIZE,Y+SIZE)
        CALL MOVEA(X-SIZE,Y+SIZE)
        CALL DRAWA(X+SIZE,Y-SIZE)
C
C                  JUMP TO END OF DO LOOP:
C                     IF LONGITUDINAL COORDINATE WAS GOOD
C                     IF CDTL 8 IS OFF
C                     IF CURRENT VIEW IS NOT ( ZY  FOR ALL HITS    OR
C                                              ZX  FOR BARREL HITS OR
C                                              XY  FOR ENDWALL HITS   )
C
        IF( VALIL .OR. .NOT.DSPDTL(8) ) GO TO 1
        IF( .NOT.(    ZXVIEW .AND. IOR.EQ.1
     +           .OR. ZYVIEW
     +           .OR. XYVIEW .AND. IOR.EQ.3) ) GO TO 1
C
C                  DRAW A CIRCLE AROUND THIS BAD CHAMBER Z
C
            SIZ2=35.0
            DPHI=0.314159
            PHI=0.
            CALL MOVEA(X+SIZ2,Y)
C
            DO 18 I=1,20
              PHI=PHI+DPHI
              X2=X+SIZ2*COS(PHI)
              Y2=Y+SIZ2*SIN(PHI)
              CALL DRAWA(X2,Y2)
   18       CONTINUE
C                  IF CDTL 24 IS OFF , FORGET THE OTHER AMBIGUITY
            IF( .NOT. DSPDTL(24) ) GO TO 3
 1    CONTINUE
 3    IF(.NOT.END) GO TO 2
      RETURN
200   IF(.NOT. FL18) CALL TRMOUT(80,'MUON HIT BANK(S) MISSING^')
      RETURN
      END
