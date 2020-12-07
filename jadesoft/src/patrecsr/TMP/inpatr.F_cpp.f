C   08/12/80 706261842  MEMBER NAME  INPATR   (S)           FORTRAN
      SUBROUTINE INPATR
C
C----------------------------------------------------------------------
C   INITIALISATION OF PATTERN RECOGNITION PROGRAM LIMITS AND TOLERANCES
C   AUTHORED BY A VARIETY OF PEOPLE.
C   TAKE OUT A FEW LIMITS NOW RUN DEPENDENT IN ADPATR  E ELSEN 26/06/87
C----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
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
C--------------------------------------------------------------------
C
C       IPFAST = 2 MEANS SLOWEST, MOST EFFICIENT VERSION
C                    OF PATTERN RECOGNITION
C
C       IPFAST = 1 MEANS INTERMEDIATE VERSION OF PATTERN RECOGNITION
C
C       IPFAST = 0 MEANS FASTEST VERSION OF PATTERN RECOGNITION
C
C--------------------------------------------------------------------
C
        IF(IPFAST.NE.0.AND.IPFAST.NE.2.AND.IPFAST.NE.1)
     %  WRITE(6,256) IPFAST
  256  FORMAT('0 @@@@@@@ ERROR IN INPATR INITIALIZATION , IPFAST=',I10,
     *'  IPFAST SET TO 2')
        IF(IPFAST.NE.0.AND.IPFAST.NE.2.AND.IPFAST.NE.1) IPFAST=2
C
C
C
C---------------------------------------------------------------------
C INTERMEDIATE PATREC .. IPFAST=1
C ===================
C
C---------------------------------------------------------------------
C
C-----------------------------
C     INITIALISE PATREC LIMITS
C-----------------------------
C
C     MIN. NUMBER OF HITS IN A CELL
      LMPATR(1) = 8
C
C     WEIGHT OF ORIGIN IN FINAL R-FI-FIT
      PATRLM(2) = 0.00
C
C     LENGTH OF ARRAY WRK IN /CWORK/
      LMPATR(5) = 6000
      LMPATR(5) = 7000
C
C-----------------------------
C     INITIALISE FLINEL LIMITS
C-----------------------------
C
C     LIMIT FOR DEF. OF CLOSE HITS
      FLINLM(1) = 10.
C
C     LIMIT FOR SLOPE AGREEMENT
C                                           NOW IN ADPATR
C     FLINLM(2) = 3.0
C-----------------------------
C     INITIALISE FTRKEL LIMITS
C-----------------------------
C
C     STAGGERING (*4)
      TRELLM( 1) = 2.40
C
C     LIMIT ON DEV. FROM STAGGERING  (*4)
      TRELLM( 2) = 2.50
C
C     MAX (STAGG.DIST. * (-1)**ILAY)
      TRELLM( 3) = .20
C
C     MAX. RMS OF GOOD TREL: SIG0 = (4)+(5)*CURV
      TRELLM( 4) = 0.35
      TRELLM( 5) = 0.15
C
C     MAX.# OF REJECTED HITS BEFORE CUT OF TREL
      LMTREL( 6) =   2
C
C     MIN. |SIGL-SIGR| FOR L/R DET.
      TRELLM( 7) = 0.10
C
C     SPECIAL LIMIT FOR STRAIGHT TRACKS AT ZERO
      TRELLM( 8) = 4.0
C
C     LIMIT OF CURV/2 FOR FIT OF 4-5-HIT TREL
      TRELLM( 9) = .05
C
C     LIMITS FOR ZERO APPROACHING TREL
      TRELLM(10) = 1.0
      TRELLM(11) = .10
      TRELLM(12) = 1000000.
C
C     LIMITS FOR # OF HITS / TREL
      LMTREL(13) = 4
C
C     SIGMA OF GOOD FIT
      TRELLM(14) = .24
C
C     MIN. DISTANEC OF 'ZERO APPROACHING TREL'
      TRELLM(15) = 1.0
C
C     LIMIT OF SIGMA FOR TREL-FIT
      TRELLM(16) = 0.25
C
C     LIMIT OF SIGMA FOR TREL-FIT *(# OF END POINTS AT WALL)
      TRELLM(17) = 0.35
C
C     LIMIT OF SIGMA FOR TREL-FIT *(# OF ENDPOINTS AT WIREPLANE)
      TRELLM(18) = 0.15
C
C     L/R DETERMINED IF |SIGL**2-SIGR**2|<LIMIT
      TRELLM(19) = 0.10
C
C     NOT YET USED
      TRELLM(20) = 0.0
11111 CONTINUE
C-----------------------------
C     INITIALISE BACKTR LIMITS
C-----------------------------
C
C     OVERLAP OF HITS FOR JOIN OF TRACKELS IN CELL
      IBKK(1)=1
C
C     GAP IN HITS FOR JOIN OF TRACKELS IN ONE CELL
      IBKK(2)=2
C
C     IF THE LAST WIRE HIT IS LESS THAN IBKK(3) OR FIRST
      IBKK(4)=3
C
C     WIRE GREATER THAN IBKK(4) A CHECK IS MADE TO SEE WHETHER
C     THE TRACK PASSES THROUGH THE CELL SIDEWALL
      IBKK(3)=12
C
C     FOR JOINING OF TWO TRACKELS IN ONE CELL AND FOR THE TRACKELS
C     TO BE CONSIDERED ON OPPOSITE SIDES OF THE WIRE PLANE:
C     THE SLOPES MUST BE CORRECT,CROSSING FLAGS CORRECT AND
C     DS1 OR DS2  .LT.BKK(5)
      BKK(5)=15.
C
C     OVERLAP OR GAP IN HITS FOR CONNECTION THROUGH
C     CELL SIDEWALL
      IBKK(6)=2
C
C     MIN OF FIRST WIRE HIT TO TRY CONNECTION WITHIN CELL
      IBKK(8)=3
C
C     DRIFT TIME TOLERANCE FOR MATCHING
C     TRACKELS WITHIN A CELL
      BKK(9)=5.
C
C     DIFFERENCE IN THE ABS VALUES OF THE SLOPES FOR A
C     CONNECTION SKIPPING RING TWO
      BKK(10)=0.
C
C     UPPER LIMIT FOR THE VALUE OF THE SLOPE FOR EACH
C     TRACKEL TO TRY A CONNECTION SKIPPING RING TWO
      BKK(11)=0.0
C
C     BKK(12) MULTIPLIES SLOPE FOR SLOPE COMPARISON
C     IN CONNECTION THROUGH CELL SIDEWALL
      BKK(12)=1.
C
C     BKK(13) IS CONSTANT TERM FOR SLOPE COMPARISON
C     FOR CONNECTION THROUGH CELL SIDEWALL
      BKK(13)=.07
C
C     BKK(14) MULTIPLIES SLOPE FOR SLOPE COMPARISON
C     FOR CONNECTION OF TRACKELS IN ONE CELL
      BKK(14)=.02
C
C     BKK(15) IS CONSTANT TERM FOR SLOPE COMPARISON
C     FOR CONNECTION OF TRACKELS IN ONE CELL
      BKK(15)=.05
C
C     IBKK(16) NE 0 FORCES FIT BEFORE CONNECTION WITHIN CELL
      IBKK(16)=1
C
C     IBKK(17) NE 0 FORCES FIT BEFORE SIDE CONNECTION
      IBKK(17)=0
C
C     IBKK(18) NE 0 FORCES FIT BEFORE RING CONNECTION
      IBKK(18)=1
C
C     IBKK(19) IS MIN NO OF HITS FOR BACKTR
C     TO BE FORCED(BY IBKK(20) BEING NE 0) TO
C     USE L-R AMBIGUITY FROM WIRE STAGGERING
      IBKK(19)=7
C
C     IBKK(20) NE 0 FORCES BACKTR TO USE THE L-R
C     AMBIGUITY FROM WIRE STAGGERING
      IBKK(20)=1
C
C     THE XBKK ARRAY IS FOR MATCHING BETWEEN RINGS IN BACKTRACE
C     IT IS ARRANGED IN 4 GROUPS OF 8 NUMBERS
C     THE FOUR GROUPS ARE THE TOLERANCES FOR :
C         XBKK(1)-(8) : DRIFT TIME MATCHING BETWEEN RINGS
C         XBKK(9)-(16) : CONSTANT THAT MULTIPLIES SLOPE FOR
C                        SLOPE COMPARISON
C         XBKK(17)-(24) : ADDITIVE CONSTANT FOR SLOPE COMPARISON
C         XBKK(25)-(32) : CONSTANT FOR DETERMING WHETHER
C                         TRACK CROSSES WIRE PLANE AS IT
C                         GOES FROM RING TO RING
C
C       WHITHIN EACH GROUP OF 8 THE FIRST 4 NUMBERS
C       REFER TO RING TO RING CONNECTION (ALWAYS WITHIN ONE SECTOR)
C       AND THE SECOND 4 REFER TO 'CORNER' CONNECTIONS (CROSSING
C       FROM ONE SECTOR TO ANOTHER)
C
C       EACH OF THESE GROUP OF 4 NUMBERS IS FURTHER DIVIDED INTO
C       2 GROUPS OF 2
C       THE FIRST GROUP IS USED FOR THE FIRST BACKTRACE PASS
C       AND THE SECOND GROUP OF TOLERANCES FOR THE SECOND PASS
C       WITH INCREASED TOLERANCES
C
C       FINALLY, THE 2 TOLERANCES IN THE SMALLEST SUB-SECTION:
C       THE FIRST IS FOR 'NORMAL' TRACK CONNECTIONS
C       THE SECOND IS FOR CONNECTIONS WHERE AT LEAST ONE TRACKEL
C       IS SHORT (LT 8 HITS)
C
C
      XBKK(1)=1.
      XBKK(2)=1.
      XBKK(3)=3.
      XBKK(4)=4.
      XBKK(5)=1.
      XBKK(6)=1.
      XBKK(7)=3.
      XBKK(8)=4.
      XBKK(9)=.01
      XBKK(10)=.02
      XBKK(11)=.03
      XBKK(12)=.04
      XBKK(13)=.01
      XBKK(14)=.02
      XBKK(15)=.03
      XBKK(16)=.04
      XBKK(17)=.04
      XBKK(18)=.04
      XBKK(19)=.1
      XBKK(20)=.1
      XBKK(21)=.04
      XBKK(22)=.04
      XBKK(23)=.1
      XBKK(24)=.1
      XBKK(25)=1.
      XBKK(26)=2.
      XBKK(27)=5.
      XBKK(28)=10.
      XBKK(29)=3.
      XBKK(30)=6.
      XBKK(31)=6.
      XBKK(32)=10.
C
C  IYBKK(1) NE 0 FORCES FIT BEFORE SKIP-RING CONNECTION
      IYBKK(1)=0
C
C   LIMITS FOR FIT WITHIN BACKTRACE
C   YBKK(2) GIVES UPPER LIMIT FOR RMS FOR WHOLE TRACK
C    FOR CONNECTION WITHIN CELL
      YBKK(2)=.5
C
C   YBKK(3) GIVES UPPER LIMIT FOR RESIDUAL/PT FOR CANDIDATE TRACK
C    ELEMENT  FOR CONNECTION WITHIN CELL
      YBKK(3)=.5
C
C   YBKK(4) GIVES UPPER LIMIT FOR RMS FOR WHOLE TRACK
C    FOR RING CONNECTION
      YBKK(4)=.5
C
C   YBKK(5) GIVES UPPER LIMIT FOR RESIDUAL/PT FOR CANDIDATE TRACK
C    ELEMENT  FOR RING CONNECTION
      YBKK(5)=.5
C
C   YBKK(6) GIVES UPPER LIMIT FOR RMS FOR WHOLE TRACK
C    FOR SIDE CONNECTION
      YBKK(6)=5.
C
C   YBKK(7) GIVES UPPER LIMIT FOR RESIDUAL/PT FOR CANDIDATE TRACK
C    ELEMENT  FOR SIDE CONNECTION
      YBKK(7)=5.
C
C   YBKK(8) GIVES UPPER LIMIT FOR RMS FOR WHOLE TRACK
C    FOR SKIP-RING CONNECTION
      YBKK(8)=0.
22222 CONTINUE
C
C
C---------------------------------------
C     INITIALIZATION OF BAKPAK LIMITS
C---------------------------------------
C
C     IBKK(7) IS LAYER GAP FOR BAKPAK TO TRY CONNECTION
      IBKK(7) = 4
C
C     XBKK(33) IS RMS LIMIT FOR FIRST PASS IN BAKPAK
      XBKK(33)=.5
C
C
C     XBKK(34) IS RMS LIMIT FOR SECOND PASS IN BAKPAK
      XBKK(34)=1.
C
C   IYBKK(11) NE 0 CAUSES BAKPAK TO BE CALLED FOR FIRST PASS
       IYBKK(11)=0
C
C   IYBKK(15) NE 0 CAUSES BAKPAK TO BE CALLED FOR SECOND PASS
       IYBKK(15)=1
C
C    IYBKK(10) IS DIF BETWEEN EXPECTED CELL AND ACTUAL
C       CELL TO ATTEMPT CONNECTION IN BAKPAK
        IYBKK(10)=1
C
C     IXBKK(35).NE. 0 MEANS PATROL IS CALLED INSIDE BAKPAK
      IXBKK(35)=0
C
C     IYBKK(9) IS MIN NO OF HITS FOR PATROL TO FIND
C        TO MAKE SKIP RING CONNECTION IN BAKPAK
      IYBKK(9)=6
C
C     IYBKK(16) IS MIN NO OF HITS FOR PATROL TO FIND
C        TO MAKE SKIP RING CONNECTION IN BAKPAK
C       WITH WALL CROSSING IN RING 2
      IYBKK(16)=5
33333 CONTINUE
C
C
C---------------------------------------
C     INITIALIZATION OF XYFIT LIMITS
C---------------------------------------
C
C     XYFIT CONTROL WORD (VIA BIT STRUCTURE)
C     BIT ON MEANS :
C     BIT 31 .. MAKE A SINGLE FIT TO THE HITS AND STOP
C     BIT 30 .. NEVER PERFORM A CIRCLE FIT
C     BIT 29 .. RECALCULATE ABERRATIONS AND MAKE ONE MORE FIT
C     BIT 28 .. STORE TRACK ELEMENT CHI**2'S WHEN BIT 31 SET
C     BIT 27 .. STOP TRYING TO REJECT WORST TRACK ELEMENT
C     BIT 26 .. NEVER CHECK L/R SOLUTION WITH FIT
C     BIT 25 .. NOT USED
      IXYF(1) = 1
C
C     GOOD TRACK FIT CHI**2 CUT    CUT = (2) + (L/R) * (3)
      XYF(2) = 0.6
      XYF(3) = 0 0
C
C     HIT RESIDUAL CUTS            CUT = (4) + (L/R) * (5)
      XYF(4) = 3.0
      XYF(5) = 0.0
C
C     CIRCLE FIT NEVER MADE WHEN CURVATURE LESS THAN XYF(6)
C     MOM=0.03*H*RAD (MMS,KG,MEV/C)
      XYF(6) = 3.0E-4
C
C     MINIMUM SIGNIFICANT FRACTIONAL CHANGE IN CHI**2 REQUIRED TO
C     UNAMBIGUOUSLY DETERMINE LEFT/RIGHT SOLUTION
      XYF(7) = 0.2
C
C     CIRCLE FIT ITERATION LIMIT ON FRACTIONAL RADIUS ADJUSTMENT
      XYF(8) = 0.05
C
C     SPARE
      IXYF(9) = 0
C
C     CIRCLE FIT NEVER MADE WHEN PARABOLA FIT CHI**2 EXCEEDS XYF(10)
      XYF(10) = 10.0
C
C     LIMIT ON NUMBER OF LINES OF ERROR PRINT FROM FXYZ/XYFIT/PATROL
      IXYF(11) = 200
C
C     SPARE
      IXYF(15) = 0
C     SPARE
      IXYF(16) = 0
C     SPARE
      IXYF(17) = 0
C     Z-AMPLITUDE DOUBLE HIT RESOLUTION
      IXYF(18) = 0
C     PRINT FLAG (FXYZ)
      IXYF(19) = 0
C     PRINT FLAG (XYFIT)
      IXYF(20) = 0
44444 CONTINUE
C---------------------------------------
C     INITIALIZATION OF PATROL TOLERANCES
C---------------------------------------
C
C     PATROL CONTROL WORD
C     BIT 31 .. NEVER CALL A RE-FIT AFTER FINDING EXTRA HITS
C     BIT 30 .. ONLY SEARCH RINGS "WITHOUT" HITS ON THIS TRACK
C     BIT 29 .. IGNORE ALL HITS THAT "ARE" ON TRACK ELEMENTS
C     BIT 28 .. ONLY SEARCH RINGS "WITH" HITS ALREADY ON THIS TRACK
C     BIT 27 .. SPARE
C     BIT 26 .. SPARE
C     BIT 25 .. IGNORE ALL HITS THAT "ARE NOT" ON TRACK ELEMENTS
      IGFP(1) = 0
C
C     RMS OF FIT AT WHICH PATROL NOT RUN.
      GFP(2) = 3.
C
C     DOUBLE HIT RESOLUTION .. DONT LOOK FOR MORE HITS CLOSER THAN THIS
C                                           NOW IN ADPATR
C     GFP(3) = 4.5
C
C     HIT RESIDUAL CUTS            CUT = (4) + (L/R) * (5)
      GFP(4) = 2.9
      GFP(5) = 0.0
C
C     MAXIMUM # OF SUCCESSIVE FRUITLESS LAYERS ALLOWED IN NEW RING
      IGFP(6) = 3
C
C     MAXIMUM RADIAL GAP ON EXTRAPOLATION
       GFP(7) = 50.0
C
C     DELTA(R) TO DEFINE CONSECUTIVE HITS
       GFP(8) = 15.0
C
C     MINIMUM # OF CONSECUTIVE HITS REQUIRED ON EXTRAPOLATION
      IGFP(9) = 2
C
C     NO. NEW HITS FOUND BEFORE RE-FIT CALLED
      IGFP(10) = 3
C
C     WALL CROSSING LIMIT
      GFP(11) = 0.8
C
C     EXTRA HOLE SIZE ALLOWED ON GFP(7) WHEN CROSSING RING GAP
      GFP(12) = 50.0
C     MAXIMUM NUMBER OF RE-FITS
      IGFP(13) = 10
C
C     CURVATURE ABOVE WHICH PATROL NOT CALLED
      GFP(14) = 1.0E-2
C
C     ITERATION LIMIT ON CONVERTING PARABOLA TO CIRCLE
      GFP(15) = 0.05
C     SPARE
      IGFP(16) = 0
C     SPARE
      IGFP(17) = 0
C     SPARE
      IGFP(18) = 0
C     SPARE
      IGFP(19) = 0
C     PRINT FLAG
      IGFP(20) = 0
55555 CONTINUE
C----------------------------
C     INITIALISE ZRFIT LIMITS
C----------------------------
C
C     LIMIT FOR RMS OF FIT
C                                           NOW IN ADPATR
C     ZFITLM( 1) =   50.
C
C     SIGMA OF Z
C                                           NOW IN ADPATR
C     ZFITLM( 2) =   20.
C
C     LIMIT FOR HITS PER SEGMENT
      LMZFIT( 3) = 6
C
C     LIMIT FOR # OF SEGMENTS
      LMZFIT( 4) = 2
C
C     LIMIT FOR # OF SEGMENTS
      ZFITLM( 5) = .05
C
C     LIMIT FOR DELTA(R) FOR VERTEX DET.
      ZFITLM( 6) =   50.
C
C     LABEL FOR HIT SELECTION PREPROCESSOR
      LMZFIT(10) = 2
C
C--------------------------------------------
C     INITIALISATION OF ADMINISTRATION LIMITS
C--------------------------------------------
C
C     INITIALIZATION OF IMCERT
C     1 = NO DELETION OF COVERED + SHORT TRACKS
C     0 = DELETION OF COVERED + SHORT TRACKS
      IMCERT=0
C
C     IXBKK(40) NE 0 MEANS KNTREL ITERATES WITH
C     UPDATED BACKTR ARRAYS
      IXBKK(40)=0
C
C     IXBKK(39) GIVES MAX NO OF ALLOWED ITERATIONS
      IXBKK(39)=2
C
C     IXBKK(38) NE 0 CAUSES PRINT IN KNTREL
C     BIT 31 ON - MIN PRINT
C     BIT 30 ON - HIT LABEL IN CWORK
C     BIT 29 ON - HIT LABEL IN CDATA
C     BIT 28 ON - PCWORK PRINT
C     BIT 26 ON - BAKPAK PRINT
      IXBKK(38)=0
C
C     IXBKK(37) IS MIN NO OF UNUSED HITS LEFT ON A TRACKEL
C     FOR IT TO BE BROUGHT BACK AS A SEPARATE TRACK
      IXBKK(37)=5
C
C     IXBKK(36) IS MAX NO OF HITS LEFT UNCORRELATED
C     BEFORE TRACKEL IS ASSIGNED TO TRACK
      IXBKK(36)=1
C
C     IYBKK(14) .NE. 0  CAUSES BACKGROUND TRACKS TO BE DELETED
        IYBKK(14)=1
C
C     YBKK(12) IS LOWER LIMIT ON CURVATURE FOR DEFINITION
C      OF BACKGROUND TRACK
       YBKK(12)=.003
C
C      YBKK(13) IS UPPER LIMIT ON COS OF ANGLE THAT THE SARTPOINT
C      OF THE TRACK MAKES WITH WIRE PLANE TO DEFINE A BACKGROUND TRACK
      YBKK(13)=.9
C
C       YBKK(15) IS INTERSECT OF TRACK WITH BEAM LINE
C        ABOVE WHICH TRACK IS DELETED AS BACKGROUND
         YBKK(15)=100.
C
C    ******* SPECIAL FOR TEST OF ZCHECK **************
      IYBKK(20)=0
C--------------------------------------------------------------------
C
C           END OF  IPFAST = 1  SECTION OF INPATR
C
C--------------------------------------------------------------------
C
        IF(IPFAST.EQ.1) RETURN
        IF(IPFAST.EQ.2) GO TO 102
C
C
C
C
C
C#####################################################################
C FASTEST LEAST EFFICIENT PATREC .. IPFAST=0
C ==============================
C       OVERWRITE SOME LIMITS TO GET FAST VERSION OF PATREC
C#####################################################################
66666 CONTINUE
C
C
C-----------------------------
C     INITIALISE FTRKEL LIMITS
C-----------------------------
C
C     MAX.# OF REJECTED HITS BEFORE CUT OF TREL
      LMTREL( 6) =   1
C
C     LIMIT OF CURV/2 FOR FIT OF 4-5-HIT TREL
      TRELLM( 9) = .005
C
C     LIMITS FOR # OF HITS / TREL
      LMTREL(13) = 6
C
C     SIGMA OF GOOD FIT
      TRELLM(14) = .30
C
C     LIMIT OF SIGMA FOR TREL-FIT
      TRELLM(16) = 0.45
C
C-----------------------------
C     INITIALISE BACKTR LIMITS
C-----------------------------
C
C     IBKK(16) NE 0 FORCES FIT BEFORE CONNECTION WITHIN CELL
      IBKK(16)=0
C
C     IBKK(17) NE 0 FORCES FIT BEFORE SIDE CONNECTION
      IBKK(17)=0
C
C     IBKK(18) NE 0 FORCES FIT BEFORE RING CONNECTION
      IBKK(18)=0
C
C     THE XBKK ARRAY IS FOR MATCHING BETWEEN RINGS IN BACKTRACE
C     IT IS ARRANGED IN 4 GROUPS OF 8 NUMBERS
C     THE FOUR GROUPS ARE THE TOLERANCES FOR :
C         XBKK(1)-(8) : DRIFT TIME MATCHING BETWEEN RINGS
C         XBKK(9)-(16) : CONSTANT THAT MULTIPLIES SLOPE FOR
C                        SLOPE COMPARISON
C         XBKK(17)-(24) : ADDITIVE CONSTANT FOR SLOPE COMPARISON
C         XBKK(25)-(32) : CONSTANT FOR DETERMING WHETHER
C                         TRACK CROSSES WIRE PLANE AS IT
C                         GOES FROM RING TO RING
C
C       WHITHIN EACH GROUP OF 8 THE FIRST 4 NUMBERS
C       REFER TO RING TO RING CONNECTION (ALWAYS WITHIN ONE SECTOR)
C       AND THE SECOND 4 REFER TO 'CORNER' CONNECTIONS (CROSSING
C       FROM ONE SECTOR TO ANOTHER)
C
C       EACH OF THESE GROUP OF 4 NUMBERS IS FURTHER DIVIDED INTO
C       2 GROUPS OF 2
C       THE FIRST GROUP IS USED FOR THE FIRST BACKTRACE PASS
C       AND THE SECOND GROUP OF TOLERANCES FOR THE SECOND PASS
C       WITH INCREASED TOLERANCES
C
C       FINALLY, THE 2 TOLERANCES IN THE SMALLEST SUB-SECTION:
C       THE FIRST IS FOR 'NORMAL' TRACK CONNECTIONS
C       THE SECOND IS FOR CONNECTIONS WHERE AT LEAST ONE TRACKEL
C       IS SHORT (LT 8 HITS)
C
      XBKK(1)=8.
      XBKK(2)=8.
      XBKK(3)=20.
      XBKK(4)=45.
      XBKK(5)=8.
      XBKK(6)=8.
      XBKK(7)=20.
      XBKK(8)=45.
      XBKK(9)=.05
      XBKK(10)=.08
      XBKK(11)=.12
      XBKK(12)=.17
      XBKK(13)=.06
      XBKK(14)=.12
      XBKK(15)=.12
      XBKK(16)=.17
      XBKK(17)=.12
      XBKK(18)=.12
      XBKK(19)=.40
      XBKK(20)=.40
      XBKK(21)=.12
      XBKK(22)=.25
      XBKK(23)=.4
      XBKK(24)=.4
      XBKK(25)=1.
      XBKK(26)=2.
      XBKK(27)=5.
      XBKK(28)=10.
      XBKK(29)=3.
      XBKK(30)=6.
      XBKK(31)=6.
      XBKK(32)=10.
77777 CONTINUE
C
C
C---------------------------------------
C     INITIALIZATION OF BAKPAK LIMITS
C---------------------------------------
C
C   IYBKK(11) NE 0 CAUSES BAKPAK TO BE CALLED FOR FIRST PASS
       IYBKK(11)=0
C
C   IYBKK(15) NE 0 CAUSES BAKPAK TO BE CALLED FOR SECOND PASS
       IYBKK(15)=0
C
C
C
C---------------------------------------
C     INITIALIZATION OF XYFIT LIMITS
C---------------------------------------
C
C
C     1).NO CLEVER HIT DELETION BY FIT
C     2).KILL ALL CIRCLE FITS
      IXYF(1)=3
C---------------------------------------
C     INITIALIZATION OF PATROL LIMITS
C---------------------------------------
C
C     KILL PATROL COMPLETELY
      GFP(2) = 0.
C
C----------------------------
C     INITIALISE ZRFIT LIMITS
C----------------------------
C
C     LABEL FOR HIT SELECTION PREPROCESSOR
      LMZFIT(10) = 2
C
C--------------------------------------------
C     INITIALISATION OF ADMINISTRATION LIMITS
C--------------------------------------------
C
C     INITIALIZATION OF IMCERT
C     1 = NO DELETION OF COVERED + SHORT TRACKS
C     0 = DELETION OF COVERED + SHORT TRACKS
      IMCERT=0
C
C     IXBKK(40) NE 0 MEANS KNTREL ITERATES WITH
C     UPDATED BACKTR ARRAYS
      IXBKK(40)=0
C
C     IXBKK(39) GIVES MAX NO OF ALLOWED ITERATIONS
      IXBKK(39)=0
C
C     IYBKK(14) .NE. 0  CAUSES BACKGROUND TRACKS TO BE DELETED
        IYBKK(14)=0
C
      RETURN
C
C--------------------------------------------------------------------
C           END OF  IPFAST = 0  SECTION OF INPATR
C--------------------------------------------------------------------
C
C
 102   CONTINUE
C
C
C
C
C
C#####################################################################
C SLOWEST BUT BEST VERSION OF PATREC .. IPFAST=2
C ==================================
C       OVERWRITE SOME LIMITS TO GET EFFICIENT VERSION OF PATREC
C#####################################################################
C
C
C---------------------------------------
C     INITIALIZATION OF BAKPAK LIMITS
C---------------------------------------
C
C   IYBKK(11) NE 0 CAUSES BAKPAK TO BE CALLED FOR FIRST PASS
       IYBKK(11)=1
C
C     IXBKK(35).NE. 0 MEANS PATROL IS CALLED INSIDE BAKPAK
      IXBKK(35)=1
C
C
C
C--------------------------------------------
C     INITIALISATION OF ADMINISTRATION LIMITS
C--------------------------------------------
C
C     IXBKK(39) GIVES MAX NO OF ALLOWED ITERATIONS
      IXBKK(39)=5
C
      RETURN
C--------------------------------------------------------------------
C           END OF  IPFAST = 2  SECTION OF INPATR
C--------------------------------------------------------------------
C
      END
      BLOCK DATA BL1            ! 01/07/99 PMF add name
C
      IMPLICIT INTEGER*2 (H)
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
C
       DATA IPFAST/2/
       END