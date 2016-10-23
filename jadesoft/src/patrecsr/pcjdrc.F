C   20/09/79 102191201  MEMBER NAME  PCJDRC   (PATRECSR)    FORTRAN
      SUBROUTINE PCJDRC
#include "cjdrch.for"
C
C
C----------------------------------------------------------------------
C         --------------  SUBROUTINE PCJDRC  -----------------
C         --- G.F.PEARCE .. LAST UPDATE : 1800 ON 11/09/79 ---
C
C   SUBROUTINE TO DUMP ALL OF THE JET CHAMBER CONSTANTS COMMON /CJDRCH/
C
C  CONTENTS OF /CJDRCH/  ####### COMPILED BY G.F.PEARCE #######
C  ====================
C
C  RDEC  (I)   ..  MEAN RADII OF CHAMBER WALLS
C  PSIIN (I)   ..  ANGULAR INCREMENT OF FIRST WIRE LINE
C  RINCR (I)   ..  DISTANCE BETWEEN TWO POTENTIAL WIRES
C  FIRSTW(I)   ..  RADII OF FIRST MAIN POTENTIAL WIRES IN EACH RING
C  FSENSW(I)   ..  RADII OF FIRST SENSE WIRES IN EACH RING
C  RDEPTH      ..  DIFFERENCE OF TWO CHAMBER RADII
C  SWDEPL      ..  WIRE STAGGERING ( > 0 MEANS +VE PHI FOR ZEROTH WIRE)
C  YSUSPN      ..  MAXIMUM VERTICAL DISPLACEMENT OF WIRE DUE TO GRAVITY
C  TIMDEL(J,I) ..  SIZE OF JET CHAMBER TIMING BIN (IN MMS)
C                  I = 1,2,3 FOR RINGS 1,2,3 RESPECTIVELY
C                  J = 1,2 FOR LAYERS 0-7 AND 8-15 RESPECTIVELY
C  ZMAX        ..  SENSITIVE HALF LENGTH OF WIRES
C  ZOFFS       ..  OFFSET OF WIRES
C  ZRESOL      ..  RESOLUTION IN Z
C  ZNORM       ..  Z NORMALISATION
C  ZAL         ..  WIRE LENGTH
C  ZSCAL       ..
C  DRIDEV      ..  ANGLE OF DRIFT SPACE TO WIRE PLANE
C  DRICOS      ..  COSINE(DRIDEV)
C  DRISIN      ..  SINE(DRIDEV)
C  DRIROT(I,J) .. DRIDEV FOR EACH SIDE (J=1,2) OF EACH CELL (I)
C                 J = 1 GIVES LEFT SIDE.   J = 2 GIVES RIGHT SIDE
C  SINDRI(I,J) .. SINE(DRIROT)
C  COSDRI(I,J) .. COSINE(DRIROT)
C
C----------------------------------------------------------------------
C
C
      PRINT11
 11   FORMAT(
     # 1X,45('-')/
     # ' DUMP OF JET CHAMBER CONSTANTS COMMON /CJDRCH/'/
     # 1X,45('-'))
C
      PRINT12,(RDEC(I),I=1,4)
     #       ,(PSIIN(I),I=1,3)
     #       ,(RINCR(I),I=1,3)
     #       ,(FIRSTW(I),I=1,3)
     #       ,(FSENSW(I),I=1,3)
     #       , RDEPTH,SWDEPL,YSUSPN,ZMAX,ZOFFS,ZRESOL,ZNORM,ZAL,ZSCAL
     #       ,DRIDEV,DRISIN,DRICOS
 12   FORMAT(' RDEC   .. CHAMBER WALL RADII .................. ',4E11.4/
     #       ' PSIIN  .. ANGULAR INCREMENT OF FIRST WIRE ..... ',3E11.4/
     #       ' RINCR  .. POTENTIAL WIRE SEPERATION ........... ',3E11.4/
     #       ' FIRSTW .. RADII OF FIRST POTENTIAL WIRES ...... ',3E11.4/
     #       ' FSENSW .. RADII OF FIRST SENSE WIRES .......... ',3E11.4/
     #       ' RDEPTH .. DIFFERENCE OF TWO CHAMBER RADII ..... ', E11.4/
     #       ' SWDEPL .. WIRE STAGGERING ..................... ', E11.4/
     #       ' YSUSPN .. MAX. VERTICAL GRAVITY DISPLACEMENT .. ', E11.4/
     #       ' ZMAX   .. SENSITIVE HALF LENGTH OF WIRES ...... ', E11.4/
     #       ' ZOFFS  .. WIRE OFFSET ......................... ', E11.4/
     #       ' ZRESOL .. RESOLUTION IN Z ..................... ', E11.4/
     #       ' ZNORM  .. NORMALISATION IN Z .................. ', E11.4/
     #       ' ZAL    .. WIRE LENGTH ......................... ', E11.4/
     #       ' ZSCAL  ..                                       ', E11.4/
     #       ' DRIDEV .. FOLLOWED BY SINE AND COSINE ......... ',3E11.4)
C
      PRINT13,((TIMDEL(J,I),J=1,2),I=1,3)
 13   FORMAT(' TIMDEL .. SIZE OF TIMING BINS (MMS) ........... ',6E11.4)
      RETURN
      END
