C   09/05/83 305092216  MEMBER NAME  MUHIT    (JADEMUS)     FORTRAN
C
C LAST CHANGE 22.15 09/05/83 CHRIS BOWDERY- COMMENT CHANGE FOR NW=22
C      CHANGE 19.52 20/10/81 CHRIS BOWDERY- CORRECTED HIT CODE SETTING
C      CHANGE 10.00 19/10/81 CHRIS BOWDERY- FLAG LOST1 AS BAD HIT
C NEW VERSION 18.08 22/09/81 CHRIS BOWDERY- USES NEW MUR2/3
C
C-----------------------------------------------------------------------
      SUBROUTINE MUHIT(HC,IHIT,ITRK,LOST1,VALID,VALIL,HAMB,END) 
C-----------------------------------------------------------------------
C
C PROVIDES MUON HIT COORDINATES AND HIT INFORMATION FOR A GIVEN HIT
C NUMBER AND OPTIONALLY AN ASSOCIATED TRACK NUMBER. INITIALISATION
C IS DONE BY THE CALLING ROUTINE VIA COMMON /CMUGRA/
C
C INPUT VARIABLES
C
C IHIT        HIT NUMBER (AS DEFINED BY MUR1/1, THE COORDINATE BANK).
C ITRK        IF NOT 0 , THE TRACK NUMBER ASSOCIATED WITH IHIT
C
C OUTPUT VARIABLES
C
C HC          THE OUTPUT - 9 WORDS...
C               WORD  1     -  4*CHAMBER NUMBER + (HIT NUMBER -1)
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
C END         = .TRUE. IF THIS IS THE LAST HIT , .FALSE. OTHERWISE
C
C-----------------------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
C PMF 03.11.98 
      LOGICAL TBIT
C
      LOGICAL VALID,VALIL,LOST1,END,MUOK
      DIMENSION HC(9),HAMB(2)
C
C-----------------------------------------------------------------------
C
C                  COMMONS
C  CMUGRA IS USED TO STORE POINTERS BETWEEN CALLS OF THIS ROUTINE.
C
#include "cmubcs.for"
      COMMON /CMUGRA/ IMR112,IMR122,IMUR20,IMUR21,NIDS,NWHIT,NTPH,
     +                NWTR,IMR222,IMR232,NHITS,MUOK
C
C                  SET MUR1/1 HIT NUMBER AND VALIDITY FLAGS FOR X * Z
C
      END   = .FALSE.
      VALID = TBIT(HDATA(IMR122+IHIT),14)
      VALIL = TBIT(HDATA(IMR122+IHIT),15)
C
C                  COPY THE HIT INFORMATION FROM MUR1/1
C
      DO 2 I=1,NWHIT
        HC(I) = HDATA(IMR112+I+(IHIT-1)*NWHIT)
 2    CONTINUE
C
C                  IF (HIT NUMBER - 1) IS ZERO AND THIS HIT RECOVERED
C                  THEN SET LOST1 TO BE TRUE, FALSE OTHERWISE.
C
      IVAL  = HC(1)
      LOST1 = MOD(IVAL,4).EQ.0 .AND. .NOT.(TBIT(HDATA(IMR122+IHIT),7) )
C***********************************************************************
C
C                  MAKE OVERWRITTEN HIT 1'S THE SAME AS BAD HITS
C
      IF(LOST1) VALID = .FALSE.
C***********************************************************************
      HAMB(1) = 0
      HAMB(2) = 0
C
C                  IS THIS HIT ASSOCIATED WITH ANY TRACK ?
C
      IF(.NOT. MUOK .OR. NIDS.LE.0) GO TO 4
      IQ222I = IMR222 + (IHIT-1)*NTPH
      IF( HDATA(IQ222I + 1).EQ.0 ) GO TO 4
C
C                  YES , THIS HIT WAS ASSOCIATED WITH SOME TRACK.
C                  CALCULATE THE POINTER TO THE AMBIGUITY FLAGS
C                  FOR THIS HIT. LOOP OVER ALL THE FLAGS AND ALSO
C                  PICK UP THE ASSOCIATED TRACK QUALITY FROM MUR2/1.
C  |--------------|----------------------------------------------------|
C  | MUR2/3 ====> | -   0   -1   +1   -2   +2   -12   +12   +12*  +22* |
C  |--------------|----------------------------------------------------|
C  | LEFT =HAMB(1)| 0   1    1    0    2    0     2     1     2     2  |
C  | RIGHT=HAMB(2)| 0   1    0    1    0    2     1     2     2     2  |
C  |______________|____________________________________________________|
C                                                         * = BAD DRIFT
C     PLUS         IF 100 < MU QUALITY > 0    "2" --> "4" IN ABOVE TABLE
C                  IF       MU QUALITY > 99   "2" --> "3" IN ABOVE TABLE
C
      IQ232I = IMR232 + (IHIT-1)*NTPH
      DO 7 I=1,NTPH
        ITRNUM = HDATA(IQ222I+I)
        ITRNUM = IABS(ITRNUM)
        IF( (ITRK.NE.0 .AND. ITRK.NE.ITRNUM ) .OR.ITRNUM.EQ.0 ) GO TO 7
        MUQUAL = IDATA(IMUR21 + (ITRNUM - 1)*NWTR + 6)
C
C                  GET THE AMBIGUITY FLAG FOR TRACK ITRNUM
C
        NW=HDATA(IQ232I+I)
        J = 1
        IF(NW.GT.0) J = 2
C
C                  FOR PHILOSOPHY 2.0 COMPATABILITY, NW=3 IS EQUIVALENT
C                  TO NW=12 (IF BAD HIT) AND THE CURRENT NW=22.
C
        IF( (NW.EQ.3 .OR. NW.EQ.12) .AND. .NOT.VALID) NW = 22
        IF(NW.EQ.0) NW = 11
        NW   = IABS(NW)
        NWRM = MOD(NW,10)
        NW10 = NW/10
        NADD = 0
        IF(MUQUAL.LE.0) GO TO 6
          NADD = 1
          IF(MUQUAL.LT.100) NADD = 2
 6      IF(NW10.EQ.2) NW10 = NW10 + NADD
        IF(NWRM.EQ.2) NWRM = NWRM + NADD
        IF(NW10.GT.HAMB(3-J) ) HAMB(3-J) = NW10
        IF(NWRM.GT.HAMB(J)   ) HAMB(J)   = NWRM
 7    CONTINUE
C
C                  IF THIS HIT IS THE LAST , SET FLAG
C
 4    IF(IHIT.LT.NHITS) RETURN
      END  = .TRUE.
      RETURN
      END
