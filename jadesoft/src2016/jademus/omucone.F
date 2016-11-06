C   28/05/82 206291134  MEMBER NAME  MUCONE   (JADEMUS)     FORTRAN
C   27/05/82 205271450  MEMBER NAME  MUCONE   (S)           FORTRAN
C   27/10/81 110271005  MEMBER NAME  MUCONE   (JADEMUS1)    FORTRAN
C   23/10/81 110231703  MEMBER NAME  MUCONE   (S)           FORTRAN
C   17/06/81 106170952  MEMBER NAME  MUCONE   (JADEMUS1)    FORTRAN
C   12/06/81 106151023  MEMBER NAME  MUCONE   (S)           FORTRAN
C   23/09/80 104100731  MEMBER NAME  MUCONE   (JADEMUS)     FORTRAN
C   26/09/79 C9092601   MEMBER NAME  MUCONE   (S)           FORTRAN
C LAST CHANGE 11.40 29/06/82 HUGH MCCANN  - ADD /CMUED/ .
C      CHANGE 10.00 28/05/82 HUGH MCCANN  - REDUCE CORE REQUIREMENTS FOR
C                                          PRODUCTION OF UPDATE DATASETS
C      CHANGE 10.00 27/10/81 HUGH MCCANN  - CORRECT BUG.
C      CHANGE 08.55 12/06/81 HUGH MCCANN  - TO ACCOM NEW JADE SYSTEM.
C      CHANGE 07.30 10/04/81 HUGH MCCANN  - JADEMUS UPDATE.
C      CHANGE 19.39 23/09/80 J.ALLISON - TO ADD ACTW.
C      CHANGE 21.07 20/09/79 HARRISON PROSPER
      SUBROUTINE MUCONE(LUNMUE,IPRINT,IFLAG)
C
      IMPLICIT INTEGER*2 (H)
C
C-----------------------------------------------------------------------
C
C EDITS MUON CALIBRATION DATA.
C UNLESS LUNMUE=0, READS EDITS FROM LOGICAL UNIT LUNMUE IN /CUNIT/.
C PRINTING SUPRESSED IF IPRINT.LT.2.
C FULL PRINTING IF IPRINT.GE.10.
C THE MODIFIED HMUCAL VALUES & THEIR ADDRESSES ARE SAVED IN /MUED/
C IF IFLAG=0 .IF A COMPLETE NEW SET OF CALIBRATION IS WANTED,
C PUT IFLAG=1 .
C
C-----------------------------------------------------------------------
C
C COMMONS.
#include "cmucalib.for"
#include "cmuedwrk.for"
#include "cmued.for"
C
C-----------------------------------------------------------------------
C
C DATA INITIALISATIION STATEMENTS,ETC.
C
      DIMENSION CMNT(10)
C
      DIMENSION AQ(20)
      DATA AQ/'OVAL','DIST','ANG ','CLLO','CLHI','CTLO','CTHI',
     * 'D1  ','CTW ','VDR ','DTP ','LTP ','LSF ','MCST',
     * 'BLLO','BLHI','BTLO','BTHI','BNLM','ACTW'/
      DATA NQ/20/
C
C-----------------------------------------------------------------------
C
      IF(IPRINT.LT.10)CALL MUMESS('MUCONE',0,'CALLED.^')
      IF(IPRINT.LT.10)CALL MUMESS('MUCONE',NVERSN,DESCRP)
C
C-----------------------------------------------------------------------
C
      IF(IPRINT.LT.10)GO TO 1
      WRITE(6,100)NQ,(AQ(I),I=1,NQ)
 100  FORMAT('1MUCONE CALLED TO EDIT MU CALIBRATION DATA.'/
     * '0THE FOLLOWING DATA CAN BE CHANGED BY SPECIFYING IN COLUMNS 1-4.
     *'/
     *'0',20A5/(20A5))
      WRITE(6,101)
 101  FORMAT('0FIRST GIVE NEW VERSION NUMBER AND DESCRIPTION (TERMINATED
     * WITH ''^'') IN FORMAT(I5,5X,15A4).'/
     *'0THE CHANGES ARE SPECIFIED BY...'/
     *' WHICH,I1,I2,I3,INEW IN FORMAT(A4,1X,3I5,I10).  E.G.'/
     *' DIST     5    7          -2378 SPECIFIES THAT THE NORMAL DISTANC
     *E TO FRAMES 5-7 IS TO BE CHANGED TO -2378.'/
     *'0DEFAULTS ARE AS FOLLOWS...'/
     *'   I1   I2   I3   IFRCH1 IFRCH2 IHIT1 IHIT2'/
     *'    L    M    0       L      M     1     4'/
     *'    L    M    N       L      M     N     N'/
     *'    L    0            L      L'/
     *'    0                 1    ALL'/
     *' WHERE IFRCH1,IFRCH2 ARE THE FIRST AND LAST FRAME (OR CHAMBER, OR
     * FACE) NUMBERS,'/
     *' AND IHIT1,IHIT2 ARE THE FIRST AND LAST HIT (OR BLOCK BOUNDARY) N
     *UMBERS WHENEVER AND/OR WHICHEVER IS RELEVENT.')
 1    CONTINUE
C
C-----------------------------------------------------------------------
C
C READ NEW VERSION DESCRIPTION.(TERMINATE WITH ^.)
      IF(IPRINT.GE.2)WRITE(6,106)NVERSN,DESCRP
 106  FORMAT('0OLD VERSION',I6,5X,15A4)
      IF(LUNMUE.EQ.0)GO TO 98
      READ(LUNMUE,200,END=98)I1,DESCRP
 200  FORMAT(I5,5X,15A4)
      IF(I1.LE.0)GO TO 15
      NVERSN=I1
      GO TO 16
 15   CONTINUE
      NVERSN=NVERSN+1
 16   CONTINUE
      IF(IPRINT.GE.2)WRITE(6,102)NVERSN,DESCRP
 102  FORMAT('0NEW VERSION',I6,5X,15A4)
C
C-----------------------------------------------------------------------
C
C PRINT HEADING.
      IF(IPRINT.GE.2)WRITE(6,103)
 103  FORMAT('0NAME      I1     I2     I3 IFRCH1 IFRCH2  IHIT1  IHIT2
     *   INEW')
C
C-----------------------------------------------------------------------
C
C READ EDIT CARDS.
 4    CONTINUE
      READ(LUNMUE,201,END=99)WHICH,I1,I2,I3,INEW,CMNT
 201  FORMAT(A4,1X,3I5,I10,10A4)
      IFRCH1=1
      IHIT1=1
      IHIT2=4
C
C FIND WHICH ONE.
      DO 5 I=1,NQ
      IF(WHICH.EQ.AQ(I))GO TO 6
 5    CONTINUE
      IF(IPRINT.GE.1)WRITE(6,105)WHICH,I1,I2,I3,INEW,CMNT
 105  FORMAT(1X,A4,1X,3I7,28X,I10,10A4)
      CALL MUERRY('MUCONE',0,'UNRECOGNISED EDIT CARD.^')
      GO TO 4
C
C-----------------------------------------------------------------------
C
C EDIT RECOGNISED.
 6    CONTINUE
      IALPH=I
      IF(I1.NE.0)GO TO 11
C
C I1.EQ.0 - SET IFRCH2 ACCORDINGLY.
      GO TO (7,8,8,8,8,8,8,9,9,9,9,9,9,9,10,10,10,10,10,9),I
 7    CONTINUE
      IFRCH2=5
      GO TO 12
 8    CONTINUE
      IFRCH2=NFRAMS
      GO TO 12
 9    CONTINUE
      IFRCH2=NCHAMS
      GO TO 12
 10   CONTINUE
      IFRCH2=6
      IHIT2=6
      GO TO 12
C
C I1.NE.0 - SET IFRCH2 ACCORDINGLY.
 11   CONTINUE
      IFRCH1=I1
      IFRCH2=I2
      IF(I2.LE.0)IFRCH2=I1
C
C SET NUMBER OF HITS IN CASE REQUIRED.
 12   CONTINUE
      IF(I3.EQ.0)GO TO 13
      IHIT1=I3
      IHIT2=I3
C
 13   CONTINUE
      IF(IPRINT.GE.2)WRITE(6,104)WHICH,I1,I2,I3,IFRCH1,IFRCH2,
     * IHIT1,IHIT2,INEW,CMNT
 104  FORMAT(1X,A4,1X,7I7,I10,10A4)
C
C-----------------------------------------------------------------------
C
C EXECUTE EDITS.
 14   CONTINUE
      GO TO (1001,1002,1003,1004,1005,1006,1007,1008,1009,1010,
     * 1011,1012,1013,1014,1015,1016,1017,1018,1019,1020),I
 1000 CONTINUE
      CALL MUMESS('MUCONE',I,'EDIT NOT IMPLEMENTED.^')
      GO TO 4
C
C-----------------------------------------------------------------------
C
C OVERALL JADE UNIT DISPLACEMENTS.
 1001 CONTINUE
      DO 2001 I=IFRCH1,IFRCH2
      HOVALL(I)=INEW
      IF(IFLAG.EQ.0)CALL MUCHAN(I)
 2001 CONTINUE
      GO TO 4
C
C-----------------------------------------------------------------------
C
C ORIGIN TO MEAN CHABER PLANE DISTANCE.
 1002 CONTINUE
      DO 2002 I=IFRCH1,IFRCH2
      HDIST(I)=INEW
      IF(IFLAG.EQ.0)CALL MUCHAN(I)
 2002 CONTINUE
      GO TO 4
C
C-----------------------------------------------------------------------
C
C ANGULAR TWIST OF CHAMBERS.
 1003 CONTINUE
      DO 2003 I=IFRCH1,IFRCH2
      HANG(I)=INEW
      IF(IFLAG.EQ.0)CALL MUCHAN(I)
 2003 CONTINUE
      GO TO 4
C
C-----------------------------------------------------------------------
C
C LOWER LONGITUDINAL SENSITIVE REGION LIMITS.
 1004 CONTINUE
      DO 2004 I=IFRCH1,IFRCH2
      HCLLO(I)=INEW
      IF(IFLAG.EQ.0)CALL MUCHAN(I)
 2004 CONTINUE
      GO TO 4
C
C-----------------------------------------------------------------------
C
C UPPER LONGITUDINAL SENSITIVE REGION LIMITS.
 1005 CONTINUE
      DO 2005 I=IFRCH1,IFRCH2
      HCLHI(I)=INEW
      IF(IFLAG.EQ.0)CALL MUCHAN(I)
 2005 CONTINUE
      GO TO 4
C
C-----------------------------------------------------------------------
C
C LOWER TRANSVERSE SENSITIVE REGION LIMITS.
 1006 CONTINUE
      WRITE(6,2006)
 2006 FORMAT(' **** CTLO AND CTHI FIXED UP AUTOMATICALLY ****')
      GO TO 4
C
C-----------------------------------------------------------------------
C
C UPPER TRANSVERSE SENSITIVE REGION LIMITS.
 1007 CONTINUE
      WRITE(6,2006)
      GO TO 4
C
C-----------------------------------------------------------------------
C
C OFFSET FROM FRAME MEDIAN PLANE.
 1008 CONTINUE
      DO 2008 I=IFRCH1,IFRCH2
      HD1(I)=INEW
      IF(IFLAG.EQ.0)CALL MUCHAN(I)
 2008 CONTINUE
      GO TO 4
C
C-----------------------------------------------------------------------
C
C WIRE COORDINATE.
 1009 CONTINUE
      DO 2009 I=IFRCH1,IFRCH2
      HCTW(I)=INEW
      IF(IFLAG.EQ.0)CALL MUCHAN(I)
 2009 CONTINUE
      GO TO 4
C
C-----------------------------------------------------------------------
C
C DRIFT VELOCITY.
 1010 CONTINUE
      DO 2010 I=IFRCH1,IFRCH2
      HVDRFT(I)=INEW
      IF(IFLAG.EQ.0)CALL MUCHAN(I)
 2010 CONTINUE
C
C MAKE OLD HVDR EQUAL TO AVERAGE.
C IF A COMPLETE NEW SET OF CALIBRATION DATA IS NOT BEING PRODUCED,
C DON'T UPDATE THE AVERAGE DRIFT VELOCITY.
      IF(IFLAG.EQ.0)GO TO 4
      IVDR=0
      DO 3010 I=1,NCHAMS
      IVDR=IVDR+HVDRFT(I)
 3010 CONTINUE
      HVDR=IVDR/NCHAMS+.5
C
      GO TO 4
C
C-----------------------------------------------------------------------
C
C DRIFT TIME PEDESTAL.
 1011 CONTINUE
      DO 2011 I=IFRCH1,IFRCH2
      HDTP(I)=INEW
      IF(IFLAG.EQ.0)CALL MUCHAN(I)
 2011 CONTINUE
      GO TO 4
C
C-----------------------------------------------------------------------
C
C LONGITUDINAL TIME PEDESTAL.
 1012 CONTINUE
      DO 2012 I=IFRCH1,IFRCH2
      HLTP(I)=INEW
      IF(IFLAG.EQ.0)CALL MUCHAN(I)
 2012 CONTINUE
      GO TO 4
C
C-----------------------------------------------------------------------
C
C LONGITUDINAL SCALE FACTORS.
 1013 CONTINUE
      DO 2013 I=IFRCH1,IFRCH2
      DO 2013 J=IHIT1,IHIT2
      HLSF(J,I)=INEW
      K=4*(I-1)+J
      IF(IFLAG.EQ.0)CALL MUCHAN(K)
 2013 CONTINUE
      GO TO 4
C
C-----------------------------------------------------------------------
C
C CHAMBER STATUS.
 1014 CONTINUE
      DO 2014 I=IFRCH1,IFRCH2
      HMCSTA(I)=INEW
      IF(IFLAG.EQ.0)CALL MUCHAN(I)
 2014 CONTINUE
      GO TO 4
C
C-----------------------------------------------------------------------
C
C LOWER LONGITUDINAL BLOCK LIMIT.
 1015 CONTINUE
      DO 2015 I=IFRCH1,IFRCH2
      HBLLO(I)=INEW
      IF(IFLAG.EQ.0)CALL MUCHAN(I)
 2015 CONTINUE
      GO TO 4
C
C-----------------------------------------------------------------------
C
C UPPER LONGITUDINAL BLOCK LIMIT.
 1016 CONTINUE
      DO 2016 I=IFRCH1,IFRCH2
      HBLHI(I)=INEW
      IF(IFLAG.EQ.0)CALL MUCHAN(I)
 2016 CONTINUE
      GO TO 4
C
C-----------------------------------------------------------------------
C
C LOWER TRANSVERSE BLOCK LIMIT.
 1017 CONTINUE
      DO 2017 I=IFRCH1,IFRCH2
      HBTLO(I)=INEW
      IF(IFLAG.EQ.0)CALL MUCHAN(I)
 2017 CONTINUE
      GO TO 4
C
C-----------------------------------------------------------------------
C
C UPPER TRANSVERSE BLOCK LIMIT.
 1018 CONTINUE
      DO 2018 I=IFRCH1,IFRCH2
      HBTHI(I)=INEW
      IF(IFLAG.EQ.0)CALL MUCHAN(I)
 2018 CONTINUE
      GO TO 4
C
C-----------------------------------------------------------------------
C
C NORMAL BLOCK LIMITS.
 1019 CONTINUE
      DO 2019 I=IFRCH1,IFRCH2
      DO 2019 J=IHIT1,IHIT2
      K=6*(I-1)+J
      HBNLIM(K)=INEW
      IF(IFLAG.EQ.0)CALL MUCHAN(K)
 2019 CONTINUE
      GO TO 4
C
C-----------------------------------------------------------------------
C
C ADD CONSTANT TO WIRE COORDINATE.
 1020 CONTINUE
      DO 2020 I=IFRCH1,IFRCH2
      HCTW(I)=HCTW(I)+INEW
      IF(IFLAG.EQ.0)CALL MUCHAN(I)
 2020 CONTINUE
      GO TO 4
C
C-----------------------------------------------------------------------
C
C ERROR CONDITIONS,ETC.
C
 98   CONTINUE
      CALL MUMESS('MUCONE',NVERSN,'NO CHANGES.^')
      GO TO 99
C
C-----------------------------------------------------------------------
C
 99   CONTINUE
C     FIX UP CTLO AND CTHI.
      CALL MUMESS('MUCONE',0,'CTLO AND CTHI FORCED TO BE 150 MM FROM WIR
     *E OF EDGE CHAMBERS.^')
      DO 199 J=1,NFRAMS
        I1=HAC(J)
        I2=HAL(J)
        HCTLO(J)=HCTW(I1)-150
        IALPH=6
        IF(IFLAG.EQ.0)CALL MUCHAN(J)
        HCTHI(J)=HCTW(I2)+150
        IALPH=7
        IF(IFLAG.EQ.0)CALL MUCHAN(J)
  199 CONTINUE
      REWIND LUNMUE
      RETURN
      END
