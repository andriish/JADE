C   29/03/84 806071705  MEMBER NAME  MUCUTS   (JADEMUS)     FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE MUCUTS
C-----------------------------------------------------------------------
C
C LAST CHANGE 17.00  07/06/88   J. HAGEMANN  - SET RMSZUP YEAR DEPEND.
C      CHANGE 16.00  30/03/84   CHRIS BOWDERY - INCREASE ARRAY SIZES
C      CHANGE 16.00  25/02/84   HUGH MCCANN   - TYPE 9 MUONS FLAGGED
C      CHANGE 20.50  23/01/84   CHRIS BOWDERY - COUNT MUONS AND MU EVNTS
C      CHANGE 15.00  23/01/84   TIM GREENSHAW - ERROR OUTPUT ALTERED
C      CHANGE 11.00  07/12/83   HUGH MCCANN  - TYPE 5 MUONS FLAGGED
C      CHANGE 03.15  14/07/83   HUGH MCCANN  - CHANGE ORDER OF CUTS.
C      CHANGE 17.10  11/10/82   HUGH MCCANN  - CLEAR FLAG FOR ALL TRACKS
C
C
C      SUBROUTINE TO ANALYSE MU RESULTS.        HUGH MCCANN 02/02/82 .
C
C      AT PRESENT TYPES 1, 5 AND 9 "GOOD" MUONS ARE DEFINED.
C
C-----------------------------------------------------------------------
C
C  NMHTR(TRACK NO.)    = NO. OF HITS IN THE BEST HIT PERMN FOR TRACK.
C  IHTR(TR#,1..NMHTR)  = HIT NO. OF ASS'D HIT.
C  PROBTR(TRACK NO.)   = PROB OF TRACK.
C
C  IMUCAN STORES THE PATR TRACK NOS. OF THE SELECTED CANDIDATE MUONS.
C
C  PMOMTR(TRACK NO.)   = MOMENTUM OF TRACK.
C  XABSTR(  "      )   = NO. OF ABSORPTION LENGTHS PENETRATED ;
C  IQUTR (  "      )   = TRACK QUALITY ;
C  MISHIT(  "      )   = NO. OF INEFFICIENT LAYERS ON TRACK .
C
C
C       N.B.    NOW THAT THE NO. OF ARRAYS HAS GROWN QUITE LARGE, IT
C               WOULD BE A GOOD IDEA TO JUST USE THE STORED TRACK NO.
C               AS A POINTER AT THE TRACK MODIFYING STAGE, THUS SAVING
C               SOME SPACE.
C
C CUTP    = THE LOWER LIMIT ON TRACK MOMENTA (GEV/C);
C CUTABS  = THE LOWER LIMIT ON NO. OF ABSORPTION LENGTHS PENETRATED
C                                 TO THE LAST ASSOCIATED HIT;
C CUTPRO  = THE LOWER LIMIT ON CHI**2 PROBABILITY ;
C CUTLEV  = THE LOWER LIMIT ON THE MAGNETIC LEVER ARM IN JETCHAMBER (MM)
C NHXYLO  = THE LOWER LIMIT ON NO. OF HITS IN XY PATREC FIT;
C NHZLO   = THE LOWER LIMIT ON NO. OF HITS IN RZ PATREC FIT;
C RMSZUP  = THE UPPER LIMIT ON THE RMS OF THE RZ PATREC FIT (MM).
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL MUEVNT
C
C                            COMMONS.
C
#include "cmubcs.for"
#include "cmustat.for"
C
      COMMON / CMUPRN / MUPRIN
      COMMON / CMUIEV / IEV,NEV,IRD,KRUN,KREC,
     +                  ISECS,IMINS,IHOURS,IDAY,IMONTH,IYEAR
C
      DIMENSION NMHTR(60),PROBTR(60),IHTR(60,20),IMUCAN(10),PMOMTR(60),
     +          XABSTR(60),IQUTR(60),MISHIT(60),NOLAYR(60)
C
      DATA CUTP/1.8/,CUTABS/4.8/,CUTPRO/0.01/,CUTLEV/290./,
     +     NHXYLO/17/,NHZLO/10/,RMSZUP/90./
C
      DATA IYEARO / -1111 /
C
C------------------  C O D E  ------------------------------------------
C
      MUEVNT = .FALSE.
C
      IF( IYEAR .EQ. IYEARO ) GOTO 100
         IYEARO = IYEAR
         RMSZUP = 90.0
         IF( IYEAR .EQ. 1986 ) RMSZUP = 180.0
  100 CONTINUE
C                            PATREC
C
      IPPATR = IDATA(IBLN('PATR'))
      ISTART = IDATA(IPPATR+1) + 1
      NWTRK  = IDATA(IPPATR+3)
      IPT    = IPPATR+ISTART - 1
C
C                            MUON ANALYSIS BANKS
C
      IBANK  = 0
      CALL CLOC(IMUR10,'MUR1',IBANK)
      NMHITS = IDATA(IMUR10+1)
C
      IBANK  = 0
      CALL CLOC(IMUR20,'MUR2',IBANK)
      IF( IMUR20 .LE. 0 ) GO TO 111
      NUMTRK = IDATA(IMUR20+1)
      NMUW   = IDATA(IMUR20+2)
      NTPH   = IDATA(IMUR20+3)
C
      IBANK  = 1
      CALL CLOC(IMUR21,'MUR2',IBANK)
      IF( IMUR21 .LE. 0 ) GO TO 11
C
      IBANK  = 2
      CALL CLOC(IMUR22,'MUR2',IBANK)
      IF( IMUR22 .LE. 0 ) GO TO 11
      J22    = 2 * IMUR22
C
      IBANK  = 3
      CALL CLOC(IMUR23,'MUR2',IBANK)
      IF( IMUR23 .LE. 0 ) GO TO 11
      J23    = 2 * IMUR23
C
      CALL VZERO( NMHTR,  60 )
      CALL VZERO( IHTR, 1200 )
      CALL VZERO( PROBTR, 60 )
      CALL VZERO( IMUCAN, 10 )
C
      CALL VZERO( PMOMTR, 60 )
      CALL VZERO( XABSTR, 60 )
      CALL VZERO( IQUTR,  60 )
      CALL VZERO( MISHIT, 60 )
      CALL VZERO( NOLAYR, 60 )
C
      NOMU = 0
C
C                            LOOP ROUND TRACKS TO FIND MUON CANDIDATES
C
      J=IMUR21
      J2=J+J
C
C                            PREVENT ARRAY OVERFLOW
C
      IF( NUMTRK .LE. 60 ) GO TO 1
        CALL MUERRY('MUCUTS',NUMTRK,' = NO. OF PATR TRACKS, EXCEEDS LIMI
     +T OF 60. EXCESS IGNORED !^')
        NUMTRK = 60
C
   1  DO  99  I = 1,NUMTRK
C
C                            FIRST, CLEAR HALF-WORD 53 FOR EACH  TRACK.
C
        HDATA(J2+53) = 0
C
C                            CUT ON QUALITY.
C
        IQUAL = IDATA(J+6)
C
        IF( IQUAL .LE. 0 ) GO TO 9
C
        CHIP  = ADATA(J+11)
        XABS  = ADATA(J+15)
C
        P     = ADATA(J+38)
C
C                            CUT ON MOMENTUM.
C
        IF( P .LT. 1.2 ) GO TO 9
        PMOMTR(I) = P
        IQUTR(I)  = IQUAL
        XABSTR(I) = XABS
        MISHIT(I) = IDATA(J+4)/10000
        INF       = IDATA(J+4)/100
        NOLAYR(I) = MOD(INF,100)/2
C
        PROBTR(I) = CHIP
C
C                            PICK UP PATREC DATA.
C
        NHXY = IDATA(IPT+24)
        NHZ  = IDATA(IPT+33)
        RMSZ = ADATA(IPT+32)
C
        X1   = ADATA(IPT+5)
        Y1   = ADATA(IPT+6)
        Z1   = ADATA(IPT+7)
C
        X2   = ADATA(IPT+12)
        Y2   = ADATA(IPT+13)
        Z2   = ADATA(IPT+14)
C
        R2   = SQRT(X2**2 + Y2**2)
        R1   = SQRT(X1**2 + Y1**2)
C
C                            CALCULATE LENGTH OF LEVER ARM .
C
        RLEV = R2 - R1
        IF( RLEV .LT. CUTLEV  .OR.
     +      NHXY .LT. NHXYLO  .OR.
     +      NHZ  .LT. NHZLO   .OR.
     +      RMSZ .GT. RMSZUP        ) GO TO 9
C
C                            TRACK HAS SURVIVED CUTS SO FAR.  COUNT IT
C
        NOMU         = NOMU + 1
C
C                            PREVENT OVERFLOW AFTER 10 CANDIDATES
C
        IF( NOMU .LE. 10 ) GO TO 2
          CALL MUERRY('MUCUTS',I,' = PATR TRACK FOUND TO BE A MUON CANDI
     +DATE THAT EXCEEDS LIMIT OF 10^')
          NOMU = 10
          GO TO 99
C
   2    IMUCAN(NOMU) = I
C
C                            DIG AROUND IN MUR2/2 AND MUR2/3 .
C
        IP22J = J22
        IP23J = J23
C
        DO  22  JHIT = 1,NMHITS
          DO  21  INTPH = 1,NTPH
            ITK      = HDATA(IP22J+INTPH)
            IF( IABS(ITK) .NE. I ) GO TO 21
            IAMB     = HDATA(IP23J+INTPH)
            IF( IABS(MOD(IAMB,10)) .NE. 2 ) GO TO 21
            NMHTR(I) = NMHTR(I) + 1
            IHTR(I,NMHTR(I)) = JHIT
   21     CONTINUE
          IP22J = IP22J + NTPH
          IP23J = IP23J + NTPH
   22   CONTINUE
C+++
C       WRITE(6,236)I,NMHTR(I),(IHTR(I,J),J=1,10)
C 236   FORMAT('0 TRACK/NO. OF HITS/HIT NOS. :',I3,I5,5X,10I3)
C+++
C
    9   J   = J   + NMUW
        J2  = J   + J
        IPT = IPT + NWTRK
C+++
C       WRITE(6,995) I,NUMTRK
C 995   FORMAT(3X,I6,' OUT OF ',I6)
C+++
   99 CONTINUE
C
C                            IF NO MUON CANDIDATES , RETURN.
C
      IF( NOMU .EQ. 0 ) GO TO 500
C
C                            IF ONLY 1 MUON CANDIDATE, GO TO
C                            BANK-MODIFYING STAGE
C
      IF( NOMU .EQ. 1 ) GO TO 400
C
C
C        OTHERWISE, LOOK INTO SHARING OF HITS . THE FOLLOWING HIERARCHY
C        OF SELECTIONS IS USED:
C
C        1)   IF 2 TRACKS SHARE HITS AND  1  HAS  MORE  HITS  THAN  THE
C             OTHER, THEN THAT ONE IS SELECTED;
C
C        2)   IF 2 TRACKS SHARE HITS AND THEY HAVE EQUAL NOS. OF  HITS,
C             THEN THE ONE WITH THE GREATER PROBABILITY IS SELECTED.
C
C        IN EITHER CASE, THE  'LOSER'  HAS  ITS  FLAG  IN  IMUCAN  MADE
C        NEGATIVE. A TRACK WHICH LOSES ONE SHOOT-OUT IS STILL  USED  IN
C        FURTHER SHOOT-OUTS, BUT IT WILL NEVER HAVE ITS FLAG IN  IMUCAN
C        MADE POSITIVE AGAIN.
C
C
      DO  199  IMU = 1,NOMU
        IF( IMU .EQ. NOMU ) GO TO 199
        I      = IABS(IMUCAN(IMU))
        NHIMU  = NMHTR(I)
        IPLUS1 = IMU + 1
C
        DO  189  JMU = IPLUS1,NOMU
          J      = IABS(IMUCAN(JMU))
          NHJMU  = NMHTR(J)
          NSHARE = 0
C
          DO  179  K = 1,NHIMU
            DO  169  L = 1,NHJMU
              IF( IHTR(I,K) .NE. IHTR(J,L) ) GO TO 169
              NSHARE = NSHARE + 1
  169       CONTINUE
  179     CONTINUE
C
C+++
C         WRITE(6,1796)I,J,NSHARE
C1796     FORMAT('0 TRACKS ',I3,' *',I3,' SHARE ',I3,' HITS.')
C+++
          IF( NSHARE .EQ. 0 ) GO TO 189
          LOSER = IMU
          IF( NHIMU .EQ. NHJMU ) GO TO 185
            IF( NHIMU .GT. NHJMU ) LOSER = JMU
            GO TO 187
C
  185       IF( PROBTR(I) .GT. PROBTR(J) ) LOSER = JMU
C
  187     IF( IMUCAN(LOSER) .GT. 0 ) IMUCAN(LOSER) = - IMUCAN(LOSER)
C
  189   CONTINUE
  199 CONTINUE
C
C
C        FINAL STAGE  :  MODIFY  MUR2/1  BANK  FOR  THE  SELECTED  MUON
C        CANDIDATES. FOR A SELECTED MUON , PUT I*2 WORD  53  OF  MUR2/1
C        EQUAL TO 1/5/9. OTHERWISE, IT REMAINS ZERO.
C
C
  400 DO  450  I = 1,NOMU
        IF( IMUCAN(I) .LE. 0 ) GO TO 450
C
        ITRAK = IABS(IMUCAN(I))
C
        IF(     PROBTR(ITRAK) .LT. CUTPRO
     +     .OR. PMOMTR(ITRAK) .LT. CUTP
     +     .OR. IQUTR(ITRAK)  .GE. 100
     +     .OR. XABSTR(ITRAK) .LT. CUTABS  )  GO TO 450
C
C+++
C       WRITE(6,445)IMUCAN(I)
C 445   FORMAT('0     SELECTED MUON TRACK ',I4)
C+++
C
C                            THIS IS A MUON EVENT SO SET FLAG
C
        MUEVNT = .TRUE.
C
        HDATA( 2*IMUR21 + 2*NMUW*(IMUCAN(I)-1) + 53 ) = 1
C
C                            MARK TRACKS WITH HIGHER NO. OF ABS LENGTHS
C                            AND NO MISSING HITS.
C
        IF(MISHIT(ITRAK).GT.0  .OR.  XABSTR(ITRAK) .LT. 5.8 ) GO TO 447
          HDATA( 2*IMUR21 + 2*NMUW*(IMUCAN(I)-1) + 53 ) = 5
C
C                            MARK TRACKS  WITH  AT  LEAST  THREE  OUTER
C                            LAYERS....TYPE 9.
C                            INCREMENT STATISTICS FOR TYPE 9
C
          IF( NOLAYR(ITRAK) .LT. 3 ) GO TO 446
            HDATA( 2*IMUR21 + 2*NMUW*(IMUCAN(I)-1) + 53 ) = 9
            NMU(14) = NMU(14) + 1
            GO TO 450
C
C                            INCREMENT STATISTICS FOR TYPE 5
C
  446       NMU(13) = NMU(13) + 1
            GO TO 450
C
C                            INCREMENT STATISTICS FOR TYPE 1
C
  447   NMU(12) = NMU(12) + 1
C
  450 CONTINUE
C
C                            COUNT THE NUMBER OF MUON EVENTS
C
  500 IF( MUEVNT ) NMU(11) = NMU(11) + 1
      GO TO 550
C
C                            PROBLEMS , PROBLEMS , .......
C
C                            RECORD OCCURRENCES OF MISSING  MUR2/0  FOR
C                            STATISTICS
C
  111 NMU(5) = NMU(5) + 1
      GO TO 112
C
C                            CHECK IF MU HITS AND PATR TRACKS  IN  THIS
C                            EVENT.IF BOTH YES THEN THIS IS A REAL BANK
C                            MISSING ERROR.INCREMENT STATISTICS COUNTER
C
   11 MUHITS = IDATA(IMUR10 + 1)
      IF( .NOT. ((MUHITS .GT. 0) .AND. (NUMTRK .GT. 0)) ) GO TO 550
      NMU(6) = NMU(6) + 1
  112 CALL MUERRY('MUCUTS',IBANK,' = I, ''MUR2/I'' EXPECTED BUT NOT FOUN
     +D. SERIOUS ERROR^')
C
C
  550 CONTINUE
      RETURN
      END
