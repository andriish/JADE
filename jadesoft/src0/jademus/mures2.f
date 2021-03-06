C   16/12/80 104100758  MEMBER NAME  MURES2   (JADEMUS)     FORTRAN
C   26/03/80 010121510  MEMBER NAME  MURES2   (JADEMUS1)    FORTRAN
C LAST CHANGE 07.30 10/04/81 HUGH MCCANN  - JADEMUS UPDATE.
C        LAST CHANGE 18.40  17/12/80  HUGH MCCANN.
      SUBROUTINE MURES2(I0,I1,I2,I3,I4)
C
C  THIS ROUTINE CONTROLS THE PRINTING OF THE MUON ANALYSIS
C  PHILOSOPHY2 RESULTS BANKS.IF ANY OF I0,I1 ETC IS
C  SET TO 1 , THE CORRESPONDING MUR2 BANK IS PRINTED OUT.
C  (N.B. AT PRESENT (17/12/80) ROUTINES EXIST ONLY TO
C  OUTPUT BANKS 0,1,2 AND 3 . IT IS INTENDED TO ADD THE
C  NECESSARY ROUTINES FOR THE OTHERS AS THE NEED ARISES.)
C
      IMPLICIT INTEGER*2 (H)
#include "cmubcs.for"
      COMMON /CMUIEV/IEV,NEV,IRD,KRUN,KREC,
     *ISECS,IMINS,IHOURS,IDAY,IMONTH,IYEAR
C PICK UP HEADER POINTER.
      IPHEAD=IDATA(IBLN('HEAD'))
C PICK UP RUN NUMBER ,RECORD NUMBER ETC
      ISECS = HDATA(2*IPHEAD+3)
      IMINS = HDATA(2*IPHEAD+4)
      IHOURS= HDATA(2*IPHEAD+5)
      IDAY  = HDATA(2*IPHEAD+6)
      IMONTH= HDATA(2*IPHEAD+7)
      IYEAR = HDATA(2*IPHEAD+8)
      KRUN  = HDATA(2*IPHEAD+10)
      KREC  = HDATA(2*IPHEAD+11)
C
C  WRITE EVENT HEADER.
C
      WRITE(6,1000)KRUN,KREC,IEV,IHOURS,IMINS,
     *ISECS,IDAY,IMONTH,IYEAR
 1000 FORMAT('0**********',2X,'RUN',I6,2X,'RECORD',I6,
     *2X,'EVENT (FILE)',I6,2X,'TIME =',I3,':',I2,':',I2,
     *2X,'DATE =',I3,':',I2,':',I4)
C
C LOOK THROUGH ARGUMENTS AND PRINT OUT REQUESTED BANKS.
C
      IF(I0.EQ.1)GO TO 10
  100 CONTINUE
      IF(I1.EQ.1)GO TO 11
  110 CONTINUE
      IF(I2.EQ.1)GO TO 12
  120 CONTINUE
      IF(I3.EQ.1)GO TO 13
  130 CONTINUE
      IF(I4.EQ.1)GO TO 14
  140 CONTINUE
      GO TO 200
C
C
   10 CONTINUE
C---------OUTPUT BANK 0 .
      CALL MUPR20
      GO TO 100
   11 CONTINUE
C
C--------OUTPUT BANK 1 . FIRST , WRITE HEADER.
      WRITE(6,1099)
 1099 FORMAT('0 1        38       4     6     11      14 ',
     *'   15    21      22     24     25      26     30   31  ',
     *' 32   33   34   35   36   37')
      WRITE(6,1101)
 1101 FORMAT(' TRK CH MOMENTUM HIT INF QUAL CHPRMU  ELOSS',
     *2X,'NABS  PIDK   PISNK   KDK   MUPROB   PIPROB FPI   FK   ',
     *'FP  FAP  FPI2 FK2  FP2  FAP2')
      WRITE(6,1102)
 1102 FORMAT(' --- -- -------- ------- ---- ------  -----',
     *2X,'----  ----- ------  ------ ------  ------- ---- ---- -',
     *'--- ---- ---- ---- ---- ----')
      CALL MUPR21
      GO TO 110
   12 CONTINUE
C--- OUTPUT BANK 2. FIRST, WRITE HEADER.
      WRITE(6,1201)
 1201 FORMAT('0 HIT   ASSIGNED TO FOLLOWING TRACK(S):')
      WRITE(6,1202)
 1202 FORMAT('  ---   --------------------------------')
      CALL MUPR22
      GO TO 120
   13 CONTINUE
C--- OUTPUT BANK 3. FIRST, WRITE HEADER.
      WRITE(6,1301)
 1301 FORMAT('0 HIT   FOLLOWING AMBIGUITIES WERE USED IN ABOVE:')
      WRITE(6,1302)
 1302 FORMAT('  ---   ------------------------------------------')
      CALL MUPR23
      GO TO 130
   14 CONTINUE
      GO TO 140
  200 CONTINUE
      RETURN
      END
