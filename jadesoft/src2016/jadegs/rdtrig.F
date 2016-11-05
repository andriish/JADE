C   24/11/83 804051406  MEMBER NAME  RDTRIG   (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE RDTRIG
C-----------------------------------------------------------------------
C
C   AUTHOR    E. ELSEN    10/05/82 :  TRIGGER SETTING
C
C        MOD  E. ELSEN    11/05/82 :
C        MOD  J. OLSSON   05/08/83 :  DIFFERENT VERSION CHOICE
C        MOD  J. OLSSON   10/08/83 :
C        MOD  C. BOWDERY  23/11/83 :  CADMIN MACRO USED - NFLAGS
C        MOD  C. BOWDERY  08/02/84 :  CADMIN MACRO FROM PATRECSR NOW
C        MOD  C. BOWDERY  17/09/84 :  S/R PRSTAT MOVED TO MEMBER PRSTAT
C        MOD  J. OLSSON 18/07 - 24/08 86:  NEW TRIGGER PERIODS 1983-198600001400
C                                NEW PARAMETERS FOR OLD/NEW TRIGGERS
C        MOD  J. OLSSON   26/07/87 :  Z-RESOLUTION FOR 1986 UPDATED
C        MOD  J. OLSSON   03/12/87 :  ZRS CHANGED WHERE IT IS USED!
C        MOD  E ELSEN     11/12/87 :  R-PHI RESOLUTION (2 GAUSS SCHEME)
C             J.HAGEMANN              FOR ID AND VTXC
C        MOD  J.OLSSON    18/12/87 :  DTRSVV UPDATED (J.HAGEMANN)
C   LAST MOD  E ELSEN     28/03/88 :  R-Z   RESOLUTION (2 GAUSS SCHEME)
C
C    THE TRIGGER PARAMETERS ARE KEPT IN COMMON /CTRIGG/
C    THE INNER DETECTOR STATUS IS KEPT IN COMMON /CRDSTA/
C    THE BLOCK DATA SETTING CORRESPONDS TO 1982 STATUS
C
C    THE CALL TO RDDATE WILL OVERWRITE EVENT DATE IN BANK HEAD
C    (WORDS 6-8) AND THE PROPER VERSION IS CHOSEN ACCORDINGLY.
C    THIS GIVES THE POSSIBILITY TO USE THE CALIBRATION CONSTANTS FOR
C    THE CORRESPONDING PERIOD, AS READ IN BY KALIBR/KLREAD.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / BCS / IW(1)
      DIMENSION HW(1)
      EQUIVALENCE ( HW(1), IW(1) )
C
C                            IHIST GIVES DAY,MONTH,YEAR
C                            FROM THIS DATE, THE CORRESPONDING
C                            DETECTOR STATUS AND TRIGGER
C                            CONDITION IS SELECTED
C
      COMMON /CTRIGG/ IHIST(3),NBPTFW,TAGADC,IDUM1(4),HDUM1,
     *  HLGBRT,HLGBST,HLGQT,HLGTOT(4,2),HECAPT(4),HLGTL,HLGTH,HTGTRG,
     *                HDUM2(9),IWIDBS,NRNBSL,NRNBSH,NTOFBS,IDUM2(10),
     *                NTOFC,NTOFC1,NTOFC2,NTBGC,NTBGC1,NTBGC2,
     *                IWCOLL,IWCOLN,IWMPRG,HFMPRL,HFMPRH,HWCOLN,HMPRON,
     *                IWCTBG,NTFCOL,NTFCLN,NTFBS2,IDUM3(9),
     *  HITCLL(3),HITWLL(3),HITSUM(3),HCHAMB(3),HMASK(16,3),HDEADC(10),
     *  HACC1,HACC2,HACC3,HACC4,HACCM,HDUM6,IWIDT2,HACCB1,HACCB2,
     *  HT1ACC,HT1PSP,IDUM4(8)
C
      COMMON / CRDSTA / NDEAD, NCDEAD, HITD(10), HCELLD(10), IPHALG
C
C                            CADMIN CONTAINS NFLAGS(10) AND ELEMENTS 2
C                            AND 3 ARE USED HERE AS FLAGS FOR CURRENT
C                            SMEARING DATE AND 4 AS A PRINTING FLAG.
C
      COMMON / CBIN / TIME(6),ZOF,ZRS,ZL,ZSC,EPSI(3),DOUB(3),IRN(3),
     +                BINDL8(6),RJITT, DLRSLN(3), DLZSLN(3)
      COMMON / CBINV /  DTRSVV, ZRSVV, EFFVV(2), DOUBVV(3), IRNHVV,
     +                  SMPRMV(3)
C
      COMMON / CJDRCH / DRCH(34),
     +                  PEDES, TZERO(3),
     +                  DRIROT(96,2), SINDRI(96,2), COSDRI(96,2),
     +                  DRIVEL(96,2)
C
C
#include "cadmin.for"
C
      DATA HT1A85 /Z'7F07'/, HT1A86 /Z'7F17'/
      DATA HT1P85 /Z'7709'/, HT1P86 /Z'7F09'/
C
C------------------  C O D E  ------------------------------------------
C
C                                           SET DATE OF JOB
      CALL RDDATE
C
      NPHEAD = IW(IBLN('HEAD'))
      IF( NPHEAD .GT. 0 ) GO TO 4771
      WRITE(6,4772)
4772  FORMAT(' * * * RDTRIG CALLED FOR HEADLESS EVENT, PROGRAMMED STOP')
      STOP
4771  IHIST(1) = HW(NPHEAD*2+6)
      IHIST(2) = HW(NPHEAD*2+7)
      IHIST(3) = HW(NPHEAD*2+8)
C
      IYEAR = IHIST(3)
      if (IYEAR.GT. 1900) IYEAR=IYEAR-1900
      write(*,*)'IYEAR=   ',IYEAR
      IYEAR = MOD(IYEAR,100)
      IF(NFLAGS(3).EQ.IYEAR) GO TO 8000
      NFLAGS(3) = IYEAR
      IMONTH = IHIST(2)
C
C                                           ****************************
C                                           *  1979 TRIGGER CONDITIONS *
C                                           ****************************
      IF( IYEAR .GT. 79 ) GO TO 1100
C                                           CHANGES
C  COPLANAR NARROW TOF, 2 FAST TRACKS
            HWCOLN = 0
C  TRACK LOGIC
            HITCLL(1) = 12
            HITCLL(2) = 11
            HITCLL(3) = 12
            HITWLL(1) = 1
            HITWLL(2) = 1
            HITWLL(3) = 1
            HITSUM(1) = 12
            HITSUM(2) = 12
            HITSUM(3) = 12
C  DEAD CELLS IN INNER DETECTOR
            HDEADC(1) = 17
            NCDEAD = 1
            HCELLD(1) = 17
C  MIN. TRACK NUMBER IN TOF + TOTAL LG ENERGY TRIGGER
            HACC1 = 1
C  TOTAL LG ENERGY THRESHOLDS
            HLGTOT(1,1) = 4000
            HLGTOT(2,1) = 2000
            HLGTOT(3,1) = 1000
            HLGTOT(4,1) = 500
C  TAGG THRESHOLDS AND ADC CONVERSION CONSTANT
            HLGTL = 4000
            HLGTH = 8000
            TAGADC = 5.5
C  NR OF RANDOM HITS IN THE RINGS OF THE INNER DETECTOR
            IRN(1) = 20
            IRN(2) = 20
            IRN(3) = 20
C                                           VERSION FOR THIS PERIOD
      IF( NFLAGS(2) .EQ. 1 ) GO TO 8000
         NFLAGS(2) = 1
         GO TO 7900
C
C                                           ****************************
C                                           *  1980 TRIGGER CONDITIONS *
C                                           *     MONTH 1 - 3          *
C                                           ****************************
 1100 IF( IYEAR .GT. 80 ) GO TO 1300
      IF( IMONTH.GT. 3  ) GO TO 1200
C                                           CHANGES
C  COPLANAR NARROW TOF, 2 FAST TRACKS
            HWCOLN = 0
C  TRACK LOGIC
            HITCLL(1) = 12
            HITCLL(2) = 11
            HITCLL(3) = 12
            HITWLL(1) = 1
            HITWLL(2) = 1
            HITWLL(3) = 1
            HITSUM(1) = 12
            HITSUM(2) = 12
            HITSUM(3) = 12
C  DEAD CELLS IN INNER DETECTOR
            HDEADC(1) = 17
            HDEADC(2) = 37
            NCDEAD = 8
            HCELLD(1) = 17
            HCELLD(2) = 37
            HCELLD(3) = 65
            HCELLD(4) = 66
            HCELLD(5) = 73
            HCELLD(6) = 74
            HCELLD(7) = 81
            HCELLD(8) = 82
C  MIN. TRACK NUMBER IN TOF + TOTAL LG ENERGY TRIGGER
            HACC1 = 1
C  TOTAL LG ENERGY THRESHOLDS
            HLGTOT(1,1) = 4000
            HLGTOT(2,1) = 2000
            HLGTOT(3,1) = 1000
            HLGTOT(4,1) = 500
C  TAGG THRESHOLDS AND ADC CONVERSION CONSTANT
            HLGTL = 4000
            HLGTH = 8000
            TAGADC = 5.5
C  NR OF RANDOM HITS IN THE RINGS OF THE INNER DETECTOR
            IRN(1) = 20
            IRN(2) = 20
            IRN(3) = 20
C                                           VERSION FOR THIS PERIOD
      IF( NFLAGS(2) .EQ. 2 ) GO TO 8000
         NFLAGS(2) = 2
         GO TO 7900
C
C                                           ****************************
C                                           *  1980 TRIGGER CONDITIONS *
C                                           *     MONTH 4 - 12         *
C                                           ****************************
 1200 CONTINUE
C                                           CHANGES
C  COPLANAR NARROW TOF, 2 FAST TRACKS
            HWCOLN = 0
C  TRACK LOGIC
            HITCLL(1) = 12
            HITCLL(2) = 11
            HITCLL(3) = 12
            HITWLL(1) = 1
            HITWLL(2) = 1
            HITWLL(3) = 1
            HITSUM(1) = 12
            HITSUM(2) = 12
            HITSUM(3) = 12
C  DEAD CELLS IN INNER DETECTOR
            HDEADC(1) = 17
            HDEADC(2) = 37
            NCDEAD = 6
            HCELLD(1) = 17
            HCELLD(2) = 37
            HCELLD(3) = 65
            HCELLD(4) = 66
            HCELLD(5) = 81
            HCELLD(6) = 82
C  MIN. TRACK NUMBER IN TOF + TOTAL LG ENERGY TRIGGER
            HACC1 = 1
C  TOTAL LG ENERGY THRESHOLDS
            HLGTOT(1,1) = 4000
            HLGTOT(2,1) = 2000
            HLGTOT(3,1) = 1000
            HLGTOT(4,1) = 500
C  TAGG THRESHOLDS AND ADC CONVERSION CONSTANT
            HLGTL = 4000
            HLGTH = 8000
            TAGADC = 5.5
C  NR OF RANDOM HITS IN THE RINGS OF THE INNER DETECTOR
            IRN(1) = 20
            IRN(2) = 20
            IRN(3) = 20
C                                           VERSION FOR THIS PERIOD
      IF( NFLAGS(2) .EQ. 3 ) GO TO 8000
         NFLAGS(2) = 3
         GO TO 7900
C
C                                           ****************************
C                                           *  1981 TRIGGER CONDITIONS *
C                                           ****************************
        
 1300 IF( IYEAR .GT. 81 ) GO TO 1400
C
C  MIN. TRACK NUMBER IN TOF + TOTAL LG ENERGY TRIGGER
            HACC1 = 1
C  MIN. TRACK NR LIMIT FOR TAGG + TOTAL LG ENERGY TRIGGER
            HACC3 = 2
C  MIN. TRACK NR LIMIT FOR NARROW COPLANAR TOF TRIGGER
            HACC4 = 2
C  TOTAL LG ENERGY THRESHOLDS
            HLGTOT(1,1) = 4000
            HLGTOT(2,1) = 2000
            HLGTOT(3,1) = 1000
            HLGTOT(4,1) = 500
C  NR OF RANDOM HITS IN THE RINGS OF THE INNER DETECTOR
            IRN(1) = 20
            IRN(2) = 20
            IRN(3) = 20
C
      IF( NFLAGS(2) .EQ. 4 ) GO TO 8000
         NFLAGS(2) = 4
         GO TO 7900
C
C
C                                           ****************************
C                                           *  1982 TRIGGER CONDITIONS *
C                                           ****************************
 1400 IF( IYEAR .GT. 82 ) GO TO 1500
      IF( NFLAGS(2) .EQ. 5 ) GO TO 8000
         NFLAGS(2) = 5
         GO TO 7900
C
C                                      *********************************
C                                      *  1983-84 TRIGGER CONDITIONS   *
C                                      *********************************
 1500 IF( IYEAR .GT. 84 ) GO TO 1600
C                                   CHANGES
C  UPPER LIMIT ON NR OF TOF IN TBG NS TRIGGER
            NTOFC2 = 6
C  UPPER LIMIT ON NR OF TOF IN NARROW COPLANAR TOF TRIGGER
            NTFCLN = 5
C  UPPER LIMIT ON NR OF TOF IN WIDE COPLANAR TOF TRIGGER
            NTFCOL = 4
C  TAGG THRESHOLDS AND ADC CONVERSION CONSTANT,INNER RING FLAG
            HLGTL = 6500
            HLGTH = 9000
            TAGADC = 6.666
            HTGTRG = 1
C  NR OF RANDOM HITS IN THE RINGS OF THE INNER DETECTOR
            IRN(1) = 80
            IRN(2) = 80
            IRN(3) = 80
C
      IF( NFLAGS(2) .EQ. 6 ) GO TO 8000
         NFLAGS(2) = 6
         GO TO 7900
C
C                                      *********************************
C                                      *  1985    TRIGGER CONDITIONS   *
C                                      *********************************
 1600 IF( IYEAR .GT. 85) GO TO 1700
C                                   CHANGES
C  T1 ACCEPT AND POSTPONE ENABLE BITS IN HEAD BANK
            HT1ACC = HT1A85
            HT1PSP = HT1P85
            IF(IYEAR.EQ.86) HT1ACC = HT1A86
            IF(IYEAR.EQ.86) HT1PSP = HT1P86
C  UPPER LIMIT OF SEPTANTS IN COPLANAR SEPTANT TRIGGERS
            NRNBSH = 8
C  TAGG THRESHOLDS AND ADC CONVERSION CONSTANT, INNER RING FLAG
            HLGTL = 6500
            HLGTH = 9000
            TAGADC = 6.666
            HTGTRG = 1
C  NR OF RANDOM HITS IN THE RINGS OF THE INNER DETECTOR
            IRN(1) = 40
            IRN(2) = 40
            IRN(3) = 40
      IF( NFLAGS(2) .EQ. 7 ) GO TO 8000
         NFLAGS(2) = 7
         GO TO 7900
C
C
C                                      *********************************
C                                      *  1986    TRIGGER CONDITIONS   *
C                                      *         MONTHS 1-6            *
C                                      *********************************
 1700 IF( IYEAR .EQ. 86.AND.IMONTH.GT.6) GO TO 1800
C                                   CHANGES
C  T1 ACCEPT AND POSTPONE ENABLE BITS IN HEAD BANK
            HT1ACC = HT1A86
            HT1PSP = HT1P86
C  UPPER LIMIT OF SEPTANTS IN COPLANAR SEPTANT TRIGGERS
            NRNBSH = 8
C  ECAP QUADRANTS THRESHOLD   (FOR KREHBIEL TRIGGER)
            HLGQT = 140
C  TAGG THRESHOLDS AND ADC CONVERSION CONSTANT, INNER RING FLAG
            HLGTL = 6500
            HLGTH = 9000
            TAGADC = 6.666
            HTGTRG = 1
C  NR OF RANDOM HITS IN THE RINGS OF THE INNER DETECTOR
            IRN(1) = 8
            IRN(2) = 6
            IRN(3) = 6
C  DOUBLE HIT RESOLUTION IN THE INNER DETECTOR, FADC IN 1986
            DOUB(1) = 2.0
            DOUB(2) = 2.0
            DOUB(3) = 2.0
C  Z-RESOLUTION WORSE FOR 1986
            ZRS = 32.0
       DRCH(28) = 32.0
            DLZSLN(1) = .70
            DLZSLN(2) = .0
            DLZSLN(3) = 4.
C  R-PHI RESOLUTION BETTER FOR 1986
            RJITT = .112
            DLRSLN(1) = .9514
            DLRSLN(2) = .0
            DLRSLN(3) = 14.087
C  NR OF RANDOM HITS IN THE VERTEX CHAMBER
            IRNHVV = 9
      IF( NFLAGS(2) .EQ. 8 ) GO TO 8000
         NFLAGS(2) = 8
         GO TO 7900
C                                           ****************************
C                                           *  1986 TRIGGER CONDITIONS *
C                                           *    MONTHS 7-12           *
C                                           ****************************
 1800 CONTINUE
C                                   CHANGES
C  T1 ACCEPT AND POSTPONE ENABLE BITS IN HEAD BANK
            HT1ACC = HT1A86
            HT1PSP = HT1P86
C  UPPER LIMIT OF SEPTANTS IN COPLANAR SEPTANT TRIGGERS
            NRNBSH = 8
C  ECAP QUADRANTS THRESHOLD   (FOR KREHBIEL TRIGGER)
            HLGQT = 140
C  TAGG THRESHOLDS AND ADC CONVERSION CONSTANT
            HLGTL = 4500
            HLGTH = 8750
            TAGADC = 6.666
C  NR OF RANDOM HITS IN THE RINGS OF THE INNER DETECTOR
            IRN(1) = 8
            IRN(2) = 6
            IRN(3) = 6
C  DOUBLE HIT RESOLUTION IN THE INNER DETECTOR, FADC IN 1986
            DOUB(1) = 2.0
            DOUB(2) = 2.0
            DOUB(3) = 2.0
C  Z-RESOLUTION WORSE FOR 1986
C  NOTE THAT DRCH(28) IS USED IN SMEARING!
            ZRS = 32.0
       DRCH(28) = 32.0
            DLZSLN(1) = .70
            DLZSLN(2) = .0
            DLZSLN(3) = 4.
C  R-PHI RESOLUTION BETTER FOR 1986
            RJITT = .112
            DLRSLN(1) = .9514
            DLRSLN(2) = .0
            DLRSLN(3) = 14.087
C
      IF( NFLAGS(2) .EQ. 9 ) GO TO 8000
         NFLAGS(2) = 9
         GO TO 7900
C
C                                           PRINTOUT OF CONSTANTS
7900  IF(NFLAGS(4).NE.1) GO TO 8000
      WRITE(6,9105)
 9105 FORMAT(/' +++ RDTRIG: VERSION OF MAR 88, COMPILED 28/03/1988'/)
C     CALL PRSTAT
 8000 CONTINUE
       write(*,*) 'return from rdtrig'
      RETURN
      END
C-----------------------------------------------------------------------
      BLOCK DATA  BL5           ! PMF 01/07/99 add name
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON /CTRIGG/ IHIST(3),NBPTFW,TAGADC,IDUM1(4),HDUM1,
     *  HLGBRT,HLGBST,HLGQT,HLGTOT(4,2),HECAPT(4),HLGTL,HLGTH,HTGTRG,
     *                HDUM2(9),IWIDBS,NRNBSL,NRNBSH,NTOFBS,IDUM2(10),
     *                NTOFC,NTOFC1,NTOFC2,NTBGC,NTBGC1,NTBGC2,
     *                IWCOLL,IWCOLN,IWMPRG,HFMPRL,HFMPRH,HWCOLN,HMPRON,
     *                IWCTBG,NTFCOL,NTFCLN,NTFBS2,IDUM3(9),
     *  HITCLL(3),HITWLL(3),HITSUM(3),HCHAMB(3),HMASK(16,3),HDEADC(10),
     *  HACC1,HACC2,HACC3,HACC4,HACCM,HDUM6,IWIDT2,HACCB1,HACCB2,
     *  HT1ACC,HT1PSP,IDUM4(8)
C
C-----------------------------------------------------------------------
C
C  T1 ACC  1979-1981                      PARAMETERS USED IN SIMULATION
C  *****************                      *****************************
C
C  BIT  1:   LUMI                              HLGTH
C  BIT  2:   TOTAL LG ENERGY                   HLGTOT(1,1)
C  BIT  3:   TAGG * TOTAL LG ENERGY            HLGTL,HLGTOT(2,1)
C
C  T1 POSTPONE 1979-1981                  PARAMETERS USED IN SIMULATION
C  *****************                      *****************************
C
C  BIT  1:   TOF AND TOTAL LG ENERGY           NTOFC,HLGTOT(3,1),HACC1
C  BIT  2:   TAGG                              HLGTL,HACC3
C  BIT 3-4:  NOT USED
C  BIT  5:   COPLANAR TOF, WIDE                NTFCOL,IWCOLL,HACC2
C  BIT  6:   COPLANAR TOF, NARROW       HWCOLN,NTFCLN,IWCOLN,HACC4
C  BIT  7:   COPLANAR TOF, MULTIPRONG   HMPRON,IWMPRG,HFMPRL,HFMPRH
C                                                            HACCM
C
C  T1 ACC  1982-1986                      PARAMETERS USED IN SIMULATION
C  *****************                      *****************************
C
C  BIT  1:   LUMI                              HLGTH
C  BIT  2:   TOTAL LG ENERGY                   HLGTOT(1,1)
C  BIT  3:   TAGG * TOTAL LG ENERGY            HLGTL,HLGTOT(3,1),HTGTRG
C  BIT  4:   NOT SIMULATED (FWMU AND ECAPQ)
C  BIT  5:   SEPTANT AND ECAPQ, NO TOF         HLGBST,HLGQT,NTOFBS
C  BIT 6-8:  NOT USED
C  BIT  9:   ECAP1 * ECAP2                     HECAPT(2)
C  BIT 10:   ECAP1 + ECAP2, LG BARREL ENERGY   HECAPT(3),HLGTOT(4,2)
C  BIT 11:   TAGG ,LG BARREL ENERGY            HLGTL,HLGTOT(4,2),HTGTRG
C  BIT 12:   LG BARREL ENERGY                  HLGTOT(2,2)
C  BIT 13:   ECAP1 * ECAP2 * TOTAL LG ENERGY   HLGTOT(2,1),HECAPT(4)
C  BIT 14:   COLL. SEPTANT, NO TOF    HLGBST,NRNBSL,NRNBSH,NTOFBS,IWIDBS
C  BIT 15:   TAGG, SEPTANT, NO TOF            NTOFBS,HLGBST,HLGTL,HTGTRG
C  BIT 16:   NOT SIMULATED (RANDOM)
C
C
C  T1 POSTPONE 1982-1986                  PARAMETERS USED IN SIMULATION
C  *****************                      *****************************
C
C  BIT  1:   TOF AND TOTAL LG ENERGY           NTOFC,HLGTOT(3,1),HACC1
C  BIT  2:   NOT USED
C  BIT  3:   TOF, ECAP1 + ECAP2                NTOFC1,HECAPT(2),HACC1
C  BIT  4:   TBG NS                           HLGBRT,NTBGC2,NTOFC2,HACC200046800
C  BIT 5-8:  NOT USED   (BIT8 FOR T3 TRIGGER)
C  BIT  9:   TBG, LG BARREL ENERGY             HLGBRT,NTBGC1,HACC3
C  BIT 10:   TBG, TAGG                  HLGBRT,NTBGC1,HACC4,HLGTL,HTGTRG
C  BIT 11:   SEPTANT COLL.,TOF                 HLGBST,HACCB1,NTFBS2
C  BIT 12:   SEPTANT, TAGG, TOF                HLGBST,HACCB2,HTGTRG
C  BIT 13:   COPLANAR TOF, WIDE                IWCOLL,NTFCOL
C  BIT 14:   COPLANAR TOF, NARROW              IWCOLN,NTFCLN,HWCOLN
C  BIT 15:   COPLANAR TBG                      IWCTBG,HLGBRT,
C  BIT 16:   NOT USED
C
C-----------------------------------------------------------------------
C
C   T1ACCEPT AND T1POSTPONE ENABLE   (USED AFTER 1984 ONLY)
C
      DATA HT1ACC/0/, HT1PSP/0/
C
C DATE OF VERSION
C
      DATA IHIST /1,7,1982/
C
C NBPTFW = WIDTH OF BP-TOF MATRIX; OBSOLETE, NEVER USED IN TRIGGER
C
      DATA NBPTFW / 6 /
C
C DUMMY SPARE VARIABLES
C
      DATA IDUM1 /4*-1/, HDUM1/-1/, HDUM6/-1/
      DATA IDUM2 /10*-1/, HDUM2/9*-1/
      DATA IDUM3 /9*-1/, IDUM4/8*-1/
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     ****   THRESHOLDS FOR LATCHES ****
C
C     BARREL GROUPS,       BARREL SEPTANCES, ENDCAP QUADRANTS
      DATA HLGBRT/ 80/,       HLGBST/180/,       HLGQT/50/
C                   TOTAL ENERGY                    BARREL ENERGY
      DATA HLGTOT/ 6000,2500,2000,1500,       6000,2500,2000,1500/
C                   ENDCAP ENERGY
      DATA HECAPT/ 4000,1000, 500, 300/
C                   TAGGING ENERGIES
      DATA HLGTL/ 4000/,    HLGTH /8000/
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C                 ADC-MEV CONVERSION CONSTANT FOR TAGGING
      DATA TAGADC /10.00/
C                 FLAG FOR THE USE OF INNER RING OF TAGGING IN TRIGGER
      DATA HTGTRG /0/
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     ****   DATA FOR T1 ACCEPT  ****
C
C           NEUTRAL COPLANAR SEPTANT TRIGGER  (OLSSON 1)
C        NRNBSL, NRNBSH   IS LOWER AND UPPER LIMIT ON NR OF SEPTANTS
C        IWIDBS           WIDTH OF COPLANARITY IN SEPTANTS
C        NTOFBS           IS LIMIT ON NR OF TOF COUNTER VETO
      DATA NRNBSL/2/,  NRNBSH/3/, IWIDBS/3/, NTOFBS/1/
C--
C           NEUTRAL SEPTANT + ECAP QUADRANT (KREHBIEL)
C
C
C

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     ****   DATA FOR T1 POSTPONE  ****
C
C                                WIDE COPLANAR TOF TRIGGER
C   IWCOLL      WIDTH OF TOF COPLANARITY
C   NTFCOL      NR OF TOF COUNTER LIMIT
      DATA IWCOLL/3/, NTFCOL/5/
C                                NARROW COPLANAR TOF TRIGGER
C   HWCOLN=1    TRIGGER IS ACTIVE
C   IWCOLN      WIDTH OF TOF COPLANARITY
C   NTFCLN      NR OF TOF COUNTER LIMIT
      DATA IWCOLN/1/, NTFCLN/7/, HWCOLN/1/
C           MULTIPRONG TRIGGER              HMPRON=1 --> TRIGGER ACTIV
      DATA IWMPRG/6/, HFMPRL/3/, HFMPRH/6/, HMPRON/0/
C           COPLANAR TBG TRIGGER
      DATA IWCTBG/2/
C           LIMITS FOR NR OF TOFS IN POSTPONE TRIGGERS
C           NTOFC  =  LIMIT FOR NTOF IN TOTAL ENERGY TRIGGER
C           NTOFC1 =  LIMIT FOR NTOF IN ENDCAP TOTAL ENERGY TRIGGER
C           NTOFC2 =  LIMIT FOR NTOF IN TBG-NS TRIGGER
C           NTFBS2 =  LIMIT FOR NTOF IN COLL.SEPTANT + TOF
      DATA NTOFC/2/,NTOFC1/2/, NTOFC2/7/, NTFBS2/6/
C           LIMITS FOR NR OF TBGS IN POSTPONE TRIGGERS
C           NTBGC  =  LIMIT FOR NTBG IN TAG TRIGGERS
C           NTBGC1 =  LIMIT FOR NTBG IN BARREL ENERGY TRIGGER
C           NTBGC2 =  LIMIT FOR NTBG IN NRTOFBG-NS TRIGGER
      DATA NTBGC/0/,NTBGC1/0/, NTBGC2/2/
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     ****    T2 DATA    ****
C
      DATA HITCLL/8,7,8/,  HITWLL/2,2,2/  ,HITSUM/10,9,10/
      DATA HCHAMB/24,24,48/,  HMASK/16*1,16*1,16*1/,HDEADC/10*0/
C
C  HACC1: TRACK NR LIMIT FOR TOF+TOTAL LG ENERGY
C         TRACK NR LIMIT FOR TOF+SUM ECAP
C  HACC2: TRACK NR LIMIT FOR WIDE COPLANAR TOF, 1979-1981
C  HACC2: TRACK NR LIMIT FOR TBGNS, 1982-...
C  HACC3: TRACK NR LIMIT FOR TAGG, 1979-1981
C  HACC3: TRACK NR LIMIT FOR TBG + BARREL ENERGY, 1982-...
C  HACC4: TRACK NR LIMIT FOR NARROW COPLANAR TOF, 1981..
C  HACC4: TRACK NR LIMIT FOR TAGG + TBG, 1982-...
C  HACCM: TRACK NR LIMIT FOR COPLANAR TOF MULTIPRONG
C  HACCB1:  TRACK NR LIMIT FOR COLL.SEPTANT + TOF
C  HACCB2:  TRACK NR LIMIT FOR TAGG+SEPTANT+TOF
      DATA HACC1/2/,  HACC2/2/,  HACC3/1/, HACC4/1/, HACCM/3/
      DATA HACCB1/1/,  HACCB2/1/
C   IWIDT2 GIVES WIDTH OF T2 COPLANARITY
      DATA IWIDT2 /4/
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      COMMON / CRDSTA / NDEAD, NCDEAD, HITD(10), HCELLD(10), IPHALG
C
C     WIRE DISABLE SECTION FOR JETC BANK
C     ==================================
C                                           DEAD WIRES
      DATA NDEAD / 0 /
      DATA HITD / 10*0 /
C                                           DEAD CELLS
      DATA NCDEAD / 0 /
      DATA HCELLD / 10*0 /
C
C                   LEAD GLASS BLOCK READOUT FLAG
      DATA IPHALG / 1 /
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      COMMON /CFPLA/ HFT(12,4),HR1(12,4),HR2(12,4)
C
C  FPLA LOGIC FOR ONE STREET
C
      DATA HR2/3*5,4*6,5*7, 2*5,6,7*7,8,9, 3*9,4*8,5*7, 5*7,2*8,4*9,1/
      DATA HR1/10,11,12,10,11,12,13,11,12,13,14,15,
     *         11,12,11,10,11,12,13,14,15,16,15,15,
     *         16,15,14,16,15,14,13,15,14,13,12,11,
     *         11,12,13,14,15,13,15,13,14,15,16, 1/
      DATA HFT/0,1,1,0,7*1,0, 5*0,3*1,4*0, 0,1,1,0,7*1,0, 2*0,8*1,2*0/
C
      COMMON /CMASK/ MASK(16),MASK4,MASK3,MASK2,MASK1,HKRE(3,8)
      DATA MASK/Z'1',Z'2',Z'4',Z'8',Z'10',Z'20',Z'40',Z'80',Z'100',Z'200',Z'400',Z'800',
     *                 Z'1000',Z'2000',Z'4000',Z'8000'/
      DATA MASK2/Z'20000'/, MASK1/Z'10000'/
      DATA MASK4/Z'40000'/, MASK3/Z'30000'/
C  CORRELATIONS FOR ECAP QUADRANT - SEPTANT  COLLINEARITY
      DATA HKRE /5,6,0,   1,6,7,   1,2,3,   3,4,5,
     $           5,6,0,   1,6,7,   1,2,3,   3,4,5/
C
C------ THIS PART WAS PREVIOUSLY CONTAINED IN RDMTCO -------------------
C
      COMMON / CBIN / TIME(6),ZOF,ZRS,ZL,ZSC,EPSI(3),DOUB(3),IRN(3),
     +                BINDL8(6),RJITT, DLRSLN(3), DLZSLN(3)
C
C      TIME     BIN WIDTH FOR DRIFT TIME IN MM
C               WIRES ARE PUT TOGETHER IN GROUPS OF 8
C      ZOF      OFFSET IN Z  DIRECTION IN MM FOR AMPLITUDE CALCULATION
C      ZRS      STANDARD DEV FOR Z RESOLUTION IN MM
C      ZL       EFFECTIVE WIRE LENGTH IN MM
C      ZSC      SCALING FACTOR FOR Z AMPLITUDES
C      EPSI     EFFICIENCY IN EACH RING
C      DOUB     DOUBLE TRACK RESOLUTION IN MM IN EACH RING
C      IRN      NO OF RANDOM HITS INSERTED IN EACH RING
C---------- BINDL8 AND RJITT ARE USED IN NEW RDRESO FOR SMEARING
C      BINDL8   BIN WIDTH FOR DRIFT TIME IN MM
C      RJITT +  NEW STANDARD SMEARING OF DRIFT COORDINATE
C      DLRSLN     (J S VALUES )
C      DLZSLN   RESOLUTIONS FOR DOUBLE GAUSSIAN IN Z
C
      DATA TIME / 6*0.380/
      DATA RJITT / .170 /
      DATA DLRSLN / .95683, 5.9568, 10.293 /
      DATA DLZSLN / .89   , 0.0   ,  5.000 /
      DATA ZOF, ZRS, ZL, ZSC / -10., 20., 2687., 1. /
      DATA EPSI / 3*0.98/
      DATA DOUB / 3*7.5 /
C   RANDOM HITS ACCORDING TO GOOD BEAM CONDITIONS OF 1982
C   THIS IS UPDATED IN RDTRIG FOR OTHER PERIODS
      DATA IRN  / 15, 15, 15 /
C
      DATA BINDL8 / 6*0.380/
C
C     CBINV HAS TO BE USED TO CHANGE THE ACTUAL
C     CONSTANTS OF THE VERTEX CHAMBER
C
      COMMON / CBINV /  DTRSVV, ZRSVV, EFFVV(2), DOUBVV(3), IRNHVV,
     +                  SMPRMV(3)
C
C      DTRSVV      DRIFT TIME RESOLUTION OF VERTEX CHAMBER (MM)
C      ZRSVV       STANDARD DEVIATION FOR Z RESOLUTION (MM)
C      EFFVV(1)    EFFICIENCY PARAMETERS
C      EFFVV(2)       (SEE MEMBER RDIEFV)
C      DOUBVV(1)   SLOPE OF DOUBLE HIT RESOLUTION DISTRIBUTION
C      DOUBVV(2)   LOW CUT    (IN MICRONS)  (SEE MEMBER RDDOUV)
C      DOUBVV(3)   HIGH CUT   (IN MICRONS)
C      IRNHVV      NO OF RANDOM HITS INSERTED
C      SMPRMV(1)   FRACTION OF SMALLER NORMAL DISTRIBUTION
C      SMPRMV(2)   SHIFT OF WIDER NORMAL DISTRIBUTION
C      SMPRMV(3)   WIDTH OF WIDER NORMAL DISTRIBUTION
C                  DTRSVV CHANGED TO .130  18.12.1987   J.O.
C     DATA DTRSVV   / 0.150 /
      DATA DTRSVV   / 0.130 /
      DATA ZRSVV    / 20.0 /
      DATA EFFVV    / .871, .091 /
      DATA DOUBVV   / 0.583, 1.484, 4.2 /
C          RANDOM HITS ACCORDING TO BEAM CONDITIONS OF 1985 (44.2 GEV)
      DATA IRNHVV   / 33 /
      DATA SMPRMV   / 0.400, 0.0, 2.0 /
C
      END
