C   20/10/81 404162044  MEMBER NAME  MUREG    (JADEMUS)     FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE MUREG(IPRINT)
C-----------------------------------------------------------------------
C
C LAST CHANGE 20.00 16/04/84 CHRIS BOWDERY-BETTER TEST  NOW MADE
C      CHANGE 13.00  9/03/84 CHRIS BOWDERY-TEST TO ENSURE CAL. READ
C      CHANGE 13.00 23/11/83 HUGH MCCANN - CORRECT YOKE PARAMETERS.
C                                          ...FURTHER ITERATION !!
C      CHANGE 22.00 22/10/83 HUGH MCCANN - CORRECT YOKE PARAMETERS.
C                                          AND INDENT ENTIRE CODE.
C      CHANGE 13.30 22/02/82 HUGH MCCANN - FOR IMPROVED ERROR OUTPUT.
C                                     MUMESS O/P FOR IPRINT >= 6 * < 10.
C      CHANGE 16.05 17/06/81 JOHN ALLISON - TO EXTEND REGIONS.
C
C ABSTRACTS FROM THE MU-FILTER CONSTANTS THE REGIONS FOR /CMUREG/.
C PRINTING SUPPRESSED IF IPRINT.LT.10.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL ERROR
C
C                            COMMONS.
C
#include "cmucalib.for"
#include "cmureg.for"
#include "cmubcs.for"
#include "cadmin.for"
C
      COMMON /CMUPRN/ MUPRIN,MUHPR,IANAC,ICOOR,IANAF,LANAC,LCOOR,LANAF,
     +                ITOTAL,LTOTAL,ICUTS,LCUTS
      COMMON /CWORK/  CNLO(500),CNHI(500),CLLO(500),CLHI(500),
     +                CTLO(500),CTHI(500),IUNIT(500)
C
C                            TEMPORARY STORE FOR REGION LIMITS.
C
C                            DATA INITIALISATIION STATEMENTS.
C
      DATA NPLMAX/48/
C
      DATA ERROR/.FALSE./
C
      DATA EXTRA/500./
C
C------------------  C O D E  ------------------------------------------
C
      IF( IPRINT .GE.6  .AND  .IPRINT .LT. 10 )
     +      CALL MUMESS('MUREG ',0,'CALLED TO COMPUTE MUON REGIONS.^')
C
C                            CHANGE HSECT FOR HANGING ROOF LAYER.
C
      HSECT(39)=9
      HSECT(40)=9
      HSECT(41)=10
      HSECT(42)=10
C
C                            CLEAR CMUREG - CURRENTLY 901 WORDS.
C
      CALL VZERO(NREGS,901)
C
C                            HAS THE CALIBRATION BEEN READ PROPERLY?
C                            CHECK SOME OF THE KNOWN NUMBERS
C
      IF( HFR(1) .EQ. 0  .AND.  HFR(200) .EQ. 0 ) GO TO 95
C
C                            DO CHAMBER PLANES.
C
      HRMASK(1)=256
      HRFACE(1)=1
      HRTYPE(1)=1
      HRORI(1)=HNORM(1)
      IUNIT(1)=3
      HRFIRS(1)=HAC(1)
      C1=HDIST(1)+HD1(HAC(1))
      C2=HDIST(1)+HD1(HAC(1)+1)
      IF(C1.GT.C2)GO TO 1
         CNLO(1)=C1
         CNHI(1)=C2
         GO TO 2
 1    CONTINUE
         CNLO(1)=C2
         CNHI(1)=C1
 2    CONTINUE
      CLLO(1)=HCLLO(1)
      CLHI(1)=HCLHI(1)
      CTLO(1)=HCTLO(1)
C
      J=1
C
      DO 3 I=2,NFRAMS
          K=HCTLO(I)-HCTHI(I-1)
          IF ( HDIST(I).EQ.HDIST(I-1) .AND.
     *         IABS(K).LT.50 ) GO TO 3
C
              J=J+1
              IF(J.GT.NPLMAX)GO TO 98
C
                  IFACE=HFACE(I)
                  HRMASK(J)=2**(IFACE-1)
                  IF(HLAYER(I).EQ.1)HRMASK(J)=256*HRMASK(J)
                  HRFACE(J)=IFACE
                  HRTYPE(J)=1
                  HRORI(J)=HNORM(I)
                  IUNIT(J)=HUNIT(I)
                  HRFIRS(J)=HAC(I)
                  C1=HDIST(I)+HD1(HAC(I))
                  C2=HDIST(I)+HD1(HAC(I)+1)
                  IF(C1.GT.C2)GO TO 4
                      CNLO(J)=C1
                      CNHI(J)=C2
                      GO TO 5
 4                CONTINUE
                      CNLO(J)=C2
                      CNHI(J)=C1
 5                CONTINUE
                  CLLO(J)=HCLLO(I)
                  CLHI(J)=HCLHI(I)
                  CTLO(J)=HCTLO(I)
C
                  HRLAST(J-1)=HAL(I-1)
                  CTHI(J-1)=HCTHI(I-1)
C
 3    CONTINUE
C
      I=NFRAMS
      HRLAST(J)=HAL(I)
      CTHI(J)=HCTHI(I)
      NPLANS=J
C
C-----------------------------------------------------------------------
C
C                            YOKE.
      DO 20 I=1,4
          J=J+1
          HRORI(J)=1
          IF(I.GE.3)HRORI(J)=2
          HRMASK(J)=2**(I+7)
          HRTYPE(J)=3
          HRFACE(J)=I
          IUNIT(J)=3
          C1=HYKNMI(I)
          C2=HYKNMO(I)
          IF(C1.GT.C2)GO TO 21
              CNLO(J)=C1
              CNHI(J)=C2
              GO TO 22
 21       CONTINUE
              CNLO(J)=C2
              CNHI(J)=C1
 22       CONTINUE
          CTLO(J)=-HYKTDM(I)
          CTHI(J)=HYKTDM(I)
          CLLO(J)=-HYKLDM(I)
          CLHI(J)=HYKLDM(I)
          IF(I.EQ.3)GO TO 20
              CLLO(J)=CLLO(J)+10
              CLHI(J)=CLHI(J)-10
 20   CONTINUE
C
C-----------------------------------------------------------------------
C
C                            END PLUG.
      DO 30 I=1,2
          J=J+1
          HRORI(J)=3
          HRMASK(J)=2**(I+11)
          HRTYPE(J)=3
          HRFACE(J)=I+4
          IUNIT(J)=3
C***       CORRECTION IMPLEMENTED ON 22/10/83 : YOKE END PLATE THICKNESS
C***       SET TO 340 MM .  SEE MUFFLY.
          CNLO(J)=-(IZEIO + 340 )
          CNHI(J)=-IZEIO
C***      CNLO(J)=-IZOEP5
C***      CNHI(J)=-IZOEP1
C
          IF(I.EQ.1)GO TO 31
C
              CNLO(J)=IZEIO
              CNHI(J)=IZEIO + 340
C***          CNLO(J)=IZOEP1
C***          CNHI(J)=IZOEP5
 31       CONTINUE
C
C              CORRECTION 23/11/83 , HMCC :
C              THE YOKE END-PLATES ARE COMPLETELY CONTAINED WITHIN
C              THE SIDE- , FLOOR- AND ROOF-PLATES IN X AND Y .
C              ( I DOUBT IF ANYONE WILL EVER NOTICE ANY EFFECT OF THIS!)
C              TO BE EVEN MORE PEDANTIC , THE Y.E.P.S ACTUALLY HAVE A
C              FLANGE AT +Y WHICH GOES A COUPLE OF CMS HIGHER THAN 1759
C              MM , BUT IGNORE THAT.
          CTLO(J)=-1759
          CTHI(J)= 1759
          CLLO(J)=-1759
          CLHI(J)= 1759
C***      CTLO(J)=HYKNMO(1)
C***      CTHI(J)=HYKNMO(2)
C***      CLLO(J)=HYKNMI(3)
C***      CLHI(J)=HYKNMO(4)
 30   CONTINUE
C
C-----------------------------------------------------------------------
C
C                            ABSORBER - FACES 1-3.
      DO 40 IFACE=1,3
          DO 40 IBLK=1,3
              J=J+1
              HRMASK(J)=2**(IFACE-1)
              HRFACE(J)=IFACE
              HRTYPE(J)=2
              IF(IFACE.EQ.3)HRTYPE(J)=3
              HRORI(J)=(IFACE+1)/2
              IUNIT(J)=IFACE
              K=6*(IFACE-1)+2*(IBLK-1)+1
              C1=HBNLIM(K)
              C2=HBNLIM(K+1)
              IF(C1.GT.C2)GO TO 41
                  CNLO(J)=C1
                  CNHI(J)=C2
                  GO TO 42
 41           CONTINUE
                  CNLO(J)=C2
                  CNHI(J)=C1
 42           CONTINUE
              CTLO(J)=HBTLO(IFACE)
              CTHI(J)=HBTHI(IFACE)
              CLLO(J)=HBLLO(IFACE)
              CLHI(J)=HBLHI(IFACE)
 40   CONTINUE
C
C                            ABSORBER - FACES 4-6.
      DO 50 IFACE=4,6
          DO 50 IBLK=1,3
              DO 50 ISIDE=1,2
                  J=J+1
                  HRMASK(J)=2**(IFACE-1)
                  HRFACE(J)=IFACE
                  HRTYPE(J)=2
                  HRORI(J)=(IFACE+1)/2
                  IUNIT(J)=ISIDE+3
                  K=6*(IFACE-1)+2*(IBLK-1)+1
                  C1=HBNLIM(K)
                  C2=HBNLIM(K+1)
                  IF(C1.GT.C2)GO TO 51
                      CNLO(J)=C1
                      CNHI(J)=C2
                      GO TO 52
 51               CONTINUE
                      CNLO(J)=C2
                      CNHI(J)=C1
 52               CONTINUE
                  IGAP=50.
                  IF(IFACE.GE.5)IGAP=350.
                  IF(ISIDE.EQ.2)GO TO 53
                      CTLO(J)=HBTLO(IFACE)
                      CTHI(J)=-IGAP
                      GO TO 54
 53               CONTINUE
                      CTLO(J)=IGAP
                      CTHI(J)=HBTHI(IFACE)
 54               CONTINUE
                  CLLO(J)=HBLLO(IFACE)
                  CLHI(J)=HBLHI(IFACE)
 50   CONTINUE
C
C-----------------------------------------------------------------------
C
C                            REMAINING PIECES - (OVERHANGING ARCH).
      DO 60 IFACE=5,6
          DO 60 IBLK=1,3
              DO 60 ISIDE=1,2
                  J=J+1
                  HRMASK(J)=2**(IFACE-1)
                  HRFACE(J)=IFACE
                  HRTYPE(J)=2
                  HRORI(J)=(IFACE+1)/2
                  IUNIT(J)=ISIDE+3
                  K=6*(IFACE-1)+2*(IBLK-1)+1
                  C1=HBNLIM(K)
                  C2=HBNLIM(K+1)
                  IF(C1.GT.C2)GO TO 61
                      CNLO(J)=C1
                      CNHI(J)=C2
                      GO TO 62
 61               CONTINUE
                      CNLO(J)=C2
                      CNHI(J)=C1
 62               CONTINUE
                  IF(ISIDE.EQ.2)GO TO 63
                      CTLO(J)=-350.
                      CTHI(J)=-50.
                      GO TO 64
 63               CONTINUE
                      CTLO(J)=50.
                      CTHI(J)=350.
 64               CONTINUE
                  CLLO(J)=350.
                  CLHI(J)=HBLHI(IFACE)
 60   CONTINUE
C
C-----------------------------------------------------------------------
C
      NREGS=J
C
C                            CHANGE FROM N,L,T TO X,Y,Z.
C                            (EXTEND CHAMBER REGIONS,I.E. I .LT. NPLANS)
      E=EXTRA
      DO 6 I=1,NREGS
          IF(I.GT.NPLANS)E=0.
          IOR=HRORI(I)
          GO TO (7,8,9),IOR
 7        CONTINUE
              XRLO(I)=CNLO(I)
              XRHI(I)=CNHI(I)
              YRLO(I)=CTLO(I)-E
              YRHI(I)=CTHI(I)+E
              ZRLO(I)=CLLO(I)-E
              ZRHI(I)=CLHI(I)+E
              GO TO 6
 8        CONTINUE
              XRLO(I)=CTLO(I)-E
              XRHI(I)=CTHI(I)+E
              YRLO(I)=CNLO(I)
              YRHI(I)=CNHI(I)
              ZRLO(I)=CLLO(I)-E
              ZRHI(I)=CLHI(I)+E
              GO TO 6
 9        CONTINUE
              XRLO(I)=CTLO(I)-E
              XRHI(I)=CTHI(I)+E
              YRLO(I)=CLLO(I)-E
              YRHI(I)=CLHI(I)+E
              ZRLO(I)=CNLO(I)
              ZRHI(I)=CNHI(I)
 6    CONTINUE
C
C-----------------------------------------------------------------------
C
C                            MAKE OVERALL UNIT TRANSLATIONS.
      DO 70 I=1,NREGS
          XRLO(I)=XRLO(I)+HOVALL(IUNIT(I))
          XRHI(I)=XRHI(I)+HOVALL(IUNIT(I))
 70   CONTINUE
C
C-----------------------------------------------------------------------
C
C                            PRINT OUT CONSTANTS.
 10   CONTINUE
      IF(IPRINT.LT.10)GO TO 99
      WRITE(6,100)NREGS,
     * (I,XRLO(I),XRHI(I),YRLO(I),YRHI(I),ZRLO(I),ZRHI(I),
     * HRMASK(I),HRFACE(I),HRTYPE(I),HRORI(I),HRFIRS(I),HRLAST(I),
     * I=1,NREGS)
 100  FORMAT('1MUREG - ABSTRACTS REGIONS AS FOLLOWS...'/
     *'0',I5,' REGIONS'/
     *'0     REGION  XRLO      XRHI      YRLO      YRHI      ZRLO',
     *'      ZRHI    HRMASK    HRFACE    HRTYPE      HRORI    HRFIRS',
     *'    HRLAST'/
     *(I10,6F10.2,Z10,5I10))
      GO TO 99
C
C-----------------------------------------------------------------------
C
C                            ERROR CONDITIONS.
C
 98   IF( MUPRIN .EQ. 0 ) MUPRIN = 2
      CALL MUERRY('MUREG ',NPLMAX,'TOO MANY MU PLANES. ++ STOP ++^')
      ERROR = .TRUE.
      GO TO 10
C
C                            NO MUON CALIBRATION IS ONLY AN ERROR FOR
C                            REAL DATA. CHECK EVENT TYPE BUT BE SURE
C
 95   IF( IEVTP .GT. 0 ) GO TO 99
      IPHEAD = IDATA(IBLN('HEAD'))
      IF( IPHEAD .LE. 0 ) GO TO 99
      KRUN   = HDATA( 2*IPHEAD + 10 )
      IF( KRUN .LT. 100 ) GO TO 99
      IF( MUPRIN .EQ. 0 ) MUPRIN = 2
      CALL MUERRY('MUREG ',0,' NO MUON CALIBRATION FOUND. ++ STOP ++^')
      ERROR = .TRUE.
C
C-----------------------------------------------------------------------
C
 99   IF( .NOT. ERROR ) RETURN
      CALL BSTA
      STOP 333
      END
