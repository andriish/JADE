     STATUS OF MONTE CARLO DATASETS
  =================================================================
                                           E.ELSEN       07/08/79
                                           LAST MOD      19/05/80
                                                         17:00


GENERAL REMARKS
========================================================================
WHERE NOT OTHERWISE INDICATED DATA REFER TO 30 GEV CM ENERGY.

DATASETS EXIST AT DIFFERENT LEVELS OF ANLAYSIS
A) 4 VECT     :THESE ARE FOUR VECTORS FOR THE GIVEN EVENT CLASS

B) TRACKED    :PARTICLES HAVE BEEN TRACKED THROUGH THE DETECTOR
               BUT THE RESOLUTIONS ARE VERY FINE FOR INNER DETECTOR
               NO TRIGGER BANKS EXIST AT THIS LEVEL
               THE DATA CANNOT BE USED WITHOUT SOPHISTICATED READING
               ROUTINES ( IF YOU WANT TO SMEAR THE DATA YOURSELF
                          ASK E.ELSEN FOR THE NAME OF THE ROUTINES)

C) PATR       :THE DATA HAVE BEEN SMEARED WITH A STANDARD SET OF
               CONSTANTS AND THE PATTERN RECOGNITION HAS BEEN PERFORMED
               A NEW AND MORE REALISTIC PATR BANK IS CREATED
               OTHER BANKS ARE ADDED

D) PATRTP     :THE ABOVE DATA HAVE BEEN RUN THROUGH THE TP STEP

AFTER THE INITIAL EXPERIMENTAL PHASE STEPS 2 AND 3 HAVE BEEN COMBINED

EXISTING DATASETS
========================================================================
DATASETS WITHOUT IDENTIFIER REFER TO F22ELS
IDENTIFIER F22PET IS ABBREVIATED AS PET
IDENTIFIER F22RAE IS ABBREVIATED AS RAE
T = TAPE       F = FAST
EV = NUMBER OF EVENTS ( TWO RECORDS OF CONSTANTS ARE NOT COUNTED )
N = INCLUDING CRUDE NUCLEAR INTERACTION IN LEAD GLASS
R = EVENTS THAT PASSED FIRST DATA REDUCTION STEP
C = INCLUDING RADIATIVE CORRECTIONS
J = LAST MESON IN JET DIRECTION
D DEAD CELLS SIMULATED

NR  4 VECT            TRACKED          PATR                 TP
========================================================================
========================================================================
 1  F11BAR.HLEPA30   .T2HLPA30     .PATR.T2HLPA30      .PATRTP.T2HLPA30
    T,EV             T,EV 3203     T,EV 3203           T,EV 3203
    ANGULAR DISTRIBUTION NOT OKAY

                     .T3HLPA30     .PATR.T3HLPA30      .PATRTP.T3HLPA30
                     T,EV          T,EV 2341           T,EV 1998

------------------------------------------------------------------------
 2  F11BAR.QQGA30    .TQQGA30      .PATR.TQQGA30       .PATRTP.TQQGA30
    T,EV             T,EV          T,EV  560           T,EV 555
    F11BAR.QQGB30    .T1QQGB30     .PATR.T1QQGB30      .PATRTP.T1QQGB30
    T,C,EV           T,C,N,EV 689  T,C,N,EV 689        T,C,N,EV

------------------------------------------------------------------------
 3  F11BAR.JETC30    .TJETC30      .PATR.TJETC30       .PATRTP.TJETC30
    T,EV             T,EV          T,EV     716        T,EV
                     .T2JETC30     .PATR.T2JETC30      .PATRTP.T2JETC30
                     T,N,EV 714    T,N,EV 714          T,N,EV 714
    F11BAR.JETE30    T1JETE30      .PATR.T1JETE30      .PATRTP.T1JETE30
    T,C,EV           T,N,C,EV 752  T,N,C,EV 752        T,N,C,EV 752

------------------------------------------------------------------------
 4  F22PET.JTOP1630  .TTOP1630     .PATR.TTOP1630      .PATRTP.TTOP1630
    T,C,EV           T,C,EV        T,C,EV              T,C,EV
                                                       .PATRTP.DTOP1630
                                                       F,C,EV
                     .T2TP1630     .PATR.T2TP1630      .PATRTP.T2TP1630
                     T,N,C,EV 691  T,N,C,EV 691        T,N,C,EV 691
    PET.JET.TOP1630  .TJOP1630     .PATR.TJOP1630      .PATRTP.TJOP1630
    T,N,C,J,EV       T,N,C,J,EV 673T,N,C,J,EV 673      T,N,C,EV 673

------------------------------------------------------------------------
 5  F22PET.JTOP1430  .TTOP1430     .PATR.TTOP1430      .PATRTP.TTOP1430
    T,C,EV           T,C,EV        T,C,EV              T,C,EV
                                                       .PATRTP.DTOP1430
                                                       F,C,EV
                     .T2TP1430     .PATR.T2TP1430      .PATRTP.T2TP1430
                     T,N,C,EV 546  T,N,C,EV 546        T,N,C,EV 546

------------------------------------------------------------------------
 6  F22PET.GGJETP30  .T1GGJP30     .PATR.T1GGJP30      .PATRTP.T1GGJP30
    T,C,EV           T,C,EV        T,C,EV              T,C,EV 1685
                                                       .RDC01.T1GGJP30
                                                       T,R,C,EV 876
                     .T2GGJP30     .PATR.T2GGJP30      .PATRTP.T2GGJP30
                     T,N,C,EV 1434 T,N,C,EV 1434       T,N,C,EV 1434

------------------------------------------------------------------------
 7  F22PET.GGJETM30  .T1GGJM30     .PATR.T1GGJM30      .PATRTP.T1GGJM30
    T,C,EV           T,C,EV 1884   T,C,EV 1884         T,C,EV  1884
                     .T2GGJM30     .PATR.T2GGJM30      .PATRTP.T2GGJM30
                     T,N,C,EV 1868 T,C,N,EV 1868       T,C,N,EV 1868
------------------------------------------------------------------------
 8 PET.JET.TOP1622   .TJOP1622     .PATR.TJOP1622      .PATRTP.TJOP1622
    T,N,C,J,EV       T,N,C,J,EV 750T,N,C,J,EV 750      T,N,C,J,EV 750

------------------------------------------------------------------------
 9 F11BAR.QQGC30     .T1QQGC30     .PATR.T1QQGC30      .PATRTP.T1QQGC30
    T,N,C,EV         T,N,C,EV 658  T,N,C,EV 658        T,N,C,EV 658
    K0 S DO NOT DECAY

------------------------------------------------------------------------
10 F11BAR.JETF30     .T1JETF30     .PATR.T1JETF30      .PATRTP.T1JETF30
    T,N,C,EV         T,N,C,EV 535  T,N,C,EV 535        T,N,C,EV 535

------------------------------------------------------------------------
11 PET.NUR.TOP1430   .NTOP1430     .PATR.NTOP1430      .PATRTP.NTOP1430
    T,N,C,EV         T,N,C,EV      T,N,C,EV 442        T,N,C,EV 442

                                                     18/03/80
                                                     PATRTP.T2NT1430
                                                     EV 315
                                            NO LIMIT IN MULTIPLICITY

------------------------------------------------------------------------
12 F11BAR.QQGC22     .T1QQGC22     PATR.T1QQGC22       .PATRTP.T1QQGC22
    T,N,C,EV         T,N,C,EV      T,N,C,EV 767        T,N,C,EV 767
    K0 S DO NOT DECAY

------------------------------------------------------------------------
13 F11BAR.QQGD22     .T1QQGD22     PATR.T1QQGD22       .PATRTP.T1QQGD22
    T,N,C,EV         T,N,C,EV      T,N,C,EV 777        T,N,C,EV 777
    K0 S DO NOT DECAY

------------------------------------------------------------------------
14 F11BAR.QQGC28     .T1QQGC28     .PATR.T1QQGC28      .PATRTP.T1QQGC28
    T,N,C,EV         T,N,C,EV 681  T,N,C,EV 681        T,N,C,EV 681
    K0 S DO NOT DECAY

------------------------------------------------------------------------
15  F22PET.TAU30     .T1TAU30      .PATR.T1TAU30       .PATRTP.T1TAU30
    T,EV             T,N,EV 849    T,N,EV 164          T,N,EV 164
                     .T2TAU30      .PATR.T2TAU30       .PATRTP.T2TAU30
                     T,N,EV 842    T,N,EV 842          T,N,EV 842

------------------------------------------------------------------------
16  F22PET.SHL1330   .T1SL1330     .PATR.T1SL1330      .PATRTP.T1SL1330
    T,N,EV           T,N,EV        T,N,EV 875          T,N,EV 875
                     .T2SL1330     .PATR.T2SL1330      .PATRTP.T2SL1330
                     T,N,EV 227    T,N,EV 227          T,N,EV 227

------------------------------------------------------------------------
17  F11BAR.QQG32     .T1QQG32      .PATR.T1QQG32       .PATRTP.T1QQG32
    T,N,C,EV         T,N,C,EV 652  T,N,C,EV 612        T,N,C,EV
                                                       .PATRTP.D1QQG32
                                                       F,N,C,EV 344

------------------------------------------------------------------------
18  F11BAR.QQGD30    .T1QQGD30     .PATR.T1QQGD30      .PATRTP.T1QQGD30
    T,C,EV           T,C,N,EV 647  T,C,N,EV 355        T,C,N,EV 355
                                   PATR.T2QQGD30       PATRTP.T2QQGD30
                                   T,C,N,EV 647        T,C,N,EV 647

------------------------------------------------------------------------
19  F11BAR.QQGE22    .T1QQGE22     .PATR.T1QQGE22      PATRTP.T1QQGE22
    T,N,C,EV         T,N,C,EV 750  T,N,C,EV 671        T,N,C,EV 671

------------------------------------------------------------------------
20  F11BAR.QQGE27    .T1QQGE27     .T1QQGE27           .PATRTP.T1QQGE27
    T,N,C,EV         T,N,C,EV 592  T,N,C,EV 498   T,N,C,EV 498

------------------------------------------------------------------------
21  F11BAR.QQGF30    .T1QQGF30     .PATR.T1QQGF30      PATRTP.T1QQGF30
    T,N,C,EV         T,N,C,EV 577  T,N,C,EV 577        T,N,C,EV 577

------------------------------------------------------------------------
22  F11BAR.JETG30    RAE.JETG30    RAE.PATR.JETG30     TPJETG30
    T,N,C,EV

------------------------------------------------------------------------
23  PET.JT50.TOP1630 RAE.JT50.TOP1630 RAE.PATR.JT50TOP1630 RAE.TPJT50.
    T,N,C,EV                                                   TOP1630

------------------------------------------------------------------------
24  PET.JET.BOT0512  T1JB0512                          PATRTP.T1JB0512
    T,N,C,EV         T,N,C,EV 878                      T,N,C,EV 878

------------------------------------------------------------------------
25  PET.JET.BOT1612  T1JB1612                        PATRTP.T1JB1612
    T,N,C,EV         T,N,C,EV 1065                     T,N,C,EV 1065

------------------------------------------------------------------------
26  .QQGG30          T1QQGG30                         PATRTP.T1QQG30
    T,N,C,EV         T,N,C,EV 571                     T,N,C,EV 220
                                                      PATRTP.T2QQG30
                                                      T,N,C,EV 298
                                                      PATRTP.T3QQG30
                                                      T,N,C,EV 571

                                                  THE ABOVE DATASETS
                                                  CONTAIN ALL THE SAME
                                                  EVENTS
    .QQGH30          .T1QQGH30                    .PATRTP.T2QQGH30
    T,N,C,EV         T,N,C,EV 1167                T,N,C,EV 1167

    01/02/80                                      PATRTP.T3QQGH30
                                                  T,N,C,EV 1167

    25/04/80           25/04/80                26/04/80
    .QQGL30          .T1QQGL30                    .PATRTP.T1QQGL30
    T,N,C,EV         T,N,C,EV 627                 T,N,C,EV 627
    TRKGAM, RADIATIVE CORRECTIONS, ORDLG NEW

------------------------------------------------------------------------
27  PET.JET.QQG1630  SUB AS F22ELSM5
    T,N,C,EV

------------------------------------------------------------------------
28  PET.GGJETP12     SUB AS F22ELSM5
    T,N,C,EV

------------------------------------------------------------------------
29  PET.TRIJET.GBD1
    T,N,C,EV

------------------------------------------------------------------------
30  PET.TRIJET.GBD2    T1TRIJET.GBD2            PATRTP.T1TRIJET.GBD2
    T,N,C,EV           T,N,C,EV                 T,N,C,EV  596
    OVERWRITTEN        T2TRIJET.GBD2            PATRTP.T2TRIJET.GBD2
    NOW                T,N,C,EV 470             T,N,C,EV 470

------------------------------------------------------------------------
31  PET.QQGJET.A30     T1QQGJET.A30            PATRTP.T1QQGJET.A30
    T,N,C,EV           T,N,C,EV 645            T,N,C,EV 639
    ERROR IN K0 DECAYS

  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    11/02/80           12/02/80                13/02/80
    PET.QQGJET.B30     T1QQGJET.B30            PATRTP.T1QQGJET.B30
    T,N,C              EV 634                  EV 634

  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    19/05/80           19/05/80                19/05/80
    PET.QQGJET.C30     T1QQGJET.B30            PATRTP.T1QQGJET.C30
    T,N,C 1056         EV 1056                 EV 1056

------------------------------------------------------------------------
32  PET.TRIJET.OMD1    T1TRIJET.OMD1           PATRTP.T1TRIJET.OMD1
    T,N,C,EV           T,N,C,EV 400            T,N,C,EV 400
    ERROR IN K0 DECAYS
                        - - - - - - - - - - - - - - - - - - - - - - - -
    01/02/80                                   SUB AS F22ELST2

  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    11/02/80           12/02/80                13/02/80
    PET.TRIJET.OMD2    T1TRIJET.OMD2           PATRTP.T1TRIJET.OMD2
    11/02/80           EV 400                  EV 400

------------------------------------------------------------------------
33  12/02/80           20/02/80               21/02/80
    PET.ALIJET.A30     T1ALIJET.A30           PATRTP.T1ALIJET.A30
                       EV 1273                EV 1200

------------------------------------------------------------------------
34  12/02/80           17/02/80               18/02/80
    PET.ALIJET.A36     T1ALIJET.A36           PATRTP.T1ALIJET.A36
                       EV 634                 EV 634

                                              17/03/80
                                              PATRTP.T2ALIJET.A36
                                              D,EV 311

------------------------------------------------------------------------
35  12/02/80           20/02/80               21/02/80
    PET.ALIJET.B36     T1ALIJET.B36           PATRTP.T1ALIJET.B36
                       EV 615                 EV 540

                                              17/03/80
                                              PATRTP.T2ALIJET.B36
                                              D,EV 485

------------------------------------------------------------------------
36  19/02/80           20/02/80               21/02/80
    PET.ALIJET.C36     T1ALIJET.C36           PATRTP.T1ALIJET.C36
                       EV 278                 EV 278

                                              17/03/80
                                              PATRTP.T2ALIJET.C36
                                              D,EV 278

------------------------------------------------------------------------
37  19/02/80           20/02/80               21/02/80
    PET.GGJETP36       T1GGJP36               PATRTP.T1GGJP36
                       EV 1595                1595

                                              17/03/80
                                              PATRTP.T2GGJP36
                                              D,EV 1399

------------------------------------------------------------------------
38  14/04/80           14/04/80               15/04/80
    BAR.QQGA37         T1QQGA37               PATRTP.T1QQGA37
                       EV 567                 EV 443

------------------------------------------------------------------------


CONTENTS OF THE DATASETS
========================================================================
 1  HEAVY LEPTON DATA
 2  QUARK QUARK GLUON ( T. MEYERS PROGRAM )
 3  STANDARD DESY U,D,S,C,B QUARK MONTE CARLO ( T. MEYER )
 4  LUND MONTE CARLO U,D,S,C,B
 5  LUND MONTE CARLO U,D,S,C,B,T   TOP MASS = 14GEV
 6  GAMMA GAMMA MONTE CARLO   ( BRODSKY, LUND PROGRAM FOR 2 PHOTON MASS,
                                RHO-LIKE GAMGAM CROSS SECTION )
 7  GAMMA GAMMA MONTE CARLO   ( BRODSKY, LUND PROGRAM FOR 2 PHOTON MASS,
                                MU-MU-LIKE GAMGAM CROSS SECTION )
 8  LUND MONTE CARLO U,D,S,C,B    ECM = 22 GEV
 9  U,D,S,C,B + GLUON
10  BOTTOM PURE
11  TOP PURE
12  U,D,S,C,B + GLUON             ECM = 22 GEV
13  U,D,S,C                       ECM = 22 GEV
14  U,D,S,C,B + GLUON             ECM = 28 GEV
15  HEAVY LEPTON ( PETERSEN )
16  SUPER HEAVY LEPTON  MASS=13GEV ( PETERSEN )   ECM=30
17  U,D,S,C,B + GLUON             ECM = 31.6 GEV
18  U,D,S,C,B + GLUON             ECM = 30 GEV
19  U,D,S,C,B + GLUON             ECM = 22 GEV
20  U,D,S,C,B + GLUON             ECM = 27 GEV
21  U,D,S,C,B + GLUON             ECM = 30 GEV, <PT> = 400 MEV
22  STANDARD U,D,S,C,B QUARK MONTE CARLO ( T. MEYER )<PT>=500MEV,ECM=30
23  LUND MONTE CARLO U,D,S,C,B <PT>=500MEV ECM=30GEV
24  LUND MONTE CARLO U,D,S,C,B    ECM=12GEV
25  LUND MONTE CARLO U,D,S,C      ECM=12GEV
26  U,D,S,C,B, + GLUON            ECM=30GEV GLUON FLAGED IN BANK 'VECT'
27  U,D,S,C,B, + GLUON (LUND MC)  ECM=30GEV
28  GAMMA GAMMA ECM=12GEV     ( BRODSKY, LUND PROGRAM FOR 2 PHOTON MASS,
                                RHO-LIKE GAMGAM CROSS SECTION )
29  U,D,S,C,B, + GLUON (LUND MC) FORMING GLUEBALL ( ECM=30GEV)
30  U,D,S,C,B, + GLUON (LUND MC) FORMING GLUEBALL ( ECM=30GEV)
                        BR(GLUEBALL-->4PI) = 100%
31  U,D,S,C,B, + GLUON (LUND MC) ( ECM=30GEV)
32  U,D,S,C,B, + GLUON (LUND MC) FORMING GLUEBALL ( ECM=30GEV)
                         BR(GLUEBALL-->OMEGA) = 100%
33  U,D,S,C,B, + G + GG (ALI MC) ( ECM=30GEV)
34  U,D,S,C,B, + G + GG (ALI MC) ( ECM=36GEV)
35  U,D,S,C,B,T + G + GG (ALI MC) ( ECM=36GEV), TOP= 17 GEV
36  TOP ONLY             (ALI MC) ( ECM=36GEV), TOP= 17 GEV
37  GAMMA GAMMA ECM=36GEV     ( BRODSKY, LUND PROGRAM FOR 2 PHOTON MASS,
                                RHO-LIKE GAMGAM CROSS SECTION )
38  U,D,S,C,B, + GLUON            ECM=37GEV


STATUS OF OLD DATASETS
========================================================================
THE OLD STOR05 DATASETS HAVE BEEN COPIED TO TAPE AND HAVE BEEN RENAMED
OLD NAME            NEW NAME
----------------------------------
.DTOP1030       .TEMPTAPE.DTOP1030
.DTOP1430       .TEMPTAPE.DTOP1430
.DTOP1630       .TEMPTAPE.DTOP1630
