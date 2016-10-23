C   23/03/97 703231335  MEMBER NAME  JEOSUM2  (JADESR)      FORTRAN
C   04/03/82 204012259  MEMBER NAME  CLGWORK1 (JADESR)      MACRO
C     USED IN LGANAL PART OF PROGRAMS
C        NO CUTS ON MAX FIRIED #CHANNELS (I.E. DIMENSION=2880)
C        CHANGE SIZE OF THE CLUSTER BANK TO 16.. 13/6/80
C        CHANGED BY Y.WATANABE ON 23/09/79. 00:30
      COMMON /CWORK/ LNG,HNORD,HNORML,HPOINT(4),HLGADC(2,2880),
     1               LGCL(3),LNGCL,NPNT(4),IDENT(2),NCLST,NCLBEC(3),
     2               ETOT(4),NNEUT,EGAM(4),IFLAG(5),NWPCL,
     3               MAPCL(81),  CLPRP(1280)
      INTEGER *2 HMAPCL(2,81)
      DIMENSION ICLSPR(16,80),CLSPRP(16,80), CLMPRP(1361)
      EQUIVALENCE (HMAPCL(1,1),MAPCL(1)), (CLMPRP(1),MAPCL(1))
      EQUIVALENCE (ICLSPR(1,1),CLPRP(1)),  (CLSPRP(1,1),CLPRP(1))
C
C   01/04/82 204012259  MEMBER NAME  CUTSR1   (JADESR)      MACRO
C-------------------------------
C  MACRO CUTSR1 .... REDUC1 CUTS
C-------------------------------
      COMMON/CUTSR1/NVRSN,NRUNST,NEVTST,NOTTOT,NOTRUN(10),ELGLM( 8)
C--------- END OF MACRO CUTSR1 ------------
C   04/03/82 204012259  MEMBER NAME  COPYME-1 (JADESR)      MACRO
------|****|---|*|---- COPYMEM -----------------------------------------
C   04/03/82 204012259  MEMBER NAME  COPYMEM  (JADESR)      MACRO
------|****|--|**|---- COPYMEM -----------------------------------------
    01/04/82            MEMBER NAME  GETMUONS (JADESR)      TEXT


PLEASE DO NOT DELETE THIS LOAD MODULE UNDER ANY CIRCUMSTANCES
IT IS THE LOAD MODULE FOR THE MUONSTEP IN THE REDUC1 MERGE JOB
THE SOURCE IS IN "F22RJB.SOURCE(GETMUONS)"
FOR INFORMATION PLEASE CONSULT ROGER BARLOW

                                      GFP
    30/03/82            MEMBER NAME  REDNOTE  (JADESR)      TEXT
      CUTS FOR FIRST DATA REDUCTION
      =============================

      P. STEFFEN 82/02/17

      1. TRIGGER CHECK: SUBROUTINE TRCK82(LBACC)
      ------------------------------------------
         LBACC = 0 : EVENT REJECTED
                 1 : EVENT ACCEPTED (BUT NO PATREC WILL BE CALLED)
                 2 : EVENT ACCEPETED, SLOW PATREC WILL BE CALLED
                 4 : EVENT ACCEPTED IF >0 TRACKS FROM BEAM VERTEX(R,FI)
                 8 : EVENT ACCEPTED IF Z-VERTEX < 300 MM
                           .AND. >0 TRACKS FROM VERTEX(R,FI,Z)

         T1-ACCEPT TRIGGERS (+ FWMU(T1-POSTP.)) :
         -----------------------------------------
             I          I                                     I
         BIT I TRIGGER  I TREATMENT                           I LBACC
         ---------------------------------------------------------------
           1   LUMI       ACC. (SCALED DOWN BY 4)               0,1
           2   E > 4      E > EBEAM/2 : ACCEPT
                          2 CONDITIONS OF (EC1>.5),(EC2>.5),    0,2
                                          (EB >.5) (GEV)
           3   TAG *      IF SINGLE TAG : E>.1GEV IN OPPOSITE0,2
               (E>2GEV)                   END CAP OR BARREL
           4   FWMU       CHECK IF MU-CAND (SUBR. MEWT3)        0,4
          (8)  FWMU       CHECK IF MU-CAND (SUBR. MEWT3)        0,4
          (T1-POSTP.)

           5   )
           6   ) UNKNOWN  PRINT WARNING + REJECT                0
           7   )
           8   )

           9   (EC1>.5)*  (EC1>2.)*(EC2*2.) : ACCEPTED          2
                (EC2>.5)  OTHERWISE TRACK FROM VERTEX (R,FI)    4
          10   (EB>1.5)*  ACCEPTED                              2
               (EC1,2>0.5)
          11   (EB>1.5)   ACC. IF TAG LATC                      2
                * TAG
          12   (EB>2.0)   ACC. IF NO COSMIC (SUBR. LGBRCS)      0,2

          13   )
          14   ) UNKNOWN  PRINT WARNING + REJECT                0
          15   )
          16   RANDOM     ACCEPTED (NO SLOW PATREC)1

         T2-BITS :  ACC. IF TRACK CANDIDATES IN RING 3, WHICH ARE
         ---------          MATCHED WITH TOF-COUNTERS
             I          I                                     I
         BIT I TRIGGER  I TREATMENT                           I LBACC
         ---------------------------------------------------------------
           1   2 TRACKS   ACC. IF 2 TRACK CAND.                 0,8
           2   3 TRACKS   ACC. IF 3 TRACK CAND.                 0,8
           3   1 TRACKS   ACC. IF 1 TRACK CAND.                 0,8
           4   2 TRACKS   ACC. IF 2 TRACK CAND.                 0,8
                 COLL.


      2. REDUC1 : SUBROUTINE USRED82
      ------------------------------
         - REJECT EVENT IF RECORD TYPE >63(E.G. PULSER RECORDS)

         - CALL LGCALB (LG-CALIBRATION)

         - CALL TRCK82(LBACC) (TRIGGER CHECK)
           LBACC
             0  :  REJECT EVENT
             1  :  ACCEPT EVENT (NO FURTHER CALCULATIONS)
             2  :  ACCEPT EVENT,  CALL ZVERTF, CALL PATREC(SLOW)
             4  :  CALL PATREC(FAST), ACCEPT IF
                   > 0 TRACKS FROM VERTEX (R,FI)
             8  :  CALL ZVERTF : POSTPONE IF |ZVERT| < 300 MM,
                   CALL PATREC(FAST): ACCEPT IF
                                      > 0 TRACKS FROM VERTEX (R,FI,Z),
                   CALL PATREC(SLOW)


      3. STATUS :
      -----------
         - 3500 REF. EVENTS (RUN 10163) ARE ON MSS : F11PST.MSSTST1

         - 1027 REDUC1-EVENTS           ARE ON MSS : JADEPR.RED1OUT1

         - WITH THE PRESENT CUTS 30 % OF THE RAW EVENTS GET ACCEPTED
           (RUN 10163 HAS BEEN ANALYZED).
           THIS IS FAR TO MUCH; ABOUT TWICE THE CPU IS NEEDED.
           THE ACCAPTED EVENTS HAVE THE FOLLOWING ORIGIN:
            9 % T1-POSTPONE TRIGGER (ACCEPTED AFTER ZVERT + PATREC)
            3 % LUMI + RANDOM TRIGGER
           18 % T1-ACCEPT TRIGGER:
                9% E  > 4GEV
                2% EC1*EC2
                7% EB > 2GEV

                9% E > 4GEV
         - THE FOLLOWING CHANGES ARE PROPOSED:
                                                                 REJECT
           LBACC = 4,8 : >1 TRACK FROM VERTEX REQUIRED            2.5 %
           E > 4GEV    : > 4 GEV IN LG                            3   %
           EC1*EC2     : ACCEPT IF EC1,2 > EBEAM/2 ,
                         >1 TRACK FROM VERTEX (R,FI)
                            IF EC1,2 > .5 GEV                     1   %
           EB > 2GEV   : ACCEPT IF EB > 3 GEV                     4   %

         - WITH THESE CHANGES WE GET DOWN TO ABOUT 20 % OF ACC. EVENTS.
           THIS IS STILL A LITTLE BIT TOO MUCH.

 ***********************************************************************
           SCAN ACCEPTED AND/OR REJECTED EVENTS.
           THE EVENTS REJECTED BY THESE ADDITIONAL CUTS CAN BE INSPECTED
           ON THE DATA SETS:
           JADEPR.REJ820   : ALL EVENTS REJECTED BY THE SHARPER CUTS.
                             A LIST WITH REASON CODES IS AVAILABLE.
           JADEPR.RED1OUT2 : EVENTS ACCEPTED WITH SHARPER CUTS.

    01/04/82 208201810  MEMBER NAME  REDUC    (JADESR)      TEXT

        *****************************************************
        *                                                   *
        *            FIRST STEP DATA REDUCTION   1981       *
        *                                                   *
        *****************************************************

          THE CURRENT STATUS OF THE REDUCTION IS STORED IN
                JADEPR.JADESR(REDUC)

     THE CORRESPONDING BOOK-KEEPING FOR  1980  IS FOUND IN THE MEMBER
             REDUC80

     THE CORRESPONDING BOOK-KEEPING FOR  1979  IS FOUND IN THE MEMBERS
             REDUC79 AND REDUC179

         REFORMATTED TAPES HAVE THE NAMES F11LHO.JDATA02.REFORM.GXXXXV00
               WHERE XXXX IS THE DATA GENERATION NO. GIVEN BELOW

         REDUCED TAPES HAVE THE NAMES JADEPR.REDUC1.GXXXXV00
               WHERE XXXX IS THE DATA GENERATION NO. GIVEN BELOW

  INTERMEDIATE TAPES HOLDING SMALL AMOUNTS OF REDUC1 OUTPUT DATA WILL
  TEMPORARILY EXIST AND WILL BE MERGED INTO THE PROPER GENERATION GROUP
  TAPES WHEN ENOUGH DATA IS READY. THESE INTERMEDIATE TAPES CARRY THE
  NAMES    JADEPR.STXXXA,B,C   ETC. WHERE XXX IS THE REFORM TAPE NR.
------------------------------------------------------------------------
               I   REFORMATTED     I      REDUCED       I  COMMENTS,ETC.
  RUN     ECM  I EVENTS ; DATA GEN#I EVENTS ; TAPE NAME ISUBMITTED JOBNR
---------------I-------------------I--------------------I---------------
      FROM HERE THE RECALIBRATION STARTS. THE NEW TAPE ARE JADEPR.RECLI
------------------------------------------------------------------------
6185-6198      I 34121 (0977)  108 I 5940 (0633)TIMEOUT INO NET
               I               108 I  137 (0359) .ST108AINO NET
               I               108 I 6077 ABOVE TWO TAPES COPIED
               I                   I  BACK TO 108 AFTER ATAG FIXUP
               I               108 I 6077 RECLI RECALIBRATED
6204-6232      I 20193 (0742)  109 I 2663 (0322)        INO NET
               I               109 I 2663 RECLI RECALIBRATED
6233-6275      I 24640 (0430)  110 I 2645 (0426)        INETID=JADEPRN0
               I               110 I 2645 RECLI RECALIBRATED
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0191V00
    11385  EVENTS      NEW CONSTANTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6276-6296      I 27759 (0636)  111 I 3456 (0656)        INO NET
               I               111 I  RECLI RECALIBRATED
6300-6315      I 30225 (0639)  112 I  COSMIC OR PULSER, NO REDUC
6316-6320      I 40742 (0642)  113 I        "
6321-6324      I 25917 (0644)  114 I        "
6253           I  7122 (0609)  115 I  799 (1317)TIMEOUT INO NET
6253           I                   I  300 (0636) .ST115AINO NET
               I               115 I 1099 RECLI RECALIBRATED
6325-6328      I  5970 (0640)  116 I  COSMIC OR PULSER, NO REDUC
   -           I     0 (1371)  117 I    0 (1282)        INETID=JADEPRN7
6329-6339      I 30779 (1385)  118 I 4199 (1285)TIMEOUT INETID=JADEPRN8
               I                   I  143 (0652) .ST118AINO NET
               I                   I   69 (0014) .ST118BINO NET
               I               118 I 4411 RECLI RECALIBRATED
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0192V00
       8966 EVENTS      NEW CONSTANTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6340-6342      I 19454 (1393)  119 I 2914 (1288)        INETID=JADEPRN9
               I               119 I 2914 RECLI RECALIBRATED
6343-6348      I 28066 (0419)  120 I 3517 (0415)TIMEOUT INETID=JADEPRN0
               I                   I   94 (0657) .ST120AINO NET
               I               120 I 3611 RECLI RECALIBRATED
6349-6351      I 21887 (1557)  121 I 2718 (1544)        INETID=JADEPRN1
               I               121 I 2718 RECLI RECALIBRATED
6352-6357      I 21746 (1559)  122 I 2900 (1547)        INETID=JADEPRN2
               I               122 I 2900 RECLI RECALIBRATED
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0193V00
      12143    EVENTS          NEW CONSTANTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6358-6368      I 25540 (1560)  123 I 3895 (1552)        INETID=JADEPRN3
               I               123 I 3895 RECLI RECALIBRATED
6369-6375      I 29899 (0605)  124 I 5362 (0601)        INETID=JADEPRN4
               I               124 I 5362 RECLI RECALIBRATEDNEW TAPE!!!
6377-6379      I 22426 (0606)  125 I 3459 (0604)        INETID=JADEPRN5
               I               125 I 3459 RECLI RECALIBRATED
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0194V00
   12716  EVENTS     NEW CONSTANTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6380-6384      I 26208 (1918)  126 I 3495 (1857)        INETID=JADEPRN6
               I               126 I 3495 RECLI RECALIBRATED
6385-6388      I 18715 (1919)  127 I 2830 (1912)        INETID=JADEPRM7
               I               127 I 2830 RECLI RECALIBRATED
6389-6396      I 26148 (1921)  128 I 3569 (1866)        INETID=JADEPRN8
               I               128 I 3569 RECLI RECALIBRATED
6397-6403      I 23342 (1922)  129 I 3553 (1871)        INETID=JADEPRN9
               I               129 I 3553 RECLI RECALIBRATED
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0195V00
     13447  EVENTS       NEW CONSTANTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6404-6407      I 28638 (1929)  130 I 4527 (1877)        INETID=JADEPRN0
               I               130 I 4527 RECLI RECALIBRATED
6408-6410      I 22888 (1335)  131 I 3492 (1332)        INETID=JADEPRN1
               I               131 I 3492 RECLI RECALIBRATED
6411-6416      I 31279 (1337)  132 I 4740 (1333) TIMEOUTINETID=JADEPRN2
               I                   I  156 (1846) .ST132AINO NET
               I               132 I 4896 RECLI RECALIBRATED
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0196V00
     12915 EVENTS                 NEW CONSTANTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6419-6423      I 32164 (1341)  133 I 5813 (1334)        INETID=JADEPRN3
               I               133 I 5813 RECLI RECALIBRATED
6424-6427      I 22205 (0927)  134 I 3571 (0923)        INETID=JADEPRN4
               I               134 I 3571 RECLI RECALIBRATED
6401,17,18     I 16560 (1764)  135 I 2791 (1709)        INETID=JADEPRN5
               I               135 I 2791 RECLI RECALIBRATED
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0197V00
     12175  EVENTS    NEW CONSTANTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6428-6435      I 30160 (1769)  136 I 5205 (1710) TIMEOUTINETID=JADEPRN6
               I                   I   80 (0118) .ST136AINO NET
               I               136 I 5285 RECLI RECALIBRATED
6436-6445      I 27330 (1773)  137 I 4957 (1722)        INETID=JADEPRM7
               I               137 I 4957 RECLI RECALIBRATED
6446-6450      I 31411 (1778)  138 I 5022 (1715)        INETID=JADEPRN8
               I               138 I 5022 RECLI RECALIBRATED
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0198V00
   15264 EVENTS  NEW CONSTANTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6451-6455      I 28754 (1762)  139 I 6072 (1508)SLOW    INO NET
               I               139 I 6072 RECLI RECALIBRATED
               I               139 I 6072 RECLA RECALIBRATED
6456-6461      I 22481 (1042)  140 I 2536 (1844)PATREC  INO NET
               I               140 I 2536 RECLI RECALIBRATED
               I               140 I 2536 RECLA RECALIBRATED
6462-6466      I 25269 (1773)  141 I 3843 (1848)OUT FROMINO NET TIMEOUT
               I                   I  413 (0316) .ST141AINO NET
               I               141 I 4256 RECLI RECALIBRATED
               I               141 I 4256 RECLA RECALIBRATED
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0199V00
         12684 EVENTS          NEW CONSTANTS   NEW TAPE
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6467-6473      I 24663 (1045)  142 I 3719 (1849)139 ON. INO NET TIMEOUT
               I                   I  320 (0321) .ST142AINO NET
               I               142 I 4039 RECLI RECALIBRATED
               I               142 I 4039 RECLA RECALIBRATED
6474-6486      I 29796 (1047)  143 I 3875 (1852)        INO NET TIMEOUT
               I                   I 1339 (0323) .ST143AINO NET
               I               143 I 5214 RECLI RECALIBRATED
               I               143 I 5214 RECLA RECALIBRATED
6487-6490      I 24362 (1056)  144 I 5314 (1856)        INO NET
               I               144 I 5314 RECLI RECALIBRATED
               I               144 I 5314 RECLA RECALIBRATED
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0200V00
       14567 EVENTS                 NEW CONSTANTS NEW TAPE
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6491-6494      I 26203 (0922)  145 I 3560 (0910)        INETID=JADEPRD5
               I               145 I 3560 RECLI RECALIBRATED
               I               145 I 3560 RECLA RECALIBRATED
6495-6501      I 28047 (0923)  146 I 4464 (0912)        INETID=JADEPRD6
               I               146 I 4464 RECLI RECALIBRATED
               I               146 I 4464 RECLA RECALIBRATED
6502-6505      I 29356 (0925)  147 I 5460 (0914)        INETID=JADEPRD7
               I               147 I 5460 RECLI RECALIBRATED
               I               147 I 5460 RECLA RECALIBRATED
6506-6510      I 23460 (0928)  148 I 4216 (0915)        INETID=JADEPRD8
               I               148 I 4216 RECLI RECALIBRATED
               I               148 I 4216 RECLA RECALIBRATED
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0201V00
         17700 EVENTS             NEW CONSTANTS   NEW TAPE
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
**********************************************************************

              THE RUTHERFORD CONNECTION STARTS WITH TAPE ST149

**********************************************************************
------------------------------------------------------------------------
6511-6516      I 29397 (0079)  149 I 5791 (0916)        I + RUTHERFORD
6517-6521      I 25155 (0988)  150 I 4578 (0917)        I + RUTHERFORD
6522-6526      I 30921 (1034)  151 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0202V00
         15817 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6527-6532      I 25251 (1038)  152 I      RUTHERFORD    I---
6533-6542      I 32156 (1045)  153 I      RUTHERFORD    I---
6543-6547      I 22141 (1211)  154 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0203V00
         14754 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6548-6566      I 28013 (1228)  155 I      RUTHERFORD    I---
6567-6571      I 30157 (1218)  156 I      RUTHERFORD    I---
6572-6576      I 28714 (1834)  157 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0204V00
         18678 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6577-6584      I 22137 (0037)  158 I      RUTHERFORD    I---
6585-6593      I 31562 (0902)  159 I      RUTHERFORD    I---
6594-6604      I 25333 (0911)  160 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0205V00
         14442 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6605-6610      I 24774 (0920)  161 I      RUTHERFORD    I---
6485           I  2157 (1375)  162 I      RUTHERFORD    I---
6611-6613      I 21286 (1382)  163 I      RUTHERFORD    I---
6614-6624      I 26013 (1390)  164 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0206V00
         13547 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6625-6629      I 25601 (1397)  165 I      RUTHERFORD    I---
6630-6633      I 27167 (1440)  166 I      RUTHERFORD    I---
6634-6637      I 22049 (0935)  167 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0207V00
         12695 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6638-6647      I 21928 (0937)  168 I      RUTHERFORD    I---
6648-6659      I 26069 (0940)  169 I      RUTHERFORD    I---
6660-6664      I 29116 (1058)  170 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0208V00
         14298 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6665-6671      I 22973 (1060)  171 I      RUTHERFORD    I---
6672-6679      I 23711 (1067)  172 I      RUTHERFORD    I---
6680-6684      I 25763 (1069)  173 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0209V00
      14665 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6688-6692      I 25176 (1073)  174 I      RUTHERFORD    I---
6693-6697      I 26402 (1602)  175 I      RUTHERFORD    I---
6703-6707      I 28952 (1614)  177 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0210V00
      17432 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6698-6702      I 29571 (1606)  176 I 6878 (1472) .ST176 I+RUTHERFORD
6708-6712      I 26926 (0189)  178 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0211V00
      13310 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6713-6720      I 27260 (0200)  179 I 6238 ST179         I+RUTHERFORD
6721-6724      I 21912 (0213)  180 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0212V00
      11721 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6726-6730      I 28760 (1471)  181 I      RUTHERFORD    I---
6731-6736      I 23302 (1477)  182 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0213V00
      12241 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6737-6741      I 27886 (1482)  183 I      RUTHERFORD    I---
6742-6746      I 27056 (1486)  184 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0214V00
      12172 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6747-6750      I 25928 (1489)  185 I      RUTHERFORD    I---
6751-6755      I 27070 (1501)  186 I 6556 (1805) .ST186 I+RUTHERFORD
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0215V00
      12503 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6756-6759      I 21437 (1483)  187 I      RUTHERFORD    INEW WRONG JTAB.
6760-6763      I 26097 (1491)  188 I      RUTHERFORD    INEW JTPL, JTAB.
                               189 ??????
6771-6774      I 23104 (1514)  190 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0216V00
      16715 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6775-6783      I 30120 (1522)  191 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0217V00
      14630 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6784-6787      I 24498 (1526)  192 I      RUTHERFORD    I---
6725           I  3497 (1667)  193 I      RUTHERFORD    I---
6781           I  6650 (1672)  194 I      RUTHERFORD    I---
6788-6792      I 22985 (1677)  195 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0218V00
      13582 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
               I       (0892)  196 I      RUTHERFORD    I---
6797-6801      I 26900 (1690)  197 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0219V00
      12834 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6802-6806      I 22628 (0778)  198 I 5307 (1226) .ST198 I+RUTHERFORD
6807-6811      I 30290 (0781)  199 I 7084 (1229) .ST199 I+RUTHERFORD
               I       ( "  )  199 I  662 (1011) .ST199AI+RUTHERFORD
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0220V00
      13053 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6812-6820      I 30178 (0788)  200 I      RUTHERFORD    I---
6821-6824      I 25480 (0792)  201 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0221V00
      13495 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6825-6829      I 22988 (0795)  202 I      RUTHERFORD    I---
6830-6834      I 25764 (0798)  203 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0222V00
      11823 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6835-6839      I 30354 (0803)  204 I      RUTHERFORD    I---
6840-6848      I 26001 (1271)  205 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0223V00
      13818 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6849-6853      I 27751 (1273)  206 I      RUTHERFORD    I---
6854-6857      I 26068 (1277)  207 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0224V00
      13037 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6858-6866      I 26653 (1041)  208 I      RUTHERFORD    I---
6867-6871      I 27547 (1046)  209 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0225V00
      13291 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6872-6877      I 22842 (1052)  210 I      RUTHERFORD    I---
6878-6882      I 25258 (0893)  211 I 5899 ST 211        I + RUTHERFORD
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0226V00
      10729 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6883-6887      I 30698 (0896)  212 I      RUTHERFORD    I---
6888-6896      I 22194 (0900)  213 I      RUTHERFORD    I---
6897-6901      I 27268 (0907)  214 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0227V00
   19337 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6902-6907      I 29403 (0910)  215 I      RUTHERFORD    I---
6908-6912      I 26666 (0913)  216 I      RUTHERFORD    I---
6861           I  5556 (0915)  217 I      RUTHERFORD    I---
6863           I  4381 (0921)  218 I      RUTHERFORD    I---
6919-6923      I 21880 (0939)  220 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0228V00
      19356 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6913-6918      I 27311 (0931)  219 I      RUTHERFORD    I---
6924-6927      I 25149 (0945)  221 I      RUTHERFORD    I---
6928-6934      I 27986 (0948)  222 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0229V00
      17250 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6935-6938      I 22249 (0952)  223 I      RUTHERFORD    I---
6939-6943      I 28711 (0954)  224 I      RUTHERFORD    I---
6944-6947      I 22142 (0697)  225 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0230V00
      15944 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6948-6952      I 26575 (0319)  226 I      RUTHERFORD    I---
6953-6958      I 24222 (0323)  227 I      RUTHERFORD    I---
6960-6969      I 32751 (0329)  228 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0231V00
    19543 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6970-6973      I 25400 (0330)  229 I      RUTHERFORD    I---
6974-6977      I 23522 (0334)  230 I      RUTHERFORD    I---
6978-6984      I 25102 (0340)  231 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0232V00
      17759 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
6985-6990      I 27348 (1408)  232 I      RUTHERFORD    I---
6991-6995      I 32294 (1414)  233 I      RUTHERFORD    I---
6996-7000      I 30315 (1418)  234 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0233V00
      17807 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7001-7007      I 29005 (1420)  235 I      RUTHERFORD    I---
7008-7015      I 25838 (1426)  236 I      RUTHERFORD    I---
7016-7023      I 29674 (1430)  237 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0234V00
      18747 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7024-7029      I 28503 (0101)  238 I 5520 (0246) .ST238 I+RUTHERFORD
               I                   I  587 (1833) .ST238AI+RUTHERFORD
7030-7034      I 24194 (0107)  239 I      RUTHERFORD    I---
7035-7038      I 22296 (0120)  240 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0235V00
      15306 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7039-7042      I 23701 (0121)  241 I      RUTHERFORD    I---
7043-7050      I 25992 (0122)  242 I      RUTHERFORD    I---
7051-7054      I 21844 (0127)  243 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0236V00
      15373 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7055-7059      I 29317 (0129)  244 I      RUTHERFORD    I---
7060-7064      I 27357 (0131)  245 I      RUTHERFORD    I---
7065-7068      I 20087 (0007)  246 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0237V00
      16288 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7070-7074      I 28852 (0011)  247 I      RUTHERFORD    I---
7075-7078      I 21089 (0013)  248 I      RUTHERFORD    I---
7079-7082      I 26598 (0014)  249 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0238V00
      16961 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7083-7087      I 28074 (0020)  250 I      RUTHERFORD    I---
7088-7091      I 18953 (0024)  251 I      RUTHERFORD    I---
7013           I  7154 (0302)  252 I      RUTHERFORD    I---
7112-7113      I  2818 (0314)  253 I      RUTHERFORD    I---
7092-7096      I 24761 (0322)  254 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0239V00
      17225 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7097-7102      I 29758 (0331)  255 I      RUTHERFORD    I---
  REFORMATTED GENERATION GROUP #5 BEGINS BELOW.
7103-7106      I 25758 (0344)  001 I      RUTHERFORD    I---
7107-7111      I 18916 (0354)  002 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0240V00
      14568 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7114-7117      I 26240 (0362)  003 I      RUTHERFORD    I---
7118-7125      I 22042 (0801)  004 I      RUTHERFORD    I---
7126-7134      I 28761 (0807)  005 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0241V00
      13404 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7135-7142      I 32748 (0819)  006 I 5325 (0006) .ST006 I---
               I                   I  493 (0594) .ST006AI---
7145-7150      I 29369 (1955)  007 IST007 RUTHERFORD    I---
7151-7153      I 19434 (0831)  008 IST007 RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0242V00
   14153  EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7154-7157      I 23097 (1731)  009 I      RUTHERFORD    I---
7158-7163      I 25721 (1738)  010 I      RUTHERFORD    I---
7164-7167      I 25654 (1805)  011 I      RUTHERFORD    I---
7168-7171      I 28103 (1963)  012 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0243V00
   16092    EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7172-7177      I 24070 (1969)  013 I      RUTHERFORD    I---
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0244V00
    4526    EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7178-7185      I 22289 (1971)  014 I 4151 (1953) .ST014 I + RUTHERFORD
        REDONE WITH NEW TRIG CHECK I 4438 (1344) .ST014 I + RUTHERFORD
7186-7195      I 21838 (1974)  015 I 3172 (1955) .ST015 I + RUTHERFORD
        REDONE WITH NEW TRIG CHECK I 4279 (1416) .ST015 I + RUTHERFORD
7196-7201      I 26514 (1976)  016 I 4101 (1956) .ST016 I + RUTHERFORD
        REDONE WITH NEW TRIG CHECK I 4758 (1421) .ST016 I + RUTHERFORD
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0245V00
     13475 EVENTS       NEW TRGCHK
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7202-7206      I 31899 (1978)  017 I 4094 (1957) .ST017 I + RUTHERFORD
        REDONE WITH NEW TRIG CHECK I 4881 (0819) .ST017 I + RUTHERFORD
7207-7210      I 24145 (1982)  018 I 2459 (1960) .ST018 I + RUTHERFORD
        REDONE WITH NEW TRIG CHECK I 3051 (0836) .ST018 I + RUTHERFORD
7211-7216      I 28316 (1987)  019 I 2865 (0590) .ST019 I + RUTHERFORD
        REDONE WITH NEW TRIG CHECK I 3959 (1290) .ST019 I + RUTHERFORD
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0246V00
      11891 EVENTS     NEW TRGCHK
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7217-7222      I 34666 (1774)  020 I 5846 (0591) .ST020 I + RUTHERFORD
        REDONE WITH NEW TRIG CHECK I 7326 (1300) .ST020 I + RUTHERFORD
7223-7227      I 29329 (1775)  021 I 5313 (0592) .ST021 I + RUTHERFORD
        REDONE WITH NEW TRIG CHECK I 6097 (1311) .ST021 I + RUTHERFORD
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0247V00
      13423 EVENTS         NEW TRGCHK
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7228-7232      I 31244 (1777)  022 I 4959 (0593) .ST022 I + RUTHERFORD
        REDONE WITH NEW TRIG CHECK I 5854 (1370) .ST022 I + RUTHERFORD
7233-7238      I 31765 (1778)  023 I 4323 (0594) .ST023 I + RUTHERFORD
        REDONE WITH NEW TRIG CHECK I 5202 (1375) .ST023 I + RUTHERFORD
7239-7242      I 27969 (0574)  024 I 3775 (0595) .ST024 I NO RUTHERFORD
        REDONE WITH NEW TRIG CHECK I 4517 (1740) .ST024 I NO RUTHERFORD
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0248V00
        15573 EVENTS          NEW TRGCHK
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7243-7248      I 28097 (0575)  025 I 4016 (0596) .ST025 I       "
        REDONE WITH NEW TRIG CHECK I 4812 (0610) .ST025 I       "
7249-7253      I 30094 (0576)  026 I 4359 (0599) .ST026 I       "
        REDONE WITH NEW TRIG CHECK I 5064 (0612) .ST026 I       "
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0249V00
      9876 EVENTS        NEW TRGCHK
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7254-7258      I 33704 (1899)  027 I 5080 (1059) .ST027 I       "
        REDONE WITH NEW TRIG CHECK I 5839 (0182) .ST027 I       "
7259-7263      I 24838 (1900)  028 I 4183 (1076) .ST028 I       "
        REDONE WITH NEW TRIG CHECK I 4647 (0198) .ST028 I       "
        REDONE WITH NEW TRIG CHECK I  142 (0189) .ST028AI       "
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0250V00
        10628 EVENTS       NEW TRGCHK
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7264-7269      I 38662 (1114)  029 I 7572 (0358) .ST029 I       "
        REDONE WITH NEW TRIG CHECK I 7852 (0249) .ST029 I       "
7270-7277      I 27744 (1116)  030 I 3525 (0361) .ST030 I       "
               I                   I  243 (0341) .ST030AI       "
        REDONE WITH NEW TRIG CHECK I 3807 (0253) .ST030 I       "
        REDONE WITH NEW TRIG CHECK I  252 (1751) .ST030AI       "
7278-7282      I 25749 (0738)  031 I 3217 (0366) .ST031 I       "
               I                   I  148 (0342) .ST031AI       "
        REDONE WITH NEW TRIG CHECK I 3621 (0256) .ST031 I       "
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0251V00
       15532 EVENTS       NEW TRGCHK
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7283-7286      I 27320 (0745)  032 I 3576 (0343) .ST032 I       "
        REDONE WITH NEW TRIG CHECK I 3798 (0259) .ST032 I       "
7287-7291      I 23799 (0788)  033 I 3726 (0344) .ST033 I       "
        REDONE WITH NEW TRIG CHECK I 3944 (0260) .ST033 I       "
7144           I  4588 (0462)  034 I  820 (0116) .ST034 I       "
        REDONE WITH NEW TRIG CHECK I  857 (0261) .ST034 I       "
        REDONE WITH NEW TRIG CHECK I   35 (1765) .ST034AI       "
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0252V00
       8634 EVENTS       NEW TRGCHK
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7292-7295      I  21080(0438)  035 I  3917(1960) .ST035 I   + RUTHERFORD
               I       (0568)  036 I  PULSER RUNS
7325-31        I  25002(0450)  037 I  4795(1965) .ST037 I       "
7332-40        I  28653(1715)  038 I  5749(1993) .ST038 I       "
               I                   I    29(1856) .ST038AI       "
7341-45        I  19082(1535)  039 I  3657(1877) .ST039 I       "
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0253V00
     18146 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7346-52        I  22576( 161)  040 I  3835(1362) .ST040 I       "
7353-57        I  26171( 164)  041 I  5354(1907) .ST041 I       "
7358-61        I  26287(   6)  042 I  4982(1919) .ST042 I       "
7368-70        I  22879(1979)  043 I  5240(0677) .ST043 I       "
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0254V00
     19411 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7371-72        I  25169(1990)  044 I  5468(0681) .ST044 I       "
7375-78        I  19179(   5)  045 I  4194(0683) .ST045 I       "
7379-85        I  26251(  31)  046 I  5981(0689) .ST046 I       "
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0255V00
         15643 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7386-91        I  31558(  46)  047 I  6478(0691) .ST047 I       "
               I                   I   724(1909) .ST047AI       "
               I                   I    55( 582) .ST047BI       "
7392-94        I  16787(0698)  048 I  3644(1381) .ST048 I       "
7395-99        I  23991(0321)  049 I  4686( 594) .ST049 I       "
7400-05        I  27022( 326)  050 I    ? ( 597) .ST050 I       "
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.MCG0256
         21377  EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7406-09        I  22673( 114)  051 I  4525( 995) .ST051 I       "
7410-16        I  30864( 124)  052 I  6624( 997) .ST052 I       "
7415-19        I  18051( 136)  053 I  3149(1001) .ST053 I       "
7420-22        I  19829( 150)  054 I  2938(1153) .ST054 I       "
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.MCG0257
   17236 EVENTS   REMADE AFTER KALIBR CORRECTION
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7423-29        I  22767( 154)  055 I  3296(1168) .ST055 I       "
7430-33        I  25713( 160)  056 I  4831(1172) .ST056 I       "
7434-37        I  24141( 167)  057 I  4625(1173) .ST057 I       "
7438-40        I  22845( 171)  058 I  4848(1990) .ST058 I       "
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.MCG0258
   17600 EVENTS   REMADE AFTER KALIBR CORRECTION
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7441-45        I  26491( 583)  059 I  5395(1994) .ST059 I       "
7446-48        I  20708( 585)  060 I  3631(1995) .ST060 I       "
7449-51        I  19587( 589)  061 I  3385(1997) .ST061 I       "
7452-55        I  28652( 680)  062 I  5798(1999) .ST062 I       "
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.MCG0259
     18209 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7456-63        I  30332( 698)  063 I  6931(   2) .ST063 I       "
7464-66        I  22776( 752)  064 I  4807(   6) .ST064 I       "
7467-70        I  27159( 771)  065 I  5860( 751) .ST065 I       "
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.MCG0260
      17598 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7471-73        I  21779( 782)  066 I  4875( 754) .ST066 I       "
7474-76        I  22881( 796)  067 I  5353( 759) .ST067 I       "
7477-80        I  26969( 801)  068 I  6226( 768) .ST068 I       "
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.MCG0261
      16454 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7481-85        I  27497( 810)  069 I  5686(1283) .ST069 I       "
7486-91        I  23658(1085)  070 I  4088( 838) .ST070 I       "
               I                   I    74(1380) .ST070AI       "
7492-97        I  17961(1091)  071 I  2948(1299) .ST071 I       "
               I                   I   266(1837) .ST071AI       "
7498-01        I  23674(1092)  072 I  4292(1308) .ST072 I       "
               I                   I   110(1839) .ST072AI       "
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.MCG0262
     17462 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7502-04        I  22199(1112)  073 I  4367(1554) .ST073 I       "
7505-07        I  23258(1115)  074 I  4583(1557) .ST074 I       "
7508-11        I  25694(1119)  075 I  5221(1559) .ST075 I       "
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.MCG0263
      14171 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7512-15        I  29060(1127)  076 I  6088(1567) .ST076 I       "
7516-18        I  23124(1133)  077 I  5197(1571) .ST077 I       "
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.MCG0264
    11285 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5135           I   7465(1064)  078 I   702( 665) .ST078 I       "
6246           I   5890(1959)  079 I   246( 670) .ST079 I       "
6218           I    624(1962)  080 I    68( 718) .ST080 I       "
6261           I   7362(1963)  081 I   567(  29) .ST081 I       "
6216           I    795(1967)  082 I    87( 682) .ST082 I       "
6111           I   1676(1969)  083 I   221(  50) .ST083 I       "
6195           I   2173(1970)  084 I   268( 689) .ST084 I       "
6194           I   2156(1973)  085 I   197(1390) .ST085 I       "
6193           I   2127(1978)  086 I   239( 698) .ST086 I       "
6251           I   5379(1979)  087 I   486( 709) .ST087 I       "
6247           I      5(1980)  088 I
6221-22        I   5960(1982)  089 I   632( 710) .ST089 I       "
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.MCG0265
      3713 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7519-22        I  24141( 371)  090 I  4621( 718) .ST090 I       "
7523-35        I  21154( 382)  091 I  3970( 720) .ST091 I       "
7536-40        I  30863( 383)  092 I  5963( 722) .ST092 I       "
7541-44        I  28817( 387)  093 I  5929( 723) .ST093 I       "
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.MCG0266
   20483 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7545-50        I  22337( 389)  094 I  4627( 725) .ST094 I       "
7551-53        I  23070( 392)  095 I  3917( 727) .ST095 I       "
                                   I   372(1372) .ST095AI       "
7554-63        I  51563( 306)  096 I 10357( 846) .ST096 I       "
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.MCG0267
     19273 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7566-72        I  23393( 406)  097 I  3769(1856) .ST097 I       "
7573-77        I  29340( 411)  098 I  6372(1859) .ST098 I       "
7578-80        I  21179( 320)  099 I  4294( 850) .ST099 I       "
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.MCG0268
   14435 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7581-85        I  31646( 417)  100 I  5660(1864) .ST100 I       "
7586-88        I  19545(1607)  101 I  3000(1870) .ST101 I       "
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.MCG0269
   8660 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7461           I  7508 ( 583)  102 I  1663(0047) .ST102 I       "
 SO FARE ALL DATA ARE REDUCED   A. PETERSEN 12.6.81
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.MCG0270
   1663 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
7584-96        I  29129(    )  104 I      (1882) .ST104 I       "
               I       (    )  104 I   197( 500) .ST104AI       "
7597-03        I  36857(  83)  105 I 10753(1646) .ST105 I       "
7604-17        I  24596(  90)  106 I  4206( 939) .ST106 I       "
7618-32        I  30278( 718)  107 I  7332(1865) .ST107 I       "
7633-44        I  40833( 730)  108 I 12876(1869) .ST108 I       "
7645-55        I  35218( 733)  109 I 10922(1882) .ST109 I       "
7656-64        I  28894( 377)  110 I  7854(1362) .ST110 I       "
7665-70        I  42254( 381)  111 I 11689(1366) .ST111 I       "
7671-76        I  32976( 388)  112 I      (1381) .ST112 I       "
               I                   I    19( 510) .ST112AI       "
7677-81        I  28870(1368)  113 I  7400(1387) .ST113 I       "
7682-86        I  31387(1373)  114 I  8360(1389) .ST114 I       "
7687-91        I  38971(1375)  115 I  9579(1925) .ST115 I       "
7692-96        I  37379(1377)  116 I  9969(1937) .ST116 I       "
7697-00        I  29517( 324)  117 I  7807(1942) .ST117 I       "
7701-07        I  34102( 326)  118 I  8497(1945) .ST118 I       "
7708-20        I  34218( 333)  119 I  6882(1951) .ST119 I       "

      REDUC1 TAPES DONE AT DESY OF THE PERIOD OCT -DEC 81

9246-50        I  31834(    )  110 I  9121(1170) .ST110 I       "
9251-55        I  31333(    )  111 I  8809(    ) .ST111 I       "
9276-80        I  25571(    )  116 I  6760( 162) .ST116 I       "
               I       (    )  116 I   718( 182) .ST116AI       "
9706-10        I  32124(    )  191 I  9984( 341) .ST191 I       "
9711-17        I  26652(    )  192 I  7927( 171) .ST192 I       "
9723-26        I   7044(    )  198 I  2108( 198) .ST198 I       "

      REDUC1 TAPES DONE AT DESY OF THE PERIOD AUGUST 1982

JDATA07 G0208  I  42021(    )  208 I 10544(    ) .ST208 I  RUNS -
JDATA07 G0209  I  39314(    )  209 I  9161(    ) .ST209 I  RUNS - 11690
    01/04/82 208201810  MEMBER NAME  REDUCSUM (JADESR)      TEXT

        =====================================================
        I                                                   I
        I                                                   I
        I             FIRST STEP DATA REDUCTION             I
        I                                                   I
        I                    SUMMARY                        I
        I                                                   I
        I                 (REDUC1 AT DESY)                  I
        =====================================================

I==============I================I======================================
IJADEPR.RED1HH.I NO OF EVENTS   I
I   STXXX      I READ   WRITTEN I
===============I================I=======================================
I    213       I 24471   11068  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0091
I    214       I 20750    9986  I
--------------------------------          RUN 17340-17360
I    215       I 25450   11360  I
------------------------------------------------------------------------
I    216       I 21741    9664  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0092
I    217       I 22516    9257  I
--------------------------------          RUN 17361-17376
I    218       I 23860    9983  I
------------------------------------------------------------------------
I    219       I 21405    9767  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0093
I    220       I 18607    8265  I
--------------------------------          RUN 17377-17394
I    221       I 22348   10025  I
------------------------------------------------------------------------
I    222       I 18975    8861  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0094
I    223       I 19243    7742  I
--------------------------------          RUN 17395-17417
I    224       I 21108    9458  I
------------------------------------------------------------------------
I    225       I 18221    7318  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0095
I    226       I 22836    8038  I
--------------------------------          RUN 17418-17432
I    227       I 16185    6674  I
------------------------------------------------------------------------
I    228       I 10217    4323  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0096
I    229       I 20991    8293  I
--------------------------------          RUN 17433-17447
I    230       I 18296    6069  I
------------------------------------------------------------------------
I    231       I 17124    6031  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0097
I    232       I 19398    5726  I
--------------------------------          RUN 17448-17469
I    233       I 21475    6673  I
------------------------------------------------------------------------
I    234       I  4750    1316  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0098
I    235       I 18135    6580  I
--------------------------------          RUN 17457,17458,17470-17482
I    236       I  1562     555  I
--------------------------------
I    237       I 27242   11035  I
------------------------------------------------------------------------
I    238       I 16730    7203  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0099
I    239       I 23772   10408  I
--------------------------------          RUN 17483 - 17498
I    240       I 18414    7435  I
------------------------------------------------------------------------
I    241       I 21866    9484  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0100
I    242       I 19558    7975  I
--------------------------------          RUN 17499 - 17513
I    243       I 22318    8724  I
------------------------------------------------------------------------
I    244       I 18886    8316  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0101
I    245       I 23661   10071  I
--------------------------------          RUN 17514 - 17530
I    246       I 18773    7510  I
------------------------------------------------------------------------
I    247       I 20230    8615  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0102
I    248       I 24205   11113  I
--------------------------------          RUN 17531 - 17549
I    249       I 21754    9660  I
------------------------------------------------------------------------
I    250       I 16723    7332  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0103
I    251       I 20285    8667  I
--------------------------------          RUN 17550 - 17570
I    001       I 20150    8224  I
------------------------------------------------------------------------
I    002       I 24869   10578  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0104
I    003       I 24110   10406  I
--------------------------------          RUN 17571 - 17592
I    004       I 20803    9392  I
------------------------------------------------------------------------
I    005       I 19968    6221  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0105
I    006       I 23247    6233  I
--------------------------------          RUN 17593 - 17609
I    007       I 18771    4928  I
------------------------------------------------------------------------
I    008       I 19164    5453  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0106
I    009       I 21550    7687  I
--------------------------------          RUN 17610 - 17632
I    010       I 25745    9493  I
------------------------------------------------------------------------
I    011       I 22565    8533  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0107
I    012       I 21595    8094  I
--------------------------------          RUN 17633 - 17653
I    013       I 22507    8243  I
------------------------------------------------------------------------
I    014       I 22866    8935  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0108
I    015       I 27050   10443  I
--------------------------------          RUN 17654 - 17672
I    016       I 22197    8783  I
------------------------------------------------------------------------
I    017       I 23748    8763  I
--------------------------------
I    018       I 22333    8493  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0109
I    019       I  4262    1488  I
--------------------------------          RUN 17673 - 17697
I    020       I 25149    9530  I
------------------------------------------------------------------------
I    021       I 25783    9784  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0110
I    022       I 22390    7837  I
--------------------------------          RUN 17698 - 17724
I    023       I 22987    7897  I
------------------------------------------------------------------------
I    024       I 24609    8952  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0111
I    025       I 23910    7423  I
--------------------------------          RUN 17725 - 17746
I    026       I 23595    8874  I
------------------------------------------------------------------------
I    027       I 19639    7682  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0112
I    028       I 22404    8384  I
--------------------------------          RUN 17747 - 17764
I    029       I 24465    8523  I
------------------------------------------------------------------------
I    030       I 22447    8122  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0113
I    031       I 25764    9198  I
--------------------------------          RUN 17765 - 17785
I    032       I 24329    8150  I
------------------------------------------------------------------------
I    033       I 24817    8119  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0114
I    034       I 25204    8767  I
--------------------------------          RUN 17786 - 17807
I    035       I 26081    9517  I
------------------------------------------------------------------------
I    036       I 26461    6809  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0115
I    037       I 23816    3470  I
--------------------------------          RUN 17808 - 17831
I    038       I 19571    5880  I
------------------------------------------------------------------------
I    039       I 20142    7084  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0116
I    040       I 24290    8106  I
--------------------------------          RUN 17832 - 17861
I    041       I 19062    6159  I
------------------------------------------------------------------------
I    042       I 23243    8288  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0117
I    043       I 20413    6642  I
--------------------------------          RUN 17862 - 17897
I    044       I 19633    6784  I
------------------------------------------------------------------------
I    045       I 23113    7874  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0118
I    046       I 20123    7240  I
--------------------------------          RUN 17898 - 17932
I    047       I 21597    6310  I
------------------------------------------------------------------------
I    048       I 18778    6151  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0119
I    049       I 19002    6178  I
--------------------------------          RUN 17933 - 17962
I    050       I 25400    9925  I
------------------------------------------------------------------------
I    051       I 22099    8659  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0120
I    052       I 26613    9638  I
--------------------------------          RUN 17963 - 17988
I    053       I 26556    9112  I
------------------------------------------------------------------------
                 ----   MAGNET PROBLEMS   ----
------------------------------------------------------------------------
I    054       I 28349   10204  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0121
I    055       I 25645   10296  I
--------------------------------          RUN 17989 - 18015
I    056       I 22940    7760  I
------------------------------------------------------------------------
I    057       I 22037    5235  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0122
I    058       I 23952    7915  I
--------------------------------          RUN 18016 - 18041
I    059       I 21797    7119  I
------------------------------------------------------------------------
I    060       I 22651    7652  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0123
I    061       I 22458    7247  I
--------------------------------          RUN 18042 - 18062
I    062       I 23086    7478  I
------------------------------------------------------------------------
I    063       I 20355    6458  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0124
I    064       I 17504    5591  I
--------------------------------          RUN 18063 - 18085
I    065       I 22094    7360  I
------------------------------------------------------------------------
I    066       I 19706    6574  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0125
I    067       I 19973    4031  I
--------------------------------          RUN 18086 - 18111
I    068       I 23011    5169  I
------------------------------------------------------------------------
I    069       I 20939    4946  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0126
I    070       I  3214    1121  I
--------------------------------          RUN 18112 - 18135
I    071       I 20731    4709  I
--------------------------------
I    072       I 22181    8310  I
------------------------------------------------------------------------
I    073       I 23612    9579  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0127
I    074       I 24469    9539  I
--------------------------------          RUN 18136 - 18170
I    075       I 21419    8442  I
------------------------------------------------------------------------
I    076       I 20233    6923  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0128
I    077       I 21554    8292  I
--------------------------------          RUN 18171 - 18200
I    078       I 22153    8291  I
------------------------------------------------------------------------
I    079       I 19166    6246  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0129
I    080       I 22031    6393  I
--------------------------------          RUN 18201 - 18225
I    081       I 21425    7015  I
------------------------------------------------------------------------
I    082       I 19327    6924  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0130
I    083       I 22790    8393  I
--------------------------------          RUN 18226 - 18248
I    084       I 23367    8810  I
------------------------------------------------------------------------
I    085       I 24989    9680  I
--------------------------------     ==>  JADEPR.REDUC1HH.G0131
I    086       I 23481    8725  I
--------------------------------          RUN 18249 - 18275
I    087       I 27421   10336  I
========================================================================
-----------------------------  END OF LIST  ----------------------------
========================================================================

    30/03/82            MEMBER NAME  REDUC0   (JADESR)      TEXT

        *****************************************************
        *                                                   *
        *            FIRST STEP DATA REDUCTION              *
        *                                                   *
        *****************************************************

          THE CURRENT STATUS OF THE REDUCTION IS STORED IN
                JADEPR.JADESR(REDUC)

     THE CORRESPONDING BOOK-KEEPING FOR  1979  IS FOUND IN THE MEMBERS
             REDUC79 AND REDUC179

         REFORMATTED TAPES HAVE THE NAMES F11LHO.JDATA02.REFORM.GXXXXV00
               WHERE XXXX IS THE DATA GENERATION NO. GIVEN BELOW

         REDUCED TAPES HAVE THE NAMES JADEPR.REDUC1.GXXXXV00
               WHERE XXXX IS THE DATA GENERATION NO. GIVEN BELOW

  INTERMEDIATE TAPES HOLDING SMALL AMOUNTS OF REDUC1 OUTPUT DATA WILL
  TEMPORARILY EXIST AND WILL BE MERGED INTO THE PROPER GENERATION GROUP
  TAPES WHEN ENOUGH DATA IS READY. THESE INTERMEDIATE TAPES CARRY THE
  NAMES    JADEPR.STXXXA,B,C   ETC. WHERE XXX IS THE REFORM TAPE NR.
------------------------------------------------------------------------
               I   REFORMATTED     I      REDUCED       I  COMMENTS,ETC.
  RUN     ECM  I EVENTS ; DATA GEN#I EVENTS ; TAPE NAME ISUBMITTED JOBNR
---------------I-------------------I--------------------I---------------
 2517-20  12.0 I 19389      076    I 7094 JADEPR.ST076  I
LAST BEAM BEAM DATA : RUN 2520 !!!
 2521-26  COSM I 54440      077    I 5704 JADEPR.ST077A I
 2521-26    "  I 54440      077    I 6510 JADEPR.ST077B I
 2521-26    "  I 54440      077    I 7557 JADEPR.ST077C I
 2527-30    "  I 51521      078    I 7338 JADEPR.ST078A I
 2527-30    "  I 51521      078    I 7597 JADEPR.ST078B I
 2527-30    "  I 51521      078    I                    I
 2531,32,65 "  I 24712      079    I                    I
------------------------------------------------------------------------
      THE 1980 DATA TAKING STARTS HERE !
------------------------------------------------------------------------
               I   REFORMATTED     I      REDUCED       I  COMMENTS,ETC.
  RUN     ECM  I EVENTS ; DATA GEN#I EVENTS ; TAPE NAME ISUBMITTED JOBNR
---------------I-------------------I--------------------I---------------
2718-32   32.0 I   475       082   I    0  NO TAPE      I
2745-54 MIXED  I 20170       083   I 1765 JADEPR.ST083  I
 2745-47  32.0 I                   I                    I
 2748     33.0 I                   I                    I
 2749     34.0 I                   I                    I
 2750     35.0 I                   I                    I
 2751-54  33.0 I                   I                    I
2755-62 MIXED  I 22011       084   I 3150 JADEPR.ST084  I
 2755     33.0 I                   I                    I
 2756-57  34.8 I                   I                    I
 2758-62  33.0 I                   I                    I
2760      33.0 I  5392       085   I  798       .ST085  I
2763-67    "   I 15576       086   I 2213       .ST086  I
2768-76    "   I 18231       087   I 2475       .ST087  I
2777-81    "   I 20816       088   I 3339       .ST088  I
------------------------------------------------------------------------

      THE ABOVE  6 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0064V00  13740      EVENTS
ORDER  ST083-88            TEST READ OK !!
------------------------------------------------------------------------
2790-91    "   I 13907       089   I 1488       .ST089  I
2792-94    "   I 17838       090   I 2543       .ST090  I
2783-89    "   I  4765       091   I  469       .ST091  I
2795-98    "   I 18001       092   I 2197       .ST092  I
2802-05 MIXED  I  9262       093   I  693       .ST093  I
 RUN 2802 IS STILL 33.0 GEV, THE REST IS 34.0 GEV       I
2806-16   34.0 I 28036       094   I 2603       .ST094  I
2821       "   I  5342       095   I  505       .ST095  I
2822-23    "   I  8028       096   I  692       .ST096  I
2817-2824      I 21351       097   I 1722       .ST097  I
------------------------------------------------------------------------

      THE ABOVE  9 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0065V00    12912    EVENTS
ORDER  ST091,89,90,92-97       TEST READ OK !!!
------------------------------------------------------------------------
2825-2830 MIXEDI 28643       098   I 1829       .ST098  I
 35.0 GEV FROM I                   I                    I
 RUN 2828 ON.  I                   I                    I
2831-34   35.0 I 15658       099   I  778       .ST099  I
2835-38    "   I 33891       100   I 2465       .ST100  I
2839-44    "   I 21906       101   I 1812       .ST101  I
2845-49    "   I 25103       102   I 2640       .ST102  I
2850-52    "   I 18877       103   I 1749       .ST103  I
2853-56    "   I 18514       104   I 1789       .ST104  I
2857-60    "   I 24299       105   I 2217       .ST105  I
------------------------------------------------------------------------

      THE ABOVE  8 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0066V00   15278     EVENTS
ORDER ST098-105          TEST READ OK !
------------------------------------------------------------------------
2861-62  35.0  I 15933         106 I 1516       .ST106  I
2863-68    "   I 19158         107 I 1929       .ST107  I
2869-74    "   I 27684         108 I 2015       .ST108  I
2875-79    "   I 20953         109 I 2416       .ST109  I
2880-88    "   I 31692         110 I 3690       .ST110  I
2886,92    "   I  8494         114 I  751       .ST114  I
2889-94    "   I 24687         111 I 2287       .ST111  I
2895-01    "   I 19823         112 I 1741       .ST112  I
------------------------------------------------------------------------

      THE ABOVE  8 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0067V00   16345     EVENTS
ORDER: 106-10,114,111-12            TEST READ OK !!
------------------------------------------------------------------------
2902-05    "   I 24264         113 I 2191       .ST113  I
2906-08    "   I 23641         115 I 2411       .ST115  I
2909-12    "   I 26527         116 I 2158       .ST116  I
2913-16    "   I 26448         117 I 2358       .ST117  I
2917-22    "   I 29540         118 I 2843       .ST118  I
2926-28    "   I 14442         119 I 1450       .ST119  I
2929-30    "   I  8995         120 I 1044       .ST120  I
------------------------------------------------------------------------

      THE ABOVE  7 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0068V00   14455     EVENTS
ORDER: 113,15-20            TEST READ OK !!
------------------------------------------------------------------------
2923-35    "   I 26788         121 I 2446       .ST121  I
2936-50    "   I 40754         122 I 4569       .ST122  I
2938,47    "   I  4053 (153)   123 I  390 (1947).ST123  I
 2939,48   "   I 10931 (0263)  138 I 1272 (0262).ST138  I
2952-57    "   I 36225         124 I 3113       .ST124  I
2958-60    "   I 20194         125 I 1596       .ST125  I
2961-65    "   I 31369         126 I 2588 (0703).ST126  I
------------------------------------------------------------------------

      THE ABOVE  7 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0069V00    15874    EVENTS
ORDER  121-23,38,24-26            TEST READ OK !!
------------------------------------------------------------------------
2966-71    "   I 30576 (0770)  127 I 3505 (0767).ST127  I
2972-76    "   I 28886 (1737)  128 I 2618       .ST128  I
2977-81    "   I 34243 (1806)  129 I            .ST129  I
2982-89    "   I 29059 (1810)  130 I 2919       .ST130  I
2990-98    "   I 27576         131 I 2885       .ST131  I
------------------------------------------------------------------------

      THE ABOVE  5 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0070V00    15338    EVENTS
ORDER: 127-31          TEST READ OK !!
------------------------------------------------------------------------
2999-03    "   I 34552         132 I 3361       .ST132  I
3004-07    "   I 26676         133 I 2503       .ST133  I
3008-11    "   I 26882 (0317)  134 I 2571 (0475).ST134  I
2985,94    "   I  9271 (1472)  135 I  844       .ST135  I
2995       "   I  7980 (1483)  136 I  633 (1983).ST136  I
3012-16    "   I 29531 (1505)  137 I 2829 (1984).ST137  I
3017-20    "   I 16012 (0122)  140 I 1466 (0989).ST140  I
------------------------------------------------------------------------

      THE ABOVE  7 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0071V00    14207   EVENTS
ORDER: 135,36,32-34,37,40       TEST READ OK !
------------------------------------------------------------------------
      FOR TAPES  ST138,140    SEE ABOVE   (35.0 GEV DATA)
 3037     35.8 I  4193 (0276)  139 I  301 (0273).ST139  I
3037-42   35.8 I 33759 (0134)  141 I 2743 (0990).ST141  I5 RD-ERR.(17EV)
3043-47   35.8 I 28900 (1224)  142 I 2367 (0991).ST142  I
3048-55   35.8 I 29346 (1336)  143 I 2531 (0994).ST143  I
3056-62   35.8 I 31789 (1796)  144 I 2835 (0997).ST144  I
3063-68   35.8 I 34504 (1481)  145 I 2992 (0674).ST145  I
3069-73   35.8 I 29032 (1174)  146 I 2089 (1173).ST146  I
------------------------------------------------------------------------

      THE ABOVE  7 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0072V00   15858     EVENTS
ORDER: 139,41-46               TEST READ OK !
------------------------------------------------------------------------
3074-78   35.8 I 22944 (0161)  147 I 1762 (0677).ST147  I
3079-84   35.8 I 33617 (0050)  148 I 2939 (0680)        IWR.ERR.(ST148)?
               I 33600         148 I 2937 (1375).ST148  I
3085-93   35.8 I 29585 (0028)  149 I 2417 (0681).ST149  I
3094-97   35.8 I 25169 (1172)  150 I 2008 (0690).ST150  I
3098-03   35.8 I 33893 (0432)  151 I 2949 (0692).ST151  I
3104-10   35.8 I 26809 (0345)  152 I 2500 (0208).ST152  I
------------------------------------------------------------------------

      THE ABOVE  6 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0073V00    14573    EVENTS
ORDER: 147-52            TEST READ OK !!
------------------------------------------------------------------------
3111-15   35.8 I 27688 (0764)  153 I 2338 (0512).ST153  I
3116-26   35.8 I 39652 (1677)  154 I 3905 (1969).ST154  I
3122      35.8 I  7988 (1184)  165 I  676 (1142).ST165  I
3127-32   35.8 I 34660 (0399)  155 I 2920 (0514).ST155  I
3133-36 MIXED  I 26719 (0408)  156 I 2682 (0008).ST156  I
 35.8 THROUGH  I                   I                    I
3135. RUN 3136 I                   I                    I
IS 35.0 GEV    I                   I                    I
3137-42   35.0 I 32869 (0418)  157 I 4462 (0050).ST157  I
 JADEPRD7 (0050) READ 32878 EVENTS(1 READ ERROR)
------------------------------------------------------------------------

      THE ABOVE  6 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0074V00    16983    EVENTS
ORDER: 153,154,165,155-57             TEST READ OK !
------------------------------------------------------------------------
                START ENERGY SCAN HERE    35.02  --  35.26  GEV
------------------------------------------------------------------------
3143-49        I 30316 (1815)  158 I 3858 (0904).ST158  I
  ENERGY 35.02   RUNS 3143 - 3150
3147           I  6096 (1187)  166 I  824 (1384).ST166  INO NET
3150-53        I 26827 (0798)  159 I 3423 (1122).ST159  I
  ENERGY 35.04   RUNS 3151 - 3159
3154-58        I 30955 (0711)  160 I 3880 (0054).ST160  I
3160           I  7997 (1189)  167 I  946 (1520).ST167  INO NET
  ENERGY 35.06   RUNS 3160 - 3171
3168           I  5079 (1193)  168 I  629 (1522).ST168  INO NET
------------------------------------------------------------------------

      THE ABOVE  6 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0075V00    13560    EVENTS
ORDER: 158,166,159,160,167-68           TEST READ OK !
------------------------------------------------------------------------
3159-69        I 42805 (1120)  161 I 3711 (0820).ST161  INETID=JADEPRN1
    JADEPRD1 (820) STOPPED AFTER TIME LIMIT: 29988 EVENTS
   "               "     "      "  I 1553 (1278).ST161A I
3170-73        I 26974 (1938)  162 I 3297 (0825).ST162  INETID=JADEPRN2
  ENERGY 35.08   RUNS 3172 - 3184
3174-79        I 34077 (1671)  163 I 3614 (0982).ST163  INETID=JADEPRN3
    JADEPRD3 (982) STOPPED AFTER TIME LIMIT: 30102 EVENTS
   "               "     "      "  I  436 (1294).ST163A I
               I 29326 (0391)  164 I 2443 (0985).ST164  INETID=JADEPRN4
     FOR TAPES 165-68, SEE ABOVE   (RUN NUMBER OUT OF ORDER)
------------------------------------------------------------------------

      THE ABOVE  6 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0076V00    15054    EVENTS
ORDER: 161,61A,62,63,63A,64              TEST READ OK !
------------------------------------------------------------------------
  ENERGY 35.10   RUNS 3185 - 3199
3189-94        I 31451 (1217)  169 I 3312 JOB #976 REMAKES TAPE
3189-94        I 31451 (1217)  169 I 3313 (1893).ST169  I
3195-3200      I 30366 (1218)  170 I 3136 (1207).ST170  INETID=JADEPRN0
  ENERGY 35.12   RUNS 3200 - 3214
3201-05        I 26333 (0834)  171 I 2968 (0739).ST171  INETID=JADEPRN1
3206-11        I 25188 (0837)  172 I 3016 (0744).ST172  INETID=JADEPRN2
3212-18        I 35300 (0841)  173 I 4511 (0747).ST173  INETID=JADEPRN3
  ENERGY 35.14   RUNS 3215 - 3226
------------------------------------------------------------------------

      THE ABOVE  5 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0077V00    16944    EVENTS
ORDER: 169-173            TEST READ OK  !
------------------------------------------------------------------------
3219-23        I 29354 (0848)  174 I 3776 (0748).ST174  INETID=JADEPRN4
3224-28        I 29615 (0852)  175 I 3799 (0749).ST175  INETID=JADEPRN5
  ENERGY 35.16   RUNS 3227 - 3237
3229-34        I 29017 (0972)  176 I 3772 (0959).ST176  INETID=JADEPRN6
  ENERGY 35.18   RUNS 3238 - 3246
3235-40        I 36210 (0975)  177 I 5123 (0963).ST177  INETID=JADEPRN7
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0078V00   16470     EVENTS
ORDER: 174-177            TEST READ OK !
------------------------------------------------------------------------
3241-45        I 26871 (0979)  178 I 3476 (0967).ST178  INETID=JADEPRN8
  ENERGY 35.20   RUNS 3247 - 3258
3246-51        I 27300 (1716)  179 I 3412 (1710).ST179  INETID=JADEPRN9
3252-57        I 28693 (1719)  180 I 3734 (1715).ST180  INETID=JADEPRN0
  ENERGY 35.22   RUNS 3259 - 3272
3258-65        I 33194 (1724)  181 I 4411 (1720).ST181  INETID=JADEPRN1
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0079V00    15033    EVENTS
ORDER: 178-181           TEST READ OK !
------------------------------------------------------------------------
3266-73        I 25313 (0194)  182 I 3348 (0179).ST182  INETID=JADEPRN2
  ENERGY 35.24   RUNS 3273 - 3290
3274-78        I 32677 (0198)  183 I 3879 (0188).ST183  INETID=JADEPRN3
3272,79-86     I 25314 (0805)  184 I 3158 (0814).ST184  INETID=JADEPRN4
3287-8,91-3    I 28216 (1799)  185 I 3443 (1813).ST185  INETID=JADEPRN5
3289-90        I  8693 (1838)  186 I 1010 (1843).ST186  INETID=JADEPRN6
------------------------------------------------------------------------

      THE ABOVE  5 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0080V00    14838    EVENTS
ORDER: 182-186                TEST READ OK !
------------------------------------------------------------------------
  ENERGY 35.26   RUNS 3291 - 3296
C
C   WE DID IT AGAIN !!!   DATA STILL SITTING ON DISK, NOT DUMPED !!
C
3294-96        I 16447 (1893)  187 I 2074 (0008).ST187  INETID=JADEPRN7
3311-18COSMICS I 41735 (1510)  188 I COSMICS NO REDUC.  I
3319-24COSMICS I 47165 (0478)  189 I COSMICS NO REDUC.  I
3313-14COSMICS I 11148 (0713)  190 I COSMICS NO REDUC.  I
C
C   TWO SPECIAL NORD 50 OVERFLOW EVENT  TAPES BELOW.
C
2751-2840 (EVENT 2982)  READ  1097 EVENTS FROM F11LHO.OVCONCN
                        WROTE  807 EVENTS TO   JADEPR.LHOOVR
2840 (EVENT 2983)-3184  READ  2423 EVENTS FROM F11LHO.OVCONCN
                        WROTE 2082 EVENTS TO   JADEPR.LHOOVR1
------------------------------------------------------------------------

         RUNNING PERIOD STARTING 80/04/21

---
       ENERGY  35.20  RUNS 3325 - 3342
---
3319-24 TEST   I 65075 (1398)  195 I    0 (0023).ST195  INETID=JADEPRN5
3325,31-33     I 17272 (1401)  196 I 1811 (0025).ST196  INETID=JADEPRN6
3334-39        I 30002 (1714)  197 I 3196 (1849).ST197  INETID=JADEPRN7
3340-42        I 21196 (1310)  198 I 1860 (1853).ST198  INETID=JADEPRN8
---
       ENERGY  35.22  RUNS 3343 - 3361
---
3343-47        I 25930 (1304)  199 I 2382 (1098).ST199  INETID=JADEPRN9
3348-52        I 21230 (1860)  200 I 2102 (1866).ST200  INETID=JADEPRN0
3353-57        I 23361 (1693)  201 I 2564 (1696).ST201  INETID=JADEPRN1
------------------------------------------------------------------------

      THE ABOVE  7 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0081V00    15989    EVENTS
ORDER: 187,196-201          TEST READ    OK !!
------------------------------------------------------------------------
3358-62        I 19478 (1697)  202 I 2071 (1703).ST202  INETID=JADEPRN2
---
       ENERGY  35.24  RUNS 3362 - 3376
---
3363,67-9      I 24209 (1702)  203 I 2661 (1704).ST203  INETID=JADEPRN3
3370-73        I 27985 (1707)  204 I 2929 (1723).ST204  INETID=JADEPRN4
3374-77        I 26440 (1709)  205 I 2664 (1725).ST205  INETID=JADEPRN5
---
       ENERGY  35.26  RUNS 3377 - 3389
---
3378-83        I 25148 (1711)  206 I 2576 (1726).ST206  INETID=JADEPRN6
3384-87        I 22007 (0617)  207 I 2359 (1734).ST207  INETID=JADEPRN7
------------------------------------------------------------------------

      THE ABOVE  6 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0082V00    15260    EVENTS
ORDER: 202-207               TEST READ  OK !!
------------------------------------------------------------------------
3388-91        I 31952 (0140)  208 I 3573 (1735).ST208  INETID=JADEPRN8
---
       ENERGY  35.28  RUNS 3390 - 3398
---
3392-95        I 29029 (0142)  209 I 3346 (0145).ST209  INETID=JADEPRN9
3396-98        I 20173 (0143)  210 I 2212 (0147).ST210  INETID=JADEPRN0
---
       ENERGY  35.30  RUNS 3399 - 3410
---
3399-3402      I 31922 (0185)  211 I 3012 (0148).ST211  INETID=JADEPRN1
3403-3407      I 31963 (0366)  212 I 3150 (0196).ST212  INETID=JADEPRN2
------------------------------------------------------------------------

      THE ABOVE  5 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0083V00     15293   EVENTS
ORDER: 208-212            TEST READ   OK !!
------------------------------------------------------------------------
3409-11        I 24345 (0936)  213 I 2588 (0197).ST213  INETID=JADEPRN3
---
       ENERGY  35.32  RUNS 3411 - 3422
---
3412-16*DELETEDI 32711 (0941)  214 I 3302 (0221).ST214  INETID=JADEPRN4
3412-16**REMADEI 32711 (0941)  214 I 3302 (0553).ST214A INETID=JADEPRN4
3417-21*DELETEDI 19514 (1357)  215 I 1646 (1343).ST215  INETID=JADEPRN5
3417-21**REMADEI 19514 (1357)  215 I 1645 (0555).ST215A INETID=JADEPRN5
3422-25        I 25120 (0278)  216 I 2350 (1374).ST216  INETID=JADEPRN6
  MINOWA COMPLAINS ABOUT TAPE ST216, PLEASE CHECK !!!
     TAPE 216 REMADE  ********************
---
       ENERGY  35.34  RUNS 3423 - 3429
---
3426-30        I 30949 (0280)  217 I 2880 (0552).ST217  INETID=JADEPRN7
---
       ENERGY  35.376  RUNS 3430 - 3432
---
---
       ENERGY  35.34  RUNS 3433 - 3439
---
3431-37        I 26074 (0282)  218 I 2463 (0293).ST218  INETID=JADEPRN8
3438-41        I 25811 (0431)  219 I 2234 (0635).ST219  INETID=JADEPRN9
            REDUC TAPE 219 REMADE , NOW CONTAINS MORE EVENTS
---
       ENERGY  35.36  RUNS 3440 - 3456
---
3442-48        I 31533 (0434)  220 I 2781 (0638).ST220  INETID=JADEPRN0
------------------------------------------------------------------------

      THE ABOVE  8 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0084V00   20243     EVENTS
ORDER: 213-220              TEST READ OK !

------------------------------------------------------------------------
3449-51        I 18773 (0435)  221 I 1880 (0641).ST221  INETID=JADEPRN1
3452-56        I 26170 (0436)  222 I 2027 (0645).ST222  INETID=JADEPRN2
---
       ENERGY  35.38  RUNS 3457 - 3471
---
3457-59        I 23099 (0445)  223 I 2118 (0444).ST223  INETID=JADEPRN3
3460-63        I 25060 (0556)  224 I 2077 (0538).ST224  INETID=JADEPRN4
3464-67        I 25657 (0561)  225 I 2378 (0540).ST225  INETID=JADEPRN5
3468-71        I 27227 (1672)  226 I 2546 (1674).ST226  INETID=JADEPRN6
---
       ENERGY  35.40  RUNS 3472 - 3491
---
3472-74        I 23979 (0588)  227 I 1905 (0197).ST227  INO NET
------------------------------------------------------------------------

      THE ABOVE  7 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0085V00    14931    EVENTS
ORDER: 221-227           TEST READ OK !

------------------------------------------------------------------------
3475-78        I 21644 (1967)  228 I 1521 (1978).ST228  INETID=JADEPRN8
3479-83        I 25842 (1968)  229 I 1652 (1983).ST229  INETID=JADEPRN9
3484-89        I 25182 (1970)  230 I 1986 (1985).ST230  INETID=JADEPRN0
3490-93        I 28676 (1971)  231 I 2541 (1988).ST231  INETID=JADEPRN1
---
       ENERGY  35.42  RUNS 3492 - 3503
---
3494-97        I 30319 (1975)  232 I 2891 (1989).ST232  INETID=JADEPRN2
3498-3501      I 23955 (1814)  233 I 2518 (1803).ST233  INETID=JADEPRN3
3502-05        I 23674 (1815)  234 I 2145 (1804).ST234  INETID=JADEPRN4
------------------------------------------------------------------------

      THE ABOVE  7 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0086V00    15254    EVENTS
ORDER: 228-234            TEST READ OK !

------------------------------------------------------------------------
---
       ENERGY  29.90  RUNS 3504 - 3518
---
3504,06-10,12,13 43503 (1817)  235 I 4319 (1806).ST235  INETID=JADEPRN5
3514-17        I 29225 (0788)  236 I 3759 (0792).ST236  INETID=JADEPRN6
---
       ENERGY  29.92  RUNS 3519 - 3530
---
3518-23        I 30948 (0373)  237 I 3740 (0480).ST237  INETID=JADEPRN7
3524-28        I 32963 (0380)  238 I 3702 (0485).ST238  INETID=JADEPRN8
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0087V00    15520    EVENTS
ORDER: 235-238            TEST READ OK !
------------------------------------------------------------------------
---
       ENERGY  29.94  RUNS 3531 - 3540
---
3529-34        I 30696 (0450)  239 I 3960 (0492).ST239  INETID=JADEPRN9
3488           I  6999 (0685)  240 I  556 (0889).ST240  INETID=JADEPRN0
3535-40        I 44066         241 I 5540 (0479).ST241  INETID=JADEPRN1
3535-40        I 44066         241 I  294 (0479).ST241A INETID=JADEPRN1
TDC TESTS      I           GAP     I                    I
TEST,NO REDUC  I            GAP    I                    I
---
       ENERGY  36.60  RUNS 3561 - 3589
---
3561-67,3539   I 32703 (0730)  244 I 3210 (1780).ST244  INETID=JADEPRN4
3568-72        I 26976 (0737)  245 I 2088 (1785).ST245  INETID=JADEPRN5
------------------------------------------------------------------------

      THE ABOVE  6 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0088V00   15648     EVENTS
ORDER: 239-241,41A,44,45
------------------------------------------------------------------------
3573-77        I 23094 (1306)  246 I 2015 (1578).ST246  INETID=JADEPRN6
3578-85        I 30686 (1267)  247 I 2911 (1581).ST247  INETID=JADEPRN7
3586-91        I 28394 (0432)  248 I 3029 (1585).ST248  INETID=JADEPRN8
---
       ENERGY  29.90  RUNS 3590 - 3606
---
3592-95        I 31972 (0458)  249 I 3514 (1714).ST249  INETID=JADEPRN9
3596-99        I 24982 (0460)  250 I 2570 (1729).ST250  INETID=JADEPRN0
3600-04        I 28759         251 I 2610 (1250).ST251  INETID=JADEPRN1
------------------------------------------------------------------------

      THE ABOVE  6 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0089V00     16649   EVENTS
ORDER: 246-251             TEST READ OK !
------------------------------------------------------------------------
3605-10        I 31797 (1892)  252 I 3872 (1258).ST252  INETID=JADEPRN2
---
       ENERGY  36.60  RUNS 3608 - 3614
---
3611-14        I 23357 (0131)  253 I 2463 (1274).ST253  INETID=JADEPRN3
---
       ENERGY  35.44  RUNS 3615 - 3629
---
3615-18        I 28530 (0133)  254 I 3110 (0741).ST254  INETID=JADEPRN4
3619-22        I 31657 (0134)  255 I 3615 (0741).ST255  INETID=JADEPRN5
    ******** TAPE 255 HAS BEEN REMADE
3623-26        I 22698 (0146)  001 I 2514 (0147).ST001  INETID=JADEPRN1
------------------------------------------------------------------------

      THE ABOVE  5 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0090V00     15574   EVENTS
ORDER: 252-255,001          TEST READ  OK !
------------------------------------------------------------------------
3627-32        I 25600         002 I 2888 (0160).ST002  INETID=JADEPRN2
---
       ENERGY  35.46  RUNS 3630 - 3642
---
3633-38        I 36413         003 I 4518 (1089).ST003  INETID=JADEPRN3
3639-42        I 25836         004 I 3273 (1101).ST004  INETID=JADEPRN4
3643-46        I 31559         005 I 3971 (1102).ST005  INETID=JADEPRN5
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0091V00    14650    EVENTS
ORDER: 002-005     ERROR S137 BY REWINDING, DELETED, TO BE REMADE
                      NOW REMADE
------------------------------------------------------------------------
3647-50        I 27596         006 I 2949 (1106).ST006  INETID=JADEPRN6
3651-58        I 35444         007 I 3963 (1111).ST007  INETID=JADEPRN7
---
       ENERGY  35.48  RUNS 3643 - 3654
---
3659-68        I 24598         008 I 2531 (1287).ST008  INETID=JADEPRN8
---
       ENERGY  35.50  RUNS 3656 - 3676
---
3669-74        I 39409         009 I 3830 (1290).ST009  INETID=JADEPRN9
3675-79        I 29803         010 I 2828 (0579).ST010  INETID=JADEPRN0
---
       ENERGY  35.52  RUNS 3677 - 3698
---
------------------------------------------------------------------------

      THE ABOVE  5 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0092V00    16101    EVENTS
ORDER: 006-010             TEST READ  OK !
------------------------------------------------------------------------
3680-83        I 25219         011 I 2471 (0847).ST011  INETID=JADEPRN1
3684-91        I 36751         012 I 3185 (1175).ST012  INETID=JADEPRN2
3693-99        I 30647         013 I 3318 (0440).ST013  INETID=JADEPRN3
3692           I  1551 (1499)  014 I  150 (1127).ST014  INETID=JADEPRN4
  TAPE ST014 OK. NO EVENTS FROM THE OLD RUN. REDUC SKIPPED FIRST 12 GOOD
  EVENTS
---
       ENERGY  35.54  RUNS 3699 - 3714
---
3700-04        I 30473 (1505)  015 I 3426 (1129).ST015  INETID=JADEPRN5
3705-13        I 28580 (1507)  016 I 3462 (1133).ST016  INETID=JADEPRN6
------------------------------------------------------------------------

      THE ABOVE  6 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0093V00   16012     EVENTS
ORDER: 011-016            TEST READ  OK !
------------------------------------------------------------------------
3714-17        I 27985 (0267)  017 I 3496 (0273).ST017  INO NET
---
       ENERGY  35.56  RUNS 3715 -
---
3718-22        I 32979         018 I 4146 (0270).ST018  INO NET
3723-27        I 26047 (1336)  019 I 3257 (0285).ST019  INETID=JADEPRN9
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0094V00     10899   EVENTS
ORDER: 017-019
------------------------------------------------------------------------
****
****     N.B. TAPES .ST020 THROUGH .ST033 WERE WRITTEN WITH OLD T0S.
****          THE DATA WILL BE RECALIBRATED AND PATTERN RECOGNITION
****          WILL BE RERUN WITHOUT REPEATING EVENT SELECTION.
****
3728-3739      I 29822 (1895)  020 I 6446 (1907).ST020  NO NET
                 RECALIBRATED      I 6446 ( 591).ST020R
3740-3745      I 36980 (0764)  021 I 5927 (0761).ST021  TIME LIMIT
                 RECALIBRATED      I 5927 (1429).ST021R
               I  3410             I  512 (1822).ST021A
                 RECALIBRATED      I  512 (1438).ST021AR
3746-3752      I 28181 (0262)  022 I 3871 (1732).ST022  INETID=JADEPRN2
                 RECALIBRATED      I 2854 (1816).ST022R
               + RECALIBRATED      I 1017 (1909).ST022S
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0095V00  AND RECALIBRATED
                        16756 EVENTS
------------------------------------------------------------------------
3753-3769      I 35982 (1734)  023 I 5758 (0303).ST023  INETID=JADEPRN3
                 RECALIBRATED      I 4817 (1819).ST023R
               + RECALIBRATED      I  941 (1913).ST023S
3770-3774      I 26476 (1737)  024 I 3886 (0332).ST024  INETID=JADEPRN4
                 RECALIBRATED      I 3274 (1820).ST024R
               + RECALIBRATED      I  612 (1915).ST024S
3775-3783      I 25652 (1746)  025 I 4218 (1739).ST025  INETID=JADEPRN5
                 RECALIBRATED      I 4090 (1826).ST025R
               + RECALIBRATED      I  128 (1919).ST025S
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0096V00   AND
                     13862 EVENTS              RECALIBRATED
------------------------------------------------------------------------
3784-3791      I 29276 (1748)  026 I 5163 (1741).ST026  INETID=JADEPRN6
                 RECALIBRATED      I 5163 (1834).ST026R
3792-96(EV 100)I       (1755)  027 I 4064 (1742).ST027  INETID=JADEPRN7
                 RECALIBRATED      I 4064 (1836).ST027R
3796(EV 101 ON)I               027 I  906 (0814).ST027A NO NET
                 RECALIBRATED      I  906 (1841).ST027AR
3797-3802      I 32666 (0074)  028 I 5434 (1752).ST028  INETID=JADEPRN8
                 RECALIBRATED      I 2336 (1039).ST028S (28 R WAS BAD.)
                 RECALIBRATED      I 3098 (0604).ST028T
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0097V00
                    15567 EVENTS
------------------------------------------------------------------------
3761-3762      I  7671 (1489)  029 I 1146 (1754).ST029  INETID=JADEPRN9
                 RECALIBRATED      I 1146 (1044).ST029R
3768           I  7989 (1493)  030 I 1234 (1756).ST030  INETID=JADEPRN0
                 RECALIBRATED      I 1234 (1046).ST030R
3803-3808      I 28099 (1499)  031 I 4477 (1551).ST031  INETID=JADEPRN1
                 RECALIBRATED      I 4477 (0184).ST031R
3809-3821      I 29410 (0173)  032 I 4859 (0166).ST032  INETID=JADEPRN2
*** TAPE 32 REWRITTEN BECAUSE OF TAPE ERROR. WILL HAVE NEW T0S.
3822-3826      I 27197 (1506)  033 I 4390 (1572).ST033  INETID=JADEPRN3
                 RECALIBRATED      I 4390 (0188).ST033R
****
****     N.B. TAPES .ST020 THROUGH .ST033 WERE WRITTEN WITH OLD T0S.
****          THE DATA WILL BE RECALIBRATED AND PATTERN RECOGNITION
****          WILL BE RERUN WITHOUT REPEATING EVENT SELECTION.
****
------------------------------------------------------------------------

      THE ABOVE  5 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0099V00
                     16106 EVENTS
------------------------------------------------------------------------
3827-3835      I 27244 (0610)  034 I 4530 (0641) ST034  INETID=JADEPRN4
3836-3846      I 33822 (0209)  035 I 5991 (0207) ST035  INETID=JADEPRN5
------------------------------------------------------------------------

      THE ABOVE  2 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0098V00   10521 EVENTS

------------------------------------------------------------------------
****
****     WARNING. THE TAPES FROM 36 TO 42 CONTAIN OLD RUNS MIXED IN.
****
3847-3853      I 31207 (1892)  036 I 5256 (1901) ST036  INO NET
3854-3857      I 26907 (1898)  037 I 4586 (1928) ST037  INETID=JADEPRN7
3858-3861,#4484I 32868 (0608)  038 I 2769 (0574) ST038  INETID=JADEPRN8
3861,#4485-3864,#6047 ST038 INCOMPLI 2372 (1825) ST038A INO NET
3864,#6045 ON  I     ST038A INCOMPLI  362 (1975) ST038B INO NET
------------------------------------------------------------------------

      THE ABOVE  5 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0100V00
          AND OLD RUNS REMOVED     15344 EVENTS
------------------------------------------------------------------------
3865-3870      I 22755 (0610)  039 I 3875 (0576) ST039  INETID=JADEPRN9
3871-3875      I 26959 (0618)  040 I 3937 (0586) ST040  INETID=JADEPRN0
3876-3880      I 28276 (0300)  041 I 3875 (1042) ST041  INETID=JADEPRN1
3881-3885      I 31031 (1234)  042 I 4203 (0179) ST042  INO NET
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0101V00
          AND OLD RUNS REMOVED      15886 EVENTS
------------------------------------------------------------------------
3886-3888      I 23984 (0611)  043 I 3525 (0307) ST043  INETID=JADEPRN3
3889-3893      I 31835 (0374)  044 I 5274 (0311) ST044  INETID=JADEPRN4
3894-3897      I 25989 (0376)  045 I 3304 (0312) ST045  INETID=JADEPRN5
------------------------------------------------------------------------
  ******  MERGED TAPE SCRATCHED AND WILL BE REMADE *********
      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0102V00
                        12103 EVENTS
------------------------------------------------------------------------
3898-3901      I 25148 (1514)  046 I 3368 (1988) ST046  INO NET
3902-3906      I 29051 (0381)  047 I 3533 (0333) ST047  INETID=JADEPRN7
3907-3910      I 20411 (1242)  048 I 2643 (1503) ST048  INO NET
3911-3914      I 25282 (0386)  049 I 3266 (0032) ST049  INO NET
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0103V00
                        12810 EVENTS
------------------------------------------------------------------------
3915-3918      I 25996 (1196)  050 I 3424 (0818) ST050  INETID=JADEPRN0
3919-3925      I 26154 (1199)  051 I 3709 (0821) ST051  INETID=JADEPRN1
3926-3929      I 29562 (1204)  052 I 3753 (0033) ST052  INO NET
3932-3936      I 26985 (1208)  053 I 2992 (0970) ST053  INETID=JADEPRN3
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0104V00
                        13878 EVENTS
------------------------------------------------------------------------
C---
C---     THE FOLLOWING THREE TAPES, FOR WHICH THE OUTPUTS WERE LOST,
C---     WILL BE SCRATCHED AND RECATALOGED BY SPECIAL LOW PRIORITY
C---     JOBS F11LHOSP, NUMBERS 821, 753 AND 760.
C---
3937-3939      I 26571 (0701)  054 I 3764 (0821) ST054  INO NET
3940,42-45     I 31342 (0084)  055 I 4806 (1796) ST055  INO NET
3946-3948      I 21152 (0720)  056 I 3545 (0760) ST056  INO NET
C---
C---     THE ABOVE THREE TAPES, FOR WHICH THE OUTPUTS WERE LOST,
C---     WILL BE SCRATCHED AND RECATALOGED BY SPECIAL LOW PRIORITY
C---     JOBS F11LHOSP, NUMBERS 821, 753 AND 760.
C---
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0105V00
                   12115 EVENTS
------------------------------------------------------------------------
3949-3958      I 32373 (0227)  057 I 5349 (0240) ST057  INETID=JADEPRN7
3959-3970      I 29907 (0229)  058 I 4397 (0241) ST058  INETID=JADEPRN8
------------------------------------------------------------------------

      THE ABOVE  2 REDUCED TAPES HAS BEEN  COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0106V00
                      9746 EVENTS
------------------------------------------------------------------------
3971-3982      I 23470 (0235)  059 I 6005 (0243) ST059  INETID=JADEPRN9
3983-3989      I 28061 (1619)  060 I 6208 (1614) ST060  INO NET
------------------------------------------------------------------------

      THE ABOVE  2 REDUCED TAPES HAS BEEN  COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0107V00
                        12213 EVENTS
------------------------------------------------------------------------
                       *******      NOTE NEW NET NAMES ********
3990-3994      I 29964 (1646)  061 I 5565 (1654) ST061  INETID=JADEPRX1
3995-4003      I 29634 (1649)  062 I 5384 (1656) ST062  INETID=JADEPRX2
3924           I  7998 (0624)  063 I 1101 (0613) ST063  INETID=JADEPRX3
3933           I  5001 (0677)  064 I  504 (1183) ST064  INO NET
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES HAS BEEN  COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0108V00
                     12554 EVENTS
------------------------------------------------------------------------
4004-4010      I 34525 (1157)  065 I 6825 (1148) ST065  INETID=JADEPRX5
4011-4017      I 37095 (1159)  066 I 6609 (1150) ST066  INETID=JADEPRX6
------------------------------------------------------------------------

      THE ABOVE  2 REDUCED TAPES HAS BEEN  COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0109V00
                     13434 EVENTS
------------------------------------------------------------------------
4018-4023      I 31809 (1613)  067 I 5761 (0050) ST067  INO NET
4024-4029      I 36042 (0684)  068 I10830 (1237) ST068  INO NET
------------------------------------------------------------------------

      THE ABOVE  2 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0110V00
                     16591 EVENTS
------------------------------------------------------------------------
4038           I  7997 (1875)  069 I 1436 (0010) ST069  INO NET
4030-4033      I 28868 (0679)  070 I 5174 (0014) ST070  INO NET
4034-4045      I 33509 (0684)  071 I 5229 (0023) ST071  INO NET
4046-4051      I 27536 (1283)  072 I 3216 (0105) ST072  INO NET
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0111V00
                     15055 EVENTS
------------------------------------------------------------------------
4052-4056      I 26654 (1846)  073 I 4507 (0968) ST073  INO NET
4057-4063      I 26535 (1851)  074 I 4560 (0953) ST074  INO NET
4064-4069      I 31560 (1853)  075 I 5490 (0957) ST075  INO NET
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0112V00
                         14557 EVENTS
------------------------------------------------------------------------
4070-4073      I 28495 (1916)  076 I 4625 (0492) ST076  INO NET
4071-4077      I 26515 (1920)  077 I 4332 (0959) ST077  INO NET
4078-4081      I 30509 (1923)  078 I 4872 (0960) ST078  INO NET
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0113V00
                         13829 EVENTS
------------------------------------------------------------------------
4082-4085      I 29153 (1924)  079 I 4813 (0961) ST079  INO NET
4086-4089      I 29739 (1942)  080 I 5117 (0502) ST080  INO NET
4090-4093      I 25343 (1946)  081 I 4182 (0807) ST081  INO NET
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0114V00
                       14112 EVENTS
------------------------------------------------------------------------
4094-4098      I 30997 (1947)  082 I 4904 (0641) ST082  INO NET
4099-4102      I 26542 (1948)  083 I 3986 (0642) ST083  INO NET
4103-4108      I 30234 (1949)  084 I 4832 (0644) ST084  INO NET
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0115V00
                   13722 EVENTS
------------------------------------------------------------------------
4109-4113      I 26886 (0947)  085 I 4255 (1843) ST085  INO NET
4114-18,23     I 29382 (0602)  086 I 4713 (0061) ST086  INO NET
4124-4127      I 27457 (0604)  087 I 4117 (0578) ST087  INO NET
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0116V00
                    13085 EVENTS
------------------------------------------------------------------------
4128-4133      I 32649 (0605)  088 I 4819 (0584) ST088  INO NET
4134-4139      I 25318 (0413)  089 I 3625 (1610) ST089  INO NET
4140-4146      I 28257 (0436)  090 I 4658 (0956) ST090  INO NET
4104           I  4192 (0739)  091 I  715 (1210)        INO NET
4165           I  7998 (0743)  092 I 1115 (1213)        INO NET
4168           I   583 (0746)  093 I   85 (0593)        INO NET
4167           I  7997 (0749)  094 I 1307 (1218)        INO NET
------------------------------------------------------------------------

      THE ABOVE  7 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0117V00
                        16324 EVENTS
------------------------------------------------------------------------
4147-4155      I 26339 (0756)  095 I 4207 (1220)        INO NET
4156-4173      I 35230 (0758)  096AI 4966 (0983)        INO NET
 %%%%%%%  TAPE ST096 CANNOT BE READ , REDUC JOB RESUBMITTED JOB 983
          **** DOES NOT EXIST  097 I  **********        INO NET
4187-4189      I 14135 (1099)  098 I 2161 (0209)        INO NET
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0118V00
                     11334 EVENTS
------------------------------------------------------------------------
4174-77,83,84  I 31325 (0046)  099 I 3622 (1046)        INO NET
4185-4193      I 19885 (1105)  100 I 3162 (0213)        INO NET
4203-07        I 25908 (0049)  101 I 4145 (1049)        INO NET
4194-96        I  3974 (1015)  102 I  476 (0545)        INO NET
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES HAS BEEN  COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0119V00
                     11405 EVENTS
------------------------------------------------------------------------
4198           I  7992 (0142)  103 I 1313 (1061)        INO NET
4208-13        I 27797 (0144)  104 I 4121 (1050)        INO NET
4214-18        I 29654 (0145)  105 I 4491 (1056)        INO NET
4219-21        I 18179 (0994)  106 I 2057 (0291) INCOMPLINO NET
4219-21        I 18179 (0994)  106 I  206 (0583) ST106A  NO NET
------------------------------------------------------------------------

      THE ABOVE  5 REDUCED TAPES HAS BEEN  COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0120V00
                       12188 EVENTS
------------------------------------------------------------------------
4222-27        I 32701 (0997)  107 I 4749 (0153)        INO NET
4228-32        I 28143 (0999)  108 I 4465 (0299)        INO NET
4233-35        I 22193 (1000)  109 I 3464 (0595)        INO NET
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0121V00
                    12678 EVENTS
------------------------------------------------------------------------
4236-42        I 34512 (1002)  110 I 4656 (0605)        INO NET
4243-47        I 21462 (1242)  111 I 3697 (0609)        INO NET
4248-51        I 30336 (0286)  112 I 4364 (0619)        INO NET
4252-57        I 25710 (0493)  113 I 2544 (1813)        INO NET
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES HAS BEEN  COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0122V00
                         15261 EVENTS
------------------------------------------------------------------------
4258-61        I 30544 (0497)  114 I 4083 (0814)        INO NET
4262-65        I 25327 (0635)  115 I 4052 (0425)        INO NET
4266-69        I 27969 (0725)  116 I 4365 (0432)        INO NET
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0123V00
                    11349 EVENTS      EXCEPT RUNS 4269-79 SEE NOTE BELOW
------------------------------------------------------------------------
4270-73        I 25610 (0315)  117 I 3931 (1651)        INO NET
4274-81        I 31784 (0316)  118 I 5228 (1652)        INO NET
4282-85        I 28611 (1639)  119 I 4191 (1685)        INO NET
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0124V00
                     5488 EVENTS    EXCEPT RUNS 4269-79 SEE NOTE BELOW
------------------------------------------------------------------------
**********************************************************************
        RUNS 4269 - 4279  HAVE SCRAMBLED LG ADCS AND ARE CONTAINED
                     ON THE MERGED TAPE
                JADEPR.ADC.MESS1
                      9013 EVENTS
***********************************************************************
------------------------------------------------------------------------
4286-92        I 31638 (1936)  120 I 5008 (1933)        INETID=JADEPRN0
4293-97        I 26504 (0945)  121 I 4260 (0981)        INETID=JADEPRN1
4298-01        I 30587 (0948)  122 I 4559 (0983)        INETID=JADEPRN2
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0125V00
                    13827 EVENTS
------------------------------------------------------------------------
4302-04        I 23971 (1217)  123 I 3351 (1224)        INETID=JADEPRN3
C---
C---             NEW VERSION OF TRGCHK FROM RUN 4305 ON. SEE #START.
C---
4305-11        I 36590 (0972)  124 I 5195 (1557)        INETID=JADEPRN4
4313           I  7998 (0296)  125 I 1337 (1650)        INETID=JADEPRN5
4312,14-19     I 28514 (0336)  126 I 4050 (0285)        INETID=JADEPRN6
4320-23        I 28042 (1553)  127 I 4070 (1560)        INETID=JADEPRN7
------------------------------------------------------------------------

      THE ABOVE  5 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0126V00
                  JOB 1462
------------------------------------------------------------------------
4324-31        I 34457 (0449)  128 I 5454 (1672)        INETID=JADEPRN8
4332-36        I 26704 (0251)  129 I 3999 (0456)        INETID=JADEPRN9
4337-40        I 27640 (0453)  130 I 4075 (0457)        INETID=JADEPRN0
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0127V00
                    13524 EVENTS
------------------------------------------------------------------------
4341-45        I 28162 (0454)  131 I 4512 (0460)        INETID=JADEPRN1
4346-49        I 31404 (0242)  132 I 4886 (0498)        INETID=JADEPRN2
4350-55        I 29712 (0245)  133 I 4531 (1885)        INO NET
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0128V00
                   13929 EVENTS
------------------------------------------------------------------------
    -4358 6804 I 22360 (0248)  134 I 3523 (0267)        INETID=JADEPRN4
  REFORM TAPE 134 WAS INCOMPLETE DUE TO READ ERROR. SCRATCH, REWRITE.
4358 6805-     I 23561 (1949)  134 I  198 (1893).ST134A INETID=JADEPRN4
4359-62        I 27434 (1953)  135 I 4893 (0971)        INO NET
4363-69        I 34480 (1955)  136 I 5239 (1917)        INETID=JADEPRN6
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0129V00
                    13853 EVENTS
------------------------------------------------------------------------
4370-73        I 24895 (1976)  137 I 3798 (1971)        INETID=JADEPRN7
4374-78        I 29851 (0757)  138 I 4672 (1133)        INO NET
4379-83        I 24705 (0781)  139 I 3363 (0786)        INETID=JADEPRN9
4384-87        I 27202 (0838)  140 I 4051 (0831)        INETID=JADEPRN0
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0130V00
                   15884 EVENTS
------------------------------------------------------------------------
4388-92        I 32114 (0238)  141 I 5191 (0259)        INETID=JADEPRN1
4393-98        I 25041 (0242)  142 I 4069 (0269)        INETID=JADEPRN2
4399-03        I 25235 (0246)  143 I 3786 (0274)        INETID=JADEPRN3
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0131V00
                       13046 EVENTS
------------------------------------------------------------------------
4404-07        I 28984 (1579)  144 I 3935 (1634)        INETID=JADEPRN4
4408-19        I 25727 (1951)  145 I 3983 (1952)        INETID=JADEPRN5
4422-27        I 28163 (0804)  146 I 3061 (1206)        INO NET
4428-34        I 24954 (0807)  147 I 4695 (0822)        INETID=JADEPRN7
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0132V00
                    10673 EVENTS   EXCEPT RUNS 4427-47 SEE NOTE BELOW
------------------------------------------------------------------------
4435-38        I 27221 (0524)  148 I 5048 (0528)        INETID=JADEPRN8
4439-44        I 27340 (1829)  149 I 4478 (1828)        INETID=JADEPRN9
4445-50,4286   I 21047 (1095)  150 I 3296 (1085)        INETID=JADEPRN0
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0133V00
                       2338 EVENTS EXCEPT RUNS 4427-47 SEE NOTE BELOW
                                         RUN 4286 DELETED
------------------------------------------------------------------------
**********************************************************************
        RUNS 4427 - 4447  HAVE SCRAMBLED LG ADCS AND ARE CONTAINED
                     ON THE MERGED TAPE
                JADEPR.ADC.MESS2
                     15311 EVENTS
***********************************************************************
4451-54        I 31447 (1100)  151 I 4664 (0416)        INETID=JADEPRN1
4455-59        I 30267 (1101)  152 I 4701 (1101)        INETID=JADEPRN2
4460-64        I 20663 (1101)  153 I 3216 (0417)        INETID=JADEPRN3
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0134V00
                   12581 EVENTS
------------------------------------------------------------------------
4465-70        I 28819 (0318)  154 I 4195 (0429)        INETID=JADEPRN4
4471-74        I 30243 (0370)  155 I 3803 (0431)        INETID=JADEPRN5
4475-77        I 23965 (1013)  156 I 1963 (1010)        INETID=JADEPRN6
4478-81        I 24670 (0183)  157 I 3069 (1012)        INETID=JADEPRN7
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0135V00
                    13030 EVENTS
------------------------------------------------------------------------
4482-4500      I 30433 (1663)  158 I 4221 (0185)        INETID=JADEPRN8
4199-4202      I 30490 (0904)  159 I 4556 (1256)        INO NET
        ATTENTION MUON LOVERS -----  STARTING WITH THE RUN BELOW
                    THE FORWARD MUON COUNTER TRIGGERS ARE
                    INCLUDED IN THE DATA REDUCTION
4501-4504      I 29383 (0357)  160 I      (1181)        INETID=JADEPRN0
4505-4515      I 24816 (1397)  161 I      (1184)        INETID=JADEPRN1
               I       (0468)  162 I      (0447)        INETID=JADEPRN2
               I       (0472)  163 I      (0453)        INETID=JADEPRN3
               I       (0480)  164 I      (0459)        INETID=JADEPRN4
    30/03/82            MEMBER NAME  REDUC79  (JADESR)      TEXT

        *****************************************************
        *                                                   *
        *            FIRST STEP DATA REDUCTION              *
        *                                                   *
        *****************************************************

          THE CURRENT STATUS OF THE REDUCTION IS STORED IN
                JADEPR.JADESR(REDUC)


         REFORMATTED TAPES HAVE THE NAMES F11LHO.JDATA01.REFORM.GXXXXV00
               WHERE XXXX IS THE DATA GENERATION NO. GIVEN BELOW

         REDUCED TAPES HAVE THE NAMES F11OLS.JDATA01.REDUC1.GXXXXV00
               WHERE XXXX IS THE DATA GENERATION NO. GIVEN BELOW

       *** N.B. ***** THESE F11OLS TAPES ARE TEMPORARY AND WILL
                      SOON BE REUSED,YOU MUST THEN USE THE
                      JADEPR.REDUC1.GXXXXV00 TAPES NOTED BELOW

------------------------------------------------------------------------
               I   REFORMATTED     I      REDUCED      I   REDUCTION
  RUN     ECM  I EVENTS ; DATA GEN#I EVENTS ; DATA GEN#I    FACTOR
---------------I-------------------I-------------------I----------------
 539-40  27.2  I  3110     0001    I   501     0001    I     16.2 %
  541    27.2  I  1709     0002    I   284     0002    I     16.7 %
  543    27.2  I  8369     0003    I   752     0003    I      9.0 %
  544    27.2  I  8341     0004    I   642     0008    I      7.7 %
  545    27.2  I  6837     0005    I   363     0008    I      5.3 %
  547    27.2  I  6523     0006    I   416     0008    I      6.4 %
  549    27.2  I  6212     0007    I   239     0009    I      3.9 %
  552    27.2  I  8634     0008    I   709     0009    I      8.2 %
  553    27.2  I  8230     0009    I  1059     0009    I     12.9 %
 559-61  27.2  I  7228     0010    I   430     0010    I       6 %
  563    27.2  I  7985     0011    I    ?      0010    I       ?
  564    27.2  I  2700     0012    I    ?      0010    I       ?
------------------------------------------------------------------------

      THE ABOVE 6 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0001V00      6195 EVENTS

        THE FIRST 250 EVENTS OF THIS TAPE ARE ON THE FILE
                  JADEPR.REDUC001
 BACKUP COPY OF REDUC1.G0001V00 :  JADEPR.ARCHIVE.REDUC001  TEST READ OK

------------------------------------------------------------------------
  581    27.72 I  2821     0013    I   138     0011    I      4.9 %
  582    27.72 I  7999     0014    I   381     0011    I      4.8 %
  584    27.72 I  3710     0015    I   205     0011    I      5.5 %
 586-87  27.72 I  7677     0016    I   381     0011    I      5 %
  588    27.72 I  1291     0017    I    64     0012    I      5.0 %
  589    27.72 I  7999     0018    I   427     0012    I      5.4 %
  590    27.72 I  4030     0019    I   220     0012    I      5.5 %
  591    27.72 I  7999     0020    I   211     0012    I      2.6 %
  592    27.72 I  3223     0021    I   194     0012    I      6.0 %
  593    27.72 I  2449     0022    I   133     0013    I      5.5 %
  594    27.72 I  8464     0023    I   300     0013    I      3.6 %
  595    27.72 I  6219     0024    I   255     0013    I      4.1 %
  596    27.72 I  6755     0025    I   424     0014    I      6.3 %
  597    27.72 I  7412     0026    I   289     0014    I      3.9 %
  598    27.72 I  9011     0027    I   368     0037    I      4.1 %
  599    27.72 I  7999     0028    I   378     0015    I      4.7 %
  600    27.72 I  7999     0029    I   361     0015    I      4.5 %
  601    27.72 I  9999     0030    I   547     0015    I      5.5 %
 602-3   27.72 I  8872     0031    I   427     0016    I      5 %
  604    27.72 I  6021     0032    I   308     0016    I      5.1 %
  605    27.72 I  3624     0033    I   166     0016    I      4.6 %
------------------------------------------------------------------------

      THE ABOVE 7 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0002V00      6166 EVENTS

        THE FIRST 250 EVENTS OF THIS TAPE ARE ON THE FILE
                  JADEPR.REDUC002
 BACKUP COPY OF REDUC1.G0002V00 :  JADEPR.ARCHIVE.REDUC002


------------------------------------------------------------------------
  606    27.72 I  5429     0034    I  273      0017    I      5.0 %
 607-8   27.72 I  9017     0035    I  462      0017    I      5 %
  609    27.72 I  7999     0036    I  441      0017    I      5.5 %
  610    27.72 I  7346     0037    I  463      0018    I      6.3 %
 611-2   27.72 I  3889     0038    I  250      0018    I      6.5 %
  613    27.72 I  7999     0039    I  430      0018    I      5.4 %
  614    27.72 I  7998     0040    I  438      0019    I      5.5 %
  616    27.72 I  7999     0041    I  417      0019    I      5.2 %
  617    27.72 I  4149     0042    I  261      0019    I      6.3 %
  618    27.72 I  7999     0043    I  519      0020    I      6.5 %
  619    27.72 I  2161     0044    I  148      0020    I      6.9 %
  620    27.72 I  7999     0045    I  385      0020    I      4.8 %
  621    27.72 I  8499     0046    I  413      0021    I      4.9 %
  622    27.72 I  4052     0047    I  223      0021    I      5.5 %
  623    27.72 I  3464     0048    I  979      0021    I      28.4 %
------------------------------------------------------------------------

      THE ABOVE 5 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0003V00     6102 EVENTS

      THE FIRST 150 EVENTS OF THIS REDUCED TAPE ARE ON THE FILE
                  JADEPR.REDUC003
 BACKUP COPY OF REDUC1.G0003V00 :  JADEPR.ARCHIVE.REDUC003


------------------------------------------------------------------------
  624    27.72 I  8999     0049    I   575     0022    I      6.4 %
  630    27.72 I  3920     0050    I   170     0022    I      4.4 %
  631    27.72 I  8999     0051    I   501     0022    I      5.6 %
  632    27.72 I  6265     0052    I   419     0023    I      6.7 %
  633    27.72 I  3934     0053    I   174     0023    I      4.4 %
  634    27.72 I  3518     0054    I   184     0023    I      5.3 %
  635    27.72 I  7999     0055    I   376     0024    I      4.7 %
  636    27.72 I  7999     0056    I   430     0024    I      5.4 %
  637    27.72 I  7999     0057    I   464     0025    I      5.8 %
  638    27.72 I  8999     0058    I   559     0025    I      6.2 %
 639-40  27.72 I 10143     0059    I   620     0026    I      6 %
 641-2   27.72 I  8714     0060    I   559     0026    I      6.5 %
 643-4   27.72 I  7252     0061    I   325     0027    I      4.5 %
  645    27.72 I  7378     0062    I   466     0027    I      6.3 %
  646    27.72 I  7398     0063    I   518     0027    I      7.0 %
------------------------------------------------------------------------

      THE ABOVE 6 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0004V00     6340 EVENTS


      THE FIRST 250 EVENTS OF THIS REDUCED TAPE ARE ON THE FILE
                  JADEPR.REDUC004
 BACKUP COPY OF REDUC1.G0004V00 :  JADEPR.ARCHIVE.REDUC004

------------------------------------------------------------------------
  647    27.72 I  9999     0064    I   482     0028    I      4.8 %
 648-9   27.72 I  8370     0065    I   457     0028    I      5.5 %
  650    27.72 I  8999     0066    I   467     0029    I      5.2 %
  651    27.72 I  8999     0067    I   537     0029    I      6.0 %
  654    27.72 I  8999     0068    I   664     0030    I      7.4 %
  655    27.72 I  7999     0069    I   402     0030    I      5.0 %
  656    27.72 I  9999     0070    I   507     0031    I      5.1 %
  657    27.72 I  5012     0071    I   288     0031    I      5.8 %
  658    27.72 I  7999     0072    I   363     0031    I      4.5 %
  659    27.72 I  9999     0073    I   532     0032    I      5.3 %
  660    27.72 I  9999     0074    I   562     0032    I      5.6 %
  661    27.72 I  9999     0075    I   558     0033    I      5.6 %
  662    27.72 I  8400     0076    I   382     0033    I      4.6 %
------------------------------------------------------------------------

      THE ABOVE 6 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0005V00  6154 EVENTS

        THE FIRST 250 EVENTS OF THIS TAPE ARE ON THE FILE
                  JADEPR.REDUC005
 BACKUP COPY OF REDUC1.G0005V00 :  JADEPR.ARCHIVE.REDUC005

------------------------------------------------------------------------
  663    27.72 I  8774     0077    I   367     0034    I      4.2 %
  664    27.72 I  8404     0078    I   395     0034    I      4.7 %
  665    27.72 I  8778     0079    I   437     0035    I      5.0 %
  666    27.72 I  8315     0080    I   633     0035    I      7.6 %
  667    27.72 I  1848     0081    I   119     0035    I      6.5 %
  668    27.72 I  4847     0082    I  3454     0036    I      71.4 %
  669    27.72 I  5705     0083    I   351     0036    I      6.2 %
  670    27.72 I  5122     0084    I   283     0036    I      5.5 %
 671-2   27.72 I  4822     0085    I   235     0036    I       5 %
------------------------------------------------------------------------

      THE ABOVE 3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0006V00     6274 EVENTS

        THE FIRST 250 EVENTS OF THIS TAPE ARE ON THE FILE
                  JADEPR.REDUC006
 BACKUP COPY OF REDUC1.G0006V00 :  JADEPR.ARCHIVE.REDUC006

------------------------------------------------------------------------
  699    31.6  I  2466     0086    I  677      0059    I
  701    31.6  I  1532     0087    I    AS ABOVE       I
  704    31.6  I  6952     0088    I    AS ABOVE       I
  705    31.6  I  6998     0089    I    AS ABOVE       I
  706    31.6  I  6997     0090    I  571      0060    I       5 %
 707-9   31.6  I  4185     0091    I    AS ABOVE       I   AS ABOVE
  764    31.6  I  1563     0092    I    AS ABOVE       I   AS ABOVE
 765-71  31.6  I 14986     0093    I  464     0042     I
  773    31.6  I  8176     0094    I  944     0061     I      10.5 %
 775-6   31.6  I  4519     0095    I     AS ABOVE      I
  778    31.6  I 22363     0096    I 1131     0038     I
  778    31.6  I 22363     0096    I 1082     0039     I
 784-95  31.6  I 51819     0097    I 1344  JADEPR.T97A I
 784-95  31.6  I 51819     0097    I 1408  JADEPR.T97B I
 784-95  31.6  I 51819     0097    I 1578  JADEPR.T97C I
------------------------------------------------------------------------

      THE ABOVE 9 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0007V00     9199 EVENTS

        THE FIRST 250 EVENTS OF THIS TAPE ARE ON THE FILE
                  JADEPR.REDUC007
 BACKUP COPY OF REDUC1.G0007V00 :  JADEPR.ARCHIVE.REDUC007

------------------------------------------------------------------------
  794    31.6  I  7403     0098    I  515     0062     I     7.0 %
 795-8   31.6  I 24879     0099    I  964     0063     I     7.5 %
 795-8   31.6  I 24879     0099    I 1303     0064     I    10.1 %
  809    31.6  I 25748     0100    I 1495     0040     I
  809    31.6  I 25748     0100    I 1405      0041    I
 815-    31.6  I 21641     0101    I 1040      0043    I     10 %
------------------------------------------------------------------------

      THE ABOVE 6 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0008V00     6722 EVENTS

        THE FIRST 250 EVENTS OF THIS TAPE ARE ON THE FILE
                  JADEPR.REDUC008
 BACKUP COPY OF REDUC1.G0008V00 :  JADEPR.ARCHIVE.REDUC008

------------------------------------------------------------------------
 815-    31.6  I 21641     0101    I 1232      0044    I     13.2 %
 819-28  31.6  I 32888     0102    I 1850      0045    I     12 %
 819-28  31.6  I 32888     0102    I 1560      0046    I     10 %
 829-35  31.6  I 28451     0103    I 1059      0005    I     8 %
 829-35  31.6  I 28451     0103    I 1728      0006    I     12 %
------------------------------------------------------------------------


      THE ABOVE 5 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0009V00     7429 EVENTS

        THE FIRST 250 EVENTS OF THIS TAPE ARE ON THE FILE
                  JADEPR.REDUC009
 BACKUP COPY OF REDUC1.G0009V00 :  JADEPR.ARCHIVE.REDUC009

------------------------------------------------------------------------
  837    31.6  I 13679     0104    I 1520      0047    I   11 %
  838    31.6  I 24638     0105    I  979      0048    I   8 %
  838    31.6  I 24638     0105    I 1354      0049    I   10.7 %
 839-55  31.6  I 22301     0106    I 1211      0050    I     9 %
 839-55  31.6  I 22301     0106    I 1210      0051    I     11 %
 860-7   31.6  I 33878     0107    I 1350      0052    I     8 %
------------------------------------------------------------------------

      THE ABOVE 6 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0010V00     7624 EVENTS

        THE FIRST 250 EVENTS OF THIS TAPE ARE ON THE FILE
                  JADEPR.REDUC010
 BACKUP COPY OF REDUC1.G0010V00 :  JADEPR.ARCHIVE.REDUC010

------------------------------------------------------------------------
 860-7   31.6  I 33878     0107    I 1241      0053    I   7.5 %
 869-74  31.6  I 26610     0108    I  968      0054    I   8 %
 869-74  31.6  I 26610     0108    I 1219      0055    I   9 %
 875-80  31.6  I 20125     0109    I  1791     0056    I   9 %
 882-87  31.6  I 22310     0110    I 1119      0057    I   10.2 %
 882-87  31.6  I 22310     0110    I  885      0058    I   11 %
------------------------------------------------------------------------

      THE ABOVE 6 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0011V00     7223 EVENTS
 BACKUP COPY OF REDUC1.G0011V00 :  JADEPR.ARCHIVE.REDUC011

------------------------------------------------------------------------
  809    31.6  I 20362     0111    I  870 JADEPR.T111A I
  809    31.6  I 25748     0111    I 1111 JADEPR.T111B I
  914    31.6  I  4755     0112    I  314 JADEPR.T112  I   6.5 %
 915-8   31.6  I 24451     0113    I  908 JADEPR.T113A I   7.5 %
 915-8   31.6  I 24451     0113    I 1279 JADEPR.T113B I   10 %
 919-26  31.6  I 29519     0114    I 1319 JADEPR.T114A I   11 %
 919-26  31.6  I 29519     0114    I 1931 JADEPR.T114B I   11 %
------------------------------------------------------------------------

      THE ABOVE 7 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0012V00     7732 EVENTS
 BACKUP COPY OF REDUC1.G0012V00 :  JADEPR.ARCHIVE.REDUC012

------------------------------------------------------------------------
 930-44  31.6  I 32236     0115    I 1582 JADEPR.T115A I
 930-44  31.6  I 32236     0115    I 1847 JADEPR.T115B I
 946-83  31.6  I 59457     0116    I 1532 JADEPR.T116A I
 946-83  31.6  I 59457     0116    I 1747 JADEPR.T116B I
 946-83  31.6  I 59457     0116    I 1424 JADEPR.T116C I
 946-83  31.6  I 59457     0116    I 1665 JADEPR.T116D I
  992    31.6  I  7999     0117    I 1659 JADEPR.T117  I
  993    31.6  I  7999     0118    I    AS ABOVE       I
  996    31.6  I  1324     0119    I    AS ABOVE       I
------------------------------------------------------------------------

      THE ABOVE 4 REDUCED TAPES HAVE BEEN COMBINED ONTO
      THE SINGLE TAPE JADEPR.REDUC1.G0013V00      11456    EVENTS
 BACKUP COPY OF REDUC1.G0013V00 :  JADEPR.ARCHIVE.REDUC013

------------------------------------------------------------------------
  997    31.6  I  7999     0120    I  556 JADEPR.T120  I
  997    31.6  I  7999     0120    I  144 JADEPR.T120B I
983-1005 31.6  I 29956     0121    I 1316 JADEPR.T121A I
983-1005 31.6  I 29956     0121    I 1407 JADEPR.T121B I
1007-13  31.6  I 18967     0122    I 2364 JADEPR.T122  I
 1014-7  31.6  I 30058     0123    I 1650 JADEPR.T123A I
 1014-7  31.6  I 30058     0123    I 1394 JADEPR.T123B I
------------------------------------------------------------------------

      THE ABOVE 7 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0014V00     8831 EVENTS
 BACKUP COPY OF REDUC1.G0014V00 :  JADEPR.ARCHIVE.REDUC014

------------------------------------------------------------------------
1018-22  31.6  I 27249     0124    I 1657 JADEPR.T124A I
1018-22  31.6  I 27249     0124    I 1142 JADEPR.T124B I
 1023-7  31.6  I 30979     0125    I 1792 JADEPR.T125C I
 1023-7  31.6  I 30979     0125    I 1180 JADEPR.T125D I
1028-39  31.6  I 32410     0126    I 1467 JADEPR.T126A I
1028-39  31.6  I 32410     0126    I 1392 JADEPR.T126B I
------------------------------------------------------------------------

      THE ABOVE 6 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0015V00     8630 EVENTS
 BACKUP COPY OF REDUC1.G0015V00 :  JADEPR.ARCHIVE.REDUC015

------------------------------------------------------------------------
 1041-8  31.6  I 34632     0127    I 1656 JADEPR.T127A I
 1041-8  31.6  I 34632     0127    I 1871 JADEPR.T127B I
1049-51  31.6  I 17073     0128    I 1626 JADEPR.T128A I
1049-51  31.6  I 17073     0128    I    9 JADEPR.T128B I
1052-60  31.6  I 40113     0129    I 1417 JADEPR.T129A I
1052-60  31.6  I 40113     0129    I 1280 JADEPR.T129B I
1052-60  31.6  I 40113     0129    I 1310 JADEPR.T129C I
 1071-8  31.6  I 29699     0130    I 1707 JADEPR.T130A I
 1071-8  31.6  I 29699     0130    I 1959 JADEPR.T130B I
------------------------------------------------------------------------

      THE ABOVE 9 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0016V00    12835 EVENTS
 BACKUP COPY OF REDUC1.G0016V00 :  JADEPR.ARCHIVE.REDUC016

------------------------------------------------------------------------
 1079    31.6  I 32895     0131    I 1810 JADEPR.T131A I
 1079    31.6  I 32895     0131    I 2070 JADEPR.T131B I
 1080-3  31.6  I 22957     0132    I 1682 JADEPR.T132A I
 R 1083 AT 22 GEV22957     0132    I 1387 JADEPR.T132B I
 1084-8  27.72 I 29865     0133    I 2068 JADEPR.T133A I
 1084-8  27.72 I 29865     0133    I 2696 JADEPR.T133B I
------------------------------------------------------------------------

      THE ABOVE 6 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0017V00    11713 EVENTS
 BACKUP COPY OF REDUC1.G0017V00 :  JADEPR.ARCHIVE.REDUC017

------------------------------------------------------------------------
 1089-93 27.72 I 48340     0134    I 2419 JADEPR.T134A I
 1089-93 27.72 I 48340     0134    I 1638 JADEPR.T134B I
 1089-93 27.72 I 48340     0134    I 1316 JADEPR.T134C I
 1095-98 27.72 I 31580     0135    I 1852 JADEPR.T135A I
 1095-98 27.72 I 31580     0135    I 2048 JADEPR.T135B I
  1099   27.72 I  7997     0136    I  979 JADEPR.T136  I
 1101-5  27.72 I 27584     0137    I 1919 JADEPR.T137A I
 1101-5  27.72 I 27584     0137    I 1971 JADEPR.T137B I
  1106   27.72 I  7998     0138    I  920 JADEPR.T138  I
  1107   27.72 I  8008     0139    I 1001 JADEPR.T139  I
------------------------------------------------------------------------

      THE ABOVE 10 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0018V00    16063 EVENTS
 BACKUP COPY OF REDUC1.G0018V00 :  JADEPR.ARCHIVE.REDUC018

------------------------------------------------------------------------
 1113-19 27.72 I 42645     0140    I 1678 JADEPR.T140A I
 1113-19 27.72 I 42645     0140    I 2062 JADEPR.T140B I
 1113-19 27.72 I 42645     0140    I 2002 JADEPR.T140C I
  CM ENERGY WAS 27.72 GEV UP TO AND INCLUDING RUN 1121
 1120-29 22.0  I 41454     0141    I 1957 JADEPR.T141A I
 1120-29 22.0  I 41454     0141    I 2314 JADEPR.T141B I
 1120-29 22.0  I 41454     0141    I   ?  JADEPR.T141C IPRINT UNREADABLE
------------------------------------------------------------------------

      THE ABOVE 6 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0019V00    11803 EVENTS
 BACKUP COPY OF REDUC1.G0019V00 :  JADEPR.ARCHIVE.REDUC019

------------------------------------------------------------------------
 1130-5  22.00 I 35655     0142    I 2014 JADEPR.T142A I
 1130-5  22.00 I 35655     0142    I 2281 JADEPR.T142B I
 1136-9  22.00 I 31586     0143    I 2044 JADEPR.T143A I
 1136-9  22.00 I 31586     0143    I 2312 JADEPR.T143B I
 1140-7  22.00 I 25419     0144    I 2064 JADEPR.T144A I
 1140-7  22.00 I 25419     0144    I 2055 JADEPR.T144B I
------------------------------------------------------------------------

      THE ABOVE 6 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0020V00    12770 EVENTS
 BACKUP COPY OF REDUC1.G0020V00 :  JADEPR.ARCHIVE.REDUC020

------------------------------------------------------------------------
  1149   22.00 I  4795     0145    I  773 JADEPR.T145  I
 1150-5  22.00 I 30656     0146    I 1853 JADEPR.T146A I
 1150-5  22.00 I 30656     0146    I 2189 JADEPR.T146B I
 1156-60 22.00 I 34193     0147    I 1729 JADEPR.T147A I
 1156-60 22.00 I 34193     0147    I 2207 JADEPR.T147B I
 1161-2  22.00 I 14762     0148    I 1919 JADEPR.T148  I
------------------------------------------------------------------------

      THE ABOVE 6 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0021V00    10670 EVENTS
 BACKUP COPY OF REDUC1.G0021V00 :  JADEPR.ARCHIVE.REDUC021

------------------------------------------------------------------------
  RUNS 1180 - 1209  VARIOUS CALIBRATION RUNS
 1210-15 30.00 I 25669     0149    I  790 JADEPR.T149A I
 1210-15 30.00 I 25669     0149    I  753 JADEPR.T149B I
 1217-25 30.00 I 27582     0150    I 1211 JADEPR.T150A I
 1217-25 30.00 I 27582     0150    I 1068 JADEPR.T150B I
 1226-39 30.00 I 47190     0151    I  959 JADEPR.T151A I
 1226-39 30.00 I 47190     0151    I 1303 JADEPR.T151B I
 1226-39 30.00 I 47190     0151    I 1912 JADEPR.T151C I
 1240-44 30.00 I 15622     0152    I 3956 JADEPR.T152  I
------------------------------------------------------------------------

      THE ABOVE 8 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0022V00    11952 EVENTS
 BACKUP COPY OF REDUC1.G0022V00 :  JADEPR.ARCHIVE.REDUC022

------------------------------------------------------------------------
 1249-57 30.00 I 33518     0153    I 2689 JADEPR.T153A I
                                     FUNNY RUN 63 INSTEAD OF 1253
 1249-57 30.00 I 33518     0153    I 4468 JADEPR.T153B I
 1258-64 30.00 I 21364     0154    I 4345 JADEPR.T154  I
 1265-68 30.00 I 21801     0155    I 1862 JADEPR.T155A I
 1265-68 30.00 I 21801     0155    I 1472 JADEPR.T155B I
 1269-72 30.00 I 26432     0156    I 1962 JADEPR.T156A I
 1269-72 30.00 I 26432     0156    I 2311 JADEPR.T156B I
------------------------------------------------------------------------

      THE ABOVE 7 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0023V00    19109 EVENTS
 BACKUP COPY OF REDUC1.G0023V00 :  JADEPR.ARCHIVE.REDUC023

------------------------------------------------------------------------
 1273-75 30.00 I 25643     0157    I 2568 JADEPR.T157A I
 1273-75 30.00 I 25643     0157    I 2799 JADEPR.T157B I
 1276-81 30.00 I 26744     0158    I 3297 JADEPR.T158A I
 1276-81 30.00 I 26744     0158    I 3125 JADEPR.T158B I   ( NEW CALIBR)
 1282-86 30.00 I 26503     0159    I 1999 JADEPR.T159A I
 1282-86 30.00 I 26503     0159    I 1892 JADEPR.T159B I
------------------------------------------------------------------------

      THE ABOVE 6 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0024V00    15680 EVENTS
 BACKUP COPY OF REDUC1.G0024V00 :  JADEPR.ARCHIVE.REDUC024

------------------------------------------------------------------------
 1287- 1291 30.I 22922     0160    I 1630 JADEPR.T160A I
 1287- 1291 30.I 22922     0160    I 1700 JADEPR.T160B I
    NO RUN 1288            0160
 1292- 1301 30.I 26764     0161    I 2866 JADEPR.T161A I
 1292- 1301 30.I 26764     0161    I 2363 JADEPR.T161B I
 1302-03 30.00 I 24859     0162    I 2056 JADEPR.T162A I
 1302-03 30.00 I 24859     0162    I 2337 JADEPR.T162BNEW I
 1304-15 30.00 I 20445     0163    I 1781 JADEPR.T163A I
 1304-15 30.00 I 20445     0163    I  894 JADEPR.T163B I
------------------------------------------------------------------------

      THE ABOVE 8 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0025V00    15627 EVENTS
 BACKUP COPY OF REDUC1.G0025V00 :  JADEPR.ARCHIVE.REDUC025

------------------------------------------------------------------------
 1317-20 30.00 I 18860     0164    I 1571 JADEPR.T164A I
 1317-20 30.00 I 18860     0164    I 1175 JADEPR.T164B I
 1321-36 30.00 I 32219     0165    I 3170 JADEPR.T165A I
 1321-36 30.00 I 32219     0165    I 3564 JADEPR.T165B I
 1337-41 30.00 I 22123     0166    I 2003 JADEPR.T166A I
 1337-41 30.00 I 22123     0166    I 2756 JADEPR.T166B I
------------------------------------------------------------------------

      THE ABOVE 6 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0026V00    14239 EVENTS
 BACKUP COPY OF REDUC1.G0026V00 :  JADEPR.ARCHIVE.REDUC026

------------------------------------------------------------------------
 1344-46 30.00 I 28358     0167    I 2842 JADEPR.T167A I
 1344-46 30.00 I 28358     0167    I 3777 JADEPR.T167B I
 1347-58 30.00 I 24168     0168    I 2522 JADEPR.T168A I
 1347-58 30.00 I 24168     0168    I 2126 JADEPR.T168B I
   NO 1349-52  1354-56    NO 1363
 1359-70 30.00 I 32723     0169    I 2352 JADEPR.T169A I
 1359-70 30.00 I 32723     0169    I 2713 JADEPR.T169B I
------------------------------------------------------------------------

      THE ABOVE 6 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0027V00    16332 EVENTS
 BACKUP COPY OF REDUC1.G0027V00 :  JADEPR.ARCHIVE.REDUC027

------------------------------------------------------------------------
 1371-74 30.00 I 25100     0170    I 2296 JADEPR.T170A I
 1371-74 30.00 I 25100     0170    I 2349 JADEPR.T170B I
 1375-80 30.00 I 23906     0171    I 2013 JADEPR.T171A I
 1375-80 30.00 I 23906     0171    I 2088 JADEPR.T171B I
 1381-83 30.00 I 24417     0172    I 2059 JADEPR.T172A I
 1381-83 30.00 I 24417     0172    I 2449 JADEPR.T172B I
------------------------------------------------------------------------

      THE ABOVE 6 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0028V00    13254 EVENTS
 BACKUP COPY OF REDUC1.G0028V00 :  JADEPR.ARCHIVE.REDUC028

------------------------------------------------------------------------
 1384-97 30.00 I 14629     0173    I 2955 JADEPR.T173  I
------------------------------------------------------------------------
 THE 30 GEV DATA TAPES, REFORM 149 - 173 HAVE BEEN SUBMITTED FOR DATA
 REDUCTION STEP 1, IN THE AUTUMN VERSION OF THE PROGRAM. THE FOLLOWING
 H-JOBS ARE IN THE QEUE (SUBMITTED 21.10.79):
         REFORM 149 - 151   110 MIN.  11437  JADEPR.TNEW151
         REFORM 152 - 155   110 MIN.         JADEPR.TNEW155
         REFORM 156 - 159   110 MIN.         JADEPR.TNEW159
         REFORM 160 - 163   110 MIN.         JADEPR.TNEW163
         REFORM 164 - 167   110 MIN.         JADEPR.TNEW167
         REFORM 168 - 170   100 MIN.         JADEPR.TNEW170
         REFORM 171 - 173    80 MIN.         JADEPR.TNEW173
------------------------------------------------------------------------
C/////////////////// END SUMMER 1979 ///////////////////////////
C
C/////////////////// BEGIN AUTUMN 79 ///////////////////////////
 1487-1503     I 46361     0177    I      JADEPR.T177A  I COSMICS
 1487-1503     I 46361     0177    I      JADEPR.T177B  I COSMICS
 1487-1503     I 46361     0177    I      JADEPR.T177C  I COSMICS
 1487-1503 30.5I 46361     0177    I  668 JADEPR.T177D  ICOSM/REAL DATA
 1487-1503 30.5I 46361     0177    I 2214 JADEPR.T177E  I REAL DATA
                                   I             FIRST LUMI 1497
  RERUN 177   JADEPR.TNEW177      4161 EVENTS
  RERUN 178   JADEPR.TNEW178C      3916 EVENTS
  RERUN 178   JADEPR.TNEW178B      3098 EVENTS
   MERGE 1NEW177,178C,178B+F22WAT.INTER29 -> JADEPR.INTER29
                 JADEPR.INTER29 HOLDS  21111 EVENTS
 1504-19 30.50 I 29728     0178    I 1440 JADEPR.T178A  I
 1504-19  "    I 29728     0178    I 1304 JADEPR.T178B  I
 1504-19  "    I 29728     0178    I 1580 JADEPR.T178C  I
 1504-19  "    I 29728     0178    I  710 JADEPR.T178D  I
 1520-30  "    I 33055     0179    I 1121 JADEPR.T179A  I
 1520-30  "    I 33055     0179    I 1257 JADEPR.T179B  I
 1520-30  "    I 33055     0179    I 1211 JADEPR.T179C  I
 1520-30  "    I 33055     0179    I 1153 JADEPR.T179D  I
 1531-45  "    I 35152     0180    I 1406 JADEPR.T180A  I  NO 1536,42,44
 1531-45  "    I 35152     0180    I 1363 JADEPR.T180B  I
 1531-45  "    I 35152     0180    I 1179 JADEPR.T180C  I
 1531-45  "    I 35152     0180    I 1246 JADEPR.T180D  I
------------------------------------------------------------------------

      THE ABOVE 14 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0029V00        17851 EVENTS
 BACKUP COPY OF REDUC1.G0028V00 :  JADEPR.ARCHIVE.REDUC029

------------------------------------------------------------------------
  CM ENERGY 30.50 GEV FOR RUN LE.1559
 1547-64 30.52 I 21491     0181    I 1178 JADEPR.T181A  I NO 1548,57
 1547-64  "    I 21491     0181    I 1182 JADEPR.T181B  I
 1547-64  "    I 21491     0181    I 1029 JADEPR.T181C  I
 1547-64         21491     0181      2948 JADEPR.T181AX**REMADE**
 1565-75  "    I 49474     0182    I 1831 JADEPR.T182A  I
 1565-75  "    I 49474     0182    I 1186 JADEPR.T182B  I
 1565-75  "    I 49474     0182    I 1238 JADEPR.T182C  I
 1565-75  "    I 49474     0182    I 1219 JADEPR.T182D  I
 1565-75  "    I 49474     0182    I 1057 JADEPR.T182E  I
 1565-75  "    I 49474     0182    I  776 JADEPR.T182F  I
 1565-75  "    I 49474     0182    I 5769 JADEPR.T182AX**REMADE**
  CM ENERGY 30.52 GEV FOR RUN LE.1576
            TAPE 182  INCLUDES RUNS       +1493,94,68,72,74
------------------------------------------------------------------------

      THE ABOVE  9 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0030V00      10625 EVENTS
17.02.80 :
DATA REPLACED BY THE CORRESPONDING DATA ON F22WAT.REDUC1.G0030V00,
     WHICH HAS CORRECTED LEAD GLASS ENERGIES         10695 EVENTS
     THIS TAPE (JADEPR.REDUC1.G0030V00) CAN NO LONGER BE READ, GIVES
     IRRECOVERABLE READ ERROR (0C1)
        PLEASE AVOID UNTIL FURTHER NOTICE !!!
                     J.OLSSON  21.03.80
  ******** JADEPR.REDUC1.G0030V00 NOW OK  CONTAINS 8717 EVENTS
------------------------------------------------------------------------
 1576-78 30.54 I 11564     0183    I  769 JADEPR.T183A  I  NO 1577
 1576-78  "    I 11564     0183    I  695 JADEPR.T183B  I
   TAPE 184 CONTAINS NO RUNS 1588,90      RUNS 1591,92 &63(?) ARE DOUBLE
 1579-1603"    I 40142     0184    I 1237 JADEPR.T184A  I
 1579-03  "    I 40142     0184    I 1374 JADEPR.T184B  I
 1579-03  "    I 40142     0184    I 1294 JADEPR.T184C  I
 1579-03  "    I 40142     0184    I 1433 JADEPR.T184D  I
 1579-03  "    I 40142     0184    I 1624 JADEPR.T184E  I
 1604-15  "    I 19931     0185    I 1272 JADEPR.T185A  I NO RUN 1605
 1604-15  "    I 19931     0185    I 1063 JADEPR.T185B  I
 1604-15  "    I 19931     0185    I  987 JADEPR.T185C  I
  CM ENERGY 30.54 GEV FOR RUN LE.1608
 1616-26 30.56 I 33359     0186    I 1280 JADEPR.T186A  I
 1616-26  "    I 33359     0186    I 1391 JADEPR.T186B  I
 1616-26  "    I 33359     0186    I 1539 JADEPR.T186C  I
 1616-26  "    I 33359     0186    I 1381 JADEPR.T186D  I
------------------------------------------------------------------------

      THE ABOVE 14 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0031V00     17323 EVENTS

17.02.80 :
DATA REPLACED BY THE CORRESPONDING DATA ON F22WAT.REDUC1.G0031V00,
     WHICH HAS CORRECTED LEAD GLASS ENERGIES        17323 EVENTS
 BACKUP COPY OF REDUC1.G0028V00 :  JADEPR.ARCHIVE.REDUC031
------------------------------------------------------------------------
 1627-32 30.58 I 27249     0187    I 1063 JADEPR.T187A  I
 1627-32  "    I 27249     0187    I 1095 JADEPR.T187B  I
 1627-32  "    I 27249     0187    I 1225 JADEPR.T187C  I
 1627-32  "    I 27249     0187    I 1396 JADEPR.T187D  I
  CM ENERGY 30.58 GEV FOR RUN LE.1637
 1633-40 30.60 I 34501     0188    I 1358 JADEPR.T188A  I
 1633-40  "    I 34501     0188    I 1519 JADEPR.T188B  I
 1633-40  "    I 34501     0188    I 1610 JADEPR.T188C  I
 1633-40  "    I 34501     0188    I 1306 JADEPR.T188D  I
 1641-43  "    I 21942     0189    I 1079 JADEPR.T189A  I
 1641-43  "    I 21942     0189    I  840 JADEPR.T189B  I
 1641-43  "    I 21942     0189    I 1253 JADEPR.T189C  I
 1644-48  "    I 28796     0190    I  988 JADEPR.T190A  I
 1644-48  "    I 28796     0190    I  510 JADEPR.T190B  I
 1644-48  "    I 28796     0190    I 1254 JADEPR.T190C  I
 1644-48  "    I 28796     0190    I 1605 JADEPR.T190D  I
------------------------------------------------------------------------

      THE ABOVE 15 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0032V00    18101 EVENTS
            DELETED AND RECREATED 05.10.79   (TRIAL READ OK!)
17.02.80 :
DATA REPLACED BY THE CORRESPONDING DATA ON F22WAT.REDUC1.G0032V00,
     WHICH HAS CORRECTED LEAD GLASS ENERGIES       18101 EVENTS
 BACKUP COPY OF REDUC1.G0028V00 :  JADEPR.ARCHIVE.REDUC032
------------------------------------------------------------------------
 1649-53 30.62 I 32952     0191    I 1174 JADEPR.T191A  I
 1649-53  "    I 32952     0191    I 1542 JADEPR.T191B  I
 1649-53  "    I 32952     0191    I 1190 JADEPR.T191C  I
 1649-53  "    I 32952     0191    I 1192 JADEPR.T191D  I
 1654-59  "    I 32057     0192    I 1163 JADEPR.T192A  I
 1654-59  "    I 32057     0192    I 1201 JADEPR.T192B  I
 1654-59  "    I 32057     0192    I 1272 JADEPR.T192C  I
 1654-59  "    I 32057     0192    I 1167 JADEPR.T192D  I
------------------------------------------------------------------------

      THE ABOVE  8 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0033V00    9901 EVENTS
            DELETED AND RECREATED 05.10.79   (TRIAL READ OK!)

17.02.80 :
DATA REPLACED BY THE CORRESPONDING DATA ON F22WAT.REDUC1.G0033V00,
     WHICH HAS CORRECTED LEAD GLASS ENERGIES        9901 EVENTS
 BACKUP COPY OF REDUC1.G0028V00 :  JADEPR.ARCHIVE.REDUC033
------------------------------------------------------------------------
  CM ENERGY 30.62 GEV FOR RUN LE.1661
 1660-67 30.64 I 25817     0193    I 1339 JADEPR.T193A  I
 1660-67  "    I 25817     0193    I 1218 JADEPR.T193B  I
 1660-67  "    I 25817     0193    I 1340 JADEPR.T193C  I
 1668-79  "    I 33514     0194    I 1187 JADEPR.T194A  I  NO 1670,75
 1668-79  "    I 33514     0194    I 1368 JADEPR.T194B  I
 1668-79  "    I 33514     0194    I 1102 JADEPR.T194C  I
 1668-79  "    I 33514     0194    I  592 JADEPR.T194D  I
 1680-81  "    I 19157     0195    I  944 JADEPR.T195A  I
 1680-81  "    I 19157     0195    I 1146 JADEPR.T195B  I
 1680-81  "    I 19157     0195    I  869 JADEPR.T195C  I
------------------------------------------------------------------------

      THE ABOVE 10 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0034V00    11105 EVENTS
             (TRIAL READ OK!)

17.02.80 :
DATA REPLACED BY THE CORRESPONDING DATA ON F22WAT.REDUC1.G0034V00,
     WHICH HAS CORRECTED LEAD GLASS ENERGIES       11105 EVENTS
 BACKUP COPY OF REDUC1.G0028V00 :  JADEPR.ARCHIVE.REDUC034
------------------------------------------------------------------------
  CM ENERGY 30.64 GEV FOR RUN LE.1686
1682-1705 30.66I 25942     0196    I 1428 JADEPR.T196A  I
1682-1705 30.66I 25942     0196    I 1501 JADEPR.T196B  I
1682-1705 30.66I 25942     0196    I 1329 JADEPR.T196C  I
1682-1705 30.66I 25942     0196    I  531 JADEPR.T196D  I
  CM ENERGY 30.66 GEV FOR RUN LE.1713
 1706-15 30.68 I 20723     0197    I 1293 JADEPR.T197A  I
 1706-15 30.68 I 20723     0197    I 2712 JADEPR.T197B  I
 1716-18  "    I  2417     0198    I  549 JADEPR.T198   I NO1717
 1719-21  "    I  9639     0199    I 1784 JADEPR.T199   I
  CM ENERGY 30.68 GEV FOR RUN LE.1724
 1722-25 30.70 I 27908     0200    I 1587 JADEPR.T200A  I
 1722-25 30.70 I 27908     0200    I 1219 JADEPR.T200B  I
 1722-25 30.70 I 27908     0200    I 1170 JADEPR.T200C  I
 1722-25 30.70 I 27908     0200    I 1097 JADEPR.T200D  I
 1726-29  "    I 31544     0201    I 1326 JADEPR.T201A  I
 1726-29  "    I 31544     0201    I  986 JADEPR.T201B  I
 1726-29  "    I 31544     0201    I  947 JADEPR.T201C  I
 1726-29  "    I 31544     0201    I 1164 JADEPR.T201D  I
------------------------------------------------------------------------

      THE ABOVE 16 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0035V00    20623 EVENTS
             (TRIAL READ OK!)

17.02.80 :
DATA REPLACED BY THE CORRESPONDING DATA ON F22WAT.REDUC1.G0035V00,
     WHICH HAS CORRECTED LEAD GLASS ENERGIES       20623 EVENTS
 BACKUP COPY OF REDUC1.G0028V00 :  JADEPR.ARCHIVE.REDUC035
------------------------------------------------------------------------
 1730-39 30.72 I 25300     0202    I 1601 JADEPR.T202A  I
 1730-39 30.72 I 25300     0202    I 1598 JADEPR.T202B  I
 1730-39 30.72 I 25300     0202    I 1287 JADEPR.T202C  I
 1740-42 30.74 I  8490     0203    I 1241 JADEPR.T203   I
 1742-43  "    I  6260     0204    I 1009 JADEPR.T204   I
                                RUN 1742 REPEATED ON 204, 1 EVENT
 1744-48  "    I 21141     0205    I 1210 JADEPR.T205A  I
 1744-48  "    I 21141     0205    I 2472 JADEPR.T205B  I
CM ENERGY WAS CHANGED INTO 30.76 IN THE MIDDLE OF RUN 1749
 1749-55 30.76 I 28170     0206    I 2433 JADEPR.T206A  I
 1749-55  "    I 28170     0206    I 2717 JADEPR.T206B  I
 1756-60 30.78 I 31178     0207    I 2504 JADEPR.T207A  I
 1756-60  "    I 31178     0207    I 2459 JADEPR.T207B  I
------------------------------------------------------------------------

      THE ABOVE 11 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0036V00       20531  EVENTS
             (TRIAL READ OK!)

17.02.80 :
DATA REPLACED BY THE CORRESPONDING DATA ON F22WAT.REDUC1.G0036V00,
     WHICH HAS CORRECTED LEAD GLASS ENERGIES       20531 EVENTS
 BACKUP COPY OF REDUC1.G0028V00 :  JADEPR.ARCHIVE.REDUC036
------------------------------------------------------------------------
CM ENERGY WAS 30.80 GEV FROM RUN 1763
 1761-65 30.80 I 29661     0208    I 2327 JADEPR.T208A  I
 1761-65  "    I 29661     0208    I 1895 JADEPR.T208B  I
 1766     "    I  3416     0209    I  324 JADEPR.T209   I
 1767     "    I  6582     0210    I  829 JADEPR.T210   I
CM ENERGY WAS 30.82 GEV FROM RUN 1770
 1768-73 30.82 I 32644     0211    I 2332 JADEPR.T211A  I
 1768-73  "    I 32644     0211    I 2241 JADEPR.T211B  I
 1774-79  "    I 26805     0212    I 2106 JADEPR.T212A  I
 1774-79  "    I 26805     0212    I 1589 JADEPR.T212B  I
CM ENERGY WAS 30.84 GEV FROM RUN 1779
 1780-83 30.84 I 22401     0213    I  840 JADEPR.T213A  I
 1780-83  "    I 22401     0213    I  795 JADEPR.T213B  I
 1780-83  "    I 22401     0213    I  779 JADEPR.T213C  I
------------------------------------------------------------------------

      THE ABOVE 11 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0037V00   16057 EVENTS
             (TRIAL READ OK!)

17.02.80 :
DATA REPLACED BY THE CORRESPONDING DATA ON F22WAT.REDUC1.G0037V00,
     WHICH HAS CORRECTED LEAD GLASS ENERGIES       16057 EVENTS
 BACKUP COPY OF REDUC1.G0028V00 :  JADEPR.ARCHIVE.REDUC037
------------------------------------------------------------------------
 1784-87 30.86 I 26022     0214    I 1306 JADEPR.T214A  I
 1784-87 30.86 I 26022     0214    I 1034 JADEPR.T214B  I
 1788-93  "    I 27097     0215    I 1326 JADEPR.T215A  I
 1788-93  "    I 27097     0215    I 1016 JADEPR.T215B  I
CM ENERGY WAS 30.88 GEV FROM RUN 1796
 1794-98 30.88 I 29432     0216    I 1438 JADEPR.T216A  I
 1794-98  "    I 29432     0216    I 1761 JADEPR.T216B  I
CM ENERGY WAS 30.90 GEV FROM RUN 1802
 1799-1802 "   I 29857     0217    I 2190 JADEPR.T217A  I
 1799-1802 "   I 29857     0217    I 2195 JADEPR.T217B  I
 1803-08 30.90 I 23473     0218    I 1971 JADEPR.T218A  I
 1803-08  "    I 23473     0218    I  663 JADEPR.T218B  I    NO 1805,06
------------------------------------------------------------------------

      THE ABOVE 10 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0038V00     14900 EVENTS
             (TRIAL READ OK!)
17.02.80 :
DATA REPLACED BY THE CORRESPONDING DATA ON F22WAT.REDUC1.G0038V00,
     WHICH HAS CORRECTED LEAD GLASS ENERGIES       14900 EVENTS
 BACKUP COPY OF REDUC1.G0028V00 :  JADEPR.ARCHIVE.REDUC038

------------------------------------------------------------------------
 1809-11 30.92 I 25918     0219    I 2027 JADEPR.T219A  I
 1809-11 30.92 I 25918     0219    I  981 JADEPR.T219B  I
LUMINOSITY RUNS END WITH RUN 1813   REST IS COSMICS
 1812-22  "    I 28184     0220    I 3721 JADEPR.T220A  I
 1812-22  "    I 28184     0220    I 2918 JADEPR.T220B  I ONLY COSMICS
CM ENERGY WAS 30.94 GEV FROM RUN 1866
CM ENERGY WAS 31.00 GEV FROM RUN 1869
 1846-69 30.94 I 37018     0221    I 2858 JADEPR.T221A  I
 1846-69 30.94 I 37018     0221    I 2527 JADEPR.T221B  I
 1846-69 30.94 I 37018     0221    I 1092 JADEPR.T221C  I
CM ENERGY WAS 30.94 GEV FROM RUN 1872
 1870-75 30.94 I 24647     0222    I 2952 JADEPR.T222A  I
 1870-75 30.94 I 24647     0222    I 1478 JADEPR.T222B  I
------------------------------------------------------------------------

      THE ABOVE  9 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0039V00       20554  EVENTS
--->
--->   THIS TAPE WILL BE REWRITTEN, TO INCLUDE TNEW221,222
--->        RECREATED:  NOW 24574 EVENTS
             (TRIAL READ OK!)
 BACKUP COPY OF REDUC1.G0028V00 :  JADEPR.ARCHIVE.REDUC039
------------------------------------------------------------------------
 1876-80 30.96 I 32742     0223    I 3054 JADEPR.T223A  I
 1876-80 30.96 I 32742     0223    I 2900 JADEPR.T223B  I
CM ENERGY WAS 30.98 GEV FROM RUN 1882
 1881-87 30.98 I 30150     0224    I 3259 JADEPR.T224A  I
 1881-87 30.98 I 30150     0224    I 2475 JADEPR.T224B  I
CM ENERGY WAS 31.00 GEV FROM RUN 1889
 1888-92 31.00 I 30503     0225    I 2876 JADEPR.T225A  I
 1888-92 31.00 I 30503     0225    I 2488 JADEPR.T225B  I
 1893-95 31.02 I 18033     0226    I 3084 JADEPR.T226   I
------------------------------------------------------------------------

      THE ABOVE  7 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0040V00         20136  EVENTS
             (TRIAL READ OK!)
--->
--->   THIS TAPE HAS BEEN TEMPORARILY DELETED, TO BE RECREATED FROM
--->   THE TAPES TNEW223-226.  SEE BOTTOM FOR DETAILS.
--->        RECREATED:  NOW 36162 EVENTS
             (TRIAL READ OK!)
  ************  * * * * OBS OBS OBS  MESSAGE FROM 12.12.79
THIS TAPE HAS BEEN THORN APART IN ITS FIRST REEL. O.HELL HAS TRIED TO
REPAIR IT BUT IT HAS NOW MOST CERTAINLY A READ ERROR BUILT IN TO THE
REPAIRED PLACE OF THE TAPE.  A COPY JOB IS SUBMITTED :
       REDUC1.G0040V00 IS COPIED INTO JADEPR.INTER140    FAILS,READ ERR
O.HELL ALSO WROTE THE TAPE JADEPR.REDUC140 WITH AN UNKNOWN PART OF THE
DATA FROM THE THORN TAPE.    TEST READ: 15911 EVENTS, WITH READ ERROR
 THESE DATA ARE PROBABLY LOST. RECREATION STARTED WITH THE FOLLOWING:

 1876-80 30.96 I 32742     0223    I10822 JADEPR.TNEW223I
 1881-87 30.98 I 30150     0224    I10338 JADEPR.TNEW224I
 1888-92 31.00 I 30503     0225    I 4628 JADEPR.TNEW225AI
 1888-92 31.00 I 30503     0225    I 4891 JADEPR.TNEW225BI
 1893-95 31.02 I 18033     0226    I 5433 JADEPR.TNEW226I
*******
******** REDUC1.G0040V00 NOW REMADE     TEST READ OK!   21.12.79
*******
 BACKUP COPY OF REDUC1.G0028V00 :  JADEPR.ARCHIVE.REDUC040
------------------------------------------------------------------------
CM ENERGY WAS 31.04 GEV FROM RUN 1898
 1896-1901 "   I 26689     0227    I 1761 JADEPR.T227A  I
 1896-1901 "   I 26689     0227    I 1278 JADEPR.T227B  I
CM ENERGY WAS 31.06 GEV FROM RUN 1905
 1902-05 31.04 I 17824     0228    I 3155 JADEPR.T228   I
CM ENERGY WAS 31.08 GEV FROM RUN 1909
 1906-09 31.06 I 28531     0229    I 3032 JADEPR.T229A  I
 1906-09  "    I 28531     0229    I 2118 JADEPR.T229B  I
 1910-12 31.08 I 26045     0230    I 3020 JADEPR.T230A  I
 1910-12  "    I 26045     0230    I 1744 JADEPR.T230B  I
CM ENERGY WAS 31.10 GEV FROM RUN 1912
CM ENERGY WAS 31.12 GEV FROM RUN 1920
 1913-21 31.10 I 30569     0231    I 3275 JADEPR.T231A  I  NO 1915
 1913-21  "    I 30569     0231    I 2245 JADEPR.T231B  I
------------------------------------------------------------------------

      THE ABOVE  9 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0041V00      21628  EVENTS
--->   THIS TAPE HAS BEEN TEMPORARILY DELETED, TO BE RECREATED FROM
--->   THE TAPES TNEW227-231.  SEE BOTTOM FOR DETAILS.
--->        RECREATED:  NOW 40044 EVENTS
             (TRIAL READ OK!)

------------------------------------------------------------------------
CM ENERGY WAS 31.14 GEV FROM RUN 1929
 1922-31 31.12 I 21952     0232    I 2750 JADEPR.T232A  I
 1922-31  "    I 21952     0232    I  814 JADEPR.T232B  I1926 REP.W 1 EV
OBS!!!  RUNS 1932-35 HAVE BEEN OMITTED FROM DATA STREAM BY MISTAKE. THEY
OBS!!!  WILL BE INCORPORATED BY SPECIAL ACTION, AFTER NOTICE GIVEN.
 F11LHO.SPREFOR --->>  35495 EVENTS   JADEPR.SPREFORA 2413
 F11LHO.SPREFOR --->>                 JADEPR.SPREFORB 1908
CM ENERGY WAS 31.16 GEV FROM RUN 1935
 1936-39 31.16 I 24627     0233    I 2343 JADEPR.T233A  I
 1936-39  "    I 24627     0233    I 1088 JADEPR.T233B  I
CM ENERGY WAS 31.18 GEV FROM RUN 1941
 1940-44 31.18 I 23846     0234    I 2667 JADEPR.T234A  I
 1940-44  "    I 23846     0234    I  977 JADEPR.T234B  I
CM ENERGY WAS 31.20 GEV FROM RUN 1946
 1945-48 31.20 I 28752     0235    I 1210 JADEPR.T235A  I
 1945-48  "    I 28752     0235    I 1503 JADEPR.T235C  I
 1945-48  "    I 28752     0235    I 1516 JADEPR.T235B  I
------------------------------------------------------------------------

      THE ABOVE 11 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0042V00     19189   EVENTS
             (TRIAL READ OK!)
--->   THIS TAPE HAS BEEN TEMPORARILY DELETED, TO BE RECREATED FROM
--->   THE TAPES TNEW232-236.  SEE BOTTOM FOR DETAILS.
--->        RECREATED:  NOW 34685 EVENTS
                 TEST READ   OK !
------------------------------------------------------------------------
CM ENERGY WAS 31.22 GEV FROM RUN 1953
 1949-55 31.22 I 33245     0236    I 2249 JADEPR.T236A  I REPT.1 EV RNS
 1949-55  "    I 33245     0236    I 2363 JADEPR.T236B  I
CM ENERGY WAS 31.24 GEV FROM RUN 1959
 1956-62 31.24 I 31090     0237    I 2569 JADEPR.T237A  I
 1956-62  "    I 31090     0237    I 1757 JADEPR.T237B  I
CM ENERGY WAS 31.26 GEV FROM RUN 1966
 1963-68 31.26 I 31628     0238    I 2095 JADEPR.T238A  I
 1963-68  "    I 31628     0238    I 2461 JADEPR.T238B  I
CM ENERGY WAS 31.28 GEV FROM RUN 1970
CM ENERGY WAS 31.30 GEV FROM RUN 1973
 1969-73 31.28 I 34130     0239    I 3579 JADEPR.T239A  I
 1969-73 31.30 I 34130     0239    I 3347 JADEPR.T239B  I
------------------------------------------------------------------------

      THE ABOVE  8 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0043V00      20420  EVENTS
--->   THIS TAPE HAS BEEN TEMPORARILY DELETED, TO BE RECREATED FROM
--->   THE TAPES TNEW237-240.  SEE BOTTOM FOR DETAILS.
--->        RECREATED:  NOW 35886 EVENTS
             (TRIAL READ OK!)

------------------------------------------------------------------------
CM ENERGY WAS 31.32 GEV FROM RUN 1978
 1974-83 31.32 I 26637     0240    I 2616 JADEPR.T240A  I
 1974-83  "    I 26637     0240    I 1819 JADEPR.T240B  I
                                  REFORM 240:    SEVERAL MIS. OR REP.
CM ENERGY WAS 31.34 GEV FROM RUN 1987
 1984-95 31.34 I 30097     0241    I 3261 JADEPR.T241A  I
 1984-95  "    I 30097     0241    I 3119 JADEPR.T241B  I
RUNS 1991 - 2007  ARE PULSER RUNS FOR INNER DETECTOR    2010,12 MISSING
CM ENERGY WAS 31.36 GEV FROM RUN 2009
 1996-2012     I 27484     0242    I  143 JADEPR.T242A  I
CM ENERGY WAS 31.38 GEV FROM RUN 2018
 2015-24 31.38 I 32666     0243    I 3213 JADEPR.T243A  I2016,20,21 MISS
 2015-24  "    I 32666     0243    I 2941 JADEPR.T243B  I
------------------------------------------------------------------------

      THE ABOVE  7 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0044V00     17112   EVENTS
             (TRIAL READ OK!)
--->   THIS TAPE HAS BEEN TEMPORARILY DELETED, TO BE RECREATED FROM
--->   THE TAPES TNEW241-244.  SEE BOTTOM FOR DETAILS.
--->        RECREATED:  NOW 38236 EVENTS
                        TEST READ  OK !
------------------------------------------------------------------------
CM ENERGY WAS 31.40 GEV FROM RUN 2026
 2025-29 31.40 I 25789     0244    I 2713 JADEPR.T244A  I
 2025-29  "    I 25789     0244    I 1636 JADEPR.T244B  I
                         245     IDENTICAL TO 244
CM ENERGY WAS 31.42 GEV FROM RUN 2033
 2030-35 31.42 I 30094     0246    I 3414 JADEPR.T246A  I  NO RUN 2035
 2030-35  "    I 30094     0246    I 2410 JADEPR.T246B  I
 2038     "    I     3     0247    I NOT DATA REDUCED 1 I RUN 63 JUNK
 2040     "    I  6498     0248    I 1166 JADEPR.T248   I
CM ENERGY WAS 31.44 GEV FROM RUN 2042
CM ENERGY WAS 31.46 GEV FROM RUN 2052 TILL RUN 2055
 2041-55 31.44 I 24104     0249    I 2426 JADEPR.T249A  I
 2041-55 31.44 I 24104     0249    I 2316 JADEPR.T249B  I
------------------------------------------------------------------------

      THE ABOVE  7 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0045V00    16081    EVENTS
             (TRIAL READ OK!)
   F11JAD36 WRITES JADEPR.INTER45   TO CONTAIN ALL RUNS AFTER 2040
  LATER TO BE COMBINED WITH TNEW245,246    SEE BELOW
   INTER45 CONTAINS 5908 EVENTS       TEST READ OK
--->   THIS TAPE HAS BEEN TEMPORARILY DELETED, TO BE RECREATED FROM
--->   THE TAPES TNEW245,246,INTER45       SEE BOTTOM FOR DETAILS.
--->        RECREATED:  NOW 24168 EVENTS
                    TEST READ   OK !
------------------------------------------------------------------------
 2057    30.00 I  5816     0250    I 2613 JADEPR.T251   I
 2058     "    I  9995     0251    I  "        "        I
         OFFSET BECOMES 184
 2059-63  "    I 25121     0252    I 1927 JADEPR.T252A  I
 2059-63  "    I 25121     0252    I 2027 JADEPR.T252B  I
CM ENERGY WAS 30.02 GEV FROM RUN 2063
 2064-71 30.02 I 31541     0253    I 2726 JADEPR.T253A  I
 2064-71 30.02 I 31541     0253    I 2092 JADEPR.T253B  I
------------------------------------------------------------------------

      THE ABOVE  5 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0046V00    11385    EVENTS
             (TRIAL READ OK!)

------------------------------------------------------------------------
CM ENERGY WAS 30.04 GEV FROM RUN 2070
 2072-75 30.04 I 13208     0254    I 2497 JADEPR.T254   I
CM ENERGY WAS 30.06 GEV FROM RUN 2077
 2076-80 30.06 I 20873     0255    I 1349 JADEPR.T255A  I
 2076-80 30.06 I 20873     0255    I 1873 JADEPR.T255B  I

         FROM HERE ON THE REFORMATTED DATA BEGIN TO APPEAR ON GENERATION
         DATA GROUP F11LHO.JDATA02.REFORM.G0001V00, ETC.THE OFFSET
         BECOMES -71

CM ENERGY WAS 30.08 GEV FROM RUN 2083
 2081-83 30.06 I 18555     0001    I 2939 JADEPR.ST001  I
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0047V00      8658   EVENTS
             (TRIAL READ OK!)

------------------------------------------------------------------------
 2084-91 30.08 I 25098     0002    I 2201 JADEPR.ST002A I NO RUN 2090
 2084-91 30.08 I 25098     0002    I 2001 JADEPR.ST002B I
 2092-96 30.10 I 20357      003    I 4330 JADEPR.ST003  I
 2097-12 30.12 I 20405      004    I 4469 JADEPR.ST004  I
                                        NO RUNS 2099 - 2107
CM ENERGY WAS 30.16 GEV FROM RUN 2117
 2118-22 30.16 I 15874   SPREFOR1  I 3141 JADEPR.SPREF1 I
        SEE BELOW, THESE DATA BELONG AFTER TAPE 005
------------------------------------------------------------------------

      THE ABOVE  5 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0048V00     16142   EVENTS
             (TRIAL READ OK!)
    F11JAD38 WRITES JADEPR.INTER48 TO KEEP ONLY THOSE RUNS THAT DONOT
    COME FROM SPREFOR.  THE LATTER RUNS WILL BE FOUND IN TNEW006, SEE
--->        RECREATED:  NOW 13001 EVENTS
                    TEST READ OK !
------------------------------------------------------------------------
CM ENERGY WAS 30.14 GEV FROM RUN 2112
 2113-17 30.14 I 23710      005    I 2326 JADEPR.ST005A I
 2113-17  "    I 23710      005    I 2534 JADEPR.ST005B I
CM ENERGY WAS 30.16 GEV FROM RUN 2117
   FOR MISSING DATA ON JADEPR.SPREF1, SEE ABOVE ON TAPE 0048
CM ENERGY WAS 30.18 GEV FROM RUN 2122
 2123-26 30.18 I 22855      006    I 2309 JADEPR.ST006A I
 2123-26  "    I 22855      006    I 2402 JADEPR.ST006B I
CM ENERGY WAS 30.20 GEV FROM RUN 2126
 2127-30 30.20 I 21055      007    I 2129 JADEPR.ST007A I
 2127-30  "    I 21055      007    I 1825 JADEPR.ST007B I
CM ENERGY WAS 30.22 GEV FROM RUN 2130 TILL RUN 2133
CM ENERGY WAS 30.24 GEV FROM RUN 2134
 2131-36 30.24 I 19851      008    I 2418 JADEPR.ST008A I
 2131-36  "    I 19851      008    I 1737 JADEPR.ST008B I
 2137     "    I  8329      009    I 1307 JADEPR.ST009  I
------------------------------------------------------------------------

      THE ABOVE  9 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0049V00     18987   EVENTS
             (TRIAL READ OK!)
--->   THIS TAPE HAS BEEN TEMPORARILY DELETED, TO BE RECREATED FROM
--->   THE TAPES TNEW005 - 010             SEE BOTTOM FOR DETAILS.
--->        RECREATED:  NOW 42122 EVENTS
                    TEST READ  OK !
------------------------------------------------------------------------
 2138    30.24 I            010    I RENUMBERED TO 011, SEE RUNLST
 2138     "    I  2710      011    I  501 JADEPR.ST011  I
 2145     "    I  2994      011    I 1483 JADEPR.ST012  I
 2146-47 30.26 I  4634      012    I           "        I
 2148     "    I  9601      013    I 1974 JADEPR.ST013  I
         OFFSET BECOMES -67
CM ENERGY WAS 30.28 GEV FROM RUN 2150
 2149-51 30.28 I 22386      014    I 2008 JADEPR.ST014A I
 2149-51  "    I 22386      014    I 2197 JADEPR.ST014B I
CM ENERGY WAS 30.30 GEV FROM RUN 2153
 2152-54 30.30 I 21483      015    I 2123 JADEPR.ST015A I
 2152-54  "    I 21483      015    I 1981 JADEPR.ST015B I
 2155-58 30.32 I 20933      016    I 2449 JADEPR.ST016A I
 2155-58  "    I 20933      016    I 2159 JADEPR.ST016B I
------------------------------------------------------------------------

      THE ABOVE  9 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0050V00     16875   EVENTS
--->   THIS TAPE HAS BEEN TEMPORARILY DELETED, TO BE RECREATED FROM
--->   THE TAPES ST011 - TNEW014,15,16,17  SEE BOTTOM FOR DETAILS.
--->        RECREATED:  NOW 31895 EVENTS
                   TEST READ OK !
------------------------------------------------------------------------
                         017    COPY OF 16
CM ENERGY WAS 30.34 GEV FROM RUN 2160
 2159-61 30.34 I 19976      018    I 1994 JADEPR.ST018A I  DELETED
 TAPE ST018A CAN NOT BE READ, WILL BE REWRITTEN AS ST018C
 2159-61  "    I 19976      018    I 1997 JADEPR.ST018C I
 2159-61  "    I 19976      018    I 1414 JADEPR.ST018B I
CM ENERGY WAS 30.36 GEV FROM RUN 2164
 2162-65  "    I 19890      019    I 1924 JADEPR.ST019A I
 2162-65  "    I 19890      019    I 1572 JADEPR.ST019B I
CM ENERGY WAS 30.38 GEV FROM RUN 2168             FUNNY RUN 63
 2166-69 30.38 I 16304      020    I 3009 JADEPR.ST020  I
 2170-77  "    I 18555      021    I 2197 JADEPR.ST021A I
 2170-77  "    I 18555      021    I 1368 JADEPR.ST021B I
CM ENERGY WAS 30.40 GEV FROM RUN 2177
 2178-83 30.40 I 18584      022    I 1997 JADEPR.ST022A I
 2178-83  "    I 18584      022    I 1354 JADEPR.ST022B I
------------------------------------------------------------------------

      THE ABOVE  9 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0051V00     16832   EVENTS
             (TRIAL READ OK!)
         F11JAD39 (AMED) WRITES JADEPR.INTER51 TO COPY ALL EVENTS
             WITH RUN GE.2166   (LATER TO BE COMBINED WITH NEW018,NEW019
         DONE, 9919 EVENTS
  REDUC1.G0051V00 TEMPORARILY DELETED, TO BE REWRITTEN FROM TNEW018,19
  AND INTER51
--->        RECREATED:  NOW 22920 EVENTS
                      TEST READ OK !
------------------------------------------------------------------------
CM ENERGY WAS 30.42 GEV FROM RUN 2183
 2184-87 30.42 I 19183      023    I 2241 JADEPR.ST023A I
 2184-87  "    I 19183      023    I 1299 JADEPR.ST023B I
 2188-90 30.44 I 24062      024    I 2174 JADEPR.ST024A I
 2188-90  "    I 24062      024    I 2214 JADEPR.ST024B I
 2191-94 30.46 I 22394      025    I 2177 JADEPR.ST025A I
 2191-94  "    I 22394      025    I 1820 JADEPR.ST025B I
 2195-99 30.48 I 14494      026    I 2882 JADEPR.ST026  I
------------------------------------------------------------------------

      THE ABOVE  7 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0052V00     14807   EVENTS
             (TRIAL READ OK!)

------------------------------------------------------------------------
CM ENERGY WAS 29.90 GEV FROM RUN 2202
 2200-05 29.90 I 18966      027    I 2141 JADEPR.ST027A I
 2200-05  "    I 18966      027    I 1228 JADEPR.ST027B I
 2206-09  "    I 25584      028    I 2288 JADEPR.ST028A I
 2206-09  "    I 25584      028    I 2254 JADEPR.ST028B I
CM ENERGY WAS 29.92 GEV FROM RUN 2208
 2210-14 29.92 I 12860      029    I 2502 JADEPR.ST029  I
------------------------------------------------------------------------

      THE ABOVE  5 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0053V00     10413   EVENTS
             (TRIAL READ OK!)

------------------------------------------------------------------------
CM ENERGY WAS 29.94 GEV FROM RUN 2217
 2215-18 29.94 I 23209      030    I 2283 JADEPR.ST030A I
 2215-18  "    I 23209      030    I 2174 JADEPR.ST030B I
CM ENERGY WAS 29.96 GEV FROM RUN 2220
 2219-21 29.96 I 19717      031    I 3736 JADEPR.ST031  I
 2222-24  "    I 15321      032    I 2729 JADEPR.ST032  I
CM ENERGY WAS 29.98 GEV FROM RUN 2223
 2225-26 29.98 I 14282      033    I 2511 JADEPR.ST033  I
------------------------------------------------------------------------

      THE ABOVE  5 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0054V00     13433   EVENTS
             (TRIAL READ OK!)

------------------------------------------------------------------------
 2251-55       I  9609      034    I                    I  LG STUFF.
 2289-2305     I 23717      035    I      LOTS OR RUN 63, JUNK
 2306-18 12.00 I 30887      036    I 2063 JADEPR.ST036A I
 2306-18   "   I 30887      036    I 3830 JADEPR.ST036B I
    ST036B  FAILS FOR TIME LIMIT. DEAD LOOP SOMEWHERE, TO BE CHECKED
                          PRINT EVENTS BEFORE DEAD LOOP  ---> R2318 E640
      TESTJOB F11JAD16 TO WRITE EVENTS 639 - 645 TO JADEPR.RUN2318
      F11JAD86 NOW SKIPS EVENT 2318:640  (WHICH IS JUNK ANYHOW)
 2319-24   "   I 40011      037    I 6668 JADEPR.ST037A I
 2319-24   "   I 40011      037    I 5377 JADEPR.ST037B I
 2325-31   "   I 40031      038    I 4837 JADEPR.ST038A I
 2325-31   "   I 40031      038    I 5010 JADEPR.ST038B I
 2332-35   "   I 35454      039    I 4458 JADEPR.ST039A I
 2332-35   "   I 35454      039    I 5315 JADEPR.ST039B I
------------------------------------------------------------------------

      THE ABOVE  8 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0055V00   33174     EVENTS
            TEST READ OK !
OBS OBS OBS OBS OBS   THIS TAPE WILL BE REWRITTEN, IT DOES NOT CONTAIN
       COMPLETE DATA FROM TAPES REFORM.G0039

             TO BE FOUND ON TAPE     4451 JADEPR.ST039A I
COPY  G0055V00   RUNS LE 2331   -> JADEPR.INTER55A    27785  EVENTS
COPY  G0055V00   RECS GT 27859  -> JADEPR.INTER55B     5315  EVENTS
  MERGE INTER55A,ST039A,INTER55B  -> JADEPR.REDUC155   37551 EVENTS
     TEST READ REDUC155    OK
  COPY REDUC155  INTO   REDUC1.G0055V00 ;  DONE; TEST READ OK
 BACKUP COPY OF REDUC1.G0055V00 :  JADEPR.ARCHIVE.REDUC055
------------------------------------------------------------------------
 2336-44   "   I 33680      040    I 5003 JADEPR.ST040A I
 2336-44   "   I 33680      040    I 2827 JADEPR.ST040B I
 2345-49   "   I 33383      041    I 5931 JADEPR.ST041A I
 2345-49   "   I 33383      041    I 5234 JADEPR.ST041B I
 2350-54   "   I 35981      042    I 5694 JADEPR.ST042A I
 2350-54   "   I 35981      042    I 5516 JADEPR.ST042B I
 2355-61   "   I 35651      043    I 5595 JADEPR.ST043A I
 2355-61   "   I 35651      043    I 5411 JADEPR.ST043B I
------------------------------------------------------------------------

      THE ABOVE  8 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0056V00    41207    EVENTS
                      TEST READ   OK !
 BACKUP COPY OF REDUC1.G0056V00 :  JADEPR.ARCHIVE.REDUC056
------------------------------------------------------------------------
 2362-65   "   I 34492      044    I 5189 JADEPR.ST044A I
 2362-65   "   I 34492      044    I 4245 JADEPR.ST044B I
 2366-71   "   I 29020      045    I 4350 JADEPR.ST045A I
 TIME LOOP AFTER EVENT 1815 / 2366:1815   TRY AGAIN AFTER REPAIR
 2366-71   "   I 29020      045    I 3842 JADEPR.ST045B I
 2372-76   "   I 19464      046    I 6153 JADEPR.ST046  I
 2377-80   "   I 22401      047    I 7185 JADEPR.ST047  I
THIS TAPE HAS A TIME LOOP AFTER A FEW EVENTS, TRY AGAIN AFTER REPAIR
 2381-85   "   I 29035      048    I 4810 JADEPR.ST048A I
 2381-85   "   I 29035      048    I 4280 JADEPR.ST048B I
------------------------------------------------------------------------

      THE ABOVE  8 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0057V00     40054   EVENTS
                      TEST READ   OK !
 BACKUP COPY OF REDUC1.G0057V00 :  JADEPR.ARCHIVE.REDUC057
------------------------------------------------------------------------
 2386-89   "   I 33922      049    I 6259 JADEPR.ST049A I
 2386-89   "   I 33922      049    I 4346 JADEPR.ST049B I
 2390-98   "   I 38802      050    I 5264 JADEPR.ST050A I
 2390-98   "   I 38802      050    I 6294 JADEPR.ST050B I
 2399-2406 "   I 39491      051    I 6670 JADEPR.ST051A I
 2399-2406 "   I 39491      051    I 6643 JADEPR.ST051B ISKIPS 2403:4472
------------------------------------------------------------------------

      THE ABOVE  6 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0058V00     29961   EVENTS
               TEST READ   OK !!
OBS OBS OBS OBS OBS   THIS TAPE WILL BE REWRITTEN, IT DOES NOT CONTAIN
       COMPLETE DATA FROM TAPES REFORM.G0051

             TO BE FOUND ON TAPE     6628 JADEPR.ST051B I
COPY  G0058V00   RECS LE 29433  -> JADEPR.INTER58     29433  EVENTS
  MERGE INTER58 ,ST051B           -> JADEPR.REDUC158   36061 EVENTS
     TEST READ REDUC158    OK
  COPY REDUC158  INTO   REDUC1.G0058V00;  DONE ;  TEST READ OK
 BACKUP COPY OF REDUC1.G0058V00 :  JADEPR.ARCHIVE.REDUC058
------------------------------------------------------------------------
 2407-13   "   I 38895      052    I 6457 JADEPR.ST052A I SKIP 2408:3372
                            JADEPR.RUN2408 WITH EVENTS 3368 - 3373
 2407-13   "   I 38895      052    I 5308 JADEPR.ST052B ISKIPS 2411:1028
 2414-17   "   I 30297      053    I 4073 JADEPR.ST053A ISKIPS 2415:4950
                                                         SKIPS 2415:4983
 2414-17   "   I 30297      053    I 4151 JADEPR.ST053B I
 2418-20   "   I 12331      054    I 3675 JADEPR.ST054  I
 2421-22   "   I 10656      055    I 2359 JADEPR.ST055  I
 2423-26   "   I 28582      056    I 3071 JADEPR.ST056A I
 2423-26   "   I 28582      056    I 3312 JADEPR.ST056B I
 2427-32   "   I 25884      057    I 2754 JADEPR.ST057A I
 2427-32   "   I 25884      057    I 2091 JADEPR.ST057B I
DIED FROM TIME LOOP AFTER FEW EVENTS IN RUN 2430, TRY AGAIN AFTER REPAIR
------------------------------------------------------------------------

      THE ABOVE 10 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0063V00     28281   EVENTS
                      TEST READ   OK !!
OBS OBS OBS OBS OBS   THIS TAPE WILL BE REWRITTEN, IT DOES NOT CONTAIN
       COMPLETE DATA FROM TAPES REFORM.G0052 AND 0053

             TO BE FOUND ON TAPE     6447 JADEPR.ST052A I
             TO BE FOUND ON TAPE     5290 JADEPR.ST052B I
             TO BE FOUND ON TAPE     4066 JADEPR.ST053A I
COPY  G0063V00   RECS GT  6868  -> JADEPR.INTER63     21413  EVENTS
  MERGE ST052A,52B,53A, INTER63   -> JADEPR.REDUC163   37216  EVENTS
      TEST READ REDUC163   OK
  COPY REDUC163  INTO   REDUC1.G0063V00; DONE;  TEST READ OK
 BACKUP COPY OF REDUC1.G0063V00 :  JADEPR.ARCHIVE.REDUC063
   OBS.!! TAPE JADEPR.REDUC1.G0063V00 GIVES READ ERROR. IT WILL BE
          RECREATED FROM THE BACKUP COPY       J.OLSSON  22.03.80
             RECREATED AND TEST READ   OK !!!     J.O.  28.03.80
------------------------------------------------------------------------
 2433-36       I 33056      058    I 4456 JADEPR.ST058A I
 2433-36       I 33056      058    I 6099 JADEPR.ST058B I
 2437-43       I 44024      059    I 7509 JADEPR.ST059A ISKIPS 2438:3474
 2437-43       I 44024      059    I 7424 JADEPR.ST059B I
 2444-47       I 32020      060    I 4859 JADEPR.ST060A I
 2444-47       I 32020      060    I 5061 JADEPR.ST060B I
------------------------------------------------------------------------

      THE ABOVE  6 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0059V00     35408   EVENTS
             TEST READ   OK, BUT ONE READ ERROR WITH RECOVERY
 OBS OBS OBS OBS OBS
   SOME DATA ON TAPE REFORM.G0060 HAS NOT YET BEEN DATA REDUCED-1
             TO BE FOUND ON TAPE      316 JADEPR.ST060C I
 OBS OBS OBS OBS OBS
COPY  G0059V00   RECS LE 30347  -> JADEPR.INTER59A    30347  EVENTS
COPY  G0059V00   RECS GT 30347  -> JADEPR.INTER59B     5061  EVENTS
  MERGE INTER59A,ST060C,INTER59B  -> JADEPR.REDUC159   35724 EVENTS
      TEST READ  REDUC159  OK
  COPY REDUC159  INTO   REDUC1.G0059V00; DONE;  TEST READ OK
 BACKUP COPY OF REDUC1.G0059V00 :  JADEPR.ARCHIVE.REDUC059
------------------------------------------------------------------------
 2448-54       I 37169      061    I 5942 JADEPR.ST061A I
 2448-54       I 37169      061    I 5636 JADEPR.ST061B I
 2455-60       I 33307      062    I 5979 JADEPR.ST062A I
 2455-60       I 33307      062    I 5522 JADEPR.ST062B I
 2461-65       I 33588      063    I 4767 JADEPR.ST063A I
 2461-65       I 33588      063    I 5204 JADEPR.ST063B I
 2466-72       I 32754      064    I 4468 JADEPR.ST064A I
 2466-72       I 32754      064    I 4801 JADEPR.ST064B I
------------------------------------------------------------------------

      THE ABOVE  8 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0060V00     42319   EVENTS
                      TEST READ   OK !!
 BACKUP COPY OF REDUC1.G0060V00 :  JADEPR.ARCHIVE.REDUC060
------------------------------------------------------------------------
 2473-80       I 34270      065    I 5407 JADEPR.ST065A I
 2473-80       I 34270      065    I 5324 JADEPR.ST065B I
 2481-82       I 12694      066    I 6116 JADEPR.ST069  I
 2484          I  3147      067    I             "      I
 2485-86       I  3764      068    I             "      I
 2487          I  3285      069    I             "      I
 2488-89       I 19089      070    I 5558 JADEPR.ST070  I
 2490-93       I 31582      071    I 4825 JADEPR.ST071A I
 2490-93       I 31582      071    I 3819 JADEPR.ST071B I
 2494-98       I 26434      072    I 2485 JADEPR.ST072A I
 2494-98       I 26434      072    I 3993 JADEPR.ST072B I
------------------------------------------------------------------------

      THE ABOVE  8 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0061V00     37527   EVENTS
               TEST READ   OK !!
 BACKUP COPY OF REDUC1.G0061V00 :  JADEPR.ARCHIVE.REDUC061

------------------------------------------------------------------------
 2499-2503     I 29330      073    I 5419 JADEPR.ST073A I
 2499-2503     I 29330      073    I 5542 JADEPR.ST073B I
 2504-11       I 39406      074    I 7518 JADEPR.ST074A I
 2504-11       I 39406      074    I 6554 JADEPR.ST074B I
 2512-16       I 29185      075    I 5455 JADEPR.ST075A I
 2512-16       I 29185      075    I 5176 JADEPR.ST075B I
------------------------------------------------------------------------

      THE ABOVE  6 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0062V00    35664    EVENTS
                        TEST READ  OK !!
   MERGE G0062 AND ST076   INTO JADEPR.REDUC162   42758 EVENTS
   TEST READ REDUC162   OK !
------------------------------------------------------------------------
 2517-20       I 19389      076    I 7094 JADEPR.ST076  I
LAST BEAM BEAM DATA : RUN 2520 !!!
 2521-26       I 54440      077    I 5704 JADEPR.ST077A I
 2521-26       I 54440      077    I 6510 JADEPR.ST077B I
 2521-26       I 54440      077    I 7557 JADEPR.ST077C I
 2527-30       I 51521      078    I 7338 JADEPR.ST078A I
 2527-30       I 51521      078    I 7597 JADEPR.ST078B I
 2527-30       I 51521      078    I      JADEPR.ST078C I
 2531,32,65    I 24712      079    I      JADEPR.ST079  I
------------------------------------------------------------------------
IMPORTANT NOTICE!!!
 THE FOLLOWING TAPES WILL EVENTUALLY BE REWRITTEN:
       REFORM 221 - 246     WIRING MISTAKES AS WELL AS DL8 MISTAKES
       REFORM 005 - 019:    DL8 MISTAKES     EXCEPT TAPE 011
  REFORMATING HAS ALREADY BEEN CORRECTED FOR THESE DATA AND THE REDUC1
  STEP WILL BE REPEATED WHEN TIME ALLOWS
 1846-69 30.94 I 37018     0221    I 6592 JADEPR.TNEW221I
       TAPE 221 WAS EARLIER FOUND TO GIVE    6477 EVENTS
 1870-75 30.94 I 24647     0222    I 8359 JADEPR.TNEW222I
 1876-80 30.96 I 32742     0223    I10853 JADEPR.TNEW223I
 1881-87 30.98 I 30150     0224    I10336 JADEPR.TNEW224I
 1888-92 31.00 I 30503     0225    I 9526 JADEPR.TNEW225I
 1893-95 31.02 I 18033     0226    I 5447 JADEPR.TNEW226I
 1896-1901 "   I 26689     0227    I 6110 JADEPR.TNEW227I
 1902-05 31.04 I 17824     0228    I 6012 JADEPR.TNEW228I
 1906-09 31.06 I 28531     0229    I 9387 JADEPR.TNEW229I
 1910-12 31.08 I 26045     0230    I 8644 JADEPR.TNEW230I
 1913-21 31.10 I 30569     0231    I 9891 JADEPR.TNEW231I
 1922-31 31.12 I 21952     0232    I 6225 JADEPR.TNEW232I
 1932-35       I 35495     0233    I 3963 JADEPR.TNEW233AI
 1932-35       I 35495     0233    I 3324 JADEPR.TNEW233BI
 OBS OBS TAPE 233 WAS EARLIER F11LHO.SPREFOR. ALL FOLLOWING NUMBERS
  CHANGED BY ONE TO ACCOMODATE THIS TAPE, DOWN TO 245  (COPY OF 244)
 1936-39 31.16 I 24627     0234    I 6082 JADEPR.TNEW234I
 1940-44 31.18 I 23846     0235    I 6904 JADEPR.TNEW235I
 1945-48 31.20 I 28752     0236    I 8187 JADEPR.TNEW236I
 1949-55 31.22 I 33245     0237    I 4654 JADEPR.TNEW237I
       TAPE 237 WAS EARLIER FOUND TO GIVE    4612 EVENTS
 1956-62 31.24 I 31090     0238    I 8591 JADEPR.TNEW238I
 1963-68 31.26 I 31628     0239    I 8847 JADEPR.TNEW239I
 1969-73 31.28 I 34130     0240    I13794 JADEPR.TNEW240I
 1974-83 31.32 I 26637     0241    I 9162 JADEPR.TNEW241I
 1984-95 31.34 I 30097     0242    I 5294 JADEPR.TNEW242AI
 1984-95 31.34 I 30097     0242    I 5021 JADEPR.TNEW242BI
 1996-2012     I 27484     0243    I 3849 JADEPR.TNEW243AI
 1996-2012     I 27484     0243    I 4417 JADEPR.TNEW243BI
    RUNS 1991 - 2007 ARE ID PULSER RUNS, OMITTED IN REDUC1
 2015-24 31.38 I 32666     0244    I10493 JADEPR.TNEW244I
 2025-29 31.40 I 25789     0245    I 8103 JADEPR.TNEW245I
 2030-35 31.42 I 30094     0246    I10065 JADEPR.TNEW246I
 2113-17 30.14 I 23710      005    I 9076 JADEPR.TNEW005I
 2118-22 30.16 I 15874      006    I 5974 JADEPR.TNEW006I TO BE DELETED
 2118-22 30.16 I 15874      006    I 5968 JADEPR.TNEW006AI
NEW006 CANNOT BE READ , REWRITTEN AS TNEW006A
 2123-26 30.18 I 22855      007    I 9367 JADEPR.TNEW007I TO BE DELETED
 2123-26 30.18 I 22855      007    I 4764 JADEPR.TNEW007AI
 2123-26 30.18 I 22855      007    I 4603 JADEPR.TNEW007BI
TNEW007 CANNOT BE READ, WILL BE REWRITTEN AS TNEW007A,B
 2127-30 30.20 I 21055      008    I 7872 JADEPR.TNEW008I
 2131-36 30.24 I 19851      009    I 7776 JADEPR.TNEW009I
 2137     "    I  8329      010    I 2068 JADEPR.TNEW010I
 2145     "    I  2994      012    I 6921 JADEPR.TNEW014I
 2146-47 30.26 I  4634      013    I           "        I
 2148     "    I  9601      014    I           "        I
 2149-51 30.28 I 22386      015    I 8302 JADEPR.TNEW015I
 2152-54 30.30 I 21483      016    I 8286 JADEPR.TNEW016I
 2155-58 30.32 I 20933      017    I 7885 JADEPR.TNEW017I
 2159-61 30.34 I 19976      018    I 6597 JADEPR.TNEW018I
 2162-65  "    I 19890      019    I 6404 JADEPR.TNEW019I
    30/03/82            MEMBER NAME  REDUC80  (JADESR)      TEXT

        *****************************************************
        *                                                   *
        *            FIRST STEP DATA REDUCTION   1980       *
        *                                                   *
        *****************************************************

          THE CURRENT STATUS OF THE REDUCTION IS STORED IN
                JADEPR.JADESR(REDUC)

     THE CORRESPONDING BOOK-KEEPING FOR  1979  IS FOUND IN THE MEMBERS
             REDUC79 AND REDUC179

         REFORMATTED TAPES HAVE THE NAMES F11LHO.JDATA02.REFORM.GXXXXV00
               WHERE XXXX IS THE DATA GENERATION NO. GIVEN BELOW

         REDUCED TAPES HAVE THE NAMES JADEPR.REDUC1.GXXXXV00
               WHERE XXXX IS THE DATA GENERATION NO. GIVEN BELOW

  INTERMEDIATE TAPES HOLDING SMALL AMOUNTS OF REDUC1 OUTPUT DATA WILL
  TEMPORARILY EXIST AND WILL BE MERGED INTO THE PROPER GENERATION GROUP
  TAPES WHEN ENOUGH DATA IS READY. THESE INTERMEDIATE TAPES CARRY THE
  NAMES    JADEPR.STXXXA,B,C   ETC. WHERE XXX IS THE REFORM TAPE NR.
------------------------------------------------------------------------
               I   REFORMATTED     I      REDUCED       I  COMMENTS,ETC.
  RUN     ECM  I EVENTS ; DATA GEN#I EVENTS ; TAPE NAME ISUBMITTED JOBNR
---------------I-------------------I--------------------I---------------
 2517-20  12.0 I 19389      076    I 7094 JADEPR.ST076  I
LAST BEAM BEAM DATA : RUN 2520 !!!
 2521-26  COSM I 54440      077    I 5704 JADEPR.ST077A I
 2521-26    "  I 54440      077    I 6510 JADEPR.ST077B I
 2521-26    "  I 54440      077    I 7557 JADEPR.ST077C I
 2527-30    "  I 51521      078    I 7338 JADEPR.ST078A I
 2527-30    "  I 51521      078    I 7597 JADEPR.ST078B I
 2527-30    "  I 51521      078    I                    I
 2531,32,65 "  I 24712      079    I                    I
------------------------------------------------------------------------
      THE 1980 DATA TAKING STARTS HERE !
------------------------------------------------------------------------
               I   REFORMATTED     I      REDUCED       I  COMMENTS,ETC.
  RUN     ECM  I EVENTS ; DATA GEN#I EVENTS ; TAPE NAME ISUBMITTED JOBNR
---------------I-------------------I--------------------I---------------
2718-32   32.0 I   475       082   I    0  NO TAPE      I
2745-54 MIXED  I 20170       083   I 1765 JADEPR.ST083  I
 2745-47  32.0 I                   I                    I
 2748     33.0 I                   I                    I
 2749     34.0 I                   I                    I
 2750     35.0 I                   I                    I
 2751-54  33.0 I                   I                    I
2755-62 MIXED  I 22011       084   I 3150 JADEPR.ST084  I
 2755     33.0 I                   I                    I
 2756-57  34.8 I                   I                    I
 2758-62  33.0 I                   I                    I
2760      33.0 I  5392       085   I  798       .ST085  I
2763-67    "   I 15576       086   I 2213       .ST086  I
2768-76    "   I 18231       087   I 2475       .ST087  I
2777-81    "   I 20816       088   I 3339       .ST088  I
------------------------------------------------------------------------

      THE ABOVE  6 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0064V00  13740      EVENTS
ORDER  ST083-88            TEST READ OK !!
------------------------------------------------------------------------
2790-91    "   I 13907       089   I 1488       .ST089  I
2792-94    "   I 17838       090   I 2543       .ST090  I
2783-89    "   I  4765       091   I  469       .ST091  I
2795-98    "   I 18001       092   I 2197       .ST092  I
2802-05 MIXED  I  9262       093   I  693       .ST093  I
 RUN 2802 IS STILL 33.0 GEV, THE REST IS 34.0 GEV       I
2806-16   34.0 I 28036       094   I 2603       .ST094  I
2821       "   I  5342       095   I  505       .ST095  I
2822-23    "   I  8028       096   I  692       .ST096  I
2817-2824      I 21351       097   I 1722       .ST097  I
------------------------------------------------------------------------

      THE ABOVE  9 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0065V00    12912    EVENTS
ORDER  ST091,89,90,92-97       TEST READ OK !!!
------------------------------------------------------------------------
2825-2830 MIXEDI 28643       098   I 1829       .ST098  I
 35.0 GEV FROM I                   I                    I
 RUN 2828 ON.  I                   I                    I
2831-34   35.0 I 15658       099   I  778       .ST099  I
2835-38    "   I 33891       100   I 2465       .ST100  I
2839-44    "   I 21906       101   I 1812       .ST101  I
2845-49    "   I 25103       102   I 2640       .ST102  I
2850-52    "   I 18877       103   I 1749       .ST103  I
2853-56    "   I 18514       104   I 1789       .ST104  I
2857-60    "   I 24299       105   I 2217       .ST105  I
------------------------------------------------------------------------

      THE ABOVE  8 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0066V00   15278     EVENTS
ORDER ST098-105          TEST READ OK !
------------------------------------------------------------------------
2861-62  35.0  I 15933         106 I 1516       .ST106  I
2863-68    "   I 19158         107 I 1929       .ST107  I
2869-74    "   I 27684         108 I 2015       .ST108  I
2875-79    "   I 20953         109 I 2416       .ST109  I
2880-88    "   I 31692         110 I 3690       .ST110  I
2886,92    "   I  8494         114 I  751       .ST114  I
2889-94    "   I 24687         111 I 2287       .ST111  I
2895-01    "   I 19823         112 I 1741       .ST112  I
------------------------------------------------------------------------

      THE ABOVE  8 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0067V00   16345     EVENTS
ORDER: 106-10,114,111-12            TEST READ OK !!
------------------------------------------------------------------------
2902-05    "   I 24264         113 I 2191       .ST113  I
2906-08    "   I 23641         115 I 2411       .ST115  I
2909-12    "   I 26527         116 I 2158       .ST116  I
2913-16    "   I 26448         117 I 2358       .ST117  I
2917-22    "   I 29540         118 I 2843       .ST118  I
2926-28    "   I 14442         119 I 1450       .ST119  I
2929-30    "   I  8995         120 I 1044       .ST120  I
------------------------------------------------------------------------

      THE ABOVE  7 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0068V00   14455     EVENTS
ORDER: 113,15-20            TEST READ OK !!
------------------------------------------------------------------------
2923-35    "   I 26788         121 I 2446       .ST121  I
2936-50    "   I 40754         122 I 4569       .ST122  I
2938,47    "   I  4053 (153)   123 I  390 (1947).ST123  I
 2939,48   "   I 10931 (0263)  138 I 1272 (0262).ST138  I
2952-57    "   I 36225         124 I 3113       .ST124  I
2958-60    "   I 20194         125 I 1596       .ST125  I
2961-65    "   I 31369         126 I 2588 (0703).ST126  I
------------------------------------------------------------------------

      THE ABOVE  7 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0069V00    15874    EVENTS
ORDER  121-23,38,24-26            TEST READ OK !!
------------------------------------------------------------------------
2966-71    "   I 30576 (0770)  127 I 3505 (0767).ST127  I
2972-76    "   I 28886 (1737)  128 I 2618       .ST128  I
2977-81    "   I 34243 (1806)  129 I            .ST129  I
2982-89    "   I 29059 (1810)  130 I 2919       .ST130  I
2990-98    "   I 27576         131 I 2885       .ST131  I
------------------------------------------------------------------------

      THE ABOVE  5 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0070V00    15338    EVENTS
ORDER: 127-31          TEST READ OK !!
------------------------------------------------------------------------
2999-03    "   I 34552         132 I 3361       .ST132  I
3004-07    "   I 26676         133 I 2503       .ST133  I
3008-11    "   I 26882 (0317)  134 I 2571 (0475).ST134  I
2985,94    "   I  9271 (1472)  135 I  844       .ST135  I
2995       "   I  7980 (1483)  136 I  633 (1983).ST136  I
3012-16    "   I 29531 (1505)  137 I 2829 (1984).ST137  I
3017-20    "   I 16012 (0122)  140 I 1466 (0989).ST140  I
------------------------------------------------------------------------

      THE ABOVE  7 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0071V00    14207   EVENTS
ORDER: 135,36,32-34,37,40       TEST READ OK !
------------------------------------------------------------------------
      FOR TAPES  ST138,140    SEE ABOVE   (35.0 GEV DATA)
 3037     35.8 I  4193 (0276)  139 I  301 (0273).ST139  I
3037-42   35.8 I 33759 (0134)  141 I 2743 (0990).ST141  I5 RD-ERR.(17EV)
3043-47   35.8 I 28900 (1224)  142 I 2367 (0991).ST142  I
3048-55   35.8 I 29346 (1336)  143 I 2531 (0994).ST143  I
3056-62   35.8 I 31789 (1796)  144 I 2835 (0997).ST144  I
3063-68   35.8 I 34504 (1481)  145 I 2992 (0674).ST145  I
3069-73   35.8 I 29032 (1174)  146 I 2089 (1173).ST146  I
------------------------------------------------------------------------

      THE ABOVE  7 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0072V00   15858     EVENTS
ORDER: 139,41-46               TEST READ OK !
------------------------------------------------------------------------
3074-78   35.8 I 22944 (0161)  147 I 1762 (0677).ST147  I
3079-84   35.8 I 33617 (0050)  148 I 2939 (0680)        IWR.ERR.(ST148)?
               I 33600         148 I 2937 (1375).ST148  I
3085-93   35.8 I 29585 (0028)  149 I 2417 (0681).ST149  I
3094-97   35.8 I 25169 (1172)  150 I 2008 (0690).ST150  I
3098-03   35.8 I 33893 (0432)  151 I 2949 (0692).ST151  I
3104-10   35.8 I 26809 (0345)  152 I 2500 (0208).ST152  I
------------------------------------------------------------------------

      THE ABOVE  6 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0073V00    14573    EVENTS
ORDER: 147-52            TEST READ OK !!
------------------------------------------------------------------------
3111-15   35.8 I 27688 (0764)  153 I 2338 (0512).ST153  I
3116-26   35.8 I 39652 (1677)  154 I 3905 (1969).ST154  I
3122      35.8 I  7988 (1184)  165 I  676 (1142).ST165  I
3127-32   35.8 I 34660 (0399)  155 I 2920 (0514).ST155  I
3133-36 MIXED  I 26719 (0408)  156 I 2682 (0008).ST156  I
 35.8 THROUGH  I                   I                    I
3135. RUN 3136 I                   I                    I
IS 35.0 GEV    I                   I                    I
3137-42   35.0 I 32869 (0418)  157 I 4462 (0050).ST157  I
 JADEPRD7 (0050) READ 32878 EVENTS(1 READ ERROR)
------------------------------------------------------------------------

      THE ABOVE  6 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0074V00    16983    EVENTS
ORDER: 153,154,165,155-57             TEST READ OK !
------------------------------------------------------------------------
                START ENERGY SCAN HERE    35.02  --  35.26  GEV
------------------------------------------------------------------------
3143-49        I 30316 (1815)  158 I 3858 (0904).ST158  I
  ENERGY 35.02   RUNS 3143 - 3150
3147           I  6096 (1187)  166 I  824 (1384).ST166  INO NET
3150-53        I 26827 (0798)  159 I 3423 (1122).ST159  I
  ENERGY 35.04   RUNS 3151 - 3159
3154-58        I 30955 (0711)  160 I 3880 (0054).ST160  I
3160           I  7997 (1189)  167 I  946 (1520).ST167  INO NET
  ENERGY 35.06   RUNS 3160 - 3171
3168           I  5079 (1193)  168 I  629 (1522).ST168  INO NET
------------------------------------------------------------------------

      THE ABOVE  6 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0075V00    13560    EVENTS
ORDER: 158,166,159,160,167-68           TEST READ OK !
------------------------------------------------------------------------
3159-69        I 42805 (1120)  161 I 3711 (0820).ST161  INETID=JADEPRN1
    JADEPRD1 (820) STOPPED AFTER TIME LIMIT: 29988 EVENTS
   "               "     "      "  I 1553 (1278).ST161A I
3170-73        I 26974 (1938)  162 I 3297 (0825).ST162  INETID=JADEPRN2
  ENERGY 35.08   RUNS 3172 - 3184
3174-79        I 34077 (1671)  163 I 3614 (0982).ST163  INETID=JADEPRN3
    JADEPRD3 (982) STOPPED AFTER TIME LIMIT: 30102 EVENTS
   "               "     "      "  I  436 (1294).ST163A I
               I 29326 (0391)  164 I 2443 (0985).ST164  INETID=JADEPRN4
     FOR TAPES 165-68, SEE ABOVE   (RUN NUMBER OUT OF ORDER)
------------------------------------------------------------------------

      THE ABOVE  6 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0076V00    15054    EVENTS
ORDER: 161,61A,62,63,63A,64              TEST READ OK !
------------------------------------------------------------------------
  ENERGY 35.10   RUNS 3185 - 3199
3189-94        I 31451 (1217)  169 I 3312 JOB #976 REMAKES TAPE
3189-94        I 31451 (1217)  169 I 3313 (1893).ST169  I
3195-3200      I 30366 (1218)  170 I 3136 (1207).ST170  INETID=JADEPRN0
  ENERGY 35.12   RUNS 3200 - 3214
3201-05        I 26333 (0834)  171 I 2968 (0739).ST171  INETID=JADEPRN1
3206-11        I 25188 (0837)  172 I 3016 (0744).ST172  INETID=JADEPRN2
3212-18        I 35300 (0841)  173 I 4511 (0747).ST173  INETID=JADEPRN3
  ENERGY 35.14   RUNS 3215 - 3226
------------------------------------------------------------------------

      THE ABOVE  5 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0077V00    16944    EVENTS
ORDER: 169-173            TEST READ OK  !
------------------------------------------------------------------------
3219-23        I 29354 (0848)  174 I 3776 (0748).ST174  INETID=JADEPRN4
3224-28        I 29615 (0852)  175 I 3799 (0749).ST175  INETID=JADEPRN5
  ENERGY 35.16   RUNS 3227 - 3237
3229-34        I 29017 (0972)  176 I 3772 (0959).ST176  INETID=JADEPRN6
  ENERGY 35.18   RUNS 3238 - 3246
3235-40        I 36210 (0975)  177 I 5123 (0963).ST177  INETID=JADEPRN7
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0078V00   16470     EVENTS
ORDER: 174-177            TEST READ OK !
------------------------------------------------------------------------
3241-45        I 26871 (0979)  178 I 3476 (0967).ST178  INETID=JADEPRN8
  ENERGY 35.20   RUNS 3247 - 3258
3246-51        I 27300 (1716)  179 I 3412 (1710).ST179  INETID=JADEPRN9
3252-57        I 28693 (1719)  180 I 3734 (1715).ST180  INETID=JADEPRN0
  ENERGY 35.22   RUNS 3259 - 3272
3258-65        I 33194 (1724)  181 I 4411 (1720).ST181  INETID=JADEPRN1
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0079V00    15033    EVENTS
ORDER: 178-181           TEST READ OK !
------------------------------------------------------------------------
3266-73        I 25313 (0194)  182 I 3348 (0179).ST182  INETID=JADEPRN2
  ENERGY 35.24   RUNS 3273 - 3290
3274-78        I 32677 (0198)  183 I 3879 (0188).ST183  INETID=JADEPRN3
3272,79-86     I 25314 (0805)  184 I 3158 (0814).ST184  INETID=JADEPRN4
3287-8,91-3    I 28216 (1799)  185 I 3443 (1813).ST185  INETID=JADEPRN5
3289-90        I  8693 (1838)  186 I 1010 (1843).ST186  INETID=JADEPRN6
------------------------------------------------------------------------

      THE ABOVE  5 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0080V00    14838    EVENTS
ORDER: 182-186                TEST READ OK !
------------------------------------------------------------------------
  ENERGY 35.26   RUNS 3291 - 3296
C
C   WE DID IT AGAIN !!!   DATA STILL SITTING ON DISK, NOT DUMPED !!
C
3294-96        I 16447 (1893)  187 I 2074 (0008).ST187  INETID=JADEPRN7
3311-18COSMICS I 41735 (1510)  188 I COSMICS NO REDUC.  I
3319-24COSMICS I 47165 (0478)  189 I COSMICS NO REDUC.  I
3313-14COSMICS I 11148 (0713)  190 I COSMICS NO REDUC.  I
C
C   TWO SPECIAL NORD 50 OVERFLOW EVENT  TAPES BELOW.
C
2751-2840 (EVENT 2982)  READ  1097 EVENTS FROM F11LHO.OVCONCN
                        WROTE  807 EVENTS TO   JADEPR.LHOOVR
2840 (EVENT 2983)-3184  READ  2423 EVENTS FROM F11LHO.OVCONCN
                        WROTE 2082 EVENTS TO   JADEPR.LHOOVR1
------------------------------------------------------------------------

         RUNNING PERIOD STARTING 80/04/21

---
       ENERGY  35.20  RUNS 3325 - 3342
---
3319-24 TEST   I 65075 (1398)  195 I    0 (0023).ST195  INETID=JADEPRN5
3325,31-33     I 17272 (1401)  196 I 1811 (0025).ST196  INETID=JADEPRN6
3334-39        I 30002 (1714)  197 I 3196 (1849).ST197  INETID=JADEPRN7
3340-42        I 21196 (1310)  198 I 1860 (1853).ST198  INETID=JADEPRN8
---
       ENERGY  35.22  RUNS 3343 - 3361
---
3343-47        I 25930 (1304)  199 I 2382 (1098).ST199  INETID=JADEPRN9
3348-52        I 21230 (1860)  200 I 2102 (1866).ST200  INETID=JADEPRN0
3353-57        I 23361 (1693)  201 I 2564 (1696).ST201  INETID=JADEPRN1
------------------------------------------------------------------------

      THE ABOVE  7 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0081V00    15989    EVENTS
ORDER: 187,196-201          TEST READ    OK !!
------------------------------------------------------------------------
3358-62        I 19478 (1697)  202 I 2071 (1703).ST202  INETID=JADEPRN2
---
       ENERGY  35.24  RUNS 3362 - 3376
---
3363,67-9      I 24209 (1702)  203 I 2661 (1704).ST203  INETID=JADEPRN3
3370-73        I 27985 (1707)  204 I 2929 (1723).ST204  INETID=JADEPRN4
3374-77        I 26440 (1709)  205 I 2664 (1725).ST205  INETID=JADEPRN5
---
       ENERGY  35.26  RUNS 3377 - 3389
---
3378-83        I 25148 (1711)  206 I 2576 (1726).ST206  INETID=JADEPRN6
3384-87        I 22007 (0617)  207 I 2359 (1734).ST207  INETID=JADEPRN7
------------------------------------------------------------------------

      THE ABOVE  6 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0082V00    15260    EVENTS
ORDER: 202-207               TEST READ  OK !!
------------------------------------------------------------------------
3388-91        I 31952 (0140)  208 I 3573 (1735).ST208  INETID=JADEPRN8
---
       ENERGY  35.28  RUNS 3390 - 3398
---
3392-95        I 29029 (0142)  209 I 3346 (0145).ST209  INETID=JADEPRN9
3396-98        I 20173 (0143)  210 I 2212 (0147).ST210  INETID=JADEPRN0
---
       ENERGY  35.30  RUNS 3399 - 3410
---
3399-3402      I 31922 (0185)  211 I 3012 (0148).ST211  INETID=JADEPRN1
3403-3407      I 31963 (0366)  212 I 3150 (0196).ST212  INETID=JADEPRN2
------------------------------------------------------------------------

      THE ABOVE  5 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0083V00     15293   EVENTS
ORDER: 208-212            TEST READ   OK !!
------------------------------------------------------------------------
3409-11        I 24345 (0936)  213 I 2588 (0197).ST213  INETID=JADEPRN3
---
       ENERGY  35.32  RUNS 3411 - 3422
---
3412-16*DELETEDI 32711 (0941)  214 I 3302 (0221).ST214  INETID=JADEPRN4
3412-16**REMADEI 32711 (0941)  214 I 3302 (0553).ST214A INETID=JADEPRN4
3417-21*DELETEDI 19514 (1357)  215 I 1646 (1343).ST215  INETID=JADEPRN5
3417-21**REMADEI 19514 (1357)  215 I 1645 (0555).ST215A INETID=JADEPRN5
3422-25        I 25120 (0278)  216 I 2350 (1374).ST216  INETID=JADEPRN6
  MINOWA COMPLAINS ABOUT TAPE ST216, PLEASE CHECK !!!
     TAPE 216 REMADE  ********************
---
       ENERGY  35.34  RUNS 3423 - 3429
---
3426-30        I 30949 (0280)  217 I 2880 (0552).ST217  INETID=JADEPRN7
---
       ENERGY  35.376  RUNS 3430 - 3432
---
---
       ENERGY  35.34  RUNS 3433 - 3439
---
3431-37        I 26074 (0282)  218 I 2463 (0293).ST218  INETID=JADEPRN8
3438-41        I 25811 (0431)  219 I 2234 (0635).ST219  INETID=JADEPRN9
            REDUC TAPE 219 REMADE , NOW CONTAINS MORE EVENTS
---
       ENERGY  35.36  RUNS 3440 - 3456
---
3442-48        I 31533 (0434)  220 I 2781 (0638).ST220  INETID=JADEPRN0
------------------------------------------------------------------------

      THE ABOVE  8 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0084V00   20243     EVENTS
ORDER: 213-220              TEST READ OK !

------------------------------------------------------------------------
3449-51        I 18773 (0435)  221 I 1880 (0641).ST221  INETID=JADEPRN1
3452-56        I 26170 (0436)  222 I 2027 (0645).ST222  INETID=JADEPRN2
---
       ENERGY  35.38  RUNS 3457 - 3471
---
3457-59        I 23099 (0445)  223 I 2118 (0444).ST223  INETID=JADEPRN3
3460-63        I 25060 (0556)  224 I 2077 (0538).ST224  INETID=JADEPRN4
3464-67        I 25657 (0561)  225 I 2378 (0540).ST225  INETID=JADEPRN5
3468-71        I 27227 (1672)  226 I 2546 (1674).ST226  INETID=JADEPRN6
---
       ENERGY  35.40  RUNS 3472 - 3491
---
3472-74        I 23979 (0588)  227 I 1905 (0197).ST227  INO NET
------------------------------------------------------------------------

      THE ABOVE  7 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0085V00    14931    EVENTS
ORDER: 221-227           TEST READ OK !

------------------------------------------------------------------------
3475-78        I 21644 (1967)  228 I 1521 (1978).ST228  INETID=JADEPRN8
3479-83        I 25842 (1968)  229 I 1652 (1983).ST229  INETID=JADEPRN9
3484-89        I 25182 (1970)  230 I 1986 (1985).ST230  INETID=JADEPRN0
3490-93        I 28676 (1971)  231 I 2541 (1988).ST231  INETID=JADEPRN1
---
       ENERGY  35.42  RUNS 3492 - 3503
---
3494-97        I 30319 (1975)  232 I 2891 (1989).ST232  INETID=JADEPRN2
3498-3501      I 23955 (1814)  233 I 2518 (1803).ST233  INETID=JADEPRN3
3502-05        I 23674 (1815)  234 I 2145 (1804).ST234  INETID=JADEPRN4
------------------------------------------------------------------------

      THE ABOVE  7 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0086V00    15254    EVENTS
ORDER: 228-234            TEST READ OK !

------------------------------------------------------------------------
---
       ENERGY  29.90  RUNS 3504 - 3518
---
3504,06-10,12,13 43503 (1817)  235 I 4319 (1806).ST235  INETID=JADEPRN5
3514-17        I 29225 (0788)  236 I 3759 (0792).ST236  INETID=JADEPRN6
---
       ENERGY  29.92  RUNS 3519 - 3530
---
3518-23        I 30948 (0373)  237 I 3740 (0480).ST237  INETID=JADEPRN7
3524-28        I 32963 (0380)  238 I 3702 (0485).ST238  INETID=JADEPRN8
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0087V00    15520    EVENTS
ORDER: 235-238            TEST READ OK !
------------------------------------------------------------------------
---
       ENERGY  29.94  RUNS 3531 - 3540
---
3529-34        I 30696 (0450)  239 I 3960 (0492).ST239  INETID=JADEPRN9
3488           I  6999 (0685)  240 I  556 (0889).ST240  INETID=JADEPRN0
3535-40        I 44066         241 I 5540 (0479).ST241  INETID=JADEPRN1
3535-40        I 44066         241 I  294 (0479).ST241A INETID=JADEPRN1
TDC TESTS      I           GAP     I                    I
TEST,NO REDUC  I            GAP    I                    I
---
       ENERGY  36.60  RUNS 3561 - 3589
---
3561-67,3539   I 32703 (0730)  244 I 3210 (1780).ST244  INETID=JADEPRN4
3568-72        I 26976 (0737)  245 I 2088 (1785).ST245  INETID=JADEPRN5
------------------------------------------------------------------------

      THE ABOVE  6 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0088V00   15648     EVENTS
ORDER: 239-241,41A,44,45
------------------------------------------------------------------------
3573-77        I 23094 (1306)  246 I 2015 (1578).ST246  INETID=JADEPRN6
3578-85        I 30686 (1267)  247 I 2911 (1581).ST247  INETID=JADEPRN7
3586-91        I 28394 (0432)  248 I 3029 (1585).ST248  INETID=JADEPRN8
---
       ENERGY  29.90  RUNS 3590 - 3606
---
3592-95        I 31972 (0458)  249 I 3514 (1714).ST249  INETID=JADEPRN9
3596-99        I 24982 (0460)  250 I 2570 (1729).ST250  INETID=JADEPRN0
3600-04        I 28759         251 I 2610 (1250).ST251  INETID=JADEPRN1
------------------------------------------------------------------------

      THE ABOVE  6 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0089V00     16649   EVENTS
ORDER: 246-251             TEST READ OK !
------------------------------------------------------------------------
3605-10        I 31797 (1892)  252 I 3872 (1258).ST252  INETID=JADEPRN2
---
       ENERGY  36.60  RUNS 3608 - 3614
---
3611-14        I 23357 (0131)  253 I 2463 (1274).ST253  INETID=JADEPRN3
---
       ENERGY  35.44  RUNS 3615 - 3629
---
3615-18        I 28530 (0133)  254 I 3110 (0741).ST254  INETID=JADEPRN4
3619-22        I 31657 (0134)  255 I 3615 (0741).ST255  INETID=JADEPRN5
    ******** TAPE 255 HAS BEEN REMADE
3623-26        I 22698 (0146)  001 I 2514 (0147).ST001  INETID=JADEPRN1
------------------------------------------------------------------------

      THE ABOVE  5 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0090V00     15574   EVENTS
ORDER: 252-255,001          TEST READ  OK !
------------------------------------------------------------------------
3627-32        I 25600         002 I 2888 (0160).ST002  INETID=JADEPRN2
---
       ENERGY  35.46  RUNS 3630 - 3642
---
3633-38        I 36413         003 I 4518 (1089).ST003  INETID=JADEPRN3
3639-42        I 25836         004 I 3273 (1101).ST004  INETID=JADEPRN4
3643-46        I 31559         005 I 3971 (1102).ST005  INETID=JADEPRN5
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0091V00    14650    EVENTS
ORDER: 002-005     ERROR S137 BY REWINDING, DELETED, TO BE REMADE
                      NOW REMADE
------------------------------------------------------------------------
3647-50        I 27596         006 I 2949 (1106).ST006  INETID=JADEPRN6
3651-58        I 35444         007 I 3963 (1111).ST007  INETID=JADEPRN7
---
       ENERGY  35.48  RUNS 3643 - 3654
---
3659-68        I 24598         008 I 2531 (1287).ST008  INETID=JADEPRN8
---
       ENERGY  35.50  RUNS 3656 - 3676
---
3669-74        I 39409         009 I 3830 (1290).ST009  INETID=JADEPRN9
3675-79        I 29803         010 I 2828 (0579).ST010  INETID=JADEPRN0
---
       ENERGY  35.52  RUNS 3677 - 3698
---
------------------------------------------------------------------------

      THE ABOVE  5 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0092V00    16101    EVENTS
ORDER: 006-010             TEST READ  OK !
------------------------------------------------------------------------
3680-83        I 25219         011 I 2471 (0847).ST011  INETID=JADEPRN1
3684-91        I 36751         012 I 3185 (1175).ST012  INETID=JADEPRN2
3693-99        I 30647         013 I 3318 (0440).ST013  INETID=JADEPRN3
3692           I  1551 (1499)  014 I  150 (1127).ST014  INETID=JADEPRN4
  TAPE ST014 OK. NO EVENTS FROM THE OLD RUN. REDUC SKIPPED FIRST 12 GOOD
  EVENTS
---
       ENERGY  35.54  RUNS 3699 - 3714
---
3700-04        I 30473 (1505)  015 I 3426 (1129).ST015  INETID=JADEPRN5
3705-13        I 28580 (1507)  016 I 3462 (1133).ST016  INETID=JADEPRN6
------------------------------------------------------------------------

      THE ABOVE  6 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0093V00   16012     EVENTS
ORDER: 011-016            TEST READ  OK !
------------------------------------------------------------------------
3714-17        I 27985 (0267)  017 I 3496 (0273).ST017  INO NET
---
       ENERGY  35.56  RUNS 3715 -
---
3718-22        I 32979         018 I 4146 (0270).ST018  INO NET
3723-27        I 26047 (1336)  019 I 3257 (0285).ST019  INETID=JADEPRN9
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES WILL BE   COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0094V00     10899   EVENTS
ORDER: 017-019
------------------------------------------------------------------------
****
****     N.B. TAPES .ST020 THROUGH .ST033 WERE WRITTEN WITH OLD T0S.
****          THE DATA WILL BE RECALIBRATED AND PATTERN RECOGNITION
****          WILL BE RERUN WITHOUT REPEATING EVENT SELECTION.
****
3728-3739      I 29822 (1895)  020 I 6446 (1907).ST020  NO NET
                 RECALIBRATED      I 6446 ( 591).ST020R
3740-3745      I 36980 (0764)  021 I 5927 (0761).ST021  TIME LIMIT
                 RECALIBRATED      I 5927 (1429).ST021R
               I  3410             I  512 (1822).ST021A
                 RECALIBRATED      I  512 (1438).ST021AR
3746-3752      I 28181 (0262)  022 I 3871 (1732).ST022  INETID=JADEPRN2
                 RECALIBRATED      I 2854 (1816).ST022R
               + RECALIBRATED      I 1017 (1909).ST022S
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0095V00  AND RECALIBRATED
                        16756 EVENTS
------------------------------------------------------------------------
3753-3769      I 35982 (1734)  023 I 5758 (0303).ST023  INETID=JADEPRN3
                 RECALIBRATED      I 4817 (1819).ST023R
               + RECALIBRATED      I  941 (1913).ST023S
3770-3774      I 26476 (1737)  024 I 3886 (0332).ST024  INETID=JADEPRN4
                 RECALIBRATED      I 3274 (1820).ST024R
               + RECALIBRATED      I  612 (1915).ST024S
3775-3783      I 25652 (1746)  025 I 4218 (1739).ST025  INETID=JADEPRN5
                 RECALIBRATED      I 4090 (1826).ST025R
               + RECALIBRATED      I  128 (1919).ST025S
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0096V00   AND
                     13862 EVENTS              RECALIBRATED
------------------------------------------------------------------------
3784-3791      I 29276 (1748)  026 I 5163 (1741).ST026  INETID=JADEPRN6
                 RECALIBRATED      I 5163 (1834).ST026R
3792-96(EV 100)I       (1755)  027 I 4064 (1742).ST027  INETID=JADEPRN7
                 RECALIBRATED      I 4064 (1836).ST027R
3796(EV 101 ON)I               027 I  906 (0814).ST027A NO NET
                 RECALIBRATED      I  906 (1841).ST027AR
3797-3802      I 32666 (0074)  028 I 5434 (1752).ST028  INETID=JADEPRN8
                 RECALIBRATED      I 2336 (1039).ST028S (28 R WAS BAD.)
                 RECALIBRATED      I 3098 (0604).ST028T
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0097V00
                    15567 EVENTS
------------------------------------------------------------------------
3761-3762      I  7671 (1489)  029 I 1146 (1754).ST029  INETID=JADEPRN9
                 RECALIBRATED      I 1146 (1044).ST029R
3768           I  7989 (1493)  030 I 1234 (1756).ST030  INETID=JADEPRN0
                 RECALIBRATED      I 1234 (1046).ST030R
3803-3808      I 28099 (1499)  031 I 4477 (1551).ST031  INETID=JADEPRN1
                 RECALIBRATED      I 4477 (0184).ST031R
3809-3821      I 29410 (0173)  032 I 4859 (0166).ST032  INETID=JADEPRN2
*** TAPE 32 REWRITTEN BECAUSE OF TAPE ERROR. WILL HAVE NEW T0S.
3822-3826      I 27197 (1506)  033 I 4390 (1572).ST033  INETID=JADEPRN3
                 RECALIBRATED      I 4390 (0188).ST033R
****
****     N.B. TAPES .ST020 THROUGH .ST033 WERE WRITTEN WITH OLD T0S.
****          THE DATA WILL BE RECALIBRATED AND PATTERN RECOGNITION
****          WILL BE RERUN WITHOUT REPEATING EVENT SELECTION.
****
------------------------------------------------------------------------

      THE ABOVE  5 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0099V00
                     16106 EVENTS
------------------------------------------------------------------------
3827-3835      I 27244 (0610)  034 I 4530 (0641) ST034  INETID=JADEPRN4
3836-3846      I 33822 (0209)  035 I 5991 (0207) ST035  INETID=JADEPRN5
------------------------------------------------------------------------

      THE ABOVE  2 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0098V00   10521 EVENTS

------------------------------------------------------------------------
****
****     WARNING. THE TAPES FROM 36 TO 42 CONTAIN OLD RUNS MIXED IN.
****
3847-3853      I 31207 (1892)  036 I 5256 (1901) ST036  INO NET
3854-3857      I 26907 (1898)  037 I 4586 (1928) ST037  INETID=JADEPRN7
3858-3861,#4484I 32868 (0608)  038 I 2769 (0574) ST038  INETID=JADEPRN8
3861,#4485-3864,#6047 ST038 INCOMPLI 2372 (1825) ST038A INO NET
3864,#6045 ON  I     ST038A INCOMPLI  362 (1975) ST038B INO NET
------------------------------------------------------------------------

      THE ABOVE  5 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0100V00
          AND OLD RUNS REMOVED     15344 EVENTS
------------------------------------------------------------------------
3865-3870      I 22755 (0610)  039 I 3875 (0576) ST039  INETID=JADEPRN9
3871-3875      I 26959 (0618)  040 I 3937 (0586) ST040  INETID=JADEPRN0
3876-3880      I 28276 (0300)  041 I 3875 (1042) ST041  INETID=JADEPRN1
3881-3885      I 31031 (1234)  042 I 4203 (0179) ST042  INO NET
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0101V00
          AND OLD RUNS REMOVED      15886 EVENTS
------------------------------------------------------------------------
3886-3888      I 23984 (0611)  043 I 3525 (0307) ST043  INETID=JADEPRN3
3889-3893      I 31835 (0374)  044 I 5274 (0311) ST044  INETID=JADEPRN4
3894-3897      I 25989 (0376)  045 I 3304 (0312) ST045  INETID=JADEPRN5
------------------------------------------------------------------------
  ******  MERGED TAPE SCRATCHED AND WILL BE REMADE *********
      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0102V00
                        12103 EVENTS
------------------------------------------------------------------------
3898-3901      I 25148 (1514)  046 I 3368 (1988) ST046  INO NET
3902-3906      I 29051 (0381)  047 I 3533 (0333) ST047  INETID=JADEPRN7
3907-3910      I 20411 (1242)  048 I 2643 (1503) ST048  INO NET
3911-3914      I 25282 (0386)  049 I 3266 (0032) ST049  INO NET
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0103V00
                        12810 EVENTS
------------------------------------------------------------------------
3915-3918      I 25996 (1196)  050 I 3424 (0818) ST050  INETID=JADEPRN0
3919-3925      I 26154 (1199)  051 I 3709 (0821) ST051  INETID=JADEPRN1
3926-3929      I 29562 (1204)  052 I 3753 (0033) ST052  INO NET
3932-3936      I 26985 (1208)  053 I 2992 (0970) ST053  INETID=JADEPRN3
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0104V00
                        13878 EVENTS
------------------------------------------------------------------------
C---
C---     THE FOLLOWING THREE TAPES, FOR WHICH THE OUTPUTS WERE LOST,
C---     WILL BE SCRATCHED AND RECATALOGED BY SPECIAL LOW PRIORITY
C---     JOBS F11LHOSP, NUMBERS 821, 753 AND 760.
C---
3937-3939      I 26571 (0701)  054 I 3764 (0821) ST054  INO NET
3940,42-45     I 31342 (0084)  055 I 4806 (1796) ST055  INO NET
3946-3948      I 21152 (0720)  056 I 3545 (0760) ST056  INO NET
C---
C---     THE ABOVE THREE TAPES, FOR WHICH THE OUTPUTS WERE LOST,
C---     WILL BE SCRATCHED AND RECATALOGED BY SPECIAL LOW PRIORITY
C---     JOBS F11LHOSP, NUMBERS 821, 753 AND 760.
C---
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0105V00
                   12115 EVENTS
------------------------------------------------------------------------
3949-3958      I 32373 (0227)  057 I 5349 (0240) ST057  INETID=JADEPRN7
3959-3970      I 29907 (0229)  058 I 4397 (0241) ST058  INETID=JADEPRN8
------------------------------------------------------------------------

      THE ABOVE  2 REDUCED TAPES HAS BEEN  COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0106V00
                      9746 EVENTS
------------------------------------------------------------------------
3971-3982      I 23470 (0235)  059 I 6005 (0243) ST059  INETID=JADEPRN9
3983-3989      I 28061 (1619)  060 I 6208 (1614) ST060  INO NET
------------------------------------------------------------------------

      THE ABOVE  2 REDUCED TAPES HAS BEEN  COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0107V00
                        12213 EVENTS
------------------------------------------------------------------------
                       *******      NOTE NEW NET NAMES ********
3990-3994      I 29964 (1646)  061 I 5565 (1654) ST061  INETID=JADEPRX1
3995-4003      I 29634 (1649)  062 I 5384 (1656) ST062  INETID=JADEPRX2
3924           I  7998 (0624)  063 I 1101 (0613) ST063  INETID=JADEPRX3
3933           I  5001 (0677)  064 I  504 (1183) ST064  INO NET
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES HAS BEEN  COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0108V00
                     12554 EVENTS
------------------------------------------------------------------------
4004-4010      I 34525 (1157)  065 I 6825 (1148) ST065  INETID=JADEPRX5
4011-4017      I 37095 (1159)  066 I 6609 (1150) ST066  INETID=JADEPRX6
------------------------------------------------------------------------

      THE ABOVE  2 REDUCED TAPES HAS BEEN  COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0109V00
                     13434 EVENTS
------------------------------------------------------------------------
4018-4023      I 31809 (1613)  067 I 5761 (0050) ST067  INO NET
4024-4029      I 36042 (0684)  068 I10830 (1237) ST068  INO NET
------------------------------------------------------------------------

      THE ABOVE  2 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0110V00
                     16591 EVENTS
------------------------------------------------------------------------
4038           I  7997 (1875)  069 I 1436 (0010) ST069  INO NET
4030-4033      I 28868 (0679)  070 I 5174 (0014) ST070  INO NET
4034-4045      I 33509 (0684)  071 I 5229 (0023) ST071  INO NET
4046-4051      I 27536 (1283)  072 I 3216 (0105) ST072  INO NET
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0111V00
                     15055 EVENTS
------------------------------------------------------------------------
4052-4056      I 26654 (1846)  073 I 4507 (0968) ST073  INO NET
4057-4063      I 26535 (1851)  074 I 4560 (0953) ST074  INO NET
4064-4069      I 31560 (1853)  075 I 5490 (0957) ST075  INO NET
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0112V00
                         14557 EVENTS
------------------------------------------------------------------------
4070-4073      I 28495 (1916)  076 I 4625 (0492) ST076  INO NET
4071-4077      I 26515 (1920)  077 I 4332 (0959) ST077  INO NET
4078-4081      I 30509 (1923)  078 I 4872 (0960) ST078  INO NET
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0113V00
                         13829 EVENTS
------------------------------------------------------------------------
4082-4085      I 29153 (1924)  079 I 4813 (0961) ST079  INO NET
4086-4089      I 29739 (1942)  080 I 5117 (0502) ST080  INO NET
4090-4093      I 25343 (1946)  081 I 4182 (0807) ST081  INO NET
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0114V00
                       14112 EVENTS
------------------------------------------------------------------------
4094-4098      I 30997 (1947)  082 I 4904 (0641) ST082  INO NET
4099-4102      I 26542 (1948)  083 I 3986 (0642) ST083  INO NET
4103-4108      I 30234 (1949)  084 I 4832 (0644) ST084  INO NET
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0115V00
                   13722 EVENTS
------------------------------------------------------------------------
4109-4113      I 26886 (0947)  085 I 4255 (1843) ST085  INO NET
4114-18,23     I 29382 (0602)  086 I 4713 (0061) ST086  INO NET
4124-4127      I 27457 (0604)  087 I 4117 (0578) ST087  INO NET
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0116V00
                    13085 EVENTS
------------------------------------------------------------------------
4128-4133      I 32649 (0605)  088 I 4819 (0584) ST088  INO NET
4134-4139      I 25318 (0413)  089 I 3625 (1610) ST089  INO NET
4140-4146      I 28257 (0436)  090 I 4658 (0956) ST090  INO NET
4104           I  4192 (0739)  091 I  715 (1210)        INO NET
4165           I  7998 (0743)  092 I 1115 (1213)        INO NET
4168           I   583 (0746)  093 I   85 (0593)        INO NET
4167           I  7997 (0749)  094 I 1307 (1218)        INO NET
------------------------------------------------------------------------

      THE ABOVE  7 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0117V00
                        16324 EVENTS
------------------------------------------------------------------------
4147-4155      I 26339 (0756)  095 I 4207 (1220)        INO NET
4156-4173      I 35230 (0758)  096AI 4966 (0983)        INO NET
 %%%%%%%  TAPE ST096 CANNOT BE READ , REDUC JOB RESUBMITTED JOB 983
          **** DOES NOT EXIST  097 I  **********        INO NET
4187-4189      I 14135 (1099)  098 I 2161 (0209)        INO NET
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0118V00
                     11334 EVENTS
------------------------------------------------------------------------
4174-77,83,84  I 31325 (0046)  099 I 3622 (1046)        INO NET
4185-4193      I 19885 (1105)  100 I 3162 (0213)        INO NET
4203-07        I 25908 (0049)  101 I 4145 (1049)        INO NET
4194-96        I  3974 (1015)  102 I  476 (0545)        INO NET
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES HAS BEEN  COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0119V00
                     11405 EVENTS
------------------------------------------------------------------------
4198           I  7992 (0142)  103 I 1313 (1061)        INO NET
4208-13        I 27797 (0144)  104 I 4121 (1050)        INO NET
4214-18        I 29654 (0145)  105 I 4491 (1056)        INO NET
4219-21        I 18179 (0994)  106 I 2057 (0291) INCOMPLINO NET
4219-21        I 18179 (0994)  106 I  206 (0583) ST106A  NO NET
------------------------------------------------------------------------

      THE ABOVE  5 REDUCED TAPES HAS BEEN  COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0120V00
                       12188 EVENTS
------------------------------------------------------------------------
4222-27        I 32701 (0997)  107 I 4749 (0153)        INO NET
4228-32        I 28143 (0999)  108 I 4465 (0299)        INO NET
4233-35        I 22193 (1000)  109 I 3464 (0595)        INO NET
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0121V00
                    12678 EVENTS
------------------------------------------------------------------------
4236-42        I 34512 (1002)  110 I 4656 (0605)        INO NET
4243-47        I 21462 (1242)  111 I 3697 (0609)        INO NET
4248-51        I 30336 (0286)  112 I 4364 (0619)        INO NET
4252-57        I 25710 (0493)  113 I 2544 (1813)        INO NET
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES HAS BEEN  COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0122V00
                         15261 EVENTS
------------------------------------------------------------------------
4258-61        I 30544 (0497)  114 I 4083 (0814)        INO NET
4262-65        I 25327 (0635)  115 I 4052 (0425)        INO NET
4266-69        I 27969 (0725)  116 I 4365 (0432)        INO NET
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0123V00
                    11349 EVENTS      EXCEPT RUNS 4269-79 SEE NOTE BELOW
------------------------------------------------------------------------
4270-73        I 25610 (0315)  117 I 3931 (1651)        INO NET
4274-81        I 31784 (0316)  118 I 5228 (1652)        INO NET
4282-85        I 28611 (1639)  119 I 4191 (1685)        INO NET
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0124V00
                     5488 EVENTS    EXCEPT RUNS 4269-79 SEE NOTE BELOW
------------------------------------------------------------------------
**********************************************************************
        RUNS 4269 - 4279  HAVE SCRAMBLED LG ADCS AND ARE CONTAINED
                     ON THE MERGED TAPE
                JADEPR.ADC.MESS1
                      9013 EVENTS
***********************************************************************
------------------------------------------------------------------------
4286-92        I 31638 (1936)  120 I 5008 (1933)        INETID=JADEPRN0
4293-97        I 26504 (0945)  121 I 4260 (0981)        INETID=JADEPRN1
4298-01        I 30587 (0948)  122 I 4559 (0983)        INETID=JADEPRN2
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0125V00
                    13827 EVENTS
------------------------------------------------------------------------
4302-04        I 23971 (1217)  123 I 3351 (1224)        INETID=JADEPRN3
C---
C---             NEW VERSION OF TRGCHK FROM RUN 4305 ON. SEE #START.
C---
4305-11        I 36590 (0972)  124 I 5195 (1557)        INETID=JADEPRN4
4313           I  7998 (0296)  125 I 1337 (1650)        INETID=JADEPRN5
4312,14-19     I 28514 (0336)  126 I 4050 (0285)        INETID=JADEPRN6
4320-23        I 28042 (1553)  127 I 4070 (1560)        INETID=JADEPRN7
------------------------------------------------------------------------

      THE ABOVE  5 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0126V00
                  JOB 1576
------------------------------------------------------------------------
4324-31        I 34457 (0449)  128 I 5454 (1672)        INETID=JADEPRN8
4332-36        I 26704 (0251)  129 I 3999 (0456)        INETID=JADEPRN9
4337-40        I 27640 (0453)  130 I 4075 (0457)        INETID=JADEPRN0
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0127V00
                    13524 EVENTS
------------------------------------------------------------------------
4341-45        I 28162 (0454)  131 I 4512 (0460)        INETID=JADEPRN1
4346-49        I 31404 (0242)  132 I 4886 (0498)        INETID=JADEPRN2
4350-55        I 29712 (0245)  133 I 4531 (1885)        INO NET
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0128V00
                   13929 EVENTS
------------------------------------------------------------------------
    -4358 6804 I 22360 (0248)  134 I 3523 (0267)        INETID=JADEPRN4
  REFORM TAPE 134 WAS INCOMPLETE DUE TO READ ERROR. SCRATCH, REWRITE.
4358 6805-     I 23561 (1949)  134 I  198 (1893).ST134A INETID=JADEPRN4
4359-62        I 27434 (1953)  135 I 4893 (0971)        INO NET
4363-69        I 34480 (1955)  136 I 5239 (1917)        INETID=JADEPRN6
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0129V00
                    13853 EVENTS
------------------------------------------------------------------------
4370-73        I 24895 (1976)  137 I 3798 (1971)        INETID=JADEPRN7
4374-78        I 29851 (0757)  138 I 4672 (1133)        INO NET
4379-83        I 24705 (0781)  139 I 3363 (0786)        INETID=JADEPRN9
4384-87        I 27202 (0838)  140 I 4051 (0831)        INETID=JADEPRN0
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0130V00
                   15884 EVENTS
------------------------------------------------------------------------
4388-92        I 32114 (0238)  141 I 5191 (0259)        INETID=JADEPRN1
4393-98        I 25041 (0242)  142 I 4069 (0269)        INETID=JADEPRN2
4399-03        I 25235 (0246)  143 I 3786 (0274)        INETID=JADEPRN3
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0131V00
                       13046 EVENTS
------------------------------------------------------------------------
4404-07        I 28984 (1579)  144 I 3935 (1634)        INETID=JADEPRN4
4408-19        I 25727 (1951)  145 I 3983 (1952)        INETID=JADEPRN5
4422-27        I 28163 (0804)  146 I 3061 (1206)        INO NET
4428-34        I 24954 (0807)  147 I 4695 (0822)        INETID=JADEPRN7
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0132V00
                    10673 EVENTS   EXCEPT RUNS 4427-47 SEE NOTE BELOW
------------------------------------------------------------------------
4435-38        I 27221 (0524)  148 I 5048 (0528)        INETID=JADEPRN8
4439-44        I 27340 (1829)  149 I 4478 (1828)        INETID=JADEPRN9
4445-50,4286   I 21047 (1095)  150 I 3296 (1085)        INETID=JADEPRN0
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0133V00
                       2338 EVENTS EXCEPT RUNS 4427-47 SEE NOTE BELOW
                                         RUN 4286 DELETED
------------------------------------------------------------------------
**********************************************************************
        RUNS 4427 - 4447  HAVE SCRAMBLED LG ADCS AND ARE CONTAINED
                     ON THE MERGED TAPE
                JADEPR.ADC.MESS2
                     15311 EVENTS
***********************************************************************
4451-54        I 31447 (1100)  151 I 4664 (0416)        INETID=JADEPRN1
4455-59        I 30267 (1101)  152 I 4701 (1101)        INETID=JADEPRN2
4460-64        I 20663 (1101)  153 I 3216 (0417)        INETID=JADEPRN3
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0134V00
                   12581 EVENTS
------------------------------------------------------------------------
4465-70        I 28819 (0318)  154 I 4195 (0429)        INETID=JADEPRN4
4471-74        I 30243 (0370)  155 I 3803 (0431)        INETID=JADEPRN5
4475-77        I 23965 (1013)  156 I 1963 (1010)        INETID=JADEPRN6
4478-81        I 24670 (0183)  157 I 3069 (1012)        INETID=JADEPRN7
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0135V00
                    13030 EVENTS
------------------------------------------------------------------------
4482-4500      I 30433 (1663)  158 I 4221 (0185)        INETID=JADEPRN8
4199-4202      I 30490 (0904)  159 I 4556 (1256)        INO NET
        ATTENTION MUON LOVERS -----  STARTING WITH THE RUN BELOW
                    THE FORWARD MUON COUNTER TRIGGERS ARE
                    INCLUDED IN THE DATA REDUCTION
4501-4504      I 29383 (0357)  160 I 4656 (0568)        INO NET
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0136V00
                    13433 EVENTS
------------------------------------------------------------------------
4505-4515      I 24816 (1397)  161 I 4093 (1661)        I
4516-4525      I 39313 (0468)  162 I 6711 (0447)        INETID=JADEPRN2
4526-4531      I 30382 (0472)  163 I 5632 (0453)        INETID=JADEPRN3
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0137V00
                    16436 EVENTS
------------------------------------------------------------------------
4532-4537      I 30046 (0480)  164 I 5769 (1925)        INO NET
4418           I  7992 (1944)  165 I 1258 (1931)        INETID=JADEPRN5
4538-4543      I 32741 (1950)  166 I 6863 (1932)        INETID=JADEPRN6
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0138V00
                     13890 EVENTS
------------------------------------------------------------------------
4544-4552      I 41004 (0991)  167 I 7745 (0990)  167   INETID=JADEPRN7
4544-4552      I 41004 (0991)  167 I  304 (1681)  167A  I
4553-4562      I 32351 (1014)  168 I 6136 (1449)        INETID=JADEPRN8
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0139V00
                     14185 EVENTS
------------------------------------------------------------------------
4563-4569      I 35520 (1384)  169 I 6387 (1451)        INETID=JADEPRN9
C--- ATN. GOT BADLY BURNED BY THE NET. JOB KILLED BECAUSE INPUT TAPE
C--- DID NOT EXIST EVEN THOUGH THE JOB WAS HELD. LHO 17.15 16.8.80.
4570-4570      I 25877 (1053)  170 I 4645 (1738)        INETID=JADEPRN0
4576-4580      I 31373 (1057)  171 I 5609 (0530)        INO NET
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0140V00
                   16641 EVENTS
------------------------------------------------------------------------
4581-4587      I 31622 (1589)  172 I 5183 (1592)        INETID=JADEPRN2
4421           I  7992 (0398)  173 I 1410 (0599)        I
4432           I  7986 (1298)  174 I 1354 (1271)        INETID=JADEPRN4
4588-91,94-96  I 28667 (1306)  175 I 4087 (1272)        INETID=JADEPRN5
------------------------------------------------------------------------

      THE ABOVE  4 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0141V00
                       12034 EVENTS
------------------------------------------------------------------------
4597-600       I 31959 (1310)  176 I 3880 (1273)        INETID=JADEPRN6
4601-5         I 32132 (1314)  177 I 4231 (1278)        INETID=JADEPRN7
4606-10        I 34175 (1316)  178 I 4283 (1279)        INETID=JADEPRN8
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0142V00
                        12394 EVENTS
------------------------------------------------------------------------
4611-4614      I 30624 (0993)  179 I 3548 (0476)        INETID=JADEPRN9
4615-4621      I 29487 (0471)  180 I 5217 (0484)        INETID=JADEPRN0
4550,51,66     I 19712 (0506)  181 I 1612 (0509) TIMEOUTINETID=JADEPRN1
               I       (0506)  181 I 1996 (0823) 181A   INO NET
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0143V00
                   12373  EVENTS
------------------------------------------------------------------------
4622-4626      I 30275 (0997)  182 I 5969 (0285)        INETID=JADEPRN2
4622-4626      I 29670 (0997)  182 I  135 (0901)  182A  I
4627-4636      I 32757 (0097)  183 I 6443 (0089)        INETID=JADEPRN3
4627-4636      I 36569 (0097)  183 I  829 (0982)  183A  I
4592-4593      I 15972 (0173)  184 I 2176 (0052)        INETID=JADEPRN4
4632,4635      I 11857 (0177)  185 I 2364 (0056)        INETID=JADEPRN5
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0144V00
                     17916 EVENTS
------------------------------------------------------------------------
4637-4642      I 31387 (0184)  186 I 6539 (0071)        INETID=JADEPRN6
4653-57,51,52  I 24403 (0188)  187 I 4433 (0074)        INETID=JADEPRN7
4653-57        I 34946 (0194)  188 I 6764 (0135)        INETID=JADEPRN8
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0145V00
                    17736 EVENTS
------------------------------------------------------------------------
4658-4662      I 29926 (1263)  189 I 6033 (1325)        INO NET
4663-4669      I 31743 (1225)  190 I 5744 (1163)        INETID=JADEPRN0
4671-4675      I 33046 (1233)  191 I 6118 (1180)        INETID=JADEPRM1
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0146V00
                    17895 EVENTS
------------------------------------------------------------------------
4650           I  1143 (0334)  192 I  263 (0314)        INETID=JADEPRM2
4676-4679      I 28122 (0339)  193 I 5349 (0320)        INETID=JADEPRM3
4680-4686      I 33998 (1285)  194 I 6057 (1272)        INETID=JADEPRM4
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0147V00
                    11669 EVENTS
------------------------------------------------------------------------
4687-4691      I 32381 (1286)  195 I 5789 (1273)        INETID=JADEPRM5
4692-4696      I 27257 (1287)  196 I 4350 (1277)        INETID=JADEPRM6
4697-4701      I 29467 (1288)  197 I 5036 (1279)        INETID=JADEPRM7
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0148V00
                      15175 EVENTS
------------------------------------------------------------------------
4702-4706      I 30765 (1290)  198 I 5125 (1281)        INETID=JADEPRM8
4708-4714      I 36120 (0866)  199 I 5329 (0881)        INETID=JADEPRN9
4715-4717      I 23952 (0866)  200 I 3150 (0885)        INETID=JADEPRN0
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0149V00
                    13604 EVENTS
------------------------------------------------------------------------
4718-4722      I 29884 (0779)  201 I 3962 (0753)        INETID=JADEPRP1
4723-4726      I 31918 (0781)  202 I 4249 (0754)        INETID=JADEPRP2
4727-4729      I 18203 (0595)  203 I 3237 (0596)        INETID=JADEPRD3
4730-4732      I 18152 (1015)  204 I 2920 (0759)        INETID=JADEPRP4
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0150V00
                     14368 EVENTS
------------------------------------------------------------------------
4733-4737      I 31257 (    )  205 I 4819 (1818)        INETID=JADEPRJ5
4738-4742      I 31107 (1121)  206 I 5009 (1820)        INETID=JADEPRJ6
4743-4747      I 32352 (0634)  207 I 5850 (1828)        INETID=JADEPRJ7
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0151V00
                    15678 EVENTS
------------------------------------------------------------------------
4748-51        I 27872 (1011)  208 I 5144 (0571)        INETID=JADEPRJ8
4752-4756      I 33823 (0602)  209 I 6179 (0577)        INETID=JADEPRJ9
4757,59-61     I 22722 (0605)  210 I 3656 (0588)        INETID=JADEPRK0
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0152V00
                    14979 EVENTS
------------------------------------------------------------------------
4771-74        I 31396 (0104)  211 I 4682 (0350)        INO NET
4762-66        I 26001 (0607)  212 I 3763 (0608)        INETID=JADEPRZ2
4775-88        I 40057 (0106)  213 I 6908 (0352)        INO NET
------------------------------------------------------------------------

      THE ABOVE  3 REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0153V00
                  15353 EVENTS
------------------------------------------------------------------------
4789-95        I 32082 (    )  214 I 4705 (0644)        INO NET
4796-800       I 26621 (0610)  215 I 3596 (0611)        INO NET
4801,2,5-7     I 26335 (0142)  216 I 4063 (0152)        INETID=JADEPRK5
4808-11        I 30899 (0142)  217 I 4902 (1337)        I
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0154V00
                17266 EVENTS
                SEE MEMBER REDUCSUM
------------------------------------------------------------------------
4707           I  7657 (0645)  218 I 1212 (1370)        INO NET
4812-22        I 32344 (1136)  219 I 5615 (1122)        INETID=JADEPRD9
4823-26        I 31177 (1137)  220 I 6003 (    )        INETID=JADEPRD0
4827-29        I 23981 (1138)  221 I 3913 (    )        INETID=JADEPRD1
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0155V00
                     16743 EVENTS
               CHECK MEMBER REDUCSUM
------------------------------------------------------------------------
4830-34        I 29267 (1139)  222 I 4928 (    )        INETID=JADEPRD2
4835-39        I 32702 (1348)  223 I 5499 (1162)        INO NET
4840-45        I 37306 (1350)  224 I 5083 (1167)        INO NET
4846-53        I 36239 (1081)  225 I  591 (0164)        INO NET
------------------------------------------------------------------------
  *****  NOTE RUNS 4842 - 4852  ARE COSMIC RUNS AND ARE NOT*******
  *****   INCLUDED ON THE MERGED TAPE        ************************

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0156V00
                     13404 EVENTS
              SEE MEMBER REDUCSUM
------------------------------------------------------------------------
4854-58        I 33624 (1095)  226 I 5487 (0102)        INO NET
4859-62        I 29295 (1098)  227 I 4562 (0100)        INO NET
4770           I  2561 (1108)  228 I  396 (0095)        INO NET
4793           I  5427 (1112)  229 I  856 (0093)        INO NET
4803-4         I  3880 (1117)  230 I  627 (0090)        INO NET
4820           I  7997 (0068)  231 I 1417 (0074)        INETID=JADEPRD1
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0157V00
                 13345 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
4863-64        I  9331 (0562)  232 I 1651 (1290)        INETID=JADEPRN2
4865-98        I 28382 (0563)  233 I 1994 (1296)        INETID=JADEPRN3
4999-5003      I 25869 (0674)  234 I 1977 (1300)        INETID=JADEPRN4
5004-10        I 34801 (1656)  235 I 2625 (1661)        INETID=JADEPRN5
5018-20        I  9846 (1688)  236 I  917 (1652)        INETID=JADEPRN6
5023           I  5076 (1690)  237 I  650 (1655)        INETID=JADEPRN7
5011-15        I 29894 (1700)  238 I 1828 (1662)        INETID=JADEPRN8
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0158V00   REMADE
                 11642 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5016-22        I 23110 (0184)  239 I 1917 (0179)        INETID=JADEPRN9
5024-37        I 22925 (1101)  240 I 2591 (1115)        INETID=JADEPRN0
5038-60        I 29557 (1102)  241 I 2845 (1119)        INETID=JADEPRN1
5061-69        I 18957 (1105)  242 I 2328 (0466)        INO NET
5070-74        I 34572 (1107)  243 I 3154 (1395)        INO NET
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0159V00   REMADE
                 12834 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5078-81        I 25489 (1108)  244 I 2378 (0104)        INETID=JADEPRM4
5039-42        I 22743 (0203)  245 I 2668 (0472)        INO NET
5043-46        I 22423 (0205)  246 I 2851 (0106)        INETID=JADEPRM6
5047-53        I 20253 (0207)  247 I 2526 (0107)        INETID=JADEPRM7
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0160V00   REMADE
              10423 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5054-56        I 20324 (0208)  248 I 2528 (1899)        INO NET
5068-76        I 21493 (0209)  249 I 2196 (0114)        INETID=JADEPRM9
5082-88        I 33541 (0211)  250 I 4599 (0124)        INETID=JADEPRM0
5092-96        I 26979 (0212)  251 I 3577 (1633)        INO NET
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0161V00   REMADE
           12900 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5097-5100      I 23763 (0214)  252 I 2942 (0129)        INETID=JADEPRM2
5101-5104      I 26046 (1180)  253 I 2514 (1138)        INETID=JADEPRA3
5105-5109      I 24291 (1182)  254 I 2990 (1140)        INETID=JADEPRA4
5110-5114      I 25967 (1184)  255 I 3278 (0487)        INO NET
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0162V00   REMADE
           11723 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5115-5120      I 25057 (1419)  001 I 3103 (1171)        INETID=JADEPRA1
5121-5124      I 24823 (1190)  002 I 2823 (0494)        INO NET
5125-5130      I 27011 (0916)  003 I 3310 (0497)        INO NET
5131-5143      I 25766 (0922)  004 I 3002 (0885)        INETID=JADEPRB4
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0163V00   REMADE
             12238 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5144-5149      I 34728 (1500)  005 I 4072 (0501)        INO NET
5150-5152      I 20166 (0956)  006 I 2568 (0889)        INETID=JADEPRB6
5153-5156      I 29016 (0964)  007 I 2925 (1767)        INO NET
                                   RUN 5153, EVENT 26 SKIPPED
5089-5164      I 21043 (1354)  008 I 2453 (1010)        INETID=JADEPRC8
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0164V00   REMADE
             12018 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5157-5161      I 24889 (1229)  009 I 2598 (1099)        INETID=JADEPRC9
5162-5176      I 32150 (1236)  010 I 3867 (0505)        INO NET
5177-5182      I 28185 (1241)  011 I 4258 (1128)        INETID=JADEPRC1
5184-5187      I 24876 (1244)  012 I 3363 (0508)        INO NET
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0165V00   REMADE
          14086 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5183           I  1140 (0402)  013 I  134 (0386)        INETID=JADEPRN3
5188-5191      I 27848 (0404)  014 I 3934 (0390)        INETID=JADEPRN4
5192-5196      I 25819 (0407)  015 I 3926 (0391)        INETID=JADEPRN5
5197-5199      I 19484 (0413)  016 I 2733 (0392)        INETID=JADEPRN6
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0166V00   REMADE
            10727 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5200-5203      I 24517 (0416)  017 I 3552 (0395)        INETID=JADEPRN7
5204-5209      I 24966 (1052)  018 I 3669 (1035)        INETID=JADEPRN8
5210-5217      I 24666 (1055)  019 I 3800 (1041)        INETID=JADEPRN9
5218-5220      I 21845 (1056)  020 I 3078 (1045)        INETID=JADEPRN0
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0167V00   REMADE
          14099 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5171-5212      I 18299 (1802)  021 I 2688 (1773)        INETID=JADEPRM1
5221-5225      I 24432 (0756)  022 I 3203 (1775)        INETID=JADEPRM2
5226-5230      I 20122 (1811)  023 I 2457 (1778)        INETID=JADEPRM3
5231-5235      I 25715 (1812)  024 I 3772 (1780)        INETID=JADEPRM4
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0168V00   REMADE
           12120 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5236-5240      I 23347 (0483)  025 I 3463 (1787)        INETID=JADEPRM5
5241-5248      I 34220 (0484)  026 I 4227 (0492)        INETID=JADEPRM6
5250-5253      I 28391 (0485)  027 I 3568 (0496)        INETID=JADEPRM7
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0169V00   REMADE
            11258 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5254-5257      I 21932 (0488)  028 I 2815 (0498)        INETID=JADEPRM8
5246,5249      I  4118 (0373)  029 I  579 (0517)        INETID=JADEPRM9
5258-5262      I 27026 (0090)  030 I 3148 (0285)        INETID=JADEPRJ0
5263-5266      I 22436 (0382)  031 I 3696 (0293)        INETID=JADEPRJ1
5267-5269      I 20892 (0071)  032 I 3105 (0297)        INETID=JADEPRJ2
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0170V00   REMADE
             13343 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5270-5274      I 29208 (0397)  033 I 4136 (0300)        INETID=JADEPRJ3
5275-5278      I 22750 (0178)  034 I 3422 (0185)        INETID=JADEPRJ4
5279-5297      I 19958 (0182)  035 I 2227 (0187)        INETID=JADEPRJ5
5298-5300      I 21640 (1892)  036 I 2629 (1873)        INETID=JADEPRN6
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0171V00   REMADE
              12414 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5301-5304      I 22004 (1893)  037 I 2484 (1878)        INETID=JADEPRN7
5305-5309      I 26274 (1897)  038 I 3155 (1879)        INETID=JADEPRN8
5310-5313      I 25128 (1720)  039 I 3175 (0190)        INO NET
5314-5319      I 16583 (1726)  040 I 2065 (1697)        INETID=JADEPRN0
5320           I  5541 (0457)  041 I  468 (0444)        INETID=JADEPRN1
   RUN 5320 REPEATED BELOW
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0172V00   REMADE
         10879 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5320-5322      I 21090 (0464)  042 I 1737 (0449)        INETID=JADEPRN2
5323-5327      I 28161 (0465)  043 I 2819 (0450)        INETID=JADEPRN3
5328-5331      I 26891 (1561)  044 I 3585 (1576)        INETID=JADEPRN4
5332-5336      I 20360 (0233)  045 I 2746 (0208)        INETID=JADEPRN5
5337-5341      I 29096 (0236)  046 I 3055 (0211)        INETID=JADEPRN6
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0173V00   REMADE
                  13942 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5344-5349      I 27908 (0241)  047 I 4158 (0216)        INETID=JADEPRN7
5350-5355      I 30915 (0319)  048 I 4789 (0218)        INETID=JADEPRN8
5356-5361      I 21139 (0324)  049 I 3265 (0329)        INETID=JADEPRN9
5362-5366      I 24727 (1942)  050 I 2821 (1919)        INETID=JADEPRN0
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0174V00   REMADE
                   15033 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5367-5371      I 28961 (1944)  051 I 4403 (1931)        INETID=JADEPRN1
5372-5375      I 21210 (0670)  052 I 3299 (1932)        INETID=JADEPRN2
5376-5379      I 23682 (0608)  053 I 4242 (1933)        INETID=JADEPRN3
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0175V00   REMADE
              11944 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5380-5389      I 31206 (0611)  054 I 5055 (0586)        INETID=JADEPRN4
5390-5393      I 26670 (1616)  055 I 4298 (0680)        INETID=JADEPRN5
5394-5401      I 48204 (0001)  056 I 5002 (1355)        INETID=JADEPRN6
    TIMEOUT AFTER 30398 EVENTS 056AI 3474 (0300)
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0176V00   REMADE
          17829 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5399-5407      I 44853 (0014)  057 I 5295 (0445)        I
    TIMEOUT AFTER 29676 EVENTS 057AI 2673 (0886)
5402-5412      I 45153 (0398)  058 I 4594 (1950) TIMEOUTINETID=JADEPRN8
    TIMEOUT AT RUN 5410 EV 900 058AI 2191 (0306)        I
5408-5417      I 43377 (0394)  059 I 4104 (1506)        INETID=JADEPRN9
    TIMEOUT AFTER 27150 EVENTS 059AI 2794 (0301)
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0177V00   REMADE
       6786 EVENTS  ( DUPLICATED EVENTS ELIMINATED)
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5413-5417      I 26068 (0281)  060 I 4809 (1838)        INETID=JADEPRN0
5418-5423      I 24878 (0428)  061 I 4544 (0447)        INETID=JADEPRN1
5424-5427      I 23382 (0487)  062 I 3748 (0413)        INETID=JADEPRN2
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0178V00   REMADE
             13101 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5428-5430      I 21745 (0494)  063 I 2820 (0416)        INETID=JADEPRN3
5431-5434      I 22283 (0497)  064 I 2894 (0422)        INETID=JADEPRN4
5435-5438      I 23608 (1004)  065 I 2775 (0912)        INETID=JADEPRM5
5439-5442      I 26461 (1008)  066 I 3833 (0926)        INETID=JADEPRM6
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0179V00   REMADE
          12322 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5443-5446      I 24156 (1010)  067 I 3984 (0941)        INETID=JADEPRM7
5447-5452      I 27814 (0686)  068 I 3775 (0678)        INETID=JADEPRM8
5453-5456,62   I 24130 (0689)  069 I 3930 (0684)        INETID=JADEPRM9
5463-5470      I 24988 (0697)  070 I 4230 (0685)        INETID=JADEPRM0
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0180V00   REMADE
             15919 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5471-5473      I 21630 (0320)  071 I 2951 (0323)        INETID=JADEPRM1
5474-5481      I 22200 (0001)  072 I 4270 (1990)        INETID=JADEPRN2
5482-5490      I 31074 (0008)  073 I 5645 (1992)        INETID=JADEPRN3
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0181V00   REMADE
           12866 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5491-5497      I 27949 (0009)  074 I 5413 (1995)        INETID=JADEPRN4
C--- GOT BURNED! NET JOB JADEPRN5 CANCELED BECAUSE REFORM TAPE NOT THERE
5498-5503      I 30043 (0124)  075 I 5454 (0185)        INO NET
5458-5466      I 22822 (0806)  076 I 3707 (0812)        INETID=JADEPRN6
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0182V00   REMADE
          14574 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5504-5513      I 27888 (0086)  077 I 5554 (0047)        INETID=JADEPRN7
5514-5520      I 20906 (0090)  078 I 3614 (0057)        INETID=JADEPRN8
5521-5525      I 30152 (0092)  079 I 4037 (0061)        INETID=JADEPRN9
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0183V00   REMADE
          13205 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5421           I  6119 (1620)  080 I  577 (1613)        INETID=JADEPRN0
C--- REDUC FOR 80 RAN OUT OF TIME.
               I               080AI  162 (0166)        INO NET
5532           I  6968 (1545)  081 I 1139 (1506)TIMEOUT INETID=JADEPRN1
                               081AI  120 (0185)  INO NET
5526-5533      I 19451 (1548)  082 I 3684 (1510)        INETID=JADEPRN2
5534-5538      I 27870 (1551)  083 I 5380 (1513)        INETID=JADEPRN3
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0184V00   REMADE
         11062 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5540-5543      I 27636 (0534)  084 I 4931 (0536)        INETID=JADEPRN4
5539           I  5092 (1567)  085 I 1107 (1521)        INETID=JADEPRN5
5544-5548      I 25400 (1576)  086 I 4941 (1527)        INETID=JADEPRN6
5549-5554      I 23302 (1583)  087 I 4675 (1546)        INETID=JADEPRN7
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0185V00   REMADE
          15654 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5555-5559      I 29731 (0469)  088 I 4582 (1515)        INO NET
5560-5563      I 20065 (1159)  089 I 2817 (1144)        INETID=JADEPRN9
5564-5569      I 30210 (1162)  090 I 5237 (1148)        INETID=JADEPRN0
    TIMEOUT AFTER 30173 EVENTS 090AI    7 (0965)
5570-5573      I 21323 (1680)  091 I 3100 (1157)        INETID=JADEPRN1
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0186V00   REMADE
       15743 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5574-5577      I 23353 (0477)  092 I 2359 (0467)        INETID=JADEPRN2
5578-5582      I 27134 (0217)  093 I 5189 (0473)        INETID=JADEPRN3
5583-5591      I 26210 (0219)  094 I 2841 (0206)        INETID=JADEPRN4
5592-5595      I 18217 (0352)  095 I 1907 (0353)        INETID=JADEPRN5
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0187V00   REMADE
        12296 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5597-5601      I 21975 (0262)  096 I 2394 (0265)        INETID=JADEPRN6
5602-5606      I 27456 (0388)  097 I 4762 (0376)        INETID=JADEPRN7
    TIMEOUT , ONLY 26871 EVS   097AI  105 (0848)        I
5607-5614      I 29895 (0326)  098 I 4704 (0336)        INETID=JADEPRN8
5615-5623      I 28519 (0357)  099 I 4435 (0367)        INETID=JADEPRN9
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0188V00   REMADE
           16400 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5588,5616,19   I 10388 (0479)  100 I 1317 (0462)        INETID=JADEPRN0
   TIMEOUT AFTER 9603 EVS      100AI  102 (1721)        I
5624-5628      I 18566 (0483)  101 I 2331 (0469)        INETID=JADEPRN1
5629-5635      I 24577 (1247)  102 I 3156 (1248)        INETID=JADEPRN2
5636-5643      I 27147 (0445)  103 I 3654 (1374)        INETID=JADEPRN3
   TIMEOUT AFTER 26758 EVS     103AI   69 (0858)        I
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0189V00   REMADE
         10627 EVENTS
                 SEE MEMBER REDUCSUM
------------------------------------------------------------------------
5644-5649      I 22429 (1973)  104 I 3196 (1964)        INETID=JADEPRN4
5650-5654      I 25589 (0240)  105 I 3729 (0218)        INETID=JADEPRN5
5655-5663      I 29897 (1726)  106 I 3458 (1729)        INETID=JADEPRN6
5664-5667      I 22059 (1543)  107 I 2583 (1530)        INETID=JADEPRN7
------------------------------------------------------------------------

      THE ABOVE  REDUCED TAPES HAVE BEEN COMBINED ONTO
         THE SINGLE TAPE JADEPR.REDUC1.G0190V00   REMADE
       12966 EVENTS
                 SEE MEMBER REDUCSUM
    29/03/82            MEMBER NAME  RUNLST   (JADESR)      TEXT





         JADE DATA TAPES SINCE THE FIRST LUMINOSITY


    THE DATA GENERATION GROUPS FOR THE IBM COPY AND REFORMATTED TAPES
    ARE RESPECTIVELY:

         F22YEN.JADE.EXDATA00.G0001V00, G0002V00 ETC.
     OR  F22YEN.JADE.EXDATA01.G0001V00, G0002V00 ETC.(N.B. SOMETIMES
           BELOW THIS ERRONEOUSLY REFERRED TO AS F22YEN.EXDATA01.)
    AND  F11LHO.JDATA01.REFORM.G0001V00, G0002V00 ETC.
         LATER F11LHO.JDATA02.REFORM.G0001V00, ETC.

         THE COLUMN "DGG" BELOW GIVES THE ORDINAL NUMBER OF THOSE
         MEMBERS OF THE ABOVE TWO DATA GENERATION GROUPS WHICH HOLD
         THE DATA FOR EACH GIVEN RUN OR GROUP OF RUNS. FOR EXAMPLE
         THE FIRST IBM COPY TAPE FOR RUNS 586 THROUGH 587 IS
         F22YEN.JADE.EXDATA00.G0016V00, WHILE THE DATA FOR THE
         SAME RUNS IN ANALYSIS FORMAT ARE ON THE TAPE
         F11LHO.JDATA01.REFORM.G0016V00. AFTER F22YEN.EXDATA00.G0174V00
         THE GENERATION GROUP IS DISCONTINUED AND F22YEN.EXDATA01.G ETC
         IS USED. THEN F22YEN.EXDATA01.G0NNNV00 CORESPONDS TO
         F11LHO.JDATA01.REFORM.G0MMMV00, WHERE MMM=NNN+174.
             *****
         FROM F11LHO.JDATA01.REFORM.G0197V00 ON A NEW COMPLICATION
         STARTS. THIS TAPE HOLDS RUNS 1706 THROUGH 1715, WHICH ARE ON
         THE FIRST PART OF F22YEN.EXDATA01.G0023V00. F11LHO.JDATA01.
         REFORM.G0198V00 HOLDS RUNS 1716 AND 1717 FROM NORD TAPE F11098.
         F11LHO.JDATA01.REFORM.G0199V00 HOLDS RUNS 1719 THROUGH 1721
         FROM THE LAST PART OF F22YEN.EXDATA01.G0023V00. FROM THEN ON
         THE OFFSET BETWEEN THE TWO GENERATION GROUP NUMBERS IS 176.
             *****
         ANOTHER TAPE INTERVENES IN THE MIDDLE OF F22YEN.EXDATA01.
         G0027V00. FROM F11LHO.JDATA01.REFORM.G0205V00 ON, THE OFFSET
         BETWEEN GENERATION GROUP NUMBERS IS 178. (THE NORD TAPE DATA
         IN THE "YEN" STAGE ARE ON F22YEN.JADE.EXTAPE00.G0001V00, ETC.)
             *****
         STILL ANOTHER TAPE INTERVENES DURING F22YEN.EXDATA01.G0031V00.
         FROM F11LHO.JDATA01.REFORM.G0211V00 ON THE OFFSET IS 180.

         LATER CHANGES IN THE OFFSET WILL BE NOTED BELOW BETWEEN THE
         USUAL ENTRIES.


     RUNS    NORD TAPE   DGG.  NORD EVS.  REFORM. EVS.     REMARKS

  539-  540    F22B12      1      3112        3111     FIRST LUM. RUN
        541    F22B11      2      1710        1709     27.2 GEV
        543    F22B14      3      8370        8369
        544    F22B21      4      8342        8341
        545    F22B22      5      6838        6837
        547    F22B24      6      6524        6523
        549    F22B26      7      6213        6212
        552    F22B34      8      8635        8634
        553    F22B27      9      8231        8230
  559-  561    F22B29     10      7231        7228
        563    F22B32     11      7986        7985
        564    F22B35     12      2701        2700
        581    F22B37     13      2822        2821     27.72 GEV
        582    F22B38     14      8000        7999
        584    F22B39     15      3711        3710
  586-  587    F22B40     16      7678        7677
        588    F22B41     17      1292        1291
        589    F22B44     18      8000        7999
        590    F22B45     19      4031        4030
        591    F22B46     20      8000        7999
        592    F22B47     21      3224        3223
        593    F11112     22      2450        2449
        594    F11113     23      8465        8464
        595    F11114     24      6220        6219
        596    F11115     25      6772        6755
        597    F11118     26      7413        7412
        598    F11119     27      9012        9011
        599    F11131     28      8000        7999
        600    F11132     29      8000        7999
        601    F11133     30     10000        9999
  602-  603    F11136     31      8873        8872
        604    F11137     32      6022        6021
        605    F11138     33      3626        3624
        606    F22B05     34      5431        5429
  607-  608    F22B08     35      9018        9017
        609    F22B23     36      8000        7999
        610    F22Y01     37      7341        7346
  611-  612    F22B91     38      3892        3889
        613    F22B92     39      8000        7999
        614    F22B93     40      8000        7998
        616    F22B94     41      8000        7999
        617    F22B95     42      4151        4149
        618    F22B96     43      8000        7999
        619    F22B99     44      2162        2161
        620    F22B98     45      8000        7999
        621    F22B97     46      8500        8499
        622    F22C01     47      4053        4052
        623    F22C02     48      3465        3464
        624    F22C04     49      9000        8999
        630    F22C05     50      3921        3920
        631    F22C06     51      9000        8999
        632    F22C07     52      6266        6265
        633    F22C08     53      3935        3934
        634    F22C09     54      3519        3518
        635    F22C10     55      8000        7999
        636    F22C11     56      8000        7999
        637    F22C12     57      8000        7999
        638    F22C13     58      9000        8999
  639-  640    F22C14     59     10145       10143
  641-  642    F22C15     60      8716        8714
  643-  644    F22C16     61      7254        7252
        645    F22C17     62      7379        7378
        646    F22C18     63      7399        7398
        647    F22C19     64     10000        9999
  648-  649    F22C20     65      8372        8370
        650    F22C21     66      9000        8999
        651    F22C22     67      9000        8999
        654    F22C23     68      9000        8999
        655    F22C24     69      8000        7999
        656    F22C25     70     10000        9999
        657    F22T92     71      5013        5012
        658    F22T93     72      8000        7999
        659    F22T94     73     10000        9999
        660    F22T95     74     10000        9999
        661    F22T96     75     10000        9999
        662    F22T97     76      8401        8400
        663    F22T98     77      8775        8774
        664    F22T99     78      8405        8404
        665    F22U01     79      8779        8778
        666    F22U02     80      8316        8315
        667    F22U03     81      1849        1848
        668    F22U04     82      4848        4847
        669    F22U05     83      5706        5705
        670    F22U06     84      5123        5122
  671-  672    F22U07     85      4824        4822
        699    F22U09     86      2467        2466     START 31.6 GEV
        701    F22U10     87      1534        1533
        704    F22U11     88      6953        6952
        705    F22U12     89      6999        6998
        706    F22U13     90      6998        6997
  707-  709    F22U14     91      4189        4185
        764    F22U25     92      1564        1563
  765-  771     LINK      93     14995       14986     766 MISSING
        773    F22U83     94      8177        8176
  775-  776    F22B49     95      4521        4519
        778     LINK      96     22363       22363
  784-  795     LINK      97     51819       51819     RUNS 789, 791,&
        794    F22B60     98      7404        7403          794 MISSING
  795-  798     LINK      99     24879       24879
  804-  809     LINK     100     25748       25748
  815-  817     LINK     101     21642       21641
  819-  828     LINK     102     32889       32888
  829-  835     LINK     103     28451       28451
        837     LINK     104     13679       13679
        838     LINK     105     24638       24638
  839-  855     LINK     106     22301       22301  NO 841-843, 851-854
  860-  867     LINK     107     33878       33878     861, 862 MISSING
  869-  874     LINK     108     26611       26610     873 MISSING
  875-  880     LINK     109     20125       20125     877 MISSING
  882-  887     LINK     110     22310       22310     885 MISSING AND
  888-  893     LINK     111     20362       20362     FUNNY RUN 2 WITH
        914    F22B64    112      4756        4755     170 EV. BETWEEN
  915-  918     LINK     113     24451       24451     882 AND 883
  919-  926     LINK     114     29519       29519     RUN 920 MISSING
  930-  944     LINK     115     32236       32236     FUNNY RUN 916, 1
  946-  983     LINK     116     59457       59457     EVENT BETWEEN 931
        992    F22B69    117      8000        7999     AND 932. 938-941
        993    F22B70    118      8000        7999     MISSING.
        996    F22B71    119      1325        1324
        997    F22SC2    120      8000        7998
  983- 1005     LINK     121     29956       29956     RUNS 983, 984, 63
 1007- 1013     LINK     122     18967       18967     986, 987, 63, 999
 1014- 1017     LINK     123     30058       30058     63, 1005, 63
 1018- 1022     LINK     124     27249       27249     RUNS 1020, 1021
 1023- 1027     LINK     125     30979       30979     MISSING
 1028- 1039     LINK     126     32410       32410  NO 1029-1031,1037-8
 1041- 1048     LINK     127     34632       34632  NO 1045
 1049- 1051     LINK     128     17073       17073
 1052- 1060     LINK     129     40113       40113  NO 1055, 1058-9
 1071- 1078     LINK     130     29699       29699  NO 1073-4
 1079           LINK     131     23027       23027  REWRITTEN 04.08.79
 1080- 1083     LINK     132     22957       22957  RUN 1083 AT 22 GEV
 1084- 1088     LINK     133     29865       29865  27.72 GEV AGAIN
 1089- 1093     LINK     134     48340       48340
 1095- 1098     LINK     135     31589       31580
 1099          F11093    136      7998        7997
 1101- 1105     LINK     137     27584       27584
 1106          F11092    138      7999        7998
 1107          F11094    139      8009        8008
 1113- 1119     LINK     140     42645       42645
 1120- 1129     LINK     141     41454       41454
 1130- 1135     LINK     142     35655       35655
 1136- 1139     LINK     143     31586       31586  NO 1138
 1140- 1147     LINK     144     25419       25419  NO 1141, 1143
 1149          F11095    145      4796        4795
 1150- 1155     LINK     146     30656       30656
 1156- 1160     LINK     147     34193       34193
 1161- 1162     LINK     148     14763       14762
 1210- 1215     LINK     149     25670       25669
 1217- 1225     LINK     150     27582       27582
 1226- 1239     LINK     151     47191       47190  NO 1234, 5, OR 6
 1240- 1244     LINK     152     15623       15622  NO 1241
 1249- 1257     LINK     153     33518       33518  FUNNY RUN 63 IN-
 1258- 1264     LINK     154     21367       21364    STEAD OF 1253
 1265- 1268     LINK     155     21805       21801
 1269- 1272     LINK     156     26433       26432
 1273- 1275     LINK     157     25643       25643
 1276- 1281     LINK     158     26744       26744
 1282- 1286     LINK     159     26504       26503
 1287- 1291     LINK     160     22924       22922  NO 1288
 1292- 1301     LINK     161     26791       26764
 1302- 1303     LINK     162     24860       24859
 1304- 1315     LINK     163     20452       20445
 1317- 1320     LINK     164     18862       18860
 1321- 1336     LINK     165     32220       32219  NO 1322-1325
 1337- 1341     LINK     166     22123       22123
 1344- 1346     LINK     167     28358       28358
 1347- 1358     LINK     168     24168       24168  NO 1349-52, 1354-56
 1359- 1370     LINK     169     32723       32723  NO 1363
 1371- 1374     LINK     170     25100       25100
 1375- 1380     LINK     171     23906       23906
 1381- 1383     LINK     172     24417       24417
 1384- 1397     LINK     173     14629       14629
 1401- 1453     LINK     174     26476       26108  COSMIC TESTS.
 1414- 1453     LINK     175     20722       20354  OVERLAPS 174
 1454- 1485     LINK     176     93797       93797  ???
 1487- 1503     LINK     177     46914       46361  FIRST LUMI 1497
 1504- 1519     LINK     178     29766       29728
 1520- 1530     LINK     179     33083       33055  1526 MISSING
 1531- 1545     LINK     180     35236       35152  NO 1536, 1542, 1544
 1547- 1564     LINK     181     21517       21491  NO 1548,1557
 1565- 1575     LINK     182     49533       49474  +1493,1494,%68,72,74
 1576- 1578     LINK     183     11577       11564  NO 1577
 1579- 1603     LINK     184     40215       40142  NO '88,'90,DOUBLE
                                                    2TIMES '91,'92 &63
 1604- 1615     LINK     185     19987       19931  NO RUN 1605
 1616- 1626     LINK     186     33448       33359
 1627- 1632     LINK     187     27298       27249
 1633- 1640     LINK     188     34566       34501
 1641- 1643     LINK     189     21962       21942
 1644- 1648     LINK     190     28821       28796
 1649- 1653     LINK     191     32984       32952
 1654- 1659     LINK     192     32096       32057
 1660- 1667     LINK     193     25852       25817
 1668- 1679     LINK     194     33553       33514  NO 1670, 1675
 1680- 1681     LINK     195     19181       19157
 1682- 1705     LINK     196     27243       25942
 1706- 1715     LINK     197     21003       20723
 1716- 1718    F11098    198      2421        2417  RUN 1717 MISSING
 1719- 1721     LINK     199      9661        9639
 1722- 1725     LINK     200     27982       27908
 1726- 1729     LINK     201     31608       31544
 1730- 1739     LINK     202     25418       25300
 1740- 1742     LINK     203      8509        8490
 1742- 1743    F11099    204      6270        6260  1742 REPEATED. 1 EV.
 1744- 1748     LINK     205     21184       21141  1747 MISSING
 1749- 1755     LINK     206     28193       28170
 1756- 1760     LINK     207     31246       31178
 1761- 1765     LINK     208     29689       29661
 1766           LINK     209      3425        3416
 1767          F11100    210      6596        6582
 1768- 1773     LINK     211     32676       32644
 1774- 1779     LINK     212     26838       26805
 1780- 1783     LINK     213     22413       22401
 1784- 1787     LINK     214     26044       26022
 1788- 1793     LINK     215     27127       27097
 1794- 1798     LINK     216     29455       29432
 1799- 1802     LINK     217     29882       29857
 1803- 1808     LINK     218     23498       23473 NO 1805, 1806
 1809- 1811     LINK     219     25938       25918
 1812- 1822     LINK     220     28279       28184 1814-1822 COSMICS
 1846- 1869     LINK     221     37079       37018
 1870- 1875     LINK     222     24664       24647
 1876- 1880     LINK     223     32759       32742
 1881- 1887     LINK     224     30183       30150
 1888- 1892     LINK     225     30523       30503
 1893- 1895     LINK     226     18047       18033
 1896- 1901     LINK     227     26706       26689
 1902- 1905     LINK     228     17833       17824
 1906- 1909     LINK     229     28548       28531
 1910- 1912     LINK     230     26063       26045
 1913- 1921     LINK     231     30507       30469 NO 1915
 1922- 1931     LINK     232     21970       21952 1926 REPEATED W 1 EV.
 1932- 1935     LINK     233     35514       35495
 1936- 1939     LINK     234     24639       24627
 1940- 1944     LINK     235     23864       23846
 1945- 1948     LINK     236     28778       28752
 1949- 1955     LINK     237     33264       33245 FUNNY REPT. 1 EV RNS.
 1956- 1962     LINK     238     31109       31090
 1963- 1968     LINK     239     31649       31628
 1969- 1973     LINK     240     34152       34130
 1974- 1983     LINK     241     26654       26637 SEVERAL MIS. OR REP.
 1984- 1995     LINK     242     30537       30097
 1996- 2014     LINK     243     28605       27484 2010, 2012 MISSING
 2015- 2024 NEW LINK     244 NEW 32689 14.12 32666 NO 2016, 2020, 2021
 2025- 2029     LINK     245     25806       25789
 2030- 2037     LINK     246     30117       30094 NO RUN 2035
 2038 (63)      LINK     247         3           3 RUN 63 JUNK
 2040          F11102    248      6504        6498
 2041- 2055     LINK     249     24127       24104
 2057          F11103    250      5828        5816
 2058           LINK     251     10002        9995
         OFFSET BECOMES 184
 2059- 2063     LINK     252     25157       25121
 2064- 2071     LINK     253     31584       31541
 2072- 2075     LINK     254     13250       13208
 2076- 2080     LINK     255     20986       20873

         FROM HERE ON THE REFORMATTED DATA BEGIN TO APPEAR ON GENERATION
         DATA GROUP F11LHO.JDATA02.REFORM.G0001V00, ETC.THE OFFSET
         BECOMES -71

 2081- 2083     LINK     001     18645       18555
 2084- 2091     LINK     002     25204       25098 NO RUN 2090
 2092- 2096     LINK     003     20426       20357
 2097- 2112     LINK     004     20454       20405 NO 2099-2107
 2113- 2117     LINK     005     23802       23710
 2118- 2122     LINK     006     15915       15874
 2123- 2126     LINK     007     22915       22855
 2127- 2130     LINK     008     21120       21055
 2131- 2136     LINK     009     19970       19851
 2137           LINK     010      8365        8329
 2138          F11104    011      2713        2710
 2145           LINK     012      3006        2994
 2146- 2147    F11105    013      4648        4634
 2148           LINK     014      9621        9601
         OFFSET BECOMES -67
 2149- 2151     LINK     015     22454       22386
 2152- 2154     LINK     016     21551       21483
 2155- 2158     LINK     017     20976       20933
 2159- 2161     LINK     018     20063       19976
 2162- 2165     LINK     019     19974       19890
 2166- 2169     LINK     020     16346       16304 FUNNY RUN 63 THERE.
 2170- 2177     LINK     021     18623       18555
 2178- 2183     LINK     022     18690       18584
 2184- 2187     LINK     023     19293       19183
 2188- 2190     LINK     024     24204       24062
 2191- 2194     LINK     025     22529       22394
 2195- 2199     LINK     026     14599       14494
 2200- 2205     LINK     027     19121       18966
 2206- 2209     LINK     028     25789       25584
 2210- 2214     LINK     029     12926       12860
 2215- 2218     LINK     030     23308       23209
 2219- 2221     LINK     031     19769       19717
 2222- 2224     LINK     032     15359       15321
 2225- 2226     LINK     033     14372       14282
 2251- 2255     LINK     034     19609       19609 LG STUFF.
 2289- 2305     LINK     035     26951       23717 2297 FIRST LUMI RUN.
 2306- 2318     LINK     036     30908       30887
 2319- 2324     LINK     037     40061       40011
 2325- 2331     LINK     038     40098       40031
 2332- 2335     LINK     039     35507       35454
 2336- 2344     LINK     040     33722       33680
 2345- 2349     LINK     041     33441       33383
 2350- 2354     LINK     042     36013       35981
 2355- 2361     LINK     043     35707       35651
 2362- 2365     LINK     044     34567       34492
 2366- 2371     LINK     045     29058       29020
 2372- 2376     LINK     046     19507       19464
 2377- 2380     LINK     047     22447       22401
 2381- 2385     LINK     048     29077       29035
 2386- 2389     LINK     049     33980       33922
 2390- 2398     LINK     050     38864       38802
 2399- 2406     LINK     051     39577       39491
 2407- 2413     LINK     052     38988       38895
 2414- 2417     LINK     053     30363       30297
 2418- 2420     LINK     054     12347       12331
 2421- 2422    F11101    055     10667       10656
         OFFSET BECOMES -65
 2423- 2426     LINK     056     28622       28582
 2427- 2432     LINK     057     25933       25884
 2433- 2436     LINK     058     33121       33056
 2437- 2443     LINK     059     44103       44024
 2444- 2447     LINK     060     32096       32020
 2448- 2454     LINK     061     37209       37169
 2455- 2460     LINK     062     33356       33307
 2461- 2465     LINK     063     33649       33588
 2466- 2472     LINK     064     32850       32754
 2473- 2480     LINK     065     34358       34270
 2481- 2482     LINK     066     12733       12694
 2484           TAPE     067      3151        3147
 2485- 2486     LINK     068      3781        3764
 2487           TAPE     069      3298        3285
         OFFSET BECOMES -61
 2488- 2489     LINK     070     19123       19089
 2490- 2493     LINK     071     31650       31582
 2494- 2498     LINK     072     26510       26434
 2499- 2503     LINK     073     29369       29330
 2504- 2511     LINK     074     39470       39406
 2512- 2516     LINK     075     29226       29185
 2517- 2520     LINK     076     19411       19389
 2521- 2526     LINK     077     54631       54440
 2527- 2530     LINK     078     51764       51521
 2531- 2532     LINK     079     23380       23213
 2562           LINK     080     19407       19273
 2563- 2566     LINK     081      6600        6550
 2718- 2732     LINK     082       475         475
 2745- 2754     LINK     083     20354       20170
 2755- 2762     LINK     084     22164       22011
 2760                    085      5426        5392
 2763-2767      LINK     086     15656       15576
 2768-2776      LINK     087                 18231
 2777-2781      LINK     088     20912       20816
 2790-2791      LINK     089     13964       13907
 2792-2794      LINK     090     17915       17838
 2783-2789               091      4796        4765
 2795-2798      LINK     092     18053       18001
 2802-2805      LINK     093      9341        9262
 2806-2816      LINK     094     28137       28036 KAWABATA 152
 2821                    095      6004        5342
 2822-2823               096      8042        8028 TAPE 14
 2817-2824      LINK     097     21387       21351 KAWABATA 153
 2825-2830      LINK     098     28699       28643
 2831-2834      LINK     099     15693       15658
 2835-2838      LINK     100     34014       33891
 2839-2844      LINK     101     22050       21906 KAWABATA 157
 2845-2849      LINK     102     25341       25103 KAWABATA 158
 2850-2852      LINK     103     18980       18877 KAWABATA 159
 2853-2856      LINK     104     18678       18514 KAWABATA 160
 2857-2860      LINK     105     24446       24299 KAWABATA 161
 2861-2862      LINK     106     16004       15933 KAWABATA 162
 2863-2868      LINK     107     19217       19158 KAWABATA 163
 2869-2874      LINK     108     27759       27684 KAWABATA 164
 2875-2879      LINK     109     20990       20953 KAWABATA 165
 2880-2888      LINK     110     31742       31692 KAWABATA 166
 2889-2894      LINK     111     24719       24687 KAWABATA 167
 2895-2901      LINK     112     19850       19823 KAWABATA 168
 2902-2905      LINK     113     24312       24264 KAWABATA 169
 2886-2892               114      8508        8494 KAWABATA EXTAPE#15
 2906-2908      LINK     115     23675       23641 KAWABATA 170
 2909-2912      LINK     116     26549       26527 KAWABATA 171
 2913-2916      LINK     117     26501       26448 KAWABATA 172
 2917-2922      LINK     118     29585       29540 KAWABATA 173
 2926-2928               119     14459       14442 KAWABATA EXTAPE#16
 2929-2930               120      9008        8995 KAWABATA EXTAPE#17
 2923-2935      LINK     121     26836       26788 KAWABATA 174
 2936-2950      LINK     122     40816       40754 KAWABATA 175
 2938,2947               123      4057        4053 KAWABATA EXTAPE#18
 2952-2957      LINK     124     36278       36225 KAWABATA 176
 2958-2960      LINK     125     20241       20194 KAWABATA 177
 2961-2965      LINK     126     31498       31369 KAWABATA 178
 2966-2971      LINK     127     30626       30576 KAWABATA 179
 2972-2976      LINK     128     28916       28886 KAWABATA 180
 2977-2981      LINK     129     34289       34243 KAWABATA 181
 2982-2989      LINK     130     29102       29059 KAWABATA 182
 2990-2998      LINK     131     27654       27576 KAWABATA 183
 2999-3003      LINK     132     34648       34552 KAWABATA 184
 3004-3007      LINK     133     26726       26676 KAWABATA 185
 3008-3011      LINK     134     26910       26882 KAWABATA 186
 2995                    135      8002        7980 KAWABATA EXTAPE#19
 2985,2994               136      9316        9271 KAWABATA EXTAPE#20
 3012-3016      LINK     137     29581       29531 KAWABATA 187
 2939,2948               138     10949       10931 KAWABATA EXTAPE#21
 3037                    139      4215        4193 KAWABATA EXTAPE#22
         OFFSET BECOMES -48
 3017-3020      LINK     140     16041       16012 KAWABATA 188
 3037-3042      LINK     141     33860       33759 KAWABATA 189
 3043-3047      LINK     142     28950       28900 KAWABATA 190
 3048-3055      LINK     143     29509       29346 KAWABATA 191
 3056-3062      LINK     144     31897       31789 KAWABATA 192
 3063-3068      LINK     145     34598       34505 KAWABATA 193
 3069-3073      LINK     146     29090       29032 KAWABATA 194
 3074-3078      LINK     147     23000       22944 KAWABATA 195
 3079-3084      LINK     148     33694       33617 KAWABATA 196
 3085-3093      LINK     149     29636       29585 KAWABATA 197
 3094-3097      LINK     150     25203       25169 KAWABATA 198
 3098-3103      LINK     151     33970       33893 KAWABATA 199
 3104-3110      LINK     152     26860       26809 KAWABATA 200
 3111-3115      LINK     153     27735       27688 KAWABATA 201
 3116-8 23-26   LINK     154     39776       39652 KAWABATA 202
 3127-3132      LINK     155     34740       34660 KAWABATA 203
 3133-3136      LINK     156     26755       26719 KAWABATA 204
 3137-3142      LINK     157     32893       32869 KAWABATA 205
 3143-3149      LINK     158     30327       30316 KAWABATA 206
 3150-3153      LINK     159     26835       26827 KAWABATA 207
 3154-3158      LINK     160     30973       30955 KAWABATA 208
 3159-3169      LINK     161     42829       42805 KAWABATA 209
 3170-3173      LINK     162     26984       26974 KAWABATA 210
 3174-3179      LINK     163     34101       34077 KAWABATA 211
 3180-3188      LINK     164     29341       29326 KAWABATA 212
 3122                    165      8002        7988 KAWABATA EXTAPE#23
 3147                    166      6099        6096 KAWABATA EXTAPE#24
 3160                    167      8002        7997 KAWABATA EXTAPE#25
 3168                    168      5082        5079 KAWABATA EXTAPE#26
 3189-3194      LINK     169     31478       31451 KAWABATA 213
 3195-3200      LINK     170     30382       30366 KAWABATA 214
 3201-3205      LINK     171     26375       26333 KAWABATA 215
 3206-3211      LINK     172     25215       25188 KAWABATA 216
 3212-3218      LINK     173     35434       35300 KAWABATA 217
 3219-3223      LINK     174     29469       29354 KAWABATA 218
 3224-3228      LINK     175     29650       29615 KAWABATA 219
 3229-3234      LINK     176     29058       29017 KAWABATA 220
 3235-3240      LINK     177     36236       36210 KAWABATA 221
 3241-3245      LINK     178     26913       26871 KAWABATA 222
 3246-3251      LINK     179     27356       27300 KAWABATA 223
 3252-3257      LINK     180     28735       28693 KAWABATA 224
 3258-3265      LINK     181     33239       33194 KAWABATA 225
 3266-3273      LINK     182     25384       25313 KAWABATA 226
 3274-3278      LINK     183     32697       32677 KAWABATA 227
 3272,79-86     LINK     184     25326       25314 KAWABATA 228
 3287-8,91-3    LINK     185     28230       28216 KAWABATA 229
 3289-90        TAPE     186      8698        8693 KAWABATA EXTAPE#27
 3294-3296      LINK     187     16455       16447 EXDUMP 304 DIRECTLY
 3311-3318      LINK     188     41739       41735 EXDUMP 305 DIRECTLY
 3319-3324      LINK     189     47170       47165 EXDUMP 306 DIRECTLY
 3313-3314      TAPE     190     11148       11148 KAWABATA EXTAPE#28
C---
C---     SPECIAL NORD 50 0VERFLOW TAPE F11LHO.OVCONDN CONTAINS ALL
C---     OVERFLOW EVENTS FROM RUNS 2751-3184 INCLUSIVE.
 2751-3184    MIXED    OVCONDN     ?          3520
C---
C---     START OF DATA TAKING : 80/04/21
C---
3319-24         LINK     195     65083       65075 KAWABATA 232
3325,31-33      LINK     196     17284       17272 KAWABATA 233
3334-39         LINK     197     30077       30002 KAWABATA 234
3340-42         LINK     198     21229       21196 KAWABATA 235
3343-47         LINK     199     25978       25930 KAWABATA 236
3348-52         LINK     200     22400       21230 KAWABATA 237
3353-57         LINK     201     24462       23361 KAWABATA 238
3358-62         LINK     202     20035       19478 KAWABATA 239
3363,67-9       LINK     203     24291       24209 KAWABATA 240
3370-73         LINK     204     28016       27985 KAWABATA 241
3374-77         LINK     205     26489       26440 KAWABATA 242
3378-83         LINK     206     25194       25148 KAWABATA 243
3384-87         LINK     207     22028       22007 KAWABATA 244
3388-91         LINK     208     32008       31952 KAWABATA 245
3392-95         LINK     209     29108       29029 KAWABATA 246
3395-98         LINK     210     20213       20173 KAWABATA 247
3399-3402       LINK     211     32008       31927 KAWABATA 248
3403-07         LINK     212     32009       31963 KAWABATA 249
3408-11         LINK     213     24384       24345 KAWABATA 250
3412-16         LINK     214     32762       32711 KAWABATA 251
3417-21         LINK     215     19538       19514 KAWABATA 001
3422-25         LINK     216     25206       25120 KAWABATA 002
3426-30         LINK     217     31037       30949 KAWABATA 003
3431-37         LINK     218     26115       26074 KAWABATA 004
3438-41         LINK     219     25843       25811 KAWABATA 005
3442-48         LINK     220     31585       31533 KAWABATA 006
3449-51         LINK     221     18786       18773 KAWABATA 007
3452-56         LINK     222     26183       26170 KAWABATA 008
3457-59         LINK     223     23126       23099 KAWABATA 009
3460-63         LINK     224     25086       25061 KAWABATA 010
3464-67         LINK     225     25687       25657 KAWABATA 011
3468-71         LINK     226     27256       27227 KAWABATA 012
3472-74         LINK     227     24006       23979 KAWABATA 013
3475-78         LINK     228     21670       21644 KAWABATA 014
3479-83         LINK     229     25873       25842 KAWABATA 015
3484-89         LINK     230     25221       25182 KAWABATA 016
3490-93         LINK     231     28704       28676 KAWABATA 017
3494-97         LINK     232     30349       30319 KAWABATA 018
3498-3501       LINK     233     24012       23955 KAWABATA 019
3502-05         LINK     234     23715       23674 KAWABATA 020
3504,06-10,12,13LINK     235     43550       43503 KAWABATA 021
3514-17         LINK     236     29260       29225 KAWABATA 022
3518-23         LINK     237     30984       30948 KAWABATA 023
3524-28         LINK     238     33005       32963 KAWABATA 024
3529-34         LINK     239     30792       30696 KAWABATA 025
3488            TAPE     240      7005        6999 KAWABATA TAPE 29
3535-40         LINK     241     44121       44066 DUMP TPS 351,352
3541-60 TDC TST LINK     242       928         928 DUMP TPS 351,352
3539-60 TEST    LINK     243     13041       13028 KAWABATA 027
3561-67,3539    LINK     244     32777       32703 KAWABATA 028
3568-72         LINK     245     27032       26976 KAWABATA 029
3573-77         LINK     246     23124       23094 KAWABATA 030
3578-85         LINK     247     30756       30686 KAWABATA 031
3586-91         LINK     248     28508       28394 KAWABATA 032
3592-95         LINK     249     32008       31972 KAWABATA 033
3596-99         LINK     250     25002       24982 KAWABATA 034
3600-04         LINK     251     28774       28759 KAWABATA 035
3605-10B        LINK     252     32041       31797 KAWABATA 036
3611-14         LINK     253     23406       23357 KAWABATA 037
3615-18         LINK     254     29368       28530 KAWABATA 038
3619-22         LINK     255     31893       31657 KAWABATA 039

  REFORMAT TAPES NOW HAVE NAMES :
         F11LHO.JDATA03.REFORM.GXXXXV00

3623-26         LINK       1     23151       22698 KAWABATA 040
3627-32         LINK       2     26656       25600 KAWABATA 041
3633-38         LINK       3     36508       36413 KAWABATA 042
3639-42         LINK       4     25886       25836 KAWABATA 043
3643-46         LINK       5     31695       31559 KAWABATA 044
3647-50         LINK       6     27647       27596 KAWABATA 045
3651-58         LINK       7     35491       35444 KAWABATA 046
3659-68         LINK       8     24663       24598 KAWABATA 047
3669-74         LINK       9     39492       39409 KAWABATA 048
3675-79         LINK      10     29844       29803 KAWABATA 049
3680-83         LINK      11     25260       25219 KAWABATA 050
3684-91         LINK      12     36820       36751 KAWABATA 051
3693-99         LINK      13     30785       30647 KAWABATA 052
3692 (NO EVS 1-12) TAPE   14      1535        1534     TAPE 030
3700-04         LINK      15     30537       30473 KAWABATA 053
3705-13         LINK      16     28626       28580 KAWABATA 054
3714-17         LINK      17     28037       27985 KAWABATA 055
3718-22         LINK      18     33082       32979 KAWABATA 056
3723-27         LINK      19     27027       26047 DUMP 382 DIRECTLY
3728-39         LINK      20     31035       29822 KAWABATA 058
3740-45  (0764) LINK      21     40487       40390 KAWABATA 059
3746-52  (0262) LINK      22     28240       28181 KAWABATA 060
3753-69  (1734) LINK      23     36049       35982 KAWABATA 061
3770-74  (1737) LINK      24     26516       26476 KAWABATA 062
3775-83  (1746) LINK      25     25703       25652 KAWABATA 063
3784-91  (1748) LINK      26     29308       29276 KAWABATA 064
3792-96  (1755) LINK      27     28100       28049 KAWABATA 065
3797-02  (0074) LINK      28     32733       32666 KAWABATA 066
3761-62  (1489) TAPE      29      7680        7671 TAPE 31
3768     (1493) TAPE      30      8002        7989 TAPE 32
3803-08  (1499) LINK      31     28129       28099 KAWABATA 067
3809-21  (0173) LINK      32     29457       29410 KAWABATA 068
3822-26  (1506) LINK      33     27230       27197 KAWABATA 069
3827-35  (0610) LINK      34     27293       27244 KAWABATA 070
3836-46  (0209) LINK      35     33873       33822 KAWABATA 071
****
****     WARNING. THE TAPES FROM 36 ON CONTAINED OLD RUNS SCRAMBLED IN.
****
3847-53  (1892) LINK      36     31244       31207 KAWABATA 072SCRAMBLED
3854-57  (1898) LINK      37     26943       26908 KAWABATA 073SCRAMBLED
3858-64  (0608) LINK      38     32929       32868 KAWABATA 074SCRAMBLED
3865-70  (0610) LINK      39     22795       22755 KAWABATA 075SCRAMBLED
3871-75  (1228) LINK      40     26987       26959 KAWABATA 076SCRAMBLED
3876-80  (0300) LINK      41     28297       28276 KAWABATA 077SCRAMBLED
3881-85  (1234) LINK      42     31064       31031 KAWABATA 078SCRAMBLED
3886-88  (0611) LINK      43     24003       23984 KAWABATA 079SCRAMBLED
3889-93  (0374) LINK      44     31862       31835 KAWABATA 080
3894-97  (0376) LINK      45     26017       25989 KAWABATA 081
3898-3901(1514) LINK      46     25199       25148 KAWABATA 082
3902-06  (0381) LINK      47     29091       29051 KAWABATA 083
3907-10  (1242) LINK      48     20434       20411 KAWABATA 084
3911-14  (0386) LINK      49     25310       25282 KAWABATA 085
3915-18  (1196) LINK      50     26021       25996 KAWABATA 086
3919-25  (1199) LINK      51     26196       26154 KAWABATA 087
3926-29  (1204) LINK      52     29600       29562 KAWABATA 088
3932-36  (1208) LINK      53     27014       26985 KAWABATA 089
C---
C---     THE FOLLOWING THREE TAPES, FOR WHICH THE OUTPUTS WERE LOST,
C---     WILL BE SCRATCHED AND RECATALOGED BY SPECIAL LOW PRIORITY
C---     JOBS F11LHOSP, NUMBERS 701, 84 AND 720.
C---
3937-39  (0701) LINK      54     26597       26571 KAWABATA 090
3940,42-5(0084) LINK      55     31372       31342 KAWABATA 091
3946-48  (0720) LINK      56     21176       21152 KAWABATA 092
C---
C---     THE ABOVE THREE TAPES, FOR WHICH THE OUTPUTS WERE LOST,
C---     WILL BE SCRATCHED AND RECATALOGED BY SPECIAL LOW PRIORITY
C---     JOBS F11LHOSP, NUMBERS 701, 84 AND 720.
C---
3949-58  (0227) LINK      57     32388       32373 KAWABATA 093
3959-70  (0229) LINK      58     29956       29907 KAWABATA 094
3971-82  (0235) LINK      59     23498       23470 KAWABATA 095
3983-89  (1619) LINK      60     28083       28061 KAWABATA 096
3990-94  (1646) LINK      61     29991       29964 KAWABATA 097
3995-4003(1649) LINK      62     29661       29634 KAWABATA 098
3924     (0624) TAPE      63      8002        7998  TAPE 33
3933     (0677) TAPE      64      5004        5001  TAPE 34
4004-10  (1157) LINK      65     34545       34525 KAWABATA 099
4011-17  (1159) LINK      66     37128       37095 KAWABATA 100
4018-23  (1613) LINK      67     31831       31809 KAWABATA 101
4024-29  (0684) LINK      68     36070       36042 KAWABATA 102
4038     (1875) TAPE      69      8002        7997  TAPE 35
4030-33  (0679) LINK      70     28887       28868 KAWABATA 103
4034-45  (0684) LINK      71     33541       33509 KAWABATA 104
4046-51  (1283) LINK      72     27591       27536 KAWABATA 105
4052-46  (1846) LINK      73     26698       26654 KAWABATA 106
4057-63  (1851) LINK      74     26558       26535 KAWABATA 107
4064-69  (1853) LINK      75     31595       31560 KAWABATA 108
4070-73  (1916) LINK      76     28526       28495 KAWABATA 109
4074-77  (1920) LINK      77     26540       26515 KAWABATA 110
4078-81  (1923) LINK      78     30539       30509 KAWABATA 111
4082-85  (1924) LINK      79     29177       29153 KAWABATA 112
4086-89  (1942) LINK      80     29771       29739 KAWABATA 113
4090-93  (1946) LINK      81     25362       25343 KAWABATA 114
4094-98  (1947) LINK      82     31025       30997 KAWABATA 115
4099-4102(1948) LINK      83     26579       26542 KAWABATA 116
4103-08  (1949) LINK      84     30273       30234 KAWABATA 117
4109-13  (0947) LINK      85     26917       26886 KAWABATA 118
4114-18,23 602) LINK      86     29407       29382 KAWABATA 119
4124-27  (0604) LINK      87     27482       27457 KAWABATA 120
4128-33  (0605) LINK      88     32688       32649 KAWABATA 121
4134-39  (0413) LINK      89     25362       25318 KAWABATA 122
4140-46  (0436) LINK      90     28272       28257 KAWABATA 123
4104     (0739) TAPE      91      4194        4192  TAPE 36
4165     (0743) TAPE      92      8000        7998  TAPE 37
4168     (0746) TAPE      93       584         583  TAPE 38
4167     (0749) TAPE      94      8002        7997  TAPE 39
4147-55  (0756) LINK      95     26402       26339 KAWABATA 124
4156-73  (0758) LINK      96     35288       35230 KAWABATA 125
         (    ) TAPE      97 TAPE 40 DOES NOT EXIST TAPE 40
4187-89  (1099) TAPE      98     14147       14135  TAPE 41
4174-7,83,4(46) LINK      99     31345       31325 KAWABATA 126
4185-92  (1105) LINK     100     19898       19885 KAWABATA 127
4203-07  (0049) LINK     101     25952       25908 KAWABATA 128
4194-96  (1015) TAPE     102      3980        3974  TAPE 42
4198     (0142) TAPE     103      8000        7992  TAPE 43
4208-13  (0144) LINK     104     27987       27797 KAWABATA 129
4214-18  (0145) LINK     105     29748       29654 KAWABATA 130
4219-21  (0994) LINK     106     18213       18179 KAWABATA 131
4222-27  (0997) LINK     107     32832       32701 KAWABATA 132
4228-32  (0999) LINK     108     28169       28143 KAWABATA 133
4233-35  (1000) LINK     109     22229       22193 KAWABATA 134
4236-42  (1002) LINK     110     34546       34512 KAWABATA 135
4243-47  (1242) LINK     111     21492       21462 KAWABATA 136
4248-51  (0286) LINK     112     30385       30336 KAWABATA 137
4252-57  (0493) LINK     113     25730       25710 KAWABATA 138
4258-61  (0497) LINK     114     30582       30544 KAWABATA 139
4262-65  (0635) LINK     115     25353       25327 KAWABATA 140
4266-69  (0838) LINK     116     28006       27971 KAWABATA 141LGRFM1
4270-73  (0315) LINK     117     25673       25663 KAWABATA 142LGRFM1
4274-81  (1977) LINK     118     32697       32656 KAWABATA 143LGRFM1
4282-85  (1639) LINK     119     28635       28611 KAWABATA 144
4286-92  (1936) LINK     120     31690       31638 KAWABATA 145
4293-97  (0945) LINK     121     26534       26504 KAWABATA 146
4298-01  (0948) LINK     122     30635       30587 KAWABATA 147
4302-04  (1217) LINK     123     24006       23971 KAWABATA 148
4305-11  (0972) LINK     124     36645       36590 KAWABATA 149
4313     (0296) TAPE     125      8002        7998  TAPE 44
4312,14-9(0336) LINK     126     28915       28514 KAWABATA 150
4320-23  (1553) LINK     127     28084       28042 KAWABATA 151
4324-31  (0449) LINK     128     34496       34457 KAWABATA 152
4332-36  (0251) LINK     129     26754       26704 KAWABATA 153EV.NO.MSS
4337-40  (0553) LINK     130     27689       27640 KAWABATA 154
4341-45  (0554) LINK     131     28216       28162 KAWABATA 155
4346-49  (0242) LINK     132     31439       31404 KAWABATA 156
4350-55  (0245) LINK     133     29755       29712 KAWABATA 157
4356-58  (1949) LINK     134     23662       23561 KAWABATA 158
4359-62  (1953) LINK     135     27462       27434 KAWABATA 159
4363-69  (1955) LINK     136     34540       34480 KAWABATA 160
4370-73  (1976) LINK     137     24934       24895 KAWABATA 161
4374-78  (0757) LINK     138     29910       29851 KAWABATA 162ADC MSS
4379-83  (0781) LINK     139     24741       24705 KAWABATA 163ADC MSS
4384-87  (0838) LINK     140     27244       27202 KAWABATA 164
4388-92  (0238) LINK     141     32149       32114 KAWABATA 165
4393-98  (0242) LINK     142     25076       25041 KAWABATA 166
4399-03  (0541) LINK     143     25278       25251 KAWABATA 167LGRFM2
4404-07  (1974) LINK     144     29065       29029 KAWABATA 168LGRFM2
4408-19  (1896) LINK     145     25759       25732 KAWABATA 169LGRFM2
4422-27  (0475) LINK     146     31889       31822 KAWABATA 170LGRFM2
4428-34  (1152) LINK     147     25151       25116 KAWABATA 171LGRFM2
4435-38  (1161) LINK     148     28394       28362 KAWABATA 172LGRFM2
4439-44  (1871) LINK     149     30203       30131 KAWABATA 173LGRFM2
4445-50  (1092) LINK     150     25367       25331 KAWABATA 174LGRFM2
4451-54  (1100) LINK     151     31493       31447 KAWABATA 175
4455-59  (1101) LINK     152     30300       30267 KAWABATA 176
4460-64  (1101) LINK     153     20692       20663 KAWABATA 177
4465-70  (0318) LINK     154     28862       28819 KAWABATA 178
4471-74  (0370) LINK     155     30298       30243 KAWABATA 179
4475-77  (1013) LINK     156     24006       23965 KAWABATA 180
4478-81  (0183) LINK     157     24695       24670 KAWABATA 181
4482-4500(1663) LINK     158     30480       30433 KAWABATA 182
4199-4202(0904) LINK     159     30508       30490 KAWABATA 183
4501-04  (1998) LINK     160     29877       29834 KAWABATA 184LGRFM1
4505-15  (1397) LINK     161     24867       24816 KAWABATA 185
4516-25  (0468) LINK     162     39430       39313 KAWABATA 186
4526-31  (0472) LINK     163     30436       30382 KAWABATA 187
4532-37  (0480) LINK     164     30100       30046 KAWABATA 188
4418     (1112) TAPE     165      8002        7993  EXTAPE   45LGRFM2
4538-43  (1950) LINK     166     32773       32741 KAWABATA 189
4544-52  (0991) LINK     167     41032       41004 KAWABATA 190
4553-62  (1014) LINK     168     32400       32351 KAWABATA 191
4563-69  (1384) LINK     169     35595       35520 KAWABATA 192
4570-75  (1053) LINK     170     25922       25877 KAWABATA 193
4576-80  (1057) LINK     171     31420       31373 KAWABATA 194
4581-87  (1589) LINK     172     31715       31622 KAWABATA 195
4421     (1136) TAPE     173      8002        7992  TAPE 46    LGRFM2
4432     (1154) TAPE     174      8000        7993  TAPE 47    LGRFM2
4588-91,94-6    LINK     175     28718       28667 KAWABATA 196
4597-600 (1310) LINK     176     32008       31959 KAWABATA 197
4601-5   (1314) LINK     177     32205       32132 KAWABATA 198
4606-10  (1316) LINK     178     34245       34175 KAWABATA 199
4611-14  (0993) LINK     179     30691       30624 KAWABATA 200 LGRFM1
4615-21  (0471) LINK     180     29530       29487 KAWABATA 201
4550,1,66(0506) TAPE     181     19727       19712 TAPE 49
4622-26  (0997) LINK     182     30324       30275 KAWABATA 202 LGRFM1
4627-36  (0097) LINK     183     36618       36569 KAWABATA 203   "
4592-93  (0173) TAPE     184     16004       15972 TAPE 50
4632,35  (0177) TAPE     185     11875       11857 TAPE 51
4637-42  (0184) LINK     186     31417       31387 KAWABATA  204
4643-6,51,2 88) LINK     187     24443       24403 KAWABATA  205
4653-57  (0194) LINK     188     34972       34946 KAWABATA  206
4658-62  (1263) LINK     189     29964       29926 KAWABATA  207
4663-69  (1225) LINK     190     31783       31743 KAWABATA  208
4671-75  (1233) LINK     191     33090       33046 KAWABATA  209
4650     (0334) TAPE     192      1443        1443  TAPE 52
4676-79  (0339) LINK     193     28154       28122 KAWABATA  210
4680-86  (1285) LINK     194     34032       33998 KAWABATA  211
4687-91  (1286) LINK     195     32428       32381 KAWABATA  212
4692-96  (1287) LINK     196     27286       27257 KAWABATA  213
4697-01  (1288) LINK     197     29521       29467 KAWABATA  214
4702-06  (1290) LINK     198     30807       30765 KAWABATA  215
4708-14  (0866) LINK     199     36170       36120 KAWABATA  216
4715-17  (0867) LINK     200     24006       23952 KAWABATA  217
4718-22  (0779) LINK     201     29940       29884 KAWABATA  218
4723-26  (0781) LINK     202     32008       31918 KAWABATA  219
4727-29  (0595) LINK     203     18251    18203    KAWABATA  220
4730-32  (1015) LINK     204     18217    18152    KAWABATA  221
4733-37  (    ) LINK     205              31257    KAWABATA  222
4738-42  (1121) LINK     206     31176    31107    KAWABATA  223
4743-47  (0634) LINK     207     32383    32352    KAWABATA  224
4748-51  (1011) LINK     208     27914    27872    KAWABATA  225
4752-56  (0602) LINK     209     33879    33823    KAWABATA  226
4757,59-61 605) LINK     210     22750    22722    KAWABATA  227
4771-74  (0104) LINK     211     31437    31396    KAWABATA  229INVERTED
4762-66  (0607) LINK     212     26035    26001    KAWABATA  228 ORDER
4775-88  (0106) LINK     213     40110    40057    KAWABATA  230
4789-95  (    ) LINK     214     32127    32082    KAWABATA  231
4796-4800 0610) LINK     215     26654    26621    KAWABATA  232
4801,2,5-7(142) LINK     216     26361    26335    KAWABATA  233
4808-11  (0166) LINK     217     30946    30899    KAWABATA  234
4707     (0645) LINK     218      7666     7657    TAPE 53
4812-22  (1136) LINK     219     32411    32344    KAWABATA  235
4823-26  (    ) LINK     220     31200    31177    KAWABATA  236
4827-29  (    ) LINK     221     24006    23981    KAWABATA  237
4830-34  (    ) LINK     222     29282    29267    KAWABATA  238
4835-39  (1348) LINK     223     32734    32702    KAWABATA  239
4840-45  (1350) LINK     224     37326    37306    KAWABATA  240
4846-53  (1081) LINK     225     36260    36239    KAWABATA  241
4854-58  (1095) LINK     226     33679    33624    KAWABATA  242
4859-62  (1098) LINK     227     29336    29295    KAWABATA  243
4770     (1108) LINK     228      2564     2561    TAPE 55
4793     (1112) LINK     229      5431     5427    TAPE 56
4803-4   (1117) LINK     230      3883     3880    TAPE 57
4820     (0068) LINK     231      8002     7997    TAPE 58
4863-64  (0562) LINK     232      9343     9331    KAWABATA  244
4865-98  (0563) LINK     233     28413    28382    KAWABATA  245
4999-5003(0674) LINK     234     25885    25869    KAWABATA  246
5004-10  (1656) LINK     235     34839    34801    KAWABATA  247
5018-20  (1688) LINK     236      9852     9846    TAPE 59
5023     (1690) LINK     237      5080     5076    TAPE 60
5011-15  (1700) LINK     238     29916    29894    KAWABATA  248
5016-22  (0184) LINK     239     23135    23110    KAWABATA  249

          START KAWABATA GENERATION GROUP EXDAT03

5024-37  (1101) LINK     240     22954    22925    KAWABATA  001
5038-60  (1102) LINK     241     29598    29557    KAWABATA  002
5061-69  (1105) LINK     242     19013    18957    KAWABATA  003
5070-74  (1107) LINK     243     34627    34572    KAWABATA  004
5078-81  (1108) LINK     244     25552    25489    KAWABATA  005
5039-42  (0203) LINK     245     22790    22743     TAPES 61-63
5043-46  (0205) LINK     246     22486    22423     TAPES 64-66
5047-53  (0207) LINK     247     20329    20253     TAPES 67-69
5054-56  (0208) LINK     248     20363    20324     TAPES 70-72
5068-76  (0209) LINK     249     21556    21493     TAPES 73-75
5082-88  (0211) LINK     250     33683    33541    KAWABATA  006
5092-96  (0212) LINK     251     27080    26979    KAWABATA  007
5097-5100(0214) LINK     252     23852    23763    KAWABATA  008
5101-04  (1180) LINK     253     26110    26046    KAWABATA  009
5105-09  (1182) LINK     254     24385    24291    KAWABATA  010
5110-14  (1184) LINK     255     26028    25967    KAWABATA  011
5115-20  (1419) LINK     001     25141    25057    KAWABATA  012
5121-24  (1190) LINK     002     24932    24823    KAWABATA  013
5125-30  (0916) LINK     003     27082    27011    KAWABATA  014
5131-43  (0922) LINK     004     25856    25766    KAWABATA  015
5144-49  (1500) LINK     005     34833    34728    KAWABATA  016
5150-52  (0956) LINK     006     20207    20166    KAWABATA  017
5153-56  (0964) LINK     007     29089    29016    KAWABATA  018
5089-5164(1354) LINK     008     21099    21043     TAPES 76-79
5157-61  (1229) LINK     009     24944    24889    KAWABATA  019
5162-76  (1236) LINK     010     32271    32150    KAWABATA  020
5177-82  (1241) LINK     011     28297    28185    KAWABATA  021
5184-87  (1244) LINK     012     24960    24876    KAWABATA  022
5183     (0402) LINK     013      1145     1140     TAPE 80
5188-91  (0404) LINK     014     27993    27848    KAWABATA  023
5192-96  (0407) LINK     015     25899    25819    KAWABATA  024
5197-99  (0413) LINK     016     19603    19484    KAWABATA  025
5200-03  (0416) LINK     017     24602    24517    KAWABATA  026
5204-09  (1052) LINK     018     25076    24966    KAWABATA  027
5210-17  (1055) LINK     019     24792    24666    KAWABATA  028
5218-20  (1056) LINK     020     21944    21845    KAWABATA  029
5171-5212(1802) LINK     021     18369    18299     TAPES 81-83
5221-25  (0756) LINK     022     24634    24432    KAWABATA  030
5226-30  (1811) LINK     023     20263    20122    KAWABATA  031
5231-35  (0800) LINK     024     25798    25715    KAWABATA  032
5236-40  (0483) LINK     025     23460    23347    KAWABATA  033
5241-48  (0484) LINK     026     34381    34220    KAWABATA  034
5250-53  (0485) LINK     027     28453    28391    KAWABATA  035
5254-57  (0488) LINK     028     22021    21932    KAWABATA  036
5246,49  (0373) LINK     029      4127     4118     TAPE 84
5258-62  (0090) LINK     030     27150    27026    KAWABATA  037
5263-66  (0382) LINK     031     22524    22436    KAWABATA  038
5267-69  (0071) LINK     032     20943    20892    KAWABATA  039
5270-74  (0397) LINK     033     29327    29208    KAWABATA  040
5275-78  (0178) LINK     034     22815    22750    KAWABATA  041
5279-97  (0182) LINK     035     20016    19958    KAWABATA  042
5298-300 (1892) LINK     036     21678    21640    KAWABATA  043
5301-04  (1893) LINK     037     22060    22004    KAWABATA  044
5305-09  (1897) LINK     038     26328    26274    KAWABATA  045
5310-13  (1720) LINK     039     25185    25128    KAWABATA  046
5314-19  (1726) LINK     040     16630    16583    KAWABATA  047
5320     (0457) LINK     041      5573     5541    KAWABATA  048
5320-22  (0464) LINK     042     21143    21090    KAWABATA  049
5323-27  (0465) LINK     043     28211    28161    KAWABATA  050
5328-31  (1561) LINK     044     26966    26891    KAWABATA  051
5332-36  (0233) LINK     045     20454    20360    KAWABATA  052
5337-41  (0236) LINK     046     29198    29096    KAWABATA  053
5344-49  (0241) LINK     047     28051    27908    KAWABATA  054
5350-55  (0319) LINK     048     31050    30915    KAWABATA  055
5356-61  (0324) LINK     049     21226    21139    KAWABATA  056
5362-66  (1942) LINK     050     24807    24727    KAWABATA  057
5367-71  (1944) LINK     051     29090    28961    KAWABATA  058
5372-75  (0670) LINK     052     21308    21210    KAWABATA  059
5376-79  (0608) LINK     053     23745    23682    KAWABATA  060
5380-89  (0611) LINK     054     31394    31206    KAWABATA  061
5390-93  (1616) LINK     055     26800    26670    KAWABATA  062
5394-5401(1600) LINK     056     48513    48204    KAWABATA  063
5399-5407(1608) LINK     057     45158    44853    KAWABATA  064
5402-12  (0398) LINK     058     45493    45153    KAWABATA  065
5408-17  (0394) LINK     059     43601    43377    KAWABATA  066
5413-17  (0281) LINK     060     26188    26088    KAWABATA  067
5418-23  (1244) LINK     061     25000    24878    KAWABATA  068
5424-27  (0487) LINK     062     23441    23382    KAWABATA  069
5428-30  (0494) LINK     063     21780    21745    KAWABATA  070
5431-34  (0497) LINK     064     22353    22283    KAWABATA  071
5435-38  (1004) LINK     065     23645    23608    KAWABATA  072
5439-42  (1008) LINK     066     26517    26461    KAWABATA  073
5443-46  (1010) LINK     067     24220    24156    KAWABATA  074
5447-52  (0686) LINK     068     27899    27814    KAWABATA  075
5453-56,62 689) LINK     069     24213    24130    KAWABATA  076
5463-70  (0697) LINK     070     25061    24988    KAWABATA  077
5471-73  (0320) LINK     071     21678    21630    KAWABATA  078
5474-81  (0001) LINK     072     22336    22200    KAWABATA  079
5482-90  (0008) LINK     073     31225    31074    KAWABATA  080
5491-97  (0009) LINK     074     28049    27949    KAWABATA  081
5498-5503(0124) LINK     075     30171    30043    KAWABATA  082
5458-66  (0806) LINK     076     22934    22822    TAPES 86 - 89
5504-13  (0086) LINK     077     28016    27888    KAWABATA  083
5514-20  (0090) LINK     078     21007    20906    KAWABATA  084
5521-25  (0092) LINK     079     30234    30152    KAWABATA  085
5421     (1620) LINK     080      6168     6119     TAPE 85
5532     (1545) LINK     081      6986     6968     TAPE 90
5526-33  (1548) LINK     082     19535    19451    KAWABATA  086
5534-38  (1551) LINK     083     27951    27870    KAWABATA  087
5540-43  (0534) LINK     084     27735    27636    KAWABATA  088
5539     (1567) LINK     085      5099     5092     TAPE 91
         (0922) LINK     145                       KAWABATA  144
5549-54  (1583) LINK     087     23418    23302    KAWABATA  090
5555-59  (0469) LINK     088     29826    29731    KAWABATA  091
5560-63  (1159) LINK     089     20166    20065    KAWABATA  092
5564-69  (1162) LINK     090     30379    30210    KAWABATA  093
5570-73  (1680) LINK     091     21422    21323    KAWABATA  094
5574-77  (0477) LINK     092     23460    23353    KAWABATA  095
5578-82  (0217) LINK     093     27249    27134    KAWABATA  096
5583-91  (0219) LINK     094     26318    26210    KAWABATA  097
5592-95  (0352) LINK     095     18353    18217    KAWABATA  098
5597-5601(0262) LINK     096     22139    21975    KAWABATA  099
5602-06  (0388) LINK     097     28213    27456    KAWABATA  100
5607-14  (0326) LINK     098     30106    29895    KAWABATA  101
5615-23  (0357) LINK     099     28708    28519    KAWABATA  102
5588,5616,19    TAPE     100     10425    10388     TAPE 92
5624-28  (0483) LINK     101     18664    18566    KAWABATA  103
5629-35  (1247) LINK     102     24718    24577    KAWABATA  104
5636-43  (0445) LINK     103     27312    27147    KAWABATA  105
5644-49  (1973) LINK     104     22541    22429    KAWABATA  106
5650-04  (0240) LINK     105     25725    25589    KAWABATA  107
5655-63  (1726) LINK     106     30178    29897    KAWABATA  108
5664-67  (1543) LINK     107     22253    22059    KAWABATA  109
6185-98  (0977) LINK     108     34195    34121    KAWABATA  110
6204-32  (0742) LINK     109     20659    20193    KAWABATA  111
6233-75  (0430) LINK     110     25047    24640    KAWABATA  112
6276-96  (0636) LINK     111     28042    27759    KAWABATA  113
6300-15  (0639) LINK     112     31950    30225    KAWABATA  114
6316-20  (0642) LINK     113     40814    40742    KAWABATA  115
6321-24  (0644) LINK     114     27703    25917    KAWABATA  116
6253     (0609) LINK     115      7172     7122     TAPE 93
6325-28  (0640) LINK     116      9377     5970     DUMP 695 DIRECTLY
  -      (1371) LINK     117         0        0    KAWABATA  117
6329-39  (1385) LINK     118     31258    30779    KAWABATA  118
6340-42  (1393) LINK     119     19966    19454    KAWABATA  119
6343-48  (0419) LINK     120     28680    28066    KAWABATA  120
6349-51  (1557) LINK     121     22026    21887    KAWABATA  121
6352-57  (1559) LINK     122     22014    21746    KAWABATA  122
6358-68  (1560) LINK     123     25697    25540    KAWABATA  123
6369-75  (0605) LINK     124     30037    29899    KAWABATA  124
6377-79  (0606) LINK     125     22583    22426    KAWABATA  125
6380-84  (1918) LINK     126     26423    26208    KAWABATA  126
6385-88  (1919) LINK     127     18735    18715    KAWABATA  127
6389-96  (1921) LINK     128     26301    26148    KAWABATA  128
6397-6403(1922) LINK     129     23467    23342    KAWABATA  129
6404-07  (1929) LINK     130     28699    28638    KAWABATA  130
6408-10  (1335) LINK     131     22928    22888    KAWABATA  131
6411-16  (1337) LINK     132     31334    31279    KAWABATA  132
6419-23  (1341) LINK     133     32228    32164    KAWABATA  133
6424-27  (0927) LINK     134     22329    22205    KAWABATA  134
6401,17,18 (1764) TAPE   135     16592    16560     TAPE 94
6428-35  (1769) LINK     136     30357    30160    KAWABATA  135
6436-45  (1773) LINK     137     27405    27330    KAWABATA  136
6446-50  (1778) LINK     138     31467    31411    KAWABATA  137
6451-55  (1762) LINK     139     28801    28754    KAWABATA  138
6456-61  (1042) LINK     140     26928    22481    KAWABATA  139
6462-66  (1773) LINK     141     25368    25269    KAWABATA  140
6467-73  (1045) LINK     142     24731    24663    KAWABATA  141
6474-86  (1047) LINK     143     29895    29796    KAWABATA  142
6487-90  (1056) LINK     144     24405    24362    KAWABATA  143
6491-94  (0922) LINK     145     26251    26203    KAWABATA  144
6495-6501(0923) LINK     146     28132    28047    KAWABATA  145
6502-05  (0924) LINK     147     29397    29356    KAWABATA  146
6506-10  (0928) LINK     148     23492    23460    KAWABATA  147
6511-16  (0079) LINK     149     29457    29397    KAWABATA  148
6517-21  (0988) LINK     150     25197    25155    KAWABATA  149
6522-26  (1034) LINK     151     30989    30921    KAWABATA  150
6527-32  (1038) LINK     152     25298    25251    KAWABATA  151
6533-42  (1045) LINK     153     32245    32156    KAWABATA  152
6543-47  (1211) LINK     154     22174    22141    KAWABATA  153
6548-66  (1228) LINK     155     28093    28013    KAWABATA  154
6567-71  (1218) LINK     156     30230    30157    KAWABATA  155
6572-76  (1834) LINK     157     28776    28714    KAWABATA  156
6577-84  (0037) LINK     158     22233    22137    KAWABATA  157
6585-93  (0902) LINK     159     31726    31562    KAWABATA  158
6594-6604(0911) LINK     160     25478    25333    KAWABATA  159
6605-10  (0920) LINK     161     24832    24774    KAWABATA  160
6485     (1375) TAPE     162      2162     2157     TAPE 95
6611-13  (1382) LINK     163     21322    21286    KAWABATA  161
6614-24  (1390) LINK     164     26188    26013    KAWABATA  162
6625-29  (1397) LINK     165     25891    25601    KAWABATA  163
6630-33  (1440) LINK     166     27214    27167    KAWABATA  164
6634-37  (0935) LINK     167     22129    22049    KAWABATA  165
6638-47  (0937) LINK     168     22161    21928    KAWABATA  166
6648-59  (0940) LINK     169     26192    26069    KAWABATA  167
6660-64  (1058) LINK     170     29181    29116    KAWABATA  168
6665-71  (1060) LINK     171     23046    22973    KAWABATA  169
6672-79  (1067) LINK     172     23785    23711    KAWABATA  170
6680-84  (1069) LINK     173     25829    25763    KAWABATA  171
6688-92  (1073) LINK     174     25269    25176    KAWABATA  172
6693-97  (1602) LINK     175     26439    26402    KAWABATA  173
6698-6702(1606) LINK     176     29647    29571    KAWABATA  174
6703-07  (1614) LINK     177     29013    28952    KAWABATA  175
6708-12  (0189) LINK     178     26969    26926    KAWABATA  176
6713-20  (0200) LINK     179     27410    27260    KAWABATA  177
6721-24  (0213) LINK     180     22030    21912    KAWABATA  178
6726-30  (1471) LINK     181     28899    28760    KAWABATA  179
6731-36  (1477) LINK     182     23393    23302    KAWABATA  180
6737-41  (1482) LINK     183     28012    27886    KAWABATA  181
6742-46  (1486) LINK     184     27165    27056    KAWABATA  182
6747-50  (1489) LINK     185     26002    25928    KAWABATA  183
6751-55  (1501) LINK     186     27150    27070    KAWABATA  184
6756-59  (1483) LINK     187     21517    21437    KAWABATA  185
6760-63  (1491) LINK     188     26215    26097    KAWABATA  186
6764-70  (1510) LINK     189     31150    30899    KAWABATA  187
6771-74  (1514) LINK     190     23243    23104    KAWABATA  188
6775-83  (1522) LINK     191     30236    30120    KAWABATA  189
6784-87  (1526) LINK     192     24609    24498    KAWABATA  190
6725     (1667) TAPE     193      3524     3497     TAPE 96
6781     (1672) TAPE     194      6678     6650     TAPE 97
6788-92  (1677) LINK     195     23084    22985    KAWABATA  191
6793-96  (0892) LINK     196     25135    24987    KAWABATA  192
6797-6801(1690) LINK     197     27011    26900    KAWABATA  193
6802-06  (0778) LINK     198     22770    22628    KAWABATA  194
6807-11  (0781) LINK     199     30394    30290    KAWABATA  195
6812-20  (0788) LINK     200     30269    30178    KAWABATA  196
6821-24  (0792) LINK     201     25560    25480    KAWABATA  197
6825-29  (0795) LINK     202     23072    22988    KAWABATA  198
6830-34  (0798) LINK     203     25844    25764    KAWABATA  199
6835-39  (0803) LINK     204     30447    30354    KAWABATA  200
6840-48  (1271) LINK     205     26091    26001    KAWABATA  201
6849-53  (1273) LINK     206     27829    27751    KAWABATA  202
6854-57  (1277) LINK     207     26142    26068    KAWABATA  203
6858-66  (1041) LINK     208     26729    26653    KAWABATA  204
6867-71  (1046) LINK     209     27662    27547    KAWABATA  205
6872-77  (1052) LINK     210     22922    22842    KAWABATA  206
6878-82  (0893) LINK     211     25321    25258    KAWABATA  207
6883-87  (0896) LINK     212     30781    30698    KAWABATA  208
6888-96  (0900) LINK     213     22271    22194    KAWABATA  209
6897-6901(0907) LINK     214     27365    27268    KAWABATA  210
6902-07  (0910) LINK     215     29514    29403    KAWABATA  211
6908-12  (0913) LINK     216     26741    26666    KAWABATA  212
6861     (0915) TAPE     217      5576     5556     TAPE 98
6863     (0921) TAPE     218      4401     4381     TAPE 99
6913-18  (0931) LINK     219     27401    27311    KAWABATA  213
6919-23  (0939) LINK     220     21959    21880    KAWABATA  214
6924-27  (0945) LINK     221     25256    25149    KAWABATA  215
6928-34  (0948) LINK     222     28091    27986    KAWABATA  216
6935-38  (0952) LINK     223     22314    22249    KAWABATA  217
6939-43  (0954) LINK     224     28809    28711    KAWABATA  218
6944-47  (0697) LINK     225     22273    22142    KAWABATA  219
6948-52  (0319) LINK     226     26701    26575    KAWABATA  220
6953-58  (0323) LINK     227     24306    24222    KAWABATA  221
6960-69  (0329) LINK     228     32917    32751    KAWABATA  222
6970-73  (0330) LINK     229     25510    25400    KAWABATA  223
6974-77  (0334) LINK     230     23611    23522    KAWABATA  224
6978-84  (0340) LINK     231     25232    25102    KAWABATA  225
6985-90  (1408) LINK     232     27557    27348    KAWABATA  226
6991-95  (1414) LINK     233     32532    32294    KAWABATA  227
6996-7000(1418) LINK     234     30492    30315    KAWABATA  228
7001-07  (1420) LINK     235     29175    29005    KAWABATA  229
7009-15  (1426) LINK     236     26004    25838    KAWABATA  230
7016-23  (1430) LINK     237     29866    29674    KAWABATA  231
   4 APRIL 1981 KAWABATA GENERATION GROUP # 4 STARTS BELOW.
7024-29  (0101) LINK     238     28762    28503    KAWABATA  001
7030-34  (0107) LINK     239     24460    24194    KAWABATA  002
7035-38  (0120) LINK     240     22491    22296    KAWABATA  003
7039-42  (0121) LINK     241     23954    23701    KAWABATA  004
7043-50  (0122) LINK     242     26292    25992    KAWABATA  005
7051-54  (0127) LINK     243     22051    21844    KAWABATA  006
7055-59  (0129) LINK     244     29579    29317    KAWABATA  007
7060-64  (0131) LINK     245     27624    27357    KAWABATA  008
7065-68  (0007) LINK     246     20328    20087    KAWABATA  009
7070-74  (0011) LINK     247     29050    28852    KAWABATA  010
7075-78  (0013) LINK     248     21225    21089    KAWABATA  011
7079-82  (0014) LINK     249     26762    26598    KAWABATA  012
7083-87  (0020) LINK     250     28280    28074    KAWABATA  013
7088-91  (0024) LINK     251     19052    18953    KAWABATA  014
7013     (0302) TAPE     252      7195     7154     TAPE 100
7112-13  (0314) TAPE     253      2856     2818     TAPE 101
7092-96  (0322) LINK     254     24923    24761    KAWABATA  015
7097-02  (0331) LINK     255     29985    29758    KAWABATA  016
   JDATA05 .. REFORMATTED GENERATION GROUP 5 STARTS HERE.
7103-06  (0344) LINK     001     25334    25179    KAWABATA  017
7107-11  (0354) LINK     002     19051    18916    KAWABATA  018
7114-17  (0362) LINK     003     26367    26240    KAWABATA  019
7118-25  (0801) LINK     004     22276    22042    KAWABATA  020
7126-34  (0807) LINK     005     28964    28761    KAWABATA  021
7134-42  (0819) LINK     006     33010    32748    KAWABATA  022
7145-50  (1955) LINK     007     29597    29369    DUMP TAPES DIRECTLY
7151-53  (0831) LINK     008     19616    19434    KAWABATA  024
7154-57  (1731) LINK     009     23198    23097    KAWABATA  025
7158-63  (1738) LINK     010     25920    25721    KAWABATA  026
7164-67  (1805) LINK     011     25915    25654    KAWABATA  027
7168-71  (1963) LINK     012     28333    28103    KAWABATA  028
7172-77  (1969) LINK     013     25716    24070    KAWABATA  029
7178-85  (1971) LINK     014     23718    22289    KAWABATA  030
7186-95  (1974) LINK     015     23725    21838    KAWABATA  031
7196-7201(1976) LINK     016     27829    26514    KAWABATA  032
7202-06  (1978) LINK     017     32161    31899    KAWABATA  033
7207-10  (1982) LINK     018     24261    24145    KAWABATA  034
7211-16  (1987) LINK     019     28535    28316    KAWABATA  035
7217-22  (1774) LINK     020     34936    34666    KAWABATA  036
7223-27  (1775) LINK     021     29641    29329    KAWABATA  037
7228-32  (1777) LINK     022     31578    31244    KAWABATA  038
7233-38  (1778) LINK     023     32044    31765    KAWABATA  039
7239-42  (0574) LINK     024     28241    27969    KAWABATA  040NO RUTH
7243-48  (0575) LINK     025     28429    28097    KAWABATA  041   "
7249-53  (0576) LINK     026     30412    30094    KAWABATA  042   "
7254-58  (1899) LINK     027     34155    33704    KAWABATA  043   "
7259-63  (1900) LINK     028     25316    24838    KAWABATA  044   "
7264-69  (1114) LINK     029     39206    38662    KAWABATA  045   "
7270-77  (1116) LINK     030     28114    27744    KAWABATA  046   "
7278-82  (0738) LINK     031     25998    25749    KAWABATA  047   "
7283-86  (0745) LINK     032     27646    27320    KAWABATA  048   "
7287-91  (0788) LINK     033     24137    23799    KAWABATA  049   "
7144     (0462) TAPE     034      4624     4588     TAPE 102       "
7292-97  (0438) LINK     035     24272    21080    KAWABATA  050 + RUTH
7155-63  (0568) LINK     036     17328     9545    KAWABATA  051
7325-31  (0450) LINK     037     25454    25002    KAWABATA  052 + RUTH
7332-40  (1715) LINK     038     29185    28653    KAWABATA  053 + RUTH
7341-45  (0155) LINK     039     19377    19082    KAWABATA  054 + RUTH
7346-52  ( 161) LINK     040     23069    22576    KAWABATA  055 NO RUTH
7353-57  ( 164) LINK     041     26940    26171    KAWABATA  056 NO RUTH
7358-61  (   6) LINK     042     26862    26287    KAWABATA  057 NO RUTH
7368-70  (1979) LINK     043     23260    22879    KAWABATA  058 NO RUTH
7371-72  (1990) LINK     044     25696    25169    KAWABATA  059 NO RUTH
7375-78  (   5) LINK     045     20275    19179    KAWABATA  060 NO RUTH
7379-85  (  31) LINK     046     26678    26251    KAWABATA  061 NO RUTH
7386-91  (  46) LINK     047     32245    31558    KAWABATA  062 NO RUTH
7392-94  ( 698) LINK     048     17084    16787    KAWABATA  063 + RUTH
7395-99  ( 321) LINK     049     24342    23991    KAWABATA  064 + RUTH
7400-05  ( 326) LINK     050     27594    27022    KAWABATA  065 + RUTH
7406-09  ( 114) LINK     051     23012    22673    KAWABATA  066 + RUTH
7410-16  ( 124) LINK     052     31403    30864    KAWABATA  067 NO RUTH
7415-19  ( 136) LINK     053     18373    18051    KAWABATA  068 + RUTH
7420-22  ( 150) LINK     054     20047    19829    KAWABATA  069 + RUTH
7423-29  ( 154) LINK     055     23036    22767    KAWABATA  070 + RUTH
7430-33  ( 160) LINK     056     26095    25713    KAWABATA  071 + RUTH
7434-37  ( 169) LINK     057     24575    24141    KAWABATA  072 + RUTH
7438-40  ( 171) LINK     058     23107    22845    KAWABATA  073 + RUTH
7441-45  ( 583) LINK     059     27039    26491    KAWABATA  074 + RUTH
7446-48  ( 585) LINK     060     21028    20708    KAWABATA  075 + RUTH
7449-51  ( 589) LINK     061     19796    19587    KAWABATA  076 + RUTH
7452-55  ( 680) LINK     062     29154    28652    KAWABATA  077 NO RUTH
7456-63  ( 698) LINK     063     30794    30332    KAWABATA  078 NO RUTH
7464-66  ( 752) LINK     064     23187    22776    KAWABATA  079 NO RUTH
7467-70  ( 771) LINK     065     27515    27159    KAWABATA  080 NO RUTH
7471-73  ( 782) LINK     066     22092    21779    KAWABATA  081 NO RUTH
7474-76  ( 796) LINK     067     23285    22881    KAWABATA  082 NO RUTH
7477-80  ( 801) LINK     068     27446    26969    KAWABATA  083 NO RUTH
7481-85  ( 810) LINK     069     27889    27497    KAWABATA  084 NO RUTH
7486-91  (1085) LINK     070     23950    23658    KAWABATA  085 + RUTH
7492-97  (1091) LINK     071     18166    17961    KAWABATA  086 + RUTH
7498-01  (1092) LINK     072     23879    23674    KAWABATA  087 + RUTH
7502-04  (1112) LINK     073     22328    22199    KAWABATA  088 + RUTH
7505-07  (1115) LINK     074     23419    23258    KAWABATA  089 + RUTH
7508-11  (1119) LINK     075     25866    25694    KAWABATA  090 + RUTH
7512-15  (1127) LINK     076     29246    29060    KAWABATA  091 NO RUTH
7516-18  (1133) LINK     077     23296    23124    KAWABATA  092 + RUTH
5135     (1064) LINK     078      7489     7465    KAWEXTAP  103 + RUTH
6246     (1959) LINK     079      5929     5890    KAWEXTAP  104 + RUTH
6218     (1962) LINK     080       632      624    KAWEXTAP  105 + RUTH
6261     (1963) LINK     081      7449     7362    KAWEXTAP  106 + RUTH
6216     (1967) LINK     082       814      795    KAWEXTAP  107 + RUTH
6111     (1969) LINK     083      1732     1676    KAWEXTAP  108 + RUTH
6195     (1970) LINK     084      2178     2173    KAWEXTAP  109 + RUTH
6194     (1973) LINK     085      2169     2156    KAWEXTAP  110 + RUTH
6193     (1978) LINK     086      2196     2127    KAWEXTAP  111 + RUTH
6251     (1979) LINK     087      5451     5379    KAWEXTAP  112 + RUTH
6247     (1980) LINK     088         5        5    KAWEXTAP  113 + RUTH
6221-22  (1982) LINK     089      6008     5960    KAWEXTAP  114 + RUTH
7519-22  ( 371) LINK     090     24329    24141    KAWABATA  093 + RUTH
7523-35  ( 382) LINK     091     21447    21154    KAWABATA  094 + RUTH
7536-40  ( 383) LINK     092     31258    30863    KAWABATA  095 + RUTH
7541-44  ( 387) LINK     093     29327    28817    KAWABATA  096 + RUTH
7545-50  ( 389) LINK     094     22656    22337    KAWABATA  097 + RUTH
7551-53  ( 392) LINK     095     23410    23070    KAWABATA  098 + RUTH
7554-63  ( 306) LINK     096     52397    51563    KAWABATA  099 + RUTH
7566-72  ( 406) LINK     097     23902    23343    KAWABATA  100 + RUTH
7573-77  ( 411) LINK     098     29868    29340    KAWABATA  101 + RUTH
7578-80  ( 320) LINK     099     21422    21179    KAWABATA  102 + RUTH
7581-85  ( 417) LINK     100     32050    31646    KAWABATA  103 + RUTH
7586-88  (1607) LINK     101     19741    19545    KAWABATA  104 + RUTH
7461     ( 583) LINK     102      7639     7508    KAWEXTAPE 115 + RUTH
7589-93  ( 431) LINK     103     10229     5912    KAWABATA  105 + RUTH
7589-96  (  81) LINK     104     31524     29129   KAWABATA  106 + RUTH
7597-03  (  83) LINK     105     37365     36857   KAWABATA  107 + RUTH
7604-17  (  90) LINK     106     27417     24596   KAWABATA  108 + RUTH
7618-32  ( 718) LINK     107     32524     30278   KAWABATA  109 + RUTH
7633-44  ( 730) LINK     108     40981     40833   KAWABATA  110 + RUTH
7645-55  ( 733) LINK     109     35361     35218   KAWABATA  111 + RUTH
7656-64  ( 377) LINK     110     28965     28894   KAWABATA  112 + RUTH
7665-70  ( 381) LINK     111     42377     42254   KAWABATA  113 + RUTH
7671-76  ( 388) LINK     112     33112     32976   KAWABATA  114 + RUTH
7677-81  (1368) LINK     113     28941     28870   KAWABATA  115 + RUTH
7682-86  (1373) LINK     114     31429     31387   KAWABATA  116 + RUTH
7687-91  (1375) LINK     115     39072     38971   KAWABATA  117 + RUTH
7692-96  (1377) LINK     116     37490     37379   KAWABATA  118 + RUTH
7697-00  ( 324) LINK     117     29602     29517   KAWABATA  119 + RUTH
7701-07  ( 326) LINK     118     34212     34102   KAWABATA  120 + RUTH
7708-20  ( 333) LINK     119     34314     34218   KAWABATA  121 + RUTH
7721-26  ( 335) LINK     120     26300     26168   KAWABATA  122 + RUTH
7727-30  ( 339) LINK     121     31843     31706   KAWABATA  123 + RUTH
7731-35  (1832) LINK     122     29726     29531   KAWABATA  124 + RUTH
7736-44  (1835) LINK     123     37445     37323   KAWABATA  125 + RUTH
7745-49  (1841) LINK     124     37269     37111   KAWABATA  126 + RUTH
7750-60  (1848) LINK     125     32484     32316   KAWABATA  127 + RUTH
7761-66  (1856) LINK     126     34329     34253   KAWABATA  128 + RUTH
7767-71  (1859) LINK     127     32246     32148   KAWABATA  129 + RUTH
7772-77  ( 825) LINK     128     42009     41903   KAWABATA  130 + RUTH
7778-82  ( 827) LINK     129     30538     30442   KAWABATA  131 + RUTH
7783-88  ( 834) LINK     130     34984     34864   KAWABATA  132 + RUTH
7789-96  ( 837) LINK     131     38147     38048   KAWABATA  133 + RUTH
7797-01  ( 846) LINK     132     29836     29727   KAWABATA  134 + RUTH
7802-07  ( 853) LINK     133     34595     34468   KAWABATA  135 + RUTH
7808-13  ( 858) LINK     134     42802     42671   KAWABATA  136 + RUTH
7814-18  ( 860) LINK     135     33268     33211   KAWABATA  137 + RUTH
7819-26  ( 467) LINK     136     38416     38215   KAWABATA  138 + RUTH
7827-32  ( 468) LINK     137     33672     33509   KAWABATA  139 + RUTH
7833-38  ( 470) LINK     138     41405     41244   KAWABATA  140 + RUTH
7839-49  (1661) LINK     139     31833     31768   KAWABATA  141 + RUTH
7850-55  (1662) LINK     140     38637     38485   KAWABATA  142 + RUTH
7856-59  (1667) LINK     141     27877     27806   KAWABATA  143 + RUTH
7860-66  (1668) LINK     142     29486     29333   KAWABATA  144 + RUTH
7719-53  (1522) LINK     143     29486     29333   KAWEXTAPE 116-121
7867-71  (1039) LINK     144     34982     34823   KAWABATA  145 + RUTH
7872     (1029) LINK     145      1429      1419   KAWABATA  146 + RUTH
7881-87  (1082) LINK     146     43445     43240   KAWABATA  147 + RUTH
7888-91  (1378) LINK     147     31926     31673   KAWABATA  148 + RUTH
7892-01  (1383) LINK     148     33235     32918   KAWABATA  149 + RUTH
7902-05  (1384) LINK     149     29962     29181   KAWABATA  150 + RUTH
7906-10  (1387) LINK     150     32948     32095   KAWABATA  151 + RUTH
7911-14  (1389) LINK     151     31829     31289   KAWABATA  152 + RUTH
79  -    (    ) LINK     1                         KAWABATA  1   + RUTH
7920-23  (1379) LINK     153     27099     26267   KAWABATA  154 + RUTH
7924-28  (1400) LINK     154     33774     32896   KAWABATA  155 + RUTH
7929-32  (1404) LINK     155     31155     30547   KAWABATA  156 + RUTH
7933-41  (1407) LINK     156     32862     32248   KAWABATA  157 + RUTH
7942-46  (1408) LINK     157     33991     33372   KAWABATA  158 + RUTH
7947-52  ( 677) LINK     158     37599     36885   KAWABATA  159 + RUTH
7723,7955( 846) LINK     159     21895     21562   KAWEXTAPE 122-125
7956,7958
7953-67  (1214) LINK     160     25422     25050   KAWABATA  160 + RUTH
7968-74  (1216) LINK     161     36366     35742   KAWABATA  161 + RUTH
7975-81  (1221) LINK     162     41483     40958   KAWABATA  162 + RUTH
7982-86  (1222) LINK     163     40010     39468   KAWABATA  163 + RUTH
7987-92  (1234) LINK     164     40154     39821   KAWABATA  164 + RUTH
7992-00  ( 978) LINK     165     61286     60801   KAWABATA  165 + RUTH
8001-06  ( 985) LINK     166     40076     39687   KAWABATA  166 + RUTH
8007-12  ( 987) LINK     167     43881     43367   KAWABATA  167 + RUTH
8013-17  ( 989) LINK     168     35277     34999   KAWABATA  168 + RUTH
8018-22  ( 991) LINK     169     39807     39444   KAWABATA  169 + RUTH
8027-39  ( 995) LINK     170     37671     37209   KAWABATA  170 + RUTH
8040-47  ( 997) LINK     171     42660     42186   KAWABATA  171 + RUTH
8048-54  ( 998) LINK     172     34803     34423   KAWABATA  172 + RUTH
8055-64  (1615) LINK     173     44416     43740   KAWABATA  173 + RUTH
8065-69  (1621) LINK     174     31593     31270   KAWABATA  174 + RUTH
8070-74  (1628) LINK     175     35844     35427   KAWABATA  175 + RUTH
8075-79  (1630) LINK     176     37067     36612   KAWABATA  176 + RUTH
8080-84  (1633) LINK     177     40009     39678   KAWABATA  177 + RUTH
8085-90  (1635) LINK     178     45539     45179   KAWABATA  178 + RUTH
8091-95  (1642) LINK     179     40008     39691   KAWABATA  179 + RUTH
8096-01  (1280) LINK     180     35672     35227   KAWABATA  180 + RUTH
8102-06  (1284) LINK     181     33252     32898   KAWABATA  181 + RUTH
8107-10  (1287) LINK     182     32007     31577   KAWABATA  182 + RUTH
8111-16  (1580) LINK     183     40931     40144   KAWABATA  183 + RUTH
8117-22  (1587) LINK     184     33558     32964   KAWABATA  184 + RUTH
8123-26  (1591) LINK     185     32008     31447   KAWABATA  185 + RUTH
8127-31  (1592) LINK     186     37056     36393   KAWABATA  186 + RUTH
8132-36  (1597) LINK     187     36234     35514   KAWABATA  187 + RUTH
8137-40  (1598) LINK     188     32007     31281   KAWABATA  188 + RUTH
8141-46  (1600) LINK     189     36773     35958   KAWABATA  189 + RUTH
8147-52  ( 107) LINK     190     42693     41953   KAWABATA  190 + RUTH
8153-57  ( 108) LINK     191     33832     33284   KAWABATA  191 + RUTH
8158-62  ( 112) LINK     192     35053     34193   KAWABATA  192 + RUTH
8163-67  ( 609) LINK     193     36843     35957   KAWABATA  193 + RUTH
8168-73  ( 614) LINK     194     42785     41915   KAWABATA  194 + RUTH
8174-79  ( 617) LINK     195     40178     39309   KAWABATA  195 + RUTH
8180-83  ( 619) LINK     196     32007     31304   KAWABATA  196 + RUTH
8184-89  ( 622) LINK     197     40488     39699   KAWABATA  197 + RUTH
8190-94  ( 625) LINK     198     38587     38013   KAWABATA  198 + RUTH
8195-01  ( 632) LINK     199     39968     39141   KAWABATA  199 + RUTH
8202-07  ( 635) LINK     200     32050     31385   KAWABATA  200 + RUTH
8208-15  ( 639) LINK     201     38872     38247   KAWABATA  201 + RUTH
8216-22  ( 643) LINK     202     38856     38236   KAWABATA  202 + RUTH
8223-27  ( 799) LINK     203     32231     31629   KAWABATA  203 + RUTH
8228-33  ( 821) LINK     204     35387     34734   KAWABATA  204 + RUTH
8234-41  ( 825) LINK     205     42998     42439   KAWABATA  205 + RUTH
8242-45  ( 829) LINK     206     28499     28093   KAWABATA  206 + RUTH
8246-50  (1733) LINK     207     35178     34448   KAWABATA  207 + RUTH
8251-55  (1658) LINK     208     34102     33294   KAWABATA  208 + RUTH
8256-62  (1659) LINK     209     42173     41416   KAWABATA  209 + RUTH
8263-71  (1663) LINK     210     44667     43589   KAWABATA  210 + RUTH
8272-76  (1664) LINK     211     29623     28495   KAWABATA  211 + RUTH
8277-82  (1666) LINK     212     38645     37509   KAWABATA  212 + RUTH
8283-87  (1667) LINK     213     32179     31515   KAWABATA  213 + RUTH
8023,8024(1682) LINK     214     27828     27493   KAWEXTAPE 126,127,
8026,8041                                                    129,130
8288-94  (1149) LINK     215     43550     42510   KAWABATA  214 + RUTH
8295-03  (1158) LINK     216     29945     29233   KAWABATA  215 + RUTH
8304-10  (1167) LINK     217     35193     34448   KAWABATA  216 + RUTH
8313-18  (1171) LINK     218     38890     38044   KAWABATA  217 + RUTH
8319-25  (1179) LINK     219     42875     42157   KAWABATA  218 + RUTH
8326-31  (  67) LINK     220     32009     31622   KAWABATA  219 + RUTH
8332-37  (  74) LINK     221     37626     37164   KAWABATA  220 + RUTH
8338-43  (  77) LINK     222     35667     35139   KAWABATA  221 + RUTH
8344-51  (  81) LINK     223     34758     34354   KAWABATA  222 + RUTH
8352-57  (  85) LINK     224     39245     38649   KAWABATA  223 + RUTH
8358-66  (  92) LINK     225     39634     38993   KAWABATA  224 + RUTH
8367-71  ( 103) LINK     226     38364     37797   KAWABATA  225 + RUTH
8372-77  ( 106) LINK     227     39749     39248   KAWABATA  226 + RUTH
8378-83  ( 108) LINK     228     39541     38995   KAWABATA  227 + RUTH
8384-88  ( 114) LINK     229     37818     37218   KAWABATA  228 + RUTH
8389-94  ( 118) LINK     230     37497     37842   KAWABATA  229 + RUTH
8395-12  ( 122) LINK     231     38341     37624   KAWABATA  230 + RUTH
8413-16  ( 124) LINK     232     31205     30498   KAWABATA  231 + RUTH
8417-23  ( 127) LINK     233     41739     40895   KAWABATA  232 + RUTH
8424-31  ( 131) LINK     234     46410     45733   KAWABATA  233 + RUTH
8434-41  ( 132) LINK     235     46306     45571   KAWABATA  234 + RUTH
8442-48  ( 137) LINK     236     49055     48579   KAWABATA  235 + RUTH
8269,8258( 174) LINK     237     19747     19303   KAWEXTAPE 131-133
8312
8427,8430( 905) LINK     238     18883     18626   KAWEXTAPE 134-137
8432,8433
8449-54  ( 912) LINK     239     44196     43607   KAWABATA  236 + RUTH
8455-65  ( 913) LINK     240     44850     44229   KAWABATA  237 + RUTH
8465-69  ( 916) LINK     241     35436     34928   KAWABATA  238 + RUTH
8470-76  ( 919) LINK     242     41339     40727   KAWABATA  239 + RUTH
8477-82  ( 924) LINK     243     36519     35862   KAWABATA  240 + RUTH
8483-87  ( 925) LINK     244     39313     38753   KAWABATA  241 + RUTH
8488-93  (1269) LINK     245     43678     42878   KAWABATA  242 + RUTH

   JDATA06 .. REFORMATTED GENERATION GROUP 6 STARTS HERE.
8494-99  (1187) LINK       1     33148     32363   KAWABATA    1 + RUTH
8500-04  (1193) LINK       2     29582     29021   KAWABATA    2 + RUTH
8505-13  (1194) LINK       3     37639     36813   KAWABATA    3 + RUTH
8514-19  (1195) LINK       4     39107     38197   KAWABATA    4 + RUTH
8520-24  (1197) LINK       5     35964     35213   KAWABATA    5 + RUTH
8525-29  (1198) LINK       6     34236     33525   KAWABATA    6 + RUTH
8530-34  (1204) LINK       7     38567     38054   KAWABATA    7 + RUTH
8535-40  (1208) LINK       8     41606     41105   KAWABATA    8 + RUTH
8547-52  (1524) LINK       9     32968     32184   KAWABATA    9 + RUTH
8553-59  (1528) LINK      10     34573     33777   KAWABATA   10 + RUTH
8560-63  (1531) LINK      11     31384     30843   KAWABATA   11 + RUTH
8564-71  (1533) LINK      12     38039     37393   KAWABATA   12 + RUTH
8572-77  ( 617) LINK      13     42396     42177   KAWABATA   13 + RUTH
8578-82  ( 625) LINK      14     39469     39046   KAWABATA   14 + RUTH
8583-88  ( 627) LINK      15     40227     39357   KAWABATA   15 + RUTH
8589-93  ( 633) LINK      16     33630     33124   KAWABATA   16 + RUTH
8594-00  ( 637) LINK      17     33898     33757   KAWABATA   17 + RUTH
8601-06  ( 640) LINK      18     36715     35987   KAWABATA   18 + RUTH
8607-13  ( 648) LINK      19     40333     39822   KAWABATA   19 + RUTH
8614-19  ( 652) LINK      20     34029     33863   KAWABATA   20 + RUTH
8620-31  ( 633) LINK      21     44263     43132   KAWABATA   21 + RUTH
8599     (   9) LINK      22      7936      7853   KAWEXTAPE 138 + RUTH
8709-22  (1453) LINK      23     37453     37254   KAWABATA   22 + RUTH
8723-26  ( 668) LINK      24     26353     26205   KAWABATA   23 + RUTH
8727-43  (1829) LINK      25     25618     24482   KAWABATA   24 + RUTH
8744-52  (1835) LINK      26     31887     31673   KAWABATA   25 + RUTH
8753-58  (1838) LINK      27     25557     25419   KAWABATA   26 + RUTH
8759-66  (1842) LINK      28     29692     29503   KAWABATA   27 + RUTH
8767-71  (1845) LINK      29     25619     25489   KAWABATA   28 + RUTH
8772-78  (1851) LINK      30     30761     30499   KAWABATA   29 + RUTH
8779-82  ( 238) LINK      31     29927     29676   KAWABATA   30 + RUTH
8783-94  ( 243) LINK      32     28328     28104   KAWABATA   31 + RUTH
8795-99  (1821) LINK      33     27396     27161   KAWABATA   32 + RUTH
8800-07  (1798) LINK      34     27566     27352   KAWABATA   33 + RUTH
8808-14  (1799) LINK      35     32366     32092   KAWABATA   34 + RUTH
8815-20  (1803) LINK      36     30512     30346   KAWABATA   35 + RUTH
8821-24  (1804) LINK      37     26934     26722   KAWABATA   36 + RUTH
8825-31  (1805) LINK      38     29233     28996   KAWABATA   37 + RUTH
8832-37  (1806) LINK      39     30431     30182   KAWABATA   38 + RUTH
8838-42  (1808) LINK      40     29645     29468   KAWABATA   39 + RUTH
8843-46  (1809) LINK      41     27737     27543   KAWABATA   40 + RUTH
8847-50  (1810) LINK      42     27485     27274   KAWABATA   41 + RUTH
8851-55  (1812) LINK      43     27693     27516   KAWABATA   42 + RUTH
8856-62  (1814) LINK      44     27245     27046   KAWABATA   43 + RUTH
8863-67  (1636) LINK      45     34885     34682   KAWABATA   44 + RUTH
8813     ( 947) LINK      46      2200      2187   KAWEXTAPE 139 + RUTH
8868-71  ( 390) LINK      47     22300     22098   KAWABATA   45 + RUTH
8872-76  ( 395) LINK      48     30639     30427   KAWABATA   46 + RUTH
8885-93  (1626) LINK      49     27119     26983   KAWABATA   47 + RUTH
8894-97  (1307) LINK      50     27633     27408   KAWABATA   48 + RUTH
8898-04  (1312) LINK      51     29787     29543   KAWABATA   49 + RUTH
8905-10  ( 410) LINK      52     31255     31041   KAWABATA   50 + RUTH
8911-16  ( 412) LINK      53     32309     32114   KAWABATA   51 + RUTH
8917-22  (1661) LINK      54                       KAWABATA   52 + RUTH
8923-29  ( 416) LINK      55     32304     32074   KAWABATA   53 + RUTH
8930-35  ( 420) LINK      56     30192     29934   KAWABATA   54 + RUTH
8936-41  ( 421) LINK      57     29938     29752   KAWABATA   55 + RUTH
8942-48  (1664) LINK      58     35284     35047   KAWABATA   56 + RUTH
8949-58  (1675) LINK      59     28350     28148   KAWABATA   57 + RUTH
8959-66  ( 388) LINK      60     32022     31822   KAWABATA   58 + RUTH
8967-73  (1946) LINK      61     30371     30070   KAWABATA   59 + RUTH
8974-79  (1948) LINK      62     30961     30727   KAWABATA   60 + RUTH
8980-83  (1952) LINK      63     27145     26962   KAWABATA   61 + RUTH
8957     (1972) LINK      64      5489      5469   KAWEXTAPE 140 + RUTH
8984-89  ( 904) LINK      65     31957     31769   KAWABATA   62 + RUTH
8990-96  (1423) LINK      66     25765     25534   KAWABATA   63 + RUTH
8997-05  (1424) LINK      67     31261     31025   KAWABATA   64 + RUTH
9006-11  (1426) LINK      68     32741     32462   KAWABATA   65 + RUTH
9012-22  (1427) LINK      69     33569     33323   KAWABATA   66 + RUTH
9023-29  (1431) LINK      70     29382     29105   KAWABATA   67 + RUTH
9030-33  (1433) LINK      71     24468     24346   KAWABATA   68 + RUTH
9034-40  (1441) LINK      72     31955     31700   KAWABATA   69 + RUTH
9041-47  ( 296) LINK      74     31864     31542   KAWABATA   70 + RUTH
9048-54  ( 299) LINK      75     21811     21578   KAWABATA   71 + RUTH
9055-59  (1110) LINK      76     30876     30674   KAWABATA   72 + RUTH
9060-66  (1065) LINK      77     26219     25961   KAWABATA   73 + RUTH
9068-72  (1070) LINK      78     34177     33943   KAWABATA   74 + RUTH
8993     (1090) LINK      79      8002      7932   KAWEXTAPE 142 + RUTH
9073-77  ( 539) LINK      80     29337     29082   KAWABATA   75 + RUTH
9078-86  ( 524) LINK      81     22010     21821   KAWABATA   76 + RUTH
9088-95  ( 529) LINK      82     29789     29442   KAWABATA   77 + RUTH
9067     ( 544) LINK      83      1001       991   KAWEXTAPE 143 + RUTH
9096-00  (1899) LINK      84     31957     31683   KAWABATA   78 + RUTH
9101-05  (1903) LINK      85     23669     23368   KAWABATA   79 + RUTH
9106-10  (1908) LINK      86     37778     37681   KAWABATA   80 + RUTH
9111-14  (1911) LINK      87     24218     24044   KAWABATA   81 + RUTH
9115-20  (1917) LINK      88     37740     37513   KAWABATA   82 + RUTH
9121-24  (1919) LINK      89     26363     26211   KAWABATA   83 + RUTH
9125-29  ( 285) LINK      90     31010     30496   KAWABATA   84 + RUTH
9130-39  ( 287) LINK      91     26452     26237   KAWABATA   85 + RUTH
9140-46  (1534) LINK      92     30890     30639   KAWABATA   86 + RUTH
9147-50  (1544) LINK      93     30688     30505   KAWABATA   87 + RUTH
9151-55  (1547) LINK      94     28574     28319   KAWABATA   88 + RUTH
9156-66  (1810) LINK      95     29032     28742   KAWABATA   89 + RUTH
9167-72  (1831) LINK      96     29145     28888   KAWABATA   90 + RUTH
9175-82  (1948) LINK      97     24958     24651   KAWABATA   91 + RUTH
9180     (1961) LINK      98      8002      7978   EXTAPE00  144 + RUTH
9183-89  (1239) LINK      99     33395     33045   KAWABATA   92 + RUTH
9190-94  (1276) LINK     100     26342     26183   KAWABATA   93 + RUTH
9195-01  (1287) LINK     101     25253     24985   KAWABATA   94 + RUTH
9202-07  (1295) LINK     102     32650     32335   KAWABATA   95 + RUTH
9208-12  (1301) LINK     103     26568     26333   KAWABATA   96 + RUTH
9213-17  (1308) LINK     104     31203     31008   KAWABATA   97 + RUTH
9218-25  (1315) LINK     105     42349     41903   KAWABATA   98 + RUTH
9226-29  (1323) LINK     106     26241     26043   KAWABATA   99 + RUTH
9230-35  (1326) LINK     107     26723     26508   KAWABATA  100 + RUTH
9236-40  (1329) LINK     108     30457     30258   KAWABATA  101 + RUTH
9241-45  (1333) LINK     109     28218     28003   KAWABATA  102 + RUTH
9246-50  (1339) LINK     110     32106     31834   KAWABATA  103 + RUTH
9251-55  (1196) LINK     111     31600     31333   KAWABATA  104 + RUTH
9256-61  (1202) LINK     112     28774     28567   KAWABATA  105 + RUTH
9262-65  (1213) LINK     113     24836     24648   KAWABATA  106 + RUTH
9266-70  (1250) LINK     114     30650     30426   KAWABATA  107 + RUTH
9271-75  (1310) LINK     115     29204     28959   KAWABATA  108 + RUTH
9276-80  (1409) LINK     116     25844     25571   KAWABATA  109 + RUTH
9281-86  (782 ) LINK     117     31828     31575   KAWABATA  110 + RUTH
9287-92  (792 ) LINK     118     28712     28529   KAWABATA  111 + RUTH
9293-04  (1746) LINK     119     28576     28404   KAWABATA  112 + RUTH
9305-15  (1761) LINK     120     32150     31847   KAWABATA  113 + RUTH
9316-21  ( 871) LINK     121     26886     26694   KAWABATA  114 + RUTH
9322-26  (1775) LINK     122     35578     36361   KAWABATA  115 + RUTH
9327-31  (1782) LINK     123     31791     31550   KAWABATA  116 + RUTH
9332-35  ( 326) LINK     124     26826     26657   KAWABATA  117 + RUTH
9336-41  ( 338) LINK     125     32410     32200   KAWABATA  118 + RUTH
9342-48  ( 340) LINK     126     31229     30889   KAWABATA  119 + RUTH
9349-53  ( 344) LINK     127     26689     26528   KAWABATA  120 + RUTH
9354-58  ( 346) LINK     128     35103     34904   KAWABATA  121 + RUTH
9257     ( 374) LINK     129       858       851   KAWEXTAPE 145 + RUTH
9301     ( 395) LINK     130      3269      3248   KAWEXTAPE 146 + RUTH
9359-61  ( 350) LINK     129     24001     23818   KAWABATA  122 + RUTH
9362-68  ( 353) LINK     130     31115     30864   KAWABATA  123 + RUTH
9369-77  ( 356) LINK     133     32957     32733   KAWABATA  124 + RUTH
9378-82  ( 362) LINK     134     31186     30989   KAWABATA  125 + RUTH
9383-89  ( 366) LINK     135     27112     26936   KAWABATA  126 + RUTH
9390-94  ( 196) LINK     136     27585     27343   KAWABATA  127 + RUTH
9395-04  ( 200) LINK     137     29344     29156   KAWABATA  128 + RUTH
9405-10  (1025) LINK     138     34734     34562   KAWABATA  129 + RUTH
9411-17  (1028) LINK     139     29650     29068   KAWABATA  130 + RUTH
9418-23  (1029) LINK     140     27342     27094   KAWABATA  131 + RUTH
9424-29  (1796) LINK     141     34803     34510   KAWABATA  132 + RUTH
9430-34  (1801) LINK     142     24153     23903   KAWABATA  133 + RUTH
9435-49  (1807) LINK     143     25073     24856   KAWABATA  134 + RUTH
9450-54  (1817) LINK     144     34927     34726   KAWABATA  135 + RUTH
9455-58  (1828) LINK     145     25262     25117   KAWABATA  136 + RUTH
9459-63  (1833) LINK     146     32482     32323   KAWABATA  137 + RUTH
9464-70  (1838) LINK     147     34241     34094   KAWABATA  138 + RUTH
9471-75  (1842) LINK     148     32291     32044   KAWABATA  139 + RUTH
9476-80  (1847) LINK     149     27770     27634   KAWABATA  140 + RUTH
9481-85  (1850) LINK     150     34813     34625   KAWABATA  141 + RUTH
9486-89  (1871) LINK     151     27343     27168   KAWABATA  142 + RUTH
9490-94  (1875) LINK     152     27541     27360   KAWABATA  143 + RUTH
9495-99  ( 829) LINK     153     31107     30873   KAWABATA  144 + RUTH
9500-04  ( 832) LINK     154     28658     28410   KAWABATA  145 + RUTH
9505-10  ( 839) LINK     155     27608     27280   KAWABATA  146 + RUTH
9511-15  ( 840) LINK     156     29377     29128   KAWABATA  147 + RUTH
9516-21  (1104) LINK     157     31227     31026   KAWABATA  148 + RUTH
9522-28  (1410) LINK     158     27250     26953   KAWABATA  149 + RUTH
9529-32  (1412) LINK     159     25156     24751   KAWABATA  150 + RUTH
9533-39  (1420) LINK     160     36611     36419   KAWABATA  151 + RUTH
9540-44  (1770) LINK     161     26678     26524   KAWABATA  152 + RUTH
9545-50  ( 344) LINK     162     28838     28579   KAWABATA  153 + RUTH
9564-76  ( 348) LINK     163     27426     27102   KAWABATA  154 + RUTH
9577-82  ( 349) LINK     164     34036     33797   KAWABATA  155 + RUTH
9583-86  ( 354) LINK     165     25098     24967   KAWABATA  156 + RUTH
9587-91  ( 358) LINK     166     37283     37002   KAWABATA  157 + RUTH
9592-01  ( 360) LINK     167     59009     58687   KAWABATA  158 + RUTH
9602-06  ( 363) LINK     168     33211     33013   KAWABATA  159 + RUTH
9607-11  ( 364) LINK     169     36385     36149   KAWABATA  160 + RUTH
9612-16  ( 367) LINK     170     29676     29402   KAWABATA  161 + RUTH
9536     ( 384) LINK     171      7002      6971   KAWEXTAPE 147 + RUTH
9617-20  ( 216) LINK     172     26423     26252   KAWABATA  162 + RUTH
9621-23, ( 218) LINK     173     30586     30339   KAWABATA  163 + RUTH
9628-30
9631-35  ( 219) LINK     174     31349     31079   KAWABATA  164 + RUTH
9578     ( 225) LINK     175      6628      6592   KAWEXTAPE 148 + RUTH
8877-84  ( 667) LINK     176     35827     35557   KAWEXTAPE 149 + RUTH
9636-39  (  56) LINK     178     26089     25925   KAWABATA  165 + RUTH
9640-44  (1748) LINK     179     28072     27829   KAWABATA  166 + RUTH
9645-47  ( 302) LINK     180     19982     19550   KAWABATA  167 + RUTH
9648-54  ( 331) LINK     181     29812     29422   KAWABATA  168 + RUTH
9657-60  ( 343) LINK     182     28065     27875   KAWABATA  169 + RUTH
9661-65  ( 352) LINK     183     29676     29376   KAWABATA  170 + RUTH
9666-70  ( 381) LINK     184     33150     32816   KAWABATA  171 + RUTH
9671-75  ( 395) LINK     185     26672     26454   KAWABATA  172 + RUTH
9677-81  ( 429) LINK     186     27547     27304   KAWABATA  173 + RUTH
9682-85  ( 532) LINK     187     31681     31446   KAWABATA  174 + RUTH
9686-91  ( 535) LINK     188     29173     28915   KAWABATA  175 + RUTH
9692-97  ( 540) LINK     189     40690     40477   KAWABATA  176 + RUTH
9698-05  ( 547) LINK     190     29756     29573   KAWABATA  177 + RUTH
9706-10  ( 559) LINK     191     32349     32124   KAWABATA  178 + RUTH
9711-17  ( 565) LINK     192     26879     26652   KAWABATA  179 + RUTH
9655     ( 821) LINK     193      8002      7977   KAWEXTAPE 150 + RUTH
9656     ( 823) LINK     194      4366      4339   KAWEXTAPE 151 + RUTH
9676     ( 838) LINK     195      2379      2367   KAWEXTAPE 152 + RUTH
9713     ( 872) LINK     196      3830      3816   KAWEXTAPE 153 + RUTH
9718-22  (1382) LINK     197     27964     27718   KAWABATA  180 + RUTH
9723-26  (    ) LINK     198      7180      7044   EXDUMP   1237
97  -    (    ) LINK     1                         KAWABATA  1   + RUTH
  -------------------------------------------------
  THE FOLLOWING RUNS ARE WRITEN DOWN IN THE MEMBER RUNLST1
  --------------------------------------------------
    29/03/82 311181305  MEMBER NAME  RUNLST1  (JADESR)      TEXT

                  ================================
                  JADE DATA TAPES SINCE THE 1.2.82
                  ================================

    THE DATA GENERATION GROUPS FOR THE IBM COPY AND REFORMATTED TAPES
    ARE RESPECTIVELY:

         F22YEN.JADE.EXDATA05.G0182V00, G0183V00 ETC.
    AND  F11LHO.JDATA06.REFORM.G0200V00, G0201V00 ETC.

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I===I===I====I======I====I=====I=====I=================================
I182I200ILINKI939423I1851I31764I31726I 9727-28,10000-10003
I   I   I    I      I    I     I     I 9729-9999 DON'T EXIST
I---I---I----I------I----I-----I-----I---------------------------------
I183I201ILINKI939424I1103I24243I24217I 0004-13
I---I---I----I------I----I-----I-----I---------------------------------
I184I202ILINKI939425I1105I23261I23236I 0014,0033-36
I   I   I    I      I    I     I     I 0015-32   DON'T EXIST
I---I---I----I------I----I-----I-----I---------------------------------
I185I203ILINKI939426I 796I     I     I
I   I   I    I      I    I     I     I 0038-40   DON'T EXIST
I   I   I    I      I    I     I     I 0041-47   COSMIC RUNS
I---I---I----I------I----I-----I-----I---------------------------------
I186I204ILINKI939427I 798I     I     I
I---I---I----I------I----I-----I-----I---------------------------------

    THE ABOVE TAPES DON'T CONTAIN GOOD DATA. THEY ARE NOT SEND TO RUTH.

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I===I===I====I======I====I=====I=====I=================================
I187I205ILINKI939428I  30I23843I23813I 0055-62
I---I---I----I------I----I-----I-----I---------------------------------
I188I206ILINKI939429I  35I32269I32222I 0063-68,70,75
I   I   I    I      I    I     I     I 0069      DON'T EXIST
I   I   I    I      I    I     I     I 0071-74   PULSER RUNS
I---I---I----I------I----I-----I-----I---------------------------------
I189I207ILINKI939430I  36I23789I23750I 0076-81
I---I---I----I------I----I-----I-----I---------------------------------
I190I208ILINKI939431I  71I24351I24334I OO82-89
I---I---I----I------I----I-----I-----I---------------------------------
I191I209ILINKI939432I 803I27738I27708I 0090-96,98,99
I   I   I    I      I    I     I     I 0097      DON'T EXIST
I---I---I----I------I----I-----I-----I---------------------------------
I192I210ILINKI939433I 818I33259I33201I 0100-102,104-111
I   I   I    I      I    I     I     I 0103      IS ON TAPE 939446
I   I   I    I      I    I     I     I 0103-07   TRIGGER STUDY
I---I---I----I------I----I-----I-----I---------------------------------
I193I211ILINKI939434I 811I23208I23181I 0112-115
I---I---I----I------I----I-----I-----I---------------------------------
I194I212ILINKI939435I 822I28064I28051I 0116-119
I---I---I----I------I----I-----I-----I---------------------------------
I195I213ILINKI939436I 837I37438I37393I 0120-124
I---I---I----I------I----I-----I-----I---------------------------------
I196I214ILINKI939437I 859I33700I33677I 0125-130,132,133
I   I   I    I      I    I     I     I 0131      DON'T EXIST
I---I---I----I------I----I-----I-----I---------------------------------
I197I215ILINKI939438I 864I33665I33640I 0134-139
I---I---I----I------I----I-----I-----I---------------------------------
I198I216ILINKI939439I 866I28885I28828I 0140-145
I---I---I----I------I----I-----I-----I---------------------------------
I199I217ILINKI939440I 870I34738I34673I 0146-151
I---II--I----I------I----I-----I-----I---------------------------------
I200I218ILINKI939441I 875I35729I35675I 0152-157
I---I---I----I------I----I-----I-----I---------------------------------
I201I219ILINKI939442I 879I31895I31860I 0158-162
I---I---I----I------I----I-----I-----I---------------------------------
I202I220ILINKI939443I 882I29664I29619I 0163-166,168,169
I   I   I    I      I    I     I     I 0167      DON'T EXIST
I---I---I----I------I----I-----I-----I---------------------------------
I203I221ILINKI939444I1332I36366I36310I 170-176
I---I---I----I------I----I-----I-----I---------------------------------
I204I222ILINKI939445I1334I33326I33277I 177-182
I   I   I    I      I    I     I     I 0180      DON'T EXIST
I---I---I----I------I----I-----I-----I---------------------------------
I154I223ITAPEI939446I1354I 1312I 1312I 103
I---I---I----I------I----I-----I-----I---------------------------------
I205I224ILINKI939447I 662I35413I35365I 183-188
I---I---I----I------I----I-----I-----I---------------------------------
I206I225ILINKI939448I 636I32016I31982I 189-195
I   I   I    I      I    I     I     I 0194      NO MAGNET FIELD
I---I---I----I------I----I-----I-----I---------------------------------
I207I226ILINKI939449I 642I26659I26637I 196-203
I   I   I    I      I    I     I     I 0201-02   TEST RUN
I---I---I----I------I----I-----I-----I---------------------------------
I208I227ILINKI939450I 644I31700I31653I 204-208
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939428-939450 WERE SENT TO RUTHERFORD ON 18.2.82
   FL.#  PA 101
   AIR.#  026-03956035

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I===I===I====I======I====I=====I=====I=================================
I209I228ILINKI939451I1152I31323I31270I 209,210,212,213,216,217
I   I   I    I      I    I     I     I 211,214,215 DUMMY
I---I---I----I------I----I-----I-----I---------------------------------
I210I229ILINKI939452I1818I30556I30499I 218,219,224-229
I---I---I----I------I----I-----I-----I---------------------------------
I211I230ILINKI939453I1821I26429I26397I 230-233
I---I---I----I------I----I-----I-----I---------------------------------
I212I231ILINKI939454I1824I39065I39015I 234-238
I---I---I----I------I----I-----I-----I---------------------------------
I213I232ILINKI939455I1826I31257I31127I 239-249,251
I   I   I    I      I    I     I     I 246-50 TEST RUNS
I---I---I----I------I----I-----I-----I---------------------------------
I155I233ITAPEI939456I1843I 8002I 7990I 222
I---I---I----I------I----I-----I-----I---------------------------------
I156I234ITAPEI939457I1849I 8002I 7988I 223
I---I---I----I------I----I-----I-----I---------------------------------
I157I235ITAPEI939622I1855I 6569I 6557I 227    NEW RUTH TAPE!!!
I---I---I----I------I----I-----I-----I---------------------------------
I214I236ILINKI939459I  88I31068I31027I252-57
I---I---I----I------I----I-----I-----I---------------------------------
I215I237ILINKI939460I  89I29066I29001I258-62
I---I---I----I------I----I-----I-----I---------------------------------
I216I238ILINKI939461I  90I33862I33764I263-73 RUN#63 ENTERS
I---I---I----I------I----I-----I-----I---------------------------------
I217I239ILINKI939462I  98I18265I18218I274-78
I---I---I----I------I----I-----I-----I---------------------------------
I218I240ILINKI939463I 100I33186I33115I279-87,89-99
I                                    I 288 LOST
I---I---I----I------I----I-----I-----I---------------------------------
I219I241ILINKI939464I 108I28355I28279I300-13
I---I---I----I------I----I-----I-----I---------------------------------
I220I242ILINKI939465I 113I37189I37114I314-20
I---I---I----I------I----I-----I-----I---------------------------------
I221I243ILINKI939466I 124I31456I31367I321-26
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939451-939466 WERE SENT TO RUTHERFORD ON 26.2.82
   FL.#  PA 101
   AIR.#  026-03956050

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I===I===I====I======I====I=====I=====I=================================
I222I244ILINKI939467I1907I29813I29735I327-33
I---I---I----I------I----I-----I-----I---------------------------------
I223I245ILINKI939468I1916I32770I32702I334-43
I---I---I----I------I----I-----I-----I---------------------------------
I224I246ILINKI939469I 594I32418I32352I344-46,49,50
I                                    I 347-348 DON'T EXIST
I                                    I 351 NORD TAPE
I---I---I----I------I----I-----I-----I---------------------------------
I225I247ILINKI939470I 598I40199I40124I352-57
I---I---I----I------I----I-----I-----I---------------------------------
I226I248ILINKI939471I 190I31883I31819I358-70
I---I---I----I------I----I-----I-----I---------------------------------
I227I249ILINKI939472I 195I28704I28631I371-75
I---I---I----I------I----I-----I-----I---------------------------------
I228I250ILINKI939473I 416I31923I31803I376-79,82-84
I                                    I 380-381 DON'T EXIST
I---I---I----I------I----I-----I-----I---------------------------------
I229I251ILINKI939474I 560I33848I33765I385-88,97,98,401
I                                    I 389-396 DON'T EXIST
I                                    I 399-400 ID PULSER
I---I---I----I------I----I-----I-----I---------------------------------
I230I252ILINKI939475I1245I33933I33852I402-09
I---I---I----I------I----I-----I-----I---------------------------------
I231I253ILINKI939476I1253I28167I28107I410-15
I---I---I----I------I----I-----I-----I---------------------------------
I232I254ILINKI939477I1258I32838I32709I416-23
I---I---I----I------I----I-----I-----I---------------------------------
I233I255ILINKI939478I1264I33744I33605I424-33
I---I---I----I------I----I-----I-----I---------------------------------
I234I256ILINKI939479I1267I33046I32941I434,440-45
I                                    I 435-437 JUNK
I                                    I 438-439 NORD TAPE
I---I---I----I------I----I-----I-----I---------------------------------
I235I257ILINKI939480I1269I30621I30550I446-51
I---I---I----I------I----I-----I-----I---------------------------------
I236I258ILINKI939481I1271I35237I35143I452-57
I---I---I----I------I----I-----I-----I---------------------------------
I237I259ILINKI939482I1275I37032I36927I458-63
I---I---I----I------I----I-----I-----I---------------------------------
I238I260ILINKI939483I1279I34707I34661I464-70
I---I---I----I------I----I-----I-----I---------------------------------
I239I261ILINKI939484I1281I30311I30134I471-76
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939467-939484 WERE SENT TO RUTHERFORD ON  8.3.82
   FL.#  LH0 40
   AIR.#  220-60585280



   JDATA07 .. REFORMATTED GENERATION GROUP 7 STARTS HERE.


I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I 06I 07I    I      I    I     I     I
I===I===I====I======I====I=====I=====I=================================
I  1I  1ILINKI939485I1919I30461I30404I477-81
I---I---I----I------I----I-----I-----I---------------------------------
I  2I  2ILINKI939486I1184I25438I25349I482-86
I                                    I 487-490 COSMIC ON TAPE
I---I---I----I------I----I-----I-----I---------------------------------
I  3I  3ILINKI939487I1195I36577I36365I491-502
I---I---I----I------I----I-----I-----I---------------------------------
I  4I  4ILINKI939488I1200I32770I32705I503-09
I---I---I----I------I----I-----I-----I---------------------------------
I  5I  5ILINKI939489I1207I33042I32957I510-14
I---I---I----I------I----I-----I-----I---------------------------------
I  6I  6ILINKI939491I1216I33014I32932I515-19
I---I---I----I------I----I-----I-----I---------------------------------
I  7I  7ILINKI939492I1217I34951I34793I520-27,33,34
I                                    I 528-531 JUNK
I                                    I 532 NORD TAPE
I---I---I----I------I----I-----I-----I---------------------------------
I  8I  8ILINKI939493I1220I33247I33178I535-37,40-42
I                                    I 538     JUNK
I                                    I 539 NORD TAPE
I---I---I----I------I----I-----I-----I---------------------------------
I  9I  9ILINKI939494I  82I30536I30480I543-47
I---I---I----I------I----I-----I-----I---------------------------------
I 10I 10ILINKI939495I  87I31565I31473I548-52
I---I---I----I------I----I-----I-----I---------------------------------
I158I 11ITAPEI939496I 723I 8686I 8668I220-21
I---I---I----I------I----I-----I-----I---------------------------------
I159I 12ITAPEI939497I 739I 7874I 7839I438-39
I---I---I----I------I----I-----I-----I---------------------------------
I160I 13ITAPEI939498I 755I  538I  538I532
I---I---I----I------I----I-----I-----I---------------------------------
I161I 14ITAPEI939499I 771I 7030I 7023I539
I---I---I----I------I----I-----I-----I---------------------------------
I 11I 15ILINKI939500I1872I22939I22894I553-56
I                                                I 554 NORD TAPE
I---I---I----I------I----I-----I-----I---------------------------------
I 12I 16ILINKI939501I1881I30159I30068I557-62
I---I---I----I------I----I-----I-----I---------------------------------
I 13I 17ILINKI939502I1890I26513I26451I563-67
I---I---I----I------I----I-----I-----I---------------------------------
I 14I 18ILINKI939503I1895I31935I31894I568,70-73
I                                                I 569 DUMMY
I---I---I----I------I----I-----I-----I---------------------------------
I 15I 19ILINKI939504I1941I25455I25409I574-77
I---I---I----I------I----I-----I-----I---------------------------------
I 16I 20ILINKI939505I1946I30910I30850I578-84
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939485-939505 WERE SENT TO RUTHERFORD ON 12.3.82
   FL.#  LH 040
   AIR.#  60585512

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I 06I 07I    I      I    I     I     I
I===I===I====I======I====I=====I=====I=================================
I162I 21ILINKI939506I1151I 1844I 1840I554
I---I---I----I------I----I-----I-----I---------------------------------
I 17I 22ILINKI939507I1154I29943I29880I585-92
I---I---I----I------I----I-----I-----I---------------------------------
I 18I 23ILINKI939508I1155I32058I31995I593-97
I---I---I----I------I----I-----I-----I---------------------------------
I 19I 24ILINKI939509I 294I27827I27778I598-603
I---I---I----I------I----I-----I-----I---------------------------------
I 20I 25ILINKI939510I1013I39563I39516I604-11
I---I---I----I------I----I-----I-----I---------------------------------
I 21I 26ILINKI939511I1201I25761I25707I612-17
I---I---I----I------I----I-----I-----I---------------------------------
I 22I 27ILINKI939512I1223I27062I26980I618,20,22-30
I                                                I 619,621 NORD TAPES
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939506-939512 WERE SENT TO RUTHERFORD ON 16.3.82
   FL.#   040/7150244
   AIR.#  220-60585615

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I 06I 07I    I      I    I     I     I
I===I===I====I======I====I=====I=====I=================================
I 23I 28ILINKI939513I 425I28274I28185I631-35,38,39
I                                                I 636,637 NORD TAPES
I---I---I----I------I----I-----I-----I---------------------------------
I 24I 29ILINKI939514I 427I29137I29089I640-46
I---I---I----I------I----I-----I-----I---------------------------------
I 25I 30ILINKI939515I 442I27506I27459I647-50,52
I                                                I 651 DUMMY
I---I---I----I------I----I-----I-----I---------------------------------
I 26I 31ILINKI939516I 446I31358I31278I653-58
I---I---I----I------I----I-----I-----I---------------------------------
I 27I 32ILINKI939517I 456I26614I26563I659-63
I---I---I----I------I----I-----I-----I---------------------------------
I 28I 33ILINKI939518I 466I32147I32085I664,67-71
I                                                I 666 NORD TAPE
I                                                I 665 DUMMY
I---I---I----I------I----I-----I-----I---------------------------------
I163I 34ITAPEI939519I1392I 4069I 4028I619
I---I---I----I------I----I-----I-----I---------------------------------
I164I 35ITAPEI939520I1400I 1243I 1234I621
I---I---I----I------I----I-----I-----I---------------------------------
I165I 36ITAPEI939521I1404I 5918I 5912I636
I---I---I----I------I----I-----I-----I---------------------------------
I166I 37ITAPEI939522I1406I 8002I 7991I637
I---I---I----I------I----I-----I-----I---------------------------------
I167I 38ITAPEI939523I1413I 7997I 7986I666
I---I---I----I------I----I-----I-----I---------------------------------
I168I 39ITAPEI939524I1417I 1914I 1911I683
I---I---I----I------I----I-----I-----I---------------------------------
I169I 40ITAPEI939525I1421I 1014I 1013I686
I---I---I----I------I----I-----I-----I---------------------------------
I170I 41ITAPEI939526I1433I 1597I 1587I684
I---I---I----I------I----I-----I-----I---------------------------------
I 29I 42ILINKI939527I 251I27748I27694I672-76
I---I---I----I------I----I-----I-----I---------------------------------
I 30I 43ILINKI939528I 257I27853I27798I677-80,82,85,87,89
I                                                I 683,684,686 NORD TAPE
I                                                I 688 NORD TAPE
I                                                I 681,682 DUMMY
I---I---I----I------I----I-----I-----I---------------------------------
I 31I 44ILINKI939529I 261I28490I28428I690-94
I---I---I----I------I----I-----I-----I---------------------------------
I 32I 45ILINKI939530I 263I32072I32024I695-99
I---I---I----I------I----I-----I-----I---------------------------------
I 33I 46ILINKI939531I1269I27440I27386I700-02,05-08
I                                                I 704 NORD TAPE
I                                                I 703 DUMMY
I---I---I----I------I----I-----I-----I---------------------------------
I 34I 47ILINKI939532I1270I27830I27769I709-14
I---I---I----I------I----I-----I-----I---------------------------------
I171I 48ITAPEI939533I1278I 8002I 7989I688
I---I---I----I------I----I-----I-----I---------------------------------
I172I 49ITAPEI939534I1280I 7997I 7981I704
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939513-939534 WERE SENT TO RUTHERFORD ON 19.3.82
   FL.#   AS 880
   AIR.#  057-36049204

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I 06I 07I    I      I    I     I     I
I===I===I====I======I====I=====I=====I=================================
I 35I 50ILINKI939535I1820I26893I26835I715-20
I                                                I 721 DUMMY
I---I---I----I------I----I-----I-----I---------------------------------
I 36I 51ILINKI939536I1830I30688I30621I722-27
I---I---I----I------I----I-----I-----I---------------------------------
I 37I 52ILINKI939537I1838I24378I24291I728-34
I---I---I----I------I----I-----I-----I---------------------------------
I 38I 53ILINKI939538I1841I27575I27509I735-42
I---I---I----I------I----I-----I-----I---------------------------------
I 39I 54ILINKI939539I 293I30720I30656I743-49
I---I---I----I------I----I-----I-----I---------------------------------
I 40I 55ILINKI939540I 296I33228I33174I750-55
I---I---I----I------I----I-----I-----I---------------------------------
I 41I 56ILINKI939541I 299I27143I27075I756-61
I---I---I----I------I----I-----I-----I---------------------------------
I 42I 57ILINKI939542I 302I24152I24090I762-64,68-70
I                                                I 765,766 DUMMY
I                                                I 767 NORD TAPE
I---I---I----I------I----I-----I-----I---------------------------------
I 43I 58ILINKI939543I1235I32115I32046I771-75
I---I---I----I------I----I-----I-----I---------------------------------
I 44I 59ILINKI939544I1237I26665I26614I776-81
I                                                I 782 NORD TAPE
I---I---I----I------I----I-----I-----I---------------------------------
I 45I 60ILINKI939545I1238I30192I30146I783-86
I---I---I----I------I----I-----I-----I---------------------------------
I 46I 61ILINKI939546I1239I29731I29684I787-90
I---I---I----I------I----I-----I-----I---------------------------------
I 47I 62ILINKI939547I 187I27721I27665I791-94,96
I                                                I 795 NORD TAPE
I---I---I----I------I----I-----I-----I---------------------------------
I 48I 63ILINKI939548I 192I28885I28821I797-801
I---I---I----I------I----I-----I-----I---------------------------------
I 49I 64ILINKI939549I 197I27774I27726I802-05
I---I---I----I------I----I-----I-----I---------------------------------
I 50I 65ILINKI939550I 209I29070I29009I806-08,11,12
I                                                I 809 DUMMY
I                                                I 810 NORD TAPE
I                                                I 813 NORD TAPE
I---I---I----I------I----I-----I-----I---------------------------------
I 51I 66ILINKI939551I 218I37008I36941I814-18
I---I---I----I------I----I-----I-----I---------------------------------
I 52I 67ILINKI939552I 223I27691I27652I819-22
I---I---I----I------I----I-----I-----I---------------------------------
I 53I 68ILINKI939553I 229I28826I28777I823-26
I---I---I----I------I----I-----I-----I---------------------------------
I 54I 69ILINKI939554I 242I34248I34187I827-31
I---I---I----I------I----I-----I-----I---------------------------------
I173I 70ILINKI939555I 268I 4017I 4013I767
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939535-939548,50,52,53,55 WERE SENT TO RUTHERFORD
                                              ON 23.3.82
   FL.#   PA 101
   AIR.#  026-03956116

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I 06I 07I    I      I    I     I     I
I===I===I====I======I====I=====I=====I=================================
I 55I 71ILINKI939556I1687I23055I23026I 832-36
I---I---I----I------I----I-----I-----I---------------------------------
I 56I 72ILINKI939557I1699I29042I28967I 837-39,42-44
I                                                I 840,841 NORD TAPE
I---I---I----I------I----I-----I-----I---------------------------------
I 57I 73ILINKI939558I1703I31830I31754I 845-49
I---I---I----I------I----I-----I-----I---------------------------------
I 58I 74ILINKI939559I 685I33297I33243I 850-54
I---I---I----I------I----I-----I-----I---------------------------------
I 59I 75ILINKI939560I 688I26143I26089I 855,57-60
I                                                I 856 DUMMY
I---I---I----I------I----I-----I-----I---------------------------------
I 60I 76ILINKI939561I 692I25060I25007I 861-63,66
I                                                I 864,865 DUMMY
I---I---I----I------I----I-----I-----I---------------------------------
I174I 77ITAPEI939562I 698I 1180I 1176I 782
I---I---I----I------I----I-----I-----I---------------------------------
I175I 78ITAPEI939563I 700I 8002I 7987I 795
I---I---I----I------I----I-----I-----I---------------------------------
I176I 79ITAPEI939564I 711I 8002I 7986I 840
I---I---I----I------I----I-----I-----I---------------------------------
I177I 80ITAPEI939565I 715I 1018I 1018I 841
I---I---I----I------I----I-----I-----I---------------------------------
I 61I 81ILINKI939566I1802I28584I28528I 867,68,70-75
I                                                I 869 NORD TAPE
I---I---I----I------I----I-----I-----I---------------------------------
I 62I 82ILINKI939567I1808I31242I31188I 876-82
I---I---I----I------I----I-----I-----I---------------------------------
I 63I 83ILINKI939568I1809I31906I31831I 883-87
I---I---I----I------I----I-----I-----I---------------------------------
I 64I 84ILINKI939569I1810I34430I34381I 888-92
I---I---I----I------I----I-----I-----I---------------------------------
I178I 85ITAPEI939570I1819I 1864I 1863I 869
I---I---I----I------I----I-----I-----I---------------------------------
I179I 86ITAPEI939571I1972I 2782I 2776I 810
I---I---I----I------I----I-----I-----I---------------------------------
I180I 87ITAPEI939572I1294I 7871I 7862I 813
I---I---I----I------I----I-----I-----I---------------------------------
I181I 88ITAPEI939573I1310I 5536I 5521I 896
I---I---I----I------I----I-----I-----I---------------------------------
I182I 89ITAPEI939575I1317I 5041I 5037I 898
I---I---I----I------I----I-----I-----I---------------------------------
I 65I 90ILINKI939576I1321I21295I21262I 893,94,97,99,900
I                                                I 895 DUMMY
I                                                I 896,898 NORD TAPE
I---I---I----I------I----I-----I-----I---------------------------------
I 66I 91ILINKI939577I1323I31062I30998I 901-04
I---I---I----I------I----I-----I-----I---------------------------------
I 67I 92ILINKI939578I1327I26982I26955I 905-07,09,10
I                                                I 908 NORD TAPE
I---I---I----I------I----I-----I-----I---------------------------------
I 68I 93ILINKI939579I1330I29566I29511I 911-14
I---I---I----I------I----I-----I-----I---------------------------------
I 69I 94ILINKI939580I 645I32936I32869I 915-19
I---I---I----I------I----I-----I-----I---------------------------------
I 70I 95ILINKI939581I 650I27584I27531I 920-23,26,27
I                                                I 924 DUMMY
I                                                I 925 NORD TAPE
I---I---I----I------I----I-----I-----I---------------------------------
I 71I 96ILINKI939582I 653I26992I26932I 928-32,34,35
I                                                I 933 NORD TAPE
I---I---I----I------I----I-----I-----I---------------------------------
I 72I 97ILINKI939583I 656I27995I27772I 936,38,39,41-43,45-49
I                                                I 944 DUMMY
I                                                I 937,940 NORD TAPE
I---I---I----I------I----I-----I-----I---------------------------------
I 73I 98ILINKI939584I 659I31785I31732I 950,53-56
I                                                I 951,952 NORD TAPE
I---I---I----I------I----I-----I-----I---------------------------------
I 74I 99ILINKI939585I 667I25953I25900I 957-59,61
I                                                I 960 NORD TAPE
I---I---I----I------I----I-----I-----I---------------------------------
I 75I100ILINKI939586I 670I32257I32203I 962-67
I---I---I----I------I----I-----I-----I---------------------------------
I183I101ITAPEI939587I 676I 8002I 7987I 908
I---I---I----I------I----I-----I-----I---------------------------------
I184I102ITAPEI939588I 684I 7486I 7466I 925
I---I---I----I------I----I-----I-----I---------------------------------
I185I103ITAPEI939589I 686I 8002I 7991I 933
I---I---I----I------I----I-----I-----I---------------------------------
I186I104ITAPEI939590I 690I 1151I 1149I 952
I---I---I----I------I----I-----I-----I---------------------------------
I187I105ITAPEI939591I 494I 8002I 7978I 951
I---I---I----I------I----I-----I-----I---------------------------------
I188I106ITAPEI939592I 506I 6650I 6646I 940
I---I---I----I------I----I-----I-----I---------------------------------
I189I107ITAPEI939593I 519I 8002I 7987I 937
I---I---I----I------I----I-----I-----I---------------------------------
I190I108ITAPEI939594I 530I 8002I 7992I 960
I---I---I----I------I----I-----I-----I---------------------------------
I191I109ITAPEI939595I 541I 4967I 4961I 973
I---I---I----I------I----I-----I-----I---------------------------------
I 76I110ILINKI939596I 656I37026I36990I 968-72
I---I---I----I------I----I-----I-----I---------------------------------
I
I   END OF THE MARCH/APRIL 1982 RUN PERIOD !!!!!!!
I
I   START OF THE MAY/JUNE  1982 RUN PERIOD !!!!!!!
I
I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I 06I 07I    I      I    I     I     I
I===I===I====I======I====I=====I=====I=================================
I 77I111ILINKI939598I 819I39043I38881I 11037-41,45,48-53
I---I---I----I------I----I-----I-----I---------------------------------
I 78I112ILINKI939599I 823I29120I28948I 11054-57
I---I---I----I------I----I-----I-----I---------------------------------
I 79I113ILINKI939600I 699I35851I35667I 11058-63
I---I---I----I------I----I-----I-----I---------------------------------
I 80I114ILINKI939601I 771I27320I27146I 11064-72
I---I---I----I------I----I-----I-----I---------------------------------
I 81I115ILINKI939602I 782I33182I32953I 11073-77,79,80
I---I---I----I------I----I-----I-----I---------------------------------
I 82I116ILINKI939603I 784I34777I34688I 11081,87-91
I---I---I----I------I----I-----I-----I---------------------------------
I 83I117ILINKI939604I1909I28484I28399I 11092-95
I---I---I----I------I----I-----I-----I---------------------------------
I 84I118ILINKI939605I1910I29913I29779I 11096-103
I---I---I----I------I----I-----I-----I---------------------------------
I 85I119ILINKI939606I1914I32896I32787I 11104,107-112
I---I---I----I------I----I-----I-----I---------------------------------
I 86I120ILINKI939607I 704I30069I30002I 11113-117
I---I---I----I------I----I-----I-----I---------------------------------
I 87I121ILINKI939608I 709I28838I28776I 11118-123
I---I---I----I------I----I-----I-----I---------------------------------
I 88I122ILINKI939609I 713I32820I32602I 11124-130
I---I---I----I------I----I-----I-----I---------------------------------
I 89I123ILINKI939610I 715I29573I29314I 11131-135
I---I---I----I------I----I-----I-----I---------------------------------
I 90I124ILINKI939611I 717I32549I32343I 11136-140
I---I---I----I------I----I-----I-----I---------------------------------
I 91I125ILINKI939612I 722I27050I26879I 11141-145
I---I---I----I------I----I-----I-----I---------------------------------
I 92I126ILINKI939613I 724I27455I27096I 11146-149
I---I---I----I------I----I-----I-----I---------------------------------
I 93I127ILINKI939614I 725I18450I18142I 11150-155
I---I---I----I------I----I-----I-----I---------------------------------
I 94I128ILINKI939615I 729I30244I30120I 11156-159
I---I---I----I------I----I-----I-----I---------------------------------
I 95I129ILINKI939616I 731I31231I31070I 11160-167
I---I---I----I------I----I-----I-----I---------------------------------
I 96I130ILINKI939617I  98I28936I28875I 11168-174
I---I---I----I------I----I-----I-----I---------------------------------
I 97I131ILINKI939618I  99I34539I34296I 11175-178,180,181,183,184
I---I---I----I------I----I-----I-----I---------------------------------
I 98I132ILINKI939619I 104I21180I20809I 11185-190
I---I---I----I------I----I-----I-----I---------------------------------
I 99I133ILINKI939620I1099I23476I23072I 11191-193
I---I---I----I------I----I-----I-----I---------------------------------
I100I134ILINKI939621I1121I25043I24574I 11194-197
I---I---I----I------I----I-----I-----I---------------------------------
I101I135ILINKI939622I1128I13726I13460I 11198-199
I---I---I----I------I----I-----I-----I---------------------------------
I102I136ILINKI939423I1179I25853I25531I 11200-204
I---I---I----I------I----I-----I-----I---------------------------------
I103I137ILINKI939424I1186I20784I20465I 11205-207
I---I---I----I------I----I-----I-----I---------------------------------
I192I138ITAPEI939425I1195I 5762I 5741I 11179
I---I---I----I------I----I-----I-----I---------------------------------
I104I139ILINKI939426I1441I22525I22205I 11208-210
I---I---I----I------I----I-----I-----I---------------------------------
I105I140ILINKI939427I1499I15949I15701I 11211-213
I---I---I----I------I----I-----I-----I---------------------------------
I106I141ILINKI939448I1502I27252I26990I 11214-218
I---I---I----I------I----I-----I-----I---------------------------------
I107I142ILINKI939449I1515I27518I27423I 11219-224
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939416-939427,939448,49 WERE SENT TO RUTHERFORD
                                              ON 14.5.82
   AIR.#  220-66052210

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I 06I 07I    I      I    I     I     I
I===I===I====I======I====I=====I=====I=================================
I108I143ILINKI939450I1725I35722I35645I 11225-230
I---I---I----I------I----I-----I-----I---------------------------------
I109I144ILINKI939451I1726I30970I30908I 11231-241
I---I---I----I------I----I-----I-----I---------------------------------
I110I145ILINKI939452I1728I30792I30718I 11242-248
I---I---I----I------I----I-----I-----I---------------------------------
I111I146ILINKI939453I1586I33271I33184I 11249-251,253-258
I---I---I----I------I----I-----I-----I---------------------------------
I112I147ILINKI939454I1589I24199I24079I 11259-265
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939450-939455 WERE SENT TO RUTHERFORD
                                              ON 19.5.82
   AIR.#  220-66052416

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I 06I 07I    I      I    I     I     I
I===I===I====I======I====I=====I=====I=================================
I113I148ILINKI939456I 645I33864I33788I 11266,68-73
I---I---I----I------I----I-----I-----I---------------------------------
I114I149ILINKI939457I 647I32450I32410I 11274-279
I---I---I----I------I----I-----I-----I---------------------------------
I115I150ILINKI939458I 294I31978I31918I 11280-288
I---I---I----I------I----I-----I-----I---------------------------------
I116I151ILINKI939459I 295I35223I35153I 11289-293
I---I---I----I------I----I-----I-----I---------------------------------
I117I152ILINKI939460I 298I33799I33750I 11294-301
I---I---I----I------I----I-----I-----I---------------------------------
I118I153ILINKI939461I 300I30990I30895I 11302-308
I---I---I----I------I----I-----I-----I---------------------------------
I119I154ILINKI939462I 304I37328I37237I 11309-315
I---I---I----I------I----I-----I-----I---------------------------------
I120I155ILINKI939463I 316I29062I29018I 11316-319
I---I---I----I------I----I-----I-----I---------------------------------
I121I156ILINKI939464I 329I33105I32954I 11320-324
I---I---I----I------I----I-----I-----I---------------------------------
I122I157ILINKI939465I 619I34448I34374I 11325-331,34
I---I---I----I------I----I-----I-----I---------------------------------
I123I158ILINKI939466I 752I34240I34080I 11335-340
I---I---I----I------I----I-----I-----I---------------------------------
I124I159ILINKI939467I 627I31258I31160I 11341-346
I---I---I----I------I----I-----I-----I---------------------------------
I125I160ILINKI939468I 629I32008I31931I 11347-350
I---I---I----I------I----I-----I-----I---------------------------------
I126I161ILINKI939469I 630I35803I35618I 11351-356
I---I---I----I------I----I-----I-----I---------------------------------
I127I162ILINKI939470I 633I31380I31147I 11357-361
I---I---I----I------I----I-----I-----I---------------------------------
I128I163ILINKI939471I 645I34441I34245I 11362-367
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939456-939458,939460-939465,939467-939471
                    WERE SENT TO RUTHERFORD ON 25.5.82
   AIR.#  220-66052641

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I 06I 07I    I      I    I     I     I
I===I===I====I======I====I=====I=====I=================================
I129I164ILINKI939472I1210I31802I31662I 11368-388
I---I---I----I------I----I-----I-----I---------------------------------
I130I165ILINKI939473I1217I33579I33477I 11389-395
I---I---I----I------I----I-----I-----I---------------------------------
I131I166ILINKI939474I1220I33349I33017I 11396-398,409-411
I                                              399-408 LG TEST RUNS
I---I---I----I------I----I-----I-----I---------------------------------
I132I167ILINKI939475I1224I32768I32630I 11412-416
I---I---I----I------I----I-----I-----I---------------------------------
I193I168ITAPEI939476I1234I  464I  462I 11333
I---I---I----I------I----I-----I-----I---------------------------------
I133I169ILINKI939477I 865I30317I30047I 11417-422
I---I---I----I------I----I-----I-----I---------------------------------
I134I170ILINKI939478I 869I34045I33853I 11423-425,427-429
I                                                I 426 NORD TAPE
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939459,939466,939472-939478 WERE SENT TO RUTHERFORD
                                            ON 27.5.82
   AIR.#  220-66052641

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I 06I 07I    I      I    I     I     I
I===I===I====I======I====I=====I=====I=================================
I135I171ILINKI939479I1717I34175I33964I 11430-435
I---I---I----I------I----I-----I-----I---------------------------------
I194I172ITAPEI939480I1730I 4000I 3987I 11426
I---I---I----I------I----I-----I-----I---------------------------------
I136I173ILINKI939481I1107I32033I31961I 11436-440
I---I---I----I------I----I-----I-----I---------------------------------
I137I174ILINKI939484I1026I28797I28714I 11441-452
I---I---I----I------I----I-----I-----I---------------------------------
I138I175ILINKI939485I1038I30302I30168I 11453-462,471,472
I                                              463-470 LG TEST RUNS
I---I---I----I------I----I-----I-----I---------------------------------
I139I176ILINKI939486I1043I32515I32433I 11473-480
I                                             481 LOST DUE TO NORD ERROR
I---I---I----I------I----I-----I-----I---------------------------------
I140I177ILINKI939487I1044I26724I26669I 11482-485
I---I---I----I------I----I-----I-----I---------------------------------
I141I178ILINKI939488I1046I30640I30591I 11486-490
I---I---I----I------I----I-----I-----I---------------------------------
I142I179ILINKI939489I1052I32897I32841I 11491-495
I---I---I----I------I----I-----I-----I---------------------------------
I143I180ILINKI939490I1054I34949I34903I 11496-500
I---I---I----I------I----I-----I-----I---------------------------------
I144I181ILINKI939491I1059I25085I25017I 11501-505
I---I---I----I------I----I-----I-----I---------------------------------
I145I182ILINKI939492I1087I26898I26800I 11506-509
I---I---I----I------I----I-----I-----I---------------------------------
I146I183ILINKI939493I0551I32678I32625I 11510-516
I---I---I----I------I----I-----I-----I---------------------------------
I147I184ILINKI939494I0554I32787I32723I 11517-524
I---I---I----I------I----I-----I-----I---------------------------------
I148I185ILINKI939495I1116I30728I30683I 11525-530
I---I---I----I------I----I-----I-----I---------------------------------
I149I186ILINKI939496I1119I40912I40861I 11531-538
I---I---I----I------I----I-----I-----I---------------------------------
I150I187ILINKI939497I1126I36997I36922I 11539-545
I---I---I----I------I----I-----I-----I---------------------------------
I151I188ILINKI939498I1139I29688I29640I 11546-549
I---I---I----I------I----I-----I-----I---------------------------------
I152I189ILINKI939499I1139I35741I35674I 11550-554
I---I---I----I------I----I-----I-----I---------------------------------
I153I190ILINKI939500I1161I29314I29231I 11555-559
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939479-939482,939484-939500 WERE SENT TO RUTHERFORD
                                            ON 22.6.82
   AIR.#  220-66107974

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I 06I 07I    I      I    I     I     I
I===I===I====I======I====I=====I=====I=================================
I154I191ILINKI939501I 840I42614I42518I 11560-567
I---I---I----I------I----I-----I-----I---------------------------------
I155I192ILINKI939502I 842I27530I27466I 11568-571
I---I---I----I------I----I-----I-----I---------------------------------
I156I193ILINKI939503I 843I35293I35205I 11572-577
I---I---I----I------I----I-----I-----I---------------------------------
I157I194ILINKI939504I 846I29153I27420I 11578-587
I---I---I----I------I----I-----I-----I---------------------------------
I158I195ILINKI939505I 848I35273I35093I 11588-593
I---I---I----I------I----I-----I-----I---------------------------------
I159I196ILINKI939506I 849I26658I26457I 11594-599
I---I---I----I------I----I-----I-----I---------------------------------
I160I197ILINKI939507I 851I36373I36187I 11600-604
I---I---I----I------I----I-----I-----I---------------------------------
I161I198ILINKI939508I 341I30606I30470I 11605-609
I---I---I----I------I----I-----I-----I---------------------------------
I162I199ILINKI939509I 344I29112I28874I 11610-616
I---I---I----I------I----I-----I-----I---------------------------------
I163I200ILINKI939510I 347I29453I29226I 11617-623
I---I---I----I------I----I-----I-----I---------------------------------
I164I201ILINKI939511I 350I30637I30449I 11624-630
I---I---I----I------I----I-----I-----I---------------------------------
I165I202ILINKI939512I1533I28799I28607I 11631-634
I---I---I----I------I----I-----I-----I---------------------------------
I166I203ILINKI939513I1534I30502I30268I 11635-639
I---I---I----I------I----I-----I-----I---------------------------------
I167I204ILINKI939514I1536I30820I30470I 11640-643
I---I---I----I------I----I-----I-----I---------------------------------
I168I205ILINKI939515I1538I25624I25408I 11644-649
I---I---I----I------I----I-----I-----I---------------------------------
I169I206ILINKI939516I1541I27136I26950I 11650-654
I---I---I----I------I----I-----I-----I---------------------------------
I170I207ILINKI939517I 785I32815I32600I 11655-672
I---I---I----I------I----I-----I-----I---------------------------------
I171I208ILINKI939518I 788I42222I42021I 11673-677,679
I            +939540                 I            678 DUMMY
I---I---I----I------I----I-----I-----I---------------------------------
I172I209ILINKI939519I 708I39549I39314I680,681,684-686,688-690
I---I---I----I------I----I-----I-----I---------------------------------
I173I210ILINKI939520I 671I29199I29002I691-694
I---I---I----I------I----I-----I-----I---------------------------------
I174I211ILINKI939521I 710I34950I34698I695-702
I---I---I----I------I----I-----I-----I---------------------------------
I175I212ILINKI939522I 716I28394I28290I703-706
I---I---I----I------I----I-----I-----I---------------------------------
I176I213ILINKI939523I 720I35491I35341I707-711
I---I---I----I------I----I-----I-----I---------------------------------
I177I214ILINKI939524I 727I29212I29022I712-717
I---I---I----I------I----I-----I-----I---------------------------------
I178I215ILINKI939525I 736I31317I31031I718-725
I---I---I----I------I----I-----I-----I---------------------------------
I179I216ILINKI939526I 742I23565I23402I726-732
I---I---I----I------I----I-----I-----I---------------------------------
I180I217ILINKI939527I 745I60766I60408I733-738,748-754
I            +939564
I---I---I----I------I----I-----I-----I---------------------------------
I181I218ILINKI939528I 749I36343I36118I755-760
I---I---I----I------I----I-----I-----I---------------------------------
I182I219ILINKI939529I 679I36995I36783I761-766
I---I---I----I------I----I-----I-----I---------------------------------
I183I220ILINKI939530I 682I25139I25028I767-770
I---I---I----I------I----I-----I-----I---------------------------------
I184I221ILINKI939531I 684I29493I29318I771-775
I---I---I----I------I----I-----I-----I---------------------------------
I185I222ILINKI939532I 686I33812I33585I776-781
I---I---I----I------I----I-----I-----I---------------------------------
I186I223ILINKI939533I1454I32867I32648I782-787
I---I---I----I------I----I-----I-----I---------------------------------
I187I224ILINKI939534I1461I26061I25875I788-791
I---I---I----I------I----I-----I-----I---------------------------------
I188I225ILINKI939535I1463I31681I31422I792-803
I---I---I----I------I----I-----I-----I---------------------------------
I189I226ILINKI939536I1470I32173I31924I804-808
I---I---I----I------I----I-----I-----I---------------------------------
I190I227ILINKI939537I1472I24447I24325I809-812
I---I---I----I------I----I-----I-----I---------------------------------
I191I228ILINKI939538I1475I32598I32342I813-17,20-22
I---I---I----I------I----I-----I-----I---------------------------------
I192I229ILINKI939539I1480I26672I26530I823-827
I---I---I----I------I----I-----I-----I---------------------------------

  F22YEN.JADE.EXDATA06.G0193V00 - ..G0196V00 DO NOT  EXSIST

   THE TAPES 939512-939517,939520-939523,939528-939539 WERE SENT TO
                                       RUTHERFORD  ON 28.6.82
   AIR.#  220-668831376

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I 06I 07I    I      I    I     I     I
I===I===I====I======I====I=====I=====I=================================
I195I230ITAPEI939541I1281I 6683I 6670I742,747
I---I---I----I------I----I-----I-----I---------------------------------
I196I231ITAPEI939542I1290I 1800I 1787I818-819
I---I---I----I------I----I-----I-----I---------------------------------
I197I236ILINKI939547I1515I30953I30743I828-835
I---I---I----I------I----I-----I-----I---------------------------------
I198I237ILINKI939548I1523I28483I28292I836-841
I---I---I----I------I----I-----I-----I---------------------------------
I199I238ILINKI939549I1621I30205I29994I842-846
I---I---I----I------I----I-----I-----I---------------------------------
I200I239ILINKI939550I1532I30713I30541I847-850
I---I---I----I------I----I-----I-----I---------------------------------
I201I240ILINKI939551I1825I28088I27924I851-855
I---I---I----I------I----I-----I-----I---------------------------------
I202I241ILINKI939552I1837I29833I29603I856-864
I---I---I----I------I----I-----I-----I---------------------------------
I203I242ILINKI939553I1839I32235I32022I866-873
I---I---I----I------I----I-----I-----I---------------------------------
I204I243ILINKI939554I1846I29187I28692I874-883
I---I---I----I------I----I-----I-----I---------------------------------
I205I244ILINKI939555I1848I26071I26766I884-893
I---I---I----I------I----I-----I-----I---------------------------------
I206I245ILINKI939556I1849I32421I32259I894,95,98-906
I---I---I----I------I----I-----I-----I---------------------------------
I207I246ILINKI939557I1850I39052I38822I907-12,14-17,33-35
I---I---I----I------I----I-----I-----I---------------------------------
I208I247ILINKI939558I1854I26378I26248I936-41
I---I---I----I------I----I-----I-----I---------------------------------
I209I248ILINKI939559I1858I35214I35012I942-952
I---I---I----I------I----I-----I-----I---------------------------------
I210I249ILINKI939560I1866I32350I32159I953-961
I---I---I----I------I----I-----I-----I---------------------------------
I211I250ILINKI939561I  35I29243I29027I962-966
I---I---I----I------I----I-----I-----I---------------------------------


   JDATA08 .. REFORMATTED GENERATION GROUP 8 STARTS HERE.


I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I 07I 08I    I      I    I     I     I
I===I===I====I======I====I=====I=====I=================================
I  1I  1ILINKI939562I  41I30712I30548I967-971
I---I---I----I------I----I-----I-----I---------------------------------
I  2I  2ILINKI939563I  56I37390I37210I971-978
I---I---I----I------I----I-----I-----I---------------------------------
I  3I  3ILINKI939565I  78I27198I27054I979-983
I---I---I----I------I----I-----I-----I---------------------------------
I  4I  4ILINKI939566I  83I30276I30057I984-994
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939518,939540,939524-939526,939541,939542,939547-939563
             939565,939566 WERE SENT TO RUTHERFORD ON 14.7.82
   AIR.#  220-71090106

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I 07I 08I    I      I    I     I     I
I===I===I====I======I====I=====I=====I=================================
I  5I  5ILINKI939567I1935I28736I28528I995-999
I---I---I----I------I----I-----I-----I---------------------------------
I  6I  6ILINKI939568I1817I31619I31387I12000-008
I---I---I----I------I----I-----I-----I---------------------------------
I  7I  7ILINKI939569I1823I33958I33756I009-017
I---I---I----I------I----I-----I-----I---------------------------------
I  8I  8ILINKI939570I1824I30153-30021I018-021
I---I---I----I------I----I-----I-----I---------------------------------
I  9I  9ILINKI939571I1825I34612I34441I022-034
I---I---I----I------I----I-----I-----I---------------------------------
I 10I 10ILINKI939584I1829I59739I59414I035-046    TAPE WAS REDONE AT
I   I   I    I939469I    I     I     I           21.7.82
I---I---I----I------I----I-----I-----I---------------------------------
I 11I 11ILINKI939573I1932I34470I34237I047-054
I---I---I----I------I----I-----I-----I---------------------------------
I 12I 12ILINKI939575I1834I29725I29533I055-062
I---I---I----I------I----I-----I-----I---------------------------------
I 13I 13ILINKI939576I 134I24533I24364I063-65,68-72
I---I---I----I------I----I-----I-----I---------------------------------
I 14I 14ILINKI939577I 137I30917I30724I073-79
I---I---I----I------I----I-----I-----I---------------------------------
I 15I 15ILINKI939578I 140I38044I37828I080-88
I---I---I----I------I----I-----I-----I---------------------------------
I 16I 16ILINKI939579I 684I30212I29902I089-094
I---I---I----I------I----I-----I-----I---------------------------------
I 17I 17ILINKI939580I 687I32751I32479I095-104
I---I---I----I------I----I-----I-----I---------------------------------
I 18I 18ILINKI939581I 698I27146I26979I105-111
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939519,939564,939527,939567-939571,939573,939575-939581
                           WERE SENT TO RUTHERFORD ON 20.7.82
   AIR.#  220-71090331

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I 07I 08I    I      I    I     I     I
I===I===I====I======I====I=====I=====I=================================
I 19I 19ILINKI939582I 701I33364I33136I112-119
I---I---I----I------I----I-----I-----I---------------------------------
I 20I 20ILINKI939543I 784I32802I32715I120-125
I---I---I----I------I----I-----I-----I---------------------------------
I 21I 21ILINKI939585I 789I29660I29535I126-129
I---I---I----I------I----I-----I-----I---------------------------------
I 22I 22ILINKI939586I 794I37916I37669I131-139
I---I---I----I------I----I-----I-----I---------------------------------
I 23I 23ILINKI939587I 798I29534I29289I140-144
I---I---I----I------I----I-----I-----I---------------------------------
I197I 24ITAPEI939588I 817I 5007I 4991I130
I---I---I----I------I----I-----I-----I---------------------------------
I 24I 25ILINKI939589I 804I32391I32172I145-150
I---I---I----I------I----I-----I-----I---------------------------------
I 25I 26ILINKI939590I 809I27760I27589I151-155
I---I---I----I------I----I-----I-----I---------------------------------
I 26I 27ILINKI939591I 812I34779I34533I156-162
I---I---I----I------I----I-----I-----I---------------------------------
I 27I 28ILINKI939592I 815I32800I32612I163-167
I---I---I----I------I----I-----I-----I---------------------------------
I 28I 29ILINKI939593I1650I29617I29473I168-175
I---I---I----I------I----I-----I-----I---------------------------------
I 29I 30ILINKI939594I1657I32469-32327I176-182
I---I---I----I------I----I-----I-----I---------------------------------
I 30I 31ILINKI939595I1661I28348I28101I185-194
I---I---I----I------I----I-----I-----I---------------------------------
I 31I 32ILINKI939596I1661I26663I26507I195-198
I---I---I----I------I----I-----I-----I---------------------------------
I 32I 33ILINKI939597I1687I33537I33361I199-204
I---I---I----I------I----I-----I-----I---------------------------------
I 33I 34ILINKI939598I 801I32323I32163I205-209
I---I---I----I------I----I-----I-----I---------------------------------
I 34I 35ILINKI939599I 802I29267I29138I210-215
I---I---I----I------I----I-----I-----I---------------------------------
I 35I 36ILINKI939600I 803I34480I34271I216-222
I---I---I----I------I----I-----I-----I---------------------------------
I 36I 37ILINKI939601I 804I29094I28943I223-227
I---I---I----I------I----I-----I-----I---------------------------------
I 37I 38ILINKI939602I 805I26737I26571I228-232
I---I---I----I------I----I-----I-----I---------------------------------
I 38I 39ILINKI939603I 808I36588I36397I233-244
I---I---I----I------I----I-----I-----I---------------------------------
I 39I 40ILINKI939604I 810I26392I26205I245-248,250
I---I---I----I------I----I-----I-----I---------------------------------
I 40I 41ILINKI939605I 816I35923I35674I251-257
I---I---I----I------I----I-----I-----I---------------------------------
I 41I 42ILINKI939606I 371I31205I31021I258-261
I---I---I----I------I----I-----I-----I---------------------------------
I 42I 43ILINKI939607I 372I29370I29285I262-265
I---I---I----I------I----I-----I-----I---------------------------------
I 43I 44ILINKI939608I 376I31910I31727I266-270
I---I---I----I------I----I-----I-----I---------------------------------
I 44I 45ILINKI939609I 378I29361I29150I271-281
I---I---I----I------I----I-----I-----I---------------------------------
I 45I 46ILINKI939610I 379I32008I31831I282-285
I---I---I----I------I----I-----I-----I---------------------------------
I 46I 47ILINKI939611I 380I26203I26052I286-290
I---I---I----I------I----I-----I-----I---------------------------------
I 47I 48ILINKI939612I 381I31020I30808I291-295
I---I---I----I------I----I-----I-----I---------------------------------
I 48I 49ILINKI939613I 382I35454I35248I296-299
I---I---I----I------I----I-----I-----I---------------------------------
I 49I 50ILINKI939614I 384I28773I28601I300-302
I---I---I----I------I----I-----I-----I---------------------------------
I 50I 51ILINKI939615I 285I31488I31321I303-307
I---I---I----I------I----I-----I-----I---------------------------------
I 51I 52ILINKI939616I 288I27373I27171I308-312
I---I---I----I------I----I-----I-----I---------------------------------
I 52I 53ILINKI939617I 290I30847I30680I313-318
I---I---I----I------I----I-----I-----I---------------------------------
I 53I 54ILINKI939618I 292I27240I27099I319-323
I---I---I----I------I----I-----I-----I---------------------------------
I 54I 55ILINKI939619I1106I33379I33154I324-331
I---I---I----I------I----I-----I-----I---------------------------------
I 55I 56ILINKI939620I1113I27527I27309I332,334-336
I---I---I----I------I----I-----I-----I---------------------------------
I 56I 57ILINKI939621I1129I27858I27700I337-341
I---I---I----I------I----I-----I-----I---------------------------------
I 57I 58ILINKI939622I1138I30399I30220I3342-347
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939582,939584,939469,939585-939594
                                  939598-939622
                           WERE SENT TO RUTHERFORD ON 29.7.82
   AIR.# 220-71090751

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I 07I 08I    I      I    I     I     I
I===I===I====I======I====I=====I=====I=================================
I 58I 59ILINKI939544I 845I32048I31884I348-353
I---I---I----I------I----I-----I-----I---------------------------------
I 59I 60ILINKI939545I 860I26237I26022I354-357
I---I---I----I------I----I-----I-----I---------------------------------
I 60I 61ILINKI939546I1539I27015I26780I358-361
I---I---I----I------I----I-----I-----I---------------------------------
I 61I 62ILINKI939423I1550I27397I26926I362-367
I---I---I----I------I----I-----I-----I---------------------------------
I 62I 63ILINKI939424I1553I33355I32814I368-372
I---I---I----I------I----I-----I-----I---------------------------------
I 63I 64ILINKI939425I1555I22432I22232I373-375
I---I---I----I------I----I-----I-----I---------------------------------
I 64I 65ILINKI939426I1556I34251I33955I376-78,82-84
I---I---I----I------I----I-----I-----I---------------------------------
I 65I 66ILINKI939427I1562I23625I23467I385-391
I---I---I----I------I----I-----I-----I---------------------------------
I 66I 67ILINKI939428I1512I30576I30399I392-396
I---I---I----I------I----I-----I-----I---------------------------------
I 67I 68ILINKI939429I1516I27918I27711I397-400
I---I---I----I------I----I-----I-----I---------------------------------
I 68I 69ILINKI939430I1519I32020I31788I401-405
I---I---I----I------I----I-----I-----I---------------------------------
I 69I 70ILINKI939431I1521I27402I27204I406-409
I---I---I----I------I----I-----I-----I---------------------------------
I 70I 71ILINKI939432I1534I29267I29049I410-412,417
I---I---I----I------I----I-----I-----I---------------------------------
I 71I 72ILINKI939433I1542I31221I30769I418-421
I---I---I----I------I----I-----I-----I---------------------------------
I 72I 73ILINKI939434I1546I25688I25293I422-425
I---I---I----I------I----I-----I-----I---------------------------------
I198I 74ITAPEI939435I1551I 3683I 3623I183-184
I---I---I----I------I----I-----I-----I---------------------------------
I199I 75ITAPEI939436I1557I 4445I 4406I249,(10619?)
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939595-939597,939543-939545,939423-939436
                           WERE SENT TO RUTHERFORD ON 4.8.82
   AIR.#  220-71090961

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I 07I 08I    I      I    I     I     I
I===I===I====I======I====I=====I=====I=================================
I200I 76ITAPEI939437I1571I 8002I 7954I380
I---I---I----I------I----I-----I-----I---------------------------------
I201I 77ITAPEI939438I1578I 8002I 7966I381
I---I---I----I------I----I-----I-----I---------------------------------
I 73I 78ILINKI939439I 796I33591I33062I426-430
I---I---I----I------I----I-----I-----I---------------------------------
I 74I 79ILINKI939440I 801I21197I20895I431-433
I---I---I----I------I----I-----I-----I---------------------------------
I 75I 80ILINKI939441I 803I30350I29964I434-439
I---I---I----I------I----I-----I-----I---------------------------------
I 76I 81ILINKI939442I 804I27536I27209I440-443
I---I---I----I------I----I-----I-----I---------------------------------
I 77I 82ILINKI939443I 827I33331I32961I444-451
I---I---I----I------I----I-----I-----I---------------------------------
I 78I 83ILINKI939444I 832I47644I47548I452-458
I---I---I----I------I----I-----I-----I---------------------------------
I 79I 84ILINKI939445I 838I34783I34593I459-465
I---I---I----I------I----I-----I-----I---------------------------------
I 80I 85ILINKI939446I 850I29590I29402I466-471
I---I---I----I------I----I-----I-----I---------------------------------
I 81I 86ILINKI939447I 863I30888I30705I472-475
I---I---I----I------I----I-----I-----I---------------------------------
I 82I 87ILINKI939448I 871I36689I36518I476-481
I---I---I----I------I----I-----I-----I---------------------------------
I 83I 88ILINKI939449I 877I25023I24840I482-487
I---I---I----I------I----I-----I-----I---------------------------------
I 84I 89ILINKI939450I 888I29859I29567I488-498
I---I---I----I------I----I-----I-----I---------------------------------
I 85I 90ILINKI939451I 897I34905I34582I499-505
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939437-939451,939452
                           WERE SENT TO RUTHERFORD ON 9.8.82
   AIR.#  220-66832102

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I 86I 91IDISKI939454I1647I23574I23416I506-519
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPE 939454
                           WERE SENT TO RUTHERFORD ON 29.8.82
    BY R.H.

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I===I===I====I======I====I=====I=====I=================================
I 88I 93ILINKI939423I1790I33349I33277I556-565
I---I---I----I------I----I-----I-----I---------------------------------
I 89I 94ILINKI939424I 317I23352I23293I566-569
I---I---I----I------I----I-----I-----I---------------------------------
I 90I 95ILINKI939425I1701I28167I28081I570-578
I---I---I----I------I----I-----I-----I---------------------------------
I 91I 96ILINKI939426I1074I26556I26485I579-584
I---I---I----I------I----I-----I-----I---------------------------------
I 92I 97ILINKI939427I 656I35901I35851I585-590
I---I---I----I------I----I-----I-----I---------------------------------
I 93I 98ILINKI939428I  58I24647I24595I591-595
I---I---I----I------I----I-----I-----I---------------------------------
I 94I 99ILINKI939429I 523I34651I34606I596-601
I---I---I----I------I----I-----I-----I---------------------------------
I 95I100ILINKI939430I1652I21776I21732I602-608
I---I---I----I------I----I-----I-----I---------------------------------
I 96I101ILINKI939431I1413I34662I34592I609-613
I---I---I----I------I----I-----I-----I---------------------------------
I 97I102ILINKI939443I1672I36835I36734I614-19,24-25
I                                    I            620-623 COSMIC RUNS
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939423-939431,939443
                           WERE SENT TO RUTHERFORD ON 19.11.82
   AIR.#  057-36049694

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I 98I103ILINKI939432I 743I27140I27044I626-630
I---I---I----I------I----I-----I-----I---------------------------------
I 99I104ILINKI939433I1487I32587I32475I631-639
I---I---I----I------I----I-----I-----I---------------------------------
I100I105ILINKI939434I 538I27513I27476I640-644,646-647
I---I---I----I------I----I-----I-----I---------------------------------
I101I106ILINKI939435I1326I38089I38035I648-655
I---I---I----I------I----I-----I-----I---------------------------------
I102I107ILINKI939436I 628I32468I32384I656-665
I---I---I----I------I----I-----I-----I---------------------------------
I103I108ILINKI939437I 591I32265I32216I666-671
I---I---I----I------I----I-----I-----I---------------------------------
I202I109ITAPEI939438I1722I 1405I 1397I686
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939432-939438 WERE SENT TO RUTHERFORD ON 27.11.82
   AIR.#  220-7694-4162

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I104I110ILINKI939439I 671I30747I30672I672-682
I---I---I----I------I----I-----I-----I---------------------------------
I105I111ILINKI939440I 503I32633I32546I683,84,87-90,92-98
I---I---I----I------I----I-----I-----I---------------------------------
I203I112ITAPEI939441I 942I 1220I 1219I691
I---I---I----I------I----I-----I-----I---------------------------------
I106I113ILINKI939442I 949I29128I29043I699-709
I---I---I----I------I----I-----I-----I---------------------------------
I107I114ILINKI939444I 490I33454I33379I710-729
I---I---I----I------I----I-----I-----I---------------------------------
I204I115ITAPEI939445I 665I 3122I 3119I753
I---I---I----I------I----I-----I-----I---------------------------------
I108I116ILINKI939446I1235I35112I35054I730-746
I---I---I----I------I----I-----I-----I---------------------------------
I109I117ILINKI939447I    I33456I33394I747-49,755-57
I---I---I----I------I----I-----I-----I---------------------------------
I110I118ILINKI939448I    I36980I36913I758-67,771-73
I---I---I----I------I----I-----I-----I---------------------------------
I205I119ITAPEI939449I1900I 6767I 6759I770
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939439-939439 WERE SENT TO RUTHERFORD ON 6.12.82
   AIR.# 220-7694-4556

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I111I120ILINKI939450I 769I35266I36211I774-782
I---I---I----I------I----I-----I-----I---------------------------------
I112I121ILINKI939451I 812I30927I30890I783-794
I---I---I----I------I----I-----I-----I---------------------------------
I206I122ITAPEI939452I 733I 5766I 5755I797
I---I---I----I------I----I-----I-----I---------------------------------
I113I123ILINKI939453I1642I26520I26438I795-96,798-99
I---I---I----I------I----I-----I-----I---------------------------------
I114I124ILINKI939454I1718I31739I31624I800-808
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939450-939454 WERE SENT TO RUTHERFORD ON 9.12.82
   AIR.#

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I115I125ILINKI939455I1478I27146I27097I809-816
I---I---I----I------I----I-----I-----I---------------------------------
I207I126ITAPEI939456I1657I 2019I 2015I833
I---I---I----I------I----I-----I-----I---------------------------------
I116I127ILINKI939457I 111I33878I33817I821-28
I---I---I----I------I----I-----I-----I---------------------------------
I117I128ILINKI939458I1002I34997I34952I829-32,834-39
I---I---I----I------I----I-----I-----I---------------------------------
I118I129ILINKI939459I1005I30793I30730I840-49
I---I---I----I------I----I-----I-----I---------------------------------
I119I130ILINKI939460I1724I30340I30299I850-54
I---I---I----I------I----I-----I-----I---------------------------------
I120I131ILINKI939461I 959I36214I36185I855-61
I---I---I----I------I----I-----I-----I---------------------------------
I121I132ILINKI939462I 268I27646I27614I862-78
I---I---I----I------I----I-----I-----I---------------------------------
I122I133ILINKI939463I1282I30810I30754I879-84
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939455-939463 WERE SENT TO RUTHERFORD ON 15.12.82
   AIR.#

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I123I134ILINKI939464I 996I19815I19775I885-889
I---I---I----I------I----I-----I-----I---------------------------------
I124I135ILINKI939465I1267I31279I31152I890-904
I---I---I----I------I----I-----I-----I---------------------------------
I125I136ILINKI939466I1585I35438I35338I905-917
I---I---I----I------I----I-----I-----I---------------------------------
I126I137ILINKI939467I 662I27779I27709I918-24,29-34
I---I---I----I------I----I-----I-----I---------------------------------
I127I138ILINKI939468I1549I32910I32812I935-942
I---I---I----I------I----I-----I-----I---------------------------------
I128I139ILINKI939469I1714I27301I27253I943-948
I---I---I----I------I----I-----I-----I---------------------------------


   THE TAPES 939464-939469 WERE SENT TO RUTHERFORD ON 22.12.82
   AIR.#

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I129I140ILINKI939470I1942I 2997I 2997I12949-12950
I---I---I----I------I----I-----I-----I---------------------------------
I130I141ILINKI939471I 137I30922I30812I12959,12982,13088-13100
I---I---I----I------I----I-----I-----I---------------------------------
I130I142ILINKI939472I 685I57652I57631I13101-13114,13116
I---I---I----I------I----I-----I-----I---------------------------------
I132I143ILINKI939473I 753I82461I82461I13117-13119,13123-13141
I---I---I----I------I----I-----I-----I---------------------------------
I133I144ILINKI939474I  13I40582I40535I13142-13150
I---I---I----I------I----I-----I-----I---------------------------------
I134I145ILINKI939475I 532I31282I31200I13151-13155
I---I---I----I------I----I-----I-----I---------------------------------
I135I146ILINKI939476I 554I36261I36131I13156-13166
I---I---I----I------I----I-----I-----I---------------------------------
I136I147ILINKI939477I 561I31617I31514I13167-13172
I---I---I----I------I----I-----I-----I---------------------------------
I137I148ILINKI939478I 857I29044I28992I13173-13178
I---I---I----I------I----I-----I-----I---------------------------------
I138I149ILINKI939479I 875I31926I31803I13179,13184-13187
I---I---I----I------I----I-----I-----I---------------------------------
I139I150ILINKI939480I 894I29806I29711I13188-13193
I---I---I----I------I----I-----I-----I---------------------------------
I140I151ILINKI939481I 900I26718I26606I13194-13198
I---I---I----I------I----I-----I-----I---------------------------------
I209I152ITAPEI939482I1570   252I  252I13115
I---I---I----I------I----I-----I-----I---------------------------------
I210I153ITAPEI939483I1579I 2495I 2495I13122
I---I---I----I------I----I-----I-----I---------------------------------
I211I154ITAPEI939484I1587I 8002I 7972I13182
I---I---I----I------I----I-----I-----I---------------------------------
I212I155ITAPEI939485I1594I 4772I 4765I13183
I---I---I----I------I----I-----I-----I---------------------------------


   THE TAPES 939470-939485 WERE SENT TO RUTHERFORD ON 4.5.83
   AIR.#  220-84050724

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I141I156ILINKI939486I 303I28638I28575I 13199-203
I---I---I----I------I----I-----I-----I---------------------------------
I213I157ITAPEI939487I 323I 2484I 2462I 13216
I---I---I----I------I----I-----I-----I---------------------------------
I142I158ILINKI939488I 589I27567I27461I13204,213-219
I---I---I----I------I----I-----I-----I---------------------------------
I143I159ILINKI939489I1377I28761I28684I13220-223
I---I---I----I------I----I-----I-----I---------------------------------
I144I160ILINKI939490I1603I27834I27735I13224-229
I---I---I----I------I----I-----I-----I---------------------------------
I145I161ILINKI939491I1618I30233I30161I13230-237
I---I---I----I------I----I-----I-----I---------------------------------
I146I162ILINKI939492I1674I31202I31133I13238-243
I---I---I----I------I----I-----I-----I---------------------------------
I147I163ILINKI939493I1705I32012I31907I13244-248
I---I---I----I------I----I-----I-----I---------------------------------
I148I164ILINKI939494I1169I25744I25691I13249-252
I---I---I----I------I----I-----I-----I---------------------------------
I149I165ILINKI939495I1609I26801I26707I13253-256
I---I---I----I------I----I-----I-----I---------------------------------
I150I166ILINKI939425I1909I37057I36947I13257-261
I   I   I    I939426I        THE RUTHEFORD TAPES WERE REWRITEN 10.6.83
I---I---I----I------I----I-----I-----I---------------------------------
I151I167ILINKI939497I 282I25696I25568I13262-267
I---I---I----I------I----I-----I-----I---------------------------------
I152I168ILINKI939498I1488I27084I26985I13268-272
I---I---I----I------I----I-----I-----I---------------------------------
I153I169ILINKI939499I 917I25727I25664I13273-276
I---I---I----I------I----I-----I-----I---------------------------------
I154I170ILINKI939500I 732I22653I22544I13277-279
I---I---I----I------I----I-----I-----I---------------------------------
I155I171ILINKI939501I 175I24134I24031I13280-298
I---I---I----I------I----I-----I-----I---------------------------------


   THE TAPES 939486-939501 WERE SENT TO RUTHERFORD ON 17.5.83
   AIR.#

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I156I172ILINKI939502I 704I29976I29869I13299-311
I---I---I----I------I----I-----I-----I---------------------------------
I157I173ILINKI939503I 715I23769I23693I13312-320
I---I---I----I------I----I-----I-----I---------------------------------
I158I174ILINKI939504I1229I22386I22316I13321-323
I---I---I----I------I----I-----I-----I---------------------------------
I159I175ILINKI939505I1372I28640I28556I13324-327,329,331-332
I---I---I----I------I----I-----I-----I---------------------------------
I214I176ITAPEI939506I1676I 1517I 1511I13338
I---I---I----I------I----I-----I-----I---------------------------------
I160I177ILINKI939507I1090I26933I26836I13333-336
I---I---I----I------I----I-----I-----I---------------------------------
I161I178ILINKI939508I1092I23635I23541I13391-341
I---I---I----I------I----I-----I-----I---------------------------------
I162I179ILINKI939509I1795I24306I24207I13342-347
I---I---I----I------I----I-----I-----I---------------------------------
I163I180ILINKI939510I1912I21456I21357I13348-350
I---I---I----I------I----I-----I-----I---------------------------------
I164I181ILINKI939511I1420I26713I26633I13351-353,358-359
I---I---I----I------I----I-----I-----I---------------------------------
I215I182ITAPEI939512I 406I 7609I 7585I13346
I---I---I----I------I----I-----I-----I---------------------------------
I216I183ITAPEI939513I 419I 4066I 4060I13357
I---I---I----I------I----I-----I-----I---------------------------------


   THE TAPES 939502-939513 WERE SENT TO RUTHERFORD ON 20.5.83
   AIR.#  220-84056254

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I165I184ILINKI939514I 826I25772I25659I13360-364
I---I---I----I------I----I-----I-----I---------------------------------
I166I185ILINKI939515I 832I24004I23907I13365-367
I---I---I----I------I----I-----I-----I---------------------------------
I167I186ILINKI939516I 836I25842I25679I13368-373
I---I---I----I------I----I-----I-----I---------------------------------
I168I187ILINKI939517I 994I24457I24332I13374-380
I---I---I----I------I----I-----I-----I---------------------------------
I169I188ILINKI939518I1716I25209I25025I13381-388
I---I---I----I------I----I-----I-----I---------------------------------
I170I189ILINKI939519I  29I23330I23171I13389-393
I---I---I----I------I----I-----I-----I---------------------------------
I171I190ILINKI939520I 528I22445I22313I13394-397
I---I---I----I------I----I-----I-----I---------------------------------
I172I191ILINKI939521I 737I24420I24256I13398-401
I---I---I----I------I----I-----I-----I---------------------------------
I173I192ILINKI939522I 738I20584I20495I13402-405
I---I---I----I------I----I-----I-----I---------------------------------
I174I193ILINKI939523I 741I29876I29770I13406-409
I---I---I----I------I----I-----I-----I---------------------------------
I175I194ILINKI939524I1408I29223I29048I13410-417
I---I---I----I------I----I-----I-----I---------------------------------
I176I195ILINKI939525I1410I22693I22488I13415-418
I---I---I----I------I----I-----I-----I---------------------------------
I177I196ILINKI939526I1411I24127I24003I13419-422
I---I---I----I------I----I-----I-----I---------------------------------
I178I197ILINKI939527I 668I20614I20541I13423-425
I---I---I----I------I----I-----I-----I---------------------------------
I179I198ILINKI939528I1103I29510I29425I13426-430
I---I---I----I------I----I-----I-----I---------------------------------
I180I199ILINKI939529I1111I31367I31218I13431-437
I---I---I----I------I----I-----I-----I---------------------------------
I217I200ITAPEI939530I1161I 7502I 7466I13385
I---I---I----I------I----I-----I-----I---------------------------------
I218I201ITAPEI939531I1171I 6409I 6365I13386
I---I---I----I------I----I-----I-----I---------------------------------
I219I202ITAPEI939532I1179I 6189I 6177I13434
I---I---I----I------I----I-----I-----I---------------------------------


   THE TAPES 939514-939532 WERE SENT TO RUTHERFORD ON 26.5.83
   AIR.#  220-84056350

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I181I203ILINKI939533I1543I21785I21709I13438-441
I---I---I----I------I----I-----I-----I---------------------------------
I182I204ILINKI939534I 180I31172I31061I13442-446
I---I---I----I------I----I-----I-----I---------------------------------
I183I205ILINKI939535I 632I23439I23342I13447,449,450,451
I---I---I----I------I----I-----I-----I---------------------------------
I184I206ILINKI939536I 219I18701I18584I13452-456
I---I---I----I------I----I-----I-----I---------------------------------
I185I207ILINKI939537I 225I26577I26353I13457-460
I---I---I----I------I----I-----I-----I---------------------------------
I186I208ILINKI939538I1442I21245I21053I13461-464
I---I---I----I------I----I-----I-----I---------------------------------
I187I209ILINKI939539I1445I28312I28176I13466-469
I---I---I----I------I----I-----I-----I---------------------------------
I188I210ILINKI939540I1454I20180I20094I13470-473
I---I---I----I------I----I-----I-----I---------------------------------
I189I211ILINKI939541I1460I28072I27926I13474-477
I---I---I----I------I----I-----I-----I---------------------------------
I190I212ILINKI939542I1292I16835I16753I13478-480
I---I---I----I------I----I-----I-----I---------------------------------
I191I213ILINKI939543I1410I22345I22217I13481-486
I---I---I----I------I----I-----I-----I---------------------------------
I192I214ILINKI939544I 160I28281I28104I13487-490
I---I---I----I------I----I-----I-----I---------------------------------
I220I215ITAPEI939545I 714I  838I  828I13448
I---I---I----I------I----I-----I-----I---------------------------------
I221I216ITAPEI939546I 721I 5464I 5441I13465
I---I---I----I------I----I-----I-----I---------------------------------
I193I217ILINKI939548I 143I21858I21774I13491-500
I---I---I----I------I----I-----I-----I---------------------------------
I194I218ILINKI939558I1005I21931I21832I13501-504
I---I---I----I------I----I-----I-----I---------------------------------
I195I219ILINKI939559I1110I24006I23883I13505-507
I---I---I----I------I----I-----I-----I---------------------------------
I196I220ILINKI939560I1276I21411I21258I13508-510,516,517
I---I---I----I------I----I-----I-----I---------------------------------


   THE TAPES 939533-939546,939548,939558-939560 WERE SENT TO RUTHERFORD
                                                  ON 2.6.1983
   AIR.# 220-8405 6744

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I222I221ITAPEI939561I1800I 3458I 3430I13511
I---I---I----I------I----I-----I-----I---------------------------------
I223I222ITAPEI939562I1816I 6815I 6772I13512
I---I---I----I------I----I-----I-----I---------------------------------
I224I223ITAPEI939563I1820I 7340I 7283I13513
I---I---I----I------I----I-----I-----I---------------------------------
I225I224ITAPEI939564I1835I 7800I 7770I13514
I---I---I----I------I----I-----I-----I---------------------------------
I226I225ITAPEI939565I1843I 1973I 1966I13515
I---I---I----I------I----I-----I-----I---------------------------------
I197I226ILINKI939566I1980I26707I26552I13518-522
I---I---I----I------I----I-----I-----I---------------------------------
I198I227ILINKI939567I1992I23497I23368I13523,526,527
I---I---I----I------I----I-----I-----I---------------------------------
I227I228ITAPEI939568I 448I 5771I 5658I13525
I---I---I----I------I----I-----I-----I---------------------------------
I199I229ILINKI939569I1013I20393I20244I13528-531
I---I---I----I------I----I-----I-----I---------------------------------
I200I230ILINKI939570I 859I29062I28934I13532-535
I---I---I----I------I----I-----I-----I---------------------------------
I201I231ILINKI939571I1157I20644I20502I13536-539
I---I---I----I------I----I-----I-----I---------------------------------
I202I232ILINKI939572I1160I27574I27349I13540-547
I---I---I----I------I----I-----I-----I---------------------------------
I203I233ILINKI939573I1115I24913I24801I13548-552
I---I---I----I------I----I-----I-----I---------------------------------
I204I234ILINKI939575I1768I24173I24046I13553-556
I---I---I----I------I----I-----I-----I---------------------------------
I205I235ILINKI939576I 642I26485I26337I13557-561
I---I---I----I------I----I-----I-----I---------------------------------
I206I236ILINKI939577I 970I22230I22135I13562-565
I---I---I----I------I----I-----I-----I---------------------------------
I207I237ILINKI939578I 975I23671I23568I13566-568
I---I---I----I------I----I-----I-----I---------------------------------
I208I238ILINKI939579I1932I22842I22735I13569-572
I---I---I----I------I----I-----I-----I---------------------------------


   THE TAPES 939561-939573,939575-939579 WERE SENT TO RUTHERFORD
                                                  ON 7.6.1983
   AIR.#

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I209I239ILINKI939580I 664I27491I27337I13573,574,579-582
I---I---I----I------I----I-----I-----I---------------------------------
I228I240ITAPEI939581I1840I 7057I 7014I13575-576
I---I---I----I------I----I-----I-----I---------------------------------
I229I241ITAPEI939582I1022I 8002I 7980I13577
I---I---I----I------I----I-----I-----I---------------------------------
I230I242ITAPEI939583I1026I 1210I 1206I13578
I---I---I----I------I----I-----I-----I---------------------------------
I210I243ILINKI939584I 445I25906I25825I13583-586
I---I---I----I------I----I-----I-----I---------------------------------
I211I244ILINKI939585I1845I30986I30807I13587-89/92-94
I---I---I----I------I----I-----I-----I---------------------------------
I231I245ITAPEI939586I1722I 1609I 1603I13591
I---I---I----I------I----I-----I-----I---------------------------------
I212I246ILINKI939587I1754I14256I14114I13595-97
I---I---I----I------I----I-----I-----I---------------------------------
I001I001ILINKI939588I 710I27017I26854I13598-03
I---I---I----I------I----I-----I-----I---------------------------------
I002I002ILINKI939589I1648I26320I26166I13604-08
I---I---I----I------I----I-----I-----I---------------------------------
I003I003ILINKI939590I1336I24869I24710I13609-11/14
I---I---I----I------I----I-----I-----I---------------------------------
I004I004ILINKI939591I1263I22321I22191I13615-19
I---I---I----I------I----I-----I-----I---------------------------------
I005I005ILINKI939592I1415I21698I21622I13620-30
I---I---I----I------I----I-----I-----I---------------------------------


   THE TAPES 939580-939592 WERE SENT TO RUTHERFORD
                                                  ON 10.6.1983
   AIR.#  220-8406 3206

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I232I006ITAPEI939593I 722I 1325I 1317I13613
I---I---I----I------I----I-----I-----I---------------------------------
I006I007ILINKI939594I1275I22983I22842I13631-35
I---I---I----I------I----I-----I-----I---------------------------------
I007I008ILINKI939595I 480I22003I21912I13636-43
I---I---I----I------I----I-----I-----I---------------------------------
I008I009ILINKI939596I 481I29579I29473I13644-55/58/59
I---I---I----I------I----I-----I-----I---------------------------------
I009I010ILINKI939597I1736I20089I19990I13660-63
I---I---I----I------I----I-----I-----I---------------------------------
I010I011ILINKI939598I 650I22451I22321I13664-67
I---I---I----I------I----I-----I-----I---------------------------------
I011I012ILINKI939599I1651I22051I21905I13668-70/72/73
I---I---I----I------I----I-----I-----I---------------------------------
I012I013ILINKI939600I1652I24208I24057I13674-76/78/79
I---I---I----I------I----I-----I-----I---------------------------------
I013I014ILINKI939601I1669I21302I21199I13680-84
I---I---I----I------I----I-----I-----I---------------------------------
I014I015ILINKI939602I1468I28634I28502I13685-88
I---I---I----I------I----I-----I-----I---------------------------------
I015I016ILINKI939603I1750I23541I23403I13689-93
I---I---I----I------I----I-----I-----I---------------------------------
I016I017ILINKI939604I 916I21631I21526I13694-98
I---I---I----I------I----I-----I-----I---------------------------------
I017I018ILINKI939605I 205I22427I22291I13699-704
I---I---I----I------I----I-----I-----I---------------------------------
I018I019ILINKI939606I1706I26798I26555I13705-716
I---I---I----I------I----I-----I-----I---------------------------------
I019I020ILINKI939607I1734I21057I20873I13717-725
I---I---I----I------I----I-----I-----I---------------------------------
I020I021ILINKI939608I1738I28402I28250I13726-730
I---I---I----I------I----I-----I-----I---------------------------------
I001I022ITAPEI939609I1889I 5504I 5482I13656
I---I---I----I------I----I-----I-----I---------------------------------


   THE TAPES 939593-939609 WERE SENT TO RUTHERFORD
                                                  ON 17.6.1983
   AIR.#  220-8406 3405

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I002I023ITAPEI939610I1864I  721I  720I13657
I---I---I----I------I----I-----I-----I---------------------------------
I021I024ILINKI939611I1507I23686I23555I13731-734
I---I---I----I------I----I-----I-----I---------------------------------
I022I025ILINKI939612I1512I27138I26954I13735-744
I---I---I----I------I----I-----I-----I---------------------------------
I023I026ILINKI939613I1515I24534I24351I13745-748
I---I---I----I------I----I-----I-----I---------------------------------
I024I027ILINKI939614I1301I27158I27014I13749-753
I---I---I----I------I----I-----I-----I---------------------------------
I025I028ILINKI939615I 457I24954I24813I13754-759
I---I---I----I------I----I-----I-----I---------------------------------
I026I029ILINKI939616I 465I21140I21015I13760-762
I---I---I----I------I----I-----I-----I---------------------------------
I027I030ILINKI939617I 481I26105I25991I13763-767
I---I---I----I------I----I-----I-----I---------------------------------
I028I031ILINKI939618I 491I29185I29060I13768-771
I---I---I----I------I----I-----I-----I---------------------------------
I029I032ILINKI939619I1008I22562I22426I13772-774
I---I---I----I------I----I-----I-----I---------------------------------
I030I033ILINKI939620I  83I21602I21452I13775-778
I---I---I----I------I----I-----I-----I---------------------------------
I031I034ILINKI939621I1335I23077I22876I13779-782
I---I---I----I------I----I-----I-----I---------------------------------
I032I035ILINKI939622I1444I32475I32324I13783-789
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939611-939622 WERE SENT TO RUTHERFORD
                                                  ON 22.6.1983
   AIR.#  220-81212390

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I033I036ILINKI939427I1314I22368I22151I13790-794
I---I---I----I------I----I-----I-----I---------------------------------
I034I037ILINKI939428I1319I19252I19142I13795-797
I---I---I----I------I----I-----I-----I---------------------------------
I035I038ILINKI939429I 653I23836I23652I13798-802
I---I---I----I------I----I-----I-----I---------------------------------
I036I039ILINKI939430I 250I25973I25820I13803-806
I---I---I----I------I----I-----I-----I---------------------------------
I037I040ILINKI939431I 949I24172I24035I13807-812
I---I---I----I------I----I-----I-----I---------------------------------
I038I041ILINKI939432I1934I25570I25345I13813-816
I---I---I----I------I----I-----I-----I---------------------------------
I039I042ILINKI939433I1943I25682I25461I13817-828
I---I---I----I------I----I-----I-----I---------------------------------
I040I043ILINKI939434I1947I21649I21480I13829-831
I---I---I----I------I----I-----I-----I---------------------------------
I003I044ITAPEI939435I 195I 6797I 6702I13736
I---I---I----I------I----I-----I-----I---------------------------------
I004I045ITAPEI939436I 233I 6925I 6831I13737
I---I---I----I------I----I-----I-----I---------------------------------
I005I046ITAPEI939437I 264I 7486I 7401I13738
I---I---I----I------I----I-----I-----I---------------------------------
I006I047ITAPEI939438I 283I 6782I 6715I13740-741
I---I---I----I------I----I-----I-----I---------------------------------
I007I048ITAPEI939439I 160I 3176I 3151I13785
I---I---I----I------I----I-----I-----I---------------------------------
I008I049ITAPEI939440I 350I 4400I 4379I13786
I---I---I----I------I----I-----I-----I---------------------------------
I009I050ITAPEI939441I 497I 1506I 1502I13823
I---I---I----I------I----I-----I-----I---------------------------------
I010I051ITAPEI939442I1146I 2879I 2860I13826
I---I---I----I------I----I-----I-----I---------------------------------
I011I052ITAPEI939443I1732I 3310I 3289I13827
I---I---I----I------I----I-----I-----I---------------------------------
I041I053ILINKI939444I 208I19164I18985I13832-835
I---I---I----I------I----I-----I-----I---------------------------------
I042I054ILINKI939445I 669I26784I26566I13836-844
I---I---I----I------I----I-----I-----I---------------------------------
I043I055ILINKI939446I1131I25269I25136I13845-848
I---I---I----I------I----I-----I-----I---------------------------------
I044I056ILINKI939447I1269I22789I22640I13849-851
I---I---I----I------I----I-----I-----I---------------------------------
I045I057ILINKI939448I1423I16682I16521I13852-854
I---I---I----I------I----I-----I-----I---------------------------------
I046I058ILINKI939449I1980I26144I25992I13855-861
I---I---I----I------I----I-----I-----I---------------------------------
I047I059ILINKI939450I 893I19322I19212I13862-864
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939427-939450,939610 WERE SENT TO RUTHERFORD
                                                  ON 24.6.1983
   AIR.#  220-81212644

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I048I060ILINKI939451I 297I28717I28549I13864-868
I---I---I----I------I----I-----I-----I---------------------------------
I049I061ILINKI939452I1093I18642I18495I13869-871
I---I---I----I------I----I-----I-----I---------------------------------
I050I062ILINKI939453I 640I25749I25589I13872-877
I---I---I----I------I----I-----I-----I---------------------------------
I051I063ILINKI939454I  25I18968I18727I13878-882
I---I---I----I------I----I-----I-----I---------------------------------
I052I064ILINKI939455I 643I19205I19047I13883-886
I---I---I----I------I----I-----I-----I---------------------------------
I053I065ILINKI939456I1129I23507I23311I13887-891
I---I---I----I------I----I-----I-----I---------------------------------
I054I066ILINKI939457I1808I23886I23722I13892-895
I---I---I----I------I----I-----I-----I---------------------------------
I055I067ILINKI939458I  12I20282I20090I13896-900
I---I---I----I------I----I-----I-----I---------------------------------
I056I068ILINKI939459I  20I24443I24257I13901-905
I---I---I----I------I----I-----I-----I---------------------------------
I057I069ILINKI939460I 502I25443I25222I13906-912
I---I---I----I------I----I-----I-----I---------------------------------
I058I070ILINKI939461I 504I26760I26539I13913-918
I---I---I----I------I----I-----I-----I---------------------------------
I059I071ILINKI939462I 753I19537I19377I13919-922
I---I---I----I------I----I-----I-----I---------------------------------
I060I072ILINKI939463I 999I29238I28913I13923-926
I            +939557
I---I---I----I------I----I-----I-----I---------------------------------
I061I073ILINKI939464I1354I16521I16382I13927-931
I---I---I----I------I----I-----I-----I---------------------------------
I062I074ILINKI939465I1355I25361I25174I13932-935
I---I---I----I------I----I-----I-----I---------------------------------
I063I075ILINKI939466I1768I21301I21095I13936-938
I---I---I----I------I----I-----I-----I---------------------------------
I064I076ILINKI939467I1676I16940I16812I13939-946
I---I---I----I------I----I-----I-----I---------------------------------
I065I077ILINKI939468I1738I22823I22619I13947-953
I---I---I----I------I----I-----I-----I---------------------------------
I066I078ILINKI939469I1226I23902I23573I13954-959
I---I---I----I------I----I-----I-----I---------------------------------
I067I079ILINKI939470I 148I21259I21676I13960-962
I---I---I----I------I----I-----I-----I---------------------------------
I068I080ILINKI939471I 149I20892I20759I13963-966
I---I---I----I------I----I-----I-----I---------------------------------
I069I081ILINKI939472I 327I21738I21591I13967-971
I---I---I----I------I----I-----I-----I---------------------------------
I070I082ILINKI939549I 153I18487I18312I13972-974
I---I---I----I------I----I-----I-----I---------------------------------
I071I083ILINKI939550I 260I22043I21915I13975-977
I---I---I----I------I----I-----I-----I---------------------------------
I072I084ILINKI939551I 384I23827I23662I13978-981
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939451-939472,939557,939549-939551 WERE SENT TO RUTHERFORD
                                                  ON
   AIR.#

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I073I085ILINKI939552I 191I21075I20858I13982-984
I---I---I----I------I----I-----I-----I---------------------------------
I074I086ILINKI939553I 338I20699I20530I13985-988
I---I---I----I------I----I-----I-----I---------------------------------
I075I087ILINKI939554I 957I21183I21006I13989-901
I---I---I----I------I----I-----I-----I---------------------------------
I076I088ILINKI939555I 970I27760I27554I13992-995
I---I---I----I------I----I-----I-----I---------------------------------
I077I089ILINKI939556I 981I20042I19882I13996-998
I---I---I----I------I----I-----I-----I---------------------------------
I078I090ILINKI939473I 997I18541I18364I13999-14001
I---I---I----I------I----I-----I-----I---------------------------------
I079I091ILINKI939474I1012I25435I25305I14002-005
I---I---I----I------I----I-----I-----I---------------------------------
I080I092ILINKI939475I 381I22966I22798I14006-009
I---I---I----I------I----I-----I-----I---------------------------------
I081I093ILINKI939476I 366I23754I23580I14010-014
I---I---I----I------I----I-----I-----I---------------------------------
I082I094ILINKI939477I1638I23942I23773I14015-017
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939552-939556,939473-939477 WERE SENT TO RUTHERFORD
                                                  ON 5.7.83
   AIR.#  220-81212972

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I083I095ILINKI939481I1371I19527I19294I14018-020
I---I---I----I------I----I-----I-----I---------------------------------
I084I096ILINKI939482I1382I28395I28165I14021-024
I---I---I----I------I----I-----I-----I---------------------------------
I085I097ILINKI939483I1997I19547I19378I14025-027
I---I---I----I------I----I-----I-----I---------------------------------
I086I098ILINKI939484I   8I26916I26708I14028-031
I---I---I----I------I----I-----I-----I---------------------------------
I087I099ILINKI939485I  64I21544I21365I14032-034
I---I---I----I------I----I-----I-----I---------------------------------
I088I100ILINKI939486I  70I26205I25945I14035-038
I---I---I----I------I----I-----I-----I---------------------------------
I089I101ILINKI939487I  72I18788I18618I14039-042
I---I---I----I------I----I-----I-----I---------------------------------
I090I102ILINKI939488I  77I19388I19199I14043-045
I---I---I----I------I----I-----I-----I---------------------------------
I091I103ILINKI939489I  84I26196I25979I14046-049
I---I---I----I------I----I-----I-----I---------------------------------
I092I104ILINKI939490I1860I18831I18594I14050--52
I---I---I----I------I----I-----I-----I---------------------------------
I093I105ILINKI939491I1863I19878I19678I14053-056
I---I---I----I------I----I-----I-----I---------------------------------
I094I106ILINKI939492I1866I20671I20481I14057-059
I---I---I----I------I----I-----I-----I---------------------------------
I095I107ILINKI939493I 920I22698I22538I14060-062
I---I---I----I------I----I-----I-----I---------------------------------
I096I108ILINKI939494I 928I19178I19053I14063-065
I---I---I----I------I----I-----I-----I---------------------------------
I097I109ILINKI939495I 986I26811I26622I14066-069
I---I---I----I------I----I-----I-----I---------------------------------
I098I110ILINKI939497I1281I19231I19097I14070-072
I---I---I----I------I----I-----I-----I---------------------------------
I099I111ILINKI939498I 354I22745I22585I14073-075
I---I---I----I------I----I-----I-----I---------------------------------
I100I112ILINKI939499I 679I19664I19537I14076-079
I---I---I----I------I----I-----I-----I---------------------------------
I101I113ILINKI939500I 699I18329I18203I14080-082
I---I---I----I------I----I-----I-----I---------------------------------
I102I114ILINKI939501I 702I23973I23773I14083-085
I---I---I----I------I----I-----I-----I---------------------------------
I103I115ILINKI949502I 873I19056I18790I14086-088
I---I---I----I------I----I-----I-----I---------------------------------
I104I116ILINKI939503I 996I29511I29113I14089-092
I---I---I----I------I----I-----I-----I---------------------------------
I105I117ILINKI939504I1371I20378I20077I14093-095
I---I---I----I------I----I-----I-----I---------------------------------
I106I118ILINKI939505I 174I17744I17519I14096-098
I---I---I----I------I----I-----I-----I---------------------------------
I107I119ILINKI939506I1669I25649I25345I14099-14104
I---I---I----I------I----I-----I-----I---------------------------------
I108I120ILINKI939507I 510I21311I21134I14105-107
I---I---I----I------I----I-----I-----I---------------------------------
I109I121ILINKI939509I1177I23756I23588I14108-111
I---I---I----I------I----I-----I-----I---------------------------------
I110I122ILINKI939510I 154I23973I23774I14112-114
I---I---I----I------I----I-----I-----I---------------------------------
I 12I123ITAPEI939511I 180I 6758I 6653I14101
I---I---I----I------I----I-----I-----I---------------------------------
I111I124ILINKI939512I1840I21219I21066I14115-119
I---I---I----I------I----I-----I-----I---------------------------------
I112I125ILINKI939513I 616I17073I16908I14120-122
I---I---I----I------I----I-----I-----I---------------------------------
I113I126ILINKI939514I 644I26681I26606I14123-126
I---I---I----I------I----I-----I-----I---------------------------------
I114I127ILINKI939515I1792I32460I32294I14127-132
I---I---I----I------I----I-----I-----I---------------------------------
I115I128ILINKI939524I1436I19702I19554I14133-135
I---I---I----I------I----I-----I-----I---------------------------------
I116I129ILINKI939525I1935I20692I20389I14136-140
I---I---I----I------I----I-----I-----I---------------------------------
I 13I130ITAPEI939526I1936I 4520I 4494I14141
I---I---I----I------I----I-----I-----I---------------------------------
I117I131ILINKI939527I1742I23117I22949I14142-144
I---I---I----I------I----I-----I-----I---------------------------------
I118I132ILINKI939528I1715I22103I21920I14145-147
I---I---I----I------I----I-----I-----I---------------------------------
I119I133ILINKI939529I 618I16582I16246I14148-152
I---I---I----I------I----I-----I-----I---------------------------------
I120I134ILINKI939530I 623I20015I19806I14153-160
I---I---I----I------I----I-----I-----I---------------------------------
I121I135ILINKI939531I 965I19464I19270I14161-165
I---I---I----I------I----I-----I-----I---------------------------------
I 14I136ITAPEI939532I 695I 3024I 3010I14171
I---I---I----I------I----I-----I-----I---------------------------------
I122I137ILINKI939533I 392I21729I21498I14166-169
I---I---I----I------I----I-----I-----I---------------------------------
I123I138ILINKI939534I 395I22979I22837I14170-175
I---I---I----I------I----I-----I-----I---------------------------------
I124I139ILINKI939535I 615I26335I26158I14176-179
I---I---I----I------I----I-----I-----I---------------------------------
I125I140ILINKI939536I 632I20747I20368I14180-183
I---I---I----I------I----I-----I-----I---------------------------------
I126I141ILINKI939537I1171I13769I13524I14184-185
I---I---I----I------I----I-----I-----I---------------------------------
I127I142ILINKI939538I1614I28850I28617I14186,191-194
I---I---I----I------I----I-----I-----I---------------------------------
I128I143ILINKI939539I 626I17773I17479I14195-199
I---I---I----I------I----I-----I-----I---------------------------------
I129I144ILINKI939540I 596I27647I27386I14200-204
I---I---I----I------I----I-----I-----I---------------------------------
I130I145ILINKI939541I 602I17753I17562I14205-207
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939481-939507,939508(RUN 13536-13539),939509-939515,
             939524-939541 WERE SENT TO RUTHERFORD ON 12.7.83
   AIR.#  220-81256350

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I131I146ILINKI939542I 119I20761I20465I14208,209,218,220,222,223
I---I---I----I------I----I-----I-----I---------------------------------
I132I147ILINKI939543I 550I18879I18700I14224,233,241-246
I---I---I----I------I----I-----I-----I---------------------------------
I 15I148ITAPEI939544I1221I 5743I 5692I14210,213
I---I---I----I------I----I-----I-----I---------------------------------
I 16I149ITAPEI939545I1426I 5588I 5560I14215
I---I---I----I------I----I-----I-----I---------------------------------
I 17I150ITAPEI939546I1440I   48I   48I14216
I---I---I----I------I----I-----I-----I---------------------------------
I 18I151ITAPEI939548I1451I 1137I 1119I14219
I---I---I----I------I----I-----I-----I---------------------------------
I 19I152ITAPEI939558I1456I 3897I 3850I14221
I---I---I----I------I----I-----I-----I---------------------------------
I 20I153ITAPEI939559I1478I 6416I 6374I14225
I---I---I----I------I----I-----I-----I---------------------------------
I 21I154ITAPEI939560I1490I 5991I 5950I14228
I---I---I----I------I----I-----I-----I---------------------------------
I 22I155ITAPEI939561I1500I 5229I 5207I14229
I---I---I----I------I----I-----I-----I---------------------------------
I 23I156ITAPEI939562I1517I 5997I 5918I14230
I---I---I----I------I----I-----I-----I---------------------------------
I 24I157ITAPEI939563I1527I 6493I 6419I14231
I---I---I----I------I----I-----I-----I---------------------------------
I 25I158ITAPEI939564I1543I 5988I 5951I14232
I---I---I----I------I----I-----I-----I---------------------------------
I 26I159ITAPEI939565I1556I 5828I 5723I14235
I---I---I----I------I----I-----I-----I---------------------------------
I 27I160ITAPEI939478I1568I 6001I 5944I14236
I---I---I----I------I----I-----I-----I---------------------------------
I 28I161ITAPEI939479I1572I 6492I 6442I14237
I---I---I----I------I----I-----I-----I---------------------------------
I 29I162ITAPEI939480I1581I 3425I 3414I14238
I---I---I----I------I----I-----I-----I---------------------------------
I 30I163ITAPEI939516I1590I 5470I 5373I14239
I---I---I----I------I----I-----I-----I---------------------------------
I 31I164ITAPEI939517I1596I 6000I 5933I14240
I---I---I----I------I----I-----I-----I---------------------------------
I 32I165ITAPEI939518I1602I 6615I 6565I14254
I---I---I----I------I----I-----I-----I---------------------------------
I133I166ILINKI939519I 610I23732I23453I14247-253,255
I---I---I----I------I----I-----I-----I---------------------------------
I134I167ILINKI939520I 611I21684I21413I14256,257,260,261
I---I---I----I------I----I-----I-----I---------------------------------
I135I168ILINKI939521I 613I29454I29368I14262-276
I---I---I----I------I----I-----I-----I---------------------------------
I136I169ILINKI939522I1214I25781I25706I14268-271
I---I---I----I------I----I-----I-----I---------------------------------
I137I170ILINKI939523I1402I23952I23883I14272-275
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939542-939546,939548,939558-939565,939478-939480,
             939516-939523 WERE SENT TO RUTHERFORD ON 14.7.83
   AIR.#

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I138I171ILINKI948065I 404I25699I25629I14276,277,280,281
I---I---I----I------I----I-----I-----I---------------------------------
I139I172ILINKI948066I1484I19759I19665I14282-285
I---I---I----I------I----I-----I-----I---------------------------------
I140I173ILINKI948067I1487I19123I18881I14286-288
I---I---I----I------I----I-----I-----I---------------------------------
I141I174ILINKI948068I1489I21926I21646I14289-291
I---I---I----I------I----I-----I-----I---------------------------------
I142I175ILINKI948069I1493I19290I19104I14292-295
I---I---I----I------I----I-----I-----I---------------------------------
I143I176ILINKI948070I1529I19951I19775I14296-299
I---I---I----I------I----I-----I-----I---------------------------------
I144I177ILINKI948071I 324I20884I20651I14300-302
I---I---I----I------I----I-----I-----I---------------------------------
I145I178ILINKI948072I 330I16417I16204I14303-305
I---I---I----I------I----I-----I-----I---------------------------------
I146I179ILINKI948073I 338I22830I22676I14306-308
I---I---I----I------I----I-----I-----I---------------------------------
I147I180ILINKI948074I1668I21090I20914I14309-311
I---I---I----I------I----I-----I-----I---------------------------------
I148I181ILINKI948075I1011I19820I19648I14312-317
I---I---I----I------I----I-----I-----I---------------------------------
I149I182ILINKI948076I1018I20085I19817I14318-322
I---I---I----I------I----I-----I-----I---------------------------------
I150I183ILINKI948077I 309I23560I23112I14323-325
I---I---I----I------I----I-----I-----I---------------------------------
I151I184ILINKI948078I 598I13245I12566I14325-329
I---I---I----I------I----I-----I-----I---------------------------------
I152I185ILINKI948079I1700I22705I22334I14332,336-338
I---I---I----I------I----I-----I-----I---------------------------------
I153I186ILINKI948080I1703I17271I17061I14339-341
I---I---I----I------I----I-----I-----I---------------------------------
I154I187ILINKI948081I1873I15988I15822I14342-343
I---I---I----I------I----I-----I-----I---------------------------------
I155I188ILINKI948082I1228I20051I19697I14344-348
I---I---I----I------I----I-----I-----I---------------------------------
I156I189ILINKI948083I1230I21861I21489I14349-351
I---I---I----I------I----I-----I-----I---------------------------------
I157I190ILINKI948084I 711I21373I21084I14355-359
I---I---I----I------I----I-----I-----I---------------------------------
I158I191ILINKI948085I1107I15983I15785I14360-361
I---I---I----I------I----I-----I-----I---------------------------------
I159I192ILINKI948086I1634I20961I20642I14362-364
I---I---I----I------I----I-----I-----I---------------------------------
I 34I193ITAPEI948087I 709I 5191I 5148I14334
I---I---I----I------I----I-----I-----I---------------------------------
I 35I194ITAPEI948088I 718I 5583I 5543I14335
I---I---I----I------I----I-----I-----I---------------------------------
I160I195ILINKI948089I 747I22714I22423I14365-367
I---I---I----I------I----I-----I-----I---------------------------------
I161I196ILINKI948090I 477I14865I14688I14368-369
I---I---I----I------I----I-----I-----I---------------------------------
I162I197ILINKI948091I1129I22239I22066I14370-373
I---I---I----I------I----I-----I-----I---------------------------------
I163I198ILINKI948092I1137I28337I28279I14374-377
I---I---I----I------I----I-----I-----I---------------------------------
I164I199ILINKI948093I1140I29892I29820I14378-382
I---I---I----I------I----I-----I-----I---------------------------------
I165I200ILINKI948094I1151I24127I24062I14383-386
I---I---I----I------I----I-----I-----I---------------------------------
I166I201ILINKI948095I 236I15936I15861I14387-388
I---I---I----I------I----I-----I-----I---------------------------------
I167I202ILINKI948096I1671I23330I23275I14389-391
I---I---I----I------I----I-----I-----I---------------------------------
I168I203ILINKI948097I1690I23492I23263I14392-395
I---I---I----I------I----I-----I-----I---------------------------------
I169I204ILINKI948098I1718I23153I22925I14396-399
I---I---I----I------I----I-----I-----I---------------------------------
I 33I205ITAPEI948099I1750I 5689I 5627I14315
I---I---I----I------I----I-----I-----I---------------------------------
I170I206ILINKI948100I  59I18127I17895I14400-402
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 948065-948100 WERE SENT TO RUTHERFORD ON 21.7.83
   AIR.#

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I171I207ILINKI948101I 130I16249I15951I14403-405
I---I---I----I------I----I-----I-----I---------------------------------
I172I208ILINKI948102I 695I17315I16980I14406-409
I---I---I----I------I----I-----I-----I---------------------------------
I173I209ILINKI948103I 958I23994I23705I14410-412
I---I---I----I------I----I-----I-----I---------------------------------
I174I210ILINKI948104I 962I18092I17861I14413-415
I---I---I----I------I----I-----I-----I---------------------------------
I175I211ILINKI948105I 965I15990I15858I14416-417
I---I---I----I------I----I-----I-----I---------------------------------
I 36I212ITAPEI948106I1992I 5492I 5440I14422
I---I---I----I------I----I-----I-----I---------------------------------
I176I213ILINKI948107I1437I26221I25855I14419-421
I---I---I----I------I----I-----I-----I---------------------------------
I177I214ILINKI948108I1488I25648I25134I14423-426
I---I---I----I------I----I-----I-----I---------------------------------
I178I215ILINKI948109I1134I12178I11981I14427-428
I---I---I----I------I----I-----I-----I---------------------------------
I179I216ILINKI948110I1789I25803I25560I14429-433
I---I---I----I------I----I-----I-----I---------------------------------
I180I217ILINKI948111I1792I14975I14837I14434-437
I---I---I----I------I----I-----I-----I---------------------------------
I181I218ILINKI948112I1795I20783I20666I14438-440
I---I---I----I------I----I-----I-----I---------------------------------
I182I219ILINKI948113I 330I28604I28389I14441-445
I---I---I----I------I----I-----I-----I---------------------------------
I183I220ILINKI948114I 331I21793I21575I14446-449
I---I---I----I------I----I-----I-----I---------------------------------
I184I221ILINKI948228I1414I13980I13748I14450-452
I---I---I----I------I----I-----I-----I---------------------------------
I185I222ILINKI948229I1195I25178I24954I14453-456
I---I---I----I------I----I-----I-----I---------------------------------
I186I223ILINKI948230I1196I21064I20928I14457-461
I---I---I----I------I----I-----I-----I---------------------------------
I187I224ILINKI948231I1203I20433I20278I14462-464
I---I---I----I------I----I-----I-----I---------------------------------
I188I225ILINKI948232I1204I22890I22764I14465-467
I---I---I----I------I----I-----I-----I---------------------------------
I189I226ILINKI948233I1682I14485I14398I14468-470
I---I---I----I------I----I-----I-----I---------------------------------
I190I227ILINKI948234I1916I22249I21733I14471-473
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 948101-948114,948228-948234 WERE SENT TO RUTHERFORD
                                                   ON 25.7.83
   AIR.#

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I191I228ILINKI948235I 472I16406I16109I14474-476
I---I---I----I------I----I-----I-----I---------------------------------
I192I229ILINKI948236I 443I19748I19659I14477-479
I---I---I----I------I----I-----I-----I---------------------------------
I193I230ILINKI948237I 876I24053I23933I14480-483
I---I---I----I------I----I-----I-----I---------------------------------
I194I231ILINKI948238I1212I28246I28157I14484-487
I---I---I----I------I----I-----I-----I---------------------------------
I195I232ILINKI948239I1992I19423I19302I14488-490
I---I---I----I------I----I-----I-----I---------------------------------
I196I233ILINKI948240I 508I24285I24116I14491-494
I---I---I----I------I----I-----I-----I---------------------------------
I197I234ILINKI948241I1206I30387I30328I14497-500
I---I---I----I------I----I-----I-----I---------------------------------
I198I235ILINKI948242I1267I24456I24390I14501,504-507
I---I---I----I------I----I-----I-----I---------------------------------
I199I236ILINKI948243I  81I23831I23759I14508-511
I---I---I----I------I----I-----I-----I---------------------------------
I200I237ILINKI948244I 148I30066I30004I14512-514,518,519
I---I---I----I------I----I-----I-----I---------------------------------
I201I238ILINKI948245I 792I24457I24359I14520-523
I---I---I----I------I----I-----I-----I---------------------------------
I202I239ILINKI948246I 812I31077I30985I14524-528
I---I---I----I------I----I-----I-----I---------------------------------
I203I240ILINKI948247I1060I25690I25646I14529-532
I---I---I----I------I----I-----I-----I---------------------------------
I037I241ITAPEI948248I 713I 7533I 7526I14517
I---I---I----I------I----I-----I-----I---------------------------------
I204I242ILINKI948249I 120I28928I28842I14533-536
I---I---I----I------I----I-----I-----I---------------------------------
I205I243ILINKI948250I 703I30797I30686I14537-541
I---I---I----I------I----I-----I-----I---------------------------------
I206I244ILINKI948251I 706I18965I18867I14542-544,548
I---I---I----I------I----I-----I-----I---------------------------------

=======================================================================-
   TAPE 948318 : F11LHO.REFORM.JDATA09.G0015 (RUN 13685-688)
   TAPE 948319 + 948320 : F11LHO.REFORM.JDATA09.G0214 (RUN 14423-426)
========================================================================

   JDATA10 .. REFORMATTED GENERATION GROUP 10 STARTS HERE.

I207I001ILINKI948252I1186I30837I30771I14549-552
I---I---I----I------I----I-----I-----I---------------------------------
I208I002ILINKI948253I1197I19758I19503I14553-557
I---I---I----I------I----I-----I-----I---------------------------------
I209I003ILINKI948254I1223I17012I16861I14558,559,563
I---I---I----I------I----I-----I-----I---------------------------------
I210I004ILINKI948255I1233I21702I21551I14564-569
I---I---I----I------I----I-----I-----I---------------------------------
I211I005ILINKI948256I1240I27235I27091I14570-573
I---I---I----I------I----I-----I-----I---------------------------------
I212I006ILINKI948257I1246I17014I16920I14574-576
I---I---I----I------I----I-----I-----I---------------------------------
I213I007ILINKI948258I1278I27375I27227I14577-582
I---I---I----I------I----I-----I-----I---------------------------------
I214I008ILINKI948259I 683I17695I17600I14583-586
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 948235-948259,948319,948320 WERE SENT TO RUTHERFORD
                                                   ON 3.8.83
   AIR.#

 -------------------------------------------------------------------
         9 TAPES WHICH WERE SENT TO RUTHERFORD ON 31.8.83
         -------------------------------------------------
  REFORM 9/072  RUNS 13923-13962          -> 948321,948322
  REFORM 9/136  RUN  14171                -> 948323
  REFORM 9/137  RUNS 14166-14169          -> 948324
  REFORM 9/138  RUNS 14170-14175          -> 948325
  REFORM 9/139  RUNS 14176-14179          -> 948326
  REFORM 9/140  RUNS 14180-14183          -> 948327
  REFORM 9/141  RUNS 14184-14185          -> 948316
  REFORM 9/142  RUNS 14186,14191-14194    -> 948317
---------------------------------------------------------------------

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I215I009ILINKI948260I    I 2877I 2856I14587-589
I---I---I----I------I----I-----I-----I---------------------------------
I216I010ILINKI948261I1991I18426I27800I14605-610,612,613
I---I---I----I------I----I-----I-----I---------------------------------
I217I011ILINKI948262I  53I21724I21465I14614-617,626-630
I---I---I----I------I----I-----I-----I---------------------------------
I218I012ILINKI948263I  54I18917I18811I14631-636
I---I---I----I------I----I-----I-----I---------------------------------
I219I013ILINKI948264I 922I22490I22368I14643-646
I---I---I----I------I----I-----I-----I---------------------------------
I220I014ILINKI948265I 929I19283I19124I14647-651
I---I---I----I------I----I-----I-----I---------------------------------
I221I015ILINKI948266I 937I27628I27373I14652-655
I---I---I----I------I----I-----I-----I---------------------------------
I 38I016ITAPEI948267I1245I 4232I 4203I14611
I---I---I----I------I----I-----I-----I---------------------------------
I 39I017ITAPEI948268I1256I 7070I 7042I14641
I---I---I----I------I----I-----I-----I---------------------------------
I222I018ILINKI948269I 241I21436I21198I14656-659
I---I---I----I------I----I-----I-----I---------------------------------
I223I019ILINKI948270I 242I24421I24311I14660-661,665-667
I---I---I----I------I----I-----I-----I---------------------------------
I224I020ILINKI948271I 244I22176I22020I14668-675
I---I---I----I------I----I-----I-----I---------------------------------
I225I021ILINKI948272I 246I23460I23289I14676-679
I            I939566I     NEW COPY SEND TO RUTH
I---I---I----I------I----I-----I-----I---------------------------------
I226I022ILINKI948273I1774I21871I21765I14680-683
I            I939567I     NEW COPY SEND TO RUTH
I---I---I----I------I----I-----I-----I---------------------------------
I227I023ILINKI948274I1776I25503I25319I14684-690
I---I---I----I------I----I-----I-----I---------------------------------
I228I024ILINKI948275I1777I20403I20256I14691-693
I---I---I----I------I----I-----I-----I---------------------------------
I229I025ILINKI948276I1780I19971I19848I14694-696
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 948260-948276 WERE SENT TO RUTHERFORDON 7.10.83
   AIR.#


I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I230I026ILINKI948277I 601I17426I16851I14697-700
I---I---I----I------I----I-----I-----I---------------------------------
I231I027ILINKI948278I 605I26014I25856I14701-706
I---I---I----I------I----I-----I-----I---------------------------------
I232I028ILINKI948279I1250I20607I20443I14707-710
I---I---I----I------I----I-----I-----I---------------------------------
I233I029ILINKI948280I1253I21427I21284I14711,712,717,718
I---I---I----I------I----I-----I-----I---------------------------------
I234I030ILINKI948281I1833I26544I26125I14719-722
I---I---I----I------I----I-----I-----I---------------------------------
I235I031ILINKI948282I 210I19882I19694I14723-725
I---I---I----I------I----I-----I-----I---------------------------------
I236I032ILINKI948283I 990I21029I20871I14726-730
I---I---I----I------I----I-----I-----I---------------------------------
I237I033ILINKI948284I 168I23711I23584I14731-735
I---I---I----I------I----I-----I-----I---------------------------------
I238I034ILINKI948285I 175I25292I25180I14736-739
I---I---I----I------I----I-----I-----I---------------------------------
I239I035ILINKI948286I1219I23163I23068I14740-744
I---I---I----I------I----I-----I-----I---------------------------------
I240I036ILINKI948287I1228I19189I19047I14745-748
I---I---I----I------I----I-----I-----I---------------------------------
I040I037ITAPEI948288I1251I 5047I 5017I14716
I---I---I----I------I----I-----I-----I---------------------------------
I241I038ILINKI948289I1278I23766I23646I14749-752
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 948277-948289 WERE SENT TO RUTHERFORDON 11.10.83
   AIR.#


I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I242I039ILINKI948290I1682I14010I13870I14753-755
I---I---I----I------I----I-----I-----I---------------------------------
I243I040ILINKI948291I1687I21798I21664I14756-759
I---I---I----I------I----I-----I-----I---------------------------------
I244I041ILINKI948292I 233I24504I24312I14760-764
I---I---I----I------I----I-----I-----I---------------------------------
I245I042ILINKI948293I 435I14696I14581I14765-766
I---I---I----I------I----I-----I-----I---------------------------------
I246I043ILINKI948294I 887I19300I19147I14767-769
I---I---I----I------I----I-----I-----I---------------------------------
I247I044ILINKI948295I 892I16271I16162I14770-772
I---I---I----I------I----I-----I-----I---------------------------------
I248I045ILINKI948296I1817I21977I21743I14773-775
I---I---I----I------I----I-----I-----I---------------------------------
I249I046ILINKI948297I 223I18128I17942I14776-778
I---I---I----I------I----I-----I-----I---------------------------------
I001I047ILINKI948298I 343I24640I24363I14779-783
I---I---I----I------I----I-----I-----I---------------------------------
I002I048ILINKI948299I1570I15951I15886I14784-787
I---I---I----I------I----I-----I-----I---------------------------------
I003I049ILINKI948300I1589I26893I26788I14788-791
I---I---I----I------I----I-----I-----I---------------------------------
I004I050ILINKI948301I1667I20855I20794I14792-794
I---I---I----I------I----I-----I-----I---------------------------------
I005I051ILINKI948302I1693I21881I21782I14795-798
I---I---I----I------I----I-----I-----I---------------------------------
I006I052ILINKI948303I1698I17169I16976I14800-811
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 948290-948303 WERE SENT TO RUTHERFORD ON 14.10.83
   AIR.#


I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I007I053ILINKI948304I1701I27920I27795I14812-816
I---I---I----I------I----I-----I-----I---------------------------------
I008I054ILINKI948305I1463I22084I21084I14817-820
I---I---I----I------I----I-----I-----I---------------------------------
I009I055ILINKI948306I 836I21160I21078I14821-824
I---I---I----I------I----I-----I-----I---------------------------------
I010I056ILINKI948307I 838I25110I24970I14825-828
I---I---I----I------I----I-----I-----I---------------------------------
I011I057ILINKI948308I 839I22513I22413I14829-833
I---I---I----I------I----I-----I-----I---------------------------------
I012I058ILINKI948309I1158I20789I20707I14834-837
I---I---I----I------I----I-----I-----I---------------------------------
I013I059ILINKI948310I1781I26355I26233I14838-841
I---I---I----I------I----I-----I-----I---------------------------------
I014I060ILINKI948311I 109I22552I22371I14842-845
I---I---I----I------I----I-----I-----I---------------------------------
I015I061ILINKI948312I  15I19021I18919I14846-849
I---I---I----I------I----I-----I-----I---------------------------------
I016I062ILINKI948313I1103I24392I24267I14850,854-856
I---I---I----I------I----I-----I-----I---------------------------------
I017I063ILINKI948314I1927I21782I21678I14857,862,863,870
I                        14859,14861,14865,14867,14868 WERE ON NORD TAPE
I---I---I----I------I----I-----I-----I---------------------------------
I018I064ILINKI948315I1612I19650I19489I14871-873
I---I---I----I------I----I-----I-----I---------------------------------
I019I065ILINKI939441I1633I22583I22477I14874-877
I---I---I----I------I----I-----I-----I---------------------------------
I020I066ILINKI939442I 366I24371I24292I14878-882
I---I---I----I------I----I-----I-----I---------------------------------
I021I067ILINKI939443I 675I20267I20212I14883-886
I---I---I----I------I----I-----I-----I---------------------------------
I022I068ILINKI939444I1046I21410I21340I14887-891
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 948304-948315
             939441-939444 WERE SENT TO RUTHERFORD ON 18.10.83
   AIR.#


I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I023I069ILINKI939445I1032I22854I22778I14892-895
I---I---I----I------I----I-----I-----I---------------------------------
I024I070ILINKI939446I 254I24694I24592I14896,897,899-906,910,911
I                                          14907,14909 WERE ON NORD TAPE
I---I---I----I------I----I-----I-----I---------------------------------
I025I071ILINKI939447I 717I18527I18449I14912-914
I---I---I----I------I----I-----I-----I---------------------------------
I026I072ILINKI939448I1562I24340I24206I14915-923
I---I---I----I------I----I-----I-----I---------------------------------
I027I073ILINKI939449I1602I21440I21363I14924-929
I---I---I----I------I----I-----I-----I---------------------------------
I028I074ILINKI939450I1628I20628I20534I14930-932
I---I---I----I------I----I-----I-----I---------------------------------
T041I075ITAPEI939451I1801I 2702I 2689I14859
I---I---I----I------I----I-----I-----I---------------------------------
T042I076ITAPEI939452I1830I 5931I 5917I14867-868
I---I---I----I------I----I-----I-----I---------------------------------
T043I077ITAPEI939453I1857I 5960I 5948I14865
I---I---I----I------I----I-----I-----I---------------------------------
T044I078ITAPEI939454I1998I 4264I 4250I14861
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939445-939454 WERE SENT TO RUTHERFORD ON 24.10.83
   AIR.#


I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I029I079ILINKI939455I1697I23018I22954I14933-940
I---I---I----I------I----I-----I-----I---------------------------------
I030I080ILINKI939456I1707I21826I21747I14941-946
I---I---I----I------I----I-----I-----I---------------------------------
I031I081ILINKI939457I  10I23000I22654I14947-950
I---I---I----I------I----I-----I-----I---------------------------------
I032I082ILINKI939458I1057I23283I23202I14951-956
I---I---I----I------I----I-----I-----I---------------------------------
I033I083ILINKI939459I1060I24905I24765I14957,969-972
I14965,66,67,68 ARE ON NORD TAPE
I---I---I----I------I----I-----I-----I---------------------------------
I034I084ILINKI939460I 525I17207I17158I14973-976
I---I---I----I------I----I-----I-----I---------------------------------
I035I085ILINKI939461I1255I19247I19173I14977-980
I---I---I----I------I----I-----I-----I---------------------------------
I036I086ILINKI939462I 746I19941I19819I14981-984
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939455-939462 AND
             939566,939567 WERE SENT TO RUTHERFORD ON 24.10.83
   AIR.#


I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I037I087ILINKI939549I 748I17973I17873I14985-988
I---I---I----I------I----I-----I-----I---------------------------------
I038I088ILINKI939464I 753I21310I21211I14989-992
I---I---I----I------I----I-----I-----I---------------------------------
I039I089ILINKI939465I1435I18529I18464I14993-995
I---I---I----I------I----I-----I-----I---------------------------------
I040I090ILINKI939466I 258I27113I27018I14996-15000
I---I---I----I------I----I-----I-----I---------------------------------
I041I091ILINKI939467I1097I14168I14098I15001-003
I---I---I----I------I----I-----I-----I---------------------------------
I042I092ILINKI939468I1357I21248I21114I15004-007
I---I---I----I------I----I-----I-----I---------------------------------
I045I093ITAPEI939469I1573I 2990I 2971I14908-909
I---I---I----I------I----I-----I-----I---------------------------------
I046I094ITAPEI939470I1588I  443I  440I14907
I---I---I----I------I----I-----I-----I---------------------------------
I047I095ITAPEI939471I1602I  621I  619I14898
I---I---I----I------I----I-----I-----I---------------------------------
I048I096ITAPEI939472I1608I 1248I 1240I14968
I---I---I----I------I----I-----I-----I---------------------------------
I049I097ITAPEI939473I1666I 6535I 6508I14965-967
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939549,939464-939470,
             939472-939473 WERE SENT TO RUTHERFORD ON 25.10.83
   AIR.#


I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I043I098ILINKI939586I 976I20653I20560I15008-011
I---I---I----I------I----I-----I-----I---------------------------------
I044I099ILINKI      I 617I23317I23198I15012-017
I---I---I----I------I----I-----I-----I---------------------------------
I045I100ILINKI939568I1944I17271I17190I15018-020
I---I---I----I------I----I-----I-----I---------------------------------
I046I101ILINKI939469I 733I23966I23878I15021-025
I---I---I----I------I----I-----I-----I---------------------------------
I047I102ILINKI939570I1949I22491I22544I15026-030
I---I---I----I------I----I-----I-----I---------------------------------
I048I103ILINKI939575I1957I20960I20896I15031-036
I---I---I----I------I----I-----I-----I---------------------------------
I049I104ILINKI939576I 960I21253I21169I15037-041
I---I---I----I------I----I-----I-----I---------------------------------
I050I105ILINKI939577I1903I22928I22802I15042-046
I---I---I----I------I----I-----I-----I---------------------------------
I050I106ITAPEI939581I 603I 2690I 2683I15044
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939471,939586,939582,939668-939570,
             939575-939577,939581 WERE SENT TO RUTHERFORD ON 28.10.83
   AIR.#


I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I051I107ILINKI939578I 696I21787I21696I15047-052
I---I---I----I------I----I-----I-----I---------------------------------
I052I108ILINKI939579I 718I30229I30093I15053-061
I---I---I----I------I----I-----I-----I---------------------------------
I053I109ILINKI939580I1849I24681I24594I15062-066
I---I---I----I------I----I-----I-----I---------------------------------
I054I110ILINKI939503I1566I22866I22764I15067-071
I---I---I----I------I----I-----I-----I---------------------------------
I055I111ILINKI939504I1687I22236I22174I15072-075
I---I---I----I------I----I-----I-----I---------------------------------
I056I112ILINKI939505I1821I24384I24308I15076-082
I---I---I----I------I----I-----I-----I---------------------------------
I057I113ILINKI939506I1001I22837I22758I15083-086
I---I---I----I------I----I-----I-----I---------------------------------
I058I114ILINKI939507I1002I26342I26241I15087-092
I---I---I----I------I----I-----I-----I---------------------------------
I059I115ILINKI939508I 537I15570I15524I15093-095
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939584,939578-939580,939503-939508
             WERE SENT TO RUTHERFORD ON 2.11.83
   AIR.#


I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I060I116ILINKI939509I 576I25084I24930I15096-103
I---I---I----I------I----I-----I-----I---------------------------------
I061I117ILINKI939510I 710I20593I20501I15104-107
I---I---I----I------I----I-----I-----I---------------------------------
I062I118ILINKI939511I 717I19199I19121I15108-111
I---I---I----I------I----I-----I-----I---------------------------------
I063I119ILINKI939512I 226I21236I21173I15112-115
I---I---I----I------I----I-----I-----I---------------------------------
I064I120ILINKI939513I1487I25093I24952I15116-120
I---I---I----I------I----I-----I-----I---------------------------------
I065I121ILINKI939514I 570I16167I16096I15121,123-126
I---I---I----I------I----I-----I-----I---------------------------------
I066I122ILINKI939515I 587I26039I25986I15128-132
I---I---I----I------I----I-----I-----I---------------------------------
I067I123ILINKI939516I 591I23245I23167I15133-137
I---I---I----I------I----I-----I-----I---------------------------------
I068I124ILINKI939517I 623I21961I21861I15138-142
I---I---I----I------I----I-----I-----I---------------------------------
I069I125ILINKI939518I 651I21688I21604I15143-147
I---I---I----I------I----I-----I-----I---------------------------------
I070I126ILINKI939519I 675I24788I24640I15148-153
I---I---I----I------I----I-----I-----I---------------------------------
I071I127ILINKI939520I 689I17374I17315I15154-156
I---I---I----I------I----I-----I-----I---------------------------------
I072I128ILINKI939521I 711I23721I23628I15157-161
I---I---I----I------I----I-----I-----I---------------------------------
I073I129ILINKI939522I1923I21350I21284I15162-165
I---I---I----I------I----I-----I-----I---------------------------------
I074I130ILINKI939523I 736I19361I19316I15166-170
I---I---I----I------I----I-----I-----I---------------------------------
I075I131ILINKI939526I 765I25561I25452I15171-175
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939509-939523,939526
             WERE SENT TO RUTHERFORD ON 8.11.83

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I076I132ILINKI939532I  14I18397I18318I15176-180
I---I---I----I------I----I-----I-----I---------------------------------
I077I133ILINKI939533I 218I22112I22030I15181-184
I---I---I----I------I----I-----I-----I---------------------------------
I078I134ILINKI939534I 523I21752I21669I15185-190
I---I---I----I------I----I-----I-----I---------------------------------
I079I135ILINKI939535I 526I24866I24732I15191-194
I---I---I----I------I----I-----I-----I---------------------------------
I080I136ILINKI939536I 303I21436I21353I15195-199
I---I---I----I------I----I-----I-----I---------------------------------
I051I137ITAPEI939537I1222I 5600I 5530I15205
I---I---I----I------I----I-----I-----I---------------------------------
I052I138ITAPEI939538I1234I 6451I 6420I15206-207
I---I---I----I------I----I-----I-----I---------------------------------
I053I139ITAPEI939539I1247I 6561I 6541I15208-209
I---I---I----I------I----I-----I-----I---------------------------------
I054I140ITAPEI939540I1267I 3654I 3641I15210
I---I---I----I------I----I-----I-----I---------------------------------
I081I141ILINKI939542I 241I21248I21149I15200-204
I---I---I----I------I----I-----I-----I---------------------------------
I082I142ILINKI939543I1025I22564I22428I15211-217
I---I---I----I------I----I-----I-----I---------------------------------
I083I143ILINKI939544I1220I18354I18264I15218-221
I---I---I----I------I----I-----I-----I---------------------------------
I084I144ILINKI939545I 240I22991I22907I15222-226
I---I---I----I------I----I-----I-----I---------------------------------
I085I145ILINKI939546I 891I19982I19904I15227-233
I---I---I----I------I----I-----I-----I---------------------------------
I086I146ILINKI939548I 539I24869I24712I15236-241
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939532-939540,939542-939546,939548
             WERE SENT TO RUTHERFORD ON 14.11.83

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I087I147ILINKI939550I 822I15461I15396I15242,243,246-249
I---I---I----I------I----I-----I-----I---------------------------------
I055I148ITAPEI939551I 524I 4646I 4632I15235
I---I---I----I------I----I-----I-----I---------------------------------
I056I149ITAPEI939552I 533I 5415I 5370I15250
I---I---I----I------I----I-----I-----I---------------------------------
I057I150ITAPEI939553I 554I 6197I 6176I15251
I---I---I----I------I----I-----I-----I---------------------------------
I088I151ILINKI939554I1019I21274I21156I15252-255
I---I---I----I------I----I-----I-----I---------------------------------
I089I152ILINKI939555I1777I21608I21518I15256-261
I---I---I----I------I----I-----I-----I---------------------------------
I090I153ILINKI939556I 524I17586I17512I15262-264
I---I---I----I------I----I-----I-----I---------------------------------
I091I154ILINKI939557I1850I18329I18225I15265-269
I---I---I----I------I----I-----I-----I---------------------------------
I092I155ILINKI939558I 453I19389I19202I15270-276
I---I---I----I------I----I-----I-----I---------------------------------
I093I156ILINKI939559I 456I19841I19776I15277-281
I---I---I----I------I----I-----I-----I---------------------------------
I094I157ILINKI939560I 676I28292I28063I15282-287
I---I---I----I------I----I-----I-----I---------------------------------
I095I158ILINKI939561I 602I20139I20040I15288-292
I---I---I----I------I----I-----I-----I---------------------------------
I096I159ILINKI939562I1553I19017I18921I15293-296
I---I---I----I------I----I-----I-----I---------------------------------
I097I160ILINKI939563I1885I16246I16202I15297-299
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939550-939563
             WERE SENT TO RUTHERFORD ON 18.11.83

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I098I161ILINKI939564I    I     I     I
I---I---I----I------I----I-----I-----I---------------------------------
    17/11/82 407121405 MEMBER NAME  RUNLST2  (JADESR)      TEXT

                  ========================================
                  JADE DATA TAPES SINCE THE SEPTEMBER 1983
                  ========================================

    THE DATA GENERATION GROUPS FOR THE IBM COPY AND REFORMATTED TAPES
    ARE RESPECTIVELY:

         F22YEN.JADE.EXDATA09.G0182V00, G0183V00 ETC.
    AND  F11LHO.JDATA10.REFORM.G0200V00, G0201V00 ETC.

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I215I009ILINKI948260I    I 2877I 2856I14587-589
I---I---I----I------I----I-----I-----I---------------------------------
I216I010ILINKI948261I1991I18426I27800I14605-610,612,613
I---I---I----I------I----I-----I-----I---------------------------------
I217I011ILINKI948262I  53I21724I21465I14614-617,626-630
I---I---I----I------I----I-----I-----I---------------------------------
I218I012ILINKI948263I  54I18917I18811I14631-636
I---I---I----I------I----I-----I-----I---------------------------------
I219I013ILINKI948264I 922I22490I22368I14643-646
I---I---I----I------I----I-----I-----I---------------------------------
I220I014ILINKI948265I 929I19283I19124I14647-651
I---I---I----I------I----I-----I-----I---------------------------------
I221I015ILINKI948266I 937I27628I27373I14652-655
I---I---I----I------I----I-----I-----I---------------------------------
I 38I016ITAPEI948267I1245I 4232I 4203I14611
I---I---I----I------I----I-----I-----I---------------------------------
I 39I017ITAPEI948268I1256I 7070I 7042I14641
I---I---I----I------I----I-----I-----I---------------------------------
I222I018ILINKI948269I 241I21436I21198I14656-659
I---I---I----I------I----I-----I-----I---------------------------------
I223I019ILINKI948270I 242I24421I24311I14660-661,665-667
I---I---I----I------I----I-----I-----I---------------------------------
I224I020ILINKI948271I 244I22176I22020I14668-675
I---I---I----I------I----I-----I-----I---------------------------------
I225I021ILINKI948272I 246I23460I23289I14676-679
I            I939566I     NEW COPY SEND TO RUTH
I---I---I----I------I----I-----I-----I---------------------------------
I226I022ILINKI948273I1774I21871I21765I14680-683
I            I939567I     NEW COPY SEND TO RUTH
I---I---I----I------I----I-----I-----I---------------------------------
I227I023ILINKI948274I1776I25503I25319I14684-690
I---I---I----I------I----I-----I-----I---------------------------------
I228I024ILINKI948275I1777I20403I20256I14691-693
I---I---I----I------I----I-----I-----I---------------------------------
I229I025ILINKI948276I1780I19971I19848I14694-696
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 948260-948276 WERE SENT TO RUTHERFORDON 7.10.83
   AIR.#


I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I230I026ILINKI948277I 601I17426I16851I14697-700
I---I---I----I------I----I-----I-----I---------------------------------
I231I027ILINKI948278I 605I26014I25856I14701-706
I---I---I----I------I----I-----I-----I---------------------------------
I232I028ILINKI948279I1250I20607I20443I14707-710
I---I---I----I------I----I-----I-----I---------------------------------
I233I029ILINKI948280I1253I21427I21284I14711,712,717,718
I---I---I----I------I----I-----I-----I---------------------------------
I234I030ILINKI948281I1833I26544I26125I14719-722
I---I---I----I------I----I-----I-----I---------------------------------
I235I031ILINKI948282I 210I19882I19694I14723-725
I---I---I----I------I----I-----I-----I---------------------------------
I236I032ILINKI948283I 990I21029I20871I14726-730
I---I---I----I------I----I-----I-----I---------------------------------
I237I033ILINKI948284I 168I23711I23584I14731-735
I---I---I----I------I----I-----I-----I---------------------------------
I238I034ILINKI948285I 175I25292I25180I14736-739
I---I---I----I------I----I-----I-----I---------------------------------
I239I035ILINKI948286I1219I23163I23068I14740-744
I---I---I----I------I----I-----I-----I---------------------------------
I240I036ILINKI948287I1228I19189I19047I14745-748
I---I---I----I------I----I-----I-----I---------------------------------
I040I037ITAPEI948288I1251I 5047I 5017I14716
I---I---I----I------I----I-----I-----I---------------------------------
I241I038ILINKI948289I1278I23766I23646I14749-752
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 948277-948289 WERE SENT TO RUTHERFORDON 11.10.83
   AIR.#


I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I242I039ILINKI948290I1682I14010I13870I14753-755
I---I---I----I------I----I-----I-----I---------------------------------
I243I040ILINKI948291I1687I21798I21664I14756-759
I---I---I----I------I----I-----I-----I---------------------------------
I244I041ILINKI948292I 233I24504I24312I14760-764
I---I---I----I------I----I-----I-----I---------------------------------
I245I042ILINKI948293I 435I14696I14581I14765-766
I---I---I----I------I----I-----I-----I---------------------------------
I246I043ILINKI948294I 887I19300I19147I14767-769
I---I---I----I------I----I-----I-----I---------------------------------
I247I044ILINKI948295I 892I16271I16162I14770-772
I---I---I----I------I----I-----I-----I---------------------------------
I248I045ILINKI948296I1817I21977I21743I14773-775
I---I---I----I------I----I-----I-----I---------------------------------
I249I046ILINKI948297I 223I18128I17942I14776-778
I---I---I----I------I----I-----I-----I---------------------------------
I001I047ILINKI948298I 343I24640I24363I14779-783
I---I---I----I------I----I-----I-----I---------------------------------
I002I048ILINKI948299I1570I15951I15886I14784-787
I---I---I----I------I----I-----I-----I---------------------------------
I003I049ILINKI948300I1589I26893I26788I14788-791
I---I---I----I------I----I-----I-----I---------------------------------
I004I050ILINKI948301I1667I20855I20794I14792-794
I---I---I----I------I----I-----I-----I---------------------------------
I005I051ILINKI948302I1693I21881I21782I14795-798
I---I---I----I------I----I-----I-----I---------------------------------
I006I052ILINKI948303I1698I17169I16976I14800-811
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 948290-948303 WERE SENT TO RUTHERFORD ON 14.10.83
   AIR.#


I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I007I053ILINKI948304I1701I27920I27795I14812-816
I---I---I----I------I----I-----I-----I---------------------------------
I008I054ILINKI948305I1463I22084I21084I14817-820
I---I---I----I------I----I-----I-----I---------------------------------
I009I055ILINKI948306I 836I21160I21078I14821-824
I---I---I----I------I----I-----I-----I---------------------------------
I010I056ILINKI948307I 838I25110I24970I14825-828
I---I---I----I------I----I-----I-----I---------------------------------
I011I057ILINKI948308I 839I22513I22413I14829-833
I---I---I----I------I----I-----I-----I---------------------------------
I012I058ILINKI948309I1158I20789I20707I14834-837
I---I---I----I------I----I-----I-----I---------------------------------
I013I059ILINKI948310I1781I26355I26233I14838-841
I---I---I----I------I----I-----I-----I---------------------------------
I014I060ILINKI948311I 109I22552I22371I14842-845
I---I---I----I------I----I-----I-----I---------------------------------
I015I061ILINKI948312I  15I19021I18919I14846-849
I---I---I----I------I----I-----I-----I---------------------------------
I016I062ILINKI948313I1103I24392I24267I14850,854-856
I---I---I----I------I----I-----I-----I---------------------------------
I017I063ILINKI948314I1927I21782I21678I14857,862,863,870
I                        14859,14861,14865,14867,14868 WERE ON NORD TAPE
I---I---I----I------I----I-----I-----I---------------------------------
I018I064ILINKI948315I1612I19650I19489I14871-873
I---I---I----I------I----I-----I-----I---------------------------------
I019I065ILINKI939441I1633I22583I22477I14874-877
I---I---I----I------I----I-----I-----I---------------------------------
I020I066ILINKI939442I 366I24371I24292I14878-882
I---I---I----I------I----I-----I-----I---------------------------------
I021I067ILINKI939443I 675I20267I20212I14883-886
I---I---I----I------I----I-----I-----I---------------------------------
I022I068ILINKI939444I1046I21410I21340I14887-891
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 948304-948315
             939441-939444 WERE SENT TO RUTHERFORD ON 18.10.83
   AIR.#


I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I023I069ILINKI939445I1032I22854I22778I14892-895
I---I---I----I------I----I-----I-----I---------------------------------
I024I070ILINKI939446I 254I24694I24592I14896,897,899-906,910,911
I                                          14907,14909 WERE ON NORD TAPE
I---I---I----I------I----I-----I-----I---------------------------------
I025I071ILINKI939447I 717I18527I18449I14912-914
I---I---I----I------I----I-----I-----I---------------------------------
I026I072ILINKI939448I1562I24340I24206I14915-923
I---I---I----I------I----I-----I-----I---------------------------------
I027I073ILINKI939449I1602I21440I21363I14924-929
I---I---I----I------I----I-----I-----I---------------------------------
I028I074ILINKI939450I1628I20628I20534I14930-932
I---I---I----I------I----I-----I-----I---------------------------------
T041I075ITAPEI939451I1801I 2702I 2689I14859
I---I---I----I------I----I-----I-----I---------------------------------
T042I076ITAPEI939452I1830I 5931I 5917I14867-868
I---I---I----I------I----I-----I-----I---------------------------------
T043I077ITAPEI939453I1857I 5960I 5948I14865
I---I---I----I------I----I-----I-----I---------------------------------
T044I078ITAPEI939454I1998I 4264I 4250I14861
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939445-939454 WERE SENT TO RUTHERFORD ON 24.10.83
   AIR.#


I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I029I079ILINKI939455I1697I23018I22954I14933-940
I---I---I----I------I----I-----I-----I---------------------------------
I030I080ILINKI939456I1707I21826I21747I14941-946
I---I---I----I------I----I-----I-----I---------------------------------
I031I081ILINKI939457I  10I23000I22654I14947-950
I---I---I----I------I----I-----I-----I---------------------------------
I032I082ILINKI939458I1057I23283I23202I14951-956
I---I---I----I------I----I-----I-----I---------------------------------
I033I083ILINKI939459I1060I24905I24765I14957,969-972
I14965,66,67,68 ARE ON NORD TAPE
I---I---I----I------I----I-----I-----I---------------------------------
I034I084ILINKI939460I 525I17207I17158I14973-976
I---I---I----I------I----I-----I-----I---------------------------------
I035I085ILINKI939461I1255I19247I19173I14977-980
I---I---I----I------I----I-----I-----I---------------------------------
I036I086ILINKI939462I 746I19941I19819I14981-984
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939455-939462 AND
             939566,939567 WERE SENT TO RUTHERFORD ON 24.10.83
   AIR.#


I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I037I087ILINKI939549I 748I17973I17873I14985-988
I---I---I----I------I----I-----I-----I---------------------------------
I038I088ILINKI939464I 753I21310I21211I14989-992
I---I---I----I------I----I-----I-----I---------------------------------
I039I089ILINKI939465I1435I18529I18464I14993-995
I---I---I----I------I----I-----I-----I---------------------------------
I040I090ILINKI939466I 258I27113I27018I14996-15000
I---I---I----I------I----I-----I-----I---------------------------------
I041I091ILINKI939467I1097I14168I14098I15001-003
I---I---I----I------I----I-----I-----I---------------------------------
I042I092ILINKI939468I1357I21248I21114I15004-007
I---I---I----I------I----I-----I-----I---------------------------------
I045I093ITAPEI939469I1573I 2990I 2971I14908-909
I---I---I----I------I----I-----I-----I---------------------------------
I046I094ITAPEI939470I1588I  443I  440I14907
I---I---I----I------I----I-----I-----I---------------------------------
I047I095ITAPEI939471I1602I  621I  619I14898
I---I---I----I------I----I-----I-----I---------------------------------
I048I096ITAPEI939472I1608I 1248I 1240I14968
I---I---I----I------I----I-----I-----I---------------------------------
I049I097ITAPEI939473I1666I 6535I 6508I14965-967
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939549,939464-939470,
             939472-939473 WERE SENT TO RUTHERFORD ON 25.10.83
   AIR.#


I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I043I098ILINKI939586I 976I20653I20560I15008-011
I---I---I----I------I----I-----I-----I---------------------------------
I044I099ILINKI      I 617I23317I23198I15012-017
I---I---I----I------I----I-----I-----I---------------------------------
I045I100ILINKI939568I1944I17271I17190I15018-020
I---I---I----I------I----I-----I-----I---------------------------------
I046I101ILINKI939469I 733I23966I23878I15021-025
I---I---I----I------I----I-----I-----I---------------------------------
I047I102ILINKI939570I1949I22491I22544I15026-030
I---I---I----I------I----I-----I-----I---------------------------------
I048I103ILINKI939575I1957I20960I20896I15031-036
I---I---I----I------I----I-----I-----I---------------------------------
I049I104ILINKI939576I 960I21253I21169I15037-041
I---I---I----I------I----I-----I-----I---------------------------------
I050I105ILINKI939577I1903I22928I22802I15042-046
I---I---I----I------I----I-----I-----I---------------------------------
I050I106ITAPEI939581I 603I 2690I 2683I15044
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939471,939586,939582,939668-939570,
             939575-939577,939581 WERE SENT TO RUTHERFORD ON 28.10.83
   AIR.#


I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I051I107ILINKI939578I 696I21787I21696I15047-052
I---I---I----I------I----I-----I-----I---------------------------------
I052I108ILINKI939579I 718I30229I30093I15053-061
I---I---I----I------I----I-----I-----I---------------------------------
I053I109ILINKI939580I1849I24681I24594I15062-066
I---I---I----I------I----I-----I-----I---------------------------------
I054I110ILINKI939503I1566I22866I22764I15067-071
I---I---I----I------I----I-----I-----I---------------------------------
I055I111ILINKI939504I1687I22236I22174I15072-075
I---I---I----I------I----I-----I-----I---------------------------------
I056I112ILINKI939505I1821I24384I24308I15076-082
I---I---I----I------I----I-----I-----I---------------------------------
I057I113ILINKI939506I1001I22837I22758I15083-086
I---I---I----I------I----I-----I-----I---------------------------------
I058I114ILINKI939507I1002I26342I26241I15087-092
I---I---I----I------I----I-----I-----I---------------------------------
I059I115ILINKI939508I 537I15570I15524I15093-095
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939584,939578-939580,939503-939508
             WERE SENT TO RUTHERFORD ON 2.11.83
   AIR.#


I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I060I116ILINKI939509I 576I25084I24930I15096-103
I---I---I----I------I----I-----I-----I---------------------------------
I061I117ILINKI939510I 710I20593I20501I15104-107
I---I---I----I------I----I-----I-----I---------------------------------
I062I118ILINKI939511I 717I19199I19121I15108-111
I---I---I----I------I----I-----I-----I---------------------------------
I063I119ILINKI939512I 226I21236I21173I15112-115
I---I---I----I------I----I-----I-----I---------------------------------
I064I120ILINKI939513I1487I25093I24952I15116-120
I---I---I----I------I----I-----I-----I---------------------------------
I065I121ILINKI939514I 570I16167I16096I15121,123-126
I---I---I----I------I----I-----I-----I---------------------------------
I066I122ILINKI939515I 587I26039I25986I15128-132
I---I---I----I------I----I-----I-----I---------------------------------
I067I123ILINKI939516I 591I23245I23167I15133-137
I---I---I----I------I----I-----I-----I---------------------------------
I068I124ILINKI939517I 623I21961I21861I15138-142
I---I---I----I------I----I-----I-----I---------------------------------
I069I125ILINKI939518I 651I21688I21604I15143-147
I---I---I----I------I----I-----I-----I---------------------------------
I070I126ILINKI939519I 675I24788I24640I15148-153
I---I---I----I------I----I-----I-----I---------------------------------
I071I127ILINKI939520I 689I17374I17315I15154-156
I---I---I----I------I----I-----I-----I---------------------------------
I072I128ILINKI939521I 711I23721I23628I15157-161
I---I---I----I------I----I-----I-----I---------------------------------
I073I129ILINKI939522I1923I21350I21284I15162-165
I---I---I----I------I----I-----I-----I---------------------------------
I074I130ILINKI939523I 736I19361I19316I15166-170
I---I---I----I------I----I-----I-----I---------------------------------
I075I131ILINKI939526I 765I25561I25452I15171-175
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939509-939523,939526
             WERE SENT TO RUTHERFORD ON 8.11.83

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I076I132ILINKI939532I  14I18397I18318I15176-180
I---I---I----I------I----I-----I-----I---------------------------------
I077I133ILINKI939533I 218I22112I22030I15181-184
I---I---I----I------I----I-----I-----I---------------------------------
I078I134ILINKI939534I 523I21752I21669I15185-190
I---I---I----I------I----I-----I-----I---------------------------------
I079I135ILINKI939535I 526I24866I24732I15191-194
I---I---I----I------I----I-----I-----I---------------------------------
I080I136ILINKI939536I 303I21436I21353I15195-199
I---I---I----I------I----I-----I-----I---------------------------------
I051I137ITAPEI939537I1222I 5600I 5530I15205
I---I---I----I------I----I-----I-----I---------------------------------
I052I138ITAPEI939538I1234I 6451I 6420I15206-207
I---I---I----I------I----I-----I-----I---------------------------------
I053I139ITAPEI939539I1247I 6561I 6541I15208-209
I---I---I----I------I----I-----I-----I---------------------------------
I054I140ITAPEI939540I1267I 3654I 3641I15210
I---I---I----I------I----I-----I-----I---------------------------------
I081I141ILINKI939542I 241I21248I21149I15200-204
I---I---I----I------I----I-----I-----I---------------------------------
I082I142ILINKI939543I1025I22564I22428I15211-217
I---I---I----I------I----I-----I-----I---------------------------------
I083I143ILINKI939544I1220I18354I18264I15218-221
I---I---I----I------I----I-----I-----I---------------------------------
I084I144ILINKI939545I 240I22991I22907I15222-226
I---I---I----I------I----I-----I-----I---------------------------------
I085I145ILINKI939546I 891I19982I19904I15227-233
I---I---I----I------I----I-----I-----I---------------------------------
I086I146ILINKI939548I 539I24869I24712I15236-241
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939532-939540,939542-939546,939548
             WERE SENT TO RUTHERFORD ON 14.11.83

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I087I147ILINKI939550I 822I15461I15396I15242,243,246-249
I---I---I----I------I----I-----I-----I---------------------------------
I055I148ITAPEI939551I 524I 4646I 4632I15235
I---I---I----I------I----I-----I-----I---------------------------------
I056I149ITAPEI939552I 533I 5415I 5370I15250
I---I---I----I------I----I-----I-----I---------------------------------
I057I150ITAPEI939553I 554I 6197I 6176I15251
I---I---I----I------I----I-----I-----I---------------------------------
I088I151ILINKI939554I1019I21274I21156I15252-255
I---I---I----I------I----I-----I-----I---------------------------------
I089I152ILINKI939555I1777I21608I21518I15256-261
I---I---I----I------I----I-----I-----I---------------------------------
I090I153ILINKI939556I 524I17586I17512I15262-264
I---I---I----I------I----I-----I-----I---------------------------------
I091I154ILINKI939557I1850I18329I18225I15265-269
I---I---I----I------I----I-----I-----I---------------------------------
I092I155ILINKI939558I 453I19389I19202I15270-276
I---I---I----I------I----I-----I-----I---------------------------------
I093I156ILINKI939559I 456I19841I19776I15277-281
I---I---I----I------I----I-----I-----I---------------------------------
I094I157ILINKI939560I 676I28292I28063I15282-287
I---I---I----I------I----I-----I-----I---------------------------------
I095I158ILINKI939561I 602I20139I20040I15288-292
I---I---I----I------I----I-----I-----I---------------------------------
I096I159ILINKI939562I1553I19017I18921I15293-296
I---I---I----I------I----I-----I-----I---------------------------------
I097I160ILINKI939563I1885I16246I16202I15297-299
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939550-939563
             WERE SENT TO RUTHERFORD ON 21.11.83

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I098I161ILINKI939564I 731I23257I23142I15300-304
I---I---I----I------I----I-----I-----I---------------------------------
I099I162ILINKI939565I1499I24757I24653I15305-319
I---I---I----I------I----I-----I-----I---------------------------------
I100I163ILINKI939571I1685I17666I17579I15320-323
I---I---I----I------I----I-----I-----I---------------------------------
I101I164ILINKI939572I 744I24459I24346I15324-328
I---I---I----I------I----I-----I-----I---------------------------------
I102I165ILINKI939583I1899I19576I19415I15329-332
I---I---I----I------I----I-----I-----I---------------------------------
I103I166ILINKI939585I1900I24014I23902I15333-342
I---I---I----I------I----I-----I-----I---------------------------------
I104I167ILINKI939587I1613I18869I18803I15343-348
I---I---I----I------I----I-----I-----I---------------------------------
I105I168ILINKI939588I  41I25640I25557I15349-356,360
I---I---I----I------I----I-----I-----I---------------------------------
I106I169ILINKI939589I  47I21408I21348I15361-367
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939622(RUN 14965-967),939564,939565,939571,939572,939583,
   939585,939587-939589  WERE SENT TO RUTHERFORD ON 24.11.83

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I107I170ILINKI939590I1131I26037I25958I15368-373
I---I---I----I------I----I-----I-----I---------------------------------
I058I171ITAPEI939591I1328I 1799I 1784I15357-359
I---I---I----I------I----I-----I-----I---------------------------------
I108I172ILINKI939592I1248I27757I27695I15374-377
I---I---I----I------I----I-----I-----I---------------------------------
I109I173ILINKI939593I1312I16714I16655I15378-381
I---I---I----I------I----I-----I-----I---------------------------------
I110I174ILINKI939594I1313I24410I24364I15382-386
I---I---I----I------I----I-----I-----I---------------------------------
I111I175ILINKI939595I1380I29518I29440I15387-391
I---I---I----I------I----I-----I-----I---------------------------------
I112I176ILINKI939596I   3I15964I15871I15392-395
I---I---I----I------I----I-----I-----I---------------------------------
I113I177ILINKI939597I 215I21521I21449I15396-399
I---I---I----I------I----I-----I-----I---------------------------------
I114I178ILINKI939598I 216I17395I17318I15400-403
I---I---I----I------I----I-----I-----I---------------------------------
I115I179ILINKI939599I 398I16456I16370I15404-407
I---I---I----I------I----I-----I-----I---------------------------------
I116I180ILINKI939600I 403I24108I23983I15408-412
I---I---I----I------I----I-----I-----I---------------------------------
I117I181ILINKI939601I1270I18375I18295I15413-415
I---I---I----I------I----I-----I-----I---------------------------------
I118I182ILINKI939602I1280I23838I23641I15416-419
I---I---I----I------I----I-----I-----I---------------------------------
I119I183ILINKI939603I 550I19484I19339I15420-424
I---I---I----I------I----I-----I-----I---------------------------------
I120I184ILINKI939604I 553I14433I14355I15425-428
I---I---I----I------I----I-----I-----I---------------------------------
I121I185ILINKI939605I1431I19715I19603I15429-435
I---I---I----I------I----I-----I-----I---------------------------------
I122I186ILINKI939606I1432I18700I18581I15436-439
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939590-939606 (AND 934002,934008,934015 FOR R. MARSHALL)
                         WERE SENT TO RUTHERFORD ON 30.11.83

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I123I187ILINKI939607I1433I22264I22098I15440-444
I---I---I----I------I----I-----I-----I---------------------------------
I124I188ILINKI939608I1434I15487I15398I15447-450
I---I---I----I------I----I-----I-----I---------------------------------
I125I189ILINKI939609I1886I18802I18616I15451-454
I---I---I----I------I----I-----I-----I---------------------------------
I126I190ILINKI939610I 482I19437I19148I15455-459
I---I---I----I------I----I-----I-----I---------------------------------
I127I191ILINKI939611I 505I18100I17996I15460-463
I---I---I----I------I----I-----I-----I---------------------------------
I128I192ILINKI939612I 520I24395I24206I15464-468
I---I---I----I------I----I-----I-----I---------------------------------
I129I193ILINKI939613I 830I18621I18440I15469-471
I---I---I----I------I----I-----I-----I---------------------------------
I130I194ILINKI939614I1356I16729I16609I15472-474
I---I---I----I------I----I-----I-----I---------------------------------
I131I195ILINKI939615I  29I16783I16683I15475-477
I---I---I----I------I----I-----I-----I---------------------------------
I132I196ILINKI939616I 900I22193I22064I15478-482
I---I---I----I------I----I-----I-----I---------------------------------
I133I197ILINKI939617I 438I13191I13120I15483-485
I---I---I----I------I----I-----I-----I---------------------------------
I134I198ILINKI939618I 245I19171I19060I15486-490
I---I---I----I------I----I-----I-----I---------------------------------
I135I199ILINKI939619I 246I23660I23561I15491-494
I---I---I----I------I----I-----I-----I---------------------------------
I136I200ILINKI939620I 753I23921I23784I15495-501
I---I---I----I------I----I-----I-----I---------------------------------
I137I201ILINKI939621I1628I19115I19039I15502-505
I---I---I----I------I----I-----I-----I---------------------------------
I138I202ILINKI939425I1994I21579I21452I15506-511
I---I---I----I------I----I-----I-----I---------------------------------
I139I203ILINKI939426I 695I18690I18594I15512-515
I---I---I----I------I----I-----I-----I---------------------------------
I140I204ILINKI939427I1283I22853I22725I15516-520
I---I---I----I------I----I-----I-----I---------------------------------
I141I205ILINKI939428I1349I16885I16814I15521-523
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939607-939621, 939425-939428
                         WERE SENT TO RUTHERFORD ON 6.12.83

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I142I206ILINKI939429I 193I23805I23651I15524-528
I---I---I----I------I----I-----I-----I---------------------------------
I143I207ILINKI939430I  43I21438I21337I15529-546
I---I---I----I------I----I-----I-----I---------------------------------
I144I208ILINKI939431I  53I18654I18496I15547-552
I---I---I----I------I----I-----I-----I---------------------------------
I145I209ILINKI939432I1956I20237I20152I15553-557
I---I---I----I------I----I-----I-----I---------------------------------
I146I210ILINKI939433I1957I17199I16989I15558-570
I---I---I----I------I----I-----I-----I---------------------------------
I147I211ILINKI939434I 575I17993I17800I15572-576
I---I---I----I------I----I-----I-----I---------------------------------
I148I212ILINKI939435I 346I18700I18439I15577-580
I---I---I----I------I----I-----I-----I---------------------------------
I149I213ILINKI939436I 614I21704I21589I15581-585
I---I---I----I------I----I-----I-----I---------------------------------
I150I214ILINKI939437I 619I18126I17968I15586-588
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939429-939437,(941530 FOR HUGH MCCANN)
                         WERE SENT TO RUTHERFORD ON 13.12.83

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I151I215ILINKI939438I1285I24664I24557I15589-594
I---I---I----I------I----I-----I-----I---------------------------------
I152I216ILINKI939439I1544I20086I20007I15595-598
I---I---I----I------I----I-----I-----I---------------------------------
I153I217ILINKI939440I1995I19444I19275I15599-606
I---I---I----I------I----I-----I-----I---------------------------------
I154I218ILINKI939441I  42I17635I17556I15607-613
I---I---I----I------I----I-----I-----I---------------------------------
I155I219ILINKI939442I 201I20764I20641I15614-619
I---I---I----I------I----I-----I-----I---------------------------------
I059I220ITAPEI939443I 748I 5433I 5416I15562
I---I---I----I------I----I-----I-----I---------------------------------
I060I221ITAPEI939444I 756I 5543I 5518I15565
I---I---I----I------I----I-----I-----I---------------------------------
I061I222ITAPEI939445I 774I 2987I 2958I15566
I---I---I----I------I----I-----I-----I---------------------------------
I062I223ITAPEI939446I 780I 1365I 1350I15567
I---I---I----I------I----I-----I-----I---------------------------------
I156I224ILINKI939447I 235I24215I24082I15620-625
I---I---I----I------I----I-----I-----I---------------------------------
I157I225ILINKI939448I 268I18635I18494I15626-631
I---I---I----I------I----I-----I-----I---------------------------------
I158I226ILINKI939449I 670I24577I24429I15632-636
I---I---I----I------I----I-----I-----I---------------------------------
I159I227ILINKI939450I 587I15297I15202I15637-639
I---I---I----I------I----I-----I-----I---------------------------------
I160I228ILINKI939451I 590I19593I19369I15640-643
I---I---I----I------I----I-----I-----I---------------------------------
I161I229ILINKI939452I1762I19475I19395I15644-647
I---I---I----I------I----I-----I-----I---------------------------------
I162I230ILINKI939453I1763I18972I18857I15648-654
I---I---I----I------I----I-----I-----I---------------------------------
I163I231ILINKI939454I1820I19468I19289I15655-658
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939438-939454,(941557 FROM HUGH MCCANN)
                         WERE SENT TO RUTHERFORD ON 16.12.83

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I164I232ILINKI939455I 669I20436I20314I15659-662
I---I---I----I------I----I-----I-----I---------------------------------
I165I233ILINKI939456I1591I15768I15674I15663-556
I---I---I----I------I----I-----I-----I---------------------------------
I166I234ILINKI939457I 591I22247I22131I15666-670
I---I---I----I------I----I-----I-----I---------------------------------
I167I235ILINKI939458I 691I20626I20528I15671-678
I---I---I----I------I----I-----I-----I---------------------------------
I168I236ILINKI939459I1580I21427I21333I15679-686
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939455-939459 WERE SENT TO RAL ON 23.12.83

   THAT WERE THE LAST TAPES IN 1983. THE RUNS 15687-689 WILL ARRIVE
   YOU TOGETHER WITH THE NEXT TAPES.

   I SEND RUN 15663-665 ON TAPE 939542 ON 26.1.1984 AGAIN.

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I169I237ILINKI939460I 292I 6864I 6826I15687-689
I---I---I----I------I----I-----I-----I---------------------------------
       **********  1984  ************
I---I---I----I------I----I-----I-----I---------------------------------
I170I238ILINKI939461I 787I18938I18841I15693-706
I---I---I----I------I----I-----I-----I---------------------------------
I171I239ILINKI939462I 868I20957I20618I15707-711
I---I---I----I------I----I-----I-----I---------------------------------
I172I240ILINKI939463I 888I16971I16859I15712-715
I---I---I----I------I----I-----I-----I---------------------------------
I173I241ILINKI939464I 918I23056I22774I15716-719
I---I---I----I------I----I-----I-----I---------------------------------
I174I242ILINKI939465I 922I16064I15934I15720-723
I---I---I----I------I----I-----I-----I---------------------------------
I175I243ILINKI939466I 364I19284I19078I15724-727
I---I---I----I------I----I-----I-----I---------------------------------
I176I244ILINKI939467I 855I22375I22304I15728-733
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939460-939467 WERE SENT TO RAL ON 1.2.84

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I177I245ILINKI939468I  81I17686I17576I15734-738
I---I---I----I------I----I-----I-----I---------------------------------
I178I246ILINKI939469I1685I23472I23346I15739-743
I---I---I----I------I----I-----I-----I---------------------------------
I179I247ILINKI939470I 766I24402I24252I15744-749
I---I---I----I------I----I-----I-----I---------------------------------
I180I248ILINKI939471I    I     I     I
I---I---I----I------I----I-----I-----I---------------------------------
I181I249ILINKI939472I 185I24788I24727I15756-770
I---I---I----I------I----I-----I-----I---------------------------------
I182I0011LINKI939473I 476I21298I21209I15771-776
I---I---I----I------I----I-----I-----I---------------------------------
I183I002ILINKI939474I 485I19183I19065I15777-784
I---I---I----I------I----I-----I-----I---------------------------------
I184I003ILINKI939475I 533I23600I23512I15785-789
I---I---I----I------I----I-----I-----I---------------------------------
I185I004ILINKI939476I 558I26697I26606I15790-796
I---I---I----I------I----I-----I-----I---------------------------------
I186I005ILINKI939477I 576I25599I25506I15797-804
I---I---I----I------I----I-----I-----I---------------------------------
I187I006ILINKI939478I 604I28657I28547I15805-812
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939468-939470, 939472-939478 WERE SENT TO RAL ON 7.2.84
   TAPE 939471 (RUN 15750-755) HAS A PROBLEM!

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I188I007ILINKI939479I 709I23694I23617I15813-817
I---I---I----I------I----I-----I-----I---------------------------------
I189I008ILINKI939480I 719I25992I25922I15818-823
I---I---I----I------I----I-----I-----I---------------------------------
I190I009ILINKI939481I1071I31118I31009I15824-830
I---I---I----I------I----I-----I-----I---------------------------------
I191I010ILINKI939482I 486I24935I24869I15831-838
I---I---I----I------I----I-----I-----I---------------------------------
I192I011ILINKI939483I 496I26926I26768I15839-845
I---I---I----I------I----I-----I-----I---------------------------------
I193I012ILINKI939484I 506I21836I21737I15846-851
I---I---I----I------I----I-----I-----I---------------------------------
I194I013ILINKI939485I 784I23588I23542I15852-855
I---I---I----I------I----I-----I-----I---------------------------------
I195I014ILINKI939486I 694I24577I24491I15856-860
I---I---I----I------I----I-----I-----I---------------------------------
I196I015ILINKI939487I 696I23848I23659I15861-865
I---I---I----I------I----I-----I-----I---------------------------------
I197I016ILINKI939488I1409I20938I20841I15866-872
I---I---I----I------I----I-----I-----I---------------------------------
I198I017ILINKI939489I1413I24694I24595I15873-878
I---I---I----I------I----I-----I-----I---------------------------------
I199I018ILINKI939490I1297I23323I23236I15879-883
I---I---I----I------I----I-----I-----I---------------------------------
I200I019ILINKI939491I1634I22543I22422I15884-888
I---I---I----I------I----I-----I-----I---------------------------------
I201I020ILINKI939492I 390I22164I22043I15889-893
I---I---I----I------I----I-----I-----I---------------------------------
I202I021ILINKI939493I 701I18501I18411I15894-898,904
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939479-939493 WERE SENT TO RAL ON 14.2.1984

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I203I022ILINKI939494I1177I24635I24527I15905-909
I---I---I----I------I----I-----I-----I---------------------------------
I204I023ILINKI939495I 258I16873I16761I15910-912,918
I---I---I----I------I----I-----I-----I---------------------------------
I205I024ILINKI939497I 284I23620I23535I15919-927
I---I---I----I------I----I-----I-----I---------------------------------
I206I025ILINKI939498I 594I24773I24692I15928-934
I---I---I----I------I----I-----I-----I---------------------------------
I207I026ILINKI939499I 611I20462I20387I15935-941
I---I---I----I------I----I-----I-----I---------------------------------
I208I027ILINKI939500I 626I24121I24047I15942-948
I---I---I----I------I----I-----I-----I---------------------------------
I209I028ILINKI939501I 204I24294I24203I15949-953
I---I---I----I------I----I-----I-----I---------------------------------
I063I029ITAPEI939502I 266I22670I22565I15899-904
I---I---I----I------I----I-----I-----I---------------------------------
I210I030ILINKI939503I 383I20287I20729I15954,957-960
I---I---I----I------I----I-----I-----I---------------------------------
I064I031ITAPEI939504I 404I 5548I 5528I15955-956
I---I---I----I------I----I-----I-----I---------------------------------
I211I032ILINKI939505I 418I27050I26951I15961-966
I---I---I----I------I----I-----I-----I---------------------------------
I212I033ILINKI939506I 419I21649I21570I15967-971
I---I---I----I------I----I-----I-----I---------------------------------
I213I034ILINKI939507I1259I22741I22591I15972-977
I---I---I----I------I----I-----I-----I---------------------------------
I214I035ILINKI939508I1262I22709I22614I15978-982
I---I---I----I------I----I-----I-----I---------------------------------
I215I036ILINKI939509I1276I22336I22207I15983-988
I---I---I----I------I----I-----I-----I---------------------------------
I216I037ILINKI939510I1282I14892I14762I15989-991
I---I---I----I------I----I-----I-----I---------------------------------
I217I038ILINKI939512I1648I23996I23857I15992-997
I---I---I----I------I----I-----I-----I---------------------------------
I218I039ILINKI939513I1248I17246I17156I15998-002
I---I---I----I------I----I-----I-----I---------------------------------
I219I040ILINKI939514I1229I25396I25271I16003-012
I---I---I----I------I----I-----I-----I---------------------------------
I220I041ILINKI939515I1240I21587I21500I16013-018
I---I---I----I------I----I-----I-----I---------------------------------
I221I042ILINKI939516I1260I21864I21766I16019-023
I---I---I----I------I----I-----I-----I---------------------------------
I065I043ITAPEI939517I1841I 5808I 5797I16007-008
I---I---I----I------I----I-----I-----I---------------------------------
I066I044ITAPEI939518I1888I 7592I 7572I16009-010
I---I---I----I------I----I-----I-----I---------------------------------
I067I045ITAPEI939519I1899I  227I  226I16011
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939494-939495,939497-939510,939512-939519,939471
              WERE SENT TO RAL ON 21.2.84

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I222I046ILINKI939520I1789I16208I16119I16024-026
I---I---I----I------I----I-----I-----I---------------------------------
I223I047ILINKI939521I1997I25437I25343I16027-034
I---I---I----I------I----I-----I-----I---------------------------------
I224I048ILINKI939522I   3I20418I20302I16035-039
I---I---I----I------I----I-----I-----I---------------------------------
I225I049ILINKI939523I1622I17689I17600I16040-044
I---I---I----I------I----I-----I-----I---------------------------------
I226I050ILINKI939526I 749I23166I23014I16045-050
I---I---I----I------I----I-----I-----I---------------------------------
I227I051ILINKI939541I1114I15406I15327I16051-054
I---I---I----I------I----I-----I-----I---------------------------------
I228I052ILINKI939549I1242I20532I20421I16055-060
I---I---I----I------I----I-----I-----I---------------------------------
I229I053ILINKI939566I 130I24946I24850I16061-065
I---I---I----I------I----I-----I-----I---------------------------------
I230I054ILINKI939567I 140I16630I16541I16066-069
I---I---I----I------I----I-----I-----I---------------------------------
I231I055ILINKI939568I 143I22748I22674I16070-079
I---I---I----I------I----I-----I-----I---------------------------------
I232I056ILINKI939569I1246I20765I20684I16080-084
I---I---I----I------I----I-----I-----I---------------------------------
I233I057ILINKI939570I1574I28181I28097I16085-092
I---I---I----I------I----I-----I-----I---------------------------------
I234I058ILINKI939575I1304I26550I26452I16093-101
I---I---I----I------I----I-----I-----I---------------------------------
I235I059ILINKI939576I 643I24113I24034I16102-107
I---I---I----I------I----I-----I-----I---------------------------------
I236I060ILINKI939577I 731I23268I23190I16108-116
I---I---I----I------I----I-----I-----I---------------------------------
I237I061ILINKI939578I1569I25855I25779I16117-122
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939520-939523,939526,939541,939549,939566-939570,
             939575-939578  WERE SENT TO RAL ON 21.2.84

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I238I062ILINKI939579I1218I20879I20801I16123-128
I---I---I----I------I----I-----I-----I---------------------------------
I239I063ILINKI939580I1705I24990I24908I16129-135
I---I---I----I------I----I-----I-----I---------------------------------
I240I064ILINKI939581I1597I24541I24464I16136-142
I---I---I----I------I----I-----I-----I---------------------------------
I241I065ILINKI939582I1354I20583I20512I16143-148
I---I---I----I------I----I-----I-----I---------------------------------
I242I066ILINKI939584I 478I24901I24816I16149-158
I---I---I----I------I----I-----I-----I---------------------------------
I243I067ILINKI939586I 484I30313I30215I16159-166
I---I---I----I------I----I-----I-----I---------------------------------
I244I068ILINKI939532I 486I21481I21419I16167-174
I---I---I----I------I----I-----I-----I---------------------------------
I245I069ILINKI939533I  21I23421I23332I16175-180
I---I---I----I------I----I-----I-----I---------------------------------
I246I070ILINKI939534I 864I22816I22741I16181-185
I---I---I----I------I----I-----I-----I---------------------------------
I247I071ILINKI939535I1550I24464I24355I16186-193
I---I---I----I------I----I-----I-----I---------------------------------
I248I072ILINKI939536I1022I21294I21206I16194-199
I---I---I----I------I----I-----I-----I---------------------------------
I249I073ILINKI939537I1813I28288I28212I16200-206
I---I---I----I------I----I-----I-----I---------------------------------
I001I074ILINKI939538I 917I23354I23273I16207-213
I---I---I----I------I----I-----I-----I---------------------------------
I002I075ILINKI939539I 112I22805I22732I16214-227
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 948228(RUN 15739-743),948229(RUN 15873-878),939579-
      939582,939584,939586,939532-939539 WERE SENT TO RALON 7.3.84


I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I003I076ILINKI939540I1125I27509I27414I16228-235
I---I---I----I------I----I-----I-----I---------------------------------
I068I077ITAPEI939544I1051I 5222I 5203I16238
I---I---I----I------I----I-----I-----I---------------------------------
I069I078ITAPEI939545I1063I 4907I 4895I16239
I---I---I----I------I----I-----I-----I---------------------------------
I070I079ITAPEI939546I1092I 1274I 1267I16240-241
I---I---I----I------I----I-----I-----I---------------------------------
I004I080ILINKI939548I 616I23512I23428I16236,237,242-245
I---I---I----I------I----I-----I-----I---------------------------------
I005I081ILINKI939550I 358I24433I24343I16246,251-258
I---I---I----I------I----I-----I-----I---------------------------------
I006I082ILINKI939551I 370I25127I25040I16259-267
I---I---I----I------I----I-----I-----I---------------------------------
I007I083ILINKI939552I 629I28130I28038I16268-277
I---I---I----I------I----I-----I-----I---------------------------------
I008I084ILINKI948230I1398I26119I26041I16278-284
I---I---I----I------I----I-----I-----I---------------------------------
I009I085ILINKI948231I1959I25438I25356I16285-291
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 939540, 939544-939546, 939548, 939550-939552,
                 948230-948231 WERE SENT TO RAL ON 12.3.84


I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I010I086ILINKI948232I  79I22405I22316I16292-296
I---I---I----I------I----I-----I-----I---------------------------------
I011I087ILINKI948233I 681I25278I25176I16297-298,300-308
I---I---I----I------I----I-----I-----I---------------------------------
I012I088ILINKI948234I1900I24105I23995I16309-317
I---I---I----I------I----I-----I-----I---------------------------------
I013I089ILINKI948235I 268I  ?  I  ?  I16318-324
I---I---I----I------I----I-----I-----I---------------------------------
I014I090ILINKI948236I 906I23148I23033I16325-332
I---I---I----I------I----I-----I-----I---------------------------------
I015I091ILINKI948237I 494I25568I25464I16333-340,343-345
I---I---I----I------I----I-----I-----I---------------------------------
I016I092ILINKI948238I1472I25988I25867I16346-350,353-356
I---I---I----I------I----I-----I-----I---------------------------------
I071I093ITAPEI948239I1744I 2957I 2945I16341-342
I---I---I----I------I----I-----I-----I---------------------------------
I017I094ILINKI948240I 513I25612I25480I16357-365
I---I---I----I------I----I-----I-----I---------------------------------
I018I095ILINKI948241I1170I26101I26008I16366-373,375-378
I---I---I----I------I----I-----I-----I---------------------------------
I019I096ILINKI948242I 540I22157I22042I16379-388
I---I---I----I------I----I-----I-----I---------------------------------
I020I097ILINKI948243I 528I24860I24743I16389-398
I---I---I----I------I----I-----I-----I---------------------------------
I021I098ILINKI948244I 598I24755I24635I16399-410
I---I---I----I------I----I-----I-----I---------------------------------
I022I099ILINKI948245I 308I22955I22798I16411-422
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 948232-948245 WERE SENT TO RAL ON 20.3.84


I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I023I100ILINKI948246I 114I23716I23576I16423-432
I---I---I----I------I----I-----I-----I---------------------------------
I024I101ILINKI948247I 102I25355I25230I16433-444
I---I---I----I------I----I-----I-----I---------------------------------
I025I102ILINKI948248I 117I24631I24488I16445-457
I---I---I----I------I----I-----I-----I---------------------------------
I026I103ILINKI948249I 586I22431I22262I16458-472
I---I---I----I------I----I-----I-----I---------------------------------
I027I104ILINKI948250I1648I23460I23273I16473-481
I---I---I----I------I----I-----I-----I---------------------------------
I028I105ILINKI948251I1983I19866I19718I16482-487
I---I---I----I------I----I-----I-----I---------------------------------
I029I106ILINKI948252I1648I20764I20625I16488-498
I---I---I----I------I----I-----I-----I---------------------------------
I030I107ILINKI948253I1989I20518I20356I16499-507
I---I---I----I------I----I-----I-----I---------------------------------
I031I108ILINKI948254I1999I18026I17862I16508-516
I---I---I----I------I----I-----I-----I---------------------------------
I032I109ILINKI948255I   1I18675I18521I16517-522
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 948246-948255 WERE SENT TO RAL ON 30.3.84


I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I033I110ILINKI948256I   9I19487I19308I16523-530
I---I---I----I------I----I-----I-----I---------------------------------
I034I111ILINKI948257I 159I21818I21579I16531-538
I---I---I----I------I----I-----I-----I---------------------------------
I035I112ILINKI948258I1822I20989I20805I16539-547
I---I---I----I------I----I-----I-----I---------------------------------
I036I113ILINKI948259I1829I21853I21716I16548-553
I---I---I----I------I----I-----I-----I---------------------------------
I037I114ILINKI948260I1348I17831I17696I16554-558
I---I---I----I------I----I-----I-----I---------------------------------
I038I115ILINKI948261I 255I23753I22557I16559-567
I---I---I----I------I----I-----I-----I---------------------------------
I039I116ILINKI948262I 257I18848I18676I16568-574
I---I---I----I------I----I-----I-----I---------------------------------
I040I117ILINKI948263I 133I19512I19303I16575-581
I---I---I----I------I----I-----I-----I---------------------------------
I041I118ILINKI948264I1366I23716I23531I16582-592
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 948256-948264 WERE SENT TO RAL ON 3.4.84


I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I042I119ILINKI948265I 811I20767I20608I16593-597,600-604
I---I---I----I------I----I-----I-----I---------------------------------
I043I120ILINKI948266I 990I19762I19596I16605-609
I---I---I----I------I----I-----I-----I---------------------------------
I044I121ILINKI948267I 661I20281I20101I16610-620
I---I---I----I------I----I-----I-----I---------------------------------
I045I122ILINKI948268I 665I19486I19263I16621-628
I---I---I----I------I----I-----I-----I---------------------------------
I046I123ILINKI948269I1633I21405I21256I16629-633
I---I---I----I------I----I-----I-----I---------------------------------
I047I124ILINKI948270I  86I20275I20145I16634-638
I---I---I----I------I----I-----I-----I---------------------------------
I048I125ILINKI948271I 902I20199I20035I16639-643
I---I---I----I------I----I-----I-----I---------------------------------
I049I126ILINKI948272I 939I18794I18628I16644-649
I---I---I----I------I----I-----I-----I---------------------------------
I050I127ILINKI948273I 387I19806I19682I16650-653
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 948265-948273 WERE SENT TO RAL ON 6.4.84


I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I051I128ILINKI948274I1480I20134I20001I16654-659
I---I---I----I------I----I-----I-----I---------------------------------
I052I129ILINKI948275I1913I22803I22670I16660-667
I---I---I----I------I----I-----I-----I---------------------------------
I053I130ILINKI948276I 254I22891I22738I16668-677
I---I---I----I------I----I-----I-----I---------------------------------
I054I131ILINKI948277I 422I19156I19024I16678-690
I---I---I----I------I----I-----I-----I---------------------------------
I055I132ILINKI948278I 424I20767I20645I16684-690
I---I---I----I------I----I-----I-----I---------------------------------
I056I133ILINKI948279I1551I23628I23456I16691-699
I---I---I----I------I----I-----I-----I---------------------------------
I057I134ILINKI948280I1625I19322I19199I16700-708
I---I---I----I------I----I-----I-----I---------------------------------
I058I135ILINKI948281I1812I21785I21611I16709 716
I---I---I----I------I----I-----I-----I---------------------------------
I059I136ILINKI948282I 913I23521I23358I16717-725
I---I---I----I------I----I-----I-----I---------------------------------
I060I137ILINKI948283I1847I21075I20962I16726-733
I---I---I----I------I----I-----I-----I---------------------------------

   THE TAPES 948274-948283 AND 948284 (RUN 16318-324)
                         WERE SENT TO RAL ON 12.4.84

I===I===I====I======I====I=====I=====I=================================
IYENILHOI ON I RUTH IJOB INORD IREFM I    RUNS             REMARKS
IGENIGENILINEI TAPE I #  IEVNT IEVNT I
I---I---I----I------I----I-----I-----I---------------------------------
I061I138ILINKI      I1046I25583I25536I16734-38,44-49
I---I---I----I------I----I-----I-----I---------------------------------
I062I139ILINKI948285I1048I18630I18630I16750-51,
I---I---I----I------I----I-----I-----I---------------------------------

   *************************************************
   ******                                    *******
   ******      Next part of runlist in :     *******
   ******                                    *******
   ******      JADEOL.REFORM.S(RUNLIST3)     *******
   ******                                    *******
   *************************************************

C   18/10/81 110181306  MEMBER NAME  RUTHLIBO (JADESR)      TEXT


 PLEASE UNDER NO CIRCUMSTANCES DELETE THIS LOAD MODULE
 IT IS USED FOR RUTHERFORD PROGRAM TRANSFER

                                           GFP
    01/04/82            MEMBER NAME  RUTHLINK (JADESR)      TEXT


 PLEASE UNDER NO CIRCUMSTANCES DELETE THIS LOAD MODULE
 IT IS USED FOR RUTHERFORD PROGRAM TRANSFER

                                            GFP
    10/05/83            MEMBER NAME  RUTMESSG (JADESR)   M  TEXT
========================================================================
========================================================================

      MESSAGES CONCERNING DATA REDUCTION AT RUTHERFORD LAB.
      =====================================================

ALL OLD MESSAGES ARE IN THE MEMBER 'JADEPR.JADESR(RUTMESS1)'

==========================================
TO SEND A MESSAGE TO SOMEONE AT RUTHERFORD
==========================================

1) WRITE YOUR MESSAGE INTO A NEWLIB MEMBER
2) TYPE :   (RUTHMAIL)
tapes in all. gina stuart

========================================================================
========================================================================
MESSAGE FROM BARRY WHITTAKER/GINA STUART 00.00AM  13.4.84

 THE FOLLOWING 8 TAPES WILL BE SENT FROM RAL TO DESY ON TUESDAY 17.4.84

      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      939499   713          JADEPR.REDUC1.G0713
      939500   714          JADEPR.REDUC1.G0714
      939501   715          JADEPR.REDUC1.G0715
      939503   716          JADEPR.REDUC1.G0716
      939504   717          JADEPR.REDUC1.G0717
      939505   718          JADEPR.REDUC1.G0718
      939506   719          JADEPR.REDUC1.G0719
      939507   720          JADEPR.REDUC1.G0720


************************************************************************
************************************************************************
========================================================================
========================================================================
********
******** MESSAGE FROM Barrie Whittaker/Gina Stuart (14.45  18/4/84)
********

 The following 8 merged tapes will be sent from RAL after Easter.

      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      939508   721          JADEPR.REDUC1.G0721
      939509   722          JADEPR.REDUC1.G0722
      939510   723          JADEPR.REDUC1.G0723
      939512   724          JADEPR.REDUC1.G0724
      939513   725          JADEPR.REDUC1.G0725
      939514   726          JADEPR.REDUC1.G0726
      939515   727          JADEPR.REDUC1.G0727
      939516   728          JADEPR.REDUC1.G0728

*****************************************************************
========================================================================
========================================================================
********
******** MESSAGE FROM Barrie Whittaker/Gina Stuart (15.15  1/5/84)
********

The  following  6  merged  tapes will be sent from RAL on Tuesday
8/5/84.

      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      939493   710          JADEPR.REDUC1.G0710
      939517   729          JADEPR.REDUC1.G0729
      939518   730          JADEPR.REDUC1.G0730
      939519   731          JADEPR.REDUC1.G0731
      939489   732          JADEPR.REDUC1.G0732
      939502   733          JADEPR.REDUC1.G0733

N.B.   We  have processed MERGE 710 - runs 16318-16324 here since
the tape finally arrived with the last batch of tapes.

*****************************************************************
========================================================================
========================================================================
********
******** MESSAGE FROM Barrie Whittaker/Gina Stuart (15.15  4/5/84)
********

 The following 84 free tapes will be sent from RAL on Tuesday 15th May 8
 939469, 939520-939523, 939526, 939532-939541, 939544-939546,
 939548-939552, 939566-939570, 939575-939582, 939584, 939586, 948228,
948230-948273
*****************************************************************
========================================================================
========================================================================
********
******** MESSAGE FROM JBW (16.00 11/5/84)
********

According  to  the  log  book runs 16734-38 are good.  These runs
have not been sent to RAL for reduction.  Can you check what  has
happened to them?

When  the  last  batch of 84 tapes arrive at DESY you should have
289 'TRANTAPE's as follows:

        939423-939622,
        948228-948273,
        948285-948327.

Some  of these have not appeared at RAL for over a year.  Perhaps
you would check that they are all there.

             Thanks,

                        Barrie Whittaker/Gina Stuart

*****************************************************************
========================================================================
========================================================================
********
******** MESSAGE FROM Barrie Whittaker/Gina Stuart (16.15  2/7/84)
********

 The following 11 merged tapes will be sent from RAL on 3/7/84.

      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      948274   735          JADEPR.REDUC1.G0735
      948275   736          JADEPR.REDUC1.G0736
      948276   737          JADEPR.REDUC1.G0737
      948277   738          JADEPR.REDUC1.G0738
      948278   739          JADEPR.REDUC1.G0739
      948279   740          JADEPR.REDUC1.G0740
      948280   741          JADEPR.REDUC1.G0741
      948281   742          JADEPR.REDUC1.G0742
      948282   743          JADEPR.REDUC1.G0743
      948283   744          JADEPR.REDUC1.G0744
      948284   745          JADEPR.REDUC1.G0745

========================================================================
========================================================================
********
******** MESSAGE FROM Barrie Whittaker/Gina Stuart (10.30 10/7/84)
********

 The following 8 merged tapes will be sent from RAL on 10/7/84.

      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      948286   746          JADEPR.REDUC1.G0746
      948287   747          JADEPR.REDUC1.G0747
      948288   748          JADEPR.REDUC1.G0748
      948289   749          JADEPR.REDUC1.G0749
      948290   750          JADEPR.REDUC1.G0750
      948291   751          JADEPR.REDUC1.G0751
      948292   752          JADEPR.REDUC1.G0752
      948293   753          JADEPR.REDUC1.G0753


*****************************************************************
*****************************************************************
========================================================================
========================================================================
********
******** MESSAGE FROM Barrie Whittaker/Gina Stuart (12.00 27/7/84)
********

The  following  28  merged tapes will be sent from RAL on Tuesday
31/7/84.

      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      948294   754          JADEPR.REDUC1.G0754
      948295   755          JADEPR.REDUC1.G0755
      948296   757          JADEPR.REDUC1.G0757
      948297   758          JADEPR.REDUC1.G0758
      948298   759          JADEPR.REDUC1.G0759
      948299   760          JADEPR.REDUC1.G0760
      948300   761          JADEPR.REDUC1.G0761
      948301   762          JADEPR.REDUC1.G0762
      948302   763          JADEPR.REDUC1.G0763
      948303   764          JADEPR.REDUC1.G0764
      948304   765          JADEPR.REDUC1.G0765
      948305   766          JADEPR.REDUC1.G0766
      948306   767          JADEPR.REDUC1.G0767
      948307   768          JADEPR.REDUC1.G0768
      948308   769          JADEPR.REDUC1.G0769
      948309   770          JADEPR.REDUC1.G0770
      948310   771          JADEPR.REDUC1.G0771
      948311   772          JADEPR.REDUC1.G0772
      948312   773          JADEPR.REDUC1.G0773
      948313   774          JADEPR.REDUC1.G0774
      948314   775          JADEPR.REDUC1.G0775
      939425   776          JADEPR.REDUC1.G0776
      939426   777          JADEPR.REDUC1.G0777
      939427   778          JADEPR.REDUC1.G0778
      939428   779          JADEPR.REDUC1.G0779
      939429   780          JADEPR.REDUC1.G0780
      939430   781          JADEPR.REDUC1.G0781
      939431   782          JADEPR.REDUC1.G0782

Please note:

Tape  948316  supposedly  sent from DESY on 4/7/84 never arrived.
Also tape 948317 supposedly  containing  runs  17013-18  actually
only contained runs 17013-15.

There has been no note in the RUNLIST3 file about the despatch of
tapes since 4/7/84 although 2 batches have arrived at RAL.

*****************************************************************
========================================================================
********
******** MESSAGE FROM a. petersen (10.30 16/8/84)
********

 The 28 merged tapes arrived  on 14/8/84.
 they are copied to desy tapes.
 what is about tape jadepr.reduc1.g0756 ????

*****************************************************************
*****************************************************************
========================================================================
========================================================================
********
******** MESSAGE FROM JBW (12.00 16/8/84)
********

The  following  10  merged tapes will be sent from RAL on Tuesday
21/8/84.

      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      939432   795          JADEPR.REDUC1.G0795
      939433   796          JADEPR.REDUC1.G0796
      939434   797          JADEPR.REDUC1.G0797
      939435   798          JADEPR.REDUC1.G0798
      939436   799          JADEPR.REDUC1.G0799
      939437   800          JADEPR.REDUC1.G0800
      939438   801          JADEPR.REDUC1.G0801
      939439   802          JADEPR.REDUC1.G0802
      939440   803          JADEPR.REDUC1.G0803
      939441   804          JADEPR.REDUC1.G0804

Please note: We left MERGE 756 out so it can be used to take runs
17006-12 and 17016-18 which haven't arrived at RAL (see  RUTMESSG
message  27/7/84).   To  avoid  further  confusion I suggest this
should be processed at DESY.

The  59 tapes corresponding to MERGEs 735 - 794 (except 756) will
be reprocessed as soon as all the problems can be sorted out.  In
the meantime we are returning to normal processing.

                        Barrie Whittaker/Gina Stuart

*****************************************************************
========================================================================
========================================================================
********
******** MESSAGE FROM JBW (15.25 24/8/84)
********

The following 30 merged tapes will be sent from RAL on Wednesday,
29/8/84.

      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      939466   805          JADEPR.REDUC1.G0805
      939467   806          JADEPR.REDUC1.G0806
      939468   807          JADEPR.REDUC1.G0807
      939470   808          JADEPR.REDUC1.G0808
      939472   809          JADEPR.REDUC1.G0809
      939473   810          JADEPR.REDUC1.G0810
      939474   811          JADEPR.REDUC1.G0811
      939475   813          JADEPR.REDUC1.G0813
      939476   815          JADEPR.REDUC1.G0815
      939478   816          JADEPR.REDUC1.G0816
      939479   817          JADEPR.REDUC1.G0817
      939480   818          JADEPR.REDUC1.G0818
      939481   819          JADEPR.REDUC1.G0819
      939482   820          JADEPR.REDUC1.G0820
      939483   821          JADEPR.REDUC1.G0821
      939484   822          JADEPR.REDUC1.G0822
      939485   823          JADEPR.REDUC1.G0823
      939486   824          JADEPR.REDUC1.G0824
      939487   825          JADEPR.REDUC1.G0825
      939488   826          JADEPR.REDUC1.G0826
      939489   827          JADEPR.REDUC1.G0827
      939495   828          JADEPR.REDUC1.G0828
      939497   829          JADEPR.REDUC1.G0829
      939498   830          JADEPR.REDUC1.G0830
      939499   831          JADEPR.REDUC1.G0831
      939500   832          JADEPR.REDUC1.G0832
      939501   833          JADEPR.REDUC1.G0833
      939502   834          JADEPR.REDUC1.G0834
      939503   835          JADEPR.REDUC1.G0835
      939506   837          JADEPR.REDUC1.G0837

Please note the following MERGE sequences are missing:

     812 - this is because runs 17436 - 17439 are missing. There
           is no mention of them in the RUNLIST3 file although
           according to the log book they should exist. If they
           can be found they should be inserted with this MERGE
           number.

     814 - this is because tape 939477 arrived at RAL with no
           data on it. It still contained the merged data for
           MERGE 692 from when it was last sent to DESY.
           Runs 17444 - 17447 should be reduced and inserted
           at this point.

     836 - this is because the final check gave some read errors.
           This data will be rewritten and sent next time.

     838 - this is because tape 939505 arrived at RAL with no
           data on it. It still contained the merged data for
           MERGE 718 from when it was last sent to DESY.
           Runs 17599 - 17605 should be reduced and inserted
           at this point.

     The first successful 'REPAIR' job was run last night. We hope
     to reprocess MERGES 735 - 794 (except 756) by next time.

                        Barrie Whittaker/Gina Stuart

*****************************************************************
========================================================================
========================================================================
********
******** MESSAGE FROM JBW (11.25 31/8/84)
********

The  following  58  reprocessed  tapes  will  be sent from RAL on
Tuesday, 4/9/84.  These have all  been  run  through  the  REPAIR
program.

      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      948240   735          JADEPR.REDUC1.G0735
      948241   736          JADEPR.REDUC1.G0736
      948242   737          JADEPR.REDUC1.G0737
      948243   738          JADEPR.REDUC1.G0738
      948244   739          JADEPR.REDUC1.G0739
      948245   740          JADEPR.REDUC1.G0740
      948246   741          JADEPR.REDUC1.G0741
      948247   742          JADEPR.REDUC1.G0742
      948248   743          JADEPR.REDUC1.G0743
      948249   744          JADEPR.REDUC1.G0744
      948250   745          JADEPR.REDUC1.G0745
      948251   746          JADEPR.REDUC1.G0746
      948252   747          JADEPR.REDUC1.G0747
      948253   748          JADEPR.REDUC1.G0748
      948254   749          JADEPR.REDUC1.G0749
      948255   750          JADEPR.REDUC1.G0750
      948256   751          JADEPR.REDUC1.G0751
      948257   752          JADEPR.REDUC1.G0752
      948258   753          JADEPR.REDUC1.G0753
      948259   754          JADEPR.REDUC1.G0754
      948260   755          JADEPR.REDUC1.G0755
      948261   757          JADEPR.REDUC1.G0757
      948262   758          JADEPR.REDUC1.G0758
      948263   759          JADEPR.REDUC1.G0759
      948315   760          JADEPR.REDUC1.G0760
      948317   761          JADEPR.REDUC1.G0761
      948318   762          JADEPR.REDUC1.G0762
      948319   763          JADEPR.REDUC1.G0763
      948320   764          JADEPR.REDUC1.G0764
      948321   765          JADEPR.REDUC1.G0765
      948322   766          JADEPR.REDUC1.G0766
      948323   767          JADEPR.REDUC1.G0767
      948324   768          JADEPR.REDUC1.G0768
      948325   769          JADEPR.REDUC1.G0769
      948326   770          JADEPR.REDUC1.G0770
      948327   771          JADEPR.REDUC1.G0771
      939442   772          JADEPR.REDUC1.G0772
      939443   773          JADEPR.REDUC1.G0773
      939445   775          JADEPR.REDUC1.G0775
      939446   776          JADEPR.REDUC1.G0776
      939447   777          JADEPR.REDUC1.G0777
      939448   778          JADEPR.REDUC1.G0778
      939449   779          JADEPR.REDUC1.G0779
      939450   780          JADEPR.REDUC1.G0780
      939451   781          JADEPR.REDUC1.G0781
      939452   782          JADEPR.REDUC1.G0782
      939453   783          JADEPR.REDUC1.G0783
      939455   784          JADEPR.REDUC1.G0784
      939456   785          JADEPR.REDUC1.G0785
      939457   786          JADEPR.REDUC1.G0786
      939458   787          JADEPR.REDUC1.G0787
      939459   788          JADEPR.REDUC1.G0788
      939460   789          JADEPR.REDUC1.G0789
      939461   790          JADEPR.REDUC1.G0790
      939462   791          JADEPR.REDUC1.G0791
      939463   792          JADEPR.REDUC1.G0792
      939464   793          JADEPR.REDUC1.G0793
      939465   794          JADEPR.REDUC1.G0794

Please note the following MERGE sequences are missing:

     756 - data never sent to RAL - as discussed before.

     774 - the copy of this data held at RAL has become corrupt.
           In particular there is a logical end of tape in
           the volume header label. I am trying to recover
           the data file but it doesn't look too hopeful.

The  following  24  merged  tapes  will  also be sent from RAL on
Tuesday, 4/9/84.

      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      939504   836          JADEPR.REDUC1.G0836
      939508   839          JADEPR.REDUC1.G0839
      939509   840          JADEPR.REDUC1.G0840
      939510   841          JADEPR.REDUC1.G0841
      939524   842          JADEPR.REDUC1.G0842
      939490   843          JADEPR.REDUC1.G0843
      939493   844          JADEPR.REDUC1.G0844
      939494   845          JADEPR.REDUC1.G0845
      939511   846          JADEPR.REDUC1.G0846
      939513   847          JADEPR.REDUC1.G0847
      939514   848          JADEPR.REDUC1.G0848
      939515   849          JADEPR.REDUC1.G0849
      939516   850          JADEPR.REDUC1.G0850
      939517   851          JADEPR.REDUC1.G0851
      939518   852          JADEPR.REDUC1.G0852
      939519   853          JADEPR.REDUC1.G0853
      939512   854          JADEPR.REDUC1.G0854
      939491   855          JADEPR.REDUC1.G0855
      939492   856          JADEPR.REDUC1.G0856
      939600   857          JADEPR.REDUC1.G0857
      939601   858          JADEPR.REDUC1.G0858
      939602   859          JADEPR.REDUC1.G0859
      939603   860          JADEPR.REDUC1.G0860
      939604   861          JADEPR.REDUC1.G0861

                        Barrie Whittaker

*****************************************************************
========================================================================
========================================================================
********
******** MESSAGE FROM JBW (13.55 7/9/84)
********

The  following reprocessed tape will be sent from RAL on Tuesday,
11/9/84.  It corresponds  to  the  corrupt  tape  mentioned  last
time.   It has been run through the REPAIR program.  Please note,
however, that in recovering the data  the  first  3  events  were
lost.  The tape now only contains 9977 events instead of 9980.

      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      939444   774          JADEPR.REDUC1.G0774

The  following  5  merged  tapes  will  also  be sent from RAL on
Tuesday, 11/9/84.

      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      939605   862          JADEPR.REDUC1.G0862
      939606   863          JADEPR.REDUC1.G0863
      939607   864          JADEPR.REDUC1.G0864
      939608   865          JADEPR.REDUC1.G0865
      939609   866          JADEPR.REDUC1.G0866

In  addition  tapes  953120  -  953129  are being sent for Susan.
Please do not use them for reform data.

                     Thanks,
                        Barrie Whittaker

*****************************************************************
========================================================================
========================================================================
********
******** MESSAGE FROM JBW (13.45 12/9/84)
********

The  following  6  merged tapes will be sent from RAL on Tuesday,
18/9/84.

      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      939617   814          JADEPR.REDUC1.G0814
      939618   838          JADEPR.REDUC1.G0838
      939610   867          JADEPR.REDUC1.G0867
      939612   868          JADEPR.REDUC1.G0868
      939614   869          JADEPR.REDUC1.G0869
      939615   870          JADEPR.REDUC1.G0870

Please  note  that  Run 17944 seems to be at a higher energy than
the rest.  See the REDUCONE file.

                        Barrie Whittaker

*****************************************************************
========================================================================
========================================================================
********
******** MESSAGE FROM JBW (14.50 28/9/84)
********

Since  as far as I know the REDUC1 program has not been corrected
for the current magnetic field yet, I cannot update  the  program
at  this  end  and  check  it before I go on holiday (Sept.  29 -
Oct.  14).  In any case according to the RUNLIST3 file  no  tapes
have  been sent since 29/8/84 and we have processed all those and
returned them.  I propose to let the reduction work rest until  I
return  from holiday.  Please make sure that the program has been
updated by then and checked in execution.

                   Thanks,
                        Barrie Whittaker

*****************************************************************
========================================================================
========================================================================
MESSAGE FROM GINA STUART & BARRY WHITTAKER   AT RAL

 The following merged tapes were sent from RAL on  TUESDAY 23.10.84

      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      939611   871          JADEPR.REDUC1.G0871
      939613   872          JADEPR.REDUC1.G0872
      939616   873          JADEPR.REDUC1.G0873
      939556   874          JADEPR.REDUC1.G0874
      939557   875          JADEPR.REDUC1.G0875
      939558   876          JADEPR.REDUC1.G0876
      939559   877          JADEPR.REDUC1.G0877
      939560   878          JADEPR.REDUC1.G0878

*****************************************************************
========================================================================
========================================================================
MESSAGE FROM GINA STUART & BARRY WHITTAKER   AT RAL

 The following merged tapes will be sent from RAL on  TUESDAY 30.10.84

      939561   879          JADEPR.REDUC1.G0879
      939619   880          JADEPR.REDUC1.G0880
      939620   881          JADEPR.REDUC1.G0881
      939621   882          JADEPR.REDUC1.G0882
      939622   883          JADEPR.REDUC1.G0883
      939562   884          JADEPR.REDUC1.G0884
      939563   885          JADEPR.REDUC1.G0885
      939564   886          JADEPR.REDUC1.G0886
      939565   887          JADEPR.REDUC1.G0887
      939575   888          JADEPR.REDUC1.G0888
      939576   889          JADEPR.REDUC1.G0889
      939566   890          JADEPR.REDUC1.G0890
      939567   891          JADEPR.REDUC1.G0891
      939568   892          JADEPR.REDUC1.G0892
      939569   893          JADEPR.REDUC1.G0893
      939570   894          JADEPR.REDUC1.G0894
      939571   895          JADEPR.REDUC1.G0895
      939572   896          JADEPR.REDUC1.G0896
      939577   897          JADEPR.REDUC1.G0897
      939578   898          JADEPR.REDUC1.G0898
      939579   899          JADEPR.REDUC1.G0899
      939580   900          JADEPR.REDUC1.G0900
      939581   901          JADEPR.REDUC1.G0901
      939582   902          JADEPR.REDUC1.G0902
      939583   903          JADEPR.REDUC1.G0903
      939584   904          JADEPR.REDUC1.G0904

========================================================================
========================================================================
********
******** MESSAGE FROM JBW (11.00 20/12/84)
********

<*** MESSAGE GOES IN HERE ***>

                        Barrie Whittaker/Gina Stuart

*****************************************************************
*****************************************************************

 The following merged tapes will be sent from RAL on  TUESDAY 18.12.84

-
                                       RUTMESSG FILE INFORMATION
                                       -------- ---- -----------

 The following merged tapes were sent from RAL on

      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      939585   905          JADEPR.REDUC1.G0905
      939586   906          JADEPR.REDUC1.G0906
      939587   907          JADEPR.REDUC1.G0907
      939588   908          JADEPR.REDUC1.G0908
      939589   909          JADEPR.REDUC1.G0909
      939590   910          JADEPR.REDUC1.G0910
      939591   911          JADEPR.REDUC1.G0911
      939592   912          JADEPR.REDUC1.G0912
      939593   913          JADEPR.REDUC1.G0913
      939594   914          JADEPR.REDUC1.G0914
      939595   915          JADEPR.REDUC1.G0915
      939596   916          JADEPR.REDUC1.G0916
      939597   917          JADEPR.REDUC1.G0917
      939598   918          JADEPR.REDUC1.G0918
      939599   919          JADEPR.REDUC1.G0919
      939600   920          JADEPR.REDUC1.G0920
      939601   921          JADEPR.REDUC1.G0921
      939602   922          JADEPR.REDUC1.G0922
      939605   923          JADEPR.REDUC1.G0923
      939606   924          JADEPR.REDUC1.G0924
      939607   925          JADEPR.REDUC1.G0925
      939608   926          JADEPR.REDUC1.G0926
      939609   927          JADEPR.REDUC1.G0927
      948230   928          JADEPR.REDUC1.G0928
      948231   929          JADEPR.REDUC1.G0929
      948248   930          JADEPR.REDUC1.G0930
      939603   931          JADEPR.REDUC1.G0931
      939604   932          JADEPR.REDUC1.G0932
      948232   933          JADEPR.REDUC1.G0933
      948233   934          JADEPR.REDUC1.G0934
      948234   935          JADEPR.REDUC1.G0935
      948235   936          JADEPR.REDUC1.G0936
      948236   937          JADEPR.REDUC1.G0937
      948237   938          JADEPR.REDUC1.G0938
      948238   939          JADEPR.REDUC1.G0939
      948239   940          JADEPR.REDUC1.G0940
      948240   941          JADEPR.REDUC1.G0941
      948241   942          JADEPR.REDUC1.G0942
      948242   943          JADEPR.REDUC1.G0943
      948243   944          JADEPR.REDUC1.G0944
========================================================================
========================================================================
********
******** MESSAGE FROM JBW & GWST (11.1.85)
********

 The following merged tapes will be sent from RAL on  TUESDAY 15.1.85
including tape 939598 listed in previous set of tapes sent from RAL.

-
                                       RUTMESSG FILE INFORMATION
                                       -------- ---- -----------



 The following merged tapes were sent from RAL on

      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      948244   945          JADEPR.REDUC1.G0945
      948245   946          JADEPR.REDUC1.G0946
      948246   947          JADEPR.REDUC1.G0947
      948247   948          JADEPR.REDUC1.G0948
      948249   949          JADEPR.REDUC1.G0949
      939431   950          JADEPR.REDUC1.G0950
      939432   951          JADEPR.REDUC1.G0951
      939433   952          JADEPR.REDUC1.G0952
      939434   953          JADEPR.REDUC1.G0953
      939435   954          JADEPR.REDUC1.G0954
      939436   955          JADEPR.REDUC1.G0955
      939437   956          JADEPR.REDUC1.G0956


                        Barrie Whittaker/Gina Stuart

*****************************************************************
========================================================================
========================================================================
********
******** MESSAGE FROM JBW & GWST (24.1.85)
********

 The following merged tapes will be sent from RAL on  TUESDAY 22.1.85


free tapes only    939425 - 939430
                   939438 - 939453
                   939455 - 939470
                   939472 - 939490
                   939505
                   58 tapes.
                        Barrie Whittaker/Gina Stuart

*****************************************************************
*****************************************************************

========================================================================
========================================================================
********
******** MESSAGE FROM GWST (11.7.85)
********

 The following merged tapes were sent from RAL on  TUESDAY 9.7.85


 The following merged tapes were sent from RAL on

      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      948250   957          JADEPR.REDUC1.G0957
      948251   958          JADEPR.REDUC1.G0958
      948252   959          JADEPR.REDUC1.G0959
      948253   960          JADEPR.REDUC1.G0960
      948254   961          JADEPR.REDUC1.G0961
      948255   962          JADEPR.REDUC1.G0962
      948256   963          JADEPR.REDUC1.G0963
      948257   964          JADEPR.REDUC1.G0964
      948258   965          JADEPR.REDUC1.G0965
      948259   966          JADEPR.REDUC1.G0966
      948260   967          JADEPR.REDUC1.G0967
      948261   968          JADEPR.REDUC1.G0968
      948262   969          JADEPR.REDUC1.G0969
      948263   970          JADEPR.REDUC1.G0970
      939491   971          JADEPR.REDUC1.G0971
      939492   972          JADEPR.REDUC1.G0972
                   16 tapes.
                        Gina Stuart

========================================================================
========================================================================
********
******** MESSAGE FROM GWST (22.7.85)
********

 The following merged tapes will be sent from RAL on  TUESDAY 23.7.85


      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      939493   973          JADEPR.REDUC1.G0973
      939494   974          JADEPR.REDUC1.G0974
      939462   975          JADEPR.REDUC1.G0975
      939463   976          JADEPR.REDUC1.G0976
      939464   977          JADEPR.REDUC1.G0977
      939465   978          JADEPR.REDUC1.G0978
      939466   979          JADEPR.REDUC1.G0979
      939467   980          JADEPR.REDUC1.G0980
      939468   981          JADEPR.REDUC1.G0981
      939469   982          JADEPR.REDUC1.G0982
      939470   983          JADEPR.REDUC1.G0983
      939472   984          JADEPR.REDUC1.G0984
      939473   985          JADEPR.REDUC1.G0985
      939474   986          JADEPR.REDUC1.G0986
      939475   987          JADEPR.REDUC1.G0987
      939476   988          JADEPR.REDUC1.G0988
      939477   989          JADEPR.REDUC1.G0989
      939478   990          JADEPR.REDUC1.G0990
      939526   991          JADEPR.REDUC1.G0991
      939527   992          JADEPR.REDUC1.G0992
      939528   993          JADEPR.REDUC1.G0993
      939529   994          JADEPR.REDUC1.G0994
      939530   995          JADEPR.REDUC1.G0995
      939531   996          JADEPR.REDUC1.G0996
      939532   997          JADEPR.REDUC1.G0997
      939533   998          JADEPR.REDUC1.G0998
      939534   999          JADEPR.REDUC1.G0999
      939535  1000          JADEPR.REDUC1.G1000
      939536  1001          JADEPR.REDUC1.G1001
      939537  1002          JADEPR.REDUC1.G1002
      939538  1003          JADEPR.REDUC1.G1003
      939539  1004          JADEPR.REDUC1.G1004
      939540  1005          JADEPR.REDUC1.G1005
                   33 tapes.
                        Gina Stuart

*****************************************************************
========================================================================
========================================================================
********
******** MESSAGE FROM GWST (26.7.85)
********

 The following merged tapes will be sent from RAL on  TUESDAY 30.7.85


      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      939541  1006          JADEPR.REDUC1.G1006
      939542  1007          JADEPR.REDUC1.G1007
      939544  1008          JADEPR.REDUC1.G1008
      939543  1009          JADEPR.REDUC1.G1009
      939545  1010          JADEPR.REDUC1.G1010
      939546  1011          JADEPR.REDUC1.G1011
      939548  1012          JADEPR.REDUC1.G1012
      939549  1013          JADEPR.REDUC1.G1013
      939550  1014          JADEPR.REDUC1.G1014
      939551  1015          JADEPR.REDUC1.G1015
      939553  1017          JADEPR.REDUC1.G1017
      939554  1018          JADEPR.REDUC1.G1018
      939555  1019          JADEPR.REDUC1.G1019
      939557  1020          JADEPR.REDUC1.G1020
      939558  1021          JADEPR.REDUC1.G1021
      939559  1022          JADEPR.REDUC1.G1022
      939560  1023          JADEPR.REDUC1.G1023
      939562  1024          JADEPR.REDUC1.G1024
      939561  1025          JADEPR.REDUC1.G1025
      939563  1026          JADEPR.REDUC1.G1026
                   20 tapes.
                        Gina Stuart

========================================================================
========================================================================
********
******** MESSAGE FROM GWST (02.8.85)
********

 The following merged tapes will be sent from RAL on  TUESDAY 6.8.85


      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      939552  1016          JADEPR.REDUC1.G1016
      939564  1027          JADEPR.REDUC1.G1027
      939565  1028          JADEPR.REDUC1.G1028
      939567  1029          JADEPR.REDUC1.G1029
      939566  1030          JADEPR.REDUC1.G1030
      939568  1031          JADEPR.REDUC1.G1031
      939556  1032          JADEPR.REDUC1.G1032
      939569  1033          JADEPR.REDUC1.G1033
      939571  1034          JADEPR.REDUC1.G1034
                   9 tapes.
                        Gina Stuart

========================================================================
========================================================================
********
******** MESSAGE FROM GWST (09.8.85)
********

 The following merged tapes will be sent from RAL on  TUESDAY 6.8.85


      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
1                                      RUTMESSG FILE INFORMATION
                                       -------- ---- -----------

      948242  1035          JADEPR.REDUC1.G1035
      948243  1036          JADEPR.REDUC1.G1036
      948244  1037          JADEPR.REDUC1.G1037
      948245  1038          JADEPR.REDUC1.G1038
      948246  1039          JADEPR.REDUC1.G1039
      948247  1040          JADEPR.REDUC1.G1040
      948248  1041          JADEPR.REDUC1.G1041
      948249  1042          JADEPR.REDUC1.G1042
      939428  1043          JADEPR.REDUC1.G1043
      939430  1044          JADEPR.REDUC1.G1044
      939431  1045          JADEPR.REDUC1.G1045
      939432  1046          JADEPR.REDUC1.G1046
                  12 tapes.
                        Gina Stuart

*****************************************************************
========================================================================
========================================================================
********
******** MESSAGE FROM GWST (04.9.85)
********

 The following merged tapes will be sent from RAL on  TUESDAY 10.9.85


      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      RUTMESSG FILE INFORMATION
      -------- ---- -----------
      939433  1047          JADEPR.REDUC1.G1047
      939435  1048          JADEPR.REDUC1.G1048
      939436  1049          JADEPR.REDUC1.G1049
      939429  1050          JADEPR.REDUC1.G1050
      939434  1051          JADEPR.REDUC1.G1051
      939437  1052          JADEPR.REDUC1.G1052
      939439  1053          JADEPR.REDUC1.G1053
      939438  1054          JADEPR.REDUC1.G1054
      939440  1055          JADEPR.REDUC1.G1055
      939442  1056          JADEPR.REDUC1.G1056
      939443  1057          JADEPR.REDUC1.G1057
      939444  1058          JADEPR.REDUC1.G1058
      939445  1059          JADEPR.REDUC1.G1059
      939446  1060          JADEPR.REDUC1.G1060
      939447  1061          JADEPR.REDUC1.G1061
      939449  1062          JADEPR.REDUC1.G1062
      939448  1063          JADEPR.REDUC1.G1063
      939450  1064          JADEPR.REDUC1.G1064
      939451  1065          JADEPR.REDUC1.G1065
      939452  1066          JADEPR.REDUC1.G1066
      939453  1067          JADEPR.REDUC1.G1067
      939455  1068          JADEPR.REDUC1.G1068
      939457  1069          JADEPR.REDUC1.G1069
      939458  1070          JADEPR.REDUC1.G1070
      939459  1071          JADEPR.REDUC1.G1071
      948264  1072          JADEPR.REDUC1.G1072
      939456  1073          JADEPR.REDUC1.G1073
      939441  1074          JADEPR.REDUC1.G1074
      948265  1075          JADEPR.REDUC1.G1075
      948266  1076          JADEPR.REDUC1.G1076
      948268  1077          JADEPR.REDUC1.G1077
      948267  1078          JADEPR.REDUC1.G1078
      948270  1079          JADEPR.REDUC1.G1079
      948269  1080          JADEPR.REDUC1.G1080
      948271  1081          JADEPR.REDUC1.G1081
      948273  1082          JADEPR.REDUC1.G1082
      948272  1083          JADEPR.REDUC1.G1083
      948274  1084          JADEPR.REDUC1.G1084

                  38 tapes.
                        Gina Stuart

*****************************************************************
========================================================================
========================================================================
********
******** MESSAGE FROM GWST (05.9.85)
********

 The following merged tapes will be sent from RAL on  TUESDAY 10.9.85

  RUTMESSG FILE INFORMATION
  ------- ---- -----------


     TAPE     MERGE SEQ.   DSN
     ------   ----------   -------------------
     948275  1085          JADEPR.REDUC1.G1085
     948276  1086          JADEPR.REDUC1.G1086
     948277  1087          JADEPR.REDUC1.G1087
     948278  1088          JADEPR.REDUC1.G1088
     948279  1089          JADEPR.REDUC1.G1089
     948280  1090          JADEPR.REDUC1.G1090
     948281  1091          JADEPR.REDUC1.G1091


                   7 tapes.
                        Gina Stuart

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

           Please send free tapes   !!!!!
           Please send free tapes   !!!!!
           Please send free tapes   !!!!!

                                           L. Becker        18.9.85

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
========================================================================
========================================================================
********
******** MESSAGE FROM GWST (07.10.85)
********

 The following merged tapes will be sent from RAL on  TUESDAY 8.10.85

  RUTMESSG FILE INFORMATION
  ------- ---- -----------

      948282  1092          JADEPR.REDUC1.G1092
      948283  1093          JADEPR.REDUC1.G1093
      948284  1094          JADEPR.REDUC1.G1094
      939462  1095          JADEPR.REDUC1.G1095
      939463  1096          JADEPR.REDUC1.G1096
      939464  1097          JADEPR.REDUC1.G1097
      939465  1098          JADEPR.REDUC1.G1098
      948250  1099          JADEPR.REDUC1.G1099
      948251  1100          JADEPR.REDUC1.G1100
      939520  1101          JADEPR.REDUC1.G1101
      939521  1102          JADEPR.REDUC1.G1102
      939522  1103          JADEPR.REDUC1.G1103
      939523  1104          JADEPR.REDUC1.G1104
      939524  1105          JADEPR.REDUC1.G1105
      939526  1106          JADEPR.REDUC1.G1106
      939525  1107          JADEPR.REDUC1.G1107
      939528  1108          JADEPR.REDUC1.G1108
      939527  1109          JADEPR.REDUC1.G1109
      939575  1110          JADEPR.REDUC1.G1110
      939529  1111          JADEPR.REDUC1.G1111
      939530  1112          JADEPR.REDUC1.G1112

                  21 tapes.
                        Gina Stuart

========================================================================
========================================================================
********
******** MESSAGE FROM GWST (16.10.85)
********

 The following merged tapes will be sent from RAL on  TUESDAY 22.10.85

  RUTMESSG FILE INFORMATION
  ------- ---- -----------

      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      939532  1113          JADEPR.REDUC1.G1113
      939531  1114          JADEPR.REDUC1.G1114
      939533  1115          JADEPR.REDUC1.G1115
      939534  1116          JADEPR.REDUC1.G1116
      939535  1117          JADEPR.REDUC1.G1117
      939537  1118          JADEPR.REDUC1.G1118
      939536  1119          JADEPR.REDUC1.G1119
      939539  1120          JADEPR.REDUC1.G1120
      939540  1121          JADEPR.REDUC1.G1121
      939538  1122          JADEPR.REDUC1.G1122
      939542  1123          JADEPR.REDUC1.G1123
      939541  1124          JADEPR.REDUC1.G1124
      939544  1125          JADEPR.REDUC1.G1125
      939545  1126          JADEPR.REDUC1.G1126
      939543  1127          JADEPR.REDUC1.G1127
      939548  1128          JADEPR.REDUC1.G1128
      939518  1129          JADEPR.REDUC1.G1129
      939549  1130          JADEPR.REDUC1.G1130
      939546  1131          JADEPR.REDUC1.G1131
      939583  1132          JADEPR.REDUC1.G1132
      939584  1133          JADEPR.REDUC1.G1133
      939585  1134          JADEPR.REDUC1.G1134
      939586  1135          JADEPR.REDUC1.G1135
      939587  1136          JADEPR.REDUC1.G1136
      939588  1137          JADEPR.REDUC1.G1137
      939589  1138          JADEPR.REDUC1.G1138
      939590  1139          JADEPR.REDUC1.G1139
      939591  1140          JADEPR.REDUC1.G1140
      939592  1141          JADEPR.REDUC1.G1141
      939593  1142          JADEPR.REDUC1.G1142
      939594  1143          JADEPR.REDUC1.G1143
      939595  1144          JADEPR.REDUC1.G1144
      939597  1145          JADEPR.REDUC1.G1145
      939596  1146          JADEPR.REDUC1.G1146


                  34 tapes.
                        Gina Stuart

*****************************************************************
========================================================================
========================================================================
********
******** MESSAGE FROM GWST (30.10.85)
********

 The following merged tapes will be sent from RAL on  TUESDAY 5.11.85

  RUTMESSG FILE INFORMATION
  ------- ---- -----------
 The following merged tapes were sent from RAL on

      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      939577  1147          JADEPR.REDUC1.G1147
      939578  1148          JADEPR.REDUC1.G1148
      939580  1149          JADEPR.REDUC1.G1149
      939581  1150          JADEPR.REDUC1.G1150
      939449  1151          JADEPR.REDUC1.G1151
      939450  1152          JADEPR.REDUC1.G1152
      939452  1153          JADEPR.REDUC1.G1153
      939451  1154          JADEPR.REDUC1.G1154
      939453  1155          JADEPR.REDUC1.G1155
      939455  1156          JADEPR.REDUC1.G1156
      939456  1157          JADEPR.REDUC1.G1157
      939458  1158          JADEPR.REDUC1.G1158
      939457  1159          JADEPR.REDUC1.G1159
      939459  1160          JADEPR.REDUC1.G1160
      948286  1161          JADEPR.REDUC1.G1161
      948287  1162          JADEPR.REDUC1.G1162
      948288  1163          JADEPR.REDUC1.G1163
      948289  1164          JADEPR.REDUC1.G1164
      948290  1165          JADEPR.REDUC1.G1165
      948291  1166          JADEPR.REDUC1.G1166
      948292  1167          JADEPR.REDUC1.G1167
      948293  1168          JADEPR.REDUC1.G1168
      948294  1169          JADEPR.REDUC1.G1169
      948295  1170          JADEPR.REDUC1.G1170
      948296  1171          JADEPR.REDUC1.G1171

                  24 tapes.
                        Gina Stuart

========================================================================
========================================================================
********
******** MESSAGE FROM GWST (06.12.85)
********

 The following merged tapes were be sent from RAL on  TUESDAY 3.12.85


      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      948297  1172          JADEPR.REDUC1.G1172
      948298  1173          JADEPR.REDUC1.G1173
      948299  1174          JADEPR.REDUC1.G1174
      948302  1175          JADEPR.REDUC1.G1175
      948301  1176          JADEPR.REDUC1.G1176
      948303  1177          JADEPR.REDUC1.G1177
      948304  1178          JADEPR.REDUC1.G1178
      948305  1179          JADEPR.REDUC1.G1179
      948306  1180          JADEPR.REDUC1.G1180
      948307  1181          JADEPR.REDUC1.G1181
      948308  1182          JADEPR.REDUC1.G1182
      948309  1183          JADEPR.REDUC1.G1183
      948310  1184          JADEPR.REDUC1.G1184
      948311  1185          JADEPR.REDUC1.G1185
      948300  1186          JADEPR.REDUC1.G1186
      948312  1187          JADEPR.REDUC1.G1187
      948313  1188          JADEPR.REDUC1.G1188
      948315  1189          JADEPR.REDUC1.G1189
      948314  1190          JADEPR.REDUC1.G1190
      948317  1191          JADEPR.REDUC1.G1191
      939462  1192          JADEPR.REDUC1.G1192
      939463  1193          JADEPR.REDUC1.G1193
      939464  1194          JADEPR.REDUC1.G1194
      939465  1195          JADEPR.REDUC1.G1195
      939466  1196          JADEPR.REDUC1.G1196
      939467  1197          JADEPR.REDUC1.G1197
      948250  1198          JADEPR.REDUC1.G1198
      948252  1199          JADEPR.REDUC1.G1199
      948251  1200          JADEPR.REDUC1.G1200
      948253  1201          JADEPR.REDUC1.G1201
      948254  1202          JADEPR.REDUC1.G1202
      948257  1203          JADEPR.REDUC1.G1203
      948255  1204          JADEPR.REDUC1.G1204
      948258  1205          JADEPR.REDUC1.G1205
      948259  1206          JADEPR.REDUC1.G1206
      948260  1207          JADEPR.REDUC1.G1207
      948262  1208          JADEPR.REDUC1.G1208
      948256  1209          JADEPR.REDUC1.G1209
      948319  1210          JADEPR.REDUC1.G1210

                  39 tapes.
========================================================================
********
******** MESSAGE FROM GWST (20.12.85)
========================================================================
========================================================================
********
******** MESSAGE FROM GWST (20.12.85)
********
 The following merged tapes will be sent from RAL on  TUESDAY 7.1.86

      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      948263  1211          JADEPR.REDUC1.G1211
      948318  1212          JADEPR.REDUC1.G1212
      948321  1213          JADEPR.REDUC1.G1213
      948261  1214          JADEPR.REDUC1.G1214
      948322  1215          JADEPR.REDUC1.G1215
      948323  1216          JADEPR.REDUC1.G1216
      948320  1217          JADEPR.REDUC1.G1217
      948324  1218          JADEPR.REDUC1.G1218
      948326  1219          JADEPR.REDUC1.G1219
      948327  1220          JADEPR.REDUC1.G1220
      948325  1221          JADEPR.REDUC1.G1221
      939469  1222          JADEPR.REDUC1.G1222
      939468  1223          JADEPR.REDUC1.G1223
      939472  1224          JADEPR.REDUC1.G1224
      939470  1225          JADEPR.REDUC1.G1225
      939473  1226          JADEPR.REDUC1.G1226
      939475  1227          JADEPR.REDUC1.G1227
      939474  1228          JADEPR.REDUC1.G1228
      939476  1229          JADEPR.REDUC1.G1229
      939477  1230          JADEPR.REDUC1.G1230
      939478  1231          JADEPR.REDUC1.G1231
      939479  1232          JADEPR.REDUC1.G1232
      939480  1233          JADEPR.REDUC1.G1233
      939481  1234          JADEPR.REDUC1.G1234
      939482  1235          JADEPR.REDUC1.G1235
      939483  1236          JADEPR.REDUC1.G1236
      939484  1237          JADEPR.REDUC1.G1237
      939485  1238          JADEPR.REDUC1.G1238
      939486  1239          JADEPR.REDUC1.G1239
      939487  1240          JADEPR.REDUC1.G1240
      939488  1241          JADEPR.REDUC1.G1241
      939490  1242          JADEPR.REDUC1.G1242
      939489  1243          JADEPR.REDUC1.G1243


                  33 tapes.  This is the end of present data until
   running starts again  (Spring 86?)

                        Gina Stuart
========================================================================
========================================================================
********
******** MESSAGE FROM GWST (29.04.86)

                                       RUTMESSG FILE INFORMATION
                                       -------- ---- -----------

 The following merged tapes were sent from RAL on 29.4.86

      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      939514  1244          JADEPR.REDUC1.G1244
      939516  1245          JADEPR.REDUC1.G1245
      939515  1246          JADEPR.REDUC1.G1246
      939518  1247          JADEPR.REDUC1.G1247
      939517  1248          JADEPR.REDUC1.G1248
      939519  1249          JADEPR.REDUC1.G1249
      939520  1250          JADEPR.REDUC1.G1250
      939522  1251          JADEPR.REDUC1.G1251
      939521  1252          JADEPR.REDUC1.G1252
      939523  1253          JADEPR.REDUC1.G1253
      939524  1254          JADEPR.REDUC1.G1254
      939525  1255          JADEPR.REDUC1.G1255
      939526  1256          JADEPR.REDUC1.G1256
      939528  1257          JADEPR.REDUC1.G1257
      939527  1258          JADEPR.REDUC1.G1258
      939529  1259          JADEPR.REDUC1.G1259
      939530  1260          JADEPR.REDUC1.G1260
      939531  1261          JADEPR.REDUC1.G1261
      939532  1262          JADEPR.REDUC1.G1262

                  19 tapes.
   running starts again  (Spring 86?)

                        Gina Stuart

========================================================================
========================================================================
********
******** MESSAGE FROM GWST (30.04.86)

                                       RUTMESSG FILE INFORMATION
                                       -------- ---- -----------

 The following merged tapes will be sent from RAL on 7.5.86

                                       -------- ---- -----------
      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      939449  1263          JADEPR.REDUC1.G1263
      939450  1264          JADEPR.REDUC1.G1264
      939451  1265          JADEPR.REDUC1.G1265
      939452  1266          JADEPR.REDUC1.G1266
      939455  1267          JADEPR.REDUC1.G1267
      939453  1268          JADEPR.REDUC1.G1268
      939456  1269          JADEPR.REDUC1.G1269
      939458  1270          JADEPR.REDUC1.G1270
      939459  1271          JADEPR.REDUC1.G1271
      939464  1272          JADEPR.REDUC1.G1272
      939463  1273          JADEPR.REDUC1.G1273
      939462  1274          JADEPR.REDUC1.G1274
      939465  1275          JADEPR.REDUC1.G1275
      939474  1276          JADEPR.REDUC1.G1276
      939476  1277          JADEPR.REDUC1.G1277
      939473  1278          JADEPR.REDUC1.G1278
      939472  1279          JADEPR.REDUC1.G1279
      939475  1280          JADEPR.REDUC1.G1280
      939479  1281          JADEPR.REDUC1.G1281
      939481  1282          JADEPR.REDUC1.G1282
      939478  1283          JADEPR.REDUC1.G1283
      939477  1284          JADEPR.REDUC1.G1284
      939480  1285          JADEPR.REDUC1.G1285
      939484  1286          JADEPR.REDUC1.G1286
      939486  1287          JADEPR.REDUC1.G1287
      939483  1288          JADEPR.REDUC1.G1288
      939482  1289          JADEPR.REDUC1.G1289
      939489  1290          JADEPR.REDUC1.G1290
      939581  1291          JADEPR.REDUC1.G1291

                  29 tapes.

                        Gina Stuart

========================================================================
========================================================================
********
******** MESSAGE FROM GWST (6.6.86)

                                       RUTMESSG FILE INFORMATION
                                       -------- ---- -----------

 The following merged tapes will be sent from RAL on 10.6.86

                                       -------- ---- -----------
      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------

      948301  1292          JADEPR.REDUC1.G1292
      948304  1293          JADEPR.REDUC1.G1293
      948308  1294          JADEPR.REDUC1.G1294
      948307  1295          JADEPR.REDUC1.G1295
      948300  1296          JADEPR.REDUC1.G1296
      948306  1297          JADEPR.REDUC1.G1297
      948309  1298          JADEPR.REDUC1.G1298
      948313  1299          JADEPR.REDUC1.G1299
      948312  1300          JADEPR.REDUC1.G1300
      948311  1301          JADEPR.REDUC1.G1301
      948314  1302          JADEPR.REDUC1.G1302
      948305  1303          JADEPR.REDUC1.G1303
      948318  1304          JADEPR.REDUC1.G1304
      948319  1305          JADEPR.REDUC1.G1305
                  14 tapes.

                        Gina Stuart

========================================================================
========================================================================
********
******** MESSAGE FROM GWST (13.6.86)

                                       RUTMESSG FILE INFORMATION
                                       -------- ---- -----------

 The following merged tapes will be sent from RAL on 17.6.86

                                       -------- ---- -----------
      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------

      939562  1306          JADEPR.REDUC1.G1306
      939564  1307          JADEPR.REDUC1.G1307
      939565  1308          JADEPR.REDUC1.G1308
      939602  1309          JADEPR.REDUC1.G1309
      939600  1310          JADEPR.REDUC1.G1310
      939601  1311          JADEPR.REDUC1.G1311
      939604  1312          JADEPR.REDUC1.G1312
      939607  1313          JADEPR.REDUC1.G1313
      939605  1314          JADEPR.REDUC1.G1314
      939603  1315          JADEPR.REDUC1.G1315
      939606  1316          JADEPR.REDUC1.G1316
      939609  1317          JADEPR.REDUC1.G1317
      939610  1318          JADEPR.REDUC1.G1318
      939608  1319          JADEPR.REDUC1.G1319
      939611  1320          JADEPR.REDUC1.G1320
      939612  1321          JADEPR.REDUC1.G1321
                  16 tapes.

                        Gina Stuart

========================================================================
========================================================================
********
******** MESSAGE FROM GWST (23.6.86)

                                       RUTMESSG FILE INFORMATION
                                       -------- ---- -----------

 The following merged tapes will be sent from RAL on 24.6.86

                                       -------- ---- -----------
      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------

      939614  1322          JADEPR.REDUC1.G1322
      939613  1323          JADEPR.REDUC1.G1323
      939617  1324          JADEPR.REDUC1.G1324
      939618  1325          JADEPR.REDUC1.G1325
      939615  1326          JADEPR.REDUC1.G1326
      939616  1327          JADEPR.REDUC1.G1327
      939560  1328          JADEPR.REDUC1.G1328
      939425  1329          JADEPR.REDUC1.G1329
      939426  1330          JADEPR.REDUC1.G1330
      939428  1331          JADEPR.REDUC1.G1331
      939429  1332          JADEPR.REDUC1.G1332
      939427  1333          JADEPR.REDUC1.G1333
      939430  1334          JADEPR.REDUC1.G1334
      939431  1335          JADEPR.REDUC1.G1335
      939433  1336          JADEPR.REDUC1.G1336
                  15 tapes.

                        Gina Stuart
========================================================================
========================================================================
********
******** MESSAGE FROM GWST (8.7.86)

                                       RUTMESSG FILE INFORMATION
                                       -------- ---- -----------

 The following merged tapes will be sent from RAL on 8.7.86

                                       -------- ---- -----------
      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------

      939432  1337          JADEPR.REDUC1.G1337
      939434  1338          JADEPR.REDUC1.G1338
      939435  1339          JADEPR.REDUC1.G1339
      939438  1340          JADEPR.REDUC1.G1340
      939436  1341          JADEPR.REDUC1.G1341
      939439  1342          JADEPR.REDUC1.G1342
      939437  1343          JADEPR.REDUC1.G1343
      939440  1344          JADEPR.REDUC1.G1344
      939443  1345          JADEPR.REDUC1.G1345
      939441  1346          JADEPR.REDUC1.G1346
      939445  1347          JADEPR.REDUC1.G1347
      939442  1348          JADEPR.REDUC1.G1348
      939446  1349          JADEPR.REDUC1.G1349
      939492  1350          JADEPR.REDUC1.G1350
      939491  1351          JADEPR.REDUC1.G1351
      939447  1352          JADEPR.REDUC1.G1352
      939493  1353          JADEPR.REDUC1.G1353
      939448  1354          JADEPR.REDUC1.G1354
      939619  1355          JADEPR.REDUC1.G1355
      939495  1356          JADEPR.REDUC1.G1356
      939508  1357          JADEPR.REDUC1.G1357
      939494  1358          JADEPR.REDUC1.G1358
      939498  1359          JADEPR.REDUC1.G1359
      939499  1360          JADEPR.REDUC1.G1360
      939500  1361          JADEPR.REDUC1.G1361
      939502  1362          JADEPR.REDUC1.G1362
      939503  1363          JADEPR.REDUC1.G1363
      939504  1364          JADEPR.REDUC1.G1364
      939505  1365          JADEPR.REDUC1.G1365
      939509  1366          JADEPR.REDUC1.G1366
      939506  1367          JADEPR.REDUC1.G1367
      939511  1368          JADEPR.REDUC1.G1368
      948231  1369          JADEPR.REDUC1.G1369
      939513  1370          JADEPR.REDUC1.G1370
      939512  1371          JADEPR.REDUC1.G1371
      948230  1372          JADEPR.REDUC1.G1372
      948234  1373          JADEPR.REDUC1.G1373
      948232  1374          JADEPR.REDUC1.G1374
                  38 tapes.

                        Gina Stuart

========================================================================
========================================================================
********
******** MESSAGE FROM GWST (29.7.86)

 The following merged tapes will be sent from RAL on 29.7.86
                                       -------- ---- -----------

      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      939478  1375          JADEPR.REDUC1.G1375
      939480  1376          JADEPR.REDUC1.G1376
      939479  1377          JADEPR.REDUC1.G1377
      939482  1378          JADEPR.REDUC1.G1378
      939481  1379          JADEPR.REDUC1.G1379
      939483  1380          JADEPR.REDUC1.G1380
      948293  1381          JADEPR.REDUC1.G1381
      948292  1382          JADEPR.REDUC1.G1382
      948294  1383          JADEPR.REDUC1.G1383
      948295  1384          JADEPR.REDUC1.G1384
      948296  1385          JADEPR.REDUC1.G1385
      948298  1386          JADEPR.REDUC1.G1386
      948297  1387          JADEPR.REDUC1.G1387
      948299  1388          JADEPR.REDUC1.G1388
      939484  1389          JADEPR.REDUC1.G1389
      939485  1390          JADEPR.REDUC1.G1390
      939486  1391          JADEPR.REDUC1.G1391
      939488  1392          JADEPR.REDUC1.G1392
      939487  1393          JADEPR.REDUC1.G1393
      939490  1394          JADEPR.REDUC1.G1394
      939489  1395          JADEPR.REDUC1.G1395
      939514  1396          JADEPR.REDUC1.G1396
      939516  1397          JADEPR.REDUC1.G1397
      939515  1398          JADEPR.REDUC1.G1398

    24 tapes in all.     gina stuart
*****************************************************************
========================================================================
========================================================================
********
******** MESSAGE FROM GWST (22.8.86)

 The following merged tapes will be sent from RAL on 27.8.86
                                       -------- ---- -----------

      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      939455  1413          JADEPR.REDUC1.G1413
      939456  1414          JADEPR.REDUC1.G1414
      939458  1415          JADEPR.REDUC1.G1415
      939459  1416          JADEPR.REDUC1.G1416
      939464  1417          JADEPR.REDUC1.G1417
      939463  1418          JADEPR.REDUC1.G1418
      939465  1419          JADEPR.REDUC1.G1419
      939527  1420          JADEPR.REDUC1.G1420
      939467  1421          JADEPR.REDUC1.G1421
      939450  1410          JADEPR.REDUC1.G1410
      939451  1411          JADEPR.REDUC1.G1411
      939453  1412          JADEPR.REDUC1.G1412

    14 tapes in all. gina stuart
*****************************************************************
*****************************************************************

========================================================================
========================================================================
********
******** MESSAGE FROM GWST (22.8.86)

please ignore this data accidentally added at after line 1453, will
be added twice, ignore the earlier entry please. gina.

 The following merged tapes will be sent from RAL on 27.8.86
                                       -------- ---- -----------

      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      939455  1413          JADEPR.REDUC1.G1413
      939456  1414          JADEPR.REDUC1.G1414
      939458  1415          JADEPR.REDUC1.G1415
      939459  1416          JADEPR.REDUC1.G1416
      939464  1417          JADEPR.REDUC1.G1417
      939463  1418          JADEPR.REDUC1.G1418
      939465  1419          JADEPR.REDUC1.G1419
      939527  1420          JADEPR.REDUC1.G1420
      939467  1421          JADEPR.REDUC1.G1421
      939529  1422          JADEPR.REDUC1.G1422
      939528  1423          JADEPR.REDUC1.G1423
      939532  1424          JADEPR.REDUC1.G1424
      939531  1425          JADEPR.REDUC1.G1425
      939469  1426          JADEPR.REDUC1.G1426
      939535  1427          JADEPR.REDUC1.G1427
      939534  1428          JADEPR.REDUC1.G1428
    16 tapes in all. gina stuart
*****************************************************************
========================================================================
========================================================================
********
******** MESSAGE FROM GWST (6.10.86)

 The following merged tapes will be sent from RAL on 7.10.86
                                       -------- ---- -----------

      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------

      939598  1429          JADEPR.REDUC1.G1429
      939599  1430          JADEPR.REDUC1.G1430
      939600  1431          JADEPR.REDUC1.G1431
      939601  1432          JADEPR.REDUC1.G1432
      939602  1433          JADEPR.REDUC1.G1433
      939603  1434          JADEPR.REDUC1.G1434
      939605  1435          JADEPR.REDUC1.G1435
      939604  1436          JADEPR.REDUC1.G1436
      939607  1437          JADEPR.REDUC1.G1437
      939606  1438          JADEPR.REDUC1.G1438
      939608  1439          JADEPR.REDUC1.G1439
      939610  1440          JADEPR.REDUC1.G1440
      939609  1441          JADEPR.REDUC1.G1441
      939617  1442          JADEPR.REDUC1.G1442
      939611  1443          JADEPR.REDUC1.G1443
      939618  1444          JADEPR.REDUC1.G1444
      948251  1445          JADEPR.REDUC1.G1445
      948250  1446          JADEPR.REDUC1.G1446
      948253  1447          JADEPR.REDUC1.G1447
      948252  1448          JADEPR.REDUC1.G1448
      948254  1449          JADEPR.REDUC1.G1449
      948255  1450          JADEPR.REDUC1.G1450
      948256  1451          JADEPR.REDUC1.G1451
      948258  1452          JADEPR.REDUC1.G1452
    24 tapes in all. gina stuart
========================================================================
========================================================================
********
******** MESSAGE FROM GWST (20.10.86)

 The following merged tapes were sent from RAL on the 14.10.86
                                       -------- ---- -----------

      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------

      948246  1453          JADEPR.REDUC1.G1453
      948248  1454          JADEPR.REDUC1.G1454
      948249  1455          JADEPR.REDUC1.G1455
      948241  1456          JADEPR.REDUC1.G1456
      948247  1457          JADEPR.REDUC1.G1457
      939439  1459          JADEPR.REDUC1.G1459
      939440  1460          JADEPR.REDUC1.G1460
      939442  1461          JADEPR.REDUC1.G1461
      939443  1462          JADEPR.REDUC1.G1462
      939445  1463          JADEPR.REDUC1.G1463
      939446  1464          JADEPR.REDUC1.G1464
      939448  1465          JADEPR.REDUC1.G1465
      939447  1466          JADEPR.REDUC1.G1466
      939472  1467          JADEPR.REDUC1.G1467
      939474  1468          JADEPR.REDUC1.G1468
      939476  1469          JADEPR.REDUC1.G1469
      939475  1470          JADEPR.REDUC1.G1470
      939477  1471          JADEPR.REDUC1.G1471
      939478  1472          JADEPR.REDUC1.G1472
      939479  1473          JADEPR.REDUC1.G1473
      939480  1474          JADEPR.REDUC1.G1474
      939481  1475          JADEPR.REDUC1.G1475
      939482  1476          JADEPR.REDUC1.G1476
      939485  1477          JADEPR.REDUC1.G1477
      939483  1478          JADEPR.REDUC1.G1478
      939487  1479          JADEPR.REDUC1.G1479
      939484  1480          JADEPR.REDUC1.G1480
      939488  1481          JADEPR.REDUC1.G1481
      939489  1482          JADEPR.REDUC1.G1482
      939491  1483          JADEPR.REDUC1.G1483
      939492  1484          JADEPR.REDUC1.G1484
      939493  1485          JADEPR.REDUC1.G1485
      939490  1486          JADEPR.REDUC1.G1486
      939495  1487          JADEPR.REDUC1.G1487
      939499  1488          JADEPR.REDUC1.G1488
    35 tapes in all. gina stuart
========================================================================
========================================================================
********
******** MESSAGE FROM GWST (29.10.86)

 The following merged tapes were sent from RAL on Tuesday 3.11.86

      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      939500  1489          JADEPR.REDUC1.G1489
      939506  1490          JADEPR.REDUC1.G1490
      939441  1491          JADEPR.REDUC1.G1491
      939498  1492          JADEPR.REDUC1.G1492
      939430  1493          JADEPR.REDUC1.G1493
      939501  1494          JADEPR.REDUC1.G1494
      939502  1495          JADEPR.REDUC1.G1495
      939503  1496          JADEPR.REDUC1.G1496
      939486  1497          JADEPR.REDUC1.G1497
      939504  1498          JADEPR.REDUC1.G1498
      939508  1499          JADEPR.REDUC1.G1499
      939511  1500          JADEPR.REDUC1.G1500
      939509  1501          JADEPR.REDUC1.G1501
      939505  1502          JADEPR.REDUC1.G1502
      939512  1503          JADEPR.REDUC1.G1503
      939516  1504          JADEPR.REDUC1.G1504
      939514  1505          JADEPR.REDUC1.G1505
      939513  1506          JADEPR.REDUC1.G1506
      939521  1507          JADEPR.REDUC1.G1507
      939517  1508          JADEPR.REDUC1.G1508
      939520  1509          JADEPR.REDUC1.G1509
      939519  1510          JADEPR.REDUC1.G1510
      939525  1511          JADEPR.REDUC1.G1511
      939522  1512          JADEPR.REDUC1.G1512
      939524  1513          JADEPR.REDUC1.G1513
      939559  1514          JADEPR.REDUC1.G1514
      939558  1515          JADEPR.REDUC1.G1515
      939523  1516          JADEPR.REDUC1.G1516
      939555  1517          JADEPR.REDUC1.G1517
      also merge1424 on tape 939515 as requested by DESY
      30 tapes in all
      best wishes gina.
*****************************************************************
*****************************************************************
========================================================================
========================================================================
********
******** MESSAGE FROM GWST (11.11.86)

PLEASE IGNORE DATA ON MERGE TAPE 939504 SENT IN LAST BATCH
DATA ON TAPE IN JUNK. WILL SEND DATA AGAIN ASAP.



      best wishes gina.
========================================================================
========================================================================
********
******** MESSAGE FROM GWST (11.1 87)


 The following merged tapes were sent from RAL on 20.1.87

      TAPE     MERGE SEQ.   DSN
      ------   ----------   -------------------
      939557  1518          JADEPR.REDUC1.G0***
      939563  1519          JADEPR.REDUC1.G0***
      939556  1521          JADEPR.REDUC1.G0***
      939560  1522          JADEPR.REDUC1.G0***
      939564  1523          JADEPR.REDUC1.G0***
      939561  1524          JADEPR.REDUC1.G0***
      939562  1525          JADEPR.REDUC1.G0***
      939565  1526          JADEPR.REDUC1.G0***
      948264  1527          JADEPR.REDUC1.G0***
      948265  1528          JADEPR.REDUC1.G0***
      948266  1529          JADEPR.REDUC1.G0***
      948267  1530          JADEPR.REDUC1.G0***
      948268  1531          JADEPR.REDUC1.G0***
      948269  1532          JADEPR.REDUC1.G0***
      948270  1533          JADEPR.REDUC1.G0***
      948271  1534          JADEPR.REDUC1.G0***
      948272  1535          JADEPR.REDUC1.G0***
      948273  1536          JADEPR.REDUC1.G0***
      948274  1537          JADEPR.REDUC1.G0***
      948275  1538          JADEPR.REDUC1.G0***
      948276  1539          JADEPR.REDUC1.G0***
      948277  1540          JADEPR.REDUC1.G0***
      948278  1541          JADEPR.REDUC1.G0***
      948279  1542          JADEPR.REDUC1.G0***
      948280  1543          JADEPR.REDUC1.G0***
      948281  1544          JADEPR.REDUC1.G0***
      948282  1545          JADEPR.REDUC1.G0***
      948284  1546          JADEPR.REDUC1.G0***
      948283  1547          JADEPR.REDUC1.G0***
      948287  1548          JADEPR.REDUC1.G0***

   30 tapes in all.  gina stuart
  Robin Middleton asked me to send his apologies for not being able
  to do the jobs he had promised for jade this week, his father died
  and he is not at work this week( ist part at least).sorry for delay.
      best wishes gina.
========================================================================
========================================================================
********
******** MESSAGE FROM GWST (21.1 87)

  ALL MAG. TAPES FROM RAL NOW HAVE RUBBER GROMETS. SORRY ABOUT
  THIS. IT IS COMPUTING DIV. DECISION, WE HAVE ASKED THAT THEY DID
  NOT USE GROMETS ON JADE TAPES, BUT NOT ALLOWED.

 ABOVE 30 TAPES WILL HAVE GROMETS & MESSAGE WAS SENT 21.1.87
      best wishes gina.
*****************************************************************
*****************************************************************
*****************************************************************
*****************************************************************
*****************************************************************
*****************************************************************
    11/02/83            MEMBER NAME  RUTMESS1 (JADESR)      TEXT
C   11/02/83            MEMBER NAME  RUTMESSG (JADESR)      FORTRAN

      MESSAGES CONCERNING DATA REDUCTION AT RUTHERFORD LAB.
      ======================================================

      82/03/05 10.00
      YAMADA FOUND SMALL ERROR IN LGBRCS
      I COPIED THE NEW VERSION TO JADEPR.JADESR/LD

********
******** MESSAGE FROM GFP AT RUTHERFORD (10.00 11/03/82)
********
         1) RUN 10062 EVENT 77 HAS WRONG LENGTH POINTER IN JETC BANK
         2) I AM GETTING 10 TO 20 OR SO "NO LEAD GLASS HITS IN RUN AND
            EVENT ...." MESSAGES PER RUN. THIS DOESNT LOOK LIKE AN ERROR
            TO ME !! SHOULD I WORRY ABOUT THIS ??

********
******** MESSAGE FROM PST (10.00 12/03/82)
********
         GEOFF, I AM VERY SORRY BUT AT PRESENT I CAN REALLY DO NOTHING.
         I SHALL GIVE A TALK AT THE MORIOND CONF. ON TUESDAY, AND I
         LEAVE TOMORROW, AND MY PREPARATION FOR THE TALK IS IN A VERY
         BAD STATE. SO PROCEED AS YOU THINK REASONABLE.
         CONCERNING YOUR ERRORS:
         -- I DID NOT CHANGE JETCAL. IF THE DIVIDE CHECK COMES REGULAR
            YOU MUST FIND OUT. OTHERWISE PROCEED. MAYBE THE DATA ARE
            BAD.
         -- WRONG POINTER OF JETC-BANK SEEMS ALSO TO INDICATE BAD DATA.
            IT LOOKS LIKE AN ERROR IN THE REFORM STEP.
         -- CONCERNING NO LG-HITS: I WOULD IGNORE THIS.

   ****  DO NOT HESITATE TO GET MALCOLM, OR PETERSEN INVOLVED.
         I AM SORRY, THAT I CANNOT DO MORE. I WILL BE BACK MONDAY IN
         A WEEK.
                             BEST WISHES, PETER.

********
******** MESSAGE FROM GFP AT RUTHERFORD (12.00 24/03/82)
********
         A PROBLEM FOR PETER,
            I HAVE JUST GOT SOME ERROR MESSAGES FROM TRCK82. THEY
         ARE ALL 'UNKNOWN TRIGGER' MESSAGES AND ALL OCCUR IN THE
         SAME RUN, RUN 10553. THE ERROR MESSAGES FOLLOW :
         UNKNOWN TRIGGER FOR EVENT 10553   27  2000  0008  0000 *****
         UNKNOWN TRIGGER FOR EVENT 10553  106  2000  0000  0000 *****
         UNKNOWN TRIGGER FOR EVENT 10553  258  2000  0009  0025 *****
         UNKNOWN TRIGGER FOR EVENT 10553  287  2000  0008  0004 *****
         UNKNOWN TRIGGER FOR EVENT 10553  421  2000  0009  0005 *****
         UNKNOWN TRIGGER FOR EVENT 10553  723  2000  0000  0000 *****
         UNKNOWN TRIGGER FOR EVENT 10553 1021  2000  0101  0005 *****
         UNKNOWN TRIGGER FOR EVENT 10553 1102  2000  0000  0000 *****
         UNKNOWN TRIGGER FOR EVENT 10553 1297  2000  0000  0000 *****
         UNKNOWN TRIGGER FOR EVENT 10553 1459  2000  300A  0005 *****
         N.B. THIS RUN HAD 6928 EVENTS.
         DID THIS RUN CONTAIN A TEST TRIGGER ? I CAN'T TELL FROM HERE.
         N.B.(2) THIS RUN IS ON REFORM JDATA07 G0015V00

********
******** MESSAGE FROM GFP AT RUTHERFORD (12.00 25/03/82)
********
         I HAVE AN UNDERFLOW FROM ONE OF THE LEAD GLASS ROUTINES.
         THE ERROR MESSAGES RECEIVED ARE AS FOLLOWS :

         *** ERROR IN LGCLUS *** IEXP=20 IRUN=10462 IREC=1036
         TOO MANY CLUSTERS

        AND THEN COMES THE UNDERFLOW (IN THE SAME EVENT !) WITH A
        TRACEBACK SHOWING THE ERROR OCCURED IN THE ROUTINE
        LGCLPC <- LGANAK <- LGKOSM <- LGBRCS <- TRCK82

        THE PROGRAM RECOVERS AND EXECUTION CONTINUES. I HAVE
        ONLY SEEN THIS ERROR SO FAR FOR THE SINGLE EVENT AS ABOVE.

********
******** MESSAGE FROM PST (14.30 82/04/06)
********
         GEOFF,
         I CHANGED TRCK82(OLD VERSION =TRCK820) IN ORDER TO
         COPE WITH THE OLSSON TRIGGER AND THE CHANGES TRACK TRIGGERS.
         HOWEVER THE ACCEPT OF THE OLSSON IS STILL COMMENTED OUT,
         BECAUSE JAN OLSSON THINKS THE RATE IS STILL TOO HIGH. ONLY
         FROM MAY ON HE WANTS THESE EVENTS AVCCEPTED.
         ANYHOW THE MESSAGE 'UNKNOWN TRIGGER' SHOULD DISAPPEAR NOW.
         SO PLEASE CONTINUE.

         CONCERNING THE OTHER ERRORS: I AM STILL SLOWLY INVESTIGATING.

                                BEST WISHES, PETER.


********
******** MESSAGE FROM GFP AT RUTHERFORD (1500 07/03/82)
********
         PETER,
               I RECEIVED YOUR ABOVE MESSAGE DIRECTLY AT RUTHERFORD
         WITH NO PROBLEMS. I WILL RECOMPILE HERE AT RUTHERFORD THEN
         AND CONTINUE THE DATA REDUCTION FROM WHERE I STOPPED IT LAST
         WEEK (IE START UP AGAIN ON RUN 10618).
                           REGARDS .... GEOFF


********
******** MESSAGE FROM PET (14.00 08/04/82)
********
          GEOFF, WHEN TAPE 934044 WILL ARRIVE???

                                      ALFRED





********
******** MESSAGE FROM GFP AT RUTHERFORD (22.30 14/04/82)
********
         THERE HAS BEEN A SUDDEN BURST OF 'WRONG JETC POINTER'
         ERRORS. THIS BURST OCCURED OVER THREE SUCCESSIVE REFORM
         TAPES AND THEN DISAPPEARED AGAIN. THE RUN/EVENTS LISTED
         BY THE REDUC1 PROGRAM AS HAVING THIS ERROR ARE AS FOLLOWS :
         ----- REFORM JDATA07 G0051 -----
         RUN 10726 EVENTS 2844 2852 2969 3102 3227 3236
         RUN 10727 EVENTS  923 1078 1336 1382 1415 1592 1631 1880
         RUN 10727 EVENTS 2034 2084 2190 2972 3406 3556 3910 4460
         RUN 10727 EVENTS 4861 5019 5044 5166
         ----- REFORM JDATA07 G0052 -----
         RUN 10729 EVENTS   63  205 2782 3598
         RUN 10730 EVENTS 2116 2605 3214 4077 4478 5440 6833
         RUN 10732 EVENTS  455
         ----- REFORM JDATA07 G0053 -----
         RUN 10742 EVENTS 6234 6413

         PERHAPS SOMEONE WOULD BE INTERESTED TO LOOK AT THIS.

********
******** MESSAGE FROM GFP AT RUTHERFORD (15.30 21/04/82)
********
         ALFRED,  THE CURRENT LIST OF DATA TAPES COMPLETED AND
         SENT TO DESY WILL FOLLOW BELOW. I HAVE JUST RECEIVED THE
         LAST BATCH OF REFORM TAPES AT RUTHERFORD AND CAN RUN THESE
         HOPEFULLY BEFORE THE WEEKEND. SORRY ABOUT THE DELAY IN
         UPDATING THE REDUCONE SUMMARY BELOW -- BUT AT EXACTLY THE
         SAME TIME THAT THE DESY COMPUTER WAS SWITCHED BACK ON LAST
         WEEK THE RUTHERFORD COMPUTER WAS SWITCHED OFF FOR 5 DAYS !!!
         I WILL ALSO SHORTLY RETURN ALL OF THE TAPES USED FOR SENDING
         REFORM COPIES TO RUTHERFORD (IE 939XXX TAPES). SO IF ANYONE
         HAS SEEN ANY BIG PROBLEMS, THEY SHOULD SPEAK UP QUICKLY.
                                REGARDS .... GEOFF

********
******** RUTHERFORD REDUCONE SUMMARY (EXPT 20) (15.30 24/04/82)
********

         SUMMARY OF REDUCONE MERGE TAPES FROM RUTHERFORD (EXPT 20)
         ---------------------------------------------------------

      NOTE THAT THE DATA SET NAME ON EACH RUTHERFORD TAPE FOLLOWS THE
      FORMAT DSN=JADEPR.REDUC1.G0XXX WHERE XXX = REDUC1 GENERATION NO
      REFORM TAPES COVER JDATA06 AND JDATA07

      I=====I========I========I===================I================I
      I GEN I  TAPE  I EVENTS I MERGED "ST" TAPES I   RUN NUMBERS  I
      I=====I========I========I===================I================I
      I 408 I 934070 I  20437 I 205->210, 223     I 10055 -> 10111 I
      I-----I--------I--------I-------------------I----------------I
      I 409 I 934069 I  20552 I 211->216          I 10112 -> 10145 I
      I-----I--------I--------I-------------------I----------------I
      I 410 I 934068 I  21943 I 217->220          I 10146 -> 10169 I
      I-----I--------I--------I-------------------I----------------I
      I 411 I 934067 I  22125 I 221->222,224->225 I 10170 -> 10195 I
      I-----I--------I--------I-------------------I----------------I
      I 412 I 934063 I  23955 I 226->229,233->234 I 10196 -> 10229 I
      I     I 934062 I        I 011               I                I
      I-----I--------I--------I-------------------I----------------I
      I 413 I 934066 I  20507 I 230->232, 236     I 10230 -> 10257 I
      I-----I--------I--------I-------------------I----------------I
      I 414 I 934065 I  42144 I 237->242          I 10258 -> 10320 I
      I     I+934064 I        I                   I                I
      I-----I--------I--------I-------------------I----------------I
      I 415 I 934061 I  18867 I 243->244          I 10321 -> 10333 I
      I-----I--------I--------I-------------------I----------------I

      TAPES 934069        TAKEN BY PLANE TO DESY BY CRH ON 12/03/82
      TAPES 934070,934068,934067 BY PLANE TO DESY BY DC ON 22/03/82
      TAPES 934064-934066 TAKEN BY PLANE TO DESY BY  RM ON 25/03/82
      TAPES 934061-934063 TAKEN BY PLANE TO DESY BY GFP ON 29/03/82

      I=====I========I========I===================I================I
      I GEN I  TAPE  I EVENTS I MERGED "ST" TAPES I   RUN NUMBERS  I
      I=====I========I========I===================I================I
      I 416 I 934044 I  19139 I 245->246          I 10334 -> 10350 I
      I-----I--------I--------I-------------------I----------------I
      I 417 I 934060 I  41984 I 247->250          I 10352 -> 10384 I
      I     I+934059 I        I                   I                I
      I-----I--------I--------I-------------------I----------------I
      I 418 I 934058 I  20702 I 251->252          I 10385 -> 10409 I
      I-----I--------I--------I-------------------I----------------I
      I 419 I 934057 I  17618 I 253->254          I 10410 -> 10423 I
      I-----I--------I--------I-------------------I----------------I
      I 420 I 934056 I  20449 I 255->256, 012     I 10424 -> 10445 I
      I-----I--------I--------I-------------------I----------------I
      I 421 I 934055 I  19007 I 257->258          I 10446 -> 10457 I
      I-----I--------I--------I-------------------I----------------I
      I 422 I 934054 I  20479 I 259->260          I 10458 -> 10470 I
      I-----I--------I--------I-------------------I----------------I
      I 423 I  DESY  I  17080 I 261, 001          I 10471 -> 10481 I
      I-----I--------I--------I-------------------I----------------I
      I 424 I 934053 I  44606 I 002->006          I 10482 -> 10519 I
      I     I+934052 I        I                   I                I
      I-----I--------I--------I-------------------I----------------I
      I 425 I 934051 I  21178 I 007,013,008,014   I 10520 -> 10542 I
      I-----I--------I--------I-------------------I----------------I

      TAPES 934051-934060 DESPATCHED FROM RUTHERFORD ON 29/03/82
      TOGETHER WITH A LARGE QUANTITY OF REDUC1 COMPUTER PRINTOUT
      FLIGHT LH041 31/03/82  AWB 5969-3933

      I=====I========I========I===================I================I
      I GEN I  TAPE  I EVENTS I MERGED "ST" TAPES I   RUN NUMBERS  I
      I=====I========I========I===================I================I
      I 426 I 934050 I  16992 I 009 -> 010        I 10543 -> 10552 I
      I-----I--------I--------I-------------------I----------------I
      I 427 I 934049 I  39095 I 021, 015 -> 019   I 10553 -> 10577 I
      I     I+934048 I        I                   I                I
      I-----I--------I--------I-------------------I----------------I
      I 428 I 934047 I  17383 I 020, 022          I 10578 -> 10592 I
      I-----I--------I--------I-------------------I----------------I
      I 429 I 934046 I  17541 I 023 -> 024        I 10593 -> 10603 I
      I-----I--------I--------I-------------------I----------------I
      I 430 I 934045 I  18580 I 025 -> 026        I 10604 -> 10617 I
      I-----I--------I--------I-------------------I----------------I

      TAPES 934045-934050 BROUGHT TO DESY BY PLANE BY CRH ON 4/4/82

      I=====I========I========I===================I================I
      I GEN I  TAPE  I EVENTS I MERGED "ST" TAPES I   RUN NUMBERS  I
      I=====I========I========I===================I================I
      I 431 I 934043 I  20444 I 027->028,034->037 I 10618 -> 10639 I
      I-----I--------I--------I-------------------I----------------I
      I 432 I 934042 I  15937 I 029 -> 030        I 10640 -> 10652 I
      I-----I--------I--------I-------------------I----------------I
      I 433 I 934041 I  17195 I 031 -> 032        I 10653 -> 10663 I
      I-----I--------I--------I-------------------I----------------I
      I 434 I 934040 I  19447 I 038, 033, 042     I 10664 -> 10676 I
      I-----I--------I--------I-------------------I----------------I
      I 435 I 934039 I  19841 I 043, 039, 041, 040I 10677 -> 10694 I
      I     I        I        I 048, 044          I                I
      I-----I--------I--------I-------------------I----------------I
      I 436 I 934038 I  19670 I 045, 046, 049     I 10695 -> 10708 I
      I-----I--------I--------I-------------------I----------------I
      I 437 I 934036 I  39275 I 047, 050->053     I 10709 -> 10742 I
      I     I 934037 I        I                   I                I
      I-----I--------I--------I-------------------I----------------I
      I 438 I 934035 I  42184 I 054->058, 070     I 10743 -> 10775 I
      I     I 939622 I        I                   I                I
      I-----I--------I--------I-------------------I----------------I

      TAPES 934035-934044 DISPATCHED FROM RUTHERFORD ON 20/04/82
      TAPE  939622             "       "       "      "     "

      I=====I========I========I===================I================I
      I GEN I  TAPE  I EVENTS I MERGED "ST" TAPES I   RUN NUMBERS  I
      I=====I========I========I===================I================I
      I 439 I 939428 I  43018 I 059-063 , 077-078 I 10776 -> 10801 I
      I     I 939429 I        I                   I                I
      I-----I--------I--------I-------------------I----------------I
      I 440 I 939430 I  19662 I 064-065 , 086-087 I 10802 -> 10813 I
      I-----I--------I--------I-------------------I----------------I
      I 441 I 939431 I  42740 I 066 -> 069 , 071  I 10814 -> 10836 I
      I     I 939432 I        I                   I                I
      I-----I--------I--------I-------------------I----------------I
      I 442 I 939433 I  19026 I 072, 079, 080, 073I 10837 -> 10849 I
      I-----I--------I--------I-------------------I----------------I
      I 443 I 939434 I  39916 I 074-076, 081-082  I 10850 -> 10882 I
      I     I+939435 I        I 085               I                I
      I-----I--------I--------I-------------------I----------------I
      I 444 I 939438 I  18811 I 083 -> 084        I 10883 -> 10892 I
      I-----I--------I--------I-------------------I----------------I
      I 445 I 939436 I  17979 I 088 -> 091        I 10893 -> 10904 I
      I-----I--------I--------I-------------------I----------------I
      I 446 I 939437 I  18585 I 092 -> 093 , 101  I 10904 -> 10914 I
      I-----I--------I--------I-------------------I----------------I
      I 447 I 939439 I  18645 I 094 -> 095 , 102  I 10915 -> 10927 I
      I-----I--------I--------I-------------------I----------------I
      I 448 I 939440 I  19465 I096-097,103,106-107I 10928 -> 10949 I
      I-----I--------I--------I-------------------I----------------I
      I 449 I 939441 I  18767 I098-099,104-105,108I 10950 -> 10961 I
      I-----I--------I--------I-------------------I----------------I
      I 450 I 939442 I  14208 I 100 , 109->110    I 10961 -> 10973 I
      I-----I--------I--------I-------------------I----------------I

      TAPES 939428-939447 DISPATCHED FROM RUTHERFORD ON 27/04/82
      -----------------   THATS ALL FOLKS   --------------------

      LIST OF RUNS MANUALLY KILLED IN THE REDUC1 STAGE
      ------------------------------------------------
      10194               NO FIELD
      10202 -> 10203      TEST RUNS
      10246 -> 10250      TEST RUNS
      10351               COSMICS
      10399 -> 10400      ID PULSER RUNS
      10435 -> 10437      JUNK

********
******** MESSAGE FROM GFP AT RUTHERFORD (1800 27/05/82)
********
        TO WHOM IT MAY CONCERN,
        FROM NOW ON TWO COPIES OF THE REDUC1 JOB PRINTOUT WILL BE MADE
        WHEN REDUC1 IS RUN AT RUTHERFORD. ONE AT RUTHERFORD AND THE
        SECOND ON THE RUTHERFORD WORKSTATION PRINTER AT DESY. THIS
        LATTER PRINTOUT WILL BE COLLECTED AND ARCHIVED BY FRL. HILDEBRAN
        AND SO MAY BE VIEWED BY ANY INTERESTED PARTY AT WILL. THE JOBS
        ARRIVING AT DESY WILL HAVE THE NAME S4REDXXX WHERE XXX IS THE
        GENERATION NUMBER OF THE REFORM TAPE THAT WAS PROCESSED. NOTE
        THAT THESE PRINTOUTS WILL BE SENT TO DESY AS SOON AS WE HAVE
        CHECKED THE OUTPUT FOR ERRORS AT RUTHERFORD.






********
******** RUTHERFORD REDUCONE SUMMARY (EXPT 20) (15.00  6/07/82)
********

         SUMMARY OF REDUCONE MERGE TAPES FROM RUTHERFORD (EXPT 20)
         ---------------------------------------------------------

      DATA TAKEN IN  MAY / JUNE 1982

      NOTE THAT THE DATA SET NAME ON EACH RUTHERFORD TAPE FOLLOWS THE
      FORMAT DSN=JADEPR.REDUC1.G0XXX WHERE XXX = REDUC1 GENERATION NO
      REFORM TAPES COVER JDATA07 ONLY

      I=====I========I========I===================I================I
      I GEN I  TAPE  I EVENTS I MERGED "ST" TAPES I   RUN NUMBERS  I
      I=====I========I========I===================I================I
      I 451 I 939598 I  40534 I    111 -> 115     I 11040 -> 11080 I
      I     I+939599 I        I                   I                I
      I-----I--------I--------I-------------------I----------------I
      I 452 I 939600 I  16356 I    116 -> 117     I 11081 -> 11095 I
      I-----I--------I--------I-------------------I----------------I
      I 453 I 939601 I  21752 I    118 -> 120     I 11098 -> 11117 I
      I-----I--------I--------I-------------------I----------------I
      I 454 I 939602 I  14907 I    121 -> 122     I 11118 -> 11130 I
      I-----I--------I--------I-------------------I----------------I
      I 455 I 939603 I  21116 I    123 -> 125     I 11131 -> 11145 I
      I-----I--------I--------I-------------------I----------------I
      I 456 I 939604 I  18464 I    126 -> 128     I 11146 -> 11159 I
      I-----I--------I--------I-------------------I----------------I
      I 457 I 939605 I  20491 I   129 -> 131, 138 I 11160 -> 11184 I
      I-----I--------I--------I-------------------I----------------I

      TAPES 939598-939605 DISPATCHED FROM RUTHERFORD ON 11/06/82
              AWB 6109-3200  FLUG LH071 (DURCH FRANKFURT ?)

      I=====I========I========I===================I================I
      I GEN I  TAPE  I EVENTS I MERGED "ST" TAPES I   RUN NUMBERS  I
      I=====I========I========I===================I================I
      I 458 I 939606 I  15252 I    132 -> 134     I 11185 -> 11197 I
      I-----I--------I--------I-------------------I----------------I
      I 459 I 939607 I  12514 I    135 -> 137     I 11198 -> 11206 I
      I-----I--------I--------I-------------------I----------------I
      I 460 I 939608 I  15346 I    139 -> 141     I 11208 -> 11218 I
      I-----I--------I--------I-------------------I----------------I
      I 461 I 939609 I  15173 I    142 -> 143     I 11219 -> 11230 I
      I-----I--------I--------I-------------------I----------------I
      I 462 I 939610 I  15598 I    144 -> 145     I 11231 -> 11248 I
      I-----I--------I--------I-------------------I----------------I
      I 463 I 939611 I  21504 I    146 -> 148     I 11249 -> 11273 I
      I-----I--------I--------I-------------------I----------------I
      I 464 I 939612 I  15530 I    149 -> 150     I 11274 -> 11288 I
      I-----I--------I--------I-------------------I----------------I
      I 465 I 939613 I  16668 I    151 -> 152     I 11289 -> 11301 I
      I-----I--------I--------I-------------------I----------------I
      I 466 I 939614 I  17250 I    153 -> 154     I 11302 -> 11315 I
      I-----I--------I--------I-------------------I----------------I
      I 467 I 939615 I  15494 I    155 -> 156     I 11316 -> 11324 I
      I-----I--------I--------I-------------------I----------------I
      I 468 I 939616 I  17545 I  157, 168, 158    I 11325 -> 11340 I
      I-----I--------I--------I-------------------I----------------I

      TAPES 939606-939616 DISPATCHED FROM RUTHERFORD ON 30/06/82
              AWB 6109-5042  FLUG LH041

      I=====I========I========I===================I================I
      I GEN I  TAPE  I EVENTS I MERGED "ST" TAPES I   RUN NUMBERS  I
      I=====I========I========I===================I================I
      I 469 I  DESY  I  24172 I    159 -> 162     I 11341 -> 11356 I
      I-----I--------I--------I-------------------I----------------I
      I 470 I 939618 I  44562 I    162 -> 166     I 11357 -> 11411 I
      I     I+939619 I        I                   I                I
      I-----I--------I--------I-------------------I----------------I
      I 471 I 939620 I  30992 I 167, 169 -> 172   I 11412 -> 11435 I
      I     I+939621 I        I                   I                I
      I-----I--------I--------I-------------------I----------------I
      I 472 I 939622 I  20347 I    173 -> 175     I 11436 -> 11462 I
      I-----I--------I--------I-------------------I----------------I

          BAENDE 939617-939622 NACH DESY AM 08/07/82 GESHICKT
                 939423-939427 (LEERE)
                 939448-939466 (LEERE)
               I WILL ATTEND TO THESE WHEN I ARRIVE AT DESY


         LIST OF RUNS MANUALLY KILLED IN THE REDUC1 STAGE
         ------------------------------------------------

      11037               NOT IN LOG BOOK, THEREFORE AXED
      11038 -> 11039      JUNK
      11048               JUNK
      11067               JUNK
      11071               JUNK
      11090               JUNK
      11096 -> 11097      JUNK
      11207               ABOUT 1000 EVENTS WITH NO JETC
      11247               ONLY 2 EVENTS
      11264               JUNK
      11266               JUNK
      11373 -> 11388      CRATE 504 PROBLEMS (SHORT RUNS)
      11399 -> 11408      LG TEST RUNS
      11447               JUNK
      11451               JUNK
      11463 -> 11470      LG TEST RUNS
      11471               HV ERRORS
      11472               T2 INPUT MISSING


********
******** MESSAGE FROM GFP ON JULY 6TH 1982
********
        WHEN WILL THE NEXT SET OF DATA TAPES BE SENT TO RUTHERFORD ?
        NONE HAVE BEEN SENT SINCE JUNE 22ND.

********
******** MESSAGE FROM GFP ON 19/08/82
********
        WHEN WILL THE REMAINING DATA BE SENT TO RUTHERFORD ?? I AM
        NOW WAITING FOR THIS DATA TO BE SENT !! THE GENERATIONS STILL
        NEEDED ARE 061, 066, 073, 089 AND FINALLY THE LAST RUNS OF THE
        RUN PERIOD 12506 TO 12518. I CAN DO NO MORE AT RUTHERFORD
        UNTIL THESE DATA ARRIVE.


********
******** MESSAGE FROM GFP ON 20/08/82
********
        1) I HAVE JUST NOTICED THE MESSAGE IN 'JADEPR.TEXT(REDUCONE)'
           ABOUT RUN 11908 HAVING THE B=0 ERROR. I STILL HAVE THE RAW
           DATA AT RUTHERFORD SO I WILL RE-RUN GENERATION 492 HERE. IN
           FACT I HAVE ALREADY SUBMITTED THE JOBS !
                DOES THIS ERROR OCCUR ON ANY OTHER RUNS ?? IT SEEMS
           MIGHTY SUSPICIOUS TO ME THAT IT HAS APPEARED ONLY ON THIS
           RUN AND THEN AGAIN LATER ON RUNS 12251 TO 12270. IS THIS
           UNDERSTOOD? PLEASE LET ME KNOW AS SOON AS POSSIBLE IF I SHOUL
           RE-RUN ANY MORE DATA AT RUTHERFORD WHILE I STILL HAVE THE RAW
           DATA AND STACKS OF CPU TIME. DONT WORRY ABOUT RUNS 12251
           TO 12270 BY THE WAY, I RE-LINKED THE REDUC1 PROGRAM HERE
           WITH THE NEW KALIBR ETC BEFORE THESE JOBS RAN.
        2) WHEN I RAN THE REDUC1 PROGRAM ON REFORM GEN. 088 I GOT AN
           INPUT BANK ERROR 3 ON RUN 12486 EVENT 2591. DOES ANYONE KNOW
           WHAT THIS MEANS AND WHAT I SHOULD DO ABOUT IT ?? IT ONLY
           OCCURRED ON THIS ONE EVENT (WHICH HAS BEEN THROWN OUT).
                I HAVE SENT THE REDUC1 OUTPUT FOR THIS JOB TO DESY SO
           THAT YOU CAN LOOK AT IT IF YOU WISH. THE JOB NAME IS S4RED088
           AND IT WILL APPEAR ON THE PRINTER IN THE RUTHERFORD LINK ROOM
        3) PLEASE NOTE THE EARLIER MESSAGE ABOUT DATA NOT SENT TO RUTH
           YET.I HAVE COME TO A HALT ON REDUCTION BECAUSE OF THIS. I HAV
           ASKED FOR REFORM GEN 073 AGAIN BECAUSE THE TAPE I RECEIVED
           WAS CHEWED BY A RUTHERFORD TAPE DRIVE THE FIRST TIME I TRIED
           TO READ IT !! (IN CASE ANYONE WAS WONDERING).

********
******** MESSAGE FROM A.PETERSEN ON 30/08/82
        I DON'T KNOW WHAT WRONG WITH THE OLD TAPE OF 061,066,089,
        BUT I COPIED THEM AGAIN FOR YOU. THEY ARE ON FOLLOWING TAPES:
        RUTH061    939456
        RUTH066    939457
        RUTH073    939455
        RUTH089    939458
        THE TAPE 939454 WAS TAKEN WITH BY ROBERT H. THE OTHERS WILL
        BE SENT BY AIR FLIGHT.

                                   ALFRED






********
******** RUTHERFORD REDUCONE SUMMARY (EXPT 20) (17.00  05/09/82)
********

         SUMMARY OF REDUCONE MERGE TAPES FROM RUTHERFORD (EXPT 20)
         ---------------------------------------------------------

        DATA TAKEN IN  JUNE / JULY / AUGUST 1982.  (RUN PERIOD 3)
        THE REDUC1 PROGRAM WAS RE-LINKED FOR THIS DATA (ON 5/7/82)

      NOTE THAT THE DATA SET NAME ON EACH RUTHERFORD TAPE FOLLOWS THE
      FORMAT DSN=JADEPR.REDUC1.G0XXX WHERE XXX = REDUC1 GENERATION NR.
      REFORM TAPES COVER JDATA07 ONLY


      I=====I========I========I===================I================I
      I GEN I  TAPE  I EVENTS I MERGED "ST" TAPES I   RUN NUMBERS  I
      I=====I========I========I===================I================I
      I 473 I 939467 I  40374 I    176 -> 180     I 11473 -> 11500 I
      I     I 939468 I        I                   I                I
      I-----I--------I--------I-------------------I----------------I
      I 474 I 939469 I  21763 I    181 -> 183     I 11501 -> 11516 I
      I-----I--------I--------I-------------------I----------------I
      I 475 I 939470 I  23127 I    184 -> 186     I 11517 -> 11538 I
      I-----I--------I--------I-------------------I----------------I
      I 476 I 939471 I  18017 I    187 -> 189     I 11539 -> 11554 I
      I-----I--------I--------I-------------------I----------------I

        TAPES 939467 -> 939476 BROUGHT TO DESY BY GFP ON 12/07/82
        ***  ALLE BAENDER OBEN SIND SCHON BEI DESY KOPIERT  ***

        *** WICHTIG *** DAS CLIST "(REDUC1) COPYRUT" KANN DEN KORREKT
        JCL FUER ZWEI RUTHERFORD BAENDER JETZT MACHEN. SIE BEKOMMEN
        EINEN 4-MINUTEN KLASSE "K" JOB FUER ZWEI BAENDER UND EINEN
        2-MINUTEN KLASSE "A" JOB FUER EINEN BAND.

      I=====I========I========I===================I================I
      I GEN I  TAPE  I EVENTS I MERGED "ST" TAPES I   RUN NUMBERS  I
      I=====I========I========I===================I================I
      I 477 I 939477 I  20223 I    190 -> 192     I 11555 -> 11571 I
      I-----I--------I--------I-------------------I----------------I
      I 478 I 939478 I  44831 I    193 -> 197     I 11572 -> 11604 I
      I     I+939479 I        I                   I                I
      I-----I--------I--------I-------------------I----------------I
      I 479 I 939480 I  43439 I    198 -> 202     I 11605 -> 11634 I
      I     I+939481 I        I                   I                I
      I-----I--------I--------I-------------------I----------------I
      I 480 I 939482 I  40208 I    203 -> 207     I 11635 -> 11672 I
      I     I+939483 I        I                   I                I
      I-----I--------I--------I-------------------I----------------I
      I 481 I 939469 I  19707 I    208 -> 209     I 11673 -> 11690 I
      I-----I--------I--------I-------------------I----------------I
      I 482 I 939485 I  20706 I    210 -> 212     I 11691 -> 11706 I
      I-----I--------I--------I-------------------I----------------I
      I 483 I 939486 I  21047 I    213 -> 215     I 11707 -> 11725 I
      I-----I--------I--------I-------------------I----------------I
      I 484 I 939487 I  20154 I 216 -> 217 , 230  I 11726 -> 11754 I
      I-----I--------I--------I-------------------I----------------I
      I 485 I 939488 I  18215 I    218 -> 219     I 11755 -> 11766 I
      I-----I--------I--------I-------------------I----------------I
      I 486 I 939489 I  21161 I    220 -> 222     I 11767 -> 11781 I
      I-----I--------I--------I-------------------I----------------I
      I 487 I 939490 I  20442 I    223 -> 225     I 11782 -> 11803 I
      I-----I--------I--------I-------------------I----------------I
      I 488 I 939491 I  20653 I 226 -> 228 , 231  I 11804 -> 11822 I
      I-----I--------I--------I-------------------I----------------I
      I 489 I 939492 I  20790 I 229 , 236 -> 237  I 11823 -> 11841 I
      I-----I--------I--------I-------------------I----------------I
      I 490 I 939493 I  20629 I    238 -> 240     I 11842 -> 11855 I
      I-----I--------I--------I-------------------I----------------I
      I 491 I 939494 I  20609 I    241 -> 244     I 11856 -> 11893 I
      I-----I--------I--------I-------------------I----------------I
      I 492 I 939495 I  19928 I    245 -> 247     I 11894 -> 11941 I
      I-----I--------I--------I-------------------I----------------I
      I 493 I 939496 I  20628 I    248 -> 250     I 11942 -> 11966 I
      I-----I--------I--------I-------------------I----------------I
      I 494 I 939497 I  22785 I    001 -> 004     I 11967 -> 11989 I
      I-----I--------I--------I-------------------I----------------I
      I 495 I 939498 I  21335 I    004 -> 007     I 11990 -> 12017 I
      I-----I--------I--------I-------------------I----------------I

          BAENDER 939477 -> 939516 NACH DESY AM 06/08/82 GESHICKT
                                   FLUG LH 041.     AWB 6109-5462
                  939499 -> 939516 SIND LEERE

      I=====I========I========I===================I================I
      I GEN I  TAPE  I EVENTS I MERGED "ST" TAPES I   RUN NUMBERS  I
      I=====I========I========I===================I================I
      I 481 I 939469 I  19707 I    208 -> 209     I 11673 -> 11690 I
      I-----I--------I--------I-------------------I----------------I
      I 496 I 939517 I  21565 I    008 -> 010     I 12018 -> 12046 I
      I-----I--------I--------I-------------------I----------------I
      I 497 I 939518 I  22781 I    011 -> 014     I 12047 -> 12077 I
      I-----I--------I--------I-------------------I----------------I
      I 498 I 939519 I  23122 I    014 -> 017     I 12078 -> 12104 I
      I-----I--------I--------I-------------------I----------------I
      I 499 I 939520 I  22928 I    018 -> 021     I 12105 -> 12127 I
      I-----I--------I--------I-------------------I----------------I
      I 500 I 939521 I  23315 I    021 -> 025     I 12128 -> 12148 I
      I-----I--------I--------I-------------------I----------------I
      I 501 I 939522 I  22257 I    025 -> 028     I 12149 -> 12167 I
      I-----I--------I--------I-------------------I----------------I
      I 502 I 939523 I  21456 I 074, 025 -> 032   I 12168 -> 12196 I
      I-----I--------I--------I-------------------I----------------I
      I 503 I 939524 I  21901 I    032 -> 035     I 12197 -> 12216 I
      I-----I--------I--------I-------------------I----------------I
      I 504 I 939525 I  22360 I    036 -> 039     I 12217 -> 12240 I
      I-----I--------I--------I-------------------I----------------I
      I 505 I 939526 I  21587 I 075, 039 -> 042   I 12241 -> 12261 I
      I-----I--------I--------I-------------------I----------------I
      I 506 I 939527 I  21192 I    043 -> 046     I 12262 -> 12284 I
      I-----I--------I--------I-------------------I----------------I
      I 507 I 939528 I  20888 I    046 -> 050     I 12285 -> 12300 I
      I-----I--------I--------I-------------------I----------------I
      I 508 I 939529 I  20496 I    050 -> 053     I 12301 -> 12318 I
      I-----I--------I--------I-------------------I----------------I
      I 509 I 939530 I  21389 I    054 -> 057     I 12319 -> 12340 I
      I-----I--------I--------I-------------------I----------------I

      BAENDER 939469, 939517-> 939545 NACH DESY AM 25/08/82 GESHICKT
                                      FLUG LH ....     AWB ....-....
              939531 -> 939545 SIND LEERE

      I=====I========I========I===================I================I
      I GEN I  TAPE  I EVENTS I MERGED "ST" TAPES I   RUN NUMBERS  I
      I=====I========I========I===================I================I
      I 510 I ...... I  ..... I    ... -> ...     I ..... -> ..... I
      I=====I========I========I===================I================I
      I GEN I  TAPE  I EVENTS I MERGED "ST" TAPES I   RUN NUMBERS  I
      I=====I========I========I===================I================I
      I 510 I 934092 I  20817 I    057 -> 061     I 12341 -> 12359 I
      I-----I--------I--------I-------------------I----------------I
      I 511 I 934091 I  21989 I    061 -> 065     I 12360 -> 12377 I
      I-----I--------I--------I-------------------I----------------I
      I 512 I 934090 I  21189 I 076->077, 065->068I 12378 -> 12398 I
      I-----I--------I--------I-------------------I----------------I
      I 513 I 934089 I  19690 I    068 -> 071     I 12399 -> 12412 I
      I-----I--------I--------I-------------------I----------------I
      I 514 I 934088 I  20319 I 071->073, 078->081I 12416 -> 12441 I
      I-----I--------I--------I-------------------I----------------I
      I 515 I 934087 I   3120 I    081 -> 082     I 12442 -> 12445 I
      I-----I--------I--------I-------------------I----------------I
      I 516 I 934086 I  20277 I    ALL 8.51 GEV   I 12446 -> 12463 I
      I-----I--------I--------I-------------------I----------------I
      I 517 I 934085 I  21635 I    084 -> 087     I 12464 -> 12481 I
      I-----I--------I--------I-------------------I----------------I
      I 518 I 934084 I  20554 I    088 -> 091     I 12482 -> 12508 I
      I-----I--------I--------I-------------------I----------------I
      I 519 I 934083 I   3055 I    091            I 12509 -> 12518 I
      I-----I--------I--------I-------------------I----------------I

      BAENDER 934092 -> 934083 WURDEN NACH DESY AM 16/09/82 GESHICKT
                                      FLUG LH ....     AWB ....-....
         LIST OF RUNS MANUALLY KILLED IN THE REDUC1 STAGE
         ------------------------------------------------

      11471               HV ERRORS
      11472               T2 INPUT MISSING
      11579 -> 11581      TEST RUNS
      11582               LG PROBLEM
      11583               GAP IN LG END CAP
      11594               JUNK
      11610               JETC LOST AT SOME TIME DURING RUN
      11664               JUNK
      11668 -> 11669      JUNK
      11697               COSMIX
      11783               NO "TAG" OR "LUMI" TRIGGERS
      11815 -> 11816      JUNK
      11856               TEST RUN
      11870               JUNK
      11881 -> 11891      BIG DL8 PROBLEM -- VERY FEW ID TRACKS FOUND
      11898               MIPROC OFF - SHORT RUN
      11958               JUNK
      12023 -> 12029      DL8 PROBLEMS
      12186               DL8 PROBLEMS
      12188 -> 12189      DL8 PROBLEMS
      12190               LEAD GLASS PROBLEM
      12271               LEAD GLASS TEST
      12276 -> 12277      JUNK RUNS
      12291               JUNK RUN
      12303               JUNK RUN
      12313               JUNK RUN
      12324 -> 12326      COSMIC RUNS
      12338               JUNK RUN
      12385               JUNK
      12388 -> 12389      JUNK
      12392               JUNK
      12402               JUNK
      12459               TEST RUN
      12467               JUNK
      12469               JUNK
      12480               JUNK
      12488               JUNK
      12497               JUNK
      12507               JUNK

********
******** MESSAGE FROM PST (14.00 23/09/82)
********
         GEOFF, I CHANGED USRED82 (OLD VERSION = USRED820) IN ORDER TO
         COPE WITH ONLY KALWRK0 AS THE CALIBRATION FILE. PLEASE TRY
         A TEST RUN.
         DO YOU KNOW WHY I GET AN OC4 FROM HBOOK WHEN I TRY TO RUN
         REDUC1 HERE AT DESY? DO YOU ALSO GET IT AT RL?
         IS THERE STIL A LINK JOB FOR REDUC1 AND WHICH ONE IS IT?
         WHICH IS THE JCL OF REDUC1 PRODUCTION?
                             BEST WISHES, PETER.
 ********
 ******** MESSAGE FROM GFP (1600 27/09/82)
 ********
         PETER,
         A) I REMEMBER THIS PROBLEM WITH HBOOK HAPPENING ONCE BEFORE.
            REDUC1 ALWAYS RUNS USING A 'LOOK-ALIKE' VERSION OF HBOOK
            CALLED GBOOK. THIS WAS (IS!) NECESSARY IN ORDER TO GET THE
            CORE SIZE BELOW 560K AT RUTHERFORD SINCE THE TURNAROUND FOR
            JOBS ABOVE THIS THRESHOLD HAS ALWAYS BEEN MUCH POORER. FOR
            CONSISTENCY, THE SAME PACKAGE IS USED AT DESY. THIS IS FINE
            FOR PRODUCTION BUT NOT, I AGREE, FOR TEST JOBS. IN ORDER TO
            CONVERT BACK TO HBOOK, YOU NEED ONLY DO THE FOLLOWING :

            1) DE-COMMENT THE BLANK COMMON IN USRED82 AND DIMENSION
               ACCORDING TO YOUR REQUIREMENTS.
            2) COMMENT OUT COMMON/CGBOOK/PLOT(116,13) IN USRED82. (THE
               13 IS THE NUMBER OF 1-D PLOTS TO BE USED IN GBOOK)
            3) DO NOT INCLUDE GBOOK IN THE LKED STEP.
            4) MAKE SURE THAT THE HBOOK LIBRARY IS LINKED IN.

         B) THE JCL FOR ALL REDUC1 JOBS AT DESY IS CONTROLLED BY THE
            CLIST "F11PEA.JDCLIST(REDUC1)". THIS IS SIMPLY INVOKED
            BY TYPING (REDUC1) WHILST IN NEWLIB ON JADEPR.JADESR. SHOULD
            ANY CHANGES TO THIS CLIST BE REQUIRED AT ANY TIME, JADEPR
            IS AUTHORISED TO MAKE THEM. PERHAPS YOU WOULD CHANGE IT NOW
            TO COPE WITH JUST ONE CALIBRATION FILE - IE DELETE THE
            FT21F001 CARD. I HAVE ALREADY CHANGED THE REFORM CLIST.

         C) THANKS FOR CHANGING USRED82 TO COPE WITH A SINGLE CALIBRATIO
            FILE. I WILL TEST IT HERE SHORTLY.

         D) FINALLY, I WOULD LIKE TO USE YOUR ROUTINE JREKAL AT RAL.
            WHERE CAN I OBTAIN A COPY ?? I NEED THE SOURCE IN ORDER
            TO TRANSFER THE ROUTINE TO RUTHERFORD.

            REGARDS,     GEOFF


********
******** MESSAGE FROM A.PETERSEN (11.00 7/02/83)
********
         WE GOT THE RUTHERFORD TAPE BACK, BUT WE DON'T KNOW ON WHICH
         TAPE ARE THE REDUCED DATA SETS. CAN SOMEONE SEND US THE
         SUMMARY OF REDUCONE MERGE TAPES FROM RUTHERFORD?
                             BEST THANKS, ALFRED

********
******** From J.B.Whittaker, R.A.L.  8/2/83   14.20 G.M.T.
********
         The merged tapes are as follows:

 TAPE    DSN                   RUNS
 ----    ---                   ----
 939423  JADEPR.REDUC1.G0520   12556 - 12584
 939424  JADEPR.REDUC1.G0521   12585 - 12608
 939425  JADEPR.REDUC1.G0522   12609 - 12647
 939426  JADEPR.REDUC1.G0523   12648 - 12671
 939427  JADEPR.REDUC1.G0524   12672 - 12709
 939428  JADEPR.REDUC1.G0525   12710 - 12757
 939429  JADEPR.REDUC1.G0526   12758 - 12799
 939430  JADEPR.REDUC1.G0527   12800 - 12828
 939431  JADEPR.REDUC1.G0528   12829 - 12854
 939443  JADEPR.REDUC1.G0529   12855 - 12889
 939432  JADEPR.REDUC1.G0530   12890 - 12942
 939433  JADEPR.REDUC1.G0531   12943 - 12948

