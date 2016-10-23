C   13/06/85 506131758  MEMBER NAME  TEMP     (S)           FORTRAN
   1 $INFO
   2 ##STATUS
   3 #JOB
   4 #START
   5 #TAGDOC
   6 #TAGDOC2
   7 ACOL
   8 AFTP83
   9 AMCTAG
  10 ANAL79
 110 CALAD
 111 CLSPS
 112 CLUSFN
 113 COMFIL
 114 CONVER
 115 CTAGGO
 116 CUTPED
 117 CUTP81
 118 CWKTAG2
 119 DATE
 120 FFITYP
 121 FHTOS
 122 FHTOS0
 123 FITAG
 124 FRTYPE
 125 FSTOH
 126 GGBLCK
 127 GGCCTL
 128 GGCLPC
 129 GGCLSD
 130 GGCLUS
 131 GGFAC1
 132 GGFAC2
 133 GGFAC3
 134 GGFAC4
 135 GGFAC5
 136 GGFAC6
 137 GGRESQ
 138 GGSORT
 139 GGSRTH
 140 GSCRUB
 141 HDDUMP
 142 INGAM1
 143 LISTGN
 144 LUCHEK
 145 LUMONS
 146 OUTGAW
 147 PEDFIX
 148 RCOORD
 149 REPACK
 150 SORT
 151 SORT1
 152 SORT2
 153 SRSUM
 154 SUBNEW
 155 SUBOLD
 156 TAGADC
 157 TAGALL
 158 TAGAN
 159 TAGCHK
 160 TAGCLS
 161 TAGCOL
 162 TAGDAT
 163 TAGDIR
 164 TAGFIT
 165 TAGFTY
 166 TAGF82
 167 TAGGTP
 168 TAGH2S
 169 TAGINT
 170 TAGKAL
 171 TAGMRK
 172 TAGNEB
 173 TAGNOTE
 174 TAGNOTE2
 175 TAGPD1
 176 TAGPD2
 177 TAGPED
 178 TAGPHI
 179 TAGPOS
 180 TAGPS1
 181 TAGPS2
 182 TAGRAW
 183 TAGRTY
 184 TAGSET
 185 TAGSR1
 186 TAGSR2
 187 TAGSTO
 188 TAGSUM
 189 TAGS2H
 190 TGDCOS
 191 TRACKS
 192 UMCPAK










-------------- LINECOMMANDS IN DIRECTORY DISPLAY -----------------------
 LINECOMMANDS ARE TO BE STARTED IN THE FIRST COLUMN
 CH    : CHANGE MEMBER              PR : PRINT MEMBER(S)
 SUBH  : SUBMIT MEMBER(S) WITH HOLD SUB: SUBMIT MEMBER(S)
 SC    : SCRATCH MEMBER(S)          RT : RESET MEMBER(S)
 COLOAD: COMPILE MEMBER WITH LOAD   CO : COMPILE MEMBER
 LIST  : LIST MEMBER                DUP: DUPLICATE MEMBER
 L     : POSITION LISTING           SCL: SCRATCH LOAD MEMBER
 GO    : EXECUTE LOAD MODULE        EX : EXECUTE CLIST
 C,CC  : COPY SINGLE LINES OR A RANGE OF LINES TO A MEMBER
 RENAME: RENAME PS AND/OR PL MEMBERS BY OVERWRITING THE NAMES
         IN THE CORRESPONDING PS/PL MEMBERS COLUMNS
 S     : SAVE MEMBER(S)

 YOU MAY ENTER SC,RT,S,RENAME,SCL,SUB,SUBH COMMANDS IN
 ARBITRARY NUMBER, BUT ONLY ONE OF CH,LIST,CO,GO,EX COMMANDS.
 IF YOU USE THE DUP COMMAND, OVERWRITE THE NAME BY A NEW NAME
------------------------------------------------------------------------
    18/03/84 403222224  MEMBER NAME  #TAGDOC  (S)           FORTRAN

THIS IS SOME RATHER HAPHAZARD DOCUMENTATION FOR THE ROUTINES USED
FOR ANALYSING TAGGING INFORMATION FROM 1981 ONWARDS AS WRITTEN BY
A.J.FINCH 21/3/84

ADDITIONAL NOTES CONCERNING 'TAGAN'-
====================================

 SOME DEBUGGING INFO CAN BE TURNED ON BY THE FOLLOWING PROCEDURE

      COMMON/CONTRL/JWRITE
      JWRITE=1      -> TURNS  ON INFO
      JWRITE=0      -> TURNS  OFF INFO


      SUBROUTINE TAGINT(*)
      ====================



  THIS IS THE INITIALISATION ROUTINE THAT SHOULD BE CALLED ONCE,BEFORE
  ANALYSING ANY EVENTS  USING  THE  ROUTINES  IN  THIS  PACKAGE.TO  BE   0000800
  PRECISE IT MUST CALLED ONCE PER EVENT TO SET UP THE CWORK COMMON       0000900
                                                                         0001000

  IT DOES A RETURN 1 IF TAGMRK DOES A RETURN 1 ,WHICH IT DOES IF 'HEAD'
   DOESNT EXIST




      SUBROUTINE COMFIL(IWRITE,*)
     ============================

    THIS SUBROUTINE TAKES THE DATA FROM HDATA BLOCK
    'ATAG' AND PUTS THE ADC CONTENTS IN THE ARRAY CATAG
                         IN ORDER OF ADC ADDRESS

     IWRITE IS A FLAG TO TELL WHETHER TO PRINT THE BANK 'CATAG' AFTER
     IT HAS BEEN FILLED.

    NOTE:- IT ALSO MULTIPLIES CHANNEL NUMBER TIMES A FACTOR TO MAKE IT
    APPROXIMATELY EQUAL TO ENERGY IN MEV



    SUBROUTINES PEDFIX,PEDFX2,PEDFX3
    ================================

 PEDFIX IS CONTROL ROUTINE WHICH CALLS ONE OF THE OTHER TWO TO DO THE
        WORK - ALTHOUGH THEY ARE SIMILAR IN CONCEPT IT AS TOO MUCH
            WORK TO COMBINE THEM INTO ONE ROUTINE

  ROUTINES  TO DO PEDESTAL SUBTRACTION

 PEDFX3   - 1983 ..ONLY
 ======
      ROUTINE TO ATTEMPT OFFLINE SUBTRACTION OF PEDESTALS
      FOR EACH 'SUPERBLOCK' OF 3 LEAD SCINTILLATOR ELEMENTS IT
    CHECKS THAT
     NO ONE CHANNEL IS GREATER THAT 500 MEV,IF THIS IS THE CASE ALL
   THREE BLOCKS ARE IGNORED , OTHERWISE THE VALUES IN THE BLOCKS ARE
    USED TO ESTIMATE A MEAN ADC PEDESTAL , WHICH IS THEN SUBTRACTED
   FROM ALL BLOCKS.


 PEDFX2  - 1981/2 ONLY
 ======
        SAME PROCEDURE AS PEDFX3 BUT 'SUPERBLOCKS' REPLACE 'CAKESLICES'


MEANING OF TERM 'SUPERBLOCK' USED IN COMMENTS OF THIS PROGRAM

  1/4 OF A TAGGING SYSTEM =

                                ___________
                                I    I    I
                      __________I    I    I
                      I    I    L____L____I
                      I    I    I    I    I
                      L____L____I    I    I
                      I    I    L____L____I
                      I    I    I
                   ___L____L____I
                   I    I    I
                   I    I    I
                   L____L____I
                   I    I    I
                   I    I    I
                   L____L____I__
                      I    I    L
                      I    I    I__________
                      L____L____I    I    I
                      I    I    L    I    I
                      I    I    I____L____I
                      L____L____I    I    I
                                I    I    I
                                L____L____I

    'SUPERBLOCK' =

                   ___________
                   I    I    I
                   I    I    I
                   L____L____I
                   I    I    I
                   I    I    I
                   L____L____I




 SUBROUTINE SRSUM(JPART,SUM,*)
 =============================


     THIS SUBROUTINES SUMS ALL  ADC DATA IN ARRAY CATAG ONE END OR THE
     OTHER TO GIVE THE VALUE OF SUM.

  JPART - INPUT - SIGN DETERMINES WHICH END TAGGER IS ANALYSED
                   JPART > 0 => +Z
                   JPART < 0 => -Z

   SUM - OUTPUT - SUM OF ENERGY

  RETURN 1 IF SUM IS A  CRAZY VALUE



 SUBROUTINE CALAD(IWRITE)
 ========================

    THIS SUBROUTINE   CALIBRATES ADC'S USING FACTORS IN
   COMMON CALIBR

  INPUT - IWRITE - IF THIS IS EQUAL TO ONE THE ROUTINE
                   WRITES OUT SOME DEBUGGING INFORMATION


 FUNCTION FSTOH(I)
 =================

  CONVERT SOFTWARE ADDRESS TO HARDWARE ADDRESS
  SEE ALSO FHTOS  - INPUT I = SOFTWARE ADDRESS TO BE CONVERTED



 SUBROUTINE FHTOS(ISAD,IHAD,*)
 =============================


  THIS ROUTINE CONVERTS HARDWARE ADDRESSES TO SOFTWARE ADDRESSES

  ISAD - OUTPUT - SOFTWARE ADDRESS
  IHAD - INPUT  - HARDWARE ADDRESS

  RETURN 1 IF IT CAN'T DO IT ( ILLEGAL ADDRESS SUPPLIED )


    THE PATTERN FOR 1983 ONWARDS IS -

    HARDWARE ADDRESS                              SOFTWARE ADDRESS

         0 - 3             NOT USED
         4 - 7           INNER RING - Z               1 - 4
         8 - 11            "      "                   5 - 8
        12 - 15            NOT USED
        16 - 19             "   "
        20 - 23           MIDDLE RING - Z             9- 12
        24 - 27              "  "                     13-16
        28 - 31            NOT USED
        32 - 35             "   '



     FOR 1981/2 SOFTARE ADDRESS = HARDWARE ADDRESS + 1


    18/03/84 503161535  MEMBER NAME  #TAGDOC2 (S)           FORTRAN

THIS IS SOME RATHER HAPHAZARD DOCUMENTATION FOR THE ROUTINES USED
FOR ANALYSING TAGGING INFORMATION FROM 1981 ONWARDS AS WRITTEN BY
A.J.FINCH 21/3/84

ADDITIONAL NOTES CONCERNING 'TAGAN'-
====================================

 SOME DEBUGGING INFO CAN BE TURNED ON BY THE FOLLOWING PROCEDURE

      COMMON/CONTRL/JWRITE
      JWRITE=1      -> TURNS  ON INFO
      JWRITE=0      -> TURNS  OFF INFO


      SUBROUTINE TAGINT(*)
      ====================



  THIS IS THE INITIALISATION ROUTINE THAT SHOULD BE CALLED ONCE,BEFORE
  ANALYSING ANY EVENTS  USING  THE  ROUTINES  IN  THIS  PACKAGE.TO  BE   0000800
  PRECISE IT MUST CALLED ONCE PER EVENT TO SET UP THE CWORK COMMON       0000900
                                                                         0001000

  IT DOES A RETURN 1 IF TAGMRK DOES A RETURN 1 ,WHICH IT DOES IF 'HEAD'
   DOESNT EXIST




      SUBROUTINE TAGADC(IWRITE,*)
     ============================

    THIS SUBROUTINE TAKES THE DATA FROM HDATA BLOCK
    'ATAG' AND PUTS THE ADC CONTENTS IN THE ARRAY CATAG
                         IN ORDER OF ADC ADDRESS

     IWRITE IS A FLAG TO TELL WHETHER TO PRINT THE BANK 'CATAG' AFTER
     IT HAS BEEN FILLED.

    NOTE:- IT ALSO MULTIPLIES CHANNEL NUMBER TIMES A FACTOR TO MAKE IT
    APPROXIMATELY EQUAL TO ENERGY IN MEV



    SUBROUTINES TAGPED,TAGPD1,TAGPD2
    ================================

 TAGPED IS CONTROL ROUTINE WHICH CALLS ONE OF THE OTHER TWO TO DO THE
        WORK - ALTHOUGH THEY ARE SIMILAR IN CONCEPT IT AS TOO MUCH
            WORK TO COMBINE THEM INTO ONE ROUTINE

  ROUTINES  TO DO PEDESTAL SUBTRACTION

 TAGPD2   - 1983 ..ONLY
 ======
      ROUTINE TO ATTEMPT OFFLINE SUBTRACTION OF PEDESTALS
      FOR EACH 'SUPERBLOCK' OF 3 LEAD SCINTILLATOR ELEMENTS IT
    CHECKS THAT
     NO ONE CHANNEL IS GREATER THAT 500 MEV,IF THIS IS THE CASE ALL
   THREE BLOCKS ARE IGNORED , OTHERWISE THE VALUES IN THE BLOCKS ARE
    USED TO ESTIMATE A MEAN ADC PEDESTAL , WHICH IS THEN SUBTRACTED
   FROM ALL BLOCKS.


 TAGPD1  - 1981/2 ONLY
 ======
        SAME PROCEDURE AS TAGPD2 BUT 'SUPERBLOCKS' REPLACE 'CAKESLICES'


MEANING OF TERM 'SUPERBLOCK' USED IN COMMENTS OF THIS PROGRAM

  1/4 OF A TAGGING SYSTEM =

                                ___________
                                I    I    I
                      __________I    I    I
                      I    I    L____L____I
                      I    I    I    I    I
                      L____L____I    I    I
                      I    I    L____L____I
                      I    I    I
                   ___L____L____I
                   I    I    I
                   I    I    I
                   L____L____I
                   I    I    I
                   I    I    I
                   L____L____I__
                      I    I    L
                      I    I    I__________
                      L____L____I    I    I
                      I    I    L    I    I
                      I    I    I____L____I
                      L____L____I    I    I
                                I    I    I
                                L____L____I

    'SUPERBLOCK' =

                   ___________
                   I    I    I
                   I    I    I
                   L____L____I
                   I    I    I
                   I    I    I
                   L____L____I




 SUBROUTINE TAGSUM(JPART,SUM,*)
 =============================


     THIS SUBROUTINES SUMS ALL  ADC DATA IN ARRAY CATAG ONE END OR THE
     OTHER TO GIVE THE VALUE OF SUM.

  JPART - INPUT - SIGN DETERMINES WHICH END TAGGER IS ANALYSED
                   JPART > 0 => +Z
                   JPART < 0 => -Z

   SUM - OUTPUT - SUM OF ENERGY

  RETURN 1 IF SUM IS A  CRAZY VALUE



 SUBROUTINE TAGKAL(IWRITE)
 ========================

    THIS SUBROUTINE   CALIBRATES ADC'S USING FACTORS IN
   COMMON CALIBR

  INPUT - IWRITE - IF THIS IS EQUAL TO ONE THE ROUTINE
                   WRITES OUT SOME DEBUGGING INFORMATION


 FUNCTION TAGS2H(I)
 =================

  CONVERT SOFTWARE ADDRESS TO HARDWARE ADDRESS
  SEE ALSO TAGH2S  - INPUT I = SOFTWARE ADDRESS TO BE CONVERTED



 SUBROUTINE TAGH2S(ISAD,IHAD,*)
 =============================


  THIS ROUTINE CONVERTS HARDWARE ADDRESSES TO SOFTWARE ADDRESSES

  ISAD - OUTPUT - SOFTWARE ADDRESS
  IHAD - INPUT  - HARDWARE ADDRESS

  RETURN 1 IF IT CAN'T DO IT ( ILLEGAL ADDRESS SUPPLIED )


    THE PATTERN FOR 1983 ONWARDS IS -

    HARDWARE ADDRESS                              SOFTWARE ADDRESS

         0 - 3             NOT USED
         4 - 7           INNER RING - Z               1 - 4
         8 - 11            "      "                   5 - 8
        12 - 15            NOT USED
        16 - 19             "   "
        20 - 23           MIDDLE RING - Z             9- 12
        24 - 27              "  "                     13-16
        28 - 31            NOT USED
        32 - 35             "   '



     FOR 1981/2 SOFTARE ADDRESS = HARDWARE ADDRESS + 1


 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74



    JADE COMPUTER NOTE NUMBER 74
===========================================


   SUBJECT: Analysis Routines for Tagging System
   =============================================


                                                Author:A.J.Finch
                                                ================


 This note can be found in 'F11LHO.TAGG.S(TAGNOTE)'




 Summary:
 ========

     This  note  desribes  briefly  the  analysis  routines  installed
     recently on F11LHO.TAGG.S and F11LHO.TAGG.L whose purpose  is  to
     produce the output banks 'ACLS', 'TAGG' /0,/1,/2 whose content is
     described  in  Jade  Computer  Note  No.16.The  input  to  these
     routines is the 'ATAG' bank which contains the raw  adc  contents
     from the tagging adcs.They also utilise a new set of  calibration
     constants installed on the  standard  Jade  calibration  file,and
     obtained in the usual way by calling  KALIBR  for  every  run.The
     programs were originally written by H.Wriedt ,A.Finch, and  J.Nye.




Introduction:
=============

      One part of JADE  that has undergone slightly more changes than
      most is the tagging system .  In  1979  and  1980 there was the
      Mark 1 . For  1981 with the arrival of mini - betas  Mark 2 was
      installed suspended on the muon chambers. In order to solve the
      problems of browning lead glass encountered with this location,
      Mark 3  was  installed for  1983  running  onwards .  All these
      systems require different software to analyse them due to their
      differing geometries etc.



The Software:
============

      The minimum information a user needs to use the package
      that  runs these analysis programs is:

       1) Have F11LHO.TAGG.L in his list of load libraries.
       2) CALL KALIBR for every run.
       3) CALL TAGAN once per event.

       4) For 1979/80 data only :
            He must also have the private calibration file
            'F22HOW.PEDESTAL.ALLSP80' attached to fortran
             stream 19.


The routine TAGAN
=================



      TAGAN(IERTAG,NRUN)



     Arguments:
     ==========

           IERTAG - OUTPUT RETURN CODE

           NRUN   - INPUT DUMMY RUN NUMBER TO OVERIDE CONTROL  OF  WHICH
                          ANALYSIS ROUTINE IS  CALLED  FOR  MONTE  CARLO
                          DATA
                         ++++++++++++++++++++++++++++++++++++++++++
                         + IN 99% OF CASES THIS CAN AND SHOULD BE +
                         + SET TO ZERO                            +
                         ++++++++++++++++++++++++++++++++++++++++++

     Description:
     ============

      This  routine  controls the  analysis  routines.   Its first job
      is to  check that the  input event can be  analysed.  Classes of
      events that can not be analysed are :
          a) Pedestal events.
          b) Events with no 'HEAD' bank.
          c) Events with no 'ATAG' bank.
      The  routine must then  decide which of the  three possible sets
      of routines to call (one for each version of the tagging sytem).
      It does this by  using the  information in the  HEAD  bank.  For
      real data it simply uses the run number. For Monte Carlo  events
      it is neccesary to tell the program which Mark of tagging system
      to expect this is done by one of two methods:-
              On encountering  Monte Carlo  data, the routine looks at
              the  input argument  'nrun' . If this number is not zero
              it takes,  and uses,  it as the  run number,  so -

   Use of NRUN:
   ============
               NRUN                ASSUMED SIMULATION
               ====                ==================

               <6000                Mark 1
          6000>    <12947           Mark 2
               >12948               Mark 3

         If nrun is set to zero there is a second line of attack which
         is to look at the 2nd half word of the bank 'ATAG'. The value
         of this determines which simulation  was  done  according  to
         following scheme.

                   Value of word             Simulation
                   =============             ==========

                         0                    Mark 1    (or real data !)
                         1                    Mark 2
                         2                    Mark 3

        (Provided this scheme has been adopted in the simulation then
          NRUN can be set to zero)

         N.B. Tagan issues a messsage the first time it is called saying
              what it thinks it is analysing, and again if it encounters
              a change from one tagger type to another.





     Meaning of return code 'IERTAG':
     ================================

                   Value of IERTAG             Meaning
                   ===============             =======

                         0             No Problems - and clusters found.

                         1            No Problems - but  no  clusters
                                      found ('ATAG',and 'HEAD' exist)

                         2             Analysis was completed but
                                        at least one of the output
                                       banks could not be created due
                                       to lack of space.

                         10           No 'ATAG' banks - no analysis done

                         11           No 'HEAD' bank - no analysis done




Stucture of the main anlysis routines:
======================================

      The routines that TAGAN calls are-

      ANATAG  for 1979/80 data
      AFTP83  for 1981 onwards data (with a flag to tell whether data
                                     is from 1981/2 or 1983 onwards)

      The structure of AFTP83 is briefly described at the end of  this
      note for anyone who is interested, for more detail refer to  the
      commented version on F11LHO.TAGG.S.




Using analysis routines for other purposes:
===========================================

 Users may wish to use a small sample of the  routines for their own
 purposes. E.g. for fast selection routines. It is possible for 1981
 data onwards to use the following sheme:


          DATA THRESH/6000.0/

          CALL TAGSET(IMARK) - Force analysis to assume  IMARK  tagger,
                               IMARK = 2 or 3.  -  optional  for  monte
                               carlo data if simulation didn't set  flag
                               in 'ATAG

          CALL TAGINT(&100)  - Initialisation - to be called once  per
                              event ; RETURN 1 if can't work  out  which
                              tagger this is ( due to no head bank).

          CALL COMFIL(IWRITE,&100)    - Gets the ATAG data,if  IWRITE=
                                      1 writes out some debugging info
                                      Applies  nominal calibration to
                                      convert channel number to MeV.

          CALL PEDFIX   - pedestal fixing - optional
                                       (no disaster if not done)

           CALL CALAD(IWRITE)    -   Calibration - optional
                                       (no disaster if not done)

           CALL SRSUM(-1,SUMM,&100) - work out sum of -z and + z
           CALL SRSUM(+1,SUMP,&100) - return 1 if sum has 'impossible'
                                                  value
C
C SUMM,SUMP,THRESH are in MeV
C

           IF((SUMM.GT.THRESH).OR.(SUMP.GT.THRESH))....
 100       CONTINUE


 | For more detailed information about these and other routines
 | in this package see 'F11LHO.TAGG.S(#TAGDOC)'

 ____________________________________________________________________


 For completeness there now follows a brief description of
 the procedure adopted in the routine 'AFTP83' for analysing
 all data from 1981 onwards.


---------------------------------------------------------------------


=======================================================================
 PROCESS            | ROUTINE NAME   |   NOTES
                    | ( if not done  |
                    |  in AFTP83)    |
========================================================================
                    |                |
 Initialisation     | TAGINT         |
                    |                |
                    |                |
                    |                |
                    |                |
 Read data in 'ATAG'| COMFIL         |1) An overall calibration that
                    |                |   converts adc channel number to
                    |                |   MeV is applied.
                    |                |2) Software addresses are used
                    |                |   from here on.
                    |                |
 Subtract pedestals | PEDFIX         |These are caused by fluctuating
                    |                |pedestals due to 50HZ AC pickup
                    |                | on signal cables.It is only
                    |                | treated in those events where
                    |                | it exceeds the cut off at 20
                    |                | channels applied by the
                    |                | Le Croy ADC controler.Ammount
                    |                | to be subtracted is estimated on
                    |                | an event by event basis.
                    |                |
 Apply calibration  | CALAD          | Factors obtained from Kalibr.
  factors.          |                |
                    |                |
  Work out the sum  |SRSUM(JPART,SUM)| Works out SUM for JPART end
 of energy in -Z and|                | (JPART = +/- 1)
+Z tagger.          |                |
                    |                |
                    |                |
LOOP1 <This section once for -Z then once for +Z>>>>>>>>>>>>>>>
                    |                |
  Sort adcs into    | SORT1          |
 order of decreasing|                |
  energy contents.  |                |
                    |                |
  Use sorted list to| CLUSFN         |
  find clusters of  |                |
  deposited energy. |                |
    Store results in|                |
   cluster map.     |                |
                    |                |
=======================================================================
 PROCESS            | ROUTINE NAME   |   NOTES
                    | ( if not done  |
                    |  in AFTP83)    |
========================================================================
                    |                |
 LOOP2 <start loop over all clusters (this end) >>>
                    |                |
 Fill the cluster   |                |
 map 'ACLS' + save  |                |
 pointers for TAGG1 |                |
                    |                |
                    |                |
 Find position of   | CLSPS          | This is the main routine where
   centre of each   |                | geometrical differences effect
    cluster,in face |                | the software.
  of blocks.        |                |   The procedure adopted compares
                    |                |   the ratio of energies in the
                    |                |   hit block and its neighbours
                    |                |   to known distributions of
                    |                |   energy within e/m showers
                    |                |   to estimate how far to move
                    |                |   the centre of the shower from
                    |                |   the centre of the block with
                    |                |   the largest ammount of energy,
                    |                |   towards its neighbours with the
                    |                |   next largest ammounts of energy
                    |                |   N.B. A large fraction of hits
                    |                |   lose some significant fraction
                    |                |   of their energy out of the
                    |                |  inner or outer edges,which makes
                    |                |   position determination
                    |                |   difficult.
   Save information |                |   (Hardware addresses are used
   on individual    |                |   in the output)
   clusters to put  |                |
   in TAGG/2        |                |
                    |                |
 <end of LOOP2 over each cluster>    |
                    |                |
 repeat LOOP1 for +Z|                |
                    |                |
Calculate angle     |                |
 between clusters   |                |
 found above.Flag   |                |
 colinear pairs of  |                |
  clusters in TAGG2 |                |
                    |                |
 Create output      | TAGSTO         |
  banks             |                |
                    |                |
 Return to TAGAN    |                |
                    |                |
========================================================================
 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74

 ==== UPDATE ==== THIS NOTE REPLACES OLD JADE C.N. 74==================


    JADE COMPUTER NOTE NUMBER 74
===========================================


   SUBJECT: Analysis Routines for Tagging System
   =============================================


                                                Author:A.J.Finch
                                                ================


 This note can be found in 'F11LHO.TAGG.S(TAGNOTE)'




 Summary:
 ========

     This  note  desribes  briefly  the  analysis  routines  installed
     recently on F11LHO.TAGG.S and F11LHO.TAGG.L whose purpose  is  to
     produce the output banks 'ACLS', 'TAGG' /0,/1,/2 whose content is
     described  in  Jade  Computer  Note  No.16.The  input  to  these
     routines is the 'ATAG' bank which contains the raw  adc  contents
     from the tagging adcs.They also utilise a new set of  calibration
     constants installed on the  standard  Jade  calibration  file,and
     obtained in the usual way by calling  KALIBR  for  every  run.The
     programs were originally written by H.Wriedt ,A.Finch, and  J.Nye.




Introduction:
=============

      One part of JADE  that has undergone slightly more changes than
      most is the tagging system .  In  1979  and  1980 there was the
      Mark 1 . For  1981 with the arrival of mini - betas  Mark 2 was
      installed suspended on the muon chambers. In order to solve the
      problems of browning lead glass encountered with this location,
      Mark 3  was  installed for  1983  running  onwards .  All these
      systems require different software to analyse them due to their
      differing geometries etc.



The Software:
============

      The minimum information a user needs to use the package
      that  runs these analysis programs is:

       1) Have F11LHO.TAGG.L in his list of load libraries.
       2) CALL KALIBR for every run.
       3) CALL TAGAN once per event.

       4) For 1979/80 data only :
            He must also have the private calibration file
            'F22HOW.PEDESTAL.ALLSP80' attached to fortran
             stream 19.


The routine TAGAN
=================



      TAGAN(IERTAG,NRUN)



     Arguments:
     ==========

           IERTAG - OUTPUT RETURN CODE

           NRUN   - INPUT DUMMY RUN NUMBER TO OVERIDE CONTROL  OF  WHICH
                          ANALYSIS ROUTINE IS  CALLED  FOR  MONTE  CARLO
                          DATA
                         ++++++++++++++++++++++++++++++++++++++++++
                         + IN 99% OF CASES THIS CAN AND SHOULD BE +
                         + SET TO ZERO                            +
                         ++++++++++++++++++++++++++++++++++++++++++

     Description:
     ============

      This  routine  controls the  analysis  routines.   Its first job
      is to  check that the  input event can be  analysed.  Classes of
      events that can not be analysed are :
          a) Pedestal events.
          b) Events with no 'HEAD' bank.
          c) Events with no 'ATAG' bank.
      The  routine must then  decide which of the  three possible sets
      of routines to call (one for each version of the tagging sytem).
      It does this by  using the  information in the  HEAD  bank.  For
      real data it simply uses the run number. For Monte Carlo  events
      it is neccesary to tell the program which Mark of tagging system
      to expect this is done by one of two methods:-
              On encountering  Monte Carlo  data, the routine looks at
              the  input argument  'nrun' . If this number is not zero
              it takes,  and uses,  it as the  run number,  so -

   Use of NRUN:
   ============
               NRUN                ASSUMED SIMULATION
               ====                ==================

               <6000                Mark 1
          6000>    <12947           Mark 2
               >12948               Mark 3

         If nrun is set to zero there is a second line of attack which
         is to look at the 2nd half word of the bank 'ATAG'. The value
         of this determines which simulation  was  done  according  to
         following scheme.

                   Value of word             Simulation
                   =============             ==========

                         0                    Mark 1    (or real data !)
                         1                    Mark 2
                         2                    Mark 3

        (Provided this scheme has been adopted in the simulation then
          NRUN can be set to zero)

         N.B. Tagan issues a messsage the first time it is called saying
              what it thinks it is analysing, and again if it encounters
              a change from one tagger type to another.





     Meaning of return code 'IERTAG':
     ================================

                   Value of IERTAG             Meaning
                   ===============             =======

                         0             No Problems - and clusters found.

                         1            No Problems - but  no  clusters
                                      found ('ATAG',and 'HEAD' exist)

                         2             Analysis was completed but
                                        at least one of the output
                                       banks could not be created due
                                       to lack of space.

                         10           No 'ATAG' banks - no analysis done

                         11           No 'HEAD' bank - no analysis done




Stucture of the main anlysis routines:
======================================

      The routines that TAGAN calls are-

      ANATAG  for 1979/80 data
      TAGGTP  for 1981 onwards data (with a flag to tell whether data
                                     is from 1981/2 or 1983 onwards)

      The structure of TAGGTP is briefly described at the end of  this
      note for anyone who is interested, for more detail refer to  the
      commented version on F11LHO.TAGG.S.




Using analysis routines for other purposes:
===========================================

 Users may wish to use a small sample of the  routines for their own
 purposes. E.g. for fast selection routines. It is possible for 1981
 data onwards to use the following sheme:


          DATA THRESH/6000.0/

          CALL TAGSET(IMARK) - Force analysis to assume  IMARK  tagger,
                               IMARK = 2 or 3.  -  optional  for  monte
                               carlo data if simulation didn't set  flag
                               in 'ATAG

          CALL TAGINT(&100)  - Initialisation - to be called once  per
                              event ; RETURN 1 if can't work  out  which
                              tagger this is ( due to no head bank).

          CALL TAGADC(IWRITE,&100)    - Gets the ATAG data,if  IWRITE=
                                      1 writes out some debugging info
                                      Applies  nominal calibration to
                                      convert channel number to MeV.

          CALL TAGPED   - pedestal fixing - optional
                                       (no disaster if not done)

           CALL TAGKAL(IWRITE)    -   Calibration - optional
                                       (no disaster if not done)

           CALL TAGSUM(-1,SUMM,&100) - work out sum of -z and + z
           CALL TAGSUM(+1,SUMP,&100) - return 1 if sum has 'impossible'
                                                  value
C
C SUMM,SUMP,THRESH are in MeV
C

           IF((SUMM.GT.THRESH).OR.(SUMP.GT.THRESH))....
 100       CONTINUE


 | For more detailed information about these and other routines
 | in this package see 'F11LHO.TAGG.S(#TAGDOC)'

 ____________________________________________________________________


 For completeness there now follows a brief description of
 the procedure adopted in the routine 'TAGGTP' for analysing
 all data from 1981 onwards.


---------------------------------------------------------------------


=======================================================================
 PROCESS            | ROUTINE NAME   |   NOTES
                    | ( if not done  |
                    |  in TAGGTP)    |
========================================================================
                    |                |
 Initialisation     | TAGINT         |
                    |                |
                    |                |
                    |                |
                    |                |
 Read data in 'ATAG'| TAGADC         |1) An overall calibration that
                    |                |   converts adc channel number to
                    |                |   MeV is applied.
                    |                |2) Software addresses are used
                    |                |   from here on.
                    |                |
 Subtract pedestals | TAGPED         |These are caused by fluctuating
                    |                |pedestals due to 50HZ AC pickup
                    |                | on signal cables.It is only
                    |                | treated in those events where
                    |                | it exceeds the cut off at 20
                    |                | channels applied by the
                    |                | Le Croy ADC controler.Ammount
                    |                | to be subtracted is estimated on
                    |                | an event by event basis.
                    |                |
 Apply calibration  | TAGKAL          | Factors obtained from Kalibr.
  factors.          |                |
                    |                |
  Work out the sum  |TAGSUM(JPART,SUM)| Works out SUM for JPART end
 of energy in -Z and|                | (JPART = +/- 1)
+Z tagger.          |                |
                    |                |
                    |                |
LOOP1 <This section once for -Z then once for +Z>>>>>>>>>>>>>>>
                    |                |
  Sort adcs into    | TAGSR1          |
 order of decreasing|                |
  energy contents.  |                |
                    |                |
  Use sorted list to| TAGCLS         |
  find clusters of  |                |
  deposited energy. |                |
    Store results in|                |
   cluster map.     |                |
                    |                |
=======================================================================
 PROCESS            | ROUTINE NAME   |   NOTES
                    | ( if not done  |
                    |  in TAGGTP)    |
========================================================================
                    |                |
 LOOP2 <start loop over all clusters (this end) >>>
                    |                |
 Fill the cluster   |                |
 map 'ACLS' + save  |                |
 pointers for TAGG1 |                |
                    |                |
                    |                |
 Find position of   | TAGPOS          | This is the main routine where
   centre of each   |                | geometrical differences effect
    cluster,in face |                | the software.
  of blocks.        |                |   The procedure adopted compares
                    |                |   the ratio of energies in the
                    |                |   hit block and its neighbours
                    |                |   to known distributions of
                    |                |   energy within e/m showers
                    |                |   to estimate how far to move
                    |                |   the centre of the shower from
                    |                |   the centre of the block with
                    |                |   the largest ammount of energy,
                    |                |   towards its neighbours with the
                    |                |   next largest ammounts of energy
                    |                |   N.B. A large fraction of hits
                    |                |   lose some significant fraction
                    |                |   of their energy out of the
                    |                |  inner or outer edges,which makes
                    |                |   position determination
                    |                |   difficult.
   Save information |                |   (Hardware addresses are used
   on individual    |                |   in the output)
   clusters to put  |                |
   in TAGG/2        |                |
                    |                |
 <end of LOOP2 over each cluster>    |
                    |                |
 repeat LOOP1 for +Z|                |
                    |                |
Calculate angle     |                |
 between clusters   |                |
 found above.Flag   |                |
 colinear pairs of  |                |
  clusters in TAGG2 |                |
                    |                |
 Create output      | TAGSTO         |
  banks             |                |
                    |                |
 Return to TAGAN    |                |
                    |                |
========================================================================
