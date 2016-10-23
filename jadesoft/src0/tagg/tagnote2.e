 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74 74

 ==== UPDATE ==== THIS NOTE REPLACES OLD JADE C.N. 74==================


    JADE COMPUTER NOTE NUMBER 74
===========================================


   SUBJECT: Analysis Routines for Tagging System
   =============================================


                                                Author:A.J.Finch
                                                ================


 THIS NOTE CAN BE FOUND IN 'F11LHO.TAGG.S(TAGNOTE2)'




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
