C   22/03/97 703221647  MEMBER NAME  JEOSUM2  (JADEMUS)     FVS
C   22/03/97 703221635  MEMBER NAME  @COMMONS (JADEMUS)     FVS
------------------------------------------------------------------------
COMMON Descriptions. ---------------------------------------------------
------------------------------------------------------------------------

   Common block CALIBR can be easily incorporated using %MACRO CMUCALIB
   on F22ALL.JADEMUS(CMUCALIB) .

      COMMON /CALIBR/ LARRY(100),MUCAL(4185)




                NVERSN
      DIMENSION DESCRP(15),HOVALL(6)
                                                     19 WORDS

      EQUIVALENCE ( NVERSN,MUCAL(1) ),( DESCRP(1),MUCAL(2) ),
     *            ( HOVALL(1),MUCAL(17) )
 ----------------------------------------------------19 WORDS SO FAR

      HMFFIX(740)                                   370 WORDS
      DIMENSION HMFFIX(740)
      EQUIVALENCE ( HMFFIX(1),MUCAL(20) )
      DIMENSION HFACE(82),HSECT(82),HLAYER(82),HNORM(82),HLONG(82),
     *          HTRANS(82),HAC(82),HAL(82),HUNIT(82)
      EQUIVALENCE (HMFFIX(1),NFRAMS),(HMFFIX(3),HFACE(1)),
     *            (HMFFIX(85),HSECT(1)),(HMFFIX(167),HLAYER(1)),
     *            (HMFFIX(249),HNORM(1)),(HMFFIX(331),HLONG(1)),
     *            (HMFFIX(413),HTRANS(1)),(HMFFIX(495),HAC(1)),
     *            (HMFFIX(577),HAL(1)),(HMFFIX(659),HUNIT(1))
 ---------------------------------------------------389 WORDS SO FAR


      HMCFIX(636)                                   318 WORDS
      DIMENSION HMCFIX(636)
      EQUIVALENCE ( HMCFIX(1),MUCAL(390) )
      DIMENSION HFR(634)
      EQUIVALENCE (HMCFIX(1),NCHAMS),(HMCFIX(3),HFR(1))
 ---------------------------------------------------707 WORDS SO FAR

      HMFSUR(492)                                   246 WORDS
      DIMENSION HMFSUR(492)
      EQUIVALENCE ( HMFSUR(1),MUCAL(708) )
      DIMENSION HDIST(82),HANG(82),HCLLO(82),HCLHI(82),HCTLO(82),
     *          HCTHI(82)
      EQUIVALENCE (HMFSUR(1),HDIST(1)),(HMFSUR(83),HANG(1)),
     *            (HMFSUR(165),HCLLO(1)),(HMFSUR(247),HCLHI(1)),
     *            (HMFSUR(329),HCTLO(1)),(HMFSUR(411),HCTHI(1))
 ---------------------------------------------------953 WORDS SO FAR


      HMCSUR(1268)                                  634 WORDS
      DIMENSION HMCSUR(1268)
      EQUIVALENCE ( HMCSUR(1),MUCAL(954) )
      DIMENSION HD1(634),HCTW(634)
      EQUIVALENCE (HMCSUR(1),HCTW(1)),(HMCSUR(635),HD1(1))
 --------------------------------------------------1587 WORDS SO FAR

@
      HMCELE(4440)                                 2220 WORDS
      DIMENSION HMCELE(4440)
      EQUIVALENCE ( HMCELE(1),MUCAL(1588) )
C ********* N.B. HMCEDM IS JUST A 'FILLER' . **********
      DIMENSION HDTP(634),HLTP(634),HLSF(4,634),HVDRFT(634)
      EQUIVALENCE (HMCELE(1),HVDR),(HMCELE(2),HDTP(1)),
     *            (HMCELE(636),HLTP(1)),(HMCELE(1270),HLSF(1,1)),
     *            (HMCELE(3806),HMCEDM),(HMCELE(3807),HVDRFT(1))
 --------------------------------------------------3807 WORDS SO FAR

      HMCSTA(634)                                   317 WORDS
      DIMENSION HMCSTA(634)
      EQUIVALENCE ( HMCSTA(1),MUCAL(3808) )
 --------------------------------------------------4124 WORDS SO FAR

      HFILDA(72)                                     36 WORDS
      DIMENSION HFILDA(72)
      EQUIVALENCE ( HFILDA(1),MUCAL(4125) )
      INTEGER*2 HBLLO(6),HBLHI(6),HBTLO(6),HBTHI(6),HBNLIM(36)
      INTEGER*4 IFCIND(6)
      INTEGER*2 HFILDA
      EQUIVALENCE (HBLLO(1),HFILDA(1)),(HBLHI(1),HFILDA(7)),
     *            (HBTLO(1),HFILDA(13)),(HBTHI(1),HFILDA(19)),
     *            (HBNLIM(1),HFILDA(25)),(IFCIND(1),HFILDA(61))
 --------------------------------------------------4160 WORDS SO FAR


      HYKNMI(4),HYKNMO(4),HYKLDM(4),HYKTDM(4),BYOKE, 10 WORDS
      IYKIND
      DIMENSION HYKNMI(4),HYKNMO(4),HYKLDM(4),HYKTDM(4)
      INTEGER*2 HYKTDM,HYKLDM,HYKNMI,HYKNMO
      EQUIVALENCE ( HYKNMI(1),MUCAL(4161) ),
     *            ( HYKNMO(1),MUCAL(4163) ),
     *            ( HYKLDM(1),MUCAL(4165) ),
     *            ( HYKTDM(1),MUCAL(4167) ),
     *            ( BYOKE,MUCAL(4169) ),( IYKIND,MUCAL(4170) )
 --------------------------------------------------4170 WORDS SO FAR


     IZEII,IZEIO,IREP1,IREP2,IREP3,IREP4,IXYEP5,     15 WORDS
     IZOEP1,IZOEP2,IZOEP3,IZOEP4,IZOEP5,CAEP2,
     IEPIND,IEPSCT

      EQUIVALENCE ( IZEII,MUCAL(4171) ),( IZEIO,MUCAL(4172) ),
     *            ( IREP1,MUCAL(4173) ),( IREP2,MUCAL(4174) ),
     *            ( IREP3,MUCAL(4175) ),( IREP4,MUCAL(4176) ),
     *            ( IXYEP5,MUCAL(4177) ),( IZOEP1,MUCAL(4178) ),
     *            ( IZOEP2,MUCAL(4179) ),( IZOEP3,MUCAL(4180) ),
     *            ( IZOEP4,MUCAL(4181) ),( IZOEP5,MUCAL(4182) ),
     *            ( CAEP2,MUCAL(4183) ),( IEPIND,MUCAL(4184) ),
     *            ( IEPSCT,MUCAL(4185) )
 --------------------------------------------------4185 WORDS SO FAR
@  DEFINITION OF CALIBRATION TERMS

  NVERSN           Version number.
  DESCRP           Description.

  HOVALL(IUNIT)    Overall translation of each unit along rails.
                     IUNIT=1 - far  side (-x) wall,
                     IUNIT=2 - near side (+x, rucksack) wall,
                     IUNIT=3 - magnet  (All translations are relative to
                               this so HOVALL(3) should ALWAYS be zero.)
                     IUNIT=4 - far  side (-x) arch,
                     IUNIT=5 - near side (+x) arch.

  IFRAME           Frame number.
  ICHAM            Chamber number.
  NFRAMS           Number of frames.
  NCHAMS           Number of chambers.

  Fixed Data For Each Frame....

  HFACE(IFRAME)    1-6 for -x,+x,-y,+y,-z,+z respectively.
                   =0 if frame not present.
  HSECT(IFRAME)    Section number of section to which frame belongsh
  HLAYER(IFRAME)   1-5 numbering from the interaction point outwardsh
                     =1, inside return yoke
                     =2-5 for layers on concrete,
  HNORM(IFRAME)    =1,normal of plane parallel to x-axis
                   =2,normal of plane parallel to y-axis
                   =3,normal of plane parallel to z-axis
  HLONG(IFRAME)    =1,wire nominally parallel to x-axis
                   =2,wire nominally parallel to y-axis
                   =3,wire nominally parallel to z-axis
  HTRANS(IFRAME)   =1,drift field parallel to x-axis
                   =2,drift field parallel to y-axis
                   =3,drift field parallel to z-axis
  HAC(IFRAME)      Chamber number of first chamber in frame.
  HAL(IFRAME)      Chamber number of last chamber in frame.
  HUNIT(IFRAME)    Unit to which this frame belongs.

  Survey Data For Each Frame....

  HDIST(IFRAME)     The coordinate of the central plane where the axis
                     specified by HNORM(IFRAME) cuts the plane(units mm)
  HANG(IFRAME)      The angle between the wire and the axis specified by
                     HLONG(IFRAME)  (units 1/10 mr)
  HCLLO(IFRAME)     Lower logitudinal coordinate limit
  HCLHI(IFRAME)     Upper logitudinal coordinate limit
  HCTLO(IFRAME)     Lower transverse coordinate limit
  HCTHI(IFRAME)     Upper transverse coordinate limit
                    (The above 4 variables apply to total sensitive area
                       of plane.They are in mm )

  Fixed Data For Each Wire....

  HFR(ICHAM)        Frame number for this chamber.

  Survey Data For Each Wire....

  HD1(ICHAM)        Amount to be added to HDIST(IFRAME) to get to
                     coordinate of the chamber.   (units mm)
@ HCTW(ICHAM)       Tranverse coordinate of each wire.  (units mm)

  Electronic Data For Chambers...

  HDTP(ICHAM)      Drift time pedestal (trans. clock units, ca. 60 ns.)
  HLTP(ICHAM)      Longitudinal time pedestal (in long. clock units,
                     ca. 0.5 ns. or 50 mm.)
  HLSF(J,ICHAM)    Long. scale factor for j'th hit
                     (units (1/100mm)/long. clock unit)
  HVDRFT(ICHAM)    Drift velocity (microns per clock unit (50 ns)).

                   The above data are used to convert signals to coor-
                     -dinates relative to the chamber as follows..
                   ICT = ( HVDRFT * (ITD-HDTP) )/1000
                   ICL = ( HLSF   * (ITL-HLTP) )/100
                     where ICT,ICL are coordinates in mm,
                     ITD is drift time in trans. clock units, and
                     ITL is long. time diff. in long. clock units.


  Status Data For The Chambers

  HMCSTA(ICHAM)    =0    if chamber ok
                   .NE.0 if chamber u/s for any reason.

------------------------------------------------------------------------
#include "cmutny.for"
    /CMUTNY/


  Condensed mu-filter paarameters for use by approximate signal to
    coordinate conversion subroutine MUTINY as used at the NORD.

  HPLANS      No. of chamber planes.
  HVDRAV      Average drift velocity.
  HDTPAV      Average drift time pedestal.
  HLTPAV      Average longitudinal time difference pedestal.
  HLSFAV      Average longitudinal scaling factor.

  FOR EACH CHAMBER PLANE...

  HLY         Layer number.
  HOR         Orientation parameter:
              =1, wires parallel to beam, and normal parallel to
                    x-axis - faces 1(-x) and 2(+x).
              =2, wires parallel to beam, and normal parallel to
                    y-axis - faces 3(-y) and 4(+y).
              =3, wires vertical, and normal parallel to z-axis -
                    faces 5(-z) and 6(+z).
  HC1         First chamber number.
  HCN0        Normal       )
  HCL0        Longitudinal )  coordinate of 'origin' of chamber plane.
  HCT0        Transverse   )
  HSP         Average spacing of chambers.

    (The 'origin' is at one end of the wire of the first chamber in the
  plane.  The end is that with the lowest longitudinal coordinate.)

      COMMON /CMUTNY/HPLANS,HVDRAV,HDTPAV,HLTPAV,HLSFAV,
     * HLY(48),HOR(48),HC1(48),HCN0(48),HCL0(48),HCT0(48),HSP(48)

------------------------------------------------------------------------

 MACRO CMUANP.
 -------------
  Mu analysis parameters filled by block data after MUINI.

      COMMON/CMUANP/IMUANP(30)
      DIMENSION AMUANP(30),HMUANP(60)
      EQUIVALENCE (IMUANP(1),AMUANP(1),HMUANP(1))

------------------------------------------------------------------------

 COMMON /CMUPRN/.
 ---------------

      COMMON /CMUPRN/MUPRIN

      MUPRIN=0 to suppress all printing of mu messages,
            .GE.1 to get mu error messages,
            .GE.2 to get mu information messages,
            .GE.10 to get full mu calibration printout (about 10 pages).

------------------------------------------------------------------------
End of COMMON Descriptions. --------------------------------------------
------------------------------------------------------------------------
C   22/03/97 703221635  MEMBER NAME  @MISC    (JADEMUS)     FVS
**********************************************************************
*  The Philosophy 1 routines (MUANAL,MUANAJ) are currently commented *
*  out in the standard driving routine MUANA.                        *
**********************************************************************

      The calling sequence is now as follows:

 1) CALL MUINI                              - at start of operations.
 2) Fill muon calibration data areas        - before any processing.
     (This can be done with the STANDARD JADE SYSTEM or with MUCON.)
 3) CALL MUREG(IPRINT) (usually IPRINT=0)   -       ditto.
 4) CALL MUANA(0) if 'PATR' absent or
    CALL MUANA(1) if 'PATR' present         - in event loop.
     (MUANA is described in more detail below.)
 5) CALL MUFINI                             - at end.

      The above are incorporated into the standard supervisor.If you
 elect to use it, there exist the following to assist mu program
 development ( in source/load libraries F22ALL.MUSEFULS/MUSEFULL ) :
 1) A special version of USER in member MUSER.
 2) Subroutine MU0 which is called at start of operations. MU0 reads a
      parameter card which (among other things) specifies the print
      parameter IPRINT, which is used as follows...
        IPRINT .LE. 0 suppresses all printing. (This is the normal case
                        for graphics, standard analysis,etc.)
        IPRINT .GE. 1 to get error messages.
        IPRINT .GE. 2 to get normal muon messages.
        IPRINT .GE. 4 to get Philosophy 2 results printing.
        IPRINT .GE. 10 to get calibration data printing.
 3) Subroutine MU1 which is called just after new calibration data have
      been read in and which will read a private set of calibration data
      if requested, or alternatively a set of updates.
 4) Subroutine MU2 which is called just after the event has been read
      and which allows event selection.
*5) Subroutine MU8 which is called after mu processing is complete.
 6) Subroutine MU99 which is called at end.
*7) A specimen set of JCL in member #MUTEST which uses all of these
      facilities.
  =====
  NOTE: The following are also available on F22ALL.MUSEFULS/L :
  =====

  1) A somewhat simplified version of #MUTEST is to be found in member
     #USERA. It contains a data input file , a data output file , and
     JADE's calibration datasets. All the relevent load libraries are
     already linked in.
@
  2) Member USERA contains a special version of USER. This version of
     user contains the Time-Of-Flight programs which produce the 'TOFR'
     results bank , calls only those programs which are needed for
     Phil.2 and bypasses the call to MUANA at level 8 in the supervisor.
     ( see below )
  ======================================================================

C   22/03/97 703221635  MEMBER NAME  @MUCALIB (JADEMUS)     FVS
------------------------------------------------------------------------


Muon Calibration Data Banks :
-----------------------------

    These are kept on datasets
      (a)  F22ALL.MUCALIB.DATA0001 ( -0002, ..... ,-0018 )
           as BOS records which can be read by BREAD ;
      (b)  F22ALL.MUCALIB.NBOS0004 ( -0005, ..... ,-0016 only, see (c))
           as single logical records of length 4185 words.
*     (c)  F22ALL.MUCALIB.JADE0017 ( -0018, .....  )
*          which are in the new JADE format ( from no. 17 onwards ):
*         LENGTH,(IBUF(I),I=1,9),(MUCAL(I),I=1,4185)   ( ALL I*4 WORDS )
*           where LENGTH = 4194  ,
*                 IBUF(1)='MUCA' ,
*                 IBUF(6)= CAMAC TIME ( in seconds since 1/1/1979 )
*                          at which this data set comes into force ,
*                 IBUF(7)= 1 , signifying that a complete set of
*                             mufilter constants follows ,
*                 ALL OTHER IBUF(I)=0.
*     (d)  F22ALL.UPDATE0m.JADE00nn
*         the mTH. UPDATE to MUCALIB.JADE00nn , which are updates of
*         small nos. of constants provided in the format
*        LENGTH,(IBUF(I),I=1,9),CONST. #,NEW VALUE,CONST #,NEW VALUE,...
*        |--------------------| |------------------------------------...
*        |< I*4 WORDS        >| |<       I*2   WORDS   ....
*
*         where   LENGTH is VARIABLE and = the no. of words following,
*                 IBUF(7)= 0, signifying that UPDATES of individual
*                             constants follow ,
*                 ALL OTHER IBUF(I) are as in (c) above.
*          N.B.THE CONSTANT LOCATIONS ARE COUNTED IN I*2 WORDS IN MUCAL.
*

   The first one of type (a) is for MONTE-CARLO analysis.  Datasets  (a)
   are provided for the purpose of private muon calibration outside  the
   JADE system. Any changes or updates are put onto these BOS  datasets.
   The changes are then checked independently before providing a copy in
   format (c)/(d) for the JADE system.

*     Viewing and changing calibration datasets are now possible using a
*  fullscreen program called EXCALIBUR which is very easy to use. It  is
*  activated using the  command  'F22BOW.EXCALIBR.S(EXCALIBR)'.  Changes
*  and updates can be implemented by routine  MUCONE  which  is  usually
*  called  by  a  simple  main  program  running  outside  the  standard
*  supervisor. This program should call MUCONR (if desired) to  read  in
*  an existing BOS calibration dataset and also MUCONW to write out  the
*  new calibration.

   Only one common is used by MUCONE and MUCONW , namely CMUCALIB.
*
*  See further information on F22ALL.JADEMUS1
*

@ Bank Names, Numbers and Lengths
  -------------------------------

   Name/Number Length          Contents
   ---- ------ ------          --------

   MUCD   0      16    Version number and description.
   MUOV   0       3    Overall jade unit translations.
   MFFI   2     370    Fixed frame parameters.
   MCFI   3     318    Fixed chamber parameters.
   MFSU   4     246    'Survey' frame parameters.
   MCSU   5     634    'Survey' chamber parameters.
   MCEL   6    2220    'Electronic' chamber parameters.
   MCST   7     317    Chamber status words.
   MUFI   8      36    Filter (absorber block) parameters.
   MUYO   9      10    Side, top and bottom yoke parameters.
   MUEN  10      15    Yoke end-plug parameters.

  Total length 4185 words.

------------------------------------------------------------------------


    The following calibration data have been provided to the JADE
    calibration system:
*    ( N.B. The key parameter for implementation is the TIME stated.)

   Dataset     Run                    Comments
   Number     Numbers
   -------  -----------   ----------------------------------
    0004    0000 - 2047   Data unreliable ; all chambers 'off'
                           up to 13.00 26.10.79

    0005    2047 - 2185   336 chambers in the barrel faces 'on'
                           17.09 26.10.79  to 00.05 03.11.79

    0006    2186 - 2402   349 chambers in the barrel faces 'on'
                           00.18 03.11.79  to 23.19 18.11.79

    0007    2403 - 2746   477 chambers in the barrel faces and
                          the endwalls 'on'
                           23.31 18.11.79  to 23.59 31.12.79

    0008    2747 - 3015   592( out of 622 installed ) chambers 'on'
                           00:00 01.01.80  to  04.24 10.03.80

    0009    3016 - 3316   597/622 chambers 'on'
                           04.51 10.03.80 to  20.35 03.04.80

    0010    3317 - 3584   597/622 chambers 'on'
                           09.30 11.04.80 to  20.30 13.05.80

    0011    3585 - 3614   All chambers off!!! water leak
                           20.30 13.05.80 to  22.54 16.05.80

    0012    3615 - 3727   586/622 chambers 'on'
                           22.55 16.05.80 to  18.39 23.05.80

    0013    3730 - 4891   592/622 chambers 'on'
                           07.03 17.06.80 to  23:59 10.09.80

    0014    4865 - 5344   575/622 chambers 'on'
                           07:00 01.09.80 to  23:59 17.10.80

@   0015    5345 - 5566   578/622 chambers 'on'
                           00:00 18.10.80 to  23:59 31.10.80

    0016    5567 -   /    576/622 chambers 'on'
                           00:00 01.11.80 to  23:59 31/12/81

*   0017            -->    00:00 01.01.81 to  23:59 10/05/81
*                         N.B. All chambers 'OFF' up till 00:00 20.03.81

*   0018            -->    00:00 11.05.81 to  23.59 26/6/81
*

*   0019            -->    00:00 27/06/81 to  23:59 17/09/81
*

*   0020            -->    00:00 18/09/81 to  ........
*

*   0021            -->    00:00 ........ TO  ........
*

*   0022            -->    00:00 ........ TO  ........
*

*   0023            -->    00:00 ........ TO  ........
*

*   0024            -->    00:00 ........ TO  ........
*

*   0025            -->    00:00 ........ TO  NOW
*

       N.B.   'OFF' and 'ON' refer to the state of a software switch:
               for any given dataset,the set of chambers off in the
              NW Hall is a subset of those switched 'off', but generally
              corresponds fairly closely. Holes in run  number  sequence
              correspond to junk data, calibration runs etc.
*             From dataset 17 onwards, the chamber status is revised  on
*             a time scale of 1 day bins. Hence, exact  nos.  of  active
*             chambers are not given here.

C   22/03/97 703221635  MEMBER NAME  @MUSTAT  (JADEMUS)     FVS
------------------------------------------------------------------------

    THIS IS MEMBER @MUSTAT. IT CONTAINS THE INFORMATION CONCERNING
    THE MUON STATISTICS PRINTOUT PRODUCED BY SUBROUTINE MUFINI

------------------------------------------------------------------------

         RUNTIME PROGRAM STATISTICS

      1     CALLS TO MUCOOR
      2     SUM(OVER EVENTS) OF NUMBER OF HITS
      3     EVENTS WITH NO HITS
      4     (MUANAF) HITS IN MUR1/0 BUT NO MUR1/1(2) EXISTING
      5     (MUCUTS) MUR2/0 MISSING
      6     (MUCUTS) MUHITS AND PATR TRACKS EXIST BUT NO MUR2/1..3
      7     OVERFLOW OF OUTPUT ARRAY ( > 200 HITS )( 400 FROM 9/10/85 )
      8     LENGTH ERROR IN HMU
      9     LAST WORD OF HMU IS NOT 0 NOR 3840
     10     (MUANAF) MUR1/0 MISSING
     11     NUMBER OF EVENTS WITH MUCUTS MUONS
     12     NUMBER OF MUONS WITH MUCUTS FLAG = 1
     13     NUMBER OF MUONS WITH MUCUTS FLAG = 5  (NOT SUBSET OF 1 HERE)
     14     NUMBER OF MUONS WITH MUCUTS FLAG = 9  (NOT SUBSET OF 5 HERE)

         STATISTICS PERTAINING TO COORDINATES

     20     DRIFT COORDINATE FLAGGED AS BAD
     21     LONGITUDINAL COORDINATE FLAGGED AS BAD
     22     DRIFT COORDINATE PULLED IN
     23     LONGITUDINAL HIT PULLED IN TO END OF CHAMBER (7)
     24     HIT DELETED BY DOUBLE PULSE CUT(1016)
     25     RECOVER OVERWRITTEN HIT1 WITH (HIT2-ICONST)
     26     IITD.GE.JITD
     27     OUTPUT HITS FLAGGED DUE TO OUT OF SEQUENCE DRIFT TIMES
            N.B. #27 IS INCREMENTED ONLY ONCE PER CHAMBER IN ANY ONE
                 EVENT, WHEREAS #26 IS INCREMENTED FOR EVERY INSTANCE
                 WHERE WE HAVE  (SAMCHM).AND.IITD.GE.JITD  , WHICH
                 MORE THAN ONCE IN ANY ONE CHAMBER.
     28     HIT NUMBER NOT DECREASED BY ONE
     29     NEW CHAMBER, BUT LAST HIT WAS NOT HIT1
     30     SINGLES DATA, BUT NOT HIT1

         STATISTICS PERTAINING TO CHAMBERS

     40     HIT FROM CHAMBER WITH STATUS NOT GOOD(1017)
     41     INVALID CHAMBER NUMBER
     42     CHAMBER OUT OF SEQUENCE
     43     CHAMBER IN WRONG CRATE
     44     IN SINGLES DATA, COUNTS OR TIME IS NEGATIVE

         STATISTICS PERTAINING TO CRATES

     60     INVALID CRATE NUMBER (901)
     61     CRATE NUMBER OUT OF ORDER
     62     TIME REFERENCE WORD NOT PRESENT
     63     END-OF-CRATE FOUND IN RECOVER SEARCH (2203)
     64     CRATE MARKER FOUND IN RECOVER SEARCH (2204)
     65     END-OF-CRATE FOUND WHEN CRATE MARKER WAS EXPECTED.

     70 + NN   COUNT OF NUMBER OF EVENTS WHERE CRATE NN WAS MISSING
     84 + NN    "    "   "     "    "      "     "   "  HAD NO E.O.C.

    THE STATISTICS INFORMATION IS STORED IN

      COMMON / CMUSTT / MUTIT(100),NMU(100)

    MUTIT    ARE THE TITLES USED IN THE PRINTOUT. SEE BLOCKDATA IN MUINI
    NMU      ARE THE STATISTICS SCALARS.

C   22/03/97 703221635  MEMBER NAME  @PHIL1   (JADEMUS)     FVS
    THIS INFORMATION USED TO BE IN @MUINFOM. IT CONTAINS DETAILS ABOUT
    PHILOSOPHY 1

 -----------------------------------------------------------------------

  2)MUANAL.  This looks for 'linear clusters', i.e. tracks, in the muon
     filter.  It follows 'Philosophy 1', i.e. gathers as much
     information as possible by looking in the muon filter alone.  It
     uses 'MUR1' banks 0,1,& 2.  It updates 'MUR1' bank 0,1,& 2.  It
     creates 'MUR1' banks 3,4,5,6,and 7 (see below).

  3)MUANAJ.  This attempts to join muon clusters to inner detector and
     lead-glass clusters.  It uses the 'MUR1' banks and updates 'MUR1'
     banks 0 and 3 (see below).

 -----------------------------------------------------------------------

  'MUR1' Bank 0 - General information.
   -----------------------------------

   Word  Type  Contents
   ----  ----  --------
     1   I*4   No. of hits.
     2   I*4   No. of clusters (tracks).
     3   I*4   No. of 2-byte words per hit in coordinate bank.
     4   I*4   No. of 4-byte words per cluster in cluster bank.
     5   I*4   =1 if muline has been called, i.e. if an attempt to
                    create clusters has been made.   =0 otherwise.
     6   I*4   =1 if an attempt to join clusters to inner detector
                    tracks has been made.   =0 otherwise.
     7   I*4   =1 if an attempt to join clusters to lead-glass clusters
                    has been made.   =0 otherwise.
     8   I*4   Date of version of signal-coordinate conversion routine.
     9   I*4   Calibration data issue, i.e. identifier of calib. data
                    used to produce coordinates.


@ 'MUR1' Bank 3 - Muon cluster information.
   ----------------------------------------

              (Note. cluster number in word 30.)
                  For each cluster...

   Word  Type               Contents
   ----  ----               --------
     1   I*4   Date of production (e.g. 791010 for 10/10/79).
     2   R*4   Identifier of program which created cluster (a 4 char-
                 acter alphanumeric word).
     3   I*4   No. of hits in cluster.
     4   I*4   Cluster number of alternative cluster (=0 if none).
     5   I*4   =0, only one layer in cluster (if so words 9-14=0).
     6   R*4   xc )
     7   R*4   yc ) coords. of 'centre of gravity' (mm).
     8   R*4   zc )
     9   R*4   dx )
    10   R*4   dy ) direction cosines of fitted line.
    11   R*4   dz )
    12   R*4   d1, distance to 'first' point (mm).
    13   R*4   d2, distance to 'last' point (mm).
                 Note. algorithm to get cordinates of first hit is...
                   x1=xc+d1*dx
                   y1=yc+d1*dy
                   z1=zc+d1*dz
                 and similarly for last hit.
    14   R*4   rms deviation for 'good' cluster - see also words 25,26.
               =0. if mulina (ambiguity resolving routine) not called.
               =-1. if it fails acceptance criteria,
               =-2. if it has more than 2 acceptable ambigiuty
                             permutations.
               =-9999. if mulina has taken no action, e.g. if only 1
                  layer, or too many ambiguities, or only 2 layers and
                  too many ambiguities.
               Note that if this word.LE.0 then words 6-11 contain the
                 the results of fitting prior to call to MULINA, i.e.
                 l and r hits of unresolved hits used with equal weight
                 (although with lower weight than resolved hits).
    15   R*4   Integral dl (=distance, mm).                 )(from
    16   R*4   Integral density*dl (= material traversed,   )(inter-
                 gm cm**-2).                                )(action
    17   R*4   Integral (-dE/dX)*dl (energy loss, minimum   )(point to
                 ionising particle, GeV).                   )(last
    18   R*4   Integral dl/(absorption length) ('number' of )(point in
                 absorption lengths) assuming a pion.       )(cluster.
    19   R*4   Mu 'goodness' parameter (very crude at this stage).
    20   R*4   Hadron 'leak' probabilty, exp(-(no. of absn. lengths)).
    21   I*4   Associated inner detector track no., if any.
    22   I*4   Associated lead glass cluster no., if any.
    23   R*4   Distance between projections  of  the  mu-track  and  the
               inner detector track, if any, at the position of the flux
               return yoke.
    24   R*4   Ultimate range of a muon with momentum of inner  detector
               track, if any (gm cm**-2).
    25   R*4   rms drift direction deviation.              ) ignore if
    26   R*4   rms longitudinal (wire) direction deviation.) wd 14.LE.0.
    27   R*4   Total weight of x coordinates
    28   R*4   Total weight of y coordinates
    29   R*4   Total weight of z coordinates
    30   I*4   Cluster number.

@ 'MUR1' Bank 4 - The pointer list HCLP.
   -------------------------------------

     HCLP(ICL) points to start of information in HCLIST (bank 5) for
       cluster ICL.
     HCLP(no. of clusters +1) points to word after the last.

  'MUR1' Bank 5 - The hit list HCLIST.
   -----------------------------------

     This gives the hits belonging to each cluster.
     Banks 4 and 5 may be used in conjunction to find the hits belonging
       to each cluster as follows...
      NCLS = no. of clusters (word 2 of bank 0).
      NWHIT = no. of words per hit (word 3 of bank 0).
      NWCL = no. of words per cluster (word 4 of bank 0).
      IPCL = IP3, where IP3 is pointer to bank 3.
      IP11 = 2*IP1, where IP1 is pointer to bank 1.
      IP44 = 2*IP4, where IP4 is pointer to bank 4.
      IP55 = 2*IP5, where IP5 is pointer to bank 5.

  BEGIN LOOP 1 - LOOP OVER CLUSTERS                   **** START LOOP 1
      DO 1000 ICL=1,NCLS
  FIND HITS FOR THIS CLUSTER.  TO GET HITS OF SECONDARY CLUSTER USE
    THE POINTERS OF PRIMARY CLUSTER.
      JCL=ICL
      IALT=IDATA(IPCL+4)
      IF(IALT.NE.0.AND.IALT.LT.ICL)JCL=IALT
      LP=HDATA(IP44+JCL)
      LPNEXT=HDATA(IP44+JCL+1)
  START LOOP 2.                                 ***** START LOOP2.
 2000 CONTINUE
      IHIT=HDATA(IP55+LP)
      IP=NWHIT*(IHIT-1)
  NOW YOU CAN FIND HITS. ADD IP TO IP11 TO GET START OF COORDINATE DATA.
   (DON'T FORGET TO USE APPROPRIATE INFORMATION,E.G. AMBIGUITY FLAGS,
    FOR SECONDARY CLUSTERS, I.E. IF(JCL.LT.ICL) ).
      ....
      ....
  END LOOP 2.                                    ***** END LOOP 2.
 2001 CONTINUE
      LP=LP+1
      IF(LP.LT.LPNEXT)GO TO 2000
  END LOOP 1.                                    ***** END LOOP 1.
 1001 CONTINUE
      IPCL=IPCL+NWCL
 1000 CONTINUE


@ 'MUR1' Bank 6 - The l/r ambiguity of hits in primary clusters
   ------------------------------------------------------------

                  For each hit....
   Word  Type        Contents
   ----  ----        --------
     1   I*2   = -1, 'left' ambiguity selected,
     .    .      +1, 'right' ambiguity selected,
     .    .       0,  both ambiguities equally acceptable.
     .    .

  'MUR1' Bank 7 - The l/r ambiguity of hits in secondary clusters
   --------------------------------------------------------------

                  For each hit....
   Word  Type        Contents
   ----  ----        --------
     1   I*2   = -1, 'left' ambiguity selected,
     .    .      +1, 'right' ambiguity selected,
     .    .       0,  both ambiguities equally acceptable.
     .    .
C   22/03/97 703221635  MEMBER NAME  CMTEMP   (JADEMUS)     FVS
C
C
C
C___MACRO___MTEX1_______________________________________________________
C
C******* NOTE TO ALL MUON PROGRAMMERS : DO NOT ADD ANYMORE
C******* TO /CWORK/ IN THIS MACRO BECAUSE CMUFWORK USES
C******* FOLLOWING LOCATIONS.
C
C                MUREGY   COMMON     SPACE
      COMMON/CWORK/DUMMY(400),IREG(200),RDOTD(200),CINT(3,200),
     *NFLAG(200),ITEM(200)
C
C___MACRO___MTEX1_______________________________________________________
C
C
C
C   22/03/97 703221635  MEMBER NAME  CMUANP   (JADEMUS)     FVS
C--------START OF MACRO CMUANP------------------------------------------
C
C MU ANALYSIS PARAMETERS. FOR DESCRIPTION SEE BLOCK DATA IN MUINI.
C
      COMMON/CMUANP/IMUANP(30)
      EQUIVALENCE (CUTP,IMUANP(1))
      EQUIVALENCE (WRES,IMUANP(2))
      EQUIVALENCE (CUTT,IMUANP(3))
      EQUIVALENCE (CUTL,IMUANP(4))
      EQUIVALENCE (FT,IMUANP(5))
      EQUIVALENCE (FL,IMUANP(6))
      EQUIVALENCE (DRES,IMUANP(7))
      EQUIVALENCE (DLRES,IMUANP(8))
      EQUIVALENCE (FS,IMUANP(9))
      EQUIVALENCE (CUTC,IMUANP(10))
      EQUIVALENCE (FR,IMUANP(11))
      EQUIVALENCE (FAT,IMUANP(12))
      EQUIVALENCE (FAL,IMUANP(13))
      EQUIVALENCE (CUTDT,IMUANP(14))
      EQUIVALENCE (CUTDL,IMUANP(15))
C
C-----END OF MACRO CMUANP-----------------------------------------------
C   22/03/97 703221635  MEMBER NAME  CMUBCS   (JADEMUS)     FVS
C----------START OF MACRO CMUBCS----------------------------------------
      COMMON /BCS/IDATA(1)
      DIMENSION HDATA(1),ADATA(1)
      EQUIVALENCE (HDATA(1),ADATA(1),IDATA(1))
C----------END OF MACRO CMUBCS------------------------------------------
C   22/03/97 703221635  MEMBER NAME  CMUCALIB (JADEMUS)     FVS
C
C
C
C
C
C=======================<< MACRO CMUCALIB >>============================
C
C LAST CHANGE  25/09/79  13.20 UHR   HARRISON PROSPER
C
C BANK NAMES, NUMBERS AND LENGTHS
C
C  NAME/NUMBER LENGTH  CONTENTS
C  MUCD   0      16    VERSION NUMBER AND DESCRIPTION.
C  MUOV   0       3    OVERALL JADE UNIT TRANSLATIONS.
C  MFFI   2     370    FIXED FRAME PARAMETERS.
C  MCFI   3     318    FIXED CHAMBER PARAMETERS.
C  MFSU   4     246    'SURVEY' FRAME PARAMETERS.
C  MCSU   5     634    'SURVEY' CHAMBER PARAMETERS.
C  MCEL   6    2220    'ELECTRONIC' CHAMBER PARAMETERS.
C  MCST   7     317    CHAMBER STATUS WORDS.
C  MUFI   8      36    FILTER (ABSORBER BLOCK) PARAMETERS.
C  MUYO   9      10    SIDE, TOP AND BOTTOM YOKE PARAMETERS.
C  MUEN  10      15    YOKE END-PLUG PARAMETERS.
C
C TOTAL LENGTH 4185 WORDS.
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      COMMON /CALIBR/ LARRY(100),MUCAL(4185)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C
C               NVERSN
      DIMENSION DESCRP(15),HOVALL(6)
C                                                    19 WORDS
C
      EQUIVALENCE ( NVERSN,MUCAL(1) ),( DESCRP(1),MUCAL(2) ),
     *            ( HOVALL(1),MUCAL(17) )
C----------------------------------------------------19 WORDS SO FAR
C
C     HMFFIX(740)                                   370 WORDS
      DIMENSION HMFFIX(740)
      EQUIVALENCE ( HMFFIX(1),MUCAL(20) )
      DIMENSION HFACE(82),HSECT(82),HLAYER(82),HNORM(82),HLONG(82),
     *          HTRANS(82),HAC(82),HAL(82),HUNIT(82)
      EQUIVALENCE (HMFFIX(1),NFRAMS),(HMFFIX(3),HFACE(1)),
     *            (HMFFIX(85),HSECT(1)),(HMFFIX(167),HLAYER(1)),
     *            (HMFFIX(249),HNORM(1)),(HMFFIX(331),HLONG(1)),
     *            (HMFFIX(413),HTRANS(1)),(HMFFIX(495),HAC(1)),
     *            (HMFFIX(577),HAL(1)),(HMFFIX(659),HUNIT(1))
C---------------------------------------------------389 WORDS SO FAR
C
C
C     HMCFIX(636)                                   318 WORDS
      DIMENSION HMCFIX(636)
      EQUIVALENCE ( HMCFIX(1),MUCAL(390) )
      DIMENSION HFR(634)
      EQUIVALENCE (HMCFIX(1),NCHAMS),(HMCFIX(3),HFR(1))
C---------------------------------------------------707 WORDS SO FAR
C
C     HMFSUR(492)                                   246 WORDS
      DIMENSION HMFSUR(492)
      EQUIVALENCE ( HMFSUR(1),MUCAL(708) )
      DIMENSION HDIST(82),HANG(82),HCLLO(82),HCLHI(82),HCTLO(82),
     *          HCTHI(82)
      EQUIVALENCE (HMFSUR(1),HDIST(1)),(HMFSUR(83),HANG(1)),
     *            (HMFSUR(165),HCLLO(1)),(HMFSUR(247),HCLHI(1)),
     *            (HMFSUR(329),HCTLO(1)),(HMFSUR(411),HCTHI(1))
C---------------------------------------------------953 WORDS SO FAR
C
C
C     HMCSUR(1268)                                  634 WORDS
      DIMENSION HMCSUR(1268)
      EQUIVALENCE ( HMCSUR(1),MUCAL(954) )
      DIMENSION HD1(634),HCTW(634)
      EQUIVALENCE (HMCSUR(1),HCTW(1)),(HMCSUR(635),HD1(1))
C--------------------------------------------------1587 WORDS SO FAR
C
C
C     HMCELE(4440)                                 2220 WORDS
      DIMENSION HMCELE(4440)
      EQUIVALENCE ( HMCELE(1),MUCAL(1588) )
      DIMENSION HDTP(634),HLTP(634),HLSF(4,634),HVDRFT(634)
      EQUIVALENCE (HMCELE(1),HVDR),(HMCELE(2),HDTP(1)),
     *            (HMCELE(636),HLTP(1)),(HMCELE(1270),HLSF(1,1)),
     *            (HMCELE(3806),HMCEDM),(HMCELE(3807),HVDRFT(1))
C--------------------------------------------------3807 WORDS SO FAR
C
C
C     HMCSTA(634)                                   317 WORDS
      DIMENSION HMCSTA(634)
      EQUIVALENCE ( HMCSTA(1),MUCAL(3808) )
C--------------------------------------------------4124 WORDS SO FAR
C
C
C     HFILDA(72)                                     36 WORDS
      DIMENSION HFILDA(72)
      EQUIVALENCE ( HFILDA(1),MUCAL(4125) )
      INTEGER*2 HBLLO(6),HBLHI(6),HBTLO(6),HBTHI(6),HBNLIM(36)
      INTEGER*4 IFCIND(6)
      INTEGER*2 HFILDA
      EQUIVALENCE (HBLLO(1),HFILDA(1)),(HBLHI(1),HFILDA(7)),
     *            (HBTLO(1),HFILDA(13)),(HBTHI(1),HFILDA(19)),
     *            (HBNLIM(1),HFILDA(25)),(IFCIND(1),HFILDA(61))
C--------------------------------------------------4160 WORDS SO FAR
C
C
C     HYKNMI(4),HYKNMO(4),HYKLDM(4),HYKTDM(4),BYOKE, 10 WORDS
C     IYKIND
      DIMENSION HYKNMI(4),HYKNMO(4),HYKLDM(4),HYKTDM(4)
      INTEGER*2 HYKTDM,HYKLDM,HYKNMI,HYKNMO
      EQUIVALENCE ( HYKNMI(1),MUCAL(4161) ),
     *            ( HYKNMO(1),MUCAL(4163) ),
     *            ( HYKLDM(1),MUCAL(4165) ),
     *            ( HYKTDM(1),MUCAL(4167) ),
     *            ( BYOKE,MUCAL(4169) ),( IYKIND,MUCAL(4170) )
C--------------------------------------------------4170 WORDS SO FAR
C
C
C    IZEII,IZEIO,IREP1,IREP2,IREP3,IREP4,IXYEP5,     15 WORDS
C    IZOEP1,IZOEP2,IZOEP3,IZOEP4,IZOEP5,CAEP2,
C    IEPIND,IEPSCT
C
      EQUIVALENCE ( IZEII,MUCAL(4171) ),( IZEIO,MUCAL(4172) ),
     *            ( IREP1,MUCAL(4173) ),( IREP2,MUCAL(4174) ),
     *            ( IREP3,MUCAL(4175) ),( IREP4,MUCAL(4176) ),
     *            ( IXYEP5,MUCAL(4177) ),( IZOEP1,MUCAL(4178) ),
     *            ( IZOEP2,MUCAL(4179) ),( IZOEP3,MUCAL(4180) ),
     *            ( IZOEP4,MUCAL(4181) ),( IZOEP5,MUCAL(4182) ),
     *            ( CAEP2,MUCAL(4183) ),( IEPIND,MUCAL(4184) ),
     *            ( IEPSCT,MUCAL(4185) )
C--------------------------------------------------4185 WORDS SO FAR
C
C=======================<< MACRO CMUCALIB >>============================
C
C
C
C
C
C   29/06/82 206291130  MEMBER NAME  CMUED    (JADEMUS)     FORTRAN
C             11.20 29/06/82 HUGH MCCANN
      COMMON /CMUED/NCHAN
C  NCHAN IS THE NO. OF I*2 WORDS WHICH ARE CHANGED BY THE CURRENT EDITS.
C    THIS APPROACH IS ADOPTED IN ORDER TO AVOID INITIALISING LOCATIONS
C  IN /CWORK/ .
C   28/05/82 206291132  MEMBER NAME  CMUEDWRK (JADEMUS)     FORTRAN
C   27/05/82 205271447  MEMBER NAME  CMUEDWRK (S)           FORTRAN
C   17/06/81 106170953  MEMBER NAME  CMUED    (JADEMUS1)    FORTRAN
C   12/06/81 106151117  MEMBER NAME  CMUED    (S)           FORTRAN
C
C LAST CHANGE 11.20 29/06/82 HUGH MCCANN  - FIX BUG..PUT NCHAN IN/CMUED/
C      CHANGE 10.00 28/05/82 HUGH MCCANN  - REDUCE CORE REQUIREMENTS FOR
C                                          PRODUCTION OF UPDATE DATASETS
C
      COMMON /CWORK/KTIME,HUPDAT(8370),HLOC(8370),IALPH,NHMUCA
C       N.B.  SEE  ALSO  CMUED.
C
C  THIS FITS IN WITHIN THE MAXIMUM SIZE OF CWORK....PATREC CWORK
C  IS APPROX 40 KBYTES.
C
      DIMENSION HMUCAL(8370)
      EQUIVALENCE (HMUCAL(1),MUCAL(1))
C  KTIME IS THE TIME AT WHICH THIS SET OF EDITS IS TO BE IMPLEMENTED
C         ( IN SECONDS FROM THE START OF 1979 VIA KTMCON)
C  HUPDAT IS AN ARRAY CONTAINING THESE NEW VALUES  (HUPDAT(I),I=1,NCHAN)
C  HLOC   IS AN ARRAY CONTAINING THEIR HALF-WORD ADDRESSES IN THE MUCAL
C         PART OF THE JADE CALIBRATION COMMON.
C  IALPH  IS THE NUMERICAL INDEX (1-20) OF THE ALPHABETIC IDENTIFIER OF
C         CURRENT MUCONE EDIT (ONLY USED DURING MUCONE OPERATIONS) .
C  NHMUCA IS THE HMUCAL LOCATION OF THE HMUCAL WORD WHICH IS CURRENTLY
C         BEING CHANGED ( ONLY USED DURING MUCONE OPERATIONS ).
C   20/10/81 408081453  MEMBER NAME  CMUFFL   (S)           FORTRAN
C
C LAST CHANGE 15.00 08/08/84 - CKB PWA     - ADD NALLWM,PMXFLG
C      CHANGE 15.00 03/12/83 - HUGH MCCANN - NEW LEAD GLASS PARAMETERS.
C      CHANGE 08.00 12/10/81 - HUGH MCCANN - TO ADD OVLCUT(FOR OVERLAPS)
C      CHANGE 09.33 17/06/81 - JOHN ALLISON - TO ADD RESOLUTIONS.
C
C-------------START OF MACRO CMUFFL-------------------------------------
C
      COMMON / CMUFFL / XYSTEP,RCOILI,RCOILO,PGMID,WMU,WMUSQ,DUMF1(4),
     +                  NSPECI,XPBAR,XPROT,XPION,XKAON,DUMF2(5),
     +                  TBP,GMBP,ABBP,RDBP,DEBP,
     +                  TJETI,GMJETI,ABJETI,RDJETI,DEJETI,
     +                  TJET1,GMJET1,ABJET1,RDJET1,DEJET1,
     +                  TJET2,GMJET2,ABJET2,RDJET2,DEJET2,
     +                  TJET4,GMJET4,ABJET4,RDJET4,DEJET4,
     +                  TJETO,GMJETO,ABJETO,RDJETO,DEJETO,
     +                  TCOIL,GMCOIL,ABCOIL,RDCOIL,DECOIL,
     +                  ZJETE,TJETE,GMJETE,ABJETE,RDJETE,DEJETE,
     +                  ZEPLG,TEPLG,GMEPLG,ABEPLG,RDEPLG,DEEPLG,
     +                  RLG,ZLG,TLG,GMLG,ABLG,RDLG,DELG,
     +                  ZEP,TEP,GMEP,ABEP,RDEP,DEEP,
     +                  GMG(3),ABG(3),RDG(3),DEG(3),
     +                  VDRES,VLRES,
     +                  REST1,REST2,RESTMX,RESL,RESLMX,FACTOR,OVLCUT,
     +                  ZLGHAF,TLGHAF,ZLGC,GMLGC,ABLGC,RDLGC,DELGC,
     +                  NALLWM,PMXFLG
C
C FOR EXPLANATION SEE BLOCK DATA AFTER SUBROUTINE MUFFLE.
C
C--------------END OF MACRO CMUFFL--------------------------------------
C   20/10/81 202041521  MEMBER NAME  CMUFWORK (JADEMUS)     FORTRAN
C   12/10/81 110152138  MEMBER NAME  CMUFWORK (JADEMUS1)    FORTRAN
C-------------START OF MACRO CMUFWORK-----------------------------------
C LAST CHANGED 21.30 15/10/81 HUGH MCCANN  - EXTEND IOVLAP TO ALLOW
C                                            > 10 ASSOC HITS IN MUFFLY.
C LAST CHANGED 10.00 04/10/81 HUGH MCCANN  - FOR USE OF OVERLAP HITS.
C LAST CHANGED 14.46 17/06/81 JOHN ALLISON - TO ADD 'OVER' VARIABLES.
C LAST CHANGED 23.00 09/04/81 HUGH MCCANN - JADEMUS UPDATE.
C
      COMMON /CWORK/X,Y,Z,DCX,DCY,DCZ,P,PSQ,E,ESQ,D,GM,AB,RD,DE,PPIDK,
     *PKDK,PPBPEN,PPPEN,PPIPEN,PKPEN,DUMW1(29),X8,Y8,Z8,DCX8,DCY8,
     *DCZ8,P8,PSQ8,E8,ESQ8,D8,GM8,AB8,RD8,DE8,PPIDK8,PKDK8,PPBPE8,
     *PPPEN8,PPIPE8,PKPEN8,DUMW28(29),X0,Y0,Z0,DCX0,DCY0,DCZ0,P0,PSQ0,
     *E0,ESQ0,D0,GM0,AB0,RD0,DE0,PPIDK0,PKDK0,PPBPE0,PPPEN0,PPIPE0,
     *PKPEN0,DUMW2(29),X9,Y9,Z9,DCX9,DCY9,DCZ9,P9,PSQ9,E9,ESQ9,D9,GM9,
     *AB9,RD9,DE9,PPIDK9,PKDK9,PPBPE9,PPPEN9,PPIPE9,PKPEN9,DUMW29(29),
C-----200 WORDS UP TO HERE----------------------------------------------
     *X1,Y1,Z1,X2,Y2,Z2,XMID,YMID,ZMID,X3,Y3,Z3,D1,D2,D3,DTC,DUMW3(4),
     *DSTEP,GMSTEP,ABSTEP,RDSTEP,DESTEP,DUMW4(15),ADCZ,COSEC,STPINI,CURV
     *,DANG,DX,DY,DZ,R,RSQ,PT,DPINA,DKNA,DUMW5(7),VX,VY,VZ,RMSXY,RMSZ,
     *VTXYD,VTZD,VTXYAN,VTZANG,VMSANG,VMSD,CMS,DUMW6(8),SDS(10),DUMW7(10
C
C    *SDX,SDY,SDZ,SDTXYD,SDTZD,SDTXYA,SDTZAN,SDMSAN,SDMSD,SDCMS,DM7(10),
C
C NOTE ABOVE CARD SHORTENED TO SDS(10),DUMW7(10), SO THAT STATEMENT DOES
C   NOT EXCEED 19 CONTINUATION CARDS.  I THINK SD--'S ARE NOT USED.
     *),CHISQT,NDFT,CHIPRT,CHISTL,NDFTL,CHIPTL,NBADCH,DUMW8(13),IDTRK,
     *NCEPTS,NTHIS,INEFF,NHLAYR,INFLAG,NELIPS,NGLAYR,DUMW9(72),IREG(200)
C-----600 WORDS UP TO HERE----------------------------------------------
     *,RDOTD(200),CINT(3,200),NFLAG(200),ITEM(200),EM(30,30),DALONG(10),
C-----2710 WORDS UP TO HERE---------------------------------------------
     *DEV(30),VXYDA(10),VZDA(10),CXYA(10),CZA(10),INCUT(30),BADA(30),G(2
     *0,20),DEVA(20),IPERM(10),MWORK(20),ITHISA(10),IJA(10),EM1(20,20),E
     *MG(20,20),ELIPSE(700),INCHAM(10),INREG(10),IAPPRO(30),HBADCH(1000)
     *,CTDASH(10),CLDASH(10),VTDASH(10),VLDASH(10),IHTREG(20),LAYRAB,IEF
C-----5421 WORDS UP TO HERE---------------------------------------------
     *FRG(10),DUMW10(69),AAA(10),BBB(10),CCC(10),DDD(10),DOVER,GMOVER,AB
     *OVER,RDOVER,DEOVER,DUMW11(55),X7(50),IHTP(30),IOVLAP(20,20)
C-----6080 WORDS UP TO HERE---------------------------------------------
      DIMENSION ILIPSE(700)
      INTEGER*2 HLIPSE(1400)
      DIMENSION IHTPER(10),IHTEMP(10),IPTEMP(10)
      EQUIVALENCE (ILIPSE(1),HLIPSE(1),ELIPSE(1))
      EQUIVALENCE (IHTP(1) ,IHTPER(1)),
     *            (IHTP(11),IHTEMP(1)),
     *            (IHTP(21),IPTEMP(1))
      LOGICAL INCUT,BADA
C
C-----------------------------------------------------------------------
C
C THE FIRST 50 LOCATIONS ARE RESERVED FOR SPECIAL WORKING VARIABLES.
C THE NEXT 150 LOCATIONS ARE RESERVED FOR COPIES OF THESE SPECIAL
C    VARIABLES, IF, E.G., ONE WISHES TO PRESERVE THEIR VALUES AT
C    THE BEGINNING OF A STEP.
C THE COPIES HAVE A SUFFIX 0 FOR VALUES AT LAST ASSOCIATED HIT.
C THE COPIES HAVE A SUFFIX 7 FOR VALUES AFTER LEAD-GLASS BARREL OR
C    YOKE END-PLUG (X7 IS LATER IN COMMON - SEE BELOW).
C THE COPIES HAVE A SUFFIX 8 FOR VALUES AT LAST BUT ONE GOOD
C    CHAMBER LAYER.
C THE COPIES HAVE A SUFFIX 9 FOR VALUES AT LAST GOOD CHAMBER LAYER.
C NSPECI (SET IN BLOCK DATA BEHIND MUFFLE) IS THE NUMBER OF SUCH
C    VARIABLES ACTUALLY USED.
C
C SPECIAL VARIABLES....
C
C X,Y,Z       = CURRENT COORDINATES.
C DCX,DCY,DCZ = CURRENT DIRECTION COSINES OF TRACK.
C P,PSQ       = CURRENT MOMENTUM (AND ITS SQUARE).
C E,ESQ       = CURRENT ENERGY (AND ITS SQUARE).
C D           = CURRENT DISTANCE FROM VERTEX (MM).
C GM          = MATERIAL TRAVERSED TO CURRENT POSITION (GM CM**-2).
C AB          = ABSORPTION LENGTHS SO FAR.
C RD          = RADIATION LENGTHS SO FAR.
C DE          = ENERGY LOSS (GEV).
C PPIDK       = PROBABILITY OF PION DECAYING TO MUON BEFORE INTERACTING.
C PKDK        = PROBABILITY OF KAON DECAYING TO MUON BEFORE INTERACTING.
C PPBPEN      = ANTI-PROTON ) ( PENETRATION PROBABILITY, I.E. PROBAB-
C PPPEN       = PROTON      ) ( ILITY OF NOT BEING ABSORBED BY A NUCLEUS
C PPIPEN      = PI          ) ( AND NOT DECAYING.  SEE SUBROUTINE
C PKPEN       = K           ) ( MUFFLS FOR FURTHER DETAILS.
C
C-----------------------------------------------------------------------
C
C THE NEXT 200 LOCATIONS ARE RESERVED FOR VARIABLES THAT DO NOT NEED
C    SPECIAL PRESERVATION.
C
C OTHER VARIABLES...
C
C X1,Y1,Z1    = COORDS OF 1ST POINT ON TRACK.
C X2,Y2,Z2    = COORDS OF LAST POINT ON TRACK.
C XMID,YMID,ZMID = COORDS OF MID POINT ON TRACK.
C X3,Y3,Z3    = COORDS OF POINT WHERE TRACK LEAVES MAGNETIC FIELD.
C D1          = DISTANCE TRAVELLED TO (X1,Y1,Z1).
C D2          = DISTANCE TRAVELLED TO (X2,Y2,Z2).
C D3          = DISTANCE TRAVELLED TO (X3,Y3,Z3).
C DTC         = DISTANCE TRAVELLED TO MID-POINT OF TRACK.
C
C DSTEP       = STEP LENGTH THIS STEP (FOR MUFFLU) (MM).
C GMSTEP      = MATERIAL THIS STEP (GM CM**-2).
C ABSTEP      = ABSORPTION LENGTHS THIS STEP.
C RDSTEP      = RADIATION LENGTHS THIS STEP.
C DESTEP      = ENERGY LOSS OF MINIMUM IONISING PARTICLE THIS STEP,
C
C ADCZ        = ABS(DCZ)
C COSEC       = ABS(COSEC(THETA)), WHERE THETA IS ANGLE TO BEAM.
C STPINI      = STEP LENGTH IN INITIAL TRACKING TO COIL OR END PLUG.
C CURV        = CURVATURE FROM 'PATR' BANK.
C DANG        = ANGLE OF TURN FOR EACH STEP.
C DX,DY,DZ    = CHANGES OF X,Y,Z.
C R,RSQ       = DISTANCE FROM BEAM (RADIUS) AND ITS SQUARE.
C PT          = MOMENTUM COMPONENT PERPENDICULAR TO BEAM.
C DPINA       = INTEGRAL OF (DECAY LENGTH)*(PROBABILTY OF NOT INTER-
C                 ACTING) FOR PION. (NO LONGER USED. CAN BE RESURECTED
C DKNA        = SIMILARLY FOR KAON. (BY UN-COMMENTING IN MUFFLS.
C
C VX,VY,VZ    = CURRENT VARIANCES ON X,Y,Z.
C RMSXY       = RMS DEV. NORMAL TO CHORD IN XY FIT.     )
C RMSZ        = RMS Z DEV. IN RZ FIT.                   ) FROM
C VTXYD       = VARIANCE ON DEVIATION NORMAL TO         ) TRACK
C                 TRACK IN XY PLANE AT TRACK CENTRE.    ) RECON-
C VTZD        = VARIANCE ON DEVIATION NORMAL TO         ) STRUCTION
C                 TRACK IN RZ PLANE AT TRACK CENTRE.    ) ERRORS.
C VTXYAN      = VARIANCE ON ANGLE IN XY PLANE (PHI)     )
C VTZANG      = VARIANCE ON ANGLE IN RZ PLANE (THETA)   )
C VMSANG      = VARIANCE OF MULTIPLE SCATTERING ANGLE.
C VMSD        = VARIANCE OF MULTIPLE SCATTERING DEVIATION NORMAL TO
C                 TRACK.
C CMS         = COVARIANCE OF ABOVE TWO QUANTITIES.
C SD--        = CORRESPONDING STANDARD DEVIATIONS (SQUARE ROOTS).
C
C CHISQT      = CHI-SQUARED OF DEVIATION OF HITS FROM   ) FOR
C                 PROJECTED TRACK.                      ) TRANSVERSE
C NDFT        = NUMBER OF DEGREES OF FREEDOM.           ) (DRIFT)
C CHIPRT      = CHI-SQUARED PROBABILITY.                ) COORDS.
C
C CHISTL      = CHI-SQUARED OF DEVIATION OF HITS FROM   ) FOR
C                 PROJECTED TRACK.                      ) TRANSVERSE
C NDFTL       = NUMBER OF DEGREES OF FREEDOM.           ) & LONG'L
C CHIPTL      = CHI-SQUARED PROBABILITY.                ) COORDS.
C
C NBADCH      = NO. OF BAD CHAMBERS (AS DEFINED BY HMCSTA).
C
C IDTRK       = INNER DETECTOR TRACK NUMBER.
C NCEPTS      = NUMBER OF INTERCEPTS FOUND BY MUREGY.
C NTHIS       = NUMBER OF MUON HITS ASSOCIATED WITH PROJECTED INNER
C                DETECTOR TRACK.
C INEFF       = NUMBER OF INEFFICIENCIES IN MUON SYSTEM FOUND THIS TRACK
C NHLAYR      = NUMBER WHICH REPRESENTS NUMBER OF MUON LAYERS WITH
C                ASSOCIATED MUON HITS. ADD 1 FOR THE INNER LAYER AND
C                2 FOR EACH SUBSEQUENT LAYER.
C INFLAG      = 1 IF LAST LAYER INTERCEPTED WAS INEFFICIENT.
C NELIPS      = NUMBER OF MULTIPLE SCATTERING ELIPSES ENTERED IN ELIPSE.
C NGLAYR      = AS NHLAYR, BUT FOR 'GOOD' LAYERS WHETHER THERE WAS AN
C                ASSOCIATED HIT OR NOT.
C
C-----------------------------------------------------------------------
C
C THE NEXT 1400 LOCATIONS USED BY MUREGY (CALCULATES REGION INTERCEPTS).
C
C IREG        = REGION NUMBER.                           )(IN PAIRS,I.E.
C RDOTD       = R.DOT.(DIR. COSINES) FOR EACH INTERCEPT. )( 2 INTERCEPTS
C CINT        = X,Y,Z COORDINATES OF EACH INTERCEPT.     )( PER REGION.
C NFLAG       = USED INTERNALLY BY MUREGY.
C ITEM        = USED INTERNALLY BY MUREGY.
C
C-----------------------------------------------------------------------
C
C THE NEXT 3600 LOCATIONS USED BY MUFFLE, ETC., TO ACCUMULATE
C   QUANTITIES NEEDED FOR CALCULATING CHI-SQUARED. IN THE DESCRIPTION
C   BELOW I,J GO AS FOLLOWS..
C     1   LEFT DRIFT COORDINATE    )  OF 1ST
C     2   RIGHT DRIFT COORDINATE   )  ASSOCIATED
C     3   LONGITUDINAL COORDINATE  )  HIT
C     4    ETC. FOR NEXT ASSOCIATED HIT.
C   K REFERS TO K'TH ASSOCIATED HIT.
C
C EM(I,J)     = 'ERROR MATRIX', I.E. VARIANCE (I=J) OR COVARIANCE (I.NE.
C                J) FOR COORDINATE I,J.
C DALONG(K)   = DISTANCE OF ASSOCIATED HIT K ALONG TRACK.
C DEV(I)      = DEVIATION OF COORDINATE I FROM EXTRAPOLATED INNER
C                DETECTOR TRACK.
C VXYDA(K)    = VARIANCE ON DEVIATION IN XY PLANE FOR HIT K.
C VZDA(K)     = VARIANCE ON DEVIATION IN RZ PLANE FOR HIT K.
C CXYA(K)     = COVARIANCE BETWEEN DEVIATION AND ANGLE IN XY PLANE.
C CZA(K)      = COVARIANCE BETWEEN DEVIATION AND ANGLE IN RZ PLANE.
C INCUT(I)    = .TRUE. IF COORDINATE I IS IN CUT (SEE MUFFLY).
C BADA(I)     = .TRUE. IF COORDINATE I IS BAD.
C G           = INVERSE ERROR MATRIX.
C DEVA        = CORRESPONDING DEVIATIONS.
C IPERM(K)    = USED TO PERMUTE LEFT/RIGHT AMBIGUITIES.
C MWORK       = USED BY MATRIX INVERTING ROUTINE AS WORKING SPACE.
C ITHISA(K)   = HIT NUMBER OF K'TH ASSOCIATED HIT.
C IJA(K)      = INDEX FOR K'TH ASSOCIATED HIT FOR ENTRY IN BANKS 2 & 3.
C EM1         = COPY OF RELEVANT PART OF ERROR MATRIX FOR TEST PURPOSES.
C EMG         = PRODUCT OF ERROR MATRIX AND ITS INVERSE. SHOULD BE UNIT
C                MATRIX. USED FOR TEST PURPOSES.
C ELIPSE      = TEMPORARY STORAGE FOR MULTIPLE SCATTERING ELIPSES.
C INCHAM      = LIST OF INEFFICIENT CHAMBERS ON THIS TRACK.
C INREG       = LIST OF INEFFICIENT REGIONS (PLANES) ON THIS TRACK.
C IAPPRO      = USED AS WORKING SPACE IN MUFFLX.
C HBADCH      = LIST OF BAD CHAMBERS (AS DEFINED BY HMCSTA).
C CTDASH(K)   = TRANSVERSE COORD. ESTIMATED FROM I.D. PROJECTION.
C CLDASH(K)   = LONGITUD'L COORD. ESTIMATED FROM I.D. PROJECTION.
C VTDASH(K)   = VARIANCE ON CTDASH.
C VLDASH(K)   = VARIANCE ON CLDASH (FOR K'TH ASSOCIATED HIT).
C
C-----------------------------------------------------------------------
C
C THE NEXT  380 LOCATIONS USED FOR VARIOUS IMPROVEMENTS TO MUFFLE, ETC.
C
C IHTREG      = FOR EACH ASSOCIATED HIT, THE REGION NO. IN WHICH
C               THE HIT OCCURS.
C LAYRAB      = ABSOLUTE NO. OF LAYERS WHICH HAVE ASSOCIATED HITS.
C IEFFRG      = LIST OF REGION NOS. OF THOSE REGIONS WHICH HAVE AN
C               ASSOCIATED HIT.
C   IHTPER = FLAG FOR EACH ASSOCIATED HIT TO TELL IF IT'S TO
C            GO FORWARD INTO THE CHI**2 CALCULATION . ONLY ONE
C            HIT PER LAYER IS TO BE USED ,APART FROM OVERLAP HITS.
C   IHTEMP = LIST OF HITS WHICH ARE TEMPORARILY IN THE HIT PERMUTATION
C            DUE TO OVERLAPS WITH THE CURRENT AMBIGUITYOF ONE OF THE
C            HITS IN IHTPER.
C   IPTEMP =  AS IHTEMP, BUT FOR L/R AMBIGUITIES.
C IOVLAP   =  MATRIX OF OVERLAP HITS. IOVLAP(I,J)=1 MEANS THAT THERE
C             IS AN OVERLAP BETWEEN ASSOCIATED HITS I & J ( I & J BOTH
C             IN THE RANGE 1 TO NTHIS ).
C AAA(K)      = TRANSFORMATION COEFF. A FOR K'TH ASSOCD. HIT.
C BBB(K)      = TRANSFORMATION COEFF. B FOR K'TH ASSOCD. HIT.
C CCC(K)      = TRANSFORMATION COEFF. C FOR K'TH ASSOCD. HIT.
C DDD(K)      = TRANSFORMATION COEFF. D FOR K'TH ASSOCD. HIT.
C
C DOVER, ETC. = STORES QUANTITIES LEFT OVER AFTER LEAVING LAST ABSORBER.
C
C X7          = VALUES OF SPECIAL VARIABLES AFTER LEAD-GLASS BARREL OR
C               YOKE END-PLUG - USED FOR RE-FITTING, ETC.
C
C-------------END OF MACRO CMUFWORK-------------------------------------
C   12/10/81 110152138  MEMBER NAME  CMUREG   (JADEMUS1)    FORTRAN
C------------START OF MACRO CMUREG--------------------------------------
C
      COMMON /CMUREG/NREGS,XRLO(100),XRHI(100),YRLO(100),YRHI(100),
     * ZRLO(100),ZRHI(100),HRMASK(100),HRFACE(100),HRTYPE(100),
     * HRORI(100),HRFIRS(100),HRLAST(100)
C
C NREGS IS NUMBER OF REGIONS.
C XRLO ETC. ARE REGION BOUNDARIES.
C HRMASK =1 (FACE 1), =2 (FACE 2), =4 (FACE 3), =8 (FACE 4), ETC.
C HRFACE = FACE NUMBER, =1-6 FOR -X,+X,-Y,+Y,-Z,+Z.
C HRTYPE =1, MU CHAMBER SENSITIVE REGION,
C        =2, CONCRETE REGION,
C        =3, IRON REGION,
C        =4, LEAD GLASS. (IRTYPE=4 USED ONLY IN MUFFLD.)
C HRORI  = ORIENTATION OF NORMAL, =1 (|| X), =2 (||Y), =3 (|| Z).
C HRFIRS = FIRST CHAMBER NUMBER.
C HRLAST = LAST CHAMBER NUMBER.
C
C------------END OF MACRO CMUREG----------------------------------------
C   12/10/81 110152138  MEMBER NAME  CMUSER   (JADEMUS1)    FORTRAN
C------START OF MACRO CMUSER--------------------------------------------
C
      COMMON /CMUSER/N1,N2,NPR,IPRINT,IPRINS,IREC,IHISTS,IJOIN,IRUN,
     * ITLFT,IOUT,LUNC,LUNE,LUNO
C
C------END OF MACRO MUSER-----------------------------------------------
C   12/10/81 110152138  MEMBER NAME  CMUSTAT  (JADEMUS1)    FORTRAN
C----------- START OF MACRO CMUSTAT ------------------------------------
C   LAST CHANGE 12.47 15/06/79 JOHN ALLISON.
C      /CMUSTT/
C
      COMMON / CMUSTT / MUTIT(100),NMU(100)
      REAL*8 MUTIT, MUT1(50), MUT2(50)
      EQUIVALENCE(MUTIT(1),MUT1(1)),(MUTIT(51),MUT2(1))
C
C  NMU ARE USED FOR STATISTICS COUNTING IN THE MUON ROUTINES
C
C------------ END OF MACRO CMUSTAT -------------------------------------
C   12/10/81 110152138  MEMBER NAME  CUNIT    (JADEMUS1)    FORTRAN
C---------------START OF MACRO CUNIT (LOGICAL UNIT NUMBERS)-------------
C
      COMMON /CUNIT/LUNMUC,LUNMUE
C
C LUNMUC  LOGICAL UNIT FOR MU CALIBRATION DATA.
C LUNMUE  LOGICAL UNIT FOR MU CALIBRATION DATA EDIT FILE.
C
C---------------END OF MACRO CUNIT--------------------------------------
C   12/10/81 110152138  MEMBER NAME  JCNCOM   (JADEMUS1)    FORTRAN
@-----------------------------------------------------------------------
COMMON Descriptions. ---------------------------------------------------
------------------------------------------------------------------------

   Common block CALIBR can be easily incorporated using %MACRO CMUCALIB
   on F22ALL.JADEMUS(CMUCALIB) .

      COMMON /CALIBR/ LARRY(100),MUCAL(4185)




                NVERSN
      DIMENSION DESCRP(15),HOVALL(6)
                                                     19 WORDS

      EQUIVALENCE ( NVERSN,MUCAL(1) ),( DESCRP(1),MUCAL(2) ),
     *            ( HOVALL(1),MUCAL(17) )
 ----------------------------------------------------19 WORDS SO FAR

      HMFFIX(740)                                   370 WORDS
      DIMENSION HMFFIX(740)
      EQUIVALENCE ( HMFFIX(1),MUCAL(20) )
      DIMENSION HFACE(82),HSECT(82),HLAYER(82),HNORM(82),HLONG(82),
     *          HTRANS(82),HAC(82),HAL(82),HUNIT(82)
      EQUIVALENCE (HMFFIX(1),NFRAMS),(HMFFIX(3),HFACE(1)),
     *            (HMFFIX(85),HSECT(1)),(HMFFIX(167),HLAYER(1)),
     *            (HMFFIX(249),HNORM(1)),(HMFFIX(331),HLONG(1)),
     *            (HMFFIX(413),HTRANS(1)),(HMFFIX(495),HAC(1)),
     *            (HMFFIX(577),HAL(1)),(HMFFIX(659),HUNIT(1))
 ---------------------------------------------------389 WORDS SO FAR


      HMCFIX(636)                                   318 WORDS
      DIMENSION HMCFIX(636)
      EQUIVALENCE ( HMCFIX(1),MUCAL(390) )
      DIMENSION HFR(634)
      EQUIVALENCE (HMCFIX(1),NCHAMS),(HMCFIX(3),HFR(1))
 ---------------------------------------------------707 WORDS SO FAR

      HMFSUR(492)                                   246 WORDS
      DIMENSION HMFSUR(492)
      EQUIVALENCE ( HMFSUR(1),MUCAL(708) )
      DIMENSION HDIST(82),HANG(82),HCLLO(82),HCLHI(82),HCTLO(82),
     *          HCTHI(82)
      EQUIVALENCE (HMFSUR(1),HDIST(1)),(HMFSUR(83),HANG(1)),
     *            (HMFSUR(165),HCLLO(1)),(HMFSUR(247),HCLHI(1)),
     *            (HMFSUR(329),HCTLO(1)),(HMFSUR(411),HCTHI(1))
 ---------------------------------------------------953 WORDS SO FAR


      HMCSUR(1268)                                  634 WORDS
      DIMENSION HMCSUR(1268)
      EQUIVALENCE ( HMCSUR(1),MUCAL(954) )
      DIMENSION HD1(634),HCTW(634)
      EQUIVALENCE (HMCSUR(1),HCTW(1)),(HMCSUR(635),HD1(1))
 --------------------------------------------------1587 WORDS SO FAR

@
      HMCELE(4440)                                 2220 WORDS
      DIMENSION HMCELE(4440)
      EQUIVALENCE ( HMCELE(1),MUCAL(1588) )
C ********* N.B. HMCEDM IS JUST A 'FILLER' . **********
      DIMENSION HDTP(634),HLTP(634),HLSF(4,634),HVDRFT(634)
      EQUIVALENCE (HMCELE(1),HVDR),(HMCELE(2),HDTP(1)),
     *            (HMCELE(636),HLTP(1)),(HMCELE(1270),HLSF(1,1)),
     *            (HMCELE(3806),HMCEDM),(HMCELE(3807),HVDRFT(1))
 --------------------------------------------------3807 WORDS SO FAR

      HMCSTA(634)                                   317 WORDS
      DIMENSION HMCSTA(634)
      EQUIVALENCE ( HMCSTA(1),MUCAL(3808) )
 --------------------------------------------------4124 WORDS SO FAR

      HFILDA(72)                                     36 WORDS
      DIMENSION HFILDA(72)
      EQUIVALENCE ( HFILDA(1),MUCAL(4125) )
      INTEGER*2 HBLLO(6),HBLHI(6),HBTLO(6),HBTHI(6),HBNLIM(36)
      INTEGER*4 IFCIND(6)
      INTEGER*2 HFILDA
      EQUIVALENCE (HBLLO(1),HFILDA(1)),(HBLHI(1),HFILDA(7)),
     *            (HBTLO(1),HFILDA(13)),(HBTHI(1),HFILDA(19)),
     *            (HBNLIM(1),HFILDA(25)),(IFCIND(1),HFILDA(61))
 --------------------------------------------------4160 WORDS SO FAR


      HYKNMI(4),HYKNMO(4),HYKLDM(4),HYKTDM(4),BYOKE, 10 WORDS
      IYKIND
      DIMENSION HYKNMI(4),HYKNMO(4),HYKLDM(4),HYKTDM(4)
      INTEGER*2 HYKTDM,HYKLDM,HYKNMI,HYKNMO
      EQUIVALENCE ( HYKNMI(1),MUCAL(4161) ),
     *            ( HYKNMO(1),MUCAL(4163) ),
     *            ( HYKLDM(1),MUCAL(4165) ),
     *            ( HYKTDM(1),MUCAL(4167) ),
     *            ( BYOKE,MUCAL(4169) ),( IYKIND,MUCAL(4170) )
 --------------------------------------------------4170 WORDS SO FAR


     IZEII,IZEIO,IREP1,IREP2,IREP3,IREP4,IXYEP5,     15 WORDS
     IZOEP1,IZOEP2,IZOEP3,IZOEP4,IZOEP5,CAEP2,
     IEPIND,IEPSCT

      EQUIVALENCE ( IZEII,MUCAL(4171) ),( IZEIO,MUCAL(4172) ),
     *            ( IREP1,MUCAL(4173) ),( IREP2,MUCAL(4174) ),
     *            ( IREP3,MUCAL(4175) ),( IREP4,MUCAL(4176) ),
     *            ( IXYEP5,MUCAL(4177) ),( IZOEP1,MUCAL(4178) ),
     *            ( IZOEP2,MUCAL(4179) ),( IZOEP3,MUCAL(4180) ),
     *            ( IZOEP4,MUCAL(4181) ),( IZOEP5,MUCAL(4182) ),
     *            ( CAEP2,MUCAL(4183) ),( IEPIND,MUCAL(4184) ),
     *            ( IEPSCT,MUCAL(4185) )
 --------------------------------------------------4185 WORDS SO FAR
@  DEFINITION OF CALIBRATION TERMS

  NVERSN           Version number.
  DESCRP           Description.

  HOVALL(IUNIT)    Overall translation of each unit along rails.
                     IUNIT=1 - far  side (-x) wall,
                     IUNIT=2 - near side (+x, rucksack) wall,
                     IUNIT=3 - magnet  (All translations are relative to
                               this so HOVALL(3) should ALWAYS be zero.)
                     IUNIT=4 - far  side (-x) arch,
                     IUNIT=5 - near side (+x) arch.

  IFRAME           Frame number.
  ICHAM            Chamber number.
  NFRAMS           Number of frames.
  NCHAMS           Number of chambers.

  Fixed Data For Each Frame....

  HFACE(IFRAME)    1-6 for -x,+x,-y,+y,-z,+z respectively.
                   =0 if frame not present.
  HSECT(IFRAME)    Section number of section to which frame belongsh
  HLAYER(IFRAME)   1-5 numbering from the interaction point outwardsh
                     =1, inside return yoke
                     =2-5 for layers on concrete,
  HNORM(IFRAME)    =1,normal of plane parallel to x-axis
                   =2,normal of plane parallel to y-axis
                   =3,normal of plane parallel to z-axis
  HLONG(IFRAME)    =1,wire nominally parallel to x-axis
                   =2,wire nominally parallel to y-axis
                   =3,wire nominally parallel to z-axis
  HTRANS(IFRAME)   =1,drift field parallel to x-axis
                   =2,drift field parallel to y-axis
                   =3,drift field parallel to z-axis
  HAC(IFRAME)      Chamber number of first chamber in frame.
  HAL(IFRAME)      Chamber number of last chamber in frame.
  HUNIT(IFRAME)    Unit to which this frame belongs.

  Survey Data For Each Frame....

  HDIST(IFRAME)     The coordinate of the central plane where the axis
                     specified by HNORM(IFRAME) cuts the plane(units mm)
  HANG(IFRAME)      The angle between the wire and the axis specified by
                     HLONG(IFRAME)  (units 1/10 mr)
  HCLLO(IFRAME)     Lower logitudinal coordinate limit
  HCLHI(IFRAME)     Upper logitudinal coordinate limit
  HCTLO(IFRAME)     Lower transverse coordinate limit
  HCTHI(IFRAME)     Upper transverse coordinate limit
                    (The above 4 variables apply to total sensitive area
                       of plane.They are in mm )

  Fixed Data For Each Wire....

  HFR(ICHAM)        Frame number for this chamber.

  Survey Data For Each Wire....

  HD1(ICHAM)        Amount to be added to HDIST(IFRAME) to get to
                     coordinate of the chamber.   (units mm)
@ HCTW(ICHAM)       Tranverse coordinate of each wire.  (units mm)

  Electronic Data For Chambers...

  HDTP(ICHAM)      Drift time pedestal (trans. clock units, ca. 60 ns.)
  HLTP(ICHAM)      Longitudinal time pedestal (in long. clock units,
                     ca. 0.5 ns. or 50 mm.)
  HLSF(J,ICHAM)    Long. scale factor for j'th hit
                     (units (1/100mm)/long. clock unit)
  HVDRFT(ICHAM)    Drift velocity (microns per clock unit (50 ns)).

                   The above data are used to convert signals to coor-
                     -dinates relative to the chamber as follows..
                   ICT = ( HVDRFT * (ITD-HDTP) )/1000
                   ICL = ( HLSF   * (ITL-HLTP) )/100
                     where ICT,ICL are coordinates in mm,
                     ITD is drift time in trans. clock units, and
                     ITL is long. time diff. in long. clock units.


  Status Data For The Chambers

  HMCSTA(ICHAM)    =0    if chamber ok
                   .NE.0 if chamber u/s for any reason.

------------------------------------------------------------------------
#include "cmutny.for"
    /CMUTNY/


  Condensed mu-filter paarameters for use by approximate signal to
    coordinate conversion subroutine MUTINY as used at the NORD.

  HPLANS      No. of chamber planes.
  HVDRAV      Average drift velocity.
  HDTPAV      Average drift time pedestal.
  HLTPAV      Average longitudinal time difference pedestal.
  HLSFAV      Average longitudinal scaling factor.

  FOR EACH CHAMBER PLANE...

  HLY         Layer number.
  HOR         Orientation parameter:
              =1, wires parallel to beam, and normal parallel to
                    x-axis - faces 1(-x) and 2(+x).
              =2, wires parallel to beam, and normal parallel to
                    y-axis - faces 3(-y) and 4(+y).
              =3, wires vertical, and normal parallel to z-axis -
                    faces 5(-z) and 6(+z).
  HC1         First chamber number.
  HCN0        Normal       )
  HCL0        Longitudinal )  coordinate of 'origin' of chamber plane.
  HCT0        Transverse   )
  HSP         Average spacing of chambers.

    (The 'origin' is at one end of the wire of the first chamber in the
  plane.  The end is that with the lowest longitudinal coordinate.)

      COMMON /CMUTNY/HPLANS,HVDRAV,HDTPAV,HLTPAV,HLSFAV,
     * HLY(48),HOR(48),HC1(48),HCN0(48),HCL0(48),HCT0(48),HSP(48)

------------------------------------------------------------------------

 MACRO CMUANP.
 -------------
  Mu analysis parameters filled by block data after MUINI.

      COMMON/CMUANP/IMUANP(30)
      DIMENSION AMUANP(30),HMUANP(60)
      EQUIVALENCE (IMUANP(1),AMUANP(1),HMUANP(1))

------------------------------------------------------------------------

 COMMON /CMUPRN/.
 ---------------

      COMMON /CMUPRN/MUPRIN

      MUPRIN=0 to suppress all printing of mu messages,
            .GE.1 to get mu error messages,
            .GE.2 to get mu information messages,
            .GE.10 to get full mu calibration printout (about 10 pages).

------------------------------------------------------------------------
End of COMMON Descriptions. --------------------------------------------
------------------------------------------------------------------------
