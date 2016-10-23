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
