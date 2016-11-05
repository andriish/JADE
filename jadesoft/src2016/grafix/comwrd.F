C   01/11/84 601301622  MEMBER NAME  COMWRD   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE COMWRD
C-----------------------------------------------------------------------
C
C    AUTHOR:   C. BOWDERY  17/05/84 :  DEFINE COMMAND WORDS DICTIONARY
C
C       MOD:   J. HAGEMANN 19/10/84 :  WORD 'AMPLITUDES' DEFINED
C  LAST MOD:   J. HAGEMANN 30/01/86 :  WORDS 'VTXC' AND 'PICK' DEFINED
C
C     THIS ROUTINE SETS UP A LIST OF WORDS THAT DEFINE THE
C     COMMAND LANGUAGE OF THE JADE GRAPHICS PROGRAM.
C     MACRO CGRCOM  CONTAINS VARIABLES WHICH STORE THE POINTERS
C     TO THESE COMMAND WORDS AND IS NEEDED BY S/R DEFCMD.
C
C     SUGGESTION: ADD NEW COMMAND WORDS IN ALPHABETICAL ORDER
C
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cgrcom.for"
C
C------------------  C O D E  ------------------------------------------
C
      CALL DEFWRD( HADC,   'ADC^'           )
      CALL DEFWRD( HADD,   'ADD^'           )
      CALL DEFWRD( HALTER, 'ALTER^'         )
      CALL DEFWRD( HAMPL,  'AMPLITUDES^'    )
      CALL DEFWRD( HAXES,  'AXES^'          )
      CALL DEFWRD( HBANK,  'BANK^'          )
C
      CALL DEFWRD( HCAPS,  'CAPS^'          )
      CALL DEFWRD( HCHAM,  'CHAMBER^'       )
      CALL DEFWRD( HCHANG, 'CHANGE^'        )
      CALL DEFWRD( HCLUST, 'CLUSTERS^'      )
      CALL DEFWRD( HCOMMD, 'COMMANDS^'      )
C
      CALL DEFWRD( HCOMEN, 'COMMENTS^'      )
      CALL DEFWRD( HCONT,  'CONTINUE^'      )
      CALL DEFWRD( HCOPY,  'COPY^'          )
      CALL DEFWRD( HCOUNT, 'COUNTERS^'      )
      CALL DEFWRD( HCWORK, 'CWORK^'         )
C
      CALL DEFWRD( HCYL,   'CYLINDER^'      )
      CALL DEFWRD( HDATA,  'DATA^'          )
      CALL DEFWRD( HDEDX,  'DEDX^'          )
      CALL DEFWRD( HDEL,   'DELETE^'        )
      CALL DEFWRD( HDETLS, 'DETAILS^'       )
C
      CALL DEFWRD( HDET,   'DETECTOR^'      )
      CALL DEFWRD( HDISP,  'DISPLAY^'       )
      CALL DEFWRD( HDRAW,  'DRAW^'          )
      CALL DEFWRD( HEDIT,  'EDIT^'          )
      CALL DEFWRD( HENDC,  'ENDCAPS^'       )
C
      CALL DEFWRD( HEVENT, 'EVENT^'         )
      CALL DEFWRD( HEXT,   'EXTERNAL^'      )
      CALL DEFWRD( HFAMP,  'FAMP^'          )
      CALL DEFWRD( HFILTR, 'FILTER^'        )
      CALL DEFWRD( HFIND,  'FIND^'          )
C
      CALL DEFWRD( HFIT,   'FIT^'           )
      CALL DEFWRD( HFLAG,  'FLAG^'          )
      CALL DEFWRD( HFLASH, 'FLASH^'         )
      CALL DEFWRD( HFORWD, 'FORWARD^'       )
      CALL DEFWRD( HFRONT, 'FRONT^'         )
C
      CALL DEFWRD( HFW,    'FW^'            )
      CALL DEFWRD( HGLASS, 'GLASS^'         )
      CALL DEFWRD( HGOOD,  'GOOD^'          )
      CALL DEFWRD( HHARD,  'HARDCOPY^'      )
      CALL DEFWRD( HHELP,  'HELP^'          )
C
      CALL DEFWRD( HID,    'ID^'            )
      CALL DEFWRD( HINNER, 'INNER^'         )
      CALL DEFWRD( HJET,   'JET^'           )
      CALL DEFWRD( HJETC,  'JETC^'          )
      CALL DEFWRD( HJUMP,  'JUMP^'          )
C
      CALL DEFWRD( HLEAD,  'LEAD^'          )
      CALL DEFWRD( HLEVEL, 'LEVEL^'         )
      CALL DEFWRD( HLG,    'LG^'            )
      CALL DEFWRD( HLIMIT, 'LIMITS^'        )
      CALL DEFWRD( HLIST,  'LIST^'          )
C
      CALL DEFWRD( HMAP,   'MAP^'           )
      CALL DEFWRD( HMASS,  'MASS^'          )
      CALL DEFWRD( HMENU,  'MENU^'          )
      CALL DEFWRD( HMUON,  'MUON^'          )
      CALL DEFWRD( HMUPT,  'MUPT^'          )
C
      CALL DEFWRD( HNEWS,  'NEWS^'          )
      CALL DEFWRD( HNEXT,  'NEXT^'          )
      CALL DEFWRD( HNO,    'NO^'            )
      CALL DEFWRD( HNORD,  'NORD50^'        )
      CALL DEFWRD( HNR,    'NR^'            )
C
      CALL DEFWRD( HNUMB,  'NUMBER^'        )
      CALL DEFWRD( HOPT,   'OPTIONS^'       )
      CALL DEFWRD( HPARAM, 'PARAMETERS^'    )
      CALL DEFWRD( HPARMS, 'PARMS^'         )
      CALL DEFWRD( HPATR,  'PATR^'          )
C
      CALL DEFWRD( HPATRN, 'PATTERN^'       )
      CALL DEFWRD( HPHI,   'PHI^'           )
      CALL DEFWRD( HPICK,  'PICK^'          )
      CALL DEFWRD( HPLOTS, 'PLOTS^'         )
      CALL DEFWRD( HPROJ,  'PROJECTIONS^'   )
      CALL DEFWRD( HQ,     'Q^'             )
C
      CALL DEFWRD( HRECAL, 'RECALIBRATE^'   )
      CALL DEFWRD( HRECOG, 'RECOGNITION^'   )
      CALL DEFWRD( HREGN,  'REGION^'        )
      CALL DEFWRD( HRESET, 'RESET^'         )
      CALL DEFWRD( HRESLT, 'RESULTS^'       )
C
      CALL DEFWRD( HRETRN, 'RETURN^'        )
      CALL DEFWRD( HR,     'R^'             )
      CALL DEFWRD( HRZ,    'RZ^'            )
      CALL DEFWRD( HSAVE,  'SAVE^'          )
      CALL DEFWRD( HSCALE, 'SCALE^'         )
C
      CALL DEFWRD( HSET,   'SET^'           )
      CALL DEFWRD( HSHOW,  'SHOW^'          )
      CALL DEFWRD( HSIDE,  'SIDE^'          )
      CALL DEFWRD( HSTAND, 'STANDARD^'      )
      CALL DEFWRD( HSTAT,  'STATUS^'        )
C
      CALL DEFWRD( HTAGG,  'TAGGING^'       )
      CALL DEFWRD( HTOF,   'TOF^'           )
      CALL DEFWRD( HTOP,   'TOP^'           )
      CALL DEFWRD( HTRACK, 'TRACK^'         )
      CALL DEFWRD( HTRUE,  'TRUE^'          )
C
      CALL DEFWRD( HTRIG,  'TRIGGER^'       )
      CALL DEFWRD( HT1,    'T1^'            )
      CALL DEFWRD( HT2,    'T2^'            )
      CALL DEFWRD( HT3,    'T3^'            )
      CALL DEFWRD( HVERTX, 'VERTEX^'        )
C
      CALL DEFWRD( HVIEW,  'VIEW^'          )
      CALL DEFWRD( HVTXC,  'VTXC^'          )
      CALL DEFWRD( HWRK,   'WORK^'          )
      CALL DEFWRD( HWRITE, 'WRITE^'         )
      CALL DEFWRD( HXY,    'XY^'            )
C
      CALL DEFWRD( HXZ,    'XZ^'            )
      CALL DEFWRD( HZ,     'Z^'             )
C
      RETURN
      END
