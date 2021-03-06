C   01/11/84 606091716  MEMBER NAME  DEFCMD0  (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE DEFCMD
C-----------------------------------------------------------------------
C
C    AUTHOR:   C. BOWDERY  11/05/84 :  DEFINE COMMANDS
C
C       MOD:   C. BOWDERY  20/06/84 :  CSLT CHANGED TO CSTL
C       MOD:   C. BOWDERY   5/07/84 :  COMMAND 'MORE' ADDED
C       MOD:   J. HAGEMANN 19/10/84 :  COMMANDS 'VC' AND 'VAC' ADDED
C       MOD:   C. BOWDERY  15/07/85 :  ALPHABETIC ORDERING NOW
C       MOD:   C. BOWDERY  17/07/85 :  SETF AND CPAR OUT.Q AND QX IN
C       MOD:   C. BOWDERY  18/07/85 :  COMMAND MACRO ADDED
C       MOD:   C. BOWDERY   3/08/85 :  COMMAND EDITMAC ADDED
C       MOD:   C. BOWDERY   5/08/85 :  COMMANDS RENAMAC,DELMAC ADDED
C       MOD:   C. BOWDERY  12/08/85 :  COMMAND LIM REMOVED
C       MOD:   C. BOWDERY  17/08/85 :  SYNONYMS ADDED
C       MOD:   C. BOWDERY  20/12/85 :  HLP / LASER ADDED
C  LAST MOD:   J. HAGEMANN 04/02/86 :  VTXC AND PICK ADDED
C
C     THIS ROUTINE SETS UP A LIST OF COMMANDS THAT IS USED BY
C     KOMMAN, THE COMMAND INTERPRETER OF THE JADE GRAPHICS PROGRAM.
C     EACH COMMAND HAS A SHORT FORM (UP TO 8 LETTERS), AN IDENTITY
C     NUMBER WHICH DOES NOT HAVE TO BE UNIQUE TO A GIVEN STRING, NUMBER
C     OF ARGUMENTS ALLOWED AND UP TO 3 ASSOCIATED COMMAND WORD POINTERS.
C
C     THUS A COMMAND CAN RECOGNISED FROM ITS ABBREVIATION, E.G. RA
C     OR FROM ITS FULL FORM, E.G. FRONT ID VIEW.
C     ALTERNATIVE WORDS MAY BE RECOGNISED LIKE 'INNER DETECTOR' OR
C     SHORT FORMS LIKE 'FR' WHERE THIS IS UNAMBIGUOUS.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cgrcom.for"
C
C------------------  C O D E  ------------------------------------------
C
C                            FIRST DEFINE THE COMMAND WORDS.
C                            THIS INVOLVES CREATING A DICTIONARY OF
C                            ENGLISH WORDS LIKE 'FRONT', 'VIEW', ETC.
C                            COMMONS CGRCO1/2/3/4 ARE FILLED WITH
C                            POINTERS TO THE COMMAND WORDS.
C
      CALL COMWRD
C
C                            NOW DEFINE THE COMMANDS.
C                            THESE CONSIST OF A NUMBER, SHORT FORM, MAX.
C                            NUMBER OF ARGUMENTS AND
C                            UP TO THREE COMMAND WORD POINTERS.
C
C                            COMMANDS 1 TO 100 ARE DISPLY COMMANDS,
C                            OF WHICH COMMANDS 1 TO 30 ARE VIEWS.
C                            SOME COMMANDS ARE SYNONYMS FOR OTHERS.
C
C                            FOR THE HELP SYSTEM, COMMANDS ARE NOW
C                            IN ALPHABETIC ORDER.
C
C                            CURRENTLY DEFINED  COMMANDS:
C                            - VIEW DISPLY COMMANDS:         1...20 + 3000006100
C                            - NON-VIEW DISPLY COMMANDS:    31...63
C                            - SCANNR COMMANDS:            101..134
C
C                            - DELETED COMMANDS:  110,116,117
C
      CALL SETCMD(123,'AX      ',2,HDISP,HJET,HAXES)
      CALL SETCMD(107,'BDLS    ',0,HDEL,HBANK,0)
      CALL SETCMD( 54,'BL      ',1,HCHANG,HLG,HOPT)
      CALL SETCMD( 32,'BW      ',1,HBANK,HWRITE,0)
C
      CALL SETCMD(108,'C       ',0,HCONT,HNEXT,HLEVEL)
      CALL SETCMD( 42,'CDST    ',1,HLIST,HDISP,HOPT)
      CALL SETCMD( 41,'CDTL    ',1,HCHANG,HDISP,HOPT)
      CALL SETCMD(102,'CLUS    ',1,HDISP,HCLUST,0)
C
      CALL SETCMD( 39,'COM     ',0,HADD,HCOMEN,0)
      CALL SETCMD(115,'CS      ',1,0,0,0)
      CALL SETCMD(114,'CSTL    ',1,HCHANG,HSTAND,HLEVEL)
      CALL SETCMD(115,'CSTV    ',1,HCHANG,HSTAND,HVIEW)
C
      CALL SETCMD( 14,'CYL     ',1,HLG,HCYL,0)
      CALL SETCMD(119,'DEDX    ',1,HDISP,HDEDX,0)
      CALL SETCMD(107,'DELETE  ',0,0,0,0)
      CALL SETCMD(133,'DELMAC  ',0,0,0,0)
      CALL SETCMD( 38,'DET     ',1,HDET,HDISP,0)
      CALL SETCMD( 56,'DRAW    ',1,HDRAW,0,0)
      CALL SETCMD( 57,'EC      ',1,HADD,HENDC,HLG)
C
      CALL SETCMD(118,'EDIT    ',0,HEDIT,HTRACK,HPATR)
      CALL SETCMD(132,'EDITMAC ',0,0,0,0)
      CALL SETCMD(113,'END     ',0,0,0,0)
      CALL SETCMD(113,'EXIT    ',0,0,0,0)
      CALL SETCMD(124,'FADC    ',0,HDISP,HFLASH,HADC)
      CALL SETCMD(127,'FAMP    ',0,HDISP,HFAMP,HRESLT)
C
      CALL SETCMD( 58,'FC      ',1,HADD,HFW,HTAGG)
      CALL SETCMD(122,'FIND    ',2,HFIND,HEVENT,0)
      CALL SETCMD( 12,'FW      ',1,HTAGG,HFW,HMAP)
      CALL SETCMD( 15,'FWMU    ',1,HFORWD,HMUON,HCOUNT)
      CALL SETCMD(104,'GVTX    ',1,HFIT,HVERTX,0)
C
      CALL SETCMD( 33,'H       ',1,HHARD,HCOPY,0)
      CALL SETCMD( 46,'HELP    ',0,HHELP,HCOMMD,HMENU)
      CALL SETCMD( 61,'HLP     ',0,0,0,0)
      CALL SETCMD( 34,'HX      ',0,HHARD,HCOPY,HEXT)
      CALL SETCMD( 60,'JETC    ',1,HCHANG,HJETC,HBANK)
      CALL SETCMD( 37,'JOYS    ',1,HCHANG,HSCALE,0)
C
      CALL SETCMD(109,'JUMP    ',1,HJUMP,HLEVEL,0)
      CALL SETCMD( 61,'LASER   ',0,0,0,0)
      CALL SETCMD(114,'LEVELS  ',1,0,0,0)
      CALL SETCMD(131,'MACRO   ',0,0,0,0)
      CALL SETCMD( 49,'MASS    ',0,HMASS,HDISP,0)
      CALL SETCMD( 35,'MENU    ',0,HCOMMD,HLIST,0)
      CALL SETCMD(129,'MORE    ',0,0,0,0)
C
      CALL SETCMD(106,'MUONS   ',1,HDISP,HGOOD,HMUON)
      CALL SETCMD(105,'MUPT    ',1,HDISP,HMUON,HRESLT)
      CALL SETCMD( 55,'MUR2    ',0,HLIST,HMUON,HRESLT)
      CALL SETCMD(111,'N       ',1,HDISP,HNEXT,HEVENT)
      CALL SETCMD(128,'ND50    ',0,HDISP,HNORD,HRESLT)
C
      CALL SETCMD(129,'NEW     ',0,0,0,0)
      CALL SETCMD( 44,'NEWS    ',0,HLIST,HNEWS,0)
      CALL SETCMD(111,'NN      ',1,0,0,0)
      CALL SETCMD(126,'NWCL    ',0,HRECAL,HEVENT,0)
      CALL SETCMD( 41,'OPT     ',1,0,0,0)
C
      CALL SETCMD( 59,'PATR    ',1,HCHANG,HPATR,HBANK)
      CALL SETCMD( 63,'PICK    ',0,HPICK,0,0)
      CALL SETCMD( 32,'PRINT   ',1,0,0,0)
      CALL SETCMD( 40,'PRO     ',1,HPROJ,HDISP,HADD)
      CALL SETCMD( 33,'Q       ',1,0,0,0)
      CALL SETCMD(120,'QP      ',1,HDISP,HQ,HPLOTS)
      CALL SETCMD( 34,'QX      ',0,0,0,0)
C
      CALL SETCMD(113,'QUIT    ',0,0,0,0)
      CALL SETCMD(  1,'RA      ',1,HFRONT,HID,0)
      CALL SETCMD(  2,'RB      ',1,HFRONT,HLG,0)
      CALL SETCMD(  3,'RC      ',1,HFRONT,HMUON,0)
C
      CALL SETCMD(126,'RECAL   ',0,0,0,0)
      CALL SETCMD(134,'RENAMAC ',0,0,0,0)
      CALL SETCMD( 45,'RES     ',1,HDISP,HRESLT,0)
C
      CALL SETCMD( 36,'RESET   ',0,0,0,0)
      CALL SETCMD( 48,'RET     ',0,HRETRN,HEDIT,0)
      CALL SETCMD( 36,'RS      ',0,HRESET,HSCALE,0)
      CALL SETCMD( 13,'RU      ',1,HLG,HMAP,0)
      CALL SETCMD( 16,'RZ      ',1,HZ,HCHAM,HMAP)
      CALL SETCMD( 30,'S       ',1,0,0,0)
      CALL SETCMD( 51,'SAVE    ',1,HSAVE,HEDIT,HWRK)
C
      CALL SETCMD(101,'SPVA    ',2,0,0,0)
      CALL SETCMD( 42,'STAT    ',1,0,0,0)
      CALL SETCMD(113,'STOP    ',0,0,0,0)
      CALL SETCMD( 30,'STVW    ',1,HSTAND,0,0)
      CALL SETCMD(121,'TOF     ',1,HDISP,HTOF,0)
C
      CALL SETCMD( 47,'TR      ',1,HDISP,HTRACK,HID)
      CALL SETCMD( 50,'TRG2    ',0,HDISP,HT2,HTRIG)
      CALL SETCMD( 43,'TRLG    ',0,HDISP,HT1,HTRIG)
      CALL SETCMD( 31,'TRUE    ',1,HTRUE,HTRACK,HDISP)
C
      CALL SETCMD(130,'VAC     ',1,HVERTX,HCHAM,HAMPL)
      CALL SETCMD( 20,'VC      ',1,HVERTX,HCHAM,0)
      CALL SETCMD(104,'VFIT    ',1,0,0,0)
      CALL SETCMD( 52,'VRES    ',1,HVERTX,HTRACK,HRESLT)
      CALL SETCMD( 17,'VRX     ',0,HFRONT,HVERTX,HREGN)
C
      CALL SETCMD( 18,'VRZX    ',0,HTOP,HVERTX,HREGN)
      CALL SETCMD( 19,'VRZY    ',0,HSIDE,HVERTX,HREGN)
      CALL SETCMD( 62,'VTXC    ',1,HCHANG,HVTXC,HBANK)
      CALL SETCMD( 53,'VX      ',1,HDISP,HVERTX,0)
      CALL SETCMD(112,'WRIT    ',1,HWRITE,HEVENT,0)
C
      CALL SETCMD( 16,'ZC      ',1,0,0,0)
      CALL SETCMD( 37,'ZOOM    ',1,0,0,0)
      CALL SETCMD(125,'ZTRG    ',0,HDISP,HZ,HTRIG)
      CALL SETCMD(103,'ZV      ',1,HDISP,HZ,HVERTX)
C
      CALL SETCMD(  4,'ZXA     ',1,HTOP,HID,0)
      CALL SETCMD(  5,'ZXB     ',1,HTOP,HLG,0)
      CALL SETCMD(  6,'ZXC     ',1,HTOP,HMUON,0)
C
      CALL SETCMD(  7,'ZXD     ',1,HTOP,HFW,0)
      CALL SETCMD(  8,'ZYA     ',1,HSIDE,HID,0)
      CALL SETCMD(  9,'ZYB     ',1,HSIDE,HLG,0)
      CALL SETCMD( 10,'ZYC     ',1,HSIDE,HMUON,0)
      CALL SETCMD( 11,'ZYD     ',1,HSIDE,HFW,0)
C
C
      RETURN
      END
