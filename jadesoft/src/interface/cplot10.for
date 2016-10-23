******************************************************************
*
*     PLOT10/HIGZ TERMINAL PARAMETERS
*
*     This file is included when compiling plot10.F
*
*     (XTMIN,YTMIN),(XTMAX,YTMAX) : Edges of the screen
*     (XMIN0,YMIN0),(XMAX0,YMAX0) : Edges of user window in screen coordinates
*     (XMIN,YMIN),(XMAX,YMAX)     : Edges of user window in virtual coordinates
*     XPOS0,YPOS0                 : Beam position in screen coordinates 
*     XPOS,YPOS                   : Beam position in virtual coordinates 
*
*     TTEST .EQ. .TRUE. : SUBROUTINE TWINDO was executed
*     DTEST .EQ. .TRUE. : SUBROUTINE DWINDO was executed
*     INIT .EQ. .TRUE.  : SUBROUTINE INITT was executed
*
*     NTS  : HIGZ normalization transformation index of screen window
*     NTV  : HIGZ normalization transformation index of virtual window
*
      LOGICAL TTEST,DTEST,INIT,TRACE,LROTAT,LSCALE,LTSO
      INTEGER NTV,NTS,KWTYPE,KLIN,KCHR,TLINE,IFNT,IPRC,ISYNC,ICHRSZ
      REAL XTMIN,XTMAX,YTMIN,YTMAX,XTMG,YTMG
     +     ,XMIN0,XMAX0,YMIN0,YMAX0,XPOS0,YPOS0
     +     ,XMIN,XMAX,YMIN,YMAX,XPOS,YPOS
     +     ,ANGLE,SCALE,XSIZE,YSIZE
     +     ,XCHAR,YCHAR
      CHARACTER FNAME*256
      COMMON /PLOT10/ TTEST,DTEST,INIT,TRACE,NTV,NTS,KWTYPE,LTSO
     +     ,XTMIN,XTMAX,YTMIN,YTMAX,XTMG,YTMG
     +     ,XMIN0,XMAX0,YMIN0,YMAX0,XPOS0,YPOS0
     +     ,XMIN,XMAX,YMIN,YMAX,XPOS,YPOS
     +     ,LROTAT,ANGLE,LSCALE,SCALE
     +     ,XCHAR,YCHAR,KLIN,KCHR,TLINE,ICHRSZ
     +     ,XSIZE,YSIZE,IFNT,IPRC,ISYNC
      COMMON /PLT10C/ FNAME
*******************************************************************
