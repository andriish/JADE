       SUBROUTINE HLABEL(ID, TXT)
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
       integer ID
       CHARACTER TXT*256
       write(*,*)' HLABEL ',POSX,POSY, POSX0,POSY0,TXT
       
       CALL ISELNT(NTS)
       call ITX(POSX0, POSY0, TXT)
       
       end
       
       
