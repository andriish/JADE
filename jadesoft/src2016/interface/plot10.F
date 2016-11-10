************************************************************************
*
* PLOT10
* ======
*
*     Emulation of some PLOT-10 Terminal Control System
*     Routines used by the JADE library.
*
*     13/08/1999  P.A. Movilla Fernandez
*     12/11/1999  PMF  last mod.
*
*
* Terminal initialization:
*
*     INITT
*     FINITT
*
* Graphics window definition:
*
*     TWINDO  SWINDO
*     DWINDO  VWINDO
*
* Drawing routines:
*
*       screen window       virtual window
*     absolute  relative   absolute relative
*
*     MOVABS    MOVREL     MOVEA    MOVER
*     DRWABS    DRWREL     DRAWA    DRAWR
*     PNTABS    PNTREL     POINTA   POINTR
*     DSHABS    DSHREL     DASHA    DASHR
*
*     RROTAT
*     RSCALE
*     ERASE
*
* Terminal I/O: 
*
*     SCURSR
*     VCURSR
*     ANCHO
*     ANSTR
*     EOUTST
*     CHRSIZ
*     CSIZE
*     HOME
*     NEWLIN
*     LINEF
*     SYSSYM
*     USRSYM
*
*     TRMOUT
*     TRMIN
*
* Utilities:
*
*     SEELOC
*     SEETW
*     SEEDW
*     HDCOPY
*     HDCEXT
*     HDCDST
*
************************************************************************

************************************************************************
*
*      I N I T I A L I Z A T I O N   A N D   T E R M I N A T I O N
* 
************************************************************************

************************************************************************
      SUBROUTINE INITT(IDUMMY)
      IMPLICIT NONE
*
*     Initialization of PLOT-10.
*
#include "cplot10.for"
#include "cpltcol.for"
*
      INTEGER IDUMMY
Code:
      INIT=.TRUE.
C
C - HIGZ normalization transformation indices
      NTS=8                     ! 'screen window'
      NTV=9                     ! 'virtual window'

C - Maximal screen dimensions of the terminal
      XTMIN=0.
      XTMAX=4095.
      YTMIN=0.
      YTMAX=4095.

C - Standard initialization of HIGZ
      CALL IGSTRT

C - Default settings of HIGZ graphics
      CALL IGDEF

C - Define a 'maximal screen window' by setting the
C   boundaries of the HIGZ normalization transformation (NT)
C   using the maximal screen dimensions ( NT index = NTS )
      CALL ISWN(NTS,XTMIN,XTMAX,YTMIN,YTMAX)
      CALL ISVP(NTS,0.,1.,0.,1.)

C - Default settings for the 'screen window'
      CALL TWINDO(INT(XTMIN),INT(XTMAX),INT(YTMIN),INT(YTMAX))
      CALL CHRSIZ(1)
      TLINE=0

C - Default settings for the 'virtual window'
      CALL DWINDO(XTMIN,XTMAX,YTMIN,YTMAX)

C - Colour settings in JADEZ
      CALL SETCOL('*INI')

C - Set trace mode
      TRACE=.FALSE.
C
      RETURN
      END

C /PLOT10/
      BLOCKDATA CPLOT10
      IMPLICIT NONE
#include "cplot10.for"
      DATA TTEST/.FALSE./,DTEST/.FALSE./,TLINE/0/,ISYNC/0/,LTSO/.FALSE./
      END

************************************************************************
      SUBROUTINE FINITT(IX,IY)
      IMPLICIT NONE
*
*     Termination of PLOT-10.
*
#include "cplot10.for"
*
      INTEGER IX,IY
Code:
      INIT=.FALSE.

C - Close current PostScript file
      CALL PSCLOS

C - Standard termination of HIGZ
      CALL IGFIN
C
      RETURN
      END

************************************************************************
*
*           D E F I N I T I O N    O F    S C R E E N
*
************************************************************************

************************************************************************
      SUBROUTINE TWINDO(MINX,MAXX,MINY,MAXY)
      IMPLICIT NONE
*     
*     Defines 'screen window'.
*
#include "cplot10.for"
*
      INTEGER MINX,MAXX,MINY,MAXY
      REAL XTERM,YTERM,XU,XO,YU,YO,AMIN1
Code:
      IF( .NOT.INIT ) CALL PABORT(1)
      IF( .NOT.TTEST ) TTEST=.TRUE.

C - Update 'screen window' in /PLOT10/
      XMIN0=REAL(MINX)
      XMAX0=REAL(MAXX)
      YMIN0=REAL(MINY)
      YMAX0=REAL(MAXY)

C - Define the 'screen window' by setting the boundaries of the
C   viewport of the HIGZ normalization transformation (NT) 
C   of the 'virtual window' (NT index =  NTV).
C   Normalization is done using the 'maximal screen' dimensions.
      XTERM=XTMAX-XTMIN
      YTERM=YTMAX-YTMIN
      XU=AMAX1(XMIN0/XTERM,0.)
      XO=AMIN1(XMAX0/XTERM,1.)
      YU=AMAX1(YMIN0/YTERM,0.)
      YO=AMIN1(YMAX0/YTERM,1.)
      IF( XO.LE.XU .OR. YO.LE.YU ) CALL PABORT(2)
      CALL ISVP(NTV,XU,XO,YU,YO)
      CALL ISELNT(NTV)

C - Initial beam position in 'screen window' coordinates
      XPOS0=0.
      YPOS0=0.

C - Suppress rotation/scaling of subsequent drawings using relative arguments
      LROTAT=.FALSE.
      LSCALE=.FALSE.
C
      IF( TRACE ) CALL SEE
C
      RETURN
      END

*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      SUBROUTINE SWINDO(MINX,MAXX,MINY,MAXY)
      IMPLICIT NONE
      INTEGER MINX,MAXX,MINY,MAXY
Code:
      CALL TWINDO(MINX,MINX+MAXX,MINY,MINY+MAXY)
C
      RETURN
      END

************************************************************************
      SUBROUTINE DWINDO(XMI,XMA,YMI,YMA)
      IMPLICIT NONE
*
*     Defines user 'virtual window'.
*
#include "cplot10.for"
*
      REAL XMI,XMA,YMI,YMA
Code:
      DTEST=.TRUE.
      IF( .NOT.INIT ) CALL PABORT(1)
      IF( XMA.LE.XMI .OR. YMA.LE.YMI ) CALL PABORT(2)
      IF( .NOT.TTEST )
     +     CALL TWINDO(INT(XTMIN),INT(XTMAX),INT(YTMIN),INT(YTMAX))

C - Update dimensions of the 'virtual window' in /PLOT10/
      XMIN=XMI
      XMAX=XMA
      YMIN=YMI
      YMAX=YMA

C - Define 'virtual window' by setting the boundaries
C   of the HIGZ normalization transformation (NT)
C   using the 'virtual window' dimensions (NT index = NTV)
      CALL ISWN(NTV,XMIN,XMAX,YMIN,YMAX)
      CALL ISELNT(NTV)

C - Set initial beam position in 'virtual window' coordinates
      XPOS=XMIN
      YPOS=YMIN

C     ... and also in 'screen window' coordinates
      CALL VTOS

C - Suppress rotation/scaling of subsequent drawings using relative arguments
      LROTAT=.FALSE.
      LSCALE=.FALSE.
C
      IF( TRACE ) CALL SEE
C
      RETURN
      END

*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      SUBROUTINE VWINDO(XMI,XMA,YMI,YMA)
      IMPLICIT NONE
      REAL XMI,XMA,YMI,YMA
Code:
      CALL DWINDO(XMI,XMI+XMA,YMI,YMI+YMA)
C
      RETURN
      END

************************************************************************
*
*                       D R A W I N G
*
************************************************************************

************************************************************************
*
*   Moving/Drawing 
*   > in absolute and relative coordinates
*   > in 'screen' and 'virtual' coordinates
*
*   For details see subroutine DRAW
*
C Screen window coordinates
      SUBROUTINE MOVABS(GOX,GOY)
      IMPLICIT NONE
      INTEGER GOX,GOY
      CALL DRAW(REAL(GOX),REAL(GOY),0,1,'A','S')
      RETURN
      END
C
      SUBROUTINE MOVREL(GOX,GOY)
      IMPLICIT NONE
      INTEGER GOX,GOY
      CALL DRAW(REAL(GOX),REAL(GOY),0,1,'R','S')
      RETURN
      END
C
      SUBROUTINE DRWABS(GOX,GOY)
      IMPLICIT NONE
      INTEGER GOX,GOY
      CALL DRAW(REAL(GOX),REAL(GOY),0,2,'A','S')
      RETURN
      END
C
      SUBROUTINE DRWREL(GOX,GOY)
      IMPLICIT NONE
      INTEGER GOX,GOY
      CALL DRAW(REAL(GOX),REAL(GOY),0,2,'R','S')
      RETURN
      END
C
      SUBROUTINE PNTABS(GOX,GOY)
      IMPLICIT NONE
      INTEGER GOX,GOY
      CALL DRAW(REAL(GOX),REAL(GOY),0,3,'A','S')
      RETURN
      END
C
      SUBROUTINE PNTREL(GOX,GOY)
      IMPLICIT NONE
      INTEGER GOX,GOY
      CALL DRAW(REAL(GOX),REAL(GOY),0,3,'R','S')
      RETURN
      END
C
      SUBROUTINE DSHABS(GOX,GOY,L)
      IMPLICIT NONE
      INTEGER GOX,GOY,L
      CALL DRAW(REAL(GOX),REAL(GOY),L,4,'A','S')
      RETURN
      END
C
      SUBROUTINE DSHREL(GOX,GOY,L)
      IMPLICIT NONE
      INTEGER GOX,GOY,L
      CALL DRAW(REAL(GOX),REAL(GOY),L,4,'R','S')
      RETURN
      END

C Virtual window coordinates
      SUBROUTINE MOVEA(XGO,YGO)
      IMPLICIT NONE
      REAL XGO,YGO
      CALL DRAW(XGO,YGO,0,1,'A','V')
      RETURN
      END
C
      SUBROUTINE MOVER(XGO,YGO)
      IMPLICIT NONE
      REAL XGO,YGO
      CALL DRAW(XGO,YGO,0,1,'R','V')
      RETURN
      END
C
      SUBROUTINE DRAWA(XGO,YGO)
      IMPLICIT NONE
      REAL XGO,YGO
      CALL DRAW(XGO,YGO,0,2,'A','V')
      RETURN
      END
C
      SUBROUTINE DRAWR(XGO,YGO)
      IMPLICIT NONE
      REAL XGO,YGO
      CALL DRAW(XGO,YGO,0,2,'R','V')
      RETURN
      END
C
      SUBROUTINE POINTA(XGO,YGO)
      IMPLICIT NONE
      REAL XGO,YGO
      CALL DRAW(XGO,YGO,0,3,'A','V')
      RETURN
      END
C
      SUBROUTINE POINTR(XGO,YGO)
      IMPLICIT NONE
      REAL XGO,YGO
      CALL DRAW(XGO,YGO,0,3,'R','V')
      RETURN
      END
C
      SUBROUTINE DASHA(XGO,YGO,L)
      IMPLICIT NONE
      INTEGER L
      REAL XGO,YGO
      CALL DRAW(XGO,YGO,L,4,'A','V')
      RETURN
      END
C
      SUBROUTINE DASHR(XGO,YGO,L)
      IMPLICIT NONE
      INTEGER L
      REAL XGO,YGO
      CALL DRAW(XGO,YGO,L,4,'R','V')
      RETURN
      END

************************************************************************
      SUBROUTINE DRAW(XGO,YGO,L0,IC0,SYS,WIN)
      IMPLICIT NONE
*
*     Performs graphics action.
*
*     XGO,YGO : Target coordinates for the beam 
*     L0      : Dash type code for dashed lines
*     IC0     : Type of graphics action 
*              = 1 move without action
*              = 2 draw solid line
*              = 3 draw point
*              = 4 draw dashed line 
*     SYS='A' : Target coordinates are absolute
*     SYS='R' :        "           are relative
*     WIN='S' :        "           are screen window  coordinates
*     WIN='V' :        "           are virtual window coordinates
*
#include "cplot10.for"
*
      REAL XGO,YGO,X(2),Y(2),AMIN1,AMAX1
      INTEGER L,IC,LTYP,DSHTYP,L0,IC0
      CHARACTER*1 WIN,SYS
Code:
C - Check if initialization was done
      IF( .NOT.INIT ) CALL PABORT(1)
      IF( WIN.EQ.'V' .AND. .NOT.DTEST ) CALL PABORT(3)
      IF( WIN.EQ.'S' .AND. .NOT.TTEST ) CALL PABORT(4)
      IC=IC0
      L=L0

C - Select HIGZ viewport for 'screen window' or 'virtual window' and
C get current beam position in the respective window coordinates 
      IF( WIN.EQ.'S' ) THEN
         CALL ISELNT(NTS)
         X(1)=XPOS0
         Y(1)=YPOS0
      ELSE
         CALL ISELNT(NTV)
         X(1)=XPOS
         Y(1)=YPOS
      ENDIF

C - Calculate target position
C ... from relative coordinates
      IF( SYS.EQ.'R' ) THEN
C       Perform transformation of the relative moving   v_e_c_t_o_r 
C       if forced by subroutines RROTAT,RSCALE
         IF( LROTAT ) THEN
            X(2)=COS(ANGLE)*XGO-SIN(ANGLE)*YGO
            Y(2)=SIN(ANGLE)*XGO+COS(ANGLE)*YGO
         ELSE
            X(2)=XGO
            Y(2)=YGO
         ENDIF
         IF( LSCALE ) THEN
            X(2)=SCALE*X(2)
            Y(2)=SCALE*Y(2)
         ENDIF
         X(2)=X(2)+X(1)
         Y(2)=Y(2)+Y(1)
      ELSE
C ... from absolute coordinates
         X(2)=XGO
         Y(2)=YGO
      ENDIF

C - Perform clipping of the beam position in the 'screen window' if needed
C     ( preliminary version )
      IF( WIN.EQ.'S' ) THEN
         X(2)=AMIN1(X(2),XMAX0)
         X(2)=AMAX1(X(2),XMIN0)
         Y(2)=AMIN1(Y(2),YMAX0)
         Y(2)=AMAX1(Y(2),YMIN0)
      ENDIF

C - Store new beam position in 'screen' and 'virtual window' coordinates
      IF( WIN.EQ.'S' ) THEN
         XPOS0=X(2)
         YPOS0=Y(2)
         IF( DTEST ) CALL STOV
      ELSE
         XPOS=X(2)
         YPOS=Y(2)
         CALL VTOS
      ENDIF

C - Perform graphics action
      IF( IC.EQ.4.AND.L.EQ.-1 ) IC=1  
      IF( IC.EQ.4.AND.L.EQ.0 )  IC=2  
      IF( IC.EQ.2.OR.IC.EQ.4 ) THEN
         IF( IC.EQ.4 ) THEN
            LTYP=DSHTYP(L)
         ELSE
            LTYP=1
         ENDIF
C ---> Draw a line
         CALL ISLN(LTYP)
         CALL IPL(2,X,Y)
      ELSEIF( IC.EQ.3 ) THEN
C ---> Draw a point
         CALL IPM(1,X(2),Y(2))
      ENDIF
C
      IF( TRACE ) CALL SEEB
C
      RETURN
      END

************************************************************************
      SUBROUTINE RROTAT(DEG)
      IMPLICIT NONE
*
*     Forces rotation of drawings with relative arguments.
*
#include "cplot10.for"
*
      REAL DEG,ACOS
Code:
      IF( DEG.NE.0.0 ) THEN
         LROTAT=.TRUE.
         ANGLE=ACOS(0.)*DEG/90.
      ELSE
         LROTAT=.FALSE.
         ANGLE=0.
      ENDIF
C
      RETURN
      END

************************************************************************
      SUBROUTINE RSCALE(SCA)
      IMPLICIT NONE
*
*     Forces scaling of drawing with relative arguments.
*
#include "cplot10.for"
*
      REAL SCA
Code:
      IF( SCA.NE.0.0 ) THEN
         LSCALE=.TRUE.
         SCALE=SCA
      ELSE
         LSCALE=.FALSE.
         SCALE=0.
      ENDIF
C
      RETURN
      END

************************************************************************
      SUBROUTINE ERASE
      IMPLICIT NONE
*
*     Clears graphics screen.
*
#include "cplot10.for"
#include "cpltcol.for"
*
      INTEGER I,IT(2)/1,5/
*
Code:
C - Clear window
      CALL ICLRWK(0,1)

C - Close current PostScript file
      CALL PSCLOS

C - Open new PostScript file with previous name
      CALL PSOPEN(' ',0.,0.)
C
      CALL HOME
      IF( LCOL ) THEN
         CALL ISELNT(NTS)
         IF( IBACK.EQ.1 ) 
     +        CALL IGBOX(XTMIN,XTMAX,YTMIN,YTMAX)
         IF( IBACK2.EQ.1 )
     +        CALL IXBOX(XTMIN,XTMAX,YTMIN,YTMAX,1)
      ENDIF
      CALL IUWK(0,1)
C
      RETURN
      END

************************************************************************
      SUBROUTINE CLEAR
      IMPLICIT NONE
*
*     Clears 'TSO' screen.
*
#include "cplot10.for"
*
Code:
      TLINE=0
      CALL ERASE
C
      RETURN
      END

************************************************************************
      SUBROUTINE TESTGA(MODEGA,GAHEX)
      IMPLICIT NONE
*
*     Tests if GA Mode (set MODEGA=0: No)
*
      INTEGER MODEGA,GAHEX
Code:
      MODEGA=0
C
      RETURN
      END


************************************************************************
*
*               T E R M I N A L     D I A L O G
*
************************************************************************

************************************************************************      
      SUBROUTINE SCURSR(ICHAR,IX,IY)
      IMPLICIT NONE
*
*  Flashes cursor on screen window and await result.
*  Does not change beam position.
*
      INTEGER ICHAR,IX,IY
      REAL X,Y
Code:
      CALL CURSR(ICHAR,X,Y,'S')
      IX=INT(X+.5)
      IY=INT(Y+.5)
C
      RETURN
      END

************************************************************************
      SUBROUTINE VCURSR(ICHAR,X,Y)
      IMPLICIT NONE
*
*  Flashes cursor on virtual window and await result.
*  Does not change beam position.
*
      INTEGER ICHAR
      REAL X,Y
Code:
      CALL CURSR(ICHAR,X,Y,'V')
C
      RETURN
      END

************************************************************************
      SUBROUTINE CURSR(ICHAR,XWC,YWC,WIN)
      IMPLICIT NONE
*
*     Gets the HIGZ cursor position in normalized
*     device coordinates and world coordinates.
*
#include "cplot10.for"
*
      INTEGER ICHAR
      CHARACTER*1 WIN
      character*80 TEXT(6)
      character*1 CHAR
      INTEGER NT,IBN
      REAL XNDC,YNDC,XWC,YWC
Code:
      IF( WIN.EQ.'S' ) THEN
         CALL ISELNT(NTS)
         WRITE(TEXT(1),'(2X,16(''-''),A,16(''-''))') ' SCREEN  WINDOW '
         WRITE(TEXT(2),'(2(A6,F6.0))') 'Xmin=',XMIN0,'Xmax=',XMAX0
         WRITE(TEXT(3),'(2(A6,F6.0))') 'Ymin=',YMIN0,'Xmax=',YMAX0
      ELSE
         CALL ISELNT(NTV)
         WRITE(TEXT(1),'(2X,16(''-''),A,16(''-''))') ' VIRTUAL WINDOW '
         WRITE(TEXT(2),'(2(A6,E13.7))') 'Xmin=',XMIN,'Xmax=',XMAX
         WRITE(TEXT(3),'(2(A6,E13.7))') 'Ymin=',YMIN,'Xmax=',YMAX
      ENDIF

C - Get graphic cursor position
      CALL IGLOC(20,NT,IBN,XNDC,YNDC,XWC,YWC)
      IF( WIN.EQ.'S' ) THEN
         WRITE(TEXT(4),'(A,F6.0)') ' Cursor position: X = ',XWC
         WRITE(TEXT(5),'(A,F6.0)') '                  Y = ',YWC
      ELSE
         WRITE(TEXT(4),'(A,E13.7)')    ' Cursor position: X = ',XWC
         WRITE(TEXT(5),'(A,E13.7)')    '                  Y = ',YWC
      ENDIF

C - Get character from terminal if right or middle button
C     of the mouse was pressed
      IF( IBN .NE. 1) THEN
         WRITE(*,'(A)') '*** CURSR: Please enter a keyboard character'
         READ(*,'(A1)') CHAR
         CALL CHTOI(CHAR,ICHAR)
         WRITE(TEXT(6)
     +    ,'( ''Keyboard character '',A1,''('',I3, '') is struck'')') 
     +    CHAR,ICHAR 
      ELSE
         TEXT(6)=' '
      ENDIF

C - Display window data and cursor postion on an extra window 
      IF ( TRACE ) THEN
         WRITE(*,'(A80)') TEXT
         CALL IGMESS(6,TEXT,'PLOT-10','P')
      ENDIF
C
      RETURN
      END

************************************************************************
      SUBROUTINE ANCHO(ICHAR)
      IMPLICIT NONE
*
*     Output of a single 7-bit ASCII character.
*     Updates beam position according to the character size set.
*
#include "cplot10.for"
*
      INTEGER ICHAR
      CHARACTER*1 CHAR
Code:
C NB.: alignment of character in HIGZ = middle/middle
C      beam position before writing = bottom/left
C      beam position after writing = bottom/right

C- Convert to ASCII
      CALL ITOCH(ICHAR,CHAR)

C - Force correct alignment at the top of the screen
      IF( YPOS0.GT.YTMAX-YCHAR ) YPOS0=YTMAX-YCHAR 

C - Perform line feed and carriage return if needed
      IF( XPOS0+XCHAR .GT. XTMAX*(1.+(XTMAX-XTMIN)*1.E-06) ) THEN
         XPOS0=0.
         YPOS0=YPOS0-YCHAR
      ENDIF

C - Goto home position if needed
      IF( YPOS0 .LT. -1.E-06*(YTMAX-YTMIN) ) THEN
         XPOS0=0.
         YPOS0=YTMAX-YCHAR
      ENDIF

C - Print out character using 'screen window' coordinates
      CALL ISELNT(NTS)
      CALL ITX(XPOS0+XCHAR/2.,YPOS0+YCHAR/2.,CHAR)

C - Update beam position
      XPOS0=XPOS0+XCHAR

C - Update also respective 'virtual window' coordinates
      CALL STOV
C
      IF( TRACE ) CALL SEEB
C
      RETURN
      END

************************************************************************
      SUBROUTINE ANSTR(NWORDS,ICHAR)
      IMPLICIT NONE
*
*     Output of an array of characters, one character per word.
*     Updates beam position according to the character size set.
*
      INTEGER NWORDS,ICHAR,I,NWRD
      DIMENSION ICHAR(136)
Code:
      NWRD=MIN(NWORDS,136)
      DO I=1,NWRD
         CALL ANCHO(ICHAR(I))
      ENDDO
C
      RETURN
      END

************************************************************************
      SUBROUTINE EOUTST(INBYTES,SOURCE)
      IMPLICIT NONE
*
*     Output of an array of four characters packed per word.
*     Updates beam position according to the character size set.
*
      INTEGER*1 SOURCE
      INTEGER NBYTES,TARGET,IBYTE,I
      DIMENSION SOURCE(*),TARGET(136)
      INTEGER*2 HTYP(2)
      INTEGER ITYP/1/
      EQUIVALENCE (HTYP(1),ITYP)
Code:
CAV 
      INTEGER INBYTES    
      NBYTES=INBYTES
C - use CERNLIB routine BLOW (M426) to unpack full words into bytes
      NBYTES=MIN(NBYTES,136)
      CALL BLOW(SOURCE,TARGET,NBYTES,8)

C - test the byteorder of the present machine and
C     print out each ASCII character coded in the INTEGER*4
C     array TARGET
      DO IBYTE=1,NBYTES
         IF( HTYP(2).EQ.0 ) THEN
C     ... DEC-like
            CALL ANCHO(TARGET(4*(INT((IBYTE-1)/4)+1)-MOD(IBYTE-1,4)))
         ELSE
C     ... IBM-like
            CALL ANCHO(TARGET(IBYTE))
         ENDIF
      ENDDO
C
      RETURN
      END

************************************************************************
      SUBROUTINE CHRSIZ(ICHAR)
      IMPLICIT NONE
*
*     Selects number of characters per line and size of character 
*     including inter character space.
*
#include "cplot10.for"
*
      INTEGER ICHAR,NC(4),NL(4)
      REAL YY(4)
C ... from PLOT10 manual
      DATA NC/74,81,121,133/
      DATA NL/35,38,58,64/
C ... from IPS users guide
C      DATA YY/63.,56.,42.,35./
Code:
      IF( ICHAR.GT.4 .OR. ICHAR.LT.1 ) ICHAR=1
C - Update text output attributes in /PLOT10/
      KLIN=NL(ICHAR)
      KCHR=NC(ICHAR)
      ICHRSZ=ICHAR
C PLOT10 manual version
      XCHAR=(XTMAX-XTMIN)/(NC(ICHAR)+1)
      YCHAR=(YTMAX-YTMIN)/NL(ICHAR)
C IPS users guide version
C      YCHAR=YY(ICHAR)
C      XCHAR=YCHAR*55./117.

C - Set height of text generated by HIGZ
      CALL ISCHH(YCHAR*.5)
C     
      RETURN
      END

************************************************************************
      SUBROUTINE CSIZE(IHORZ,IVERT)
      IMPLICIT NONE
*
*     Returns horizontal and vertical character dimensions
*     including inter character space in 'raster units'.
*
#include "cplot10.for"
*
      INTEGER IHORZ,IVERT
Code:
      IHORZ=INT(XCHAR+.5)
      IVERT=INT(YCHAR+.5)
C
      RETURN
      END

************************************************************************
      SUBROUTINE HOME
      IMPLICIT NONE
*
*     Moves alphanumeric cursor to the upper left corner of the screen.
*     Updates beam position.
*
#include "cplot10.for"
Code:
      TLINE=1

C- Update beam position
      XPOS0=0.
C     YPOS0=YTMAX-YCHAR
      YPOS0=YTMAX
      CALL STOV
C
      RETURN
      END

************************************************************************
      SUBROUTINE NEWLIN
      IMPLICIT NONE
*
*     Generates a line feed and carriage return. Updates beam position.
*
#include "cplot10.for"
Code:

C- Update beam position
      XPOS0=0.
      YPOS0=YPOS0-YCHAR
      IF( YPOS0 .LT. 0.) YPOS0=YTMAX-YCHAR
      CALL STOV
C
      RETURN
      END

************************************************************************
      SUBROUTINE LINEF
      IMPLICIT NONE
*
*     Generates a line feed. Updates beam position. 
*
#include "cplot10.for"
Code:
C- Update beam position
      YPOS0=YPOS0-YCHAR
      IF( YPOS .LT. 0.) YPOS0=YTMAX-YCHAR
      CALL STOV
C
      RETURN
      END

************************************************************************
      SUBROUTINE TRMOUT(NCHAR,TXT)
      IMPLICIT NONE
*
*  Writes a new line of characters to the screen.
*  Does not change beam position.
*
#include "cplot10.for"
*
      INTEGER NCHAR,ISV
      CHARACTER*1 TXT(*)
      character*1 TEXT(136)
*
      CHARACTER CFMT*9,C*1
      INTEGER LEN,NCH,I,L
      LOGICAL FIRST /.TRUE./
      SAVE ISV,FIRST
Code:
      DO I=1,NCHAR
         TEXT(I)=' '
      ENDDO
      L=LEN(TXT)
      IF( TXT(L).EQ.'$') L=L-1
      DO I=1,MIN(NCHAR,L)
         IF( TXT(I).EQ.'^' ) THEN
            TEXT(I)=' '
         ELSE
            TEXT(I)=TXT(I)
         ENDIF
      ENDDO

C Normal ouput:
      NCH=MIN(NCHAR,KCHR)
      WRITE(CFMT,'(A1,I3,A3)') '(',NCH,'A1)'
      IF( L.NE.LEN(TXT) ) CFMT(7:9)=',$)'
      WRITE(*,FMT=CFMT)  (TEXT(I),I=1,NCH)

      RETURN
      END
C Output to additional window:
C      CALL IUWK(1,1)
C      CALL IDAWK(1)
C      IF( FIRST ) THEN
C         CALL IOPWK(5,1,3)
C         FIRST=.FALSE.
C      ENDIF
C Activate new workstation
C      LTSO=.TRUE.
C      CALL IACWK(5)
C      CALL ISELNT(NTS)
C      ISV=ICHRSZ
C      CALL CHRSIZ(2)
C Print out the text
C      IF( TLINE.GE.KLIN ) THEN
C         WRITE(*,'(T20,A)') '*** PRESS A KEY ***'
C         READ(*,'(A1)') C
C         TLINE=0
C      ENDIF
C      IF( TLINE.EQ.0 ) CALL CLEAR
C      CALL ISTXCI(3)
C      CALL NEWLIN
C      CALL EOUTST(NCH,TEXT)
C      TLINE=TLINE+1
C Reset test attributes
C      CALL CHRSIZ(ISV)
C      CALL SETCOL(' ')
C Deaktivate extra workstation, reactivate the first one
C      LTSO=.FALSE.
C      CALL IUWK(5,1)
C      CALL IDAWK(5)
C      CALL IACWK(1)
C
C      RETURN
C      END

************************************************************************
      SUBROUTINE TRMIN(NCHAR,TXT)
      IMPLICIT NONE
*
*     Gets a line of text from the terminal.
*     Does not change beam position.
*
#include "cplot10.for"
*
      INTEGER NCHAR,LEN,I,NCH
      CHARACTER TXT*(*),CFMT*6,TEXT*132
Code:
      NCH=MIN(NCHAR,132)
      DO I=1,NCH
         TEXT(I:I)=' '
      ENDDO
      WRITE(CFMT,'(A2,I3,A1)') '(A',NCH,')'
      CALL IUWK(0)
      READ(*,FMT=CFMT) TEXT
      CALL CLTOU(TEXT)
      DO I=1,NCH
         TXT(I:I)=TEXT(I:I)
      ENDDO
      CALL TRMOUT(NCH,TXT)
C
      RETURN
      END

************************************************************************
      SUBROUTINE SYSSYM(X,Y,HEIGHT,HSTR,NSTR,THETA)
      IMPLICIT NONE
*
*  Adds text strings of arbitrary size and  orientation to the graphic. 
*  It works in screen coordinates. Does not change beam position.
*
      REAL X,Y,HEIGHT,THETA
      INTEGER HSTR,NSTR
      DIMENSION HSTR(INT(NSTR/4)+1)
Code:
      CALL GRTEXT(X,Y,HEIGHT,HSTR,NSTR,THETA,'S')
C
      RETURN
      END

************************************************************************
      SUBROUTINE USRSYM(X,Y,HEIGHT,HSTR,NSTR,THETA)
      IMPLICIT NONE
*     
*  Adds text strings of arbitrary size and orientation to the graphic.
*  It works in virtual coordinates. Does not change beam position.
*
      REAL X,Y,HEIGHT,THETA
      INTEGER HSTR,NSTR
      DIMENSION HSTR(INT(NSTR/4)+1)
Code:
      CALL GRTEXT(X,Y,HEIGHT,HSTR,NSTR,THETA,'V')
C
      RETURN
      END

************************************************************************
      SUBROUTINE GRTEXT(X,Y,HGHT,HSTR,NSTR,THETA,WIN)
      IMPLICIT NONE
*
*  Adds text strings of arbitrary size and orientation to the graphic. 
*
#include "cplot10.for"
*
      REAL X,Y,HEIGHT,THETA,WIDTH,HGHT,W,H
      REAL XS,YS,COS,SIN,CS,SN,DX,DY
      INTEGER HSTR,ISTR,NSTR,I
      CHARACTER WIN*1
      CHARACTER CSTR*1,CSTR4*4,SPCSYM*4
      DIMENSION ISTR(ABS(NSTR)),CSTR(ABS(NSTR)),CSTR4(ABS(NSTR))
     +     ,HSTR(INT(ABS(NSTR)/4)+1)
*
      INTEGER*2 HTYP(2)
      INTEGER ITYP/1/
      EQUIVALENCE (HTYP(1),ITYP)
Code:
C - Check if initialization was done
      IF( .NOT.INIT ) CALL PABORT(1)
      IF( WIN.EQ.'V' .AND. .NOT.DTEST ) CALL PABORT(3)
      IF( WIN.EQ.'S' .AND. .NOT.TTEST ) CALL PABORT(4)
C --- Conversion from ASCII integer to character string
      CALL BLOW(HSTR,ISTR,ABS(NSTR),8)
      IF( NSTR.GT.0 ) THEN
         DO I=1,NSTR
            IF( HTYP(2).EQ.0 ) THEN
C     ... DEC like byte order
               CALL ITOCH(ISTR(4*(INT((I-1)/4)+1)-MOD(I-1,4)),CSTR(I))
            ELSE
C     ... IBM like byte order
               CALL ITOCH(ISTR(I),CSTR(I))
            ENDIF
         ENDDO
      ELSE
C ---- Conversion from ASCII integer to special symbols
         DO I=1,ABS(NSTR)
            CSTR4(I)=SPCSYM(HSTR(I))
         ENDDO
      ENDIF

C - Select HIGZ viewport for 'screen window' or 'virtual window'
      IF( WIN.EQ.'S' ) THEN
         CALL ISELNT(NTS)
      ELSE
         CALL ISELNT(NTV)
      ENDIF

C - Fetch beam position
      XS=X
      YS=Y

C - Define text attributes ( preliminary )
C      IF( HGHT.GE. 60.) THEN
C         HEIGHT=117./63.*HGHT
C         WIDTH=55./117.*HEIGHT
C      ELSEIF( HGHT.GE.49. .AND. HGHT.LT.60.) THEN
C         HEIGHT=108./56.*HGHT
C         WIDTH=50./108.*HEIGHT
C      ELSEIF( HGHT.GE.39. .AND. HGHT.LT.50.) THEN
C         HEIGHT=71./42.*HGHT
C         WIDTH=33.6/71.*HEIGHT
C      ELSE
C         HEIGHT=64./35.*HGHT
C         WIDTH=30.5/64.*HEIGHT
C      ENDIF
      HEIGHT=HGHT*1.3
      WIDTH=HEIGHT*55./117.
      CALL ISCHH(HEIGHT*.5)
      CALL IGSET('TANG',THETA)

C - Print out the text
C NB.: Character-wise print out is needed in order
C      to ensure equidistant character output !!!
      DO I=1,ABS(NSTR)
         CS=COS(THETA*3.1415927/180.)
         SN=SIN(THETA*3.1415927/180.)
         DX=WIDTH*CS
         DY=WIDTH*SN
         W=.5*WIDTH*CS
         H=.45*HEIGHT*(1+SN)
         IF( NSTR.GT.0 ) THEN
            CALL ITX(XS+W,YS+H,CSTR(I)) ! normal text
         ELSE
            CALL ISTXFP(0,2)
            CALL ITX(XS+W,YS+H
     +           ,CSTR4(I)(1:INDEX(CSTR4(I)//' ',' ')-1)) ! special symbols
            CALL ISTXFP(IFNT,IPRC) ! restore font
         ENDIF
         XS=XS+DX
         YS=YS+DY
      ENDDO

C - Restore inclination angle and text height
      CALL IGSET('TANG',0.)
      CALL ISCHH(YCHAR*.5)

C
      RETURN
      END


************************************************************************
*
*                       U T I L I T I E S
*
************************************************************************


************************************************************************
      SUBROUTINE SEELOC(IX,IY)
      IMPLICIT NONE
*
*     Gets current beam position.
*
#include "cplot10.for"
*
      INTEGER IX,IY
Code:
      IX=XPOS0
      IY=YPOS0
C
      RETURN
      END

************************************************************************
      SUBROUTINE SEETW(MINX,MAXX,MINY,MAXY)
      IMPLICIT NONE
*
*     Gets screen window data.
*
#include "cplot10.for"
*
      INTEGER MINX,MAXX,MINY,MAXY
Code:
      IF (.NOT. TTEST) RETURN
      MINX=INT(XMIN0+.5)
      MAXX=INT(XMAX0+.5)
      MINY=INT(YMIN0+.5)
      MAXY=INT(YMAX0+.5)
C
      RETURN
      END

************************************************************************
      SUBROUTINE SEEDW(XMIND,XMAXD,YMIND,YMAXD)
      IMPLICIT NONE
*
*     Gets virtual window data.
*
#include "cplot10.for"
*
      REAL XMIND,XMAXD,YMIND,YMAXD
Code:
      IF (.NOT. DTEST) RETURN
      XMIND=XMIN
      XMAXD=XMAX
      YMIND=YMIN
      YMAXD=YMAX
C
      RETURN
      END

************************************************************************
      SUBROUTINE HDCOPY
      IMPLICIT NONE
*
*     Copy to printer.
*     Save current picture as PostScript file.
*
#include "cplot10.for"
*
      INTEGER IC/1/,LRECL/1024/,ISTAT,ICYCLE
      CHARACTER CIC*3/'001'/
      SAVE IC,CIC
Code:
C - Close current PostScript file
      WRITE(*,FMT=100) FNAME(1:INDEX(FNAME,' ')-1)//'.'//CIC//'.ps'
 100  FORMAT('*** HDCOPY: Saving current picture as ',A)
      CALL PSCLOS

C - Increment copy counter for next copy
      IC=MOD(IC+1,1000)
      IF(IC.LT.10) THEN
         WRITE(CIC,'(A2,I1)') '00',MOD(IC,1000)
      ELSEIF(IC.GE.10.AND.IC.LT.100) THEN
         WRITE(CIC,'(A1,I2)') '0',IC
      ELSEIF(IC.GE.100.AND.IC.LT.1000) THEN
         WRITE(CIC,'(I3)') IC
      ENDIF

C - Prepare for next copy:
      CALL PSOPEN(FNAME(1:INDEX(FNAME,' ')-1)//'.'//CIC//'.ps'
     +     ,XSIZE,YSIZE)
C
      RETURN
      END

************************************************************************
      SUBROUTINE HDCEXT

*     Copy to printer.
*     Save current picture as PostScript file
*
Code:
      CALL HDCOPY
C
      RETURN
      END

************************************************************************
      SUBROUTINE HDCDST(DUMMY)
      IMPLICIT NONE
*
*     Copy to printer.
*     Save current picture as PostScript file
*
      REAL*8 DUMMY
Code:
      WRITE (*,'(''HDCDST: PRINTER ='',A8)') DUMMY
      CALL HDCOPY
C
      RETURN
      END

***THE***END***THE***END***THE***END***THE***END***THE***END***THE***END

************************************************************************
*
*     Further support routines for the PLOT-10 emulation
*
************************************************************************

************************************************************************
      SUBROUTINE IGSTRT
      IMPLICIT NONE
*
*     Standard initialization of HIGZ
*
#include "cplot10.for"
* PAW
      INTEGER NWPAW
      REAL RPAW
      PARAMETER(NWPAW=50000)
      COMMON/PAWC/ RPAW(NWPAW)
Code:
C - Initialization of ZEBRA
      CALL MZEBRA(-3)

C - Initialization of storage in /PAWC/
      CALL MZPAW(NWPAW,'M')

C - Initialization of HIGZ
      CALL IGINIT(0)

C - Get Workstation type from standard input
C      CALL IGWKTY(KWTYPE)
      KWTYPE=2

C - Initialization of underlaying graphics package
      CALL IGSSE(6,KWTYPE)

C - Clear HIGZ window and force X11 synchronization
      CALL ICLRWK(0,1)
      CALL IXSYNC(ISYNC)

C - Physical dimensions of later graphics output (in cm)
C   and name of graphics output file
C   ...for A4 format
CC      XSIZE=21.
CC      YSIZE=29.7!XSIZE*YTMAX/XTMAX
C   ...for eps format
      XSIZE=25.
      YSIZE=XSIZE*YTMAX/XTMAX
C
      FNAME='JADE'

C - Prepare PostScript output of HIGZ graphics
C  via subroutine HDCOPY 
      CALL PSOPEN(FNAME(1:INDEX(FNAME,' ')-1)//'.001.ps'
     +     ,XSIZE,YSIZE)
C
      RETURN
      END

************************************************************************
      SUBROUTINE IGFIN
*
*     Standard termination of HIGZ
*
C - Update the open workstation and return to alphanumeric mode
      CALL IGTERM

C - Terminate HIGZ
      CALL IGEND
C
      RETURN
      END

************************************************************************
      SUBROUTINE IGDEF
      IMPLICIT NONE
*
*     Default settings of HIGZ graphics
*
#include "cplot10.for"
C
C Line attributes

C     - line type
      CALL ISLN(1)

C     - line width
      CALL ISLWSC(1.)

C     - colour
      CALL ISPLCI(1)

C
C Marker attributes
C     - type
      CALL ISMK(1)

C     - scale
      CALL ISMKSC(.2)

C     - colour
      CALL ISPMCI(1)

C
C Text attributes
C     - alignment
      CALL ISTXAL(2,3)

C     - height
      CALL ISCHH(.2)

C     - font
      IFNT=0
      IPRC=2
      CALL ISTXFP(IFNT,IPRC)
C     - colour
      CALL ISTXCI(1)
C
      RETURN
      END

************************************************************************
      INTEGER FUNCTION DSHTYP(L)
      IMPLICIT NONE
*
*     Approximates PLOT-10 dash types with HIGZ dash types.
*
      INTEGER L
Code:
      IF(L.EQ.1) THEN
         DSHTYP=3
      ELSEIF(L.EQ.2) THEN
         DSHTYP=4
      ELSEIF(L.EQ.3.OR.L.EQ.12.OR.L.EQ.34) THEN
         DSHTYP=2
      ELSEIF(L.EQ.4.OR.L.EQ.38.OR.L.EQ.47) THEN
         DSHTYP=12
      ELSE
         DSHTYP=3
      ENDIF
C
      RETURN
      END

************************************************************************
      CHARACTER*4 FUNCTION SPCSYM(I)
      IMPLICIT NONE
*
*     Approximates special symbols generated by the 
*     software generator subroutines SYSSYM and USRSYM.
*
      INTEGER I,J
      CHARACTER*4 C(0:15)
      DATA C /                  ! translate to IGTEXT 'specials' code
     +     '<"1#',              !  0 empty square
     +     '<"0#',              !  1 circle
     +     '<"2#',              !  2 empty triangle
     +     '>"+#',              !  3 plus
     +     '>"X#',              !  4 cross
     +     '<"U#',              !  5 rhomb
     +     '>"6#',              !  6 arrow up (approx.)
     +     '>"W#',              !  7 ampersand (approx.)
     +     'Z   ',              !  8 Z (approx.)
     +     'Y   ',              !  9 Y (approx.)
     +     '>"F#',              ! 10 double cross ( approx.)
     +     '>"*#',              ! 11 star *
     +     '<"C#',              ! 12 empty cross (approx.)
     +     '>"B#',              ! 13 vertical line
     +     '<"4#',              ! 14 empty star
     +     '    ' /             ! 15 
Code:
      IF( I.LT.0 .OR. I.GT.14 ) THEN
         J=15
      ELSE
         J=I
      ENDIF
      SPCSYM=C(J)
C
      RETURN
      END

************************************************************************
      SUBROUTINE STOV
      IMPLICIT NONE
*
*     Transformation of beam position from 'screen window' 
*     coordinates into 'virtual window' coordinates .
*
#include "cplot10.for"
Code:
      XPOS=(XMAX-XMIN)/(XMAX0-XMIN0)*(XPOS0-XMIN0)+XMIN
      YPOS=(YMAX-YMIN)/(YMAX0-YMIN0)*(YPOS0-YMIN0)+YMIN
C
      RETURN
      END

************************************************************************
      SUBROUTINE VTOS
      IMPLICIT NONE
*
*     Transformation of beam position from 'virtual window' 
*     coordinates into 'screen window' coordinates.
*     Clipping is also performed ( preliminary version ).
*
#include "cplot10.for"
*
      REAL AMAX1,AMIN1
Code:
      XPOS0=(XMAX0-XMIN0)/(XMAX-XMIN)*(XPOS-XMIN)+XMIN0
      YPOS0=(YMAX0-YMIN0)/(YMAX-YMIN)*(YPOS-YMIN)+YMIN0
      XPOS0=AMIN1(XPOS0,XMAX0)
      XPOS0=AMAX1(XPOS0,XMIN0)
      YPOS0=AMIN1(YPOS0,YMAX0)
      YPOS0=AMAX1(YPOS0,YMIN0)
C
      RETURN
      END

************************************************************************
      SUBROUTINE PABORT(INDX)
      IMPLICIT NONE
*
*     Program stop in case of error.
*
      INTEGER INDX
Code:
      IF( INDX.EQ.1 ) THEN
         WRITE(*,'(A)') 'PABORT: Call of subroutine INITT missed!'
      ELSEIF (INDX.EQ.2) THEN
         WRITE(*,'(A)') 'PABORT: Window parameters are wrong!'
      ELSEIF (INDX.EQ.3) THEN
         WRITE(*,'(A)') 'PABORT: Virtual window not defined!'
      ELSEIF (INDX.EQ.4) THEN
         WRITE(*,'(A)') 'PABORT: Screen window not defined!'
      ENDIF
      WRITE(*,'(A)')  'Will stop now !'
      STOP
C
      END

************************************************************************
      SUBROUTINE SEE
      IMPLICIT NONE
*
*     Shows beam position plus windows settings.
*
      INTEGER MINX,MAXX,MINY,MAXY,BEAMX,BEAMY
      REAL XMIN,XMAX,YMIN,YMAX,XBEAM,YBEAM
      CHARACTER*80 TEXT(9)
Code:
      CALL SEELOC(BEAMX,BEAMY)
      CALL SEELC2(XBEAM,YBEAM)
      CALL SEETW(MINX,MAXX,MINY,MAXY)
      CALL SEEDW(XMIN,XMAX,YMIN,YMAX)
C
      WRITE(TEXT(1),'(2X,12(''-''),A,12(''-''))') 
     +     ' CURRENT WINDOW PARAMETERS '
      TEXT(2)=' Screen  window:'
      WRITE(TEXT(3),'(2(A6,I6))') 'Xmin=',MINX,'Xmax=',MAXX
      WRITE(TEXT(4),'(2(A6,I6))') 'Ymin=',MINY,'Xmax=',MAXY
      WRITE(TEXT(5),'(A,2I6)') ' Beam: (X,Y) = ',BEAMX,BEAMY
C
      TEXT(6)=' Virtual window:'
      WRITE(TEXT(7),'(2(A6,E13.7))') 'Xmin=',XMIN,'Xmax=',XMAX
      WRITE(TEXT(8),'(2(A6,E13.7))') 'Ymin=',YMIN,'Xmax=',YMAX
      WRITE(TEXT(9),'(A,2E13.7)')   ' Beam: X = ',XBEAM,YBEAM
      CALL IGMESS(9,TEXT,'PLOT-10','P') 
      WRITE(*,'(A80)') TEXT
C
      RETURN
      END

************************************************************************
      SUBROUTINE SEEB
      IMPLICIT NONE
*
*     Shows beam position.
*
      INTEGER BEAMX,BEAMY
      REAL XBEAM,YBEAM
      CHARACTER*80 TEXT(3)
Code:
      CALL SEELOC(BEAMX,BEAMY)
      CALL SEELC2(XBEAM,YBEAM)
      WRITE(TEXT(1),'(2X,15(''-''),A,15(''-''))') 
     +     ' CURRENT BEAM POSITON '
      WRITE(TEXT(2),'(A,2I6)') ' Screen window : (X,Y) = ',BEAMX,BEAMY
      WRITE(TEXT(3),'(A,2E13.7)') ' Virtual window: (X,Y) = '
     +     ,XBEAM,YBEAM
      CALL IGMESS(3,TEXT,'PLOT-10','P') 
      WRITE(*,'(A80)') TEXT
C
      RETURN
      END

************************************************************************
      SUBROUTINE SEELC2(X,Y)
      IMPLICIT NONE
*
*     Gets current beam position.
*
#include "cplot10.for"
*
      REAL X,Y
Code:
      X=XPOS
      Y=YPOS
C
      RETURN
      END

************************************************************************
      SUBROUTINE PSCLOS
*
*     Closes current Postscript file and deactivates metafile control.
*
C  - deactivate metafile 
      CALL IGMETA(0,0)

C  - clear workstation 2 (used by IGMETA)  
      CALL ICLWK(2)

C  - close PostScript file
      CLOSE(77)
C
      RETURN
      END

************************************************************************
      SUBROUTINE PSOPEN(FNAM,XSIZ,YSIZ)
      IMPLICIT NONE
*
*     Opens Postscript file and activates metafile control.
*
      CHARACTER FNAM*(*)
      REAL XSIZ,YSIZ
      LOGICAL FIRST/.TRUE./
      CHARACTER*200 FNAM0,F
      REAL XSIZ0,YSIZ0,X,Y
      SAVE FNAM0,XSIZ0,YSIZ0
Code:
      IF( FNAM.EQ.' ' ) THEN
         IF( FIRST ) RETURN
         X=XSIZ0
         Y=YSIZ0
         F=FNAM0
      ELSE
         CALL VBLANK(F,50)
         CALL VBLANK(FNAM0,50)
         X=XSIZ
         Y=YSIZ
         F=FNAM
         XSIZ0=X
         YSIZ0=Y
         FNAM0=F
      ENDIF

C - Open PostScript file
      OPEN(UNIT=77,FILE=F(1:INDEX(F,' ')-1)
     +     ,FORM='FORMATTED',STATUS='UNKNOWN')

C - activate metafile control -[format] nx ny type
C     format  = 4     : A4
C     (nx,ny) = (1,1) : one zone in x/y direction
C     type    = 1,4   : portrait mode
C               2,5   : landscape mode
C
      CALL IGMETA(77,-113)

C - define physical dimensions of the picture 
      CALL IGRNG(X,Y)
C
      FIRST=.FALSE.
C
      RETURN
      END

************************************************************************
      SUBROUTINE SETLIN(Y)
      IMPLICIT NONE
*
*     Updates current line position in COMMON /PLOT10/.
*     A call of SETLIN is sometimes needed when the former terminal output
*     is not performed with the PLOT-10 subroutines EOUTST, ANCHO ...
*
#include "cplot10.for"
*
      REAL Y
Code:
      YPOS0=INT(Y+.5)
C
      RETURN
      END
************************************************************************
      SUBROUTINE ITOADE(I,A,N)
      IMPLICIT NONE
*
*     Converts ASCII character set into the integer representation 
*     'ADE format' (1...127).
*
      INTEGER A(N),F,I,N,J,K
Code:
      F=10**(N-1)
      K=I
      DO J=1,N
         A(J)=K/F+48
         K=K-K/F*F
         F=F/10
      ENDDO
C
      RETURN
      END
 
************************************************************************
      SUBROUTINE EBCASC(IA,IB,IOPT)
      IMPLICIT NONE
*
*    Converts ASCII <-> EBCDIC.
*     Input : IA INTEGER
*     Output: IB INTEGER
*     IOPT=1: Conversion ASCII  -> EBCDIC
*          0: Conversion EBCDIC -> ASCII
*
      INTEGER IA,IB
      INTEGER I,IOPT,EBCDIC(95)
      DATA EBCDIC/64,90,127,123,91,108,80,125,77,93,92
     _     ,78,107,96,75,97
     _     ,240,241,242,243,244,245,246,247,248,249
     _     ,122,94,76,126,110,111,124
     _     ,193,194,195,196,197,198,199,200,201
     _     ,209,210,211,212,213,214,215,216,217
     _     ,226,227,228,229,230,231,232,233
     _     ,-1,224,-1,-1,109,121
     _     ,129,130,131,132,133,134,135,136,137
     _     ,145,146,147,148,149,150,151,152,153
     _     ,162,163,164,165,166,167,168,169,192
     _     ,106,208,161/
Code:
      IB=-1
      IF( IOPT.EQ.0 ) THEN
         DO I=1,95
            IF( EBCDIC(I).EQ.IA .AND. EBCDIC(I).NE.-1 ) IB=I+31
         ENDDO
      ELSEIF( IOPT.EQ.1 ) THEN
         IF( IA.GT.31 .AND. IA.LT.127 ) IB=EBCDIC(IA-31)
      ENDIF
C
      IF( IB.EQ.-1 ) THEN
        WRITE(*,10) IA,IOPT
 10     FORMAT('EBCASC: WRONG ARGUMENT, IA=',I6,' IOPT=',I6)
        IB=0
      ENDIF
C
      RETURN
      END


* The End
