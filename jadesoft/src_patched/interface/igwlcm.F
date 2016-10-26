      SUBROUTINE IGWLCM
      IMPLICIT NONE
*******************************************************************
*
*  Print welcome message of the JADE interactive graphics
*  program on the HIGZ window.
*
*  20/06/2000 Pedro Movilla Fernandez
*
******************************************************************
#include "cplot10.for"
      REAL XDOWN,XUP,YDOWN,YUP,XMID,YMID
C
      XDOWN = XTMIN+(XTMAX-XTMIN)/6.
      XUP   = XTMAX-(XTMAX-XTMIN)/6.
      YDOWN = YTMIN+(YTMAX-YTMIN)/2.
      YUP   = YTMAX-(YTMAX-YTMIN)/4.
      XMID  = (XDOWN+XUP)/2.
C - print title 
      CALL ISELNT(NTS)
      CALL IGPAVE(XDOWN,XUP,YDOWN,YUP,200.,1000,1011,'TRS')
      CALL ISTXFP(-6,2)
      CALL ISTXCI(4)
      CALL ISCHH(100.)
      CALL ISTXAL(2,3)
      CALL ITX(XMID,YUP*.92,' J A D E ')
      CALL ITX(XMID,YUP*.85,' Interactive  Graphics  Program ')
      CALL ISCHH(50.)
      CALL ISTXAL(1,3)
      CALL ITX(XDOWN*1.3,YUP*.76,' Original version: 17/10/88')
      CALL ITX(XDOWN*1.3,YUP*.72,' Modified for use on RS6000/AIX'
     + //' platforms: 18/11/99 (Pedro Movilla Fernandez)')
      CALL ISTXAL(3,0)
      CALL ISCHH(40.)

      CALL IUWK(0,1)
      CALL ISTXCI(1)
C - reset
      CALL SETCOL(' ')
      CALL IGDEF
C
      RETURN
      END
