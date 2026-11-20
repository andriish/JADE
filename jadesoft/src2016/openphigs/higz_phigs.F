************************************************************************
*
*  HIGZ_PHIGS : the subset of the CERN HIGZ interface used by JADE,
*  =========== implemented on top of OpenPHIGS (Fortran binding).
*
*  JADE draws through the PLOT-10 emulation (interface/plot10.F), which
*  calls HIGZ.  This file provides the same entry points so that the
*  JADE code does not change: with -DJADE_GRAPHICS=OPENPHIGS it is
*  linked instead of the CERNLIB HIGZ (libgrafX11).
*
*  Mapping
*    HIGZ is immediate mode, PHIGS is retained mode.  Two PHIGS
*    structures are filled:
*      structure 1 : "screen"    -> drawn on the display workstation
*                                   (attributes of the IXSETxx calls)
*      structure 2 : "metafile"  -> drawn on an EPS workstation when the
*                                   picture is saved (attributes of the
*                                   ISxxxx calls); this replaces the
*                                   HIGZ PostScript metafile.
*    World coordinates are converted to NDC (0..1) here, so PHIGS
*    always works in a unit square with its default (identity) view.
*
*  Not implemented: graphics input (IGLOC returns button 0), HIGZ
*  fill-area patterns (IGPAVE draws a plain frame).
*
************************************************************************
      BLOCK DATA HPGSBD
      IMPLICIT NONE
#include "chpgs.inc"
      DATA LOPEN/.FALSE./, LMETA/.FALSE./, LPOSTD/.FALSE./
      DATA LPEND/.FALSE./, NPRIM2/0/
      DATA ICURNT/0/, MLUN/0/
      DATA CHH/0.01/, TANG/0./, ITXH/1/, ITXV/0/
      DATA ILC1,IMC1,ITC1,IFC1/1,1,1,1/
      DATA ILC2,IMC2,ITC2,IFC2/1,1,1,1/
      DATA ILN/1/, WLW/1./, IMK/1/, SMK/1./
      END
************************************************************************
      SUBROUTINE HPGERR(TXT)
      IMPLICIT NONE
      CHARACTER*(*) TXT
      WRITE(*,'(A,A)') ' HIGZ_PHIGS: ',TXT
      RETURN
      END
************************************************************************
      SUBROUTINE IGINIT(KDUM)
      IMPLICIT NONE
#include "chpgs.inc"
      INTEGER KDUM,I
*     Initialise PHIGS and the normalization transformations
      CALL POPPH(0,1)
      DO I=0,MAXNT
         WX1(I)=0.
         WX2(I)=1.
         WY1(I)=0.
         WY2(I)=1.
         VX1(I)=0.
         VX2(I)=1.
         VY1(I)=0.
         VY2(I)=1.
      ENDDO
      ICURNT=0
*     Default HIGZ colours: 0 white, 1 black, 2 red, 3 green, 4 blue,
*     5 yellow, 6 magenta, 7 cyan
      DO I=0,MAXCI
         CR(I)=0.
         CG(I)=0.
         CB(I)=0.
      ENDDO
      CALL HPGCOL(0,1.,1.,1.)
      CALL HPGCOL(1,0.,0.,0.)
      CALL HPGCOL(2,1.,0.,0.)
      CALL HPGCOL(3,0.,1.,0.)
      CALL HPGCOL(4,0.,0.,1.)
      CALL HPGCOL(5,1.,1.,0.)
      CALL HPGCOL(6,1.,0.,1.)
      CALL HPGCOL(7,0.,1.,1.)
      RETURN
      END
************************************************************************
      SUBROUTINE IGSSE(KDUM,KWTYPE)
      IMPLICIT NONE
#include "chpgs.inc"
      INTEGER KDUM,KWTYPE,I
*     Open the display workstation (wk id 1; type 3 = GL, input/output,
*     double buffered) and the two structures
      CALL POPWK(1,0,3)
      CALL PSDUS(1,4,0)
      DO I=0,MAXCI
         CALL HPGCSET(1,I)
      ENDDO
      LOPEN=.TRUE.
      LPOSTD=.FALSE.
      CALL POPST(1)
      CALL PCLST
      CALL POPST(2)
      CALL PCLST
      RETURN
      END
************************************************************************
      SUBROUTINE HPGCOL(I,R,G,B)
      IMPLICIT NONE
#include "chpgs.inc"
      INTEGER I
      REAL R,G,B
      IF( I.LT.0 .OR. I.GT.MAXCI ) RETURN
      CR(I)=R
      CG(I)=G
      CB(I)=B
      IF( LOPEN ) CALL HPGCSET(1,I)
      RETURN
      END
************************************************************************
      SUBROUTINE HPGCSET(IWK,I)
      IMPLICIT NONE
#include "chpgs.inc"
      INTEGER IWK,I
      REAL C(3)
      C(1)=CR(I)
      C(2)=CG(I)
      C(3)=CB(I)
      CALL PSCR(IWK,I,3,C)
      RETURN
      END
************************************************************************
      SUBROUTINE ISCR(IWK,I,R,G,B)
      IMPLICIT NONE
      INTEGER IWK,I
      REAL R,G,B
      CALL HPGCOL(I,R,G,B)
      RETURN
      END
************************************************************************
      SUBROUTINE IXSETCO(I,R,G,B)
      IMPLICIT NONE
      INTEGER I
      REAL R,G,B
      CALL HPGCOL(I,R,G,B)
      RETURN
      END
************************************************************************
      SUBROUTINE IGEND
      IMPLICIT NONE
#include "chpgs.inc"
      IF( LOPEN ) THEN
         IF( LPEND ) CALL HPGFLUSH
         CALL PCLWK(1)
         CALL PCLPH()
         LOPEN=.FALSE.
      ENDIF
      RETURN
      END
************************************************************************
      SUBROUTINE IGTERM
      IMPLICIT NONE
      CALL IUWK(0,1)
      RETURN
      END
************************************************************************
      SUBROUTINE IXSYNC(K)
      IMPLICIT NONE
      INTEGER K
      RETURN
      END
************************************************************************
      SUBROUTINE IGRNG(X,Y)
      IMPLICIT NONE
#include "chpgs.inc"
      REAL X,Y
      PSX=X
      PSY=Y
      RETURN
      END
************************************************************************
*   Normalization transformations
************************************************************************
      SUBROUTINE ISWN(NT,X1,X2,Y1,Y2)
      IMPLICIT NONE
#include "chpgs.inc"
      INTEGER NT
      REAL X1,X2,Y1,Y2
      IF( NT.LT.0 .OR. NT.GT.MAXNT ) RETURN
      WX1(NT)=X1
      WX2(NT)=X2
      WY1(NT)=Y1
      WY2(NT)=Y2
      RETURN
      END
************************************************************************
      SUBROUTINE ISVP(NT,X1,X2,Y1,Y2)
      IMPLICIT NONE
#include "chpgs.inc"
      INTEGER NT
      REAL X1,X2,Y1,Y2
      IF( NT.LT.0 .OR. NT.GT.MAXNT ) RETURN
      VX1(NT)=X1
      VX2(NT)=X2
      VY1(NT)=Y1
      VY2(NT)=Y2
      RETURN
      END
************************************************************************
      SUBROUTINE ISELNT(NT)
      IMPLICIT NONE
#include "chpgs.inc"
      INTEGER NT
      IF( NT.LT.0 .OR. NT.GT.MAXNT ) RETURN
      ICURNT=NT
      RETURN
      END
************************************************************************
      REAL FUNCTION HPGX(X)
      IMPLICIT NONE
#include "chpgs.inc"
      REAL X,D
      D=WX2(ICURNT)-WX1(ICURNT)
      IF( D.EQ.0. ) D=1.
      HPGX=VX1(ICURNT)+(X-WX1(ICURNT))*(VX2(ICURNT)-VX1(ICURNT))/D
      RETURN
      END
************************************************************************
      REAL FUNCTION HPGY(Y)
      IMPLICIT NONE
#include "chpgs.inc"
      REAL Y,D
      D=WY2(ICURNT)-WY1(ICURNT)
      IF( D.EQ.0. ) D=1.
      HPGY=VY1(ICURNT)+(Y-WY1(ICURNT))*(VY2(ICURNT)-VY1(ICURNT))/D
      RETURN
      END
************************************************************************
      REAL FUNCTION HPGSY()
*     scale factor world -> NDC in y
      IMPLICIT NONE
#include "chpgs.inc"
      REAL D
      D=WY2(ICURNT)-WY1(ICURNT)
      IF( D.EQ.0. ) D=1.
      HPGSY=(VY2(ICURNT)-VY1(ICURNT))/D
      RETURN
      END
************************************************************************
*   Attributes
************************************************************************
      SUBROUTINE ISPLCI(I)
      IMPLICIT NONE
#include "chpgs.inc"
      INTEGER I
      ILC1=I
      RETURN
      END
      SUBROUTINE ISPMCI(I)
      IMPLICIT NONE
#include "chpgs.inc"
      INTEGER I
      IMC1=I
      RETURN
      END
      SUBROUTINE ISTXCI(I)
      IMPLICIT NONE
#include "chpgs.inc"
      INTEGER I
      ITC1=I
      RETURN
      END
      SUBROUTINE ISFACI(I)
      IMPLICIT NONE
#include "chpgs.inc"
      INTEGER I
      IFC1=I
      RETURN
      END
      SUBROUTINE ISFAIS(I)
      IMPLICIT NONE
      INTEGER I
      RETURN
      END
      SUBROUTINE IXSETLC(I)
      IMPLICIT NONE
#include "chpgs.inc"
      INTEGER I
      ILC2=I
      RETURN
      END
      SUBROUTINE IXSETMC(I)
      IMPLICIT NONE
#include "chpgs.inc"
      INTEGER I
      IMC2=I
      RETURN
      END
      SUBROUTINE IXSETTC(I)
      IMPLICIT NONE
#include "chpgs.inc"
      INTEGER I
      ITC2=I
      RETURN
      END
      SUBROUTINE IXSETFC(I)
      IMPLICIT NONE
#include "chpgs.inc"
      INTEGER I
      IFC2=I
      RETURN
      END
      SUBROUTINE ISLN(I)
      IMPLICIT NONE
#include "chpgs.inc"
      INTEGER I
      ILN=MAX(1,MIN(4,I))
      RETURN
      END
      SUBROUTINE IXSETLN(I)
      IMPLICIT NONE
      INTEGER I
      RETURN
      END
      SUBROUTINE ISLWSC(W)
      IMPLICIT NONE
#include "chpgs.inc"
      REAL W
      WLW=MAX(W,1.)
      RETURN
      END
      SUBROUTINE ISMK(I)
      IMPLICIT NONE
#include "chpgs.inc"
      INTEGER I
      IMK=MAX(1,MIN(5,I))
      RETURN
      END
      SUBROUTINE ISMKSC(S)
      IMPLICIT NONE
#include "chpgs.inc"
      REAL S
      SMK=MAX(S,0.1)
      RETURN
      END
      SUBROUTINE ISCHH(H)
      IMPLICIT NONE
#include "chpgs.inc"
      REAL H
      CHH=H
      RETURN
      END
      SUBROUTINE ISTXAL(IH,IV)
      IMPLICIT NONE
#include "chpgs.inc"
      INTEGER IH,IV
      ITXH=IH
      ITXV=IV
      RETURN
      END
      SUBROUTINE ISTXFP(IFONT,IPREC)
      IMPLICIT NONE
      INTEGER IFONT,IPREC
      RETURN
      END
      SUBROUTINE IGSET(CHOPT,VAL)
      IMPLICIT NONE
#include "chpgs.inc"
      CHARACTER*(*) CHOPT
      REAL VAL
      IF( CHOPT(1:4).EQ.'TANG' ) TANG=VAL
      RETURN
      END
************************************************************************
*   Output primitives. Each is written into structure 1 (screen) and,
*   if the metafile is active, into structure 2 (EPS copy).
************************************************************************
      SUBROUTINE HPGOPN(IS)
*     Open structure IS for appending
      IMPLICIT NONE
      INTEGER IS
      CALL POPST(IS)
      RETURN
      END
************************************************************************
      SUBROUTINE HPGLIN(IS)
*     line attributes of scheme IS
      IMPLICIT NONE
#include "chpgs.inc"
      INTEGER IS
      IF( IS.EQ.1 ) THEN
         CALL PSPLCI(ILC2)
      ELSE
         CALL PSPLCI(ILC1)
      ENDIF
      CALL PSLN(ILN)
*     HIGZ line widths are multiples of a thin base width, OpenPHIGS
*     widths are in pixels: scale them down (JADE uses 1 ... 6)
      CALL PSLWSC(MAX(1.0,WLW*0.35))
      RETURN
      END
************************************************************************
      SUBROUTINE IPL(N,X,Y)
      IMPLICIT NONE
#include "chpgs.inc"
      INTEGER N,I,IS
      REAL X(*),Y(*),HPGX,HPGY
      REAL XN(MAXP),YN(MAXP)
      IF( N.LT.2 .OR. N.GT.MAXP .OR. .NOT.LOPEN ) RETURN
      DO I=1,N
         XN(I)=HPGX(X(I))
         YN(I)=HPGY(Y(I))
      ENDDO
      DO IS=1,2
         IF( IS.EQ.1 .OR. LMETA ) THEN
            CALL POPST(IS)
            CALL HPGLIN(IS)
            CALL PPL(N,XN,YN)
            CALL PCLST
            IF( IS.EQ.2 ) NPRIM2=NPRIM2+1
         ENDIF
      ENDDO
      LPOSTD=.FALSE.
      RETURN
      END
************************************************************************
      SUBROUTINE IPM(N,X,Y)
      IMPLICIT NONE
#include "chpgs.inc"
      INTEGER N,I,IS
      REAL X(*),Y(*),HPGX,HPGY
      REAL XN(MAXP),YN(MAXP)
      IF( N.LT.1 .OR. N.GT.MAXP .OR. .NOT.LOPEN ) RETURN
      DO I=1,N
         XN(I)=HPGX(X(I))
         YN(I)=HPGY(Y(I))
      ENDDO
      DO IS=1,2
         IF( IS.EQ.1 .OR. LMETA ) THEN
            CALL POPST(IS)
            IF( IS.EQ.1 ) THEN
               CALL PSPMCI(IMC2)
            ELSE
               CALL PSPMCI(IMC1)
            ENDIF
            CALL PSMK(IMK)
*           OpenPHIGS markers are about 200 times larger than HIGZ ones
*           (scale 1 = 2 NDC units; HIGZ: dot = 1 pixel at scale 0.2)
            CALL PSMKSC(SMK*0.005)
            CALL PPM(N,XN,YN)
            CALL PCLST
            IF( IS.EQ.2 ) NPRIM2=NPRIM2+1
         ENDIF
      ENDDO
      RETURN
      END
************************************************************************
      SUBROUTINE ITX(X,Y,CTEXT)
      IMPLICIT NONE
#include "chpgs.inc"
      REAL X,Y,HPGX,HPGY,HPGSY,XN,YN,H,UX,UY
      CHARACTER*(*) CTEXT
      INTEGER IS,L
      IF( .NOT.LOPEN ) RETURN
      L=LEN_TRIM(CTEXT)
      IF( L.LT.1 ) RETURN
      XN=HPGX(X)
      YN=HPGY(Y)
      H=ABS(CHH*HPGSY())
      UX=-SIN(TANG*0.017453292)
      UY= COS(TANG*0.017453292)
      DO IS=1,2
         IF( IS.EQ.1 .OR. LMETA ) THEN
            CALL POPST(IS)
            IF( IS.EQ.1 ) THEN
               CALL PSTXCI(ITC2)
            ELSE
               CALL PSTXCI(ITC1)
            ENDIF
*           stroke text is drawn with the current line width: keep it thin
            CALL PSLWSC(1.0)
            CALL PSLN(1)
            CALL PSCHH(H)
            CALL PSCHUP(UX,UY)
            CALL PSTXAL(ITXH,ITXV)
            CALL PTX(XN,YN,CTEXT(1:L))
            CALL PCLST
            IF( IS.EQ.2 ) NPRIM2=NPRIM2+1
         ENDIF
      ENDDO
      RETURN
      END
************************************************************************
      SUBROUTINE HPGBOX(X1,X2,Y1,Y2,IS,ICOL,IFILL)
*     rectangle (world coordinates of the current NT) in structure IS
      IMPLICIT NONE
#include "chpgs.inc"
      REAL X1,X2,Y1,Y2,HPGX,HPGY,XB(5),YB(5)
      INTEGER IS,ICOL,IFILL
      XB(1)=HPGX(X1)
      YB(1)=HPGY(Y1)
      XB(2)=HPGX(X2)
      YB(2)=YB(1)
      XB(3)=XB(2)
      YB(3)=HPGY(Y2)
      XB(4)=XB(1)
      YB(4)=YB(3)
      XB(5)=XB(1)
      YB(5)=YB(1)
      CALL POPST(IS)
      IF( IFILL.NE.0 ) THEN
         CALL PSIS(2)
         CALL PSICI(ICOL)
         CALL PFA(4,XB,YB)
      ELSE
         CALL PSPLCI(ICOL)
         CALL PSLN(1)
         CALL PSLWSC(1.)
         CALL PPL(5,XB,YB)
      ENDIF
      CALL PCLST
      IF( IS.EQ.2 ) NPRIM2=NPRIM2+1
      RETURN
      END
************************************************************************
      SUBROUTINE IXBOX(X1,X2,Y1,Y2,MODE)
*     screen only; MODE=1 filled with the IXSETFC colour
      IMPLICIT NONE
#include "chpgs.inc"
      REAL X1,X2,Y1,Y2
      INTEGER MODE
      IF( .NOT.LOPEN ) RETURN
      IF( MODE.EQ.1 ) THEN
         CALL HPGBOX(X1,X2,Y1,Y2,1,IFC2,1)
      ELSE
         CALL HPGBOX(X1,X2,Y1,Y2,1,ILC2,0)
      ENDIF
      RETURN
      END
************************************************************************
      SUBROUTINE IGBOX(X1,X2,Y1,Y2)
*     HIGZ-level box with the fill attributes: metafile copy only
      IMPLICIT NONE
#include "chpgs.inc"
      REAL X1,X2,Y1,Y2
      IF( .NOT.LOPEN .OR. .NOT.LMETA ) RETURN
      CALL HPGBOX(X1,X2,Y1,Y2,2,IFC1,1)
      RETURN
      END
************************************************************************
      SUBROUTINE IGPAVE(X1,X2,Y1,Y2,DZ,ISBOX,ISFRAM,CHOPT)
*     "pave": plain filled box with a frame (no HIGZ fill patterns)
      IMPLICIT NONE
#include "chpgs.inc"
      REAL X1,X2,Y1,Y2,DZ
      INTEGER ISBOX,ISFRAM
      CHARACTER*(*) CHOPT
      IF( .NOT.LOPEN ) RETURN
      CALL HPGBOX(X1,X2,Y1,Y2,1,0,1)
      CALL HPGBOX(X1,X2,Y1,Y2,1,ILC2,0)
      RETURN
      END
************************************************************************
*   Workstation control
************************************************************************
      SUBROUTINE ICLRWK(IWK,MODE)
      IMPLICIT NONE
#include "chpgs.inc"
      INTEGER IWK,MODE
      IF( .NOT.LOPEN ) RETURN
      CALL PDST(1)
      CALL PDST(2)
      NPRIM2=0
      CALL POPST(1)
      CALL PCLST
      CALL POPST(2)
      CALL PCLST
      LPOSTD=.FALSE.
      CALL IUWK(0,1)
      RETURN
      END
************************************************************************
      SUBROUTINE IUWK(IWK,MODE)
      IMPLICIT NONE
#include "chpgs.inc"
      INTEGER IWK,MODE
      IF( .NOT.LOPEN ) RETURN
      IF( .NOT.LPOSTD ) THEN
         CALL PPOST(1,1,1.0)
         LPOSTD=.TRUE.
      ENDIF
      CALL PUWK(1,1)
      RETURN
      END
************************************************************************
      SUBROUTINE IGMETA(LUN,KMETA)
*     Activate (KMETA=-113) or deactivate (0) the PostScript metafile.
*     If a picture is waiting to be written (see ICLWK) it is written now
*     unless the same file is opened again (JADE does this at every
*     ERASE: the new picture simply replaces the old one).
      IMPLICIT NONE
#include "chpgs.inc"
      INTEGER LUN,KMETA
      CHARACTER*256 FN
      LOGICAL LNAMED
      IF( KMETA.NE.0 ) THEN
         LMETA=.TRUE.
         MLUN=LUN
         IF( LPEND ) THEN
            INQUIRE(UNIT=LUN,NAMED=LNAMED,NAME=FN)
            IF( LNAMED .AND. FN.EQ.CPEND ) THEN
               LPEND=.FALSE.
            ELSE
               CALL HPGFLUSH
            ENDIF
         ENDIF
      ELSE
         LMETA=.FALSE.
      ENDIF
      RETURN
      END
************************************************************************
      SUBROUTINE ICLWK(IWK)
*     Workstation 2 is the HIGZ metafile. The picture in structure 2 is
*     only marked as pending here and written by HPGFLUSH.
      IMPLICIT NONE
#include "chpgs.inc"
      INTEGER IWK
      CHARACTER*256 FN
      LOGICAL LNAMED
      IF( IWK.NE.2 .OR. .NOT.LOPEN .OR. MLUN.LE.0 ) RETURN
      INQUIRE(UNIT=MLUN,NAMED=LNAMED,NAME=FN)
      IF( .NOT.LNAMED ) RETURN
      CPEND=FN
      LPEND=.TRUE.
      RETURN
      END
************************************************************************
      SUBROUTINE HPGFLUSH
*     Write the pending picture (structure 2) with an OpenPHIGS
*     hardcopy workstation, then start a new empty picture.
*       HPG_HCTYPE=7 : EPS (default for 64 bit; JADE.nnn.ps)
*       HPG_HCTYPE=5 : PNG (default for 32 bit; JADE.nnn.png). The EPS
*                      export of OpenPHIGS asks gl2ps for a 4 GB buffer
*                      whose size overflows in 32 bit programs.
      IMPLICIT NONE
#include "chpgs.inc"
#ifndef HPG_HCTYPE
#define HPG_HCTYPE 7
#endif
      INTEGER I,L
      CHARACTER*256 FN
      CHARACTER*8 CENV
      REAL HCSF
      LPEND=.FALSE.
      FN=CPEND
      L=LEN_TRIM(FN)
#if HPG_HCTYPE == 5
*     JADE has created FN (.ps) for the HIGZ PostScript output: the
*     stub is removed and FN.png written instead
      IF( L.GT.3 .AND. FN(L-2:L).EQ.'.ps' ) THEN
         OPEN(UNIT=78,FILE=FN(1:L),STATUS='OLD',ERR=5)
         CLOSE(78,STATUS='DELETE')
 5       CONTINUE
         L=L-3
      ENDIF
      FN=FN(1:L)//'.png'
      L=LEN_TRIM(FN)
#endif
*     hardcopy scale factor (default 2: 2048 x 2048 pixels for PNG);
*     JADE_PHIGS_HCSCALE overrides it
      HCSF=2.0
      CALL GETENV('JADE_PHIGS_HCSCALE',CENV)
      IF( LEN_TRIM(CENV).GT.0 ) THEN
         READ(CENV,*,ERR=6) HCSF
 6       CONTINUE
      ENDIF
      IF( NPRIM2.GT.0 .AND. LOPEN ) THEN
         CALL PXSHCSF(95,HCSF)
         CALL POPWK(95,78,HPG_HCTYPE)
         CALL PSFNAME(95,FN(1:L))
         DO I=0,MAXCI
            CALL HPGCSET(95,I)
         ENDDO
         CALL PPOST(95,2,1.0)
         CALL PUWK(95,1)
         CALL PCLWK(95)
         WRITE(*,'(A,A)') ' HIGZ_PHIGS: wrote ',FN(1:L)
#if HPG_HCTYPE == 5
*        JADE_PHIGS_SCREENSHOT set: also write what is shown on the
*        screen (structure 1, IXSETxx colours) as FN.screen.png
         CALL GETENV('JADE_PHIGS_SCREENSHOT',CENV)
         IF( LEN_TRIM(CENV).GT.0 ) THEN
            CALL PXSHCSF(96,HCSF)
            CALL POPWK(96,78,HPG_HCTYPE)
            CALL PSFNAME(96,FN(1:L-4)//'.screen.png')
            DO I=0,MAXCI
               CALL HPGCSET(96,I)
            ENDDO
            CALL PPOST(96,1,1.0)
            CALL PUWK(96,1)
            CALL PCLWK(96)
         ENDIF
#endif
      ENDIF
      NPRIM2=0
      CALL PDST(2)
      CALL POPST(2)
      CALL PCLST
      RETURN
      END
************************************************************************
*   Input and messages
************************************************************************
      SUBROUTINE IGLOC(ICHOIC,NT,IBN,XNDC,YNDC,XWC,YWC)
      IMPLICIT NONE
#include "chpgs.inc"
      INTEGER ICHOIC,NT,IBN
      REAL XNDC,YNDC,XWC,YWC
      CALL HPGERR('IGLOC: locator input is not implemented')
      NT=ICURNT
      IBN=0
      XNDC=0.
      YNDC=0.
      XWC=0.
      YWC=0.
      RETURN
      END
************************************************************************
      SUBROUTINE IGMESS(N,TEXT,CNAME,CHOPT)
      IMPLICIT NONE
      INTEGER N,I
      CHARACTER*(*) TEXT(*),CNAME,CHOPT
      DO I=1,N
         WRITE(*,'(A)') TEXT(I)(1:LEN_TRIM(TEXT(I)))
      ENDDO
      RETURN
      END
