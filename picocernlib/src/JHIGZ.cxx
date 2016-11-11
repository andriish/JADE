#include "TText.h"
#include "TPad.h"
#include "TCanvas.h"
#include "TTree.h"
#include "TBox.h"
#include "TStyle.h"
#include "TPaveLabel.h"
#include "TPolyLine.h"
#include "TPolyMarker.h"
#include <iostream>
#include <map>
#include <string>
#include <sstream>

TCanvas*   J_GLOBAL_C;
TPad*   J_GLOBAL_P;
extern "C" {
/*
void igstrt_()
{
//Init

}
*/

//      CALL ISWN(NTS,XTMIN,XTMAX,YTMIN,YTMAX)
//      CALL ISVP(NTS,0.,1.,0.,1.)

void iswn_(int &NTS,float &XTMIN,float &XTMAX,float &YTMIN,float &YTMAX)
{
J_GLOBAL_C= new TCanvas();


}


void isvp_(int &NTS,float &XTMIN,float &XTMAX,float &YTMIN,float &YTMAX)
{

J_GLOBAL_C->cd();
J_GLOBAL_P= new TPad("AAA","AAA",XTMIN,XTMAX,YTMIN,YTMAX);

}
// CALL SETCOL('*INI')
/*
void setcol_(const char*)
{
	
	
	
}	
*/

/*
//CALL IGFIN
void igfin_()
{
if (J_GLOBAL_C) delete 	J_GLOBAL_C;
if (J_GLOBAL_P) delete 	J_GLOBAL_P;
}	
*/
//`iclrwk_'

void iclrwk_()
{
if (J_GLOBAL_P) J_GLOBAL_P->Clear();
}	


//`iclwk_'
void iclwk_()
{
//terminates usage of ws
}


//`igbox_'

void igbox_(float& x1, float & x2,float& y1, float & y2)
{
TBox* B= new TBox(x1,y1,x2,y2);
B->SetFillColor(1);
B->SetFillStyle(1);
B->SetLineWidth(1);
B->SetLineColor(1);
B->Draw();
}	

void ixbox_(float& x1, float & x2,float& y1, float & y2, int mode)
{
TBox* B= new TBox(x1,y1,x2,y2);
B->SetFillColor(1);
B->SetFillStyle(1);
B->SetLineWidth(1);
B->SetLineColor(1);
B->Draw();
}	


void igrng_(float *a, float*b)
{
	
	
}	

void igset_(char* , float val){
/*
CALL IGSET
 (CHNAME,VAL)
Action: Routine used to set the value of attributes related to primitives and/or macroprimitives. The first
parameter is the mnemonic name of the parameter, the second is the value to be assigned. Note that all
the basic primitives attributes can also be set with this routine.
CHNAME
VAL
Character variable specifying the name of the parameter to be set (type CHARACTER*4). This
is an UPPERCASE character string.
Floating point value of the parameter (must be specified as a REAL number).
A value of 0.0 indicates that the parameter value must be reset to its default value.
*/


}

//`igend_'
void igend_()
{
	
	
}	
//`iginit_'
void iginit_(int& a)
{
	
	
}	
//`igloc_'

void igloc_(int *is, int nt, int BN, float &a,float &b,float &c,float &d)
{
	
}	

//`igmess_'
void igmess_(int &n,char** chmess,char *chtit,char chopt)
{
	
	
}	
//`igmeta_'
void igmeta_(int& LUN, int& KWTYPE)
{
	
	
}	
//`igpave_'
//(X1,X2,Y1,Y2,DZ,ISBOX,ISFRAM,CHOPT)
void igpave_()
{
	TPaveLabel* P= new TPaveLabel();
	
	
	P->Draw();
}	
/*`
`igrng_'
`igset_'
`igsse_'
`igterm_'
*/ 

//`ipl_'
void ipl_(int&n, float* x, float* y)
{
		TPolyLine* P=new TPolyLine(n,x,y);
	P->SetLineWidth(1);
	P->SetLineStyle(1);
	P->Draw();
	
}	


//`ipm_'
void ipm_(int&n, float* x, float* y)
{
		TPolyMarker* P=new TPolyMarker(n,x,y);
	//P->SetLineWidth(1);
	//P->SetLineStyle(1);
	P->Draw();
	
}	


//`ischh_'
void ischh_(float &h)
{
//gStyle->SetFontSize(h);
 
} 
//`iscr_'
void iscr_(int &w, int &ci, float &r,float&g,float&b)
{
	
	
	
}	
//`iselnt_'
void iselnt_(int & t)
{
	
	
}	

void isfaci_(int* col )
{
/*
Action: This routine sets the fill area colour index attribute for use by future invocations of IFA. The
routine IGSET (see section 4.11) can also be used with the parameter FACI.
Parameter description:
ICOLI
 Fill area colour index.

*/
}	

void isfais_(int* sty )
{
/*
Action: This routine sets the fill area interior style attribute for use by future invocations of IFA. The
routine IGSET (see section 4.11) can also be used with the parameter FAIS.
Parameter description:
INTS
 Fill area interior style. Possible values are:
0
 Hollow: the perimeter of the filled area, after clipping, is drawn using solid lines.
1
 Solid: the area is filled solidly.
2
 Pattern: the area is filled with a dot-dashed pattern.
3
 Hatched: the area is filled according to the current value of the fill area style index.


*/
}	

void isln_(int *a)
{
/*
Action: This routine sets the line type attribute for use b
* y future invocations of IPL. All workstations
support at least line types 1 through 4 (see figure 3.5). Other line types may be supported. If a requested
line type is not supported on a workstation, line type 1 is used when polylines are created. The routine
IGSET (see section 4.11) can also be used with the parameter LTYP.
Parameter description:
LTYPE
Line type (positive number).
1
 Solid lines
2
 Dashed lines
3
 Dotted lines
4
 Dashed-dotted lines
*/
}



void islwsc_(int* a)
{
/*
Action: This routine sets the width of a line for use by future invocations of the polyline drawing routine
IPL. The actual line width is determined by a nominal line width (workstation-dependent) multiplied by
the line width scale factor. The nominal line width is one pixel on screens. On PostScript printers the
nominal line width is one “dot”. Therefore the width of a line can vary from a printer to another depending
on the printer definition (300 dots per inch, 400 dots per inch etc.). The figure 3.6 shows some examples
of various line width. The routine IGSET (see section 4.11) can also be used with the parameter LWID.
Parameter description:
WIDTH
 Line width scale factor.
*/



}



void ismk_(int *a)
{
/*
Action: This routine sets the marker type attribute for use by future invocations of IPM. All workstations
support at least the marker types 1 through 5 (see below). More marker types may be supported by the
underlying graphics package. Marker types 20 to 31 are also defined, according to the figure 3.7, and
are independent from the underlying graphics package used. If a requested marker type is not supported
on a workstation, marker type 1 (a point) is used when polymarkers are created. The routine IGSET (see
section 4.11) can also be used with the parameter MTYP.
Parameter description:
MTYPE
Marker type (positive number)
1 Point shape (·).
2 Plus shape (+).
3 Asterisk shape (∗).
4 Circle shape (◦).
5 X shape (×).
*/
	
}	
void ismksc_(int* a)
{
/*
Action: This routine sets the marker scale factor. This scale factor is applied on the nominal size of the
marker. On all workstation, except PostScript files, the marker type 1 is not scalable. The routine IGSET
(see section 4.11) can also be used with the parameter MSCF.
Parameter description:
SSFM
 Scale factor applied to markers. (≥ 0.)
*/	
	
	
}	

/*
`isplci_'
`ispmci_'
`istxal_'
`istxci_'
`istxfp_'
`isvp_'
`iswn_'
`itx_'
`iuwk_'
`ixbox_'
`ixsetco_'
`ixsetfc_'
`ixsetlc_'
`ixsetln_'
`ixsetmc_'
`ixsettc_'
*/
void ixsync_(bool mode)
{
	
	
}	


/*
 CALL HLABEL (ID,NLAB,*CLAB*,CHOPT)

Action: Associates alphanumeric labels with a histogram. This routine can be called for a histogram after it has been filled, and then the labels specified will be shown on the respective axes. The routine can also be called before a histogram is filled , and in this case, when filling, a certain order can be imposed. By default the entries will be automatically ordered.
*/
void hlabel_(int*id , int N,char** clab,const char* opt)
{


}

void iuwk_(int*a, int*b)
{
/*
GKS
 CALL IUWK
 (KWKID,IRFLG)
Action: This routine updates the workstation KWKID. It send all buffered output to the screen. In the
X11 version of HIGZ, this routine allows to flush the X11 buffer. This routine is usually called with the
first parameter equal to 0 and the second to 1.
Parameter description:
KWKID
 Workstation identifier. KWKID = 0 updates all the current open workstations.
IRFLG
 Regeneration flag:
0
 postpone update workstation (only when the underlying graphics package is GKS)
1
 refresh entire display
2
 update current view
*/

gPad->Update();

}


void itx_(float*x, float*y, char* txt)
{
	
	   TText *t = new TText(*x,*y,txt);
   t->SetTextAlign(22);
   t->SetTextColor(kRed+2);
   t->SetTextFont(43);
   t->SetTextSize(40);
   t->SetTextAngle(45);
   t->Draw();
	
}	

void mzebra_(int *a){}
void mzpaw_(int *a){}



void ixsetco_(int*i, float r, float g, float b)
{/*

CALL IXSETCO
 (INDEX,R,G,B)
Action: Set colour intensities for given colour index.
Parameter description:
INDEX
 Colour index.
R
 Red intensity between 0.0 and 1.0.
G
 Green intensity between 0.0 and 1.0.
B
 Blue intensity between 0.0 and 1.0.
*/



}
void istxal_(int* a, int *b)
{/*
GKS
 CALL ISTXAL
 (ITXALH,ITXALV)
Action: This routine sets the text alignment attribute for use by future invocations of ITX. Text alignment
controls the placement of the character string with respect to the position specified in the call to ITX.
Horizontal alignment specifies which end of the string (or its geometric center) is aligned with the point
specified in ITX. For a given horizontal alignment, the vertical alignment controls whether the tops of tall
characters or the bottoms of capital letters line up with the point specified (see figure 3.9). The routine
IGSET (see section 4.11) can also be used with the parameter TXAL.
Parameter description:
ITXALH
ITXALV
Horizontal alignment specifier (0≤ITXALH≤3) 0
 Left end of string at point specified (normal).
1 Same as 0.
2 Center of string at point specified.
3 Right end of string at point specified.
Vertical alignment specifier (0≤ITXALV≤5)
0 Base of the characters (normal).
1 Top of tallest characters.
2 Same as 2.
3 Middle of tallest characters.
*/




}



void istxci_(int* a)
{
/*
Text colour index.
GKS
 CALL ISTXCI
 (ICOLI)
Action: This routine sets the text colour index attribute for use by future invocations of ITX. The routinIGSET (see section 4.11) can also be used with the parameter TXCI.
Parameter description:
ICOLI
 Text colour index.
3.6.3
 Fill area interior style
*/


}
void isplci_(int*a)
{/*
GKS
 CALL ISPLCI
 (ICOLI)
Action: This routine sets the polyline colour index attribute for use by future invocations of IPL. The
routine IGSET (see section 4.11) can also be used with the parameter PLCI.
Parameter description:
ICOLI
 Polyline colour index.
*/


}



void ispmci_(int* a)
{
/*
Polymarker colour index.
GKS
 CALL ISPMCI
 (ICOLI)
Action: This routine sets the polymarker colour index attribute for use by future invocations of IPM. The
routine IGSET (see section 4.11) can also be used with the parameter PMCI.
Parameter description:
ICOLI
 Polymarker colour index.
*/





}

void istxfp_(int *a, float *prec)
{
/*
GKS
 CALL ISTXFP
 (IFONT,IPREC)
Action: This routine sets the text font and precision attributes for use by future invocations of ITX. The
text font parameter selects among possible character shapes, as a roman font, a sans-serif font, etc. The
text precision parameter specifies how closely HIGZ (and also the underlying graphics package) must
follow the current size and orientation attributes. String precision is most liberal, stroke precision is most
strict. Character precision is in the middle. The routine IGSET (see section 4.11) can also be used with
the parameter TXFP.
Parameter description:
IFONT
IPREC
Text font. The value of IFONTText precision (0≤IPREC≤2).
depends on the underlying graphics package used.
*/
}

void ixsettc_(int*a)
{
/*
CALL IXSETTC
 (INDEX)
Action: Set colour index for text.
Parameter description:
INDEX
 Colour index defined my IXSETCOL.
*/

}


void ixsetlc_(int*a)
{




}



void ixsetmc_(int*a)
{




}








void ixsetfc_(int*a)
{




}




void ixsetln_(int*a)
{




}


void igsse_(int*a , int* b)
{


}

void igterm_(){}




}
