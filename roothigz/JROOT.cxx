TCanvas*   J_GLOBAL_C;
TPad*   J_GLOBAL_P;


void IGSTRT()
{
//Init

}


//      CALL ISWN(NTS,XTMIN,XTMAX,YTMIN,YTMAX)
//      CALL ISVP(NTS,0.,1.,0.,1.)

void ISWN(int &NTS,float &XTMIN,float &XTMAX,float &YTMIN,float &YTMAX)
{
J_GLOBAL_C= new TCanvas();


}


void ISVP(int &NTS,float &XTMIN,float &XTMAX,float &YTMIN,float &YTMAX)
{

J_GLOBAL_C->cd();
J_GLOBAL_P= new TPad("AAA",XTMIN,XTMAX,YTMIN,YTMAX);

}
// CALL SETCOL('*INI')

void SETCOL(const char*)
{
	
	
	
}	



//CALL IGFIN
void IGFIN()
{
if (J_GLOBAL_C) delete 	J_GLOBAL_C;
if (J_GLOBAL_P) delete 	J_GLOBAL_P;
}	
//`iclrwk_'

void iclrwk_()
{
if (J_GLOBAL_P) J_GLOBAL_P->Clear();
}	


//`iclwk_'
void iclrwk_()
{
//terminates usage of ws
}


//`igbox_'

void igbox_(float& x1, float & x2,float& y1, float & y2)
{
TBox* B= new TBox(x1,y1,x2,y2);
B->SetFillColor();
B->SetFillStyle();
B->SetLineWidth();
B->SetLineColor();
B->Draw();
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
		TpolyLine* P=new TPolyLine(n,x,y);
	P->SetLineWidth();
	P->SetLineStyle();
	P->Draw();
	
}	


//`ipm_'
void ipm_(int&n, float* x, float* y)
{
		TPolyMarker* P=new TPolyMarker(n,x,y);
	P->SetLineWidth();
	P->SetLineStyle();
	P->Draw();
	
}	


//`ischh_'
void ischh_(float &h)
{
gStyle->SetFontSize(h);
 
} 
//`iscr_'
void iscr_(int &w, int &ci, float &r,float&g,float&b)
{
	
	
	
}	
//`iselnt_'
void iselnt_(int & t)
{
	
	
}	
/*

`isfaci_'
`isfais_'
`isln_'
`islwsc_'
`ismk_'
`ismksc_'
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

