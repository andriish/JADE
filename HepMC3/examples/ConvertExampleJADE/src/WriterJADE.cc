#include "WriterJADE.h"
#include "HepMC/GenVertex.h"
#include "HepMC/GenParticle.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
extern "C"
{
 struct JADEEVT* loccprod_();
 struct JADENAMES* locchcprd_();
 int jadeco_(int& a);
}

std::vector<int> code(int i)
{
	int q=std::abs(i);
	std::vector<int> c(7);
	c[0]=q/10000000-0;
	c[1]=q/1000000-10*c[0];
	c[2]=q/100000-10*c[1];
	c[3]=q/10000-10*c[2];
	c[4]=q/1000-10*c[3];
	c[5]=q/100-10*c[4];
	c[6]=q/10-10*c[5];
	if (c[0]<0) c[0]=-c[0];
	return c;
}	

int charge (int i)
{
	int q=-2;

  
	switch (i){

	case 21: q=0; break;
	case 22: q=0; break;
	case 23: q=0; break;
	case 111: q=0; break;
	case 130: q=0; break;
	case 310: q=0; break;
	case 311: q=0; break;
	case 421: q=0; break;
	case 2112: q=0; break;
	case 3112: q=0; break;
	case 12: q=0; break;
	case 14: q=0; break;
	case 16: q=0; break;
	case -12: q=0; break;
	case -14: q=0; break;
	case -16: q=0; break;


	
	case 1:  q=-1; break;
	case -2:  q=-1; break;
	case 3:  q=-1; break;
	case -4:  q=-1; break;
	case 5:  q=-1; break;
	case 11:  q=-1; break;
	case 13:  q=-1; break;
	case 15:  q=-1; break;
	
	case -211:  q=-1; break;
	case -321:  q=-1; break;
	case -411:  q=-1; break;
	case -2212:  q=-1; break;
	case -431:  q=-1; break;
	

	
	case -1:  q=1; break;
	case 2:  q=1; break;
	case -3:  q=1; break;
	case 4:  q=1; break;
	case -5:  q=1; break;
	case -11:  q=1; break;
	case -13:  q=1; break;
	case -15:  q=1; break;

	case 211:  q=1; break;
	case 321:  q=1; break;
	case 411:  q=1; break;
	case 2212:  q=1; break;
	case 431:  q=1; break;
    default: break;
 
}
if (q!=-2) return q;

int QQ[7]={0,-1,2,-1,2,-1,2};
std::vector<int> r=code(i);

if (r[3]<6&&r[4]<6&&r[5]<6)
q=(int)((QQ[r[3]]+QQ[r[4]]+QQ[r[5]])/3);
if (i<0) q*=(-1);
printf("TRY CHARGE FOR  %i: %i\n",i,q);
return q;
}

namespace HepMC
{
WriterJADE::WriterJADE(const std::string &filename, int mode)
{
fUNIT=100;
fMODE=mode;/// BINARY OR TEXT
fJ=loccprod_();
fN=locchcprd_();
const char* f=filename.c_str();
int s=filename.length();
jfopen_(f,fUNIT,fMODE,s);
}
void WriterJADE::write_event(const GenEvent &evt)
{
	fJ->NEV=evt.event_number();
	fJ->BEAM=std::abs(evt.particles().at(0)->momentum().e());
	fJ->PT=0;
	fJ->PHI=0;     /* Calculated later */
	fJ->THETA=0;    /* Calculated later */
	fJ->IFLAVR=1;  /* FIXME  code of LO quark */
	
	fJ->NP=0;
	fJ->NF=0;
	
	fJ->NC=0;
	fJ->NN=0;
	
	
	fJ->NCF=0;
	fJ->NNF=0;
    char buf[10];

	int i=0,j=0,k;                            
	for (  k=0;(k<evt.particles().size())&&(i<500)&&(j<300);k++)
	{
	int q= charge(evt.particles().at(i)->pid());
	if (q==0) fJ->NN=fJ->NN+1; else fJ->NC=fJ->NC+1;
	int KF=evt.particles().at(i)->pid();
	    sprintf(buf,"%d",KF);
	sprintf(&(fN->CP[i][0]),"%.16s",                (std::string("PDGID=")+std::string(buf)+std::string("                ")).c_str());
	//sprintf(&(fN->CP[i][0]),"%.16s",    "E-              ");
	if (KF==11) sprintf(&(fN->CP[i][0]),"%.16s",    "E-              ");
	if (KF==-11) sprintf(&(fN->CP[i][0]),"%.16s",   "E+              ");
	if (KF==21) sprintf(&(fN->CP[i][0]),"%.16s",   "g               ");
	
	fJ->JCH[i]=q;
	fJ->JTP[i]=jadeco_(KF);
	fJ->PP[i][0]=evt.particles().at(i)->momentum().px();
	fJ->PP[i][1]=evt.particles().at(i)->momentum().py();
	fJ->PP[i][2]=evt.particles().at(i)->momentum().pz();
	fJ->PP[i][3]=evt.particles().at(i)->momentum().e();
	fJ->XM[i]=evt.particles().at(i)->momentum().m();
	if (fJ->XM[i]<0.000001) fJ->XM[i]=0.0;
	
		
	if (evt.particles().at(i)->status()!=1||(std::abs(KF)==12)||(std::abs(KF)==14)||(std::abs(KF)==16)) { i++; continue;}


	sprintf(&(fN->CF[j][0]),"%.16s",                (std::string("PDGID=")+std::string(buf)+std::string("                ")).c_str());
	//sprintf(&(fN->CF[j][0]),"%.16s",    "E-              ");
	if (KF==11) sprintf(&(fN->CF[j][0]),"%.16s",    "E-              ");
	if (KF==-11) sprintf(&(fN->CF[j][0]),"%.16s",   "E+              ");
   if (KF==21) sprintf(&(fN->CF[j][0]),"%.16s",   "g               ");

	fJ->ICF[j]=fJ->JCH[i];
	fJ->PF[j][0]=fJ->PP[i][0];
	fJ->PF[j][1]=fJ->PP[i][1];
	fJ->PF[j][2]=fJ->PP[i][2];
	fJ->PF[j][3]=fJ->PP[i][3];
	fJ->XMF[j]=fJ->XM[i];
	fJ->ICF[j]=q;
	fJ->ITF[j]=jadeco_(KF);;
	
	
	if (q==0) fJ->NCF=fJ->NCF+1; else fJ->NNF=fJ->NNF+1;
	
	GenVertexPtr VP=evt.particles().at(i)->production_vertex();
	GenVertexPtr VE=evt.particles().at(i)->end_vertex();
	
	int dmax=0;
	int pmin=evt.particles().size();
	if (VE)
	for (int y=0;y<evt.particles().size();y++)
	if (evt.particles().at(y)->production_vertex()==VE) dmax=std::max(dmax,y);
	

	if (VP)
	for (int y=0;y<evt.particles().size();y++)
	if (evt.particles().at(y)->end_vertex()==VP) pmin=std::min(pmin,y);

	fJ->JP[0][i]=pmin;
	fJ->JP[1][i]=dmax;

	if (VP)
	{
	fJ->PSTR[j][1]=VP->position().x();
	fJ->PSTR[j][2]=VP->position().y();
	fJ->PSTR[j][3]=VP->position().z();
	
    }
    else
    {
	fJ->PSTR[j][1]=0;
	fJ->PSTR[j][2]=0;
	fJ->PSTR[j][3]=0;
	}
	fJ->PT=fJ->PT+sqrt(fJ->PP[i][0]*fJ->PP[i][0]+fJ->PP[i][1]*fJ->PP[i][1]);
	i++;
	j++;
	}
	fJ->NP=i;
	fJ->NF=j;
	
	jfwrite_(fUNIT, fMODE);
}

bool WriterJADE::failed(){ return true; }

void WriterJADE::close()
{
jfclose_(fUNIT);
}
}// namespace HepMC
