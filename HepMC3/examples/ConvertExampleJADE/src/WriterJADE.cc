#include "WriterJADE.h"
namespace HepMC
{
WriterJADE::WriterJADE(const std::string &filename)
{
fUNIT=100;
fMODE=0;
const char* f=filename.c_str();
int s=filename.length();
jfopen_(f,fUNIT,s);

}
void WriterJADE::write_event(const GenEvent &evt)
{
	cprod_->NP=evt.particles().size();
	cprod_->BEAM=std::abs(evt.particles().at(0)->momentum().e());
	cprod_->PT=0;
	cprod_->PHI=0;
	cprod_->THETA=0;
	cprod_->IFLAVR=0;
	cprod_->NP=0;
	cprod_->NC=0;
	cprod_->NN=0;
	
	for (int  i=0;i<evt.particles().size();i++)
	{
	
	}
	cprod_->NF=0;
	cprod_->NFC=0;
	cprod_->NNF=0;
	
	
	jfwrite_(fUNIT, fMODE);
}

bool WriterJADE::failed(){ return true; }

void WriterJADE::close()
{
jfclose_(fUNIT);
}
}// namespace HepMC


//struct JADEEVT
//{
   //int NEV;
   //float BEAM;
   //float  PT;
   //float  PHI;
   //float  THETA;
   //int  IFLAVR;
   //int  NP;
   //int  NC;
   //int  NN;
   //float  PP[500][4];
   //float  XM[500];
   //int  JCH[500];
   //int  JTP[500];
   //int  JP[2][500];
   //int  NF;
   //int  NFC;
   //int  NNF;
   //float  PF[300][4];
   //float  XMF[300];
   //int  ICF[300];
   //int  ITF[300];
   //float PSTR[300][3];
//};                               //!< Fortran common block HEPEVT

////      REAL PP,PF,XM,XMF,BEAM,PT,THETA,PHI,PSTRT
////      INTEGER NEV,NP,NC,NN,JCH,JTP,JP,NF,NCF,NNF,ICF,ITF,IFLAVR
////      CHARACTER*16 CP,CF

////      COMMON/CPROD/ NEV,BEAM,PT,PHI,THETA,IFLAVR,
////     *        NP,NC,NN,PP(4,500),XM(500),JCH(500),JTP(500),JP(500,2),
////     *        NF,NCF,NNF,PF(4,300),XMF(300),ICF(300),ITF(300),
////     *        PSTRT(3,300)

