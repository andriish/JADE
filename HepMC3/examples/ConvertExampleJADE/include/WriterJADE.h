#ifndef  HEPMC_WRITERJADE_H
#define  HEPMC_WRITERJADE_H
#include "HepMC/Writer.h"
#include "HepMC/GenEvent.h"
#include "HepMC/GenParticle.h"
#include "HepMC/Data/GenEventData.h"

struct JADEEVT
{
   int NEV;
   float BEAM;
   float  PT;
   float  PHI;
   float  THETA;
   int  IFLAVR;
   int  NP;
   int  NC;
   int  NN;
   float  PP[500][4];
   float  XM[500];
   int  JCH[500];
   int  JTP[500];
   int  JP[2][500];
   int  NF;
   int  NFC;
   int  NNF;
   float  PF[300][4];
   float  XMF[300];
   int  ICF[300];
   int  ITF[300];
   float PSTR[300][3];
};                               //!< Fortran common block HEPEVT

//      REAL PP,PF,XM,XMF,BEAM,PT,THETA,PHI,PSTRT
//      INTEGER NEV,NP,NC,NN,JCH,JTP,JP,NF,NCF,NNF,ICF,ITF,IFLAVR
//      CHARACTER*16 CP,CF

//      COMMON/CPROD/ NEV,BEAM,PT,PHI,THETA,IFLAVR,
//     *        NP,NC,NN,PP(4,500),XM(500),JCH(500),JTP(500),JP(500,2),
//     *        NF,NCF,NNF,PF(4,300),XMF(300),ICF(300),ITF(300),
//     *        PSTRT(3,300)



extern "C" 
{
extern struct JADEEVT*  cprod_;
void jfopen_(char * filename,int &u, int &size);
void jfclose_(int&);
void jfwrite_(int&, int&);
}	

namespace HepMC
{
class WriterJADE : public  Writer
{
public:
    int fUNIT;
    int fMODE;
//    WriterJADE(const std::string &filename);
};
}
#endif
