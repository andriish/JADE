#ifndef  HEPMC3_WRITERJADE_H
#define  HEPMC3_WRITERJADE_H
#include "HepMC3/Writer.h"
#include "HepMC3/GenEvent.h"
#include "HepMC3/GenParticle.h"
#include "HepMC3/Data/GenEventData.h"

struct JADENAMES
{
    char CP[500][16];
    char CF[300][16];

}	;


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
    int  NCF;
    int  NNF;
    float  PF[300][4];
    float  XMF[300];
    int  ICF[300];
    int  ITF[300];
    float PSTR[300][3];
} ;                               //!< Fortran common block HEPEVT

//      REAL PP,PF,XM,XMF,BEAM,PT,THETA,PHI,PSTRT
//      INTEGER NEV,NP,NC,NN,JCH,JTP,JP,NF,NCF,NNF,ICF,ITF,IFLAVR
//      CHARACTER*16 CP,CF

//      COMMON/CPROD/ NEV,BEAM,PT,PHI,THETA,IFLAVR,
//     *        NP,NC,NN,PP(4,500),XM(500),JCH(500),JTP(500),JP(500,2),
//     *        NF,NCF,NNF,PF(4,300),XMF(300),ICF(300),ITF(300),
//     *        PSTRT(3,300)


extern "C"
{

    void jfopen_(const char * filename,int &u, int &O,int &size);
    void jfclose_(int&);
    void jfwrite_(int&, int&);
}

namespace HepMC3
{
class WriterJADE : public  Writer
{
public:
    struct JADEEVT*  fJ;
    struct JADENAMES*  fN;
    int fUNIT;
    int fMODE;
    WriterJADE(const std::string &filename, int mode);

    bool failed();
    void write_event(const GenEvent &evt);
    void close();

};
}
#endif
