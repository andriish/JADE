//////////////////////////////////////////////////////////
// This class has been automatically generated on
// Thu Oct 15 18:06:01 2015 by ROOT version 5.34/32
// from TChain h10/
//////////////////////////////////////////////////////////

#ifndef jadeanalysis_h
#define jadeanalysis_h

#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>
#include <TSelector.h>
#include <vector>
using namespace std;
// Header file for the classes stored in the TTree if any.
// Fixed size dimensions of array or collections stored in the TTree if any.
const Int_t kMaxparticles = 2000;
const Int_t kMaxvertices = 2000;

class jadeanalysis : public TSelector
{
public :
    TTree          *fChain;   ///<pointer to the analyzed TTree or TChain

    // Declaration of leaf types
    /*@{*/
    UShort_t        Irun;     ///< Run number.
    Int_t           Ievnt;    ///< Event number
    UChar_t         Itkmh;    ///< Tokio multihadron selection flag, see primer , flag ITKMH=1 indicates event is TKMH, =0 otherwise
    UChar_t         Igpmh;    ///< flag IGPMH=1 indicates event is GPMH, =0 otherwise
    UChar_t         Isist;
    UChar_t         Icvst;
    UChar_t         Icjst;    ///< Detector status, see primer, document me
    UChar_t         Iczst;
    UChar_t         Iebst;    ///< Detector status, see primer, document me
    UChar_t         Ieest;
    UChar_t         Istg1;    ///< flag for stage 1 selection, =1 selected, =0 rejected
    UShort_t        Ntkd02;
    Float_t         Ebeam;    ///< Beam energy
    Float_t         Pgce[4];  ///< 4-vector of hadron system from GCE CT+EC+HT
    Float_t         Tvectc[3];///< Thrust vector calculated from tracks and clusters, see \cite OPAL:2011aa
    UChar_t         Il2mh;    ///< flag IL2MH=1 indicates event is L2MH, =0 otherwise
    UChar_t         Iwqqqq;   ///< \f$WW \rightarrow qqqq\f$    log-likelihood selection, see \cite Abbiendi:2000eg
    Char_t          Iwqqln;   ///< \f$WW \rightarrow qql\nu\f$   log-likelihood selection, see \cite Abbiendi:2000eg
    Int_t           Idqqln;   ///< \f$WW \rightarrow qql\nu\f$   lepton id
    UChar_t         Iebal;    ///< flag for energy balance selection, =1 selected, =0 rejected
    UChar_t         Icat;
    UChar_t         Iminrmp;  ///< bit indicating whether a mini ramp is in progress
    Float_t         Pspri[4]; ///< 4-vector of hadron system from kin fit or ECAL cluster (WWSPRI)
    Float_t         Pspr[4];  ///< 4-vector of hadron system from kin fit or ECAL cluster (WWSPR)
    Float_t         Ephot;    ///< energy of most energetic isolated photon in ECAL
    Float_t         Efit;     ///< energy of hypothetical ISR photon returned by kin fit, sign        indicates direction +z or -z (WWSPRI)
    Float_t         Pfit;     ///< \f$\chi^2\f$ probability of kin fit (WWSPRI)
    Float_t         Efd;      ///< energy in forward detectors FD+SW+GC
    Float_t         W310;     ///< Some QCD matrix element weights for 2-4 jets from PXERT3
    Float_t         W420;     ///< Some QCD matrix element weights for 2-4 jets from PXERT3
    Float_t         Lwqqqq;   ///< Likelihood variable for WW all hadronic decay, see \cite Abbiendi:2000eg
    Float_t         Lwqqln;   ///< Likelihood variable for WW  leptonic/hadronic decay, see \cite Abbiendi:2000eg
    Float_t         Lwexef0;  ///< 4 fermion matrix element weight for all 4f diagrams
    Float_t         Lwexef1;  ///< 4 fermion matrix element weight for CC03 WW only
    Float_t         Tdtc;     ///< Thrust from tracks and clusters, see \cite OPAL:2011aa
    Float_t         Tmadtc;   ///< Thrust major from tracks and clusters, see \cite OPAL:2011aa
    Float_t         Tmidtc;   ///< Thrust minor from tracks and clusters, see \cite OPAL:2011aa
    Float_t         Mhdtc;    ///< Mass of heavy hemisphere from tracks and clusters, see \cite OPAL:2011aa
    Float_t         Mldtc;    ///< Mass of lite hemisphere from tracks and clusters, see \cite OPAL:2011aa
    Float_t         Btdtc;    ///< Total jet broadening from tracks and clusters, see \cite OPAL:2011aa
    Float_t         Bwdtc;    ///< Wide jet broadening from tracks and clusters, see \cite OPAL:2011aa
    Float_t         Cpdtc;    ///< C parameter from tracks and clusters, see \cite OPAL:2011aa
    Float_t         Dpdtc;    ///< D parameter from tracks and clusters, see \cite OPAL:2011aa
    Float_t         Sdtc;     ///< Sphericity from tracks and clusters, see \cite OPAL:2011aa
    Float_t         Adtc;     ///< Aplanarity from tracks and clusters, see \cite OPAL:2011aa
    Float_t         Acpdtc;   ///< Acoplanarity from tracks and clusters, see \cite OPAL:2011aa
    Float_t         Tdt;      ///< Thrust from tracks, see \cite OPAL:2011aa
    Float_t         Tmadt;    ///< Thrust major from tracks, see \cite OPAL:2011aa
    Float_t         Tmidt;    ///< Thrust minor from tracks , see \cite OPAL:2011aa
    Float_t         Mhdt;     ///< Mass of heavy hemisphere from tracks, see \cite OPAL:2011aa
    Float_t         Mldt;     ///< Mass of lite hemisphere from tracks, see \cite OPAL:2011aa
    Float_t         Btdt;     ///< Total jet broadening from tracks, see \cite OPAL:2011aa
    Float_t         Bwdt;     ///< Wide jet broadening from tracks, see \cite OPAL:2011aa
    Float_t         Cpdt;     ///< C parameter from tracks, see \cite OPAL:2011aa
    Float_t         Dpdt;     ///< D parameter from tracks, see \cite OPAL:2011aa
    Float_t         Sdt;      ///< Sphericisy parameter from tracks, see \cite OPAL:2011aa
    Float_t         Adt;      ///< Aplanarity parameter from tracks, see \cite OPAL:2011aa
    Float_t         Acpdt;    ///< Acoplanarity parameter from tracks, see \cite OPAL:2011aa
    Float_t         Tdc;      ///< Thrust minor from clusters, see \cite OPAL:2011aa
    Float_t         Tmadc;    ///< Thrust major from clusters, see \cite OPAL:2011aa
    Float_t         Tmidc;    ///< Thrust minor from clusters, see \cite OPAL:2011aa
    Float_t         Mhdc;     ///< Mass of heavy hemisphere from clusters, see \cite OPAL:2011aa
    Float_t         Mldc;     ///< Mass of lite hemisphere from clusters, see \cite OPAL:2011aa
    Float_t         Btdc;     ///< Total jet broadening from clusters, see \cite OPAL:2011aa
    Float_t         Bwdc;     ///< Wide jet broadening from clusters, see \cite OPAL:2011aa
    Float_t         Cpdc;     ///< C parameter from clusters, see \cite OPAL:2011aa
    Float_t         Dpdc;     ///< D parameter from clusters, see \cite OPAL:2011aa
    Float_t         Sdc;      ///< Sphericity parameter from clusters, see \cite OPAL:2011aa
    Float_t         Adc;      ///< Aplanarity parameter from clusters, see \cite OPAL:2011aa
    Float_t         Acpdc;    ///< Acoplanarity parameter from clusters, see \cite OPAL:2011aa
    Float_t         Tdmt;     ///< Thrust from enegry flow, see \cite OPAL:2011aa
    Float_t         Tmadmt;   ///< Thrust major  from enegry flow, see \cite OPAL:2011aa
    Float_t         Tmidmt;   ///< Thrust minor from enegry flow, see \cite OPAL:2011aa
    Float_t         Mhdmt;    ///< Mass of heavy hemisphere from energy flow, see \cite OPAL:2011aa
    Float_t         Mldmt;    ///< Mass of lite hemisphere from energy flow, see \cite OPAL:2011aa
    Float_t         Btdmt;    ///< Total jet broadining from  energy flow, see \cite OPAL:2011aa
    Float_t         Bwdmt;    ///< Wide jet broadining from  energy flow, see \cite OPAL:2011aa
    Float_t         Cpdmt;    ///< C parameter from  energy flow, see \cite OPAL:2011aa
    Float_t         Dpdmt;    ///< D parameter from  energy flow, see \cite OPAL:2011aa
    Float_t         Sdmt;     ///< Sphericity from  energy flow, see \cite OPAL:2011aa
    Float_t         Admt;     ///< Aplanarity from  energy flow, see \cite OPAL:2011aa
    Float_t         Acpdmt;   ///< Acoplanarity from  energy flow, see \cite OPAL:2011aa
    Int_t           Nxjdtc;   ///< Number of Durham jets from tracks and clusters
    Int_t           Nxjdt;    ///< Number of Durham jets from tracks
    Int_t           Nxjdc;    ///< Number of Durham jets from  clusters
    Int_t           Nxjdmt;   ///< Number of Durham jets from energy flow
    Int_t           Nxjetc;   ///< Number of JADE jets from tracks and clusters
    Int_t           Nxjet;    ///< Number of JADE jets from tracks
    Int_t           Nxjec;    ///< Number of JADE jets from  clusters
    Int_t           Nxjemt;   ///< Number of JADE jets from energy flow
    Int_t           Nxjctc;   ///< Number of CA jets from tracks and clusters
    Int_t           Nxjct;    ///< Number of CA jets from tracks
    Int_t           Nxjcc;    ///< Number of CA jets from  clusters
    Int_t           Nxjcmt;   ///< Number of CA jets from energy flow
    Float_t         Yddtc[31];        ///< Transition parameters for Durham jets from tracks and clusters, size [Nxjdtc],
    Float_t         Yedtc[31];        ///< Transition parameters for JADE jets from tracks and clusters, size [Nxjetc],
    Float_t         Ycdtc[31];        ///< Transition parameters for CA jets from tracks and clusters, size [Nxjctc],
    Char_t          Njcedtc[7];
    Char_t          Njcrdtc[7];
    Float_t         Yddt[31];         ///< Transition parameters for Durham jets from tracks and clusters, size [Nxjdt],
    Float_t         Yedt[31];         ///< Transition parameters for JADE jets from tracks and clusters, size [Nxjet],
    Float_t         Ycdt[31];         ///< Transition parameters for CA jets from tracks and clusters, size [Nxjct],
    Char_t          Njcedt[7];
    Char_t          Njcrdt[7];
    Float_t         Yddc[31];         ///< Transition parameters for Durham jets from  clusters, size [Nxjdc],
    Float_t         Yedc[31];         ///< Transition parameters for JADE jets from  clusters, size [Nxjdc]
    Float_t         Ycdc[31];         ///< Transition parameters for CA jets from  clusters, size [Nxjdc]
    Char_t          Njcedc[7];
    Char_t          Njcrdc[7];
    Float_t         Yddmt[31];         ///< Transition parameters for Durham jets from energy flow, size [Nxjdmt]
    Float_t         Yedmt[31];         ///< Transition parameters for JADE jets from energy flow, size [Nxjdmt]
    Float_t         Ycdmt[31];         ///< Transition parameters for CA jets from energy flow, size [Nxjdmt]
    Char_t          Njcedmt[7];
    Char_t          Njcrdmt[7];
    Int_t           Ntrk;          ///< Number of tracks
    UChar_t         Id02[501];     //[Ntrk]
    Float_t         Dedx[501];     ///< Energy losses for track, size [Ntrk]
    Float_t         Dded[501];     ///< Error on energy losses for track, size [Ntrk]
    UChar_t         Nhde[501];     ///< Number of hist used for energy losses for track, size [Ntrk]
    Float_t         Dp[501];       ///< Track peregee parameter, see primer, size [Ntrk]
    Float_t         Ptrk[501][3];  ///< Track momenta, size  [Ntrk]
    Char_t          Ichg[501];     ///< Track charge, size [Ntrk]
    UChar_t         Nhcj[501];     //[Ntrk]
    Float_t         Z0[501];       ///< Track peregee parameter Z0, see primer, size [Ntrk]
    Float_t         D0[501];       ///< Track peregee parameter Z0, see primer, size [Ntrk]
    Int_t           Nmttrk;
    UShort_t        Imttrk[501];   //[Nmttrk]
    Float_t         Mtscft[501];   //[Nmttrk]
    Int_t           Nclus;         ///< Number of clusters
    Int_t           Nmtcls;        ///< Number of energy flow objects
    UShort_t        Imtcls[501];   //[Nmtcls]
    Int_t           Nmtkil;
    UShort_t        Imtkil[501];   //[Nmtkil]
    Float_t         Pclus[1503][3];///< Cluster momenta, see primer, size [Nclus]
    Float_t         Mtscfc[501];   //[Nmtcls]
    UChar_t         Ioselbt;
    UChar_t         Levslbt;
    Int_t           Nvtxbt;
    UChar_t         Ivmulbt[30];   //[Nvtxbt]
    UChar_t         Nvsigbt[30];   //[Nvtxbt]
    Float_t         Thrvecbt[3];
    Float_t         Prvtxbt[3];
    Float_t         Vnnbt[30];   //[Nvtxbt]
    Float_t         Vchi2bt[30];   //[Nvtxbt]
    Float_t         Vtxbt[30][3];   //[Nvtxbt]
    Float_t         Pvtxbt[30][5];   //[Nvtxbt]
    Float_t         Vdlen3bt[30];   //[Nvtxbt]
    Float_t         Vderr3bt[30];   //[Nvtxbt]
    UChar_t         Ievtyp;          ///< event type returned by WWTYPE, see WW primer pg. 8
    UChar_t         Inonr;
    Float_t         Pisr[4];
    Int_t           Nprimf;
    Char_t          Iferid[4];   //[Nprimf]
    Float_t         Primf[4][4];   //[Nprimf]
    Float_t         Tp;         ///< Thrust at parton level, MC trees only, see \cite OPAL:2011aa
    Float_t         Tmap;       ///< Thrust major at parton level, MC trees only, see \cite OPAL:2011aa
    Float_t         Tmip;       ///< Thrust minor at parton level, MC trees only, see \cite OPAL:2011aa
    Float_t         Mhp;        ///< Mass of heavy hemisphere at parton level, MC trees only, see \cite OPAL:2011aa
    Float_t         Mlp;        ///< Mass of lite hemisphere at parton level, MC trees only, see \cite OPAL:2011aa
    Float_t         Btp;        ///< Total broadening at parton level, MC trees only, see \cite OPAL:2011aa
    Float_t         Bwp;        ///< Wide  broadeningat parton level, MC trees only, see \cite OPAL:2011aa
    Float_t         Cpp;        ///< C parameter at parton level, MC trees only, see \cite OPAL:2011aa
    Float_t         Dpp;        ///< D parameter at parton level, MC trees only, see \cite OPAL:2011aa
    Float_t         Sp;         ///< Sphericityat parton level, MC trees only, see \cite OPAL:2011aa
    Float_t         Ap;         ///< Aplanarity at parton level, MC trees only, see \cite OPAL:2011aa
    Float_t         Acpp;       ///< Acoplanarity  at parton level, MC trees only, see \cite OPAL:2011aa
    Float_t         Th;         ///< Thrust at hadron level, MC trees only, see \cite OPAL:2011aa
    Float_t         Tmah;       ///< Thrust major at hadron level, MC trees only, see \cite OPAL:2011aa
    Float_t         Tmih;       ///< Thrust manor at hadron level, MC trees only, see \cite OPAL:2011aa
    Float_t         Mhh;        ///< Mass of heavy hemisphere at hadron level, MC trees only, see \cite OPAL:2011aa
    Float_t         Mlh;        ///< Mass of lite hemisphere at hadron level, MC trees only, see \cite OPAL:2011aa
    Float_t         Bth;        ///< Total broadening at hadron level, MC trees only, see \cite OPAL:2011aa
    Float_t         Bwh;        ///< Wide jet broadening at hadron level, MC trees only, see \cite OPAL:2011aa
    Float_t         Cph;        ///< C parameter at parton level, MC trees only, see \cite OPAL:2011aa
    Float_t         Dph;        ///< D parameter at hadron level, MC trees only, see \cite OPAL:2011aa
    Float_t         Sh;         ///< Sphericity at hadron level, MC trees only, see \cite OPAL:2011aa
    Float_t         Ah;         ///< Aplanarity at hadron level, MC trees only, see \cite OPAL:2011aa
    Float_t         Acph;       ///< Acoplanarity at hadron level, MC trees only, see \cite OPAL:2011aa
    Int_t           Nxjdp;      ///< Number of jets at parton level with Durham algorithm, MC trees only
    Int_t           Nxjdh;      ///< Number of jets at parton level with Durham algorithm, MC trees only
    Int_t           Nxjep;      ///< Number of jets at parton level with JADE algorithm, MC trees only
    Int_t           Nxjeh;      ///< Number of jets at parton level with JADE algorithm, MC trees only
    Int_t           Nxjcp;      ///< Number of jets at parton level with CA algorithm, MC trees only
    Int_t           Nxjch;      ///< Number of jets at parton level with CA algorithm, MC trees only
    Float_t         Ydp[31];   ///< Transition parameters for Durham jets at parton level, size [Nxjdp], MC trees only
    Float_t         Yep[31];   ///< Transition parameters for JADE jets at parton level, size [Nxjep], MC trees only
    Float_t         Ycp[31];   ///< Transition parameters for CA jets at parton level, size [Nxjcp], MC trees only
    Char_t          Njcep[7];
    Char_t          Njcrp[7];
    Float_t         Ydh[31];   ///< Transition parameters for Durham jets at particle level, size [Nxjdh], MC trees only
    Float_t         Yeh[31];   ///< Transition parameters for JADE jets at particle level, size [Nxjeh], MC trees only
    Float_t         Ych[31];   ///< Transition parameters for CA jets at particle level, size [Nxjch], MC trees only
    Char_t          Njceh[7];
    Char_t          Njcrh[7];
    Int_t           Ntrkp;       ///<Number of partons,  MC trees only
    Char_t          Ilucp[50];   //[Ntrkp]
    Float_t         Ptrkp[50][4];   ///<4-Momenta of partons, size [Ntrkp], MC trees only
    Int_t           Ntrkh;            ///<Number of final hadrons,  MC trees only
    Int_t           Ntrk2;
    Float_t         Ptrkh[2004][4];   ///<4-Momenta of final hadrons, size [Ntrkh], MC trees only
    Int_t           Iluch[2004];   //[Ntrkh]
    Int_t           Iluc[501];   ///< MC ID code of particle associated with track, size [Ntrk2], MC trees only
    UChar_t         Istrt[501];   //[Ntrk2]
    Char_t          Ichgh[2004];   //[Ntrkh]
    /*@}*/
    
    //JADE
   UInt_t          Ino;
   UInt_t          Inttr;
   UInt_t          Iltr;
   UInt_t          Itauct;
   UInt_t          Imcred;
   UInt_t          Njdcut[2];
   Float_t         Eb;
   Float_t         Ee[2];
   Float_t         Zvert;    
   //JADE

    Int_t           event_number;                            ///< Event number in generated MC, HepMC trees only
    Int_t           momentum_unit;                           ///< HepMC units GEV/MEV, HepMC trees only
    Int_t           length_unit;                             ///< HepMC units cm/mm, HepMC trees only
    Int_t           particles_;                              ///< Number of HepMC particles, HepMC trees only
    Int_t           particles_pid[kMaxparticles];            ///< HepMC PDG id, size [particles_], HepMC trees only
    Int_t           particles_status[kMaxparticles];         ///< HepMC particles status, size [particles_], HepMC trees only
    Bool_t          particles_is_mass_set[kMaxparticles];    ///< Some flag, see HepMC docs, size   [particles_], HepMC trees only
    Double_t        particles_mass[kMaxparticles];           ///< HepMC particles mass, size[particles_], HepMC trees only
    Double_t        particles_momentum_m_v1[kMaxparticles];  ///< HepMC particles \f$ p_x \f$, size [particles_], HepMC trees only
    Double_t        particles_momentum_m_v2[kMaxparticles];  ///< HepMC particles \f$ p_y \f$, size [particles_], HepMC trees only
    Double_t        particles_momentum_m_v3[kMaxparticles];  ///< HepMC particles \f$ p_z \f$, size [particles_], HepMC trees only
    Double_t        particles_momentum_m_v4[kMaxparticles];  ///< HepMC particles \f$ e   \f$, size [particles_], HepMC trees only
    Int_t           vertices_;                               ///< Number of HepMC vertices, HepMC trees only
    Double_t        vertices_position_m_v1[kMaxvertices];    ///< HepMC vertex \f$x\f$, size [vertices_], HepMC trees only
    Double_t        vertices_position_m_v2[kMaxvertices];    ///< HepMC vertex \f$y\f$, size [vertices_], HepMC trees only
    Double_t        vertices_position_m_v3[kMaxvertices];    ///< HepMC vertex \f$z\f$, size [vertices_], HepMC trees only
    Double_t        vertices_position_m_v4[kMaxvertices];    ///< HepMC vertex \f$t\f$, size [vertices_], HepMC trees only
    vector<int>     links1;                                  ///< Relations between particles and vertices, see HepMC documentation, HepMC trees only
    vector<int>     links2;                                  ///< Relations between particles and vertices, see HepMC documentation, HepMC trees only
    vector<int>     attribute_id;                            ///< HepMC event attribute id, see HepMC documentation, HepMC trees only
    vector<string>  attribute_name;                          ///< HepMC event attribute name , see HepMC documentation, HepMC trees only
    vector<string>  attribute_string;                        ///< HepMC event attribute string, see HepMC documentation, HepMC trees only
    vector<double>  weights;

#ifndef DOXYGEN_SHOULD_SKIP_THIS
    // List of branches
    TBranch        *b_hepmc3_event_event_number;   //!
    TBranch        *b_hepmc3_event_momentum_unit;   //!
    TBranch        *b_hepmc3_event_length_unit;   //!
    TBranch        *b_hepmc3_event_particles_;   //!
    TBranch        *b_particles_pid;   //!
    TBranch        *b_particles_status;   //!
    TBranch        *b_particles_is_mass_set;   //!
    TBranch        *b_particles_mass;   //!
    TBranch        *b_particles_momentum_m_v1;   //!
    TBranch        *b_particles_momentum_m_v2;   //!
    TBranch        *b_particles_momentum_m_v3;   //!
    TBranch        *b_particles_momentum_m_v4;   //!
    TBranch        *b_hepmc3_event_vertices_;   //!
    TBranch        *b_vertices_position_m_v1;   //!
    TBranch        *b_vertices_position_m_v2;   //!
    TBranch        *b_vertices_position_m_v3;   //!
    TBranch        *b_vertices_position_m_v4;   //!
    TBranch        *b_hepmc3_event_links1;   //!
    TBranch        *b_hepmc3_event_links2;   //!
    TBranch        *b_hepmc3_event_attribute_id;   //!
    TBranch        *b_hepmc3_event_attribute_name;   //!
    TBranch        *b_hepmc3_event_attribute_string;   //!
    TBranch        *b_hepmc3_event_weights;   //!

    // List of branches
    TBranch        *b_Irun;   //!
    TBranch        *b_Ievnt;   //!
    TBranch        *b_Itkmh;   //!
    TBranch        *b_Igpmh;   //!
    TBranch        *b_Isist;   //!
    TBranch        *b_Icvst;   //!
    TBranch        *b_Icjst;   //!
    TBranch        *b_Iczst;   //!
    TBranch        *b_Iebst;   //!
    TBranch        *b_Ieest;   //!
    TBranch        *b_Istg1;   //!
    TBranch        *b_Ntkd02;   //!
    TBranch        *b_Ebeam;   //!
    TBranch        *b_Pgce;   //!
    TBranch        *b_Tvectc;   //!
    TBranch        *b_Il2mh;   //!
    TBranch        *b_Iwqqqq;   //!
    TBranch        *b_Iwqqln;   //!
    TBranch        *b_Idqqln;   //!
    TBranch        *b_Iebal;   //!
    TBranch        *b_Icat;   //!
    TBranch        *b_Iminrmp;   //!
    TBranch        *b_Pspri;   //!
    TBranch        *b_Pspr;   //!
    TBranch        *b_Ephot;   //!
    TBranch        *b_Efit;   //!
    TBranch        *b_Pfit;   //!
    TBranch        *b_Efd;   //!
    TBranch        *b_W310;   //!
    TBranch        *b_W420;   //!
    TBranch        *b_Lwqqqq;   //!
    TBranch        *b_Lwqqln;   //!
    TBranch        *b_Lwexef0;   //!
    TBranch        *b_Lwexef1;   //!
    TBranch        *b_Tdtc;   //!
    TBranch        *b_Tmadtc;   //!
    TBranch        *b_Tmidtc;   //!
    TBranch        *b_Mhdtc;   //!
    TBranch        *b_Mldtc;   //!
    TBranch        *b_Btdtc;   //!
    TBranch        *b_Bwdtc;   //!
    TBranch        *b_Cpdtc;   //!
    TBranch        *b_Dpdtc;   //!
    TBranch        *b_Sdtc;   //!
    TBranch        *b_Adtc;   //!
    TBranch        *b_Acpdtc;   //!
    TBranch        *b_Tdt;   //!
    TBranch        *b_Tmadt;   //!
    TBranch        *b_Tmidt;   //!
    TBranch        *b_Mhdt;   //!
    TBranch        *b_Mldt;   //!
    TBranch        *b_Btdt;   //!
    TBranch        *b_Bwdt;   //!
    TBranch        *b_Cpdt;   //!
    TBranch        *b_Dpdt;   //!
    TBranch        *b_Sdt;   //!
    TBranch        *b_Adt;   //!
    TBranch        *b_Acpdt;   //!
    TBranch        *b_Tdc;   //!
    TBranch        *b_Tmadc;   //!
    TBranch        *b_Tmidc;   //!
    TBranch        *b_Mhdc;   //!
    TBranch        *b_Mldc;   //!
    TBranch        *b_Btdc;   //!
    TBranch        *b_Bwdc;   //!
    TBranch        *b_Cpdc;   //!
    TBranch        *b_Dpdc;   //!
    TBranch        *b_Sdc;   //!
    TBranch        *b_Adc;   //!
    TBranch        *b_Acpdc;   //!
    TBranch        *b_Tdmt;   //!
    TBranch        *b_Tmadmt;   //!
    TBranch        *b_Tmidmt;   //!
    TBranch        *b_Mhdmt;   //!
    TBranch        *b_Mldmt;   //!
    TBranch        *b_Btdmt;   //!
    TBranch        *b_Bwdmt;   //!
    TBranch        *b_Cpdmt;   //!
    TBranch        *b_Dpdmt;   //!
    TBranch        *b_Sdmt;   //!
    TBranch        *b_Admt;   //!
    TBranch        *b_Acpdmt;   //!
    TBranch        *b_Nxjdtc;   //!
    TBranch        *b_Nxjdt;   //!
    TBranch        *b_Nxjdc;   //!
    TBranch        *b_Nxjdmt;   //!
    TBranch        *b_Nxjetc;   //!
    TBranch        *b_Nxjet;   //!
    TBranch        *b_Nxjec;   //!
    TBranch        *b_Nxjemt;   //!
    TBranch        *b_Nxjctc;   //!
    TBranch        *b_Nxjct;   //!
    TBranch        *b_Nxjcc;   //!
    TBranch        *b_Nxjcmt;   //!
    TBranch        *b_Yddtc;   //!
    TBranch        *b_Yedtc;   //!
    TBranch        *b_Ycdtc;   //!
    TBranch        *b_Njcedtc;   //!
    TBranch        *b_Njcrdtc;   //!
    TBranch        *b_Yddt;   //!
    TBranch        *b_Yedt;   //!
    TBranch        *b_Ycdt;   //!
    TBranch        *b_Njcedt;   //!
    TBranch        *b_Njcrdt;   //!
    TBranch        *b_Yddc;   //!
    TBranch        *b_Yedc;   //!
    TBranch        *b_Ycdc;   //!
    TBranch        *b_Njcedc;   //!
    TBranch        *b_Njcrdc;   //!
    TBranch        *b_Yddmt;   //!
    TBranch        *b_Yedmt;   //!
    TBranch        *b_Ycdmt;   //!
    TBranch        *b_Njcedmt;   //!
    TBranch        *b_Njcrdmt;   //!
    TBranch        *b_Ntrk;   //!
    TBranch        *b_Id02;   //!
    TBranch        *b_Dedx;   //!
    TBranch        *b_Dded;   //!
    TBranch        *b_Nhde;   //!
    TBranch        *b_Dp;   //!
    TBranch        *b_Ptrk;   //!
    TBranch        *b_Ichg;   //!
    TBranch        *b_Nhcj;   //!
    TBranch        *b_Z0;   //!
    TBranch        *b_D0;   //!
    TBranch        *b_Nmttrk;   //!
    TBranch        *b_Imttrk;   //!
    TBranch        *b_Mtscft;   //!
    TBranch        *b_Nclus;   //!
    TBranch        *b_Nmtcls;   //!
    TBranch        *b_Imtcls;   //!
    TBranch        *b_Nmtkil;   //!
    TBranch        *b_Imtkil;   //!
    TBranch        *b_Pclus;   //!
    TBranch        *b_Mtscfc;   //!
    TBranch        *b_Ioselbt;   //!
    TBranch        *b_Levslbt;   //!
    TBranch        *b_Nvtxbt;   //!
    TBranch        *b_Ivmulbt;   //!
    TBranch        *b_Nvsigbt;   //!
    TBranch        *b_Thrvecbt;   //!
    TBranch        *b_Prvtxbt;   //!
    TBranch        *b_Vnnbt;   //!
    TBranch        *b_Vchi2bt;   //!
    TBranch        *b_Vtxbt;   //!
    TBranch        *b_Pvtxbt;   //!
    TBranch        *b_Vdlen3bt;   //!
    TBranch        *b_Vderr3bt;   //!
    TBranch        *b_Ievtyp;   //!
//JADE->
   TBranch        *b_Ino;   //!
   TBranch        *b_Inttr;   //!
   TBranch        *b_Iltr;   //!
   TBranch        *b_Itauct;   //!
   TBranch        *b_Imcred;   //!
   TBranch        *b_Njdcut;   //!
   TBranch        *b_Eb;   //!
   TBranch        *b_Ee;   //!
   TBranch        *b_Zvert;   //!
///<-JADE
    TBranch        *b_Inonr;   //!
    TBranch        *b_Pisr;   //!
    TBranch        *b_Nprimf;   //!
    TBranch        *b_Iferid;   //!
    TBranch        *b_Primf;   //!
    TBranch        *b_Tp;   //!
    TBranch        *b_Tmap;   //!
    TBranch        *b_Tmip;   //!
    TBranch        *b_Mhp;   //!
    TBranch        *b_Mlp;   //!
    TBranch        *b_Btp;   //!
    TBranch        *b_Bwp;   //!
    TBranch        *b_Cpp;   //!
    TBranch        *b_Dpp;   //!
    TBranch        *b_Sp;   //!
    TBranch        *b_Ap;   //!
    TBranch        *b_Acpp;   //!
    TBranch        *b_Th;   //!
    TBranch        *b_Tmah;   //!
    TBranch        *b_Tmih;   //!
    TBranch        *b_Mhh;   //!
    TBranch        *b_Mlh;   //!
    TBranch        *b_Bth;   //!
    TBranch        *b_Bwh;   //!
    TBranch        *b_Cph;   //!
    TBranch        *b_Dph;   //!
    TBranch        *b_Sh;   //!
    TBranch        *b_Ah;   //!
    TBranch        *b_Acph;   //!
    TBranch        *b_Nxjdp;   //!
    TBranch        *b_Nxjdh;   //!
    TBranch        *b_Nxjep;   //!
    TBranch        *b_Nxjeh;   //!
    TBranch        *b_Nxjcp;   //!
    TBranch        *b_Nxjch;   //!
    TBranch        *b_Ydp;   //!
    TBranch        *b_Yep;   //!
    TBranch        *b_Ycp;   //!
    TBranch        *b_Njcep;   //!
    TBranch        *b_Njcrp;   //!
    TBranch        *b_Ydh;   //!
    TBranch        *b_Yeh;   //!
    TBranch        *b_Ych;   //!
    TBranch        *b_Njceh;   //!
    TBranch        *b_Njcrh;   //!
    TBranch        *b_Ntrkp;   //!
    TBranch        *b_Ilucp;   //!
    TBranch        *b_Ptrkp;   //!
    TBranch        *b_Ntrkh;   //!
    TBranch        *b_Ntrk2;   //!
    TBranch        *b_Ptrkh;   //!
    TBranch        *b_Iluch;   //!
    TBranch        *b_Iluc;   //!
    TBranch        *b_Istrt;   //!
    TBranch        *b_Ichgh;   //!
#endif
    jadeanalysis(TTree * /*tree*/ =0) : fChain(0)
    {
        b_hepmc3_event_event_number=0;
        b_hepmc3_event_momentum_unit=0;
        b_hepmc3_event_length_unit=0;
        b_hepmc3_event_particles_=0;
        b_particles_pid=0;
        b_particles_status=0;
        b_particles_is_mass_set=0;
        b_particles_mass=0;
        b_particles_momentum_m_v1=0;
        b_particles_momentum_m_v2=0;
        b_particles_momentum_m_v3=0;
        b_particles_momentum_m_v4=0;
        b_hepmc3_event_vertices_=0;
        b_vertices_position_m_v1=0;
        b_vertices_position_m_v2=0;
        b_vertices_position_m_v3=0;
        b_vertices_position_m_v4=0;
        b_hepmc3_event_links1=0;
        b_hepmc3_event_links2=0;
        b_hepmc3_event_attribute_id=0;
        b_hepmc3_event_attribute_name=0;
        b_hepmc3_event_attribute_string=0;
        b_hepmc3_event_weights=0;
        b_Irun=0;
        b_Ievnt=0;
        b_Itkmh=0;
        b_Igpmh=0;
        b_Isist=0;
        b_Icvst=0;
        b_Icjst=0;
        b_Iczst=0;
        b_Iebst=0;
        b_Ieest=0;
        b_Istg1=0;
        b_Ntkd02=0;
        b_Ebeam=0;
        b_Pgce=0;
        b_Tvectc=0;
        b_Il2mh=0;
        b_Iwqqqq=0;
        b_Iwqqln=0;
        b_Idqqln=0;
        b_Iebal=0;
        b_Icat=0;
        b_Iminrmp=0;
        b_Pspri=0;
        b_Pspr=0;
        b_Ephot=0;
        b_Efit=0;
        b_Pfit=0;
        b_Efd=0;
        b_W310=0;
        b_W420=0;
        b_Lwqqqq=0;
        b_Lwqqln=0;
        b_Lwexef0=0;
        b_Lwexef1=0;
        b_Tdtc=0;
        b_Tmadtc=0;
        b_Tmidtc=0;
        b_Mhdtc=0;
        b_Mldtc=0;
        b_Btdtc=0;
        b_Bwdtc=0;
        b_Cpdtc=0;
        b_Dpdtc=0;
        b_Sdtc=0;
        b_Adtc=0;
        b_Acpdtc=0;
        b_Tdt=0;
        b_Tmadt=0;
        b_Tmidt=0;
        b_Mhdt=0;
        b_Mldt=0;
        b_Btdt=0;
        b_Bwdt=0;
        b_Cpdt=0;
        b_Dpdt=0;
        b_Sdt=0;
        b_Adt=0;
        b_Acpdt=0;
        b_Tdc=0;
        b_Tmadc=0;
        b_Tmidc=0;
        b_Mhdc=0;
        b_Mldc=0;
        b_Btdc=0;
        b_Bwdc=0;
        b_Cpdc=0;
        b_Dpdc=0;
        b_Sdc=0;
        b_Adc=0;
        b_Acpdc=0;
        b_Tdmt=0;
        b_Tmadmt=0;
        b_Tmidmt=0;
        b_Mhdmt=0;
        b_Mldmt=0;
        b_Btdmt=0;
        b_Bwdmt=0;
        b_Cpdmt=0;
        b_Dpdmt=0;
        b_Sdmt=0;
        b_Admt=0;
        b_Acpdmt=0;
        b_Nxjdtc=0;
        b_Nxjdt=0;
        b_Nxjdc=0;
        b_Nxjdmt=0;
        b_Nxjetc=0;
        b_Nxjet=0;
        b_Nxjec=0;
        b_Nxjemt=0;
        b_Nxjctc=0;
        b_Nxjct=0;
        b_Nxjcc=0;
        b_Nxjcmt=0;
        b_Yddtc=0;
        b_Yedtc=0;
        b_Ycdtc=0;
        b_Njcedtc=0;
        b_Njcrdtc=0;
        b_Yddt=0;
        b_Yedt=0;
        b_Ycdt=0;
        b_Njcedt=0;
        b_Njcrdt=0;
        b_Yddc=0;
        b_Yedc=0;
        b_Ycdc=0;
        b_Njcedc=0;
        b_Njcrdc=0;
        b_Yddmt=0;
        b_Yedmt=0;
        b_Ycdmt=0;
        b_Njcedmt=0;
        b_Njcrdmt=0;
        b_Ntrk=0;
        b_Id02=0;
        b_Dedx=0;
        b_Dded=0;
        b_Nhde=0;
        b_Dp=0;
        b_Ptrk=0;
        b_Ichg=0;
        b_Nhcj=0;
        b_Z0=0;
        b_D0=0;
        b_Nmttrk=0;
        b_Imttrk=0;
        b_Mtscft=0;
        b_Nclus=0;
        b_Nmtcls=0;
        b_Imtcls=0;
        b_Nmtkil=0;
        b_Imtkil=0;
        b_Pclus=0;
        b_Mtscfc=0;
        b_Ioselbt=0;
        b_Levslbt=0;
        b_Nvtxbt=0;
        b_Ivmulbt=0;
        b_Nvsigbt=0;
        b_Thrvecbt=0;
        b_Prvtxbt=0;
        b_Vnnbt=0;
        b_Vchi2bt=0;
        b_Vtxbt=0;
        b_Pvtxbt=0;
        b_Vdlen3bt=0;
        b_Vderr3bt=0;
        b_Ievtyp=0;
        b_Inonr=0;
        b_Pisr=0;
        b_Nprimf=0;
        b_Iferid=0;
        b_Primf=0;
        b_Tp=0;
        b_Tmap=0;
        b_Tmip=0;
        b_Mhp=0;
        b_Mlp=0;
        b_Btp=0;
        b_Bwp=0;
        b_Cpp=0;
        b_Dpp=0;
        b_Sp=0;
        b_Ap=0;
        b_Acpp=0;
        b_Th=0;
        b_Tmah=0;
        b_Tmih=0;
        b_Mhh=0;
        b_Mlh=0;
        b_Bth=0;
        b_Bwh=0;
        b_Cph=0;
        b_Dph=0;
        b_Sh=0;
        b_Ah=0;
        b_Acph=0;
        b_Nxjdp=0;
        b_Nxjdh=0;
        b_Nxjep=0;
        b_Nxjeh=0;
        b_Nxjcp=0;
        b_Nxjch=0;
        b_Ydp=0;
        b_Yep=0;
        b_Ycp=0;
        b_Njcep=0;
        b_Njcrp=0;
        b_Ydh=0;
        b_Yeh=0;
        b_Ych=0;
        b_Njceh=0;
        b_Njcrh=0;
        b_Ntrkp=0;
        b_Ilucp=0;
        b_Ptrkp=0;
        b_Ntrkh=0;
        b_Ntrk2=0;
        b_Ptrkh=0;
        b_Iluch=0;
        b_Iluc=0;
        b_Istrt=0;
        b_Ichgh=0;
    }
    virtual ~jadeanalysis() { }
    virtual Int_t   Version() const { return 2; }
    virtual void    Begin(TTree *tree);
    virtual void    SlaveBegin(TTree *tree);
    virtual void    Init(TTree *tree);
    virtual Bool_t  Notify();
    virtual Bool_t  Process(Long64_t entry);
    virtual Int_t   GetEntry(Long64_t entry, Int_t getall = 0) { return fChain ? fChain->GetTree()->GetEntry(entry, getall) : 0; }
    virtual void    SetOption(const char *option) { fOption = option; }
    virtual void    SetObject(TObject *obj) { fObject = obj; }
    virtual void    SetInputList(TList *input) { fInput = input; }
    virtual TList  *GetOutputList() const { return fOutput; }
    virtual void    SlaveTerminate();
    virtual void    Terminate();

    //ClassDef(jadeanalysis,0);
};

#endif

#ifdef jadeanalysis_cxx
void jadeanalysis::Init(TTree *tree)
{
    // The Init() function is called when the selector needs to initialize
    // a new tree or chain. Typically here the branch addresses and branch
    // pointers of the tree will be set.
    // It is normally not necessary to make changes to the generated
    // code, but the routine can be extended by the user if needed.
    // Init() will be called many times when running on PROOF
    // (once per file to be processed).

    // Set branch addresses and branch pointers
    if (!tree) return;
    fChain = tree;
    fChain->SetMakeClass(1);

   fChain->SetBranchAddress("Irun", &Irun, &b_Irun);
   fChain->SetBranchAddress("Ievnt", &Ievnt, &b_Ievnt);
   fChain->SetBranchAddress("Itkmh", &Itkmh, &b_Itkmh);
   fChain->SetBranchAddress("Igpmh", &Igpmh, &b_Igpmh);
   fChain->SetBranchAddress("Isist", &Isist, &b_Isist);
   fChain->SetBranchAddress("Icvst", &Icvst, &b_Icvst);
   fChain->SetBranchAddress("Icjst", &Icjst, &b_Icjst);
   fChain->SetBranchAddress("Iczst", &Iczst, &b_Iczst);
   fChain->SetBranchAddress("Iebst", &Iebst, &b_Iebst);
   fChain->SetBranchAddress("Ieest", &Ieest, &b_Ieest);
   fChain->SetBranchAddress("Istg1", &Istg1, &b_Istg1);
   fChain->SetBranchAddress("Ntkd02", &Ntkd02, &b_Ntkd02);
   fChain->SetBranchAddress("Ebeam", &Ebeam, &b_Ebeam);
   fChain->SetBranchAddress("Pgce", Pgce, &b_Pgce);
   fChain->SetBranchAddress("Tvectc", Tvectc, &b_Tvectc);
   fChain->SetBranchAddress("Ino", &Ino, &b_Ino);
   fChain->SetBranchAddress("Inttr", &Inttr, &b_Inttr);
   fChain->SetBranchAddress("Iltr", &Iltr, &b_Iltr);
   fChain->SetBranchAddress("Itauct", &Itauct, &b_Itauct);
   fChain->SetBranchAddress("Imcred", &Imcred, &b_Imcred);
   fChain->SetBranchAddress("Njdcut", Njdcut, &b_Njdcut);
   fChain->SetBranchAddress("Eb", &Eb, &b_Eb);
   fChain->SetBranchAddress("Ee", Ee, &b_Ee);
   fChain->SetBranchAddress("Zvert", &Zvert, &b_Zvert);
   fChain->SetBranchAddress("Tdtc", &Tdtc, &b_Tdtc);
   fChain->SetBranchAddress("Tmadtc", &Tmadtc, &b_Tmadtc);
   fChain->SetBranchAddress("Tmidtc", &Tmidtc, &b_Tmidtc);
   fChain->SetBranchAddress("Mhdtc", &Mhdtc, &b_Mhdtc);
   fChain->SetBranchAddress("Mldtc", &Mldtc, &b_Mldtc);
   fChain->SetBranchAddress("Btdtc", &Btdtc, &b_Btdtc);
   fChain->SetBranchAddress("Bwdtc", &Bwdtc, &b_Bwdtc);
   fChain->SetBranchAddress("Cpdtc", &Cpdtc, &b_Cpdtc);
   fChain->SetBranchAddress("Dpdtc", &Dpdtc, &b_Dpdtc);
   fChain->SetBranchAddress("Sdtc", &Sdtc, &b_Sdtc);
   fChain->SetBranchAddress("Adtc", &Adtc, &b_Adtc);
   fChain->SetBranchAddress("Acpdtc", &Acpdtc, &b_Acpdtc);
   fChain->SetBranchAddress("Tdt", &Tdt, &b_Tdt);
   fChain->SetBranchAddress("Tmadt", &Tmadt, &b_Tmadt);
   fChain->SetBranchAddress("Tmidt", &Tmidt, &b_Tmidt);
   fChain->SetBranchAddress("Mhdt", &Mhdt, &b_Mhdt);
   fChain->SetBranchAddress("Mldt", &Mldt, &b_Mldt);
   fChain->SetBranchAddress("Btdt", &Btdt, &b_Btdt);
   fChain->SetBranchAddress("Bwdt", &Bwdt, &b_Bwdt);
   fChain->SetBranchAddress("Cpdt", &Cpdt, &b_Cpdt);
   fChain->SetBranchAddress("Dpdt", &Dpdt, &b_Dpdt);
   fChain->SetBranchAddress("Sdt", &Sdt, &b_Sdt);
   fChain->SetBranchAddress("Adt", &Adt, &b_Adt);
   fChain->SetBranchAddress("Acpdt", &Acpdt, &b_Acpdt);
   fChain->SetBranchAddress("Tdc", &Tdc, &b_Tdc);
   fChain->SetBranchAddress("Tmadc", &Tmadc, &b_Tmadc);
   fChain->SetBranchAddress("Tmidc", &Tmidc, &b_Tmidc);
   fChain->SetBranchAddress("Mhdc", &Mhdc, &b_Mhdc);
   fChain->SetBranchAddress("Mldc", &Mldc, &b_Mldc);
   fChain->SetBranchAddress("Btdc", &Btdc, &b_Btdc);
   fChain->SetBranchAddress("Bwdc", &Bwdc, &b_Bwdc);
   fChain->SetBranchAddress("Cpdc", &Cpdc, &b_Cpdc);
   fChain->SetBranchAddress("Dpdc", &Dpdc, &b_Dpdc);
   fChain->SetBranchAddress("Sdc", &Sdc, &b_Sdc);
   fChain->SetBranchAddress("Adc", &Adc, &b_Adc);
   fChain->SetBranchAddress("Acpdc", &Acpdc, &b_Acpdc);
   fChain->SetBranchAddress("Tdmt", &Tdmt, &b_Tdmt);
   fChain->SetBranchAddress("Tmadmt", &Tmadmt, &b_Tmadmt);
   fChain->SetBranchAddress("Tmidmt", &Tmidmt, &b_Tmidmt);
   fChain->SetBranchAddress("Mhdmt", &Mhdmt, &b_Mhdmt);
   fChain->SetBranchAddress("Mldmt", &Mldmt, &b_Mldmt);
   fChain->SetBranchAddress("Btdmt", &Btdmt, &b_Btdmt);
   fChain->SetBranchAddress("Bwdmt", &Bwdmt, &b_Bwdmt);
   fChain->SetBranchAddress("Cpdmt", &Cpdmt, &b_Cpdmt);
   fChain->SetBranchAddress("Dpdmt", &Dpdmt, &b_Dpdmt);
   fChain->SetBranchAddress("Sdmt", &Sdmt, &b_Sdmt);
   fChain->SetBranchAddress("Admt", &Admt, &b_Admt);
   fChain->SetBranchAddress("Acpdmt", &Acpdmt, &b_Acpdmt);
   fChain->SetBranchAddress("Nxjdtc", &Nxjdtc, &b_Nxjdtc);
   fChain->SetBranchAddress("Nxjdt", &Nxjdt, &b_Nxjdt);
   fChain->SetBranchAddress("Nxjdc", &Nxjdc, &b_Nxjdc);
   fChain->SetBranchAddress("Nxjdmt", &Nxjdmt, &b_Nxjdmt);
   fChain->SetBranchAddress("Nxjetc", &Nxjetc, &b_Nxjetc);
   fChain->SetBranchAddress("Nxjet", &Nxjet, &b_Nxjet);
   fChain->SetBranchAddress("Nxjec", &Nxjec, &b_Nxjec);
   fChain->SetBranchAddress("Nxjemt", &Nxjemt, &b_Nxjemt);
   fChain->SetBranchAddress("Nxjctc", &Nxjctc, &b_Nxjctc);
   fChain->SetBranchAddress("Nxjct", &Nxjct, &b_Nxjct);
   fChain->SetBranchAddress("Nxjcc", &Nxjcc, &b_Nxjcc);
   fChain->SetBranchAddress("Nxjcmt", &Nxjcmt, &b_Nxjcmt);
   fChain->SetBranchAddress("Yddtc", Yddtc, &b_Yddtc);
   fChain->SetBranchAddress("Yedtc", Yedtc, &b_Yedtc);
   fChain->SetBranchAddress("Ycdtc", Ycdtc, &b_Ycdtc);
   fChain->SetBranchAddress("Njcedtc", Njcedtc, &b_Njcedtc);
   fChain->SetBranchAddress("Njcrdtc", Njcrdtc, &b_Njcrdtc);
   fChain->SetBranchAddress("Yddt", Yddt, &b_Yddt);
   fChain->SetBranchAddress("Yedt", Yedt, &b_Yedt);
   fChain->SetBranchAddress("Ycdt", Ycdt, &b_Ycdt);
   fChain->SetBranchAddress("Njcedt", Njcedt, &b_Njcedt);
   fChain->SetBranchAddress("Njcrdt", Njcrdt, &b_Njcrdt);
   fChain->SetBranchAddress("Yddc", Yddc, &b_Yddc);
   fChain->SetBranchAddress("Yedc", Yedc, &b_Yedc);
   fChain->SetBranchAddress("Ycdc", Ycdc, &b_Ycdc);
   fChain->SetBranchAddress("Njcedc", Njcedc, &b_Njcedc);
   fChain->SetBranchAddress("Njcrdc", Njcrdc, &b_Njcrdc);
   fChain->SetBranchAddress("Yddmt", Yddmt, &b_Yddmt);
   fChain->SetBranchAddress("Yedmt", Yedmt, &b_Yedmt);
   fChain->SetBranchAddress("Ycdmt", Ycdmt, &b_Ycdmt);
   fChain->SetBranchAddress("Njcedmt", Njcedmt, &b_Njcedmt);
   fChain->SetBranchAddress("Njcrdmt", Njcrdmt, &b_Njcrdmt);
   fChain->SetBranchAddress("Ntrk", &Ntrk, &b_Ntrk);
   fChain->SetBranchAddress("Id02", Id02, &b_Id02);
   fChain->SetBranchAddress("Dedx", Dedx, &b_Dedx);
   fChain->SetBranchAddress("Dded", Dded, &b_Dded);
   fChain->SetBranchAddress("Nhde", Nhde, &b_Nhde);
   fChain->SetBranchAddress("Dp", Dp, &b_Dp);
   fChain->SetBranchAddress("Ptrk", Ptrk, &b_Ptrk);
   fChain->SetBranchAddress("Ichg", Ichg, &b_Ichg);
   fChain->SetBranchAddress("Nhcj", Nhcj, &b_Nhcj);
   fChain->SetBranchAddress("Z0", Z0, &b_Z0);
   fChain->SetBranchAddress("D0", D0, &b_D0);
   fChain->SetBranchAddress("Nmttrk", &Nmttrk, &b_Nmttrk);
   fChain->SetBranchAddress("Imttrk", Imttrk, &b_Imttrk);
   fChain->SetBranchAddress("Mtscft", Mtscft, &b_Mtscft);
   fChain->SetBranchAddress("Nclus", &Nclus, &b_Nclus);
   fChain->SetBranchAddress("Nmtcls", &Nmtcls, &b_Nmtcls);
   fChain->SetBranchAddress("Imtcls", Imtcls, &b_Imtcls);
   fChain->SetBranchAddress("Nmtkil", &Nmtkil, &b_Nmtkil);
   fChain->SetBranchAddress("Imtkil", Imtkil, &b_Imtkil);
   fChain->SetBranchAddress("Pclus", Pclus, &b_Pclus);
   fChain->SetBranchAddress("Mtscfc", Mtscfc, &b_Mtscfc);
   fChain->SetBranchAddress("Ievtyp", &Ievtyp, &b_Ievtyp);
   fChain->SetBranchAddress("Inonr", &Inonr, &b_Inonr);
   fChain->SetBranchAddress("Pisr", Pisr, &b_Pisr);
   fChain->SetBranchAddress("Nprimf", &Nprimf, &b_Nprimf);
   fChain->SetBranchAddress("Iferid", Iferid, &b_Iferid);
   fChain->SetBranchAddress("Primf", Primf, &b_Primf);
   fChain->SetBranchAddress("Tp", &Tp, &b_Tp);
   fChain->SetBranchAddress("Tmap", &Tmap, &b_Tmap);
   fChain->SetBranchAddress("Tmip", &Tmip, &b_Tmip);
   fChain->SetBranchAddress("Mhp", &Mhp, &b_Mhp);
   fChain->SetBranchAddress("Mlp", &Mlp, &b_Mlp);
   fChain->SetBranchAddress("Btp", &Btp, &b_Btp);
   fChain->SetBranchAddress("Bwp", &Bwp, &b_Bwp);
   fChain->SetBranchAddress("Cpp", &Cpp, &b_Cpp);
   fChain->SetBranchAddress("Dpp", &Dpp, &b_Dpp);
   fChain->SetBranchAddress("Sp", &Sp, &b_Sp);
   fChain->SetBranchAddress("Ap", &Ap, &b_Ap);
   fChain->SetBranchAddress("Acpp", &Acpp, &b_Acpp);
   fChain->SetBranchAddress("Th", &Th, &b_Th);
   fChain->SetBranchAddress("Tmah", &Tmah, &b_Tmah);
   fChain->SetBranchAddress("Tmih", &Tmih, &b_Tmih);
   fChain->SetBranchAddress("Mhh", &Mhh, &b_Mhh);
   fChain->SetBranchAddress("Mlh", &Mlh, &b_Mlh);
   fChain->SetBranchAddress("Bth", &Bth, &b_Bth);
   fChain->SetBranchAddress("Bwh", &Bwh, &b_Bwh);
   fChain->SetBranchAddress("Cph", &Cph, &b_Cph);
   fChain->SetBranchAddress("Dph", &Dph, &b_Dph);
   fChain->SetBranchAddress("Sh", &Sh, &b_Sh);
   fChain->SetBranchAddress("Ah", &Ah, &b_Ah);
   fChain->SetBranchAddress("Acph", &Acph, &b_Acph);
   fChain->SetBranchAddress("Nxjdp", &Nxjdp, &b_Nxjdp);
   fChain->SetBranchAddress("Nxjdh", &Nxjdh, &b_Nxjdh);
   fChain->SetBranchAddress("Nxjep", &Nxjep, &b_Nxjep);
   fChain->SetBranchAddress("Nxjeh", &Nxjeh, &b_Nxjeh);
   fChain->SetBranchAddress("Nxjcp", &Nxjcp, &b_Nxjcp);
   fChain->SetBranchAddress("Nxjch", &Nxjch, &b_Nxjch);
   fChain->SetBranchAddress("Ydp", Ydp, &b_Ydp);
   fChain->SetBranchAddress("Yep", Yep, &b_Yep);
   fChain->SetBranchAddress("Ycp", Ycp, &b_Ycp);
   fChain->SetBranchAddress("Njcep", Njcep, &b_Njcep);
   fChain->SetBranchAddress("Njcrp", Njcrp, &b_Njcrp);
   fChain->SetBranchAddress("Ydh", Ydh, &b_Ydh);
   fChain->SetBranchAddress("Yeh", Yeh, &b_Yeh);
   fChain->SetBranchAddress("Ych", Ych, &b_Ych);
   fChain->SetBranchAddress("Njceh", Njceh, &b_Njceh);
   fChain->SetBranchAddress("Njcrh", Njcrh, &b_Njcrh);
   fChain->SetBranchAddress("Ntrkp", &Ntrkp, &b_Ntrkp);
   fChain->SetBranchAddress("Ilucp", Ilucp, &b_Ilucp);
   fChain->SetBranchAddress("Ptrkp", Ptrkp, &b_Ptrkp);
   fChain->SetBranchAddress("Ntrkh", &Ntrkh, &b_Ntrkh);
   fChain->SetBranchAddress("Ntrk2", &Ntrk2, &b_Ntrk2);
   fChain->SetBranchAddress("Ptrkh", Ptrkh, &b_Ptrkh);
   fChain->SetBranchAddress("Iluch", Iluch, &b_Iluch);
   fChain->SetBranchAddress("Iluc", Iluc, &b_Iluc);
   fChain->SetBranchAddress("Istrt", Istrt, &b_Istrt);
   fChain->SetBranchAddress("Ichgh", Ichgh, &b_Ichgh);
/*
    fChain->SetBranchAddress("event_number", &event_number, &b_hepmc3_event_event_number);
    fChain->SetBranchAddress("momentum_unit", &momentum_unit, &b_hepmc3_event_momentum_unit);
    fChain->SetBranchAddress("length_unit", &length_unit, &b_hepmc3_event_length_unit);
    fChain->SetBranchAddress("particles", &particles_, &b_hepmc3_event_particles_);
    fChain->SetBranchAddress("particles.pid", particles_pid, &b_particles_pid);
    fChain->SetBranchAddress("particles.status", particles_status, &b_particles_status);
    fChain->SetBranchAddress("particles.is_mass_set", particles_is_mass_set, &b_particles_is_mass_set);
    fChain->SetBranchAddress("particles.mass", particles_mass, &b_particles_mass);
    fChain->SetBranchAddress("particles.momentum.m_v1", particles_momentum_m_v1, &b_particles_momentum_m_v1);
    fChain->SetBranchAddress("particles.momentum.m_v2", particles_momentum_m_v2, &b_particles_momentum_m_v2);
    fChain->SetBranchAddress("particles.momentum.m_v3", particles_momentum_m_v3, &b_particles_momentum_m_v3);
    fChain->SetBranchAddress("particles.momentum.m_v4", particles_momentum_m_v4, &b_particles_momentum_m_v4);
    fChain->SetBranchAddress("vertices", &vertices_, &b_hepmc3_event_vertices_);
    fChain->SetBranchAddress("vertices.position.m_v1", vertices_position_m_v1, &b_vertices_position_m_v1);
    fChain->SetBranchAddress("vertices.position.m_v2", vertices_position_m_v2, &b_vertices_position_m_v2);
    fChain->SetBranchAddress("vertices.position.m_v3", vertices_position_m_v3, &b_vertices_position_m_v3);
    fChain->SetBranchAddress("vertices.position.m_v4", vertices_position_m_v4, &b_vertices_position_m_v4);
    fChain->SetBranchAddress("links1", &links1, &b_hepmc3_event_links1);
    fChain->SetBranchAddress("links2", &links2, &b_hepmc3_event_links2);
    fChain->SetBranchAddress("attribute_id", &attribute_id, &b_hepmc3_event_attribute_id);
    fChain->SetBranchAddress("attribute_name", &attribute_name, &b_hepmc3_event_attribute_name);
    fChain->SetBranchAddress("attribute_string", &attribute_string, &b_hepmc3_event_attribute_string);


    fChain->SetBranchAddress("weights", &weights, &b_hepmc3_event_weights);
*/
//}
}
#endif // #ifdef jadeanalysis_cxx
