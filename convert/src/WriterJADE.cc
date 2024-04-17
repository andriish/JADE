#include <string>

#include "WriterJADE.h"
#include "HepMC3/GenVertex.h"
#include "HepMC3/GenParticle.h"
#include <math.h>
#include <stdio.h>
#include <set>
#include <stdlib.h>
#include <algorithm>
#include "PIDUtils.h"
#include "HepMC3/Print.h"
extern "C"
{
    struct JADEEVT cprod_;
    struct JADENAMES chcprd_;
    int jadeco_(int& a);
}
const size_t maxNP = 500;
const size_t maxNF = 300;
namespace HepMC3
{
WriterJADE::WriterJADE(const std::string &filename, const int mode)
{
    fUNIT = 100;
    fMODE = mode;/// BINARY OR TEXT

    fJ = (struct JADEEVT* )(&(cprod_.NEV));
    fN = (struct JADENAMES* )(&(chcprd_.CP[0][0]));
    const char* f = filename.c_str();
    int s = filename.length();
    jfopen_(f, fUNIT, fMODE,s);
}
static bool if_intersect(const vector<ConstGenParticlePtr>& a, const vector<ConstGenParticlePtr>& b)
{
    for( auto pp1: a )
        for( auto pp2: b )
            if (pp1==pp2)
                return true;
    return false;
}
static ConstGenParticlePtr get_parton_parent(ConstGenParticlePtr me,  std::vector< ConstGenParticlePtr> & bank)
{
    for (auto p: bank) if (me == p) return me;
    if ( me->production_vertex() && me->production_vertex()->particles_in().size() > 0 ) {
        return get_parton_parent(me->production_vertex()->particles_in().front(), bank);
    }
    return nullptr;
}
void WriterJADE::write_event(const GenEvent &evt)
{
    if (!fPDGID_to_JADE_name.size()) fill_names();
    Print::content(evt);
    fJ->NEV = evt.event_number();
    fJ->BEAM = std::abs(evt.beams().at(0)->momentum().e());
    fJ->PT = 0.0;     /* Calculated below */
    fJ->PHI = 0;      /* To do: calculate */
    fJ->THETA = 0;    /* To do: calculate*/
    fJ->IFLAVR = 0;   /* Calculated below */
    std::vector< ConstGenParticlePtr>  final_partons = GetPartons(evt);
    /* Flavour: collect hard quarks */
    std::vector < ConstGenParticlePtr > quarks;
    for (auto p: evt.particles() )
    {
        if (std::abs(p->pid()) < 7 ) quarks.push_back(p);
    }
    if (quarks.size() < 2) {
        fJ->IFLAVR = 0;
    } else {
        double sumE = 0;
        int flav = 0;
        for (size_t i1 = 0; i1 < quarks.size(); i1++) {
            for (size_t i2 = i1 + 1; i2 < quarks.size(); i2++) {
                if (quarks.at(i1)->pid() + quarks.at(i2)->pid() == 0 && quarks.at(i1)->momentum().e() + quarks.at(i2)->momentum().e() > sumE) {
                    flav = std::abs(quarks.at(i1)->pid());
                    sumE = quarks.at(i1)->momentum().e() + quarks.at(i2)->momentum().e();
                }
            }
        }

    /* This is an arbitrary cut that demands that a pair of found quarks has an evergy more than 0.5*fJ->BEAM */
    if (sumE > 0.5*fJ->BEAM) {
        fJ->IFLAVR = flav;
    } else {
        fJ->IFLAVR = 0;
    }
    }

    fJ->NP = 0;
    fJ->NF = 0;

    fJ->NC = 0;
    fJ->NN = 0;

    fJ->NCF = 0;
    fJ->NNF = 0;


    int i = 0, j = 0;
    std::map<int,int> particles_to_fJ;
    for ( unsigned int k = 0; (k < evt.particles().size()) && (i < maxNP) && ( j < maxNF ); k++)
    {

        auto particle = evt.particles().at(k);
        particles_to_fJ[particle->id()]=i+1;
        int KF = particle->pid();
        int q = MCUtils::PID::charge3(KF)/3;
        if (q == 0) {
            fJ->NN = fJ->NN + 1;
        }
        else {
            fJ->NC = fJ->NC + 1;
        }
        char buf[10];
        snprintf(buf, 10, "%d", KF);
        int resulti0 = snprintf(&(fN->CP[i][0]), 16, "%.16s", (std::string("PDGID=") + std::string(buf) + std::string("                ")).c_str());
        if (resulti0 < 0) printf("Warning: the particle name could be wrong!\n");
        if (fPDGID_to_JADE_name.find(KF) != fPDGID_to_JADE_name.end())  {
            int resulti = snprintf(&(fN->CP[i][0]), 16, "%.16s",    fPDGID_to_JADE_name.at(KF).c_str());
            if (resulti < 0) printf("Warning: the particle name could be wrong!\n");
        }
        fJ->JCH[i] = q;
        fJ->JTP[i] = jadeco_(KF);
        fJ->PP[i][0] = particle->momentum().px();
        fJ->PP[i][1] = particle->momentum().py();
        fJ->PP[i][2] = particle->momentum().pz();
        fJ->PP[i][3] = particle->momentum().e();
        fJ->XM[i] = particle->momentum().m();
//        fJ->JP[i][0] = 0; /* Is calculated below */
//        fJ->JP[i][1] = 0; /* Is calculated below */
        fJ->JP[0][i] = 0; /* Is calculated below */
        fJ->JP[1][i] = 0; /* Is calculated below */

        /* We multiply by the beam energy just to avoid dealing with units */
        if (fJ->XM[i] < 0.000001*fJ->BEAM) fJ->XM[i] = 0.0;
        auto VP = particle->production_vertex();
        for (auto parent: VP->particles_in()) {
            int parent_particle_id = parent->id();
            if (particles_to_fJ.find(parent_particle_id) != particles_to_fJ.end()) {
                //fJ->JP[i][0] = particles_to_fJ[parent_particle_id];
                fJ->JP[0][i] = particles_to_fJ[parent_particle_id];
                if (VP->particles_in().size() > 1) printf("Warning: the particle has more than one parent!\n");
                break;
            }
        }
        auto parton_parent = get_parton_parent(particle,final_partons);
        if (parton_parent) {
            int parton_parent_id = parton_parent->id();
            if (particles_to_fJ.find(parton_parent_id) != particles_to_fJ.end()) {
                //fJ->JP[i][1] = particles_to_fJ[parton_parent_id];
                fJ->JP[1][i] = particles_to_fJ[parton_parent_id];
            }
        }

        if (particle->status() != 1 
            // Do not track neutrinos
            ||  std::abs(KF) == 12
            ||  std::abs(KF) == 14
            ||  std::abs(KF) == 16
        ) {
            i++;
            continue;
        }

        int resultj0 = snprintf(&(fN->CF[j][0]), 16, "%.16s", (std::string("PDGID=") + std::string(buf)+std::string("                ")).c_str());
        if (resultj0 < 0) printf("Warning: the particle name could be wrong!\n");
        if (fPDGID_to_JADE_name.find(KF) != fPDGID_to_JADE_name.end())  {
            int resultj = snprintf(&(fN->CF[j][0]), 16, "%.16s", fPDGID_to_JADE_name.at(KF).c_str());
            if (resultj < 0 ) printf("Warning: the particle name could be wrong!\n");
        }

        fJ->ICF[j] = fJ->JCH[i];
        fJ->ITF[j] = fJ->JTP[i];
        fJ->PF[j][0] = fJ->PP[i][0];
        fJ->PF[j][1] = fJ->PP[i][1];
        fJ->PF[j][2] = fJ->PP[i][2];
        fJ->PF[j][3] = fJ->PP[i][3];
        fJ->XMF[j] = fJ->XM[i];

        if (q == 0) {
            fJ->NCF = fJ->NCF + 1;
        }
        else  {
            fJ->NNF = fJ->NNF + 1;
        }

        if (VP)
        {
            fJ->PSTR[j][0] = VP->position().x();
            fJ->PSTR[j][1] = VP->position().y();
            fJ->PSTR[j][2] = VP->position().z();
        }
        else
        {
            fJ->PSTR[j][0] = 0;
            fJ->PSTR[j][1] = 0;
            fJ->PSTR[j][2] = 0;
        }
        fJ->PT = fJ->PT + std::sqrt(fJ->PP[i][0]*fJ->PP[i][0] + fJ->PP[i][1]*fJ->PP[i][1]);
        i++;
        j++;
    }
    fJ->NP = i;
    fJ->NF = j;

    jfwrite_(fUNIT, fMODE);
}

bool WriterJADE::failed() {
    return true;
}

void WriterJADE::close()
{
    jfclose_(fUNIT);
}


std::vector< ConstGenParticlePtr>  WriterJADE::GetPartons(const GenEvent &evt)
{
    const std::set<int>  allowed{-6,-5,-4,-3,-2,-1,1,2,3,4,5,6,-16,-15,-14,-13,-12,-11,11,12,13,14,15,16,21,22,23,24,-24};
    std::vector< ConstGenVertexPtr> good;
    std::vector<bool> vertices_status_in;//(N+1,true);
    std::vector<bool> vertices_status_ou;//(N+1,true);
    std::vector<bool> vertices_status_active;//(N+1,true);
    std::vector<ConstGenVertexPtr> vertices_ou;//(N+1);
    std::vector<ConstGenVertexPtr> vertices_in;//(N+1);
    for( auto v: evt.vertices() )
    {
        bool inok=true;
        for(auto  pp: (*v).particles_in() ) if (allowed.find((*pp).pid())==allowed.end()) {
                inok=false;
                break;
            }
        bool outok=true;
        for( auto pp: (*v).particles_out() ) if (allowed.find((*pp).pid())==allowed.end())  {
                outok=false;
                break;
            }
        good.push_back(v);
        vertices_ou.push_back(v);
        vertices_in.push_back(v);

        vertices_status_ou.push_back(outok);
        vertices_status_in.push_back(inok);
        vertices_status_active.push_back(true);
    }

    bool have_what_to_do=true;
    while (have_what_to_do)
    {
        have_what_to_do=false;
        for (size_t i=0; i<good.size(); i++)
            if ((vertices_status_in[i]&&!vertices_status_ou[i])||(!vertices_status_active[i]))
                for (size_t j=0; j<good.size(); j++)
                    if (vertices_status_active[j])
                        if (if_intersect((*vertices_ou[i]).particles_out(),(*vertices_in[j]).particles_in()))
                        {
                            vertices_status_active[j]=false;
                            have_what_to_do=true;
                        }
    }
    std::set<ConstGenParticlePtr> particles_final;
    for (size_t i=0; i<good.size(); i++)
    {
        if (!vertices_status_active[i]) continue;
        if (vertices_status_in[i]&&!vertices_status_ou[i])
            for( auto it: good[i]->particles_in() )
                particles_final.insert(it);
        if (vertices_status_in[i]&& vertices_status_ou[i])
            for( auto it: good[i]->particles_out() )
                if (it->status()==1)  particles_final.insert(it);
    }
    std::vector<ConstGenParticlePtr> ret;
    for (auto it: particles_final )
        ret.push_back(it);
    return ret;
}


}// namespace HepMC3
