#include "WriterJADE.h"
#include "HepMC3/GenVertex.h"
#include "HepMC3/GenParticle.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <algorithm>
#include "PIDUtils.h"
extern "C"
{
    struct JADEEVT cprod_;
    struct JADENAMES chcprd_;
    int jadeco_(int& a);
}
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
void WriterJADE::write_event(const GenEvent &evt)
{
    if (!fPDGID_to_JADE_name.size()) fill_names();
    fJ->NEV = evt.event_number();
    fJ->BEAM = std::abs(evt.particles().at(0)->momentum().e());
    fJ->PT = 0.0;
    fJ->PHI = 0;     /* Calculated later */
    fJ->THETA = 0;    /* Calculated later */
    fJ->IFLAVR = 0;
    std::vector<std::pair<double, int > > quarks;

    /*Flavour: collect hard quarks */
    for (unsigned int k = 0; k < evt.particles().size(); k++)
    {
        if (std::abs(evt.particles().at(k)->pid()) < 6 /*&& (evt.particles().at(k)->momentum().e() > 0.1*fJ->BEAM)*/)
            quarks.push_back(std::pair<double, int > (evt.particles().at(k)->momentum().e(), k));
    }
    if (quarks.size() < 2) fJ->IFLAVR = 0;
    if (quarks.size() > 2)
    {
        std::sort(quarks.begin(),quarks.end());
        double sumE = 0;
        for (unsigned int  y = 0; y<quarks.size(); y++) if (evt.particles().at(quarks.at(0).second)->pid() == -evt.particles().at(quarks.at(y).second)->pid())
                sumE = quarks.at(0).first + quarks.at(y).first;
        if (sumE > 0.5*fJ->BEAM) {
            fJ->IFLAVR = std::abs(evt.particles().at(quarks.at(0).second)->pid());
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
    char buf[10];

    int i = 0, j = 0;
    for ( unsigned int k = 0; (k < evt.particles().size()) && (i < 500) && ( j < 300 ); k++)
    {
        int q = MCUtils::PID::charge3(evt.particles().at(i)->pid());
        if (q == 0) fJ->NN = fJ->NN + 1;
        else fJ->NC = fJ->NC + 1;
        int KF = evt.particles().at(i)->pid();
        snprintf(buf, 10, "%d", KF);
        int resulti0 = snprintf(&(fN->CP[i][0]), 16, "%.16s", (std::string("PDGID=") + std::string(buf) + std::string("                ")).c_str());
        if (resulti0<0) printf("Warning: the particle name could be wrong!\n");
        if (fPDGID_to_JADE_name.find(KF)!=fPDGID_to_JADE_name.end())  {
           int resulti = snprintf(&(fN->CP[i][0]), 16, "%.16s",    fPDGID_to_JADE_name.at(KF).c_str());
           if (resulti < 0) printf("Warning: the particle name could be wrong!\n");
        }
        fJ->JCH[i] = q;
        fJ->JTP[i] = jadeco_(KF);
        fJ->PP[i][0] = evt.particles().at(i)->momentum().px();
        fJ->PP[i][1] = evt.particles().at(i)->momentum().py();
        fJ->PP[i][2] = evt.particles().at(i)->momentum().pz();
        fJ->PP[i][3] = evt.particles().at(i)->momentum().e();
        fJ->XM[i] = evt.particles().at(i)->momentum().m();
        if (fJ->XM[i] < 0.000001) fJ->XM[i] = 0.0;

        if (evt.particles().at(i)->status() != 1 || (std::abs(KF) == 12) || (std::abs(KF) == 14) || (std::abs(KF) == 16)) {
            i++;
            continue;
        }


        int resultj0 = snprintf(&(fN->CF[j][0]), 16, "%.16s", (std::string("PDGID=") + std::string(buf)+std::string("                ")).c_str());
        if (resultj0 < 0) printf("Warning: the particle name could be wrong!\n");
        if (fPDGID_to_JADE_name.find(KF) != fPDGID_to_JADE_name.end())  {
           int resultj = snprintf(&(fN->CP[j][0]), 16, "%.16s", fPDGID_to_JADE_name.at(KF).c_str());
           if (resultj < 0 ) printf("Warning: the particle name could be wrong!\n");
        }

        fJ->ICF[j] = fJ->JCH[i];
        fJ->PF[j][0] = fJ->PP[i][0];
        fJ->PF[j][1] = fJ->PP[i][1];
        fJ->PF[j][2] = fJ->PP[i][2];
        fJ->PF[j][3] = fJ->PP[i][3];
        fJ->XMF[j] = fJ->XM[i];
        fJ->ICF[j] = q;
        fJ->ITF[j] = jadeco_(KF);

        if (q == 0) fJ->NCF = fJ->NCF + 1;
        else fJ->NNF = fJ->NNF + 1;

        auto VP = evt.particles().at(i)->production_vertex();
        auto VE = evt.particles().at(i)->end_vertex();

        unsigned int dmax = 0;
        unsigned int pmin = evt.particles().size();
        if (VE) {
            for (unsigned int y = 0; y < evt.particles().size(); y++) {
                if (evt.particles().at(y)->production_vertex() == VE) dmax = std::max(dmax,y);
            }

        }
        if (VP) {
            for (unsigned int y = 0; y < evt.particles().size(); y++) {
                if (evt.particles().at(y)->end_vertex() == VP) pmin = std::min(pmin,y);
            }
        }
        fJ->JP[0][i] = int(pmin);
        fJ->JP[1][i] = int(dmax);

        if (VP)
        {
            fJ->PSTR[j][1-1] = VP->position().x();
            fJ->PSTR[j][2-1] = VP->position().y();
            fJ->PSTR[j][3-1] = VP->position().z();

        }
        else
        {
            fJ->PSTR[j][1-1] = 0;
            fJ->PSTR[j][2-1] = 0;
            fJ->PSTR[j][3-1] = 0;
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
}// namespace HepMC3
