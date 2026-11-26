#include <iostream>
#include <string>
#define jadeanalysis_cxx
#include "jadeanalysis.h"
void    jadeanalysis::Begin(TTree *tree) {}
void    jadeanalysis::SlaveBegin(TTree *tree) {}
Bool_t  jadeanalysis::Notify() {
    return true;
}
Bool_t  jadeanalysis::Process(Long64_t entry) {
    return true;
}
void    jadeanalysis::SlaveTerminate() {}
void    jadeanalysis::Terminate() {}
#include "TCanvas.h"
#include "TH1D.h"
#include "TLegend.h"
#include "TStyle.h"
#include "TPad.h"
#include <cstdio>
#include <cmath>
// Histogram of the thrust variable 1-T (tracks and clusters): 20 bins, normalised to the unit integral
// and divided by the bin width,
// with the statistical uncertainties, in the logarithmic scale. The centre-of-mass energy in the legend
// is calculated from the beam energy of the events (the variable Ebeam of the ntuple).
// A second picture (<output>_energy.pdf) has the sum of the energies of the tracks of an event (pion mass assumed) and
// the sum of the energies of the lead glass clusters of an event, in bins of 1.5 GeV from 0 to 60 GeV (the same
// variables as in the Fortran analysis k0s of the test suite).
int main(int argc, char **argv) {
    if (argc < 3) {
        std::cout << "Usage: " << argv[0] << " <input ROOT file> <output pdf file without extension> [<title of the plot>]" << std::endl;
        exit(-1);
    }
    gStyle->SetOptStat(0);
    std::shared_ptr<TCanvas> outputCanvas = std::make_shared<TCanvas>("JADE","JADE",1024,768);
    std::shared_ptr<TH1D> outputHisto = std::make_shared<TH1D>("Thrust",(argc > 3 ? argv[3] : ""),20,0.0,1.0);
    outputHisto->Sumw2();
    TChain* input =  new TChain("h10"); ///Note the fixed name
    input->Add(argv[1]);
    jadeanalysis*  myAnalysis =  new jadeanalysis();
    myAnalysis->fChain = input;
    myAnalysis->Init(input);
    std::shared_ptr<TH1D> hTrk = std::make_shared<TH1D>("Etrk",(argc > 3 ? argv[3] : ""),40,0.0,60.0);
    std::shared_ptr<TH1D> hCls = std::make_shared<TH1D>("Ecls",(argc > 3 ? argv[3] : ""),40,0.0,60.0);
    const double mpi = 0.13957;
    double sumBeam = 0.0;
    long   nEvents = 0;
    for (int gentry=0; gentry<myAnalysis->fChain->GetEntries(); gentry++)
    {
        int entry=myAnalysis->fChain->LoadTree(gentry);
        myAnalysis->fChain->GetEntry(entry);
        outputHisto->Fill(myAnalysis->Tdtc);
        // sums of the energies of the tracks (pion mass) and of the clusters (massless)
        double etrk = 0.0, ecls = 0.0;
        for (int i = 0; i < myAnalysis->Ntrk && i < 501; i++) {
            const double px = myAnalysis->Ptrk[i][0], py = myAnalysis->Ptrk[i][1], pz = myAnalysis->Ptrk[i][2];
            etrk += std::sqrt(px*px + py*py + pz*pz + mpi*mpi);
        }
        for (int i = 0; i < myAnalysis->Nclus && i < 1503; i++) {
            const double px = myAnalysis->Pclus[i][0], py = myAnalysis->Pclus[i][1], pz = myAnalysis->Pclus[i][2];
            ecls += std::sqrt(px*px + py*py + pz*pz);
        }
        hTrk->Fill(etrk);
        hCls->Fill(ecls);
        sumBeam += myAnalysis->Ebeam;
        nEvents++;
    }
    // Centre-of-mass energy: twice the (mean) beam energy
    const double ecm = nEvents ? 2.0*sumBeam/nEvents : 0.0;
    // Normalisation to the unit integral and division by the bin width: 1/N dN/d(1-T)
    // (the errors are scaled with the contents)
    if (outputHisto->Integral() > 0) outputHisto->Scale(1.0/outputHisto->Integral(), "width");
    outputHisto->SetXTitle("1-T");
    outputHisto->SetYTitle("1/N dN/d(1-T)");
    outputHisto->SetLineColor(kBlue);
    outputHisto->SetMarkerColor(kBlue);
    outputHisto->SetMarkerStyle(20);
    outputHisto->SetMinimum(0.012);
    outputHisto->SetMaximum(12.0);
    outputCanvas->cd();
    gPad->SetLogy();
    outputHisto->Draw("E1");
    char label[64];
    snprintf(label, sizeof(label), "#sqrt{s} = %.1f GeV (%ld events)", ecm, nEvents);
    TLegend* legend = new TLegend(0.45, 0.78, 0.88, 0.88);
    legend->SetBorderSize(0);
    legend->SetFillStyle(0);
    legend->AddEntry(outputHisto.get(), label, "lep");
    legend->Draw();
    outputCanvas->SaveAs( (std::string(argv[2])+".pdf").c_str());
    // Sums of the energies of the tracks and of the clusters of an event
    std::shared_ptr<TCanvas> energyCanvas = std::make_shared<TCanvas>("JADEE","JADEE",1024,512);
    energyCanvas->Divide(2, 1);
    TH1D* hh[2] = {hTrk.get(), hCls.get()};
    const char* xt[2] = {"sum of the track energies, GeV", "sum of the lead glass cluster energies, GeV"};
    for (int k = 0; k < 2; k++) {
        energyCanvas->cd(k + 1);
        gPad->SetLogy();
        gPad->SetLeftMargin(0.15);
        hh[k]->SetStats(0);
        hh[k]->SetLineColor(kBlue);
        hh[k]->SetFillColor(kAzure - 9);
        hh[k]->SetMinimum(0.5);
        hh[k]->SetMaximum(3.0 * hh[k]->GetMaximum() + 1.0);
        hh[k]->SetXTitle(xt[k]);
        hh[k]->SetYTitle("events per 1.5 GeV");
        hh[k]->Draw("HIST");
        TLegend* lg = new TLegend(0.35, 0.78, 0.92, 0.88);
        lg->SetBorderSize(0);
        lg->SetFillStyle(0);
        char lab[96];
        snprintf(lab, sizeof(lab), "#sqrt{s} = %.1f GeV (%ld events, mean %.1f GeV)", ecm, nEvents, hh[k]->GetMean());
        lg->AddEntry(hh[k], lab, "f");
        lg->Draw();
    }
    energyCanvas->SaveAs( (std::string(argv[2])+"_energy.pdf").c_str());
    return 0;
}
