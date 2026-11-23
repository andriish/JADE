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
// Histogram of the thrust variable 1-T (tracks and clusters): 20 bins, normalised to the unit integral,
// with the statistical uncertainties, in the logarithmic scale. The centre-of-mass energy in the legend
// is calculated from the beam energy of the events (the variable Ebeam of the ntuple).
int main(int argc, char **argv) {
    if (argc < 3) {
        std::cout << "Usage: " << argv[0] << " <input ROOT file> <output pdf file without extension>" << std::endl;
        exit(-1);
    }
    gStyle->SetOptStat(0);
    std::shared_ptr<TCanvas> outputCanvas = std::make_shared<TCanvas>("JADE","JADE",1024,768);
    std::shared_ptr<TH1D> outputHisto = std::make_shared<TH1D>("Thrust","",20,0.0,1.0);
    outputHisto->Sumw2();
    TChain* input =  new TChain("h10"); ///Note the fixed name
    input->Add(argv[1]);
    jadeanalysis*  myAnalysis =  new jadeanalysis();
    myAnalysis->fChain = input;
    myAnalysis->Init(input);
    double sumBeam = 0.0;
    long   nEvents = 0;
    for (int gentry=0; gentry<myAnalysis->fChain->GetEntries(); gentry++)
    {
        int entry=myAnalysis->fChain->LoadTree(gentry);
        myAnalysis->fChain->GetEntry(entry);
        outputHisto->Fill(myAnalysis->Tdtc);
        sumBeam += myAnalysis->Ebeam;
        nEvents++;
    }
    // Centre-of-mass energy: twice the (mean) beam energy
    const double ecm = nEvents ? 2.0*sumBeam/nEvents : 0.0;
    // Normalisation to the integral (the errors are scaled with the contents)
    if (outputHisto->Integral() > 0) outputHisto->Scale(1.0/outputHisto->Integral());
    outputHisto->SetXTitle("1-T");
    outputHisto->SetYTitle("1/N dN/d(1-T) per bin");
    outputHisto->SetLineColor(kBlue);
    outputHisto->SetMarkerColor(kBlue);
    outputHisto->SetMarkerStyle(20);
    outputHisto->SetMinimum(1e-4);
    outputHisto->SetMaximum(2.0);
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
    return 0;
}
