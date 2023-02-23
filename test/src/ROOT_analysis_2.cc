#include <iostream>
#include <string>
#include "jadehelpers.h"
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
#include "fjcore/fjcore.hh"

template <>  void set_momentum<fjcore::PseudoJet>(fjcore::PseudoJet &vv, const float x, const float y, const float z, const float e) 
{vv.reset(double(x),double(y),double(z),double(e));}

int main(int argc, char **argv) {
    if (argc < 3) {
        std::cout << "Usage: " << argv[0] << " <input ROOT file> <output pdf file without extension>" << std::endl;
        exit(-1);
    }
    std::shared_ptr<TCanvas> outputCanvas = std::make_shared<TCanvas>("JADE","JADE",1024,768);
    std::shared_ptr<TH1D> outputHisto = std::make_shared<TH1D>("y23","y23",20,-10.0,10.0);
    TChain* input =  new TChain("h10"); ///Note the fixed name
    input->Add(argv[1]);
    jadeanalysis*  myAnalysis =  new jadeanalysis();
    myAnalysis->fChain = input;
    myAnalysis->Init(input);
    for (int gentry=0; gentry<myAnalysis->fChain->GetEntries(); gentry++)
    {
        int entry = myAnalysis->fChain->LoadTree(gentry);
        myAnalysis->fChain->GetEntry(entry);
        std::vector<fjcore::PseudoJet> pjets = GetFromTracks<jadeanalysis,fjcore::PseudoJet>(myAnalysis);
        if ( pjets.size() < 3 ) {
            outputHisto->Fill(-1);
            continue;
        }
        fjcore::JetDefinition jet_definition(fjcore::ee_genkt_algorithm, 3*M_PI/2, 0.0);
        fjcore::ClusterSequence cluster_sequence(pjets, jet_definition);
        std::vector<fjcore::PseudoJet > jets = cluster_sequence.exclusive_jets(3);
        outputHisto->Fill(jets[0].E());
    }
    outputCanvas->cd();
    outputHisto->Draw();
    outputCanvas->SaveAs( (std::string(argv[2])+".pdf").c_str());
    return 0;
}
