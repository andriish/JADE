#include <iostream>
#include <string>
#define jadeanalysis_cxx
#include "jadeanalysis.h"
void    jadeanalysis::Begin(TTree *tree){}
void    jadeanalysis::SlaveBegin(TTree *tree){}
Bool_t  jadeanalysis::Notify(){}
Bool_t  jadeanalysis::Process(Long64_t entry){}
void    jadeanalysis::SlaveTerminate(){}
void    jadeanalysis::Terminate(){}
#include "TCanvas.h"
#include "TH1D.h"
int main(int argc, char **argv) {
    if (argc < 3) {
        std::cout << "Usage: " << argv[0] << " <input ROOT file> <output pdf file without extension>" << std::endl;
        exit(-1);
    }
    std::shared_ptr<TCanvas> outputCanvas = std::make_shared<TCanvas>("JADE","JADE",1024,768);
    std::shared_ptr<TH1D> outputHisto = std::make_shared<TH1D>("Thrust","Thrust",10,0.0,1.0);
    TChain* input =  new TChain("h10"); ///Note the fixed name
    input->Add(argv[1]);
    jadeanalysis*  myAnalysis =  new jadeanalysis();
    myAnalysis->fChain = input;
    myAnalysis->Init(input);
    for (int gentry=0;gentry<myAnalysis->fChain->GetEntries();gentry++)
    {
       int entry=myAnalysis->fChain->LoadTree(gentry);
       myAnalysis->fChain->GetEntry(entry);
       outputHisto->Fill(myAnalysis->Tdtc);
    }
     outputCanvas->cd();
     outputHisto->Draw();
     outputCanvas->SaveAs( (std::string(argv[2])+".pdf").c_str());
return 0;
}
