// Plots of the Fortran analysis test/src/k0s.f (tests K0S_DATA_1, K0S_MC_1, K0S_MC_3) for the note:
//  - the invariant mass of the pi+ pi- pairs (files k0s_data_1.txt, k0s_mc_1.txt, k0s_mc_3.txt:
//    bin centre, contents, error)
//  - the distributions of the distances of the cuts (file k0s_mc_3_dist.txt: the lines G, R, Z with the bin
//    centre in mm and the contents; the first lines have the cuts)
// usage: root -l -b -q 'k0s_plots.C("<directory of the tests>","<output directory>")'
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include "TCanvas.h"
#include "TH1D.h"
#include "TLine.h"
#include "TBox.h"
#include "TStyle.h"
#include "TPad.h"
#include "TLatex.h"

static void mass(const std::string& dir, const std::string& out) {
    const char* files[3] = {"k0s_data_1.txt", "k0s_mc_1.txt", "k0s_mc_3.txt"};
    const char* titles[3] = {"real data sample", "Pythia8, 34.6 GeV", "Pythia8, 43.8 GeV"};
    TCanvas c("kmass", "kmass", 1100, 330);
    c.Divide(3, 1);
    std::vector<TH1D*> hs;
    for (int i = 0; i < 3; i++) {
        c.cd(i + 1);
        gPad->SetLeftMargin(0.19);
        gPad->SetBottomMargin(0.16);
        TH1D* h = new TH1D(Form("hm%d", i), titles[i], 40, 0.3, 0.7);
        std::ifstream in(dir + "/" + files[i]);
        double x, n, e;
        int ib = 1;
        while (in >> x >> n >> e && ib <= 40) {
            h->SetBinContent(ib, n);
            h->SetBinError(ib, n > 0 ? e : 0.0);
            ib++;
        }
        h->SetStats(0);
        h->SetMarkerStyle(20);
        h->SetMarkerSize(0.6);
        h->SetMarkerColor(kBlue);
        h->SetLineColor(kBlue);
        h->SetMinimum(0);
        h->GetXaxis()->SetTitle("m(#pi^{+}#pi^{-}), GeV");
        h->GetYaxis()->SetTitle("pairs per 10 MeV");
        h->GetXaxis()->SetTitleSize(0.055);
        h->GetYaxis()->SetTitleSize(0.055);
        h->GetXaxis()->SetLabelSize(0.05);
        h->GetYaxis()->SetLabelSize(0.05);
        h->GetYaxis()->SetTitleOffset(1.5);
        h->Draw("E1");
        gPad->Update();
        TLine* l = new TLine(0.4976, 0, 0.4976, gPad->GetUymax());
        l->SetLineColor(kRed);
        l->SetLineStyle(2);
        l->Draw();
        hs.push_back(h);
    }
    c.SaveAs((out + "/k0s_mass.pdf").c_str());
}

static void cuts(const std::string& dir, const std::string& out) {
    std::ifstream in(dir + "/k0s_mc_3_dist.txt");
    TH1D* h[3] = {new TH1D("hg", "", 40, 0, 20), new TH1D("hr", "", 40, 0, 400), new TH1D("hz", "", 40, 0, 200)};
    double lo[3] = {0, 0, 0}, hi[3] = {0, 0, 0};
    std::string line;
    int ib[3] = {1, 1, 1};
    while (std::getline(in, line)) {
        std::istringstream s(line);
        if (line.rfind("# cuts", 0) == 0) {
            std::string t;
            std::vector<std::string> w;
            while (s >> t) w.push_back(t);
            hi[0] = std::stod(w[w.size() - 3]);
            lo[1] = std::stod(w[w.size() - 2]);
            hi[1] = std::stod(w[w.size() - 1]);
        } else if (line.rfind("# cut Z", 0) == 0) {
            std::string t, last;
            while (s >> t) last = t;
            hi[2] = std::stod(last);
        } else if (!line.empty() && (line[0] == 'G' || line[0] == 'R' || line[0] == 'Z')) {
            std::string k;
            double x, n;
            s >> k >> x >> n;
            int j = k == "G" ? 0 : (k == "R" ? 1 : 2);
            h[j]->SetBinContent(ib[j]++, n);
        }
    }
    const char* titles[3] = {"distance of the helices, mm", "distance of the vertex from the beam, mm",
                             "difference of z at the vertex, mm"};
    TCanvas c("kcuts", "kcuts", 1100, 330);
    c.Divide(3, 1);
    for (int j = 0; j < 3; j++) {
        c.cd(j + 1);
        gPad->SetLogy();
        gPad->SetLeftMargin(0.19);
        gPad->SetBottomMargin(0.16);
        h[j]->SetStats(0);
        h[j]->SetFillColor(kAzure - 9);
        h[j]->SetLineColor(kBlue);
        h[j]->SetMinimum(0.5);
        h[j]->GetXaxis()->SetTitle(titles[j]);
        h[j]->GetYaxis()->SetTitle("pairs");
        h[j]->GetXaxis()->SetTitleSize(0.05);
        h[j]->GetYaxis()->SetTitleSize(0.055);
        h[j]->GetXaxis()->SetLabelSize(0.05);
        h[j]->GetYaxis()->SetLabelSize(0.05);
        h[j]->GetYaxis()->SetTitleOffset(1.5);
        h[j]->Draw("HIST");
        gPad->Update();
        double y0 = std::pow(10.0, gPad->GetUymin()), y1 = std::pow(10.0, gPad->GetUymax());
        TBox* b = new TBox(lo[j], y0, hi[j], y1);
        b->SetFillColorAlpha(kGreen + 1, 0.2);
        b->SetLineWidth(0);
        b->Draw();
        double cutv[2] = {lo[j], hi[j]};
        for (int k = 0; k < 2; k++) {
            if (cutv[k] <= 0) continue;
            TLine* l = new TLine(cutv[k], y0, cutv[k], y1);
            l->SetLineColor(kRed);
            l->SetLineStyle(2);
            l->Draw();
        }
        h[j]->Draw("HIST SAME");
    }
    c.SaveAs((out + "/k0s_cuts.pdf").c_str());
}

void k0s_plots(const char* dir = "../../buildGNU64/test", const char* out = "Figures") {
    gStyle->SetOptStat(0);
    mass(dir, out);
    cuts(dir, out);
}
