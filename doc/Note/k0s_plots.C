// Plots of the Fortran analysis test/src/k0s.f (tests K0S_DATA_1, K0S_MC_1, K0S_MC_3) for the note:
//  - the invariant mass of the pi+ pi- pairs (files k0s_data_1.txt, k0s_mc_1.txt, k0s_mc_3.txt:
//    bin centre, contents, error)
//  - the sum of the track energies of an event (the lines H for the events with 5 or more selected tracks and L for fewer
//    in the files *_dist.txt of the three samples)
//  - the distributions of the distances of the cuts (files k0s_data_1_dist.txt, k0s_mc_3_dist.txt: the lines G, R, Z with the bin
//    centre in mm and the contents; the first lines have the cuts)
// usage: root -l -b -q 'k0s_plots.C("<directory of the tests>","<output directory>")'
#include <cstdio>
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
#include "TLegend.h"

// the samples: the tag of the files k0s_<tag>.txt and k0s_<tag>_dist.txt of the tests, and the title of the plots
typedef std::vector<std::pair<std::string, std::string> > Samples;

// title of a sample; for the real data the mean beam energy of the events (the line "# Ebeam" of the file *_dist.txt) is added
static std::string title_of(const std::string& dir, const std::pair<std::string, std::string>& smp) {
    if (smp.first.compare(0, 4, "data") != 0) return smp.second;
    std::ifstream in(dir + "/k0s_" + smp.first + "_dist.txt");
    std::string line;
    while (std::getline(in, line)) {
        if (line.rfind("# Ebeam", 0) == 0) {
            double e = std::stod(line.substr(line.find(':') + 1));
            char buf[64];
            snprintf(buf, sizeof(buf), ", E_{beam} = %.1f GeV", e);
            return smp.second + buf;
        }
    }
    return smp.second;
}

static void mass(const std::string& dir, const std::string& out, const Samples& S, const std::string& suffix) {
    const int ns = S.size();
    TCanvas c(("kmass" + suffix).c_str(), "kmass", 367 * ns, 330);
    c.Divide(ns, 1);
    std::vector<TH1D*> hs;
    for (int i = 0; i < ns; i++) {
        c.cd(i + 1);
        gPad->SetLeftMargin(0.19);
        gPad->SetBottomMargin(0.16);
        TH1D* h = new TH1D(Form("hm%s%d", suffix.c_str(), i), title_of(dir, S[i]).c_str(), 40, 0.3, 0.7);
        std::ifstream in(dir + "/k0s_" + S[i].first + ".txt");
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
        h->GetYaxis()->SetTitle("candidates per 10 MeV");
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
    c.SaveAs((out + "/k0s_mass" + suffix + ".pdf").c_str());
}

// panels: 0 distance of the helices, 1 distance of the vertex from the primary vertex, 2 difference of z,
// 3 z of the primary vertex, 4 chi2 of the fit of the vertex, 5 number of selected tracks of the event
static void cutsrow(TCanvas& c, int row, int ncol, const std::vector<int>& panels, const std::string& file,
                    const char* tt) {
    std::ifstream in(file);
    TH1D* h[6] = {new TH1D(Form("hg%d", row), tt, 40, 0, 20), new TH1D(Form("hr%d", row), tt, 40, 0, 400),
                  new TH1D(Form("hz%d", row), tt, 40, 0, 200), new TH1D(Form("hp%d", row), tt, 40, -800, 800),
                  new TH1D(Form("hc%d", row), tt, 40, 0, 20), new TH1D(Form("ht%d", row), tt, 40, -0.5, 39.5)};
    double lo[6] = {0, 0, 0, 0, 0, 0}, hi[6] = {0, 0, 0, 0, 0, 0};
    double zpcut = 0.0, ntcut = 0.0;
    std::string line;
    int ib[6] = {1, 1, 1, 1, 1, 1};
    while (std::getline(in, line)) {
        std::istringstream s(line);
        if (line.rfind("# cuts", 0) == 0) {
            std::string t;
            std::vector<std::string> w;
            while (s >> t) w.push_back(t);
            hi[0] = std::stod(w[w.size() - 3]);
            lo[1] = std::stod(w[w.size() - 2]);
            hi[1] = std::stod(w[w.size() - 1]);
        } else if (line.rfind("# cut CHI", 0) == 0) {
            std::string t, last;
            while (s >> t) last = t;
            hi[4] = std::stod(last);
        } else if (line.rfind("# cut NT", 0) == 0) {
            std::string t, last;
            while (s >> t) last = t;
            ntcut = std::stod(last);
        } else if (line.rfind("# cut ZP", 0) == 0) {
            std::string t, last;
            while (s >> t) last = t;
            zpcut = std::stod(last);
        } else if (line.rfind("# cut Z", 0) == 0) {
            std::string t, last;
            while (s >> t) last = t;
            hi[2] = std::stod(last);
        } else if (!line.empty() && std::string("GRZPCT").find(line[0]) != std::string::npos) {
            std::string k;
            double x, n;
            s >> k >> x >> n;
            int j = std::string("GRZPCT").find(k[0]);
            // the order of the panels: G R Z P C T -> 0 1 2 3 4 5
            h[j]->SetBinContent(ib[j]++, n);
        }
    }
    // a cut above the range of the histogram is no cut: the accepted region goes to the edge, no line
    const double xmax[6] = {20.0, 400.0, 200.0, 800.0, 20.0, 39.5};
    bool showhi[6];
    for (int j = 0; j < 6; j++) {
        showhi[j] = hi[j] < xmax[j];
        if (!showhi[j]) hi[j] = xmax[j];
    }
    const char* titles[6] = {"distance of the helices, mm", "vertex distance from the primary vertex, mm",
                             "difference of z at the vertex, mm", "z of the primary vertex, mm",
                             "#chi^{2} of the fit of the vertex", "selected tracks of the event"};
    for (size_t k = 0; k < panels.size(); k++) {
        int j = panels[k];
        c.cd(row * ncol + k + 1);
        bool lin = (j == 3 || j == 5);
        if (!lin) gPad->SetLogy();
        gPad->SetLeftMargin(0.19);
        gPad->SetBottomMargin(0.16);
        h[j]->SetStats(0);
        h[j]->SetFillColor(kAzure - 9);
        h[j]->SetLineColor(kBlue);
        h[j]->SetMinimum(lin ? 0.0 : 0.5);
        h[j]->GetXaxis()->SetTitle(titles[j]);
        // bin widths: 0.5 mm, 10 mm, 5 mm, 40 mm, 0.5 (chi2), 1 track
        const char* ytitles[6] = {"candidates per 0.5 mm", "candidates per 10 mm", "candidates per 5 mm",
                                  "events per 40 mm", "candidates per 0.5", "events per track"};
        h[j]->GetYaxis()->SetTitle(ytitles[j]);
        h[j]->GetXaxis()->SetTitleSize(0.05);
        h[j]->GetYaxis()->SetTitleSize(0.055);
        h[j]->GetXaxis()->SetLabelSize(0.05);
        h[j]->GetYaxis()->SetLabelSize(0.05);
        h[j]->GetYaxis()->SetTitleOffset(1.5);
        h[j]->Draw("HIST");
        gPad->Update();
        if (j == 5) {
            double u0 = gPad->GetUymin(), u1 = gPad->GetUymax();
            if (ntcut > 0) {
                TBox* bt = new TBox(ntcut - 0.5, u0, 39.5, u1);
                bt->SetFillColorAlpha(kGreen + 1, 0.2);
                bt->SetLineWidth(0);
                bt->Draw();
                TLine* lt = new TLine(ntcut - 0.5, u0, ntcut - 0.5, u1);
                lt->SetLineColor(kRed);
                lt->SetLineStyle(2);
                lt->Draw();
            }
            h[j]->Draw("HIST SAME");
            continue;
        }
        if (j == 3) {
            double u0 = gPad->GetUymin(), u1 = gPad->GetUymax();
            if (zpcut > 0 && zpcut < 800) {
                TBox* bz = new TBox(-zpcut, u0, zpcut, u1);
                bz->SetFillColorAlpha(kGreen + 1, 0.2);
                bz->SetLineWidth(0);
                bz->Draw();
                for (double xz : {-zpcut, zpcut}) {
                    TLine* lz = new TLine(xz, u0, xz, u1);
                    lz->SetLineColor(kRed);
                    lz->SetLineStyle(2);
                    lz->Draw();
                }
            }
            h[j]->Draw("HIST SAME");
            continue;
        }
        double y0 = std::pow(10.0, gPad->GetUymin()), y1 = std::pow(10.0, gPad->GetUymax());
        TBox* b = new TBox(lo[j], y0, hi[j], y1);
        b->SetFillColorAlpha(kGreen + 1, 0.2);
        b->SetLineWidth(0);
        b->Draw();
        double cutv[2] = {lo[j], hi[j]};
        for (int q = 0; q < 2; q++) {
            if (cutv[q] <= 0 || (q == 1 && !showhi[j])) continue;
            TLine* l = new TLine(cutv[q], y0, cutv[q], y1);
            l->SetLineColor(kRed);
            l->SetLineStyle(2);
            l->Draw();
        }
        h[j]->Draw("HIST SAME");
    }
}

// the two groups of the plots of the cuts, real data (top row) and Pythia8 hadronic events (bottom row), three
// plots each: the variables of the events and the first of the pairs (k0s_event: z of the primary vertex, number
// of selected tracks, distance of the helices) and the variables of the vertex of the pairs (k0s_cuts: distance
// of the vertex from the primary vertex, difference of z, chi2 of the fit)
static void cuts(const std::string& dir, const std::string& out, const Samples& S, const std::string& suffix) {
    const int nr = S.size();
    {
        TCanvas c(("kevent" + suffix).c_str(), "kevent", 1200, 350 * nr);
        c.Divide(3, nr);
        std::vector<int> p = {3, 5, 0};
        for (int r = 0; r < nr; r++) cutsrow(c, r, 3, p, dir + "/k0s_" + S[r].first + "_dist.txt", title_of(dir, S[r]).c_str());
        c.SaveAs((out + "/k0s_event" + suffix + ".pdf").c_str());
    }
    {
        TCanvas c(("kcuts" + suffix).c_str(), "kcuts", 1200, 350 * nr);
        c.Divide(3, nr);
        std::vector<int> p = {1, 2, 4};
        for (int r = 0; r < nr; r++) cutsrow(c, r, 3, p, dir + "/k0s_" + S[r].first + "_dist.txt", title_of(dir, S[r]).c_str());
        c.SaveAs((out + "/k0s_cuts" + suffix + ".pdf").c_str());
    }
}

// sum of the energies of the selected tracks of an event (pion mass), in bins of 1.5 GeV: the events with 5 or more
// selected tracks (H, blue) and with fewer (L, red), for the real data sample and the two Monte Carlo samples
static void energy(const std::string& dir, const std::string& out, const Samples& S, const std::string& suffix) {
    const int ns = S.size();
    // row 0: sum of the energies of the selected tracks (lines H and L), row 1: sum of the energies of the clusters of the
    // lead glass (lines M and N)
    const char* kh[2] = {"H", "M"};
    const char* kl[2] = {"L", "N"};
    const char* xt[2] = {"sum of the track energies, GeV", "sum of the lead glass cluster energies, GeV"};
    TCanvas c(("cen" + suffix).c_str(), "cen", 400 * ns, 800);
    c.Divide(ns, 2);
    for (int row = 0; row < 2; row++)
        for (int i = 0; i < ns; i++) {
            c.cd(row * ns + i + 1);
            gPad->SetLogy();
            gPad->SetLeftMargin(0.19);
            gPad->SetBottomMargin(0.16);
            TH1D* hh = new TH1D(Form("eh%s%d%d", suffix.c_str(), row, i), title_of(dir, S[i]).c_str(), 40, 0, 60);
            TH1D* hl = new TH1D(Form("el%s%d%d", suffix.c_str(), row, i), title_of(dir, S[i]).c_str(), 40, 0, 60);
            std::ifstream in(dir + "/k0s_" + S[i].first + "_dist.txt");
            std::string line;
            int ih = 1, il = 1;
            while (std::getline(in, line)) {
                if (line.empty()) continue;
                std::istringstream s(line);
                std::string k;
                double x, n;
                s >> k >> x >> n;
                if (k == kh[row]) hh->SetBinContent(ih++, n);
                else if (k == kl[row]) hl->SetBinContent(il++, n);
            }
            hh->SetStats(0);
            hh->SetLineColor(kBlue);
            hh->SetFillColor(kAzure - 9);
            hl->SetLineColor(kRed);
            hl->SetLineWidth(2);
            double mx = std::max(hh->GetMaximum(), hl->GetMaximum());
            hh->SetMaximum(mx * 3);
            hh->SetMinimum(0.5);
            hh->GetXaxis()->SetTitle(xt[row]);
            hh->GetYaxis()->SetTitle("events per 1.5 GeV");
            hh->GetXaxis()->SetTitleSize(0.05);
            hh->GetYaxis()->SetTitleSize(0.055);
            hh->GetXaxis()->SetLabelSize(0.05);
            hh->GetYaxis()->SetLabelSize(0.05);
            hh->GetYaxis()->SetTitleOffset(1.5);
            hh->Draw("HIST");
            hl->Draw("HIST SAME");
            TLegend* lg = new TLegend(0.45, 0.72, 0.93, 0.88);
            lg->SetBorderSize(0);
            lg->SetFillStyle(0);
            lg->AddEntry(hh, Form("5 or more tracks (%.0f events)", hh->Integral()), "f");
            lg->AddEntry(hl, Form("fewer tracks (%.0f events)", hl->Integral()), "l");
            lg->Draw();
        }
    c.SaveAs((out + "/k0s_energy" + suffix + ".pdf").c_str());
}

void k0s_plots(const char* dir = "../../buildGNU64/test", const char* out = "Figures") {
    gStyle->SetOptStat(0);
    // the real data sample and the hadronic Monte Carlo (Pythia8)
    Samples A;
    A.push_back(std::make_pair("data_1", "real data sample"));
    A.push_back(std::make_pair("mc_1", "Pythia8 hadronic events, 34.6 GeV"));
    A.push_back(std::make_pair("mc_3", "Pythia8 hadronic events, 43.8 GeV"));
    mass(dir, out, A, "");
    energy(dir, out, A, "");
    // the cuts: the real data sample and the Pythia8 events at 43.8 GeV
    Samples A2;
    A2.push_back(A[0]);
    A2.push_back(A[2]);
    cuts(dir, out, A2, "");
    // the simulated muon pairs, pion pairs and K0S pairs (dummy generator)
    Samples B;
    B.push_back(std::make_pair("mc_4", "Pythia8 #mu^{+}#mu^{-}, 34.6 GeV"));
    B.push_back(std::make_pair("mc_5", "e^{+}e^{-} #rightarrow #pi^{+}#pi^{-}, 34.6 GeV"));
    B.push_back(std::make_pair("mc_6", "e^{+}e^{-} #rightarrow K^{0}_{S}K^{0}_{S}, 14 GeV"));
    mass(dir, out, B, "_lept");
    energy(dir, out, B, "_lept");
    cuts(dir, out, B, "_lept");
}
