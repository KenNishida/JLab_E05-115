// compare_HKS_tracking.C

#include "analysis_style.h"
#include <TFile.h>
#include <TTree.h>
#include <TH1D.h>
#include <TProfile.h>
#include <TCanvas.h>
#include <TLegend.h>
#include <TStyle.h>
#include <TString.h>
#include <TLine.h>

#include <vector>
#include <iostream>

using namespace std;

struct RunInfo {int run; TString label; int color;};

void Normalize(TH1 *h)
{
    double integral = h->Integral();
    if (integral > 0) h->Scale(1.0/integral);
}

TLegend* MakeLegend()
{
    TLegend *leg = new TLegend(0.70, 0.60, 0.89, 0.80);
    leg->SetTextSize(0.025);
    leg->SetBorderSize(1);
    leg->SetFillColor(kWhite);
    leg->SetFillStyle(1001);

    return leg;
}

void compare_tracking()
{
    gStyle->SetOptStat(0);

    vector<RunInfo> runs = {
        {76312, "CH2 2.0 uA",  kBlack},
        {75898, "H2O 2.8 uA",  kBlue+1},
        {75919, "12C 10 uA",   kGreen+2},
        {76830, "12C 20 uA",   kOrange+7},
        {75953, "12C 35 uA",   kRed+1},
        {76700, "52Cr 8.0 uA", kMagenta+1}
    };

    const int NR = runs.size();

    vector<TFile*> f(NR);
    vector<TTree*> T(NR);

    for (int ir=0; ir<NR; ir++) {

        TString fname = Form("../rootfiles/hks%d.root", runs[ir].run);

        f[ir] = TFile::Open(fname);

        if (!f[ir] || f[ir]->IsZombie()) {
            cerr << "Cannot open " << fname << endl;
            return;
        }

        T[ir] = (TTree*)f[ir]->Get("h9020");

        if (!T[ir]) {
            cerr << "Cannot find h9020 in " << fname << endl;
            return;
        }

        cout << runs[ir].run
             << " : entries = "
             << T[ir]->GetEntries()
             << endl;
    }


    // ============================================================
    // 1. chi2 / ndf
    // ============================================================
    vector<TH1D*> hchi(NR);
    for (int ir=0; ir<NR; ir++) {
        hchi[ir] = new TH1D(Form("hchi_%d",runs[ir].run),"chi2/ndf",500,0,20);
        SetHist1D(hchi[ir], "chi2/ndf", "Normalized counts", runs[ir].color);

        T[ir]->Draw(Form("trkchi2>>hchi_%d",runs[ir].run),"","goff");

        Normalize(hchi[ir]);
    }

    TLegend *leg1 = MakeLegend();

    for(int ir=0;ir<NR;ir++)
        leg1->AddEntry(hchi[ir],runs[ir].label,"l");

    
    // ============================================================
    // 2. Hit multiplicity
    // ============================================================
    vector<TH1D*> hmulti(NR);
    for(int ir=0;ir<NR;ir++) {
        hmulti[ir] = new TH1D(Form("hmulti_%d",runs[ir].run),"Multiplicity",10,-0.5,9.5);
        SetHist1D(hmulti[ir], "Multiplicity", "Normalized counts", runs[ir].color);

        for(int il=1;il<=12;il++) {
            T[ir]->Draw(Form("multi%d>>+hmulti_%d",il,runs[ir].run),"","goff");
        }

        Normalize(hmulti[ir]);
    }

    TLegend *leg2 = MakeLegend();

    for(int ir=0;ir<NR;ir++)
        leg2->AddEntry(hmulti[ir],runs[ir].label,"l");


    // ============================================================
    // 3. Single-hit residual
   // ============================================================
    vector<TH1D*> hres(NR);
    for(int ir=0;ir<NR;ir++) {
        hres[ir] = new TH1D(Form("hres_%d",runs[ir].run),"Single-hit residual",500,-0.3,0.3);
        SetHist1D(hres[ir], "Single-hit residual [cm]", "Normalized counts", runs[ir].color);

        for(int il=1;il<=12;il++) {
            T[ir]->Draw(Form("sngres%d>>+hres_%d", il, runs[ir].run),
                        Form("dtime%d!=-1000",il), "goff");
        }

        Normalize(hres[ir]);
    }

    TLegend *leg3 = MakeLegend();
    for(int ir=0;ir<NR;ir++)
        leg3->AddEntry(hres[ir],runs[ir].label,"l");


    // ============================================================
    // 4. x-t curve
    // ============================================================
    vector<TGraph*> gxt(NR);
    for(int ir=0; ir<NR; ir++) {
        vector<double> vx;
        vector<double> vy;

        T[ir]->SetEstimate(T[ir]->GetEntries() + 1);

        for(int il=1; il<=12; il++) {
            Long64_t n = T[ir]->Draw(
                Form("ddist%d:dtime%d", il, il),
                Form("dtime%d!=-1000 && ddist%d!=-1000", il, il),
                "goff"
            );

            double *y = T[ir]->GetV1();
            double *x = T[ir]->GetV2();

            for(Long64_t i=0; i<n; i++) {
                vx.push_back(x[i]);
                vy.push_back(y[i]);
            }
        }

        gxt[ir] = new TGraph(vx.size(), vx.data(), vy.data());
        SetPlotGraph(gxt[ir],Form("X-T curve of %s", runs[ir].label.Data()),"Drift time [ns]","Drift distance [cm]",runs[ir].color);

        gxt[ir]->GetXaxis()->SetLimits(-10, 125);
        gxt[ir]->GetYaxis()->SetLimits(0.0, 0.55);
    }

    TLegend *leg4 = MakeLegend();
    for(int ir=0; ir<NR; ir++)
        leg4->AddEntry(gxt[ir], runs[ir].label, "p");

    // ============================================================
    // 5. hit coordinate - wire center
    // ============================================================
    vector<TH1D*> hwire(NR);
    for(int ir=0;ir<NR;ir++) {
        hwire[ir] = new TH1D(Form("hwire_%d",runs[ir].run),"Hit coordinate",400,-0.6,0.6);
        SetHist1D(hwire[ir], "Hit coordinate - wire center [cm]", "Normalized counts", runs[ir].color);

        for(int il=1;il<=12;il++) {
            T[ir]->Draw(Form("(hwcoor%d-hwcent%d)>>+hwire_%d",il,il,runs[ir].run),
                        Form("dtime%d!=-1000",il), "goff");
        }

        Normalize(hwire[ir]);
    }

    TLegend *leg5 = MakeLegend();
    for(int ir=0;ir<NR;ir++)
        leg5->AddEntry(hwire[ir],runs[ir].label,"l");

    
    // ============================================================
    // Draw hisgtograms
    // ============================================================
    TCanvas *c1 = new TCanvas("c1","Tracking quality",800,800);
    c1->Divide(2,2);

    c1->cd(1);
    double ymax=0; for(auto h:hchi) ymax=max(ymax,h->GetMaximum());
    hchi[0]->SetMaximum(ymax*1.1);
    hchi[0]->Draw("hist");
    for(int ir=1;ir<NR;ir++)
        hchi[ir]->Draw("hist same");
    leg1->Draw();

    c1->cd(2); c1->SetLogy();
    ymax=0; for(auto h:hmulti) ymax=max(ymax,h->GetMaximum());
    hmulti[0]->SetMaximum(ymax*1.1);
    hmulti[0]->Draw("hist");
    for(int ir=1;ir<NR;ir++)
        hmulti[ir]->Draw("hist same");
    leg2->Draw();

    c1->cd(3); 
    ymax=0; for(auto h:hres) ymax=max(ymax,h->GetMaximum());
    hres[0]->SetMaximum(ymax*1.1);
    hres[0]->Draw("hist");
    for(int ir=1;ir<NR;ir++)
        hres[ir]->Draw("hist same");
    leg3->Draw();
    
    c1->cd(4);
    ymax=0; for(auto h:hwire) ymax=max(ymax,h->GetMaximum());
    hwire[0]->SetMaximum(ymax*0.15);
    hwire[0]->Draw("hist");
    for(int ir=1;ir<NR;ir++)
        hwire[ir]->Draw("hist same");
    leg5->Draw();

    TCanvas *c2 = new TCanvas("c2","x-t curve",800,1200);
    c2->Divide(2,3);

    for(int ir=0; ir<NR; ir++) {
        c2->cd(ir+1);
        gxt[ir]->Draw("AP");
    }
}