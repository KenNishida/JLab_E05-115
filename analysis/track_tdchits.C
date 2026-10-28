#include <TFile.h>
#include <TTree.h>
#include <TH1D.h>
#include <TH2D.h>
#include <TCanvas.h>
#include <TLegend.h>
#include <TPad.h>
#include <TString.h>
#include <TStyle.h>

#include <array>
#include <map>
#include <tuple>
#include <vector>
#include <cmath>
#include <algorithm>
#include <iostream>

using namespace std;

void track_hit_analysis(const char* stagefile="hstage76312.root",
                        const char* rawfile="hraw76312.root")
{
    TString stagepath=Form("../replay/e05115replay/rootfiles/%s",stagefile);
    TFile* fs=TFile::Open(stagepath,"READ");
    if (!fs || fs->IsZombie()) {
        cerr << "Cannot open " << stagepath << endl;
        return;
    }
    TTree* hits=nullptr;
    fs->GetObject("h9042",hits);
    if (!hits) {
        cerr << "h9042 is missing in " << stagepath << endl;
        fs->Close();
        return;
    }
    for (const char* name : {"run","event","track","layer","tdc","chi2dof","resid"}) {
        if (!hits->GetBranch(name)) {
            cerr << "Missing h9042 branch: " << name << endl;
            fs->Close();
            return;
        }
    }

    TString rawpath=Form("../replay/e05115replay/rootfiles/%s",rawfile);
    TFile* fr=TFile::Open(rawpath,"READ");
    TTree* raw=nullptr;
    if (fr && !fr->IsZombie()) fr->GetObject("h9031",raw);
    if (!raw) cout << "No h9031: drawing adopted track hits only." << endl;

    TH1::AddDirectory(kFALSE);
    const int nbins=130;
    const double xmin=2000.0, xmax=3300.0;
    const double halfRangeNs=600.0;
    const double nsPerChannel=0.5;

    TH1D* h_track[12];
    TH1D* h_dec[12];
    TH1D* h_diff[12]={nullptr};
    for (int i=0;i<12;++i) {
        h_track[i]=new TH1D(Form("h_track_tdc%02d",i+1),
            Form("Plane %d;TDC [ch];Normalized counts",i+1),nbins,xmin,xmax);
        h_dec[i]=new TH1D(Form("h_dec_tdc%02d",i+1),
            Form("Plane %d;TDC [ch];Normalized counts",i+1),nbins,xmin,xmax);
        h_track[i]->SetLineColor(kBlue+1);
        h_dec[i]->SetLineColor(kGray+2);
        h_track[i]->SetLineWidth(2);
        h_dec[i]->SetLineWidth(2);
        h_track[i]->SetStats(0);
        h_dec[i]->SetStats(0);
        if (i>0) {
            h_diff[i]=new TH1D(Form("h_dtdc_p%02d",i+1),
                Form("Plane %d - Plane 1;#Delta t [ns];Track-hit pairs",i+1),
                240,-halfRangeNs,halfRangeNs);
            h_diff[i]->SetLineWidth(2);
        }
    }
    TH1D* h_alltrack=new TH1D("h_alltrack_tdc",
        "All planes;TDC [ch];Normalized counts",nbins,xmin,xmax);
    TH1D* h_alldec=new TH1D("h_alldec_tdc",
        "All planes;TDC [ch];Normalized counts",nbins,xmin,xmax);
    h_alltrack->SetLineColor(kBlue+1);
    h_alldec->SetLineColor(kGray+2);
    h_alltrack->SetLineWidth(2);
    h_alldec->SetLineWidth(2);
    h_alltrack->SetStats(0);
    h_alldec->SetStats(0);
    TH2D* h_map=new TH2D("h_dtdc_map",
        "FP-track TDC differences;Plane;#Delta t [ns]",
        11,1.5,12.5,240,-halfRangeNs,halfRangeNs);

    Float_t run=0,event=0,track=0,layer=0,tdc=0,chi2dof=0,resid=0;
    hits->SetBranchAddress("run",&run);
    hits->SetBranchAddress("event",&event);
    hits->SetBranchAddress("track",&track);
    hits->SetBranchAddress("layer",&layer);
    hits->SetBranchAddress("tdc",&tdc);
    hits->SetBranchAddress("chi2dof",&chi2dof);
    hits->SetBranchAddress("resid",&resid);

    using Key=tuple<int,int,int>; // (run, event, FP-track number)
    map<Key,array<vector<double>,12>> trackTDC;
    Long64_t nused=0;
    for (Long64_t i=0;i<hits->GetEntries();++i) {
        hits->GetEntry(i);
        const int pl=(int)layer;
        if (pl<1 || pl>12 || !std::isfinite(tdc) || tdc<=0) continue;

        // ===== TRACK / HIT QUALITY CUTS: edit these lines directly =====
        // if (chi2dof > 10.0) continue;   // FP-track chi2/ndf cut
        // if (std::fabs(resid) > 1.0) continue; // per-plane residual cut (check units)
        // if ((int)track != 1) continue; // only the first FP track candidate
        // ==============================================================

        h_track[pl-1]->Fill(tdc);
        h_alltrack->Fill(tdc);
        trackTDC[make_tuple((int)run,(int)event,(int)track)][pl-1].push_back(tdc);
        ++nused;
    }

    Long64_t ndecoded=0;
    if (raw) {
        Float_t rlayer=0,rtdc=0,accepted=0;
        raw->SetBranchAddress("layer",&rlayer);
        raw->SetBranchAddress("tdcused",&rtdc);
        raw->SetBranchAddress("accepted",&accepted);
        for (Long64_t i=0;i<raw->GetEntries();++i) {
            raw->GetEntry(i);
            const int pl=(int)rlayer;
            if (pl<1 || pl>12 || accepted!=1 || !std::isfinite(rtdc) || rtdc<=0) continue;
            h_dec[pl-1]->Fill(rtdc);
            h_alldec->Fill(rtdc);
            ++ndecoded;
        }
    }

    Long64_t noRef=0,multiRef=0,withRef=0,pairs=0;
    for (const auto& item : trackTDC) {
        const auto& values=item.second;
        if (values[0].empty()) { ++noRef; continue; }
        if (values[0].size()!=1) { ++multiRef; continue; }
        ++withRef;
        const double ref=values[0][0];
        for (int pl=2;pl<=12;++pl) {
            for (double t : values[pl-1]) {
                const double dt=(t-ref)*nsPerChannel;
                h_diff[pl-1]->Fill(dt);
                h_map->Fill(pl,dt);
                ++pairs;
            }
        }
    }

    cout << "Adopted track hits: " << nused << ", decoded hits: " << ndecoded << endl;
    cout << "FP tracks: " << trackTDC.size()
         << ", unique Plane-1 hit: " << withRef
         << ", no Plane-1 hit: " << noRef
         << ", multiple Plane-1 hits (skipped): " << multiRef << endl;
    cout << "Plane-1 / downstream hit pairs: " << pairs << endl;
    for (int i=0;i<12;++i) {
        cout << "Plane " << i+1 << ": adopted hits=" << (Long64_t)h_track[i]->GetEntries()
             << ", decoded hits=" << (Long64_t)h_dec[i]->GetEntries();
        if (i>0) cout << ", delta-TDC pairs=" << (Long64_t)h_diff[i]->GetEntries();
        cout << endl;
        if (h_track[i]->Integral()>0) h_track[i]->Scale(1.0/h_track[i]->Integral());
        if (h_dec[i]->Integral()>0) h_dec[i]->Scale(1.0/h_dec[i]->Integral());
    }
    if (h_alltrack->Integral()>0) h_alltrack->Scale(1.0/h_alltrack->Integral());
    if (h_alldec->Integral()>0) h_alldec->Scale(1.0/h_alldec->Integral());

    TCanvas* c1=new TCanvas("c_track_tdc","Adopted-hit TDC by plane",1200,1200);
    c1->Divide(3,4);
    for (int i=0;i<12;++i) {
        c1->cd(i+1);
        TH1D* base=raw ? h_dec[i] : h_track[i];
        base->SetMaximum(1.15*max(h_dec[i]->GetMaximum(),h_track[i]->GetMaximum())+1.e-4);
        base->Draw("HIST");
        if (raw) h_track[i]->Draw("HIST SAME");
        if (i==0 && raw) {
            TLegend* leg=new TLegend(0.43,0.72,0.88,0.88);
            leg->AddEntry(h_dec[i],"Decoded hits (all)","l");
            leg->AddEntry(h_track[i],"FP-track hits (cut)","l");
            leg->Draw();
        }
    }

    TCanvas* c2=new TCanvas("c_track_tdc_all","Adopted-hit TDC: all planes",800,500);
    TH1D* base=raw ? h_alldec : h_alltrack;
    base->SetMaximum(1.15*max(h_alldec->GetMaximum(),h_alltrack->GetMaximum())+1.e-4);
    base->Draw("HIST");
    if (raw) {
        h_alltrack->Draw("HIST SAME");
        TLegend* leg=new TLegend(0.62,0.72,0.88,0.88);
        leg->AddEntry(h_alldec,"Decoded hits (all)","l");
        leg->AddEntry(h_alltrack,"FP-track hits (cut)","l");
        leg->Draw();
    }

    TCanvas* c3=new TCanvas("c_track_tdc_diff","TDC difference from Plane 1",1400,1100);
    c3->Divide(3,4);
    for (int pl=2;pl<=12;++pl) {
        c3->cd(pl-1);
        h_diff[pl-1]->Draw("HIST");
    }
    c3->cd(12);
    gPad->SetRightMargin(0.18);
    gStyle->SetPalette(kBird);
    h_map->Draw("COLZ");

    fs->Close();
    if (fr) fr->Close();
}
