#include <TFile.h>
#include <TTree.h>
#include <TH1D.h>
#include <TH2D.h>
#include <TCanvas.h>
#include <TPad.h>
#include <TString.h>
#include <TStyle.h>

#include <map>
#include <tuple>
#include <vector>
#include <cmath>
#include <algorithm>
#include <iostream>

#include "analysis_style.h"

using namespace std;

void wire_multiplicity(const char* filename="hraw76312.root", int plane=2, int sampleEvent=1128)
{
    TString filepath = Form("../replay/e05115replay/rootfiles/%s", filename);
    TFile* file = TFile::Open(filepath.Data(), "READ");
    if (!file || file->IsZombie()) {
        cerr << "Cannot open " << filepath << endl;
        return;
    }
    TTree *hits = nullptr, *events = nullptr;
    file->GetObject("h9031", hits);
    file->GetObject("h9030", events);
    if (!hits || !events) {
        cerr << "Missing h9030 or h9031" << endl;
        file->Close();
        return;
    }

    Float_t run=0, event=0, layer=0, wire=0, tdc=0, inwin=0;
    hits->SetBranchAddress("run", &run);
    hits->SetBranchAddress("event", &event);
    hits->SetBranchAddress("layer", &layer);
    hits->SetBranchAddress("wire", &wire);
    hits->SetBranchAddress("tdc", &tdc);
    hits->SetBranchAddress("inwin", &inwin);

    const int nwire = 122;
    const Long64_t nevents = events->GetEntries();
    TH1::AddDirectory(kFALSE);

    TH1D *h_totrawhits = new TH1D("h_totrawhits", Form("Plane %d: Total raw hits",plane), nwire,0,nwire);
    TH1D *h_planerate = new TH1D("h_planerate", "Plane hit rates", 12,1,13);
    TH1D *h_wirerate[12];
    for (int i=0; i<12; ++i) {
        h_wirerate[i] = new TH1D(Form("h_wirerate%02d",i+1),Form("Plane %d",i+1),nwire,0,nwire);
        SetHist1D(h_wirerate[i], "Wire", "Single hit rate [kHz]");
    }
     
    SetHist1D(h_totrawhits, "Wire", "Number of raw hits");
    SetHist1D(h_planerate, "Plane", "Single hit rate [MHz]");

    map<tuple<int,int,int>,vector<int>> wireTDC;
    map<tuple<int,int,int>,vector<int>> wireTDCinwin;
    const Long64_t nentries = hits->GetEntries();
    for (Long64_t i=0; i<nentries; ++i) {
        hits->GetEntry(i);

        if (tdc>=2100 && tdc<2400 && layer>=1 && layer<=12) {
            h_planerate->Fill(layer);
            h_wirerate[(int)layer-1]->Fill(wire);
        }

        if ((int)layer!=plane) continue;
        const int r = (int)run;
        const int e = (int)event;
        const int w = (int)wire;
        wireTDC[make_tuple(r,e,w)].push_back((int)tdc);
        if (inwin == 1) wireTDCinwin[make_tuple(r,e,w)].push_back((int)tdc);
        h_totrawhits->Fill(w);
    }

    if (nevents>0) {
        h_planerate->Scale(1.0/(nevents*150.e-9*1.e6));
        for (int i=0; i<12; ++i) {
            h_wirerate[i]->Scale(1.0/(nevents*150.e-9*1.e3));
        }
    }

    TH1D *h_wiremulti = new TH1D("h_wiremulti", Form("Plane %d: Raw multiplicity of single wire",plane), 6,0.5,6.5);
    TH2D *h_wire_wiremulti = new TH2D("h_wire_wiremulti", Form("Plane %d: Raw multiplicity of single wire",plane), nwire,0,nwire,6,0.5,6.5);
    TH1D *h_deltaTDC = new TH1D("h_deltaTDC", Form("Plane %d: #DeltaTDC of multi-hits on single wire",plane), 100,0.0,1000.0);
    TH1D *h_deltaTDC_inwin = new TH1D("h_deltaTDC_inwin", Form("Plane %d: #DeltaTDC of multi-hits on single wire",plane), 100,0.0,1000.0);
    TH2D *h_wire_deltaTDC = new TH2D("h_wire_deltaTDC", Form("Plane %d: #DeltaTDC vs wire",plane), nwire,0,nwire,100,0.0,1000.0);

    SetHist1D(h_wiremulti, "Raw multiplicity of single wire", "Number of (event, wire) groups");
    SetHist2D(h_wire_wiremulti, "Wire", "Raw multiplicity of single wire");
    SetHist1D(h_deltaTDC, "#DeltaTDC [ch]", "Counts");
    SetHist1D(h_deltaTDC_inwin, "#DeltaTDC [ch]", "Counts",kRed);
    SetHist2D(h_wire_deltaTDC, "Wire", "#DeltaTDC [ch]");

    for (const auto& item : wireTDC) {
        const int r = get<0>(item.first);
        const int e = get<1>(item.first);
        const int w = get<2>(item.first);
        const vector<int>& values = item.second;
        const int m = (int)values.size();
        h_wiremulti->Fill(m);
        h_wire_wiremulti->Fill(w,m);
        if (m<2) continue;

        for (size_t i=1; i<values.size(); ++i) {
            const int delta = values[i]-values[i-1];
            h_deltaTDC->Fill(delta);
            h_wire_deltaTDC->Fill(w,delta);
        }
    }
    
    for (const auto& item : wireTDCinwin) {
        const vector<int>& values = item.second;
        for (size_t i=1; i<values.size(); ++i) {
            const int delta = values[i]-values[i-1];
            h_deltaTDC_inwin->Fill(delta);
        }
    }

    TCanvas *c1 = new TCanvas("c1","c1",800,800);
    c1->Divide(2,2);
    gStyle->SetPalette(kRainbow);
    c1->cd(1); h_wire_wiremulti->Draw("COLZ");
    c1->cd(2); h_wiremulti->Draw("HIST");
    c1->cd(3); h_totrawhits->Draw("HIST");
    c1->cd(4); h_deltaTDC->Draw("HIST"); h_deltaTDC_inwin->Draw("HIST SAME");

    TCanvas *c2 = new TCanvas("c2","Plane hit rates",1600,1600);
    c2->Divide(4,4);
    c2->cd(1); h_planerate->Draw("HIST");
    for (int i=0; i<12; ++i) {
        c2->cd(i+5); h_wirerate[i]->Draw("HIST");
    }

    file->Close();
}
