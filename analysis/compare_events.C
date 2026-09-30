#include "analysis_style.h"
#include <TFile.h>
#include <TTree.h>
#include <TH1D.h>
#include <TCanvas.h>
#include <TPaveText.h>
#include <fstream>
#include <iostream>
#include <cmath>

using namespace std;

static Double_t CalcF2T(const Double_t *P, Double_t xf, Double_t xpf, Double_t yf, Double_t ypf, Int_t nMat)
{
    Double_t Y=0.0;
    Int_t npar=0;
    for(Int_t n=0;n<=nMat;n++)
        for(Int_t d=0;d<=n;d++)
            for(Int_t c=0;c<=n;c++)
                for(Int_t b=0;b<=n;b++)
                    for(Int_t a=0;a<=n;a++)
                        if(a+b+c+d==n)
                            Y+=pow(xf,a)*pow(xpf,b)*pow(yf,c)*pow(ypf,d)*P[npar++];
    return Y;
}

static Bool_t ReadMatrix(const char *filename, Double_t *P)
{
    ifstream fin(filename);
    if(!fin){ cerr<<"ERROR: cannot open optics matrix: "<<filename<<endl; return kFALSE; }

    for(Int_t i=0;i<210;i++){
        Int_t a,b,c,d;
        fin>>P[i]>>a>>b>>c>>d;
        if(!fin){ cerr<<"ERROR: failed reading "<<filename<<" at coefficient "<<i<<endl; return kFALSE; }
    }
    return kTRUE;
}

void compare_events()
{
    const Int_t targetRun=76312;

    TFile *fc=TFile::Open("../rootfiles_gogami/ch2_9-3.root","READ");
    TFile *fr=TFile::Open("../rootfiles/ch2.root","READ");
    if(!fc || fc->IsZombie()){ cerr<<"Cannot open condensed ROOT file"<<endl; return; }
    if(!fr || fr->IsZombie()){ cerr<<"Cannot open replayed ROOT file"<<endl; return; }

    TTree *tc=(TTree*)fc->Get("h9500");
    TTree *tr=(TTree*)fr->Get("h9500");
    if(!tc || !tr){ cerr<<"Cannot find h9500"<<endl; return; }

    // ===== condensed branches =====
    Float_t crun,cevent;
    Float_t chsp,chsbeta,chac1,chac2,chac3,chwc1,chwc2;
    Float_t ckmom,ckxpt,ckypt,chtimetar,chstimefp,chsxfp,chsyfp,chsxpfp,chsypfp;
    Float_t cemom,cexpt,ceypt,cetimetar,cestimefp,cesxfp,cesyfp,cesxpfp,cesypfp;

    tc->SetBranchAddress("runnum",&crun);
    tc->SetBranchAddress("eventid",&cevent);
    tc->SetBranchAddress("hsp",&chsp);
    tc->SetBranchAddress("hsbeta",&chsbeta);
    tc->SetBranchAddress("haernpe1",&chac1);
    tc->SetBranchAddress("haernpe2",&chac2);
    tc->SetBranchAddress("haernpe3",&chac3);
    tc->SetBranchAddress("hwatnpe1",&chwc1);
    tc->SetBranchAddress("hwatnpe2",&chwc2);
    tc->SetBranchAddress("kmom",&ckmom);
    tc->SetBranchAddress("kxpt",&ckxpt);
    tc->SetBranchAddress("kypt",&ckypt);
    tc->SetBranchAddress("htimetar",&chtimetar);
    tc->SetBranchAddress("hstimefp",&chstimefp);
    tc->SetBranchAddress("hsxfp",&chsxfp);
    tc->SetBranchAddress("hsyfp",&chsyfp);
    tc->SetBranchAddress("hsxpfp",&chsxpfp);
    tc->SetBranchAddress("hsypfp",&chsypfp);
    tc->SetBranchAddress("emom",&cemom);
    tc->SetBranchAddress("expt",&cexpt);
    tc->SetBranchAddress("eypt",&ceypt);
    tc->SetBranchAddress("etimetar",&cetimetar);
    tc->SetBranchAddress("estimefp",&cestimefp);
    tc->SetBranchAddress("esxfp",&cesxfp);
    tc->SetBranchAddress("esyfp",&cesyfp);
    tc->SetBranchAddress("esxpfp",&cesxpfp);
    tc->SetBranchAddress("esypfp",&cesypfp);

    // ===== replayed branches =====
    Float_t rrun,revent;
    Float_t rhsp,rhsbeta,rhac1,rhac2,rhac3,rhwc1,rhwc2;
    Float_t rhtimetar,rhstimefp,rhsxfp,rhsyfp,rhsxpfp,rhsypfp;
    Float_t retimetar,restimefp,resxfp,resyfp,resxpfp,resypfp;

    tr->SetBranchAddress("runnum",&rrun);
    tr->SetBranchAddress("eventid",&revent);
    tr->SetBranchAddress("hsp",&rhsp);
    tr->SetBranchAddress("hsbeta",&rhsbeta);
    tr->SetBranchAddress("haernpe1",&rhac1);
    tr->SetBranchAddress("haernpe2",&rhac2);
    tr->SetBranchAddress("haernpe3",&rhac3);
    tr->SetBranchAddress("hwatnpe1",&rhwc1);
    tr->SetBranchAddress("hwatnpe2",&rhwc2);
    tr->SetBranchAddress("htimetar",&rhtimetar);
    tr->SetBranchAddress("hstimefp",&rhstimefp);
    tr->SetBranchAddress("hsxfp",&rhsxfp);
    tr->SetBranchAddress("hsyfp",&rhsyfp);
    tr->SetBranchAddress("hsxpfp",&rhsxpfp);
    tr->SetBranchAddress("hsypfp",&rhsypfp);
    tr->SetBranchAddress("etimetar",&retimetar);
    tr->SetBranchAddress("estimefp",&restimefp);
    tr->SetBranchAddress("esxfp",&resxfp);
    tr->SetBranchAddress("esyfp",&resyfp);
    tr->SetBranchAddress("esxpfp",&resxpfp);
    tr->SetBranchAddress("esypfp",&resypfp);

    // ===== optics matrices =====
    Double_t momK[210]={0},xptK[210]={0},yptK[210]={0};
    Double_t momE[210]={0},xptE[210]={0},yptE[210]={0};
    char smomK[256],sxptK[256],syptK[256],smomE[256],sxptE[256],syptE[256];

    ifstream recon("matrices/reconData.dat");
    if(!recon){ cerr<<"Cannot open matrices/reconData.dat"<<endl; return; }
    recon>>smomK>>sxptK>>syptK>>smomE>>sxptE>>syptE;
    recon.close();

    if(!ReadMatrix(smomK,momK)) return;
    if(!ReadMatrix(sxptK,xptK)) return;
    if(!ReadMatrix(syptK,yptK)) return;
    if(!ReadMatrix(smomE,momE)) return;
    if(!ReadMatrix(sxptE,xptE)) return;
    if(!ReadMatrix(syptE,yptE)) return;

    tr->BuildIndex("runnum","eventid");

    // ===== original histogram range widths =====
    const Double_t range_m2       = 1.0;
    const Double_t range_ctime    = 15.0;
    const Double_t range_kacnpe   = 30.0;
    const Double_t range_kwcnpe   = 5.0;

    const Double_t range_kmom     = 0.5;
    const Double_t range_ktimetar = 50.0;
    const Double_t range_ktimefp  = 50.0;
    const Double_t range_kxptar   = 0.2;
    const Double_t range_kyptar   = 0.2;
    const Double_t range_kxfp     = 100.0;
    const Double_t range_kyfp     = 20.0;
    const Double_t range_kxpfp    = 0.2;
    const Double_t range_kypfp    = 0.2;

    const Double_t range_emom     = 0.5;
    const Double_t range_etimetar = 100.0;
    const Double_t range_etimefp  = 50.0;
    const Double_t range_exptar   = 0.2;
    const Double_t range_eyptar   = 0.2;
    const Double_t range_exfp     = 100.0;
    const Double_t range_eyfp     = 20.0;
    const Double_t range_expfp    = 0.2;
    const Double_t range_eypfp    = 0.2;

    // ===== difference histograms: C - R =====
    TH1D *h_m2      =new TH1D("h_m2","Mass squared",500,-range_m2,range_m2);
    TH1D *h_ctime   =new TH1D("h_ctime","Coincidence time",500,-range_ctime,range_ctime);
    TH1D *h_kacnpe  =new TH1D("h_kacnpe","Kaon AC NPE",500,-range_kacnpe,range_kacnpe);
    TH1D *h_kwcnpe  =new TH1D("h_kwcnpe","Kaon WC NPE",500,-range_kwcnpe,range_kwcnpe);

    TH1D *h_kmom    =new TH1D("h_kmom","Kaon momentum",500,-range_kmom,range_kmom);
    TH1D *h_ktimetar=new TH1D("h_ktimetar","Kaon time at target",500,-range_ktimetar,range_ktimetar);
    TH1D *h_ktimefp =new TH1D("h_ktimefp","Kaon time at focal plane",500,-range_ktimefp,range_ktimefp);
    TH1D *h_kxptar  =new TH1D("h_kxptar","Kaon x' at target",500,-range_kxptar,range_kxptar);
    TH1D *h_kyptar  =new TH1D("h_kyptar","Kaon y' at target",500,-range_kyptar,range_kyptar);
    TH1D *h_kxfp    =new TH1D("h_kxfp","Kaon x at focal plane",500,-range_kxfp,range_kxfp);
    TH1D *h_kyfp    =new TH1D("h_kyfp","Kaon y at focal plane",500,-range_kyfp,range_kyfp);
    TH1D *h_kxpfp   =new TH1D("h_kxpfp","Kaon x' at focal plane",500,-range_kxpfp,range_kxpfp);
    TH1D *h_kypfp   =new TH1D("h_kypfp","Kaon y' at focal plane",500,-range_kypfp,range_kypfp);

    TH1D *h_emom    =new TH1D("h_emom","Electron momentum",500,-range_emom,range_emom);
    TH1D *h_etimetar=new TH1D("h_etimetar","Electron time at target",500,-range_etimetar,range_etimetar);
    TH1D *h_etimefp =new TH1D("h_etimefp","Electron time at focal plane",500,-range_etimefp,range_etimefp);
    TH1D *h_exptar  =new TH1D("h_exptar","Electron x' at target",500,-range_exptar,range_exptar);
    TH1D *h_eyptar  =new TH1D("h_eyptar","Electron y' at target",500,-range_eyptar,range_eyptar);
    TH1D *h_exfp    =new TH1D("h_exfp","Electron x at focal plane",500,-range_exfp,range_exfp);
    TH1D *h_eyfp    =new TH1D("h_eyfp","Electron y at focal plane",500,-range_eyfp,range_eyfp);
    TH1D *h_expfp   =new TH1D("h_expfp","Electron x' at focal plane",500,-range_expfp,range_expfp);
    TH1D *h_eypfp   =new TH1D("h_eypfp","Electron y' at focal plane",500,-range_eypfp,range_eypfp);

TH1D *h[]={
    h_m2,h_ctime,h_kacnpe,h_kwcnpe,
    h_kmom,h_ktimetar,h_ktimefp,h_kxptar,h_kyptar,h_kxfp,h_kyfp,h_kxpfp,h_kypfp,
    h_emom,h_etimetar,h_etimefp,h_exptar,h_eyptar,h_exfp,h_eyfp,h_expfp,h_eypfp
};

Double_t range[]={
    range_m2,range_ctime,range_kacnpe,range_kwcnpe,
    range_kmom,range_ktimetar,range_ktimefp,range_kxptar,range_kyptar,
    range_kxfp,range_kyfp,range_kxpfp,range_kypfp,
    range_emom,range_etimetar,range_etimefp,range_exptar,range_eyptar,
    range_exfp,range_eyfp,range_expfp,range_eypfp
};

const char *xtitle[]={
    "GeV^{2}/c^{4}", // m2
    "ns",            // ctime
    "NPE",           // AC
    "NPE",           // WC

    "GeV/c",         // kmom
    "ns",            // ktimetar
    "ns",            // ktimefp
    "mrad",           // kxptar
    "mrad",           // kyptar
    "cm",            // kxfp
    "cm",            // kyfp
    "mrad",           // kxpfp
    "mrad",           // kypfp

    "GeV/c",         // emom
    "ns",            // etimetar
    "ns",            // etimefp
    "mrad",           // exptar
    "mrad",           // eyptar
    "cm",            // exfp
    "cm",            // eyfp
    "mrad",           // expfp
    "mrad"            // eypfp
};

const Int_t NH=sizeof(h)/sizeof(h[0]);
for(Int_t i=0;i<NH;i++) SetHist1D(h[i],"Difference ["+string(xtitle[i])+"]","Counts");

    Long64_t nSame[NH]={0};
    Long64_t nValid[NH]={0};
    Long64_t nCondensed=0,nMatched=0,nNotFound=0;

    // ===== event loop =====
    for(Long64_t i=0;i<tc->GetEntries();i++){
        tc->GetEntry(i);
        if((Int_t)crun!=targetRun) continue;
        nCondensed++;

        Long64_t j=tr->GetEntryNumberWithIndex((Long64_t)crun,(Long64_t)cevent);
        if(j<0){ nNotFound++; continue; }

        tr->GetEntry(j);
        nMatched++;

        Double_t c_m2=chsp*chsp*(1.0/(chsbeta*chsbeta)-1.0);
        Double_t r_m2=rhsp*rhsp*(1.0/(rhsbeta*rhsbeta)-1.0);
        Double_t c_ct=chtimetar-cetimetar;
        Double_t r_ct=rhtimetar-retimetar;
        Double_t c_ac=chac1+chac2+chac3;
        Double_t r_ac=rhac1+rhac2+rhac3;
        Double_t c_wc=chwc1+chwc2;
        Double_t r_wc=rhwc1+rhwc2;

        Double_t xK=rhsxfp/60.0,xpK=rhsxpfp/0.15,yK=rhsyfp/6.0,ypK=rhsypfp/0.01;
        Double_t r_pK =CalcF2T(momK,xK,xpK,yK,ypK,6);
        Double_t r_xpK=CalcF2T(xptK,xK,xpK,yK,ypK,6);
        Double_t r_ypK=CalcF2T(yptK,xK,xpK,yK,ypK,6);

        Double_t xE=resxfp/25.0,xpE=resxpfp/0.1,yE=resyfp/3.0,ypE=(resypfp+0.03)/0.01;
        Double_t r_pE =CalcF2T(momE,xE,xpE,yE,ypE,6);
        Double_t r_xpE=CalcF2T(xptE,xE,xpE,yE,ypE,6);
        Double_t r_ypE=CalcF2T(yptE,xE,xpE,yE,ypE,6);

        Double_t C[NH]={
            c_m2,c_ct,c_ac,c_wc,
            ckmom,chtimetar,chstimefp,ckxpt,ckypt,chsxfp,chsyfp,chsxpfp,chsypfp,
            cemom,cetimetar,cestimefp,cexpt,ceypt,cesxfp,cesyfp,cesxpfp,cesypfp
        };

        Double_t R[NH]={
            r_m2,r_ct,r_ac,r_wc,
            r_pK,rhtimetar,rhstimefp,r_xpK,r_ypK,rhsxfp,rhsyfp,rhsxpfp,rhsypfp,
            r_pE,retimetar,restimefp,r_xpE,r_ypE,resxfp,resyfp,resxpfp,resypfp
        };

        for(Int_t k=0;k<NH;k++){
            Double_t diff=C[k]-R[k];
            h[k]->Fill(diff);
            nValid[k]++;
            if(fabs(diff)<0.02*range[k]) nSame[k]++;
        }
    }

    // ===== draw =====
    auto DrawHist=[&](TH1D *hist,Int_t k,Bool_t logy=kFALSE){
        if(logy) gPad->SetLogy();
        hist->Draw();

        TPaveText *p=new TPaveText(0.52,0.67,0.88,0.88,"NDC");
        p->SetFillStyle(0);
        p->SetBorderSize(0);
        p->SetTextAlign(12);
        p->SetTextSize(0.035);
        p->AddText(Form("Run = %d",targetRun));
        p->AddText(Form("Matched = %lld",nMatched));
        p->AddText(Form("Agreed(<2%%) = %lld",nSame[k]));
        p->AddText(Form("Agreement = %.2f %%",nMatched>0 ? 100.0*nSame[k]/nMatched : 0.0));
        p->Draw();
    };

    TCanvas *c1=new TCanvas("c1","Kaon Cherenkov",800,1200);
    c1->Divide(2,3);
    c1->cd(1); DrawHist(h_m2,0);
    c1->cd(2); DrawHist(h_ctime,1);
    c1->cd(3); DrawHist(h_kacnpe,2,kTRUE);
    c1->cd(4); DrawHist(h_kwcnpe,3,kTRUE);

    TCanvas *c2=new TCanvas("c2","Kinematics",800,1200);
    c2->Divide(2,3);
    c2->cd(1); DrawHist(h_kmom,4);
    c2->cd(2); DrawHist(h_emom,13);
    c2->cd(3); DrawHist(h_ktimetar,5);
    c2->cd(4); DrawHist(h_etimetar,14);
    c2->cd(5); DrawHist(h_ktimefp,6);
    c2->cd(6); DrawHist(h_etimefp,15);

    TCanvas *c3=new TCanvas("c3","Kaon kinematics",800,1200);
    c3->Divide(2,3);
    c3->cd(1); DrawHist(h_kxptar,7);
    c3->cd(2); DrawHist(h_kyptar,8);
    c3->cd(3); DrawHist(h_kxfp,9);
    c3->cd(4); DrawHist(h_kyfp,10);
    c3->cd(5); DrawHist(h_kxpfp,11);
    c3->cd(6); DrawHist(h_kypfp,12);

    TCanvas *c4=new TCanvas("c4","Electron kinematics",800,1200);
    c4->Divide(2,3);
    c4->cd(1); DrawHist(h_exptar,16);
    c4->cd(2); DrawHist(h_eyptar,17);
    c4->cd(3); DrawHist(h_exfp,18);
    c4->cd(4); DrawHist(h_eyfp,19);
    c4->cd(5); DrawHist(h_expfp,20);
    c4->cd(6); DrawHist(h_eypfp,21);

    c1->Modified(); c1->Update();
    c2->Modified(); c2->Update();
    c3->Modified(); c3->Update();
    c4->Modified(); c4->Update();
}