//////////////////////////////////////////////////////////
// analysis_replayed.h
// Direct analysis class for ENGINE replay COINntuple (h9500).
// Branch layout taken from the replayed h9500 supplied for run 76312.
//////////////////////////////////////////////////////////

#ifndef analysis_replayed_h
#define analysis_replayed_h

#include "analysis_param.h"
#include "analysis_style.h"
#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>
#include <TString.h>
#include <iostream>
#include <cmath>

class analysis_replayed {
public :
   TTree          *fChain;
   Int_t           fCurrent;

   Float_t         hallcp;
   Float_t         hstimefp;
   Float_t         htimetar;
   Float_t         hsxfp;
   Float_t         hsyfp;
   Float_t         hsxpfp;
   Float_t         hsypfp;
   Float_t         hsxptar;
   Float_t         hsyptar;
   Float_t         hsxsv;
   Float_t         hsysv;
   Float_t         hsxpsv;
   Float_t         hsypsv;
   Float_t         hsp;
   Float_t         hsbeta;
   Float_t         hsbeta1y;
   Float_t         hsbeta_k;
   Float_t         hmsq;
   Float_t         haernhit;
   Float_t         haernh1;
   Float_t         haernh2;
   Float_t         haernh3;
   Float_t         haernpe1;
   Float_t         haernpe2;
   Float_t         haernpe3;
   Float_t         hacnpe1m;
   Float_t         hacnpe2m;
   Float_t         hacnpe3m;
   Float_t         haern1p;
   Float_t         haern2p;
   Float_t         haern3p;
   Float_t         hacn1pm;
   Float_t         hacn2pm;
   Float_t         hacn3pm;
   Float_t         haern1n;
   Float_t         haern2n;
   Float_t         haern3n;
   Float_t         hacn1nm;
   Float_t         hacn2nm;
   Float_t         hacn3nm;
   Float_t         hact1;
   Float_t         hact2;
   Float_t         hact3;
   Float_t         hact1m;
   Float_t         hact2m;
   Float_t         hact3m;
   Float_t         hact1p;
   Float_t         hact2p;
   Float_t         hact3p;
   Float_t         hact1pm;
   Float_t         hact2pm;
   Float_t         hact3pm;
   Float_t         hact1n;
   Float_t         hact2n;
   Float_t         hact3n;
   Float_t         hact1nm;
   Float_t         hact2nm;
   Float_t         hact3nm;
   Float_t         haernum1;
   Float_t         haernum2;
   Float_t         haernum3;
   Float_t         hwatnhit;
   Float_t         hwatnh1;
   Float_t         hwatnh2;
   Float_t         hwatnpe1;
   Float_t         hwatnpe2;
   Float_t         hwcnpe1m;
   Float_t         hwcnpe2m;
   Float_t         hwatnkn1;
   Float_t         hwatnkn2;
   Float_t         hwcnkn1m;
   Float_t         hwcnkn2m;
   Float_t         hwatn1p;
   Float_t         hwatn2p;
   Float_t         hwcn1pm;
   Float_t         hwcn2pm;
   Float_t         hwatn1n;
   Float_t         hwatn2n;
   Float_t         hwcn1nm;
   Float_t         hwcn2nm;
   Float_t         hwct1;
   Float_t         hwct2;
   Float_t         hwct1m;
   Float_t         hwct2m;
   Float_t         hwatnum1;
   Float_t         hwatnum2;
   Float_t         htrkchi2;
   Float_t         htrkndf;
   Float_t         hsgres1;
   Float_t         hsgres2;
   Float_t         hsgres3;
   Float_t         hsgres4;
   Float_t         hsgres5;
   Float_t         hsgres6;
   Float_t         hsgres7;
   Float_t         hsgres8;
   Float_t         hsgres9;
   Float_t         hsgres10;
   Float_t         hsgres11;
   Float_t         hsgres12;
   Float_t         htcoor1;
   Float_t         htcoor2;
   Float_t         htcoor3;
   Float_t         htcoor4;
   Float_t         htcoor5;
   Float_t         htcoor6;
   Float_t         htcoor7;
   Float_t         htcoor8;
   Float_t         htcoor9;
   Float_t         htcoor10;
   Float_t         htcoor11;
   Float_t         htcoor12;
   Float_t         hscnhit;
   Float_t         hsnco1;
   Float_t         hsnco2;
   Float_t         hsnco3;
   Float_t         hsnt1;
   Float_t         hsnt2;
   Float_t         hsnt3;
   Float_t         hsnt1m;
   Float_t         hsnt2m;
   Float_t         hsnt3m;
   Float_t         hsna1;
   Float_t         hsna2;
   Float_t         hsna3;
   Float_t         hsna1m;
   Float_t         hsna2m;
   Float_t         hsna3m;
   Float_t         hrftime;
   Float_t         estimefp;
   Float_t         etimetar;
   Float_t         esxfp;
   Float_t         esyfp;
   Float_t         esxpfp;
   Float_t         esypfp;
   Float_t         esxptar;
   Float_t         esyptar;
   Float_t         esxsv;
   Float_t         esysv;
   Float_t         esxpsv;
   Float_t         esypsv;
   Float_t         esp;
   Float_t         etrkchi2;
   Float_t         etrkndf;
   Float_t         esgres1;
   Float_t         esgres2;
   Float_t         esgres3;
   Float_t         esgres4;
   Float_t         esgres5;
   Float_t         esgres6;
   Float_t         esgres7;
   Float_t         esgres8;
   Float_t         esgres9;
   Float_t         esgres10;
   Float_t         esgres11;
   Float_t         esgres12;
   Float_t         esgres13;
   Float_t         esgres14;
   Float_t         esgres15;
   Float_t         esgres16;
   Float_t         etcoor1;
   Float_t         etcoor2;
   Float_t         etcoor3;
   Float_t         etcoor4;
   Float_t         etcoor5;
   Float_t         etcoor6;
   Float_t         etcoor7;
   Float_t         etcoor8;
   Float_t         etcoor9;
   Float_t         etcoor10;
   Float_t         etcoor11;
   Float_t         etcoor12;
   Float_t         etcoor13;
   Float_t         etcoor14;
   Float_t         etcoor15;
   Float_t         etcoor16;
   Float_t         ewcoor1;
   Float_t         ewcoor2;
   Float_t         ewcoor3;
   Float_t         ewcoor4;
   Float_t         ewcoor5;
   Float_t         ewcoor6;
   Float_t         ewcoor7;
   Float_t         ewcoor8;
   Float_t         ewcoor9;
   Float_t         ewcoor10;
   Float_t         ewcoor11;
   Float_t         ewcoor12;
   Float_t         ewcoor13;
   Float_t         ewcoor14;
   Float_t         ewcoor15;
   Float_t         ewcoor16;
   Float_t         escnhit;
   Float_t         esnco1;
   Float_t         esnco2;
   Float_t         esnt1;
   Float_t         esnt2;
   Float_t         esnt1m;
   Float_t         esnt2m;
   Float_t         esna1;
   Float_t         esna2;
   Float_t         esna1m;
   Float_t         esna2m;
   Float_t         erftime;
   Float_t         gbeamx;
   Float_t         gbeamy;
   Float_t         gespread;
   Float_t         gferon;
   Float_t         bpm3c07x;
   Float_t         bpm3c07y;
   Float_t         bpm3c08x;
   Float_t         bpm3c08y;
   Float_t         bpm3c12x;
   Float_t         bpm3c12y;
   Float_t         hnphys;
   Float_t         enphys;
   Float_t         cnphys;
   Float_t         eventid;
   Float_t         runnum;
   Float_t         cointime;
   Float_t         tg_hes;
   Float_t         tg_hks;
   Float_t         tg_g1;
   Float_t         tg_g2;
   Float_t         tg_g3;
   Float_t         tg_g4;
   Float_t         tg_g5;
   Float_t         tg_g6;
   Float_t         ts_hks;
   Float_t         ts_hes;
   Float_t         ts_coin;
   Float_t         ts_cp0;
   Float_t         gfbon;
   Float_t         happex;
   Float_t         hlucnhit;
   Float_t         hlucnpe1;
   Float_t         hlcnpe1m;
   Float_t         hlct1;
   Float_t         hlct1m;
   Float_t         hlucnum1;

   // List of branches
   TBranch        *b_hallcp;   //!
   TBranch        *b_hstimefp;   //!
   TBranch        *b_htimetar;   //!
   TBranch        *b_hsxfp;   //!
   TBranch        *b_hsyfp;   //!
   TBranch        *b_hsxpfp;   //!
   TBranch        *b_hsypfp;   //!
   TBranch        *b_hsxptar;   //!
   TBranch        *b_hsyptar;   //!
   TBranch        *b_hsxsv;   //!
   TBranch        *b_hsysv;   //!
   TBranch        *b_hsxpsv;   //!
   TBranch        *b_hsypsv;   //!
   TBranch        *b_hsp;   //!
   TBranch        *b_hsbeta;   //!
   TBranch        *b_hsbeta1y;   //!
   TBranch        *b_hsbeta_k;   //!
   TBranch        *b_hmsq;   //!
   TBranch        *b_haernhit;   //!
   TBranch        *b_haernh1;   //!
   TBranch        *b_haernh2;   //!
   TBranch        *b_haernh3;   //!
   TBranch        *b_haernpe1;   //!
   TBranch        *b_haernpe2;   //!
   TBranch        *b_haernpe3;   //!
   TBranch        *b_hacnpe1m;   //!
   TBranch        *b_hacnpe2m;   //!
   TBranch        *b_hacnpe3m;   //!
   TBranch        *b_haern1p;   //!
   TBranch        *b_haern2p;   //!
   TBranch        *b_haern3p;   //!
   TBranch        *b_hacn1pm;   //!
   TBranch        *b_hacn2pm;   //!
   TBranch        *b_hacn3pm;   //!
   TBranch        *b_haern1n;   //!
   TBranch        *b_haern2n;   //!
   TBranch        *b_haern3n;   //!
   TBranch        *b_hacn1nm;   //!
   TBranch        *b_hacn2nm;   //!
   TBranch        *b_hacn3nm;   //!
   TBranch        *b_hact1;   //!
   TBranch        *b_hact2;   //!
   TBranch        *b_hact3;   //!
   TBranch        *b_hact1m;   //!
   TBranch        *b_hact2m;   //!
   TBranch        *b_hact3m;   //!
   TBranch        *b_hact1p;   //!
   TBranch        *b_hact2p;   //!
   TBranch        *b_hact3p;   //!
   TBranch        *b_hact1pm;   //!
   TBranch        *b_hact2pm;   //!
   TBranch        *b_hact3pm;   //!
   TBranch        *b_hact1n;   //!
   TBranch        *b_hact2n;   //!
   TBranch        *b_hact3n;   //!
   TBranch        *b_hact1nm;   //!
   TBranch        *b_hact2nm;   //!
   TBranch        *b_hact3nm;   //!
   TBranch        *b_haernum1;   //!
   TBranch        *b_haernum2;   //!
   TBranch        *b_haernum3;   //!
   TBranch        *b_hwatnhit;   //!
   TBranch        *b_hwatnh1;   //!
   TBranch        *b_hwatnh2;   //!
   TBranch        *b_hwatnpe1;   //!
   TBranch        *b_hwatnpe2;   //!
   TBranch        *b_hwcnpe1m;   //!
   TBranch        *b_hwcnpe2m;   //!
   TBranch        *b_hwatnkn1;   //!
   TBranch        *b_hwatnkn2;   //!
   TBranch        *b_hwcnkn1m;   //!
   TBranch        *b_hwcnkn2m;   //!
   TBranch        *b_hwatn1p;   //!
   TBranch        *b_hwatn2p;   //!
   TBranch        *b_hwcn1pm;   //!
   TBranch        *b_hwcn2pm;   //!
   TBranch        *b_hwatn1n;   //!
   TBranch        *b_hwatn2n;   //!
   TBranch        *b_hwcn1nm;   //!
   TBranch        *b_hwcn2nm;   //!
   TBranch        *b_hwct1;   //!
   TBranch        *b_hwct2;   //!
   TBranch        *b_hwct1m;   //!
   TBranch        *b_hwct2m;   //!
   TBranch        *b_hwatnum1;   //!
   TBranch        *b_hwatnum2;   //!
   TBranch        *b_htrkchi2;   //!
   TBranch        *b_htrkndf;   //!
   TBranch        *b_hsgres1;   //!
   TBranch        *b_hsgres2;   //!
   TBranch        *b_hsgres3;   //!
   TBranch        *b_hsgres4;   //!
   TBranch        *b_hsgres5;   //!
   TBranch        *b_hsgres6;   //!
   TBranch        *b_hsgres7;   //!
   TBranch        *b_hsgres8;   //!
   TBranch        *b_hsgres9;   //!
   TBranch        *b_hsgres10;   //!
   TBranch        *b_hsgres11;   //!
   TBranch        *b_hsgres12;   //!
   TBranch        *b_htcoor1;   //!
   TBranch        *b_htcoor2;   //!
   TBranch        *b_htcoor3;   //!
   TBranch        *b_htcoor4;   //!
   TBranch        *b_htcoor5;   //!
   TBranch        *b_htcoor6;   //!
   TBranch        *b_htcoor7;   //!
   TBranch        *b_htcoor8;   //!
   TBranch        *b_htcoor9;   //!
   TBranch        *b_htcoor10;   //!
   TBranch        *b_htcoor11;   //!
   TBranch        *b_htcoor12;   //!
   TBranch        *b_hscnhit;   //!
   TBranch        *b_hsnco1;   //!
   TBranch        *b_hsnco2;   //!
   TBranch        *b_hsnco3;   //!
   TBranch        *b_hsnt1;   //!
   TBranch        *b_hsnt2;   //!
   TBranch        *b_hsnt3;   //!
   TBranch        *b_hsnt1m;   //!
   TBranch        *b_hsnt2m;   //!
   TBranch        *b_hsnt3m;   //!
   TBranch        *b_hsna1;   //!
   TBranch        *b_hsna2;   //!
   TBranch        *b_hsna3;   //!
   TBranch        *b_hsna1m;   //!
   TBranch        *b_hsna2m;   //!
   TBranch        *b_hsna3m;   //!
   TBranch        *b_hrftime;   //!
   TBranch        *b_estimefp;   //!
   TBranch        *b_etimetar;   //!
   TBranch        *b_esxfp;   //!
   TBranch        *b_esyfp;   //!
   TBranch        *b_esxpfp;   //!
   TBranch        *b_esypfp;   //!
   TBranch        *b_esxptar;   //!
   TBranch        *b_esyptar;   //!
   TBranch        *b_esxsv;   //!
   TBranch        *b_esysv;   //!
   TBranch        *b_esxpsv;   //!
   TBranch        *b_esypsv;   //!
   TBranch        *b_esp;   //!
   TBranch        *b_etrkchi2;   //!
   TBranch        *b_etrkndf;   //!
   TBranch        *b_esgres1;   //!
   TBranch        *b_esgres2;   //!
   TBranch        *b_esgres3;   //!
   TBranch        *b_esgres4;   //!
   TBranch        *b_esgres5;   //!
   TBranch        *b_esgres6;   //!
   TBranch        *b_esgres7;   //!
   TBranch        *b_esgres8;   //!
   TBranch        *b_esgres9;   //!
   TBranch        *b_esgres10;   //!
   TBranch        *b_esgres11;   //!
   TBranch        *b_esgres12;   //!
   TBranch        *b_esgres13;   //!
   TBranch        *b_esgres14;   //!
   TBranch        *b_esgres15;   //!
   TBranch        *b_esgres16;   //!
   TBranch        *b_etcoor1;   //!
   TBranch        *b_etcoor2;   //!
   TBranch        *b_etcoor3;   //!
   TBranch        *b_etcoor4;   //!
   TBranch        *b_etcoor5;   //!
   TBranch        *b_etcoor6;   //!
   TBranch        *b_etcoor7;   //!
   TBranch        *b_etcoor8;   //!
   TBranch        *b_etcoor9;   //!
   TBranch        *b_etcoor10;   //!
   TBranch        *b_etcoor11;   //!
   TBranch        *b_etcoor12;   //!
   TBranch        *b_etcoor13;   //!
   TBranch        *b_etcoor14;   //!
   TBranch        *b_etcoor15;   //!
   TBranch        *b_etcoor16;   //!
   TBranch        *b_ewcoor1;   //!
   TBranch        *b_ewcoor2;   //!
   TBranch        *b_ewcoor3;   //!
   TBranch        *b_ewcoor4;   //!
   TBranch        *b_ewcoor5;   //!
   TBranch        *b_ewcoor6;   //!
   TBranch        *b_ewcoor7;   //!
   TBranch        *b_ewcoor8;   //!
   TBranch        *b_ewcoor9;   //!
   TBranch        *b_ewcoor10;   //!
   TBranch        *b_ewcoor11;   //!
   TBranch        *b_ewcoor12;   //!
   TBranch        *b_ewcoor13;   //!
   TBranch        *b_ewcoor14;   //!
   TBranch        *b_ewcoor15;   //!
   TBranch        *b_ewcoor16;   //!
   TBranch        *b_escnhit;   //!
   TBranch        *b_esnco1;   //!
   TBranch        *b_esnco2;   //!
   TBranch        *b_esnt1;   //!
   TBranch        *b_esnt2;   //!
   TBranch        *b_esnt1m;   //!
   TBranch        *b_esnt2m;   //!
   TBranch        *b_esna1;   //!
   TBranch        *b_esna2;   //!
   TBranch        *b_esna1m;   //!
   TBranch        *b_esna2m;   //!
   TBranch        *b_erftime;   //!
   TBranch        *b_gbeamx;   //!
   TBranch        *b_gbeamy;   //!
   TBranch        *b_gespread;   //!
   TBranch        *b_gferon;   //!
   TBranch        *b_bpm3c07x;   //!
   TBranch        *b_bpm3c07y;   //!
   TBranch        *b_bpm3c08x;   //!
   TBranch        *b_bpm3c08y;   //!
   TBranch        *b_bpm3c12x;   //!
   TBranch        *b_bpm3c12y;   //!
   TBranch        *b_hnphys;   //!
   TBranch        *b_enphys;   //!
   TBranch        *b_cnphys;   //!
   TBranch        *b_eventid;   //!
   TBranch        *b_runnum;   //!
   TBranch        *b_cointime;   //!
   TBranch        *b_tg_hes;   //!
   TBranch        *b_tg_hks;   //!
   TBranch        *b_tg_g1;   //!
   TBranch        *b_tg_g2;   //!
   TBranch        *b_tg_g3;   //!
   TBranch        *b_tg_g4;   //!
   TBranch        *b_tg_g5;   //!
   TBranch        *b_tg_g6;   //!
   TBranch        *b_ts_hks;   //!
   TBranch        *b_ts_hes;   //!
   TBranch        *b_ts_coin;   //!
   TBranch        *b_ts_cp0;   //!
   TBranch        *b_gfbon;   //!
   TBranch        *b_happex;   //!
   TBranch        *b_hlucnhit;   //!
   TBranch        *b_hlucnpe1;   //!
   TBranch        *b_hlcnpe1m;   //!
   TBranch        *b_hlct1;   //!
   TBranch        *b_hlct1m;   //!
   TBranch        *b_hlucnum1;   //!

   // If tree==0, filename is opened and h9500 is used.
   // Default path assumes the macro is run from E05-115/analysis/.
   analysis_replayed(TTree *tree=0,
                     const char *filename="../rootfiles/ch2.root");
   virtual ~analysis_replayed();
   virtual Int_t    GetEntry(Long64_t entry);
   virtual Long64_t LoadTree(Long64_t entry);
   virtual void     Init(TTree *tree);

   virtual void     Loop();
   virtual bool     KaonCut();
   virtual bool     CtimeCut();

   virtual Double_t RasterCorrection(Double_t xt, Double_t yt, Int_t TargetFlag);
   virtual Double_t GetMM(Double_t eBeam,
                          Double_t xpKaon, Double_t ypKaon, Double_t pKaon,
                          Double_t xpElectron, Double_t ypElectron, Double_t pElectron,
                          Int_t TargetFlag);
};

#endif

#ifdef analysis_replayed_cxx
analysis_replayed::analysis_replayed(TTree *tree, const char *filename) : fChain(0), fCurrent(-1)
{
   if (tree == 0) {
      TString filepath = filename;
      TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject(filepath);

      if (!f || !f->IsOpen()) {
         f = new TFile(filepath, "UPDATE");
      }

      if (!f || f->IsZombie()) {
         std::cout << "Cannot open ROOT file: " << filepath << std::endl;
         return;
      }

      f->GetObject("h9500", tree);

      if (!tree) {
         std::cout << "Cannot find TTree h9500 in " << filepath << std::endl;
         return;
      }

      std::cout << "TargetFlag : " << gTargetFlag << std::endl;
      std::cout << "Input file : " << filepath << std::endl;
   }

   Init(tree);
}

analysis_replayed::~analysis_replayed()
{
   if (!fChain) return;
   delete fChain->GetCurrentFile();
}

Int_t analysis_replayed::GetEntry(Long64_t entry)
{
   if (!fChain) return 0;
   return fChain->GetEntry(entry);
}

Long64_t analysis_replayed::LoadTree(Long64_t entry)
{
   if (!fChain) return -5;
   Long64_t centry = fChain->LoadTree(entry);
   if (centry < 0) return centry;
   if (fChain->GetTreeNumber() != fCurrent) {
      fCurrent = fChain->GetTreeNumber();
   }
   return centry;
}

void analysis_replayed::Init(TTree *tree)
{
   if (!tree) return;
   fChain = tree;
   fCurrent = -1;
   fChain->SetMakeClass(1);

   fChain->SetBranchAddress("hallcp", &hallcp, &b_hallcp);
   fChain->SetBranchAddress("hstimefp", &hstimefp, &b_hstimefp);
   fChain->SetBranchAddress("htimetar", &htimetar, &b_htimetar);
   fChain->SetBranchAddress("hsxfp", &hsxfp, &b_hsxfp);
   fChain->SetBranchAddress("hsyfp", &hsyfp, &b_hsyfp);
   fChain->SetBranchAddress("hsxpfp", &hsxpfp, &b_hsxpfp);
   fChain->SetBranchAddress("hsypfp", &hsypfp, &b_hsypfp);
   fChain->SetBranchAddress("hsxptar", &hsxptar, &b_hsxptar);
   fChain->SetBranchAddress("hsyptar", &hsyptar, &b_hsyptar);
   fChain->SetBranchAddress("hsxsv", &hsxsv, &b_hsxsv);
   fChain->SetBranchAddress("hsysv", &hsysv, &b_hsysv);
   fChain->SetBranchAddress("hsxpsv", &hsxpsv, &b_hsxpsv);
   fChain->SetBranchAddress("hsypsv", &hsypsv, &b_hsypsv);
   fChain->SetBranchAddress("hsp", &hsp, &b_hsp);
   fChain->SetBranchAddress("hsbeta", &hsbeta, &b_hsbeta);
   fChain->SetBranchAddress("hsbeta1y", &hsbeta1y, &b_hsbeta1y);
   fChain->SetBranchAddress("hsbeta_k", &hsbeta_k, &b_hsbeta_k);
   fChain->SetBranchAddress("hmsq", &hmsq, &b_hmsq);
   fChain->SetBranchAddress("haernhit", &haernhit, &b_haernhit);
   fChain->SetBranchAddress("haernh1", &haernh1, &b_haernh1);
   fChain->SetBranchAddress("haernh2", &haernh2, &b_haernh2);
   fChain->SetBranchAddress("haernh3", &haernh3, &b_haernh3);
   fChain->SetBranchAddress("haernpe1", &haernpe1, &b_haernpe1);
   fChain->SetBranchAddress("haernpe2", &haernpe2, &b_haernpe2);
   fChain->SetBranchAddress("haernpe3", &haernpe3, &b_haernpe3);
   fChain->SetBranchAddress("hacnpe1m", &hacnpe1m, &b_hacnpe1m);
   fChain->SetBranchAddress("hacnpe2m", &hacnpe2m, &b_hacnpe2m);
   fChain->SetBranchAddress("hacnpe3m", &hacnpe3m, &b_hacnpe3m);
   fChain->SetBranchAddress("haern1p", &haern1p, &b_haern1p);
   fChain->SetBranchAddress("haern2p", &haern2p, &b_haern2p);
   fChain->SetBranchAddress("haern3p", &haern3p, &b_haern3p);
   fChain->SetBranchAddress("hacn1pm", &hacn1pm, &b_hacn1pm);
   fChain->SetBranchAddress("hacn2pm", &hacn2pm, &b_hacn2pm);
   fChain->SetBranchAddress("hacn3pm", &hacn3pm, &b_hacn3pm);
   fChain->SetBranchAddress("haern1n", &haern1n, &b_haern1n);
   fChain->SetBranchAddress("haern2n", &haern2n, &b_haern2n);
   fChain->SetBranchAddress("haern3n", &haern3n, &b_haern3n);
   fChain->SetBranchAddress("hacn1nm", &hacn1nm, &b_hacn1nm);
   fChain->SetBranchAddress("hacn2nm", &hacn2nm, &b_hacn2nm);
   fChain->SetBranchAddress("hacn3nm", &hacn3nm, &b_hacn3nm);
   fChain->SetBranchAddress("hact1", &hact1, &b_hact1);
   fChain->SetBranchAddress("hact2", &hact2, &b_hact2);
   fChain->SetBranchAddress("hact3", &hact3, &b_hact3);
   fChain->SetBranchAddress("hact1m", &hact1m, &b_hact1m);
   fChain->SetBranchAddress("hact2m", &hact2m, &b_hact2m);
   fChain->SetBranchAddress("hact3m", &hact3m, &b_hact3m);
   fChain->SetBranchAddress("hact1p", &hact1p, &b_hact1p);
   fChain->SetBranchAddress("hact2p", &hact2p, &b_hact2p);
   fChain->SetBranchAddress("hact3p", &hact3p, &b_hact3p);
   fChain->SetBranchAddress("hact1pm", &hact1pm, &b_hact1pm);
   fChain->SetBranchAddress("hact2pm", &hact2pm, &b_hact2pm);
   fChain->SetBranchAddress("hact3pm", &hact3pm, &b_hact3pm);
   fChain->SetBranchAddress("hact1n", &hact1n, &b_hact1n);
   fChain->SetBranchAddress("hact2n", &hact2n, &b_hact2n);
   fChain->SetBranchAddress("hact3n", &hact3n, &b_hact3n);
   fChain->SetBranchAddress("hact1nm", &hact1nm, &b_hact1nm);
   fChain->SetBranchAddress("hact2nm", &hact2nm, &b_hact2nm);
   fChain->SetBranchAddress("hact3nm", &hact3nm, &b_hact3nm);
   fChain->SetBranchAddress("haernum1", &haernum1, &b_haernum1);
   fChain->SetBranchAddress("haernum2", &haernum2, &b_haernum2);
   fChain->SetBranchAddress("haernum3", &haernum3, &b_haernum3);
   fChain->SetBranchAddress("hwatnhit", &hwatnhit, &b_hwatnhit);
   fChain->SetBranchAddress("hwatnh1", &hwatnh1, &b_hwatnh1);
   fChain->SetBranchAddress("hwatnh2", &hwatnh2, &b_hwatnh2);
   fChain->SetBranchAddress("hwatnpe1", &hwatnpe1, &b_hwatnpe1);
   fChain->SetBranchAddress("hwatnpe2", &hwatnpe2, &b_hwatnpe2);
   fChain->SetBranchAddress("hwcnpe1m", &hwcnpe1m, &b_hwcnpe1m);
   fChain->SetBranchAddress("hwcnpe2m", &hwcnpe2m, &b_hwcnpe2m);
   fChain->SetBranchAddress("hwatnkn1", &hwatnkn1, &b_hwatnkn1);
   fChain->SetBranchAddress("hwatnkn2", &hwatnkn2, &b_hwatnkn2);
   fChain->SetBranchAddress("hwcnkn1m", &hwcnkn1m, &b_hwcnkn1m);
   fChain->SetBranchAddress("hwcnkn2m", &hwcnkn2m, &b_hwcnkn2m);
   fChain->SetBranchAddress("hwatn1p", &hwatn1p, &b_hwatn1p);
   fChain->SetBranchAddress("hwatn2p", &hwatn2p, &b_hwatn2p);
   fChain->SetBranchAddress("hwcn1pm", &hwcn1pm, &b_hwcn1pm);
   fChain->SetBranchAddress("hwcn2pm", &hwcn2pm, &b_hwcn2pm);
   fChain->SetBranchAddress("hwatn1n", &hwatn1n, &b_hwatn1n);
   fChain->SetBranchAddress("hwatn2n", &hwatn2n, &b_hwatn2n);
   fChain->SetBranchAddress("hwcn1nm", &hwcn1nm, &b_hwcn1nm);
   fChain->SetBranchAddress("hwcn2nm", &hwcn2nm, &b_hwcn2nm);
   fChain->SetBranchAddress("hwct1", &hwct1, &b_hwct1);
   fChain->SetBranchAddress("hwct2", &hwct2, &b_hwct2);
   fChain->SetBranchAddress("hwct1m", &hwct1m, &b_hwct1m);
   fChain->SetBranchAddress("hwct2m", &hwct2m, &b_hwct2m);
   fChain->SetBranchAddress("hwatnum1", &hwatnum1, &b_hwatnum1);
   fChain->SetBranchAddress("hwatnum2", &hwatnum2, &b_hwatnum2);
   fChain->SetBranchAddress("htrkchi2", &htrkchi2, &b_htrkchi2);
   fChain->SetBranchAddress("htrkndf", &htrkndf, &b_htrkndf);
   fChain->SetBranchAddress("hsgres1", &hsgres1, &b_hsgres1);
   fChain->SetBranchAddress("hsgres2", &hsgres2, &b_hsgres2);
   fChain->SetBranchAddress("hsgres3", &hsgres3, &b_hsgres3);
   fChain->SetBranchAddress("hsgres4", &hsgres4, &b_hsgres4);
   fChain->SetBranchAddress("hsgres5", &hsgres5, &b_hsgres5);
   fChain->SetBranchAddress("hsgres6", &hsgres6, &b_hsgres6);
   fChain->SetBranchAddress("hsgres7", &hsgres7, &b_hsgres7);
   fChain->SetBranchAddress("hsgres8", &hsgres8, &b_hsgres8);
   fChain->SetBranchAddress("hsgres9", &hsgres9, &b_hsgres9);
   fChain->SetBranchAddress("hsgres10", &hsgres10, &b_hsgres10);
   fChain->SetBranchAddress("hsgres11", &hsgres11, &b_hsgres11);
   fChain->SetBranchAddress("hsgres12", &hsgres12, &b_hsgres12);
   fChain->SetBranchAddress("htcoor1", &htcoor1, &b_htcoor1);
   fChain->SetBranchAddress("htcoor2", &htcoor2, &b_htcoor2);
   fChain->SetBranchAddress("htcoor3", &htcoor3, &b_htcoor3);
   fChain->SetBranchAddress("htcoor4", &htcoor4, &b_htcoor4);
   fChain->SetBranchAddress("htcoor5", &htcoor5, &b_htcoor5);
   fChain->SetBranchAddress("htcoor6", &htcoor6, &b_htcoor6);
   fChain->SetBranchAddress("htcoor7", &htcoor7, &b_htcoor7);
   fChain->SetBranchAddress("htcoor8", &htcoor8, &b_htcoor8);
   fChain->SetBranchAddress("htcoor9", &htcoor9, &b_htcoor9);
   fChain->SetBranchAddress("htcoor10", &htcoor10, &b_htcoor10);
   fChain->SetBranchAddress("htcoor11", &htcoor11, &b_htcoor11);
   fChain->SetBranchAddress("htcoor12", &htcoor12, &b_htcoor12);
   fChain->SetBranchAddress("hscnhit", &hscnhit, &b_hscnhit);
   fChain->SetBranchAddress("hsnco1", &hsnco1, &b_hsnco1);
   fChain->SetBranchAddress("hsnco2", &hsnco2, &b_hsnco2);
   fChain->SetBranchAddress("hsnco3", &hsnco3, &b_hsnco3);
   fChain->SetBranchAddress("hsnt1", &hsnt1, &b_hsnt1);
   fChain->SetBranchAddress("hsnt2", &hsnt2, &b_hsnt2);
   fChain->SetBranchAddress("hsnt3", &hsnt3, &b_hsnt3);
   fChain->SetBranchAddress("hsnt1m", &hsnt1m, &b_hsnt1m);
   fChain->SetBranchAddress("hsnt2m", &hsnt2m, &b_hsnt2m);
   fChain->SetBranchAddress("hsnt3m", &hsnt3m, &b_hsnt3m);
   fChain->SetBranchAddress("hsna1", &hsna1, &b_hsna1);
   fChain->SetBranchAddress("hsna2", &hsna2, &b_hsna2);
   fChain->SetBranchAddress("hsna3", &hsna3, &b_hsna3);
   fChain->SetBranchAddress("hsna1m", &hsna1m, &b_hsna1m);
   fChain->SetBranchAddress("hsna2m", &hsna2m, &b_hsna2m);
   fChain->SetBranchAddress("hsna3m", &hsna3m, &b_hsna3m);
   fChain->SetBranchAddress("hrftime", &hrftime, &b_hrftime);
   fChain->SetBranchAddress("estimefp", &estimefp, &b_estimefp);
   fChain->SetBranchAddress("etimetar", &etimetar, &b_etimetar);
   fChain->SetBranchAddress("esxfp", &esxfp, &b_esxfp);
   fChain->SetBranchAddress("esyfp", &esyfp, &b_esyfp);
   fChain->SetBranchAddress("esxpfp", &esxpfp, &b_esxpfp);
   fChain->SetBranchAddress("esypfp", &esypfp, &b_esypfp);
   fChain->SetBranchAddress("esxptar", &esxptar, &b_esxptar);
   fChain->SetBranchAddress("esyptar", &esyptar, &b_esyptar);
   fChain->SetBranchAddress("esxsv", &esxsv, &b_esxsv);
   fChain->SetBranchAddress("esysv", &esysv, &b_esysv);
   fChain->SetBranchAddress("esxpsv", &esxpsv, &b_esxpsv);
   fChain->SetBranchAddress("esypsv", &esypsv, &b_esypsv);
   fChain->SetBranchAddress("esp", &esp, &b_esp);
   fChain->SetBranchAddress("etrkchi2", &etrkchi2, &b_etrkchi2);
   fChain->SetBranchAddress("etrkndf", &etrkndf, &b_etrkndf);
   fChain->SetBranchAddress("esgres1", &esgres1, &b_esgres1);
   fChain->SetBranchAddress("esgres2", &esgres2, &b_esgres2);
   fChain->SetBranchAddress("esgres3", &esgres3, &b_esgres3);
   fChain->SetBranchAddress("esgres4", &esgres4, &b_esgres4);
   fChain->SetBranchAddress("esgres5", &esgres5, &b_esgres5);
   fChain->SetBranchAddress("esgres6", &esgres6, &b_esgres6);
   fChain->SetBranchAddress("esgres7", &esgres7, &b_esgres7);
   fChain->SetBranchAddress("esgres8", &esgres8, &b_esgres8);
   fChain->SetBranchAddress("esgres9", &esgres9, &b_esgres9);
   fChain->SetBranchAddress("esgres10", &esgres10, &b_esgres10);
   fChain->SetBranchAddress("esgres11", &esgres11, &b_esgres11);
   fChain->SetBranchAddress("esgres12", &esgres12, &b_esgres12);
   fChain->SetBranchAddress("esgres13", &esgres13, &b_esgres13);
   fChain->SetBranchAddress("esgres14", &esgres14, &b_esgres14);
   fChain->SetBranchAddress("esgres15", &esgres15, &b_esgres15);
   fChain->SetBranchAddress("esgres16", &esgres16, &b_esgres16);
   fChain->SetBranchAddress("etcoor1", &etcoor1, &b_etcoor1);
   fChain->SetBranchAddress("etcoor2", &etcoor2, &b_etcoor2);
   fChain->SetBranchAddress("etcoor3", &etcoor3, &b_etcoor3);
   fChain->SetBranchAddress("etcoor4", &etcoor4, &b_etcoor4);
   fChain->SetBranchAddress("etcoor5", &etcoor5, &b_etcoor5);
   fChain->SetBranchAddress("etcoor6", &etcoor6, &b_etcoor6);
   fChain->SetBranchAddress("etcoor7", &etcoor7, &b_etcoor7);
   fChain->SetBranchAddress("etcoor8", &etcoor8, &b_etcoor8);
   fChain->SetBranchAddress("etcoor9", &etcoor9, &b_etcoor9);
   fChain->SetBranchAddress("etcoor10", &etcoor10, &b_etcoor10);
   fChain->SetBranchAddress("etcoor11", &etcoor11, &b_etcoor11);
   fChain->SetBranchAddress("etcoor12", &etcoor12, &b_etcoor12);
   fChain->SetBranchAddress("etcoor13", &etcoor13, &b_etcoor13);
   fChain->SetBranchAddress("etcoor14", &etcoor14, &b_etcoor14);
   fChain->SetBranchAddress("etcoor15", &etcoor15, &b_etcoor15);
   fChain->SetBranchAddress("etcoor16", &etcoor16, &b_etcoor16);
   fChain->SetBranchAddress("ewcoor1", &ewcoor1, &b_ewcoor1);
   fChain->SetBranchAddress("ewcoor2", &ewcoor2, &b_ewcoor2);
   fChain->SetBranchAddress("ewcoor3", &ewcoor3, &b_ewcoor3);
   fChain->SetBranchAddress("ewcoor4", &ewcoor4, &b_ewcoor4);
   fChain->SetBranchAddress("ewcoor5", &ewcoor5, &b_ewcoor5);
   fChain->SetBranchAddress("ewcoor6", &ewcoor6, &b_ewcoor6);
   fChain->SetBranchAddress("ewcoor7", &ewcoor7, &b_ewcoor7);
   fChain->SetBranchAddress("ewcoor8", &ewcoor8, &b_ewcoor8);
   fChain->SetBranchAddress("ewcoor9", &ewcoor9, &b_ewcoor9);
   fChain->SetBranchAddress("ewcoor10", &ewcoor10, &b_ewcoor10);
   fChain->SetBranchAddress("ewcoor11", &ewcoor11, &b_ewcoor11);
   fChain->SetBranchAddress("ewcoor12", &ewcoor12, &b_ewcoor12);
   fChain->SetBranchAddress("ewcoor13", &ewcoor13, &b_ewcoor13);
   fChain->SetBranchAddress("ewcoor14", &ewcoor14, &b_ewcoor14);
   fChain->SetBranchAddress("ewcoor15", &ewcoor15, &b_ewcoor15);
   fChain->SetBranchAddress("ewcoor16", &ewcoor16, &b_ewcoor16);
   fChain->SetBranchAddress("escnhit", &escnhit, &b_escnhit);
   fChain->SetBranchAddress("esnco1", &esnco1, &b_esnco1);
   fChain->SetBranchAddress("esnco2", &esnco2, &b_esnco2);
   fChain->SetBranchAddress("esnt1", &esnt1, &b_esnt1);
   fChain->SetBranchAddress("esnt2", &esnt2, &b_esnt2);
   fChain->SetBranchAddress("esnt1m", &esnt1m, &b_esnt1m);
   fChain->SetBranchAddress("esnt2m", &esnt2m, &b_esnt2m);
   fChain->SetBranchAddress("esna1", &esna1, &b_esna1);
   fChain->SetBranchAddress("esna2", &esna2, &b_esna2);
   fChain->SetBranchAddress("esna1m", &esna1m, &b_esna1m);
   fChain->SetBranchAddress("esna2m", &esna2m, &b_esna2m);
   fChain->SetBranchAddress("erftime", &erftime, &b_erftime);
   fChain->SetBranchAddress("gbeamx", &gbeamx, &b_gbeamx);
   fChain->SetBranchAddress("gbeamy", &gbeamy, &b_gbeamy);
   fChain->SetBranchAddress("gespread", &gespread, &b_gespread);
   fChain->SetBranchAddress("gferon", &gferon, &b_gferon);
   fChain->SetBranchAddress("bpm3c07x", &bpm3c07x, &b_bpm3c07x);
   fChain->SetBranchAddress("bpm3c07y", &bpm3c07y, &b_bpm3c07y);
   fChain->SetBranchAddress("bpm3c08x", &bpm3c08x, &b_bpm3c08x);
   fChain->SetBranchAddress("bpm3c08y", &bpm3c08y, &b_bpm3c08y);
   fChain->SetBranchAddress("bpm3c12x", &bpm3c12x, &b_bpm3c12x);
   fChain->SetBranchAddress("bpm3c12y", &bpm3c12y, &b_bpm3c12y);
   fChain->SetBranchAddress("hnphys", &hnphys, &b_hnphys);
   fChain->SetBranchAddress("enphys", &enphys, &b_enphys);
   fChain->SetBranchAddress("cnphys", &cnphys, &b_cnphys);
   fChain->SetBranchAddress("eventid", &eventid, &b_eventid);
   fChain->SetBranchAddress("runnum", &runnum, &b_runnum);
   fChain->SetBranchAddress("cointime", &cointime, &b_cointime);
   fChain->SetBranchAddress("tg_hes", &tg_hes, &b_tg_hes);
   fChain->SetBranchAddress("tg_hks", &tg_hks, &b_tg_hks);
   fChain->SetBranchAddress("tg_g1", &tg_g1, &b_tg_g1);
   fChain->SetBranchAddress("tg_g2", &tg_g2, &b_tg_g2);
   fChain->SetBranchAddress("tg_g3", &tg_g3, &b_tg_g3);
   fChain->SetBranchAddress("tg_g4", &tg_g4, &b_tg_g4);
   fChain->SetBranchAddress("tg_g5", &tg_g5, &b_tg_g5);
   fChain->SetBranchAddress("tg_g6", &tg_g6, &b_tg_g6);
   fChain->SetBranchAddress("ts_hks", &ts_hks, &b_ts_hks);
   fChain->SetBranchAddress("ts_hes", &ts_hes, &b_ts_hes);
   fChain->SetBranchAddress("ts_coin", &ts_coin, &b_ts_coin);
   fChain->SetBranchAddress("ts_cp0", &ts_cp0, &b_ts_cp0);
   fChain->SetBranchAddress("gfbon", &gfbon, &b_gfbon);
   fChain->SetBranchAddress("happex", &happex, &b_happex);
   fChain->SetBranchAddress("hlucnhit", &hlucnhit, &b_hlucnhit);
   fChain->SetBranchAddress("hlucnpe1", &hlucnpe1, &b_hlucnpe1);
   fChain->SetBranchAddress("hlcnpe1m", &hlcnpe1m, &b_hlcnpe1m);
   fChain->SetBranchAddress("hlct1", &hlct1, &b_hlct1);
   fChain->SetBranchAddress("hlct1m", &hlct1m, &b_hlct1m);
   fChain->SetBranchAddress("hlucnum1", &hlucnum1, &b_hlucnum1);
}

bool analysis_replayed::KaonCut()
{
   double m2 = hsp*hsp*(1.0/(hsbeta*hsbeta) - 1.0);

   const Int_t iwc = (Int_t)(hwatnum1*2 - 2);
   if (iwc < 0 || iwc >= 23) return false;

   return (std::abs(m2-gKaonMass*gKaonMass) < 0.3 &&
           (haernpe1 + haernpe2 + haernpe3) < 20 &&
           (hwatnpe1 / gWC1_norm[iwc] +
            hwatnpe2 / gWC2_norm[iwc]) < 10);
}

bool analysis_replayed::CtimeCut()
{
   double ctime = htimetar - etimetar;
   return (ctime > -63.0 && ctime < -47.0);
   //return (ctime > -55.5 && ctime < -53.5); 
}
#endif // #ifdef analysis_replayed_cxx
