//////////////////////////////////////////////////////////
// This class has been automatically generated on
// Sun Aug 23 20:01:58 2026 by ROOT version 6.30/04
// from TTree h9500/COINntuple
// found on file: ../rootfiles/ch2_test.root
//////////////////////////////////////////////////////////

#ifndef ch2_analysis_h
#define ch2_analysis_h

#include "analysis_param.h"
#include "analysis_style.h"
#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>

class ch2_analysis {
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
   Float_t         hsxtar;
   Float_t         hsytar;
   Float_t         hsxptar;
   Float_t         hsyptar;
   Float_t         hsxsv;
   Float_t         hsysv;
   Float_t         hsxpsv;
   Float_t         hsypsv;
   Float_t         hsp;
   Float_t         hsdelta;
   Float_t         hsbeta;
   Float_t         hsbeta_k;
   Float_t         haernhit;
   Float_t         haernpe1;
   Float_t         haernpe2;
   Float_t         haernpe3;
   Float_t         hacnpe1m;
   Float_t         hacnpe2m;
   Float_t         hacnpe3m;
   Float_t         hact1;
   Float_t         hact2;
   Float_t         hact3;
   Float_t         hact1m;
   Float_t         hact2m;
   Float_t         hact3m;
   Float_t         haernum1;
   Float_t         haernum2;
   Float_t         haernum3;
   Float_t         hwatnhit;
   Float_t         hwatnpe1;
   Float_t         hwatnpe2;
   Float_t         hwcnpe1m;
   Float_t         hwcnpe2m;
   Float_t         hwct1;
   Float_t         hwct2;
   Float_t         hwct1m;
   Float_t         hwct2m;
   Float_t         hwatnum1;
   Float_t         hwatnum2;
   Float_t         htrkchi2;
   Float_t         htrkndf;
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
   Float_t         hscdepo;
   Float_t         hrftime;
   Float_t         hrfdiff;
   Float_t         estimefp;
   Float_t         etimetar;
   Float_t         esxfp;
   Float_t         esyfp;
   Float_t         esxpfp;
   Float_t         esypfp;
   Float_t         esxtar;
   Float_t         esytar;
   Float_t         esxptar;
   Float_t         esyptar;
   Float_t         esxsv;
   Float_t         esysv;
   Float_t         esxpsv;
   Float_t         esypsv;
   Float_t         esp;
   Float_t         esdelta;
   Float_t         etrkchi2;
   Float_t         etrkndf;
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
   Float_t         escdepo;
   Float_t         erftime;
   Float_t         erfdiff;
   Float_t         gbeamx;
   Float_t         gbeamy;
   Float_t         gespread;
   Float_t         gfbon;
   Float_t         gferon;
   Float_t         ipm3c07x;
   Float_t         ipm3c07y;
   Float_t         ipm3c08x;
   Float_t         ipm3c08y;
   Float_t         ipm3c11x;
   Float_t         ipm3c11y;
   Float_t         ipm3c12x;
   Float_t         ipm3c12y;
   Float_t         ipm3c14x;
   Float_t         ipm3c14y;
   Float_t         ipm3c16x;
   Float_t         ipm3c16y;
   Float_t         ipm3c17x;
   Float_t         ipm3c17y;
   Float_t         ipm3c18x;
   Float_t         ipm3c18y;
   Float_t         ipm3c20x;
   Float_t         ipm3c20y;
   Float_t         hnphys;
   Float_t         enphys;
   Float_t         cnphys;
   Float_t         eventid;
   Float_t         runnum;
   Float_t         coinrf;
   Float_t         cointime;
   Float_t         tg_hes;
   Float_t         tg_hks;
   Float_t         tg_g1;
   Float_t         tg_g2;
   Float_t         tg_g3;
   Float_t         tg_g4;
   Float_t         tg_g5;
   Float_t         tg_g6;
   Float_t         tg_unb;
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
   TBranch        *b_hsxtar;   //!
   TBranch        *b_hsytar;   //!
   TBranch        *b_hsxptar;   //!
   TBranch        *b_hsyptar;   //!
   TBranch        *b_hsxsv;   //!
   TBranch        *b_hsysv;   //!
   TBranch        *b_hsxpsv;   //!
   TBranch        *b_hsypsv;   //!
   TBranch        *b_hsp;   //!
   TBranch        *b_hsdelta;   //!
   TBranch        *b_hsbeta;   //!
   TBranch        *b_hsbeta_k;   //!
   TBranch        *b_haernhit;   //!
   TBranch        *b_haernpe1;   //!
   TBranch        *b_haernpe2;   //!
   TBranch        *b_haernpe3;   //!
   TBranch        *b_hacnpe1m;   //!
   TBranch        *b_hacnpe2m;   //!
   TBranch        *b_hacnpe3m;   //!
   TBranch        *b_hact1;   //!
   TBranch        *b_hact2;   //!
   TBranch        *b_hact3;   //!
   TBranch        *b_hact1m;   //!
   TBranch        *b_hact2m;   //!
   TBranch        *b_hact3m;   //!
   TBranch        *b_haernum1;   //!
   TBranch        *b_haernum2;   //!
   TBranch        *b_haernum3;   //!
   TBranch        *b_hwatnhit;   //!
   TBranch        *b_hwatnpe1;   //!
   TBranch        *b_hwatnpe2;   //!
   TBranch        *b_hwcnpe1m;   //!
   TBranch        *b_hwcnpe2m;   //!
   TBranch        *b_hwct1;   //!
   TBranch        *b_hwct2;   //!
   TBranch        *b_hwct1m;   //!
   TBranch        *b_hwct2m;   //!
   TBranch        *b_hwatnum1;   //!
   TBranch        *b_hwatnum2;   //!
   TBranch        *b_htrkchi2;   //!
   TBranch        *b_htrkndf;   //!
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
   TBranch        *b_hscdepo;   //!
   TBranch        *b_hrftime;   //!
   TBranch        *b_hrfdiff;   //!
   TBranch        *b_estimefp;   //!
   TBranch        *b_etimetar;   //!
   TBranch        *b_esxfp;   //!
   TBranch        *b_esyfp;   //!
   TBranch        *b_esxpfp;   //!
   TBranch        *b_esypfp;   //!
   TBranch        *b_esxtar;   //!
   TBranch        *b_esytar;   //!
   TBranch        *b_esxptar;   //!
   TBranch        *b_esyptar;   //!
   TBranch        *b_esxsv;   //!
   TBranch        *b_esysv;   //!
   TBranch        *b_esxpsv;   //!
   TBranch        *b_esypsv;   //!
   TBranch        *b_esp;   //!
   TBranch        *b_esdelta;   //!
   TBranch        *b_etrkchi2;   //!
   TBranch        *b_etrkndf;   //!
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
   TBranch        *b_escdepo;   //!
   TBranch        *b_erftime;   //!
   TBranch        *b_erfdiff;   //!
   TBranch        *b_gbeamx;   //!
   TBranch        *b_gbeamy;   //!
   TBranch        *b_gespread;   //!
   TBranch        *b_gfbon;   //!
   TBranch        *b_gferon;   //!
   TBranch        *b_ipm3c07x;   //!
   TBranch        *b_ipm3c07y;   //!
   TBranch        *b_ipm3c08x;   //!
   TBranch        *b_ipm3c08y;   //!
   TBranch        *b_ipm3c11x;   //!
   TBranch        *b_ipm3c11y;   //!
   TBranch        *b_ipm3c12x;   //!
   TBranch        *b_ipm3c12y;   //!
   TBranch        *b_ipm3c14x;   //!
   TBranch        *b_ipm3c14y;   //!
   TBranch        *b_ipm3c16x;   //!
   TBranch        *b_ipm3c16y;   //!
   TBranch        *b_ipm3c17x;   //!
   TBranch        *b_ipm3c17y;   //!
   TBranch        *b_ipm3c18x;   //!
   TBranch        *b_ipm3c18y;   //!
   TBranch        *b_ipm3c20x;   //!
   TBranch        *b_ipm3c20y;   //!
   TBranch        *b_hnphys;   //!
   TBranch        *b_enphys;   //!
   TBranch        *b_cnphys;   //!
   TBranch        *b_eventid;   //!
   TBranch        *b_runnum;   //!
   TBranch        *b_coinrf;   //!
   TBranch        *b_cointime;   //!
   TBranch        *b_tg_hes;   //!
   TBranch        *b_tg_hks;   //!
   TBranch        *b_tg_g1;   //!
   TBranch        *b_tg_g2;   //!
   TBranch        *b_tg_g3;   //!
   TBranch        *b_tg_g4;   //!
   TBranch        *b_tg_g5;   //!
   TBranch        *b_tg_g6;   //!
   TBranch        *b_tg_unb;   //!
   TBranch        *b_happex;   //!
   TBranch        *b_hlucnhit;   //!
   TBranch        *b_hlucnpe1;   //!
   TBranch        *b_hlcnpe1m;   //!
   TBranch        *b_hlct1;   //!
   TBranch        *b_hlct1m;   //!
   TBranch        *b_hlucnum1;   //!

   ch2_analysis(Int_t run=0);
   virtual ~ch2_analysis();

   virtual Int_t    GetEntry(Long64_t entry);
   virtual Long64_t LoadTree(Long64_t entry);
   virtual void     Init(TTree *tree);
   virtual void     Loop();
   virtual void     Calibration();
   virtual Bool_t   Notify();
   virtual void     Show(Long64_t entry = -1);
   virtual bool     KaonCut();
};

#endif

#ifdef ch2_analysis_cxx
ch2_analysis::ch2_analysis(Int_t run) : fChain(0) 
{
   TTree *tree = 0;
   //TString fname = Form("/volatile/hallc/c-hks/E05-115/coin%d_1.root", run);
   TString fname = "/volatile/hallc/c-hks/E05-115/ch2_test.root";
   //TString fname = "/volatile/hallc/c-hks/E05-115/gogami_condensed/ch2_9-3.root";
   TFile *f = TFile::Open(fname);
   if (!f || f->IsZombie()) {
      std::cout << "Error opening file: " << fname << std::endl;
      return;
   }

   f->GetObject("h9500",tree);

   Init(tree);
}

ch2_analysis::~ch2_analysis()
{
   if (!fChain) return;
   delete fChain->GetCurrentFile();
}

Int_t ch2_analysis::GetEntry(Long64_t entry)
{
   if (!fChain) return 0;
   return fChain->GetEntry(entry);
}

Long64_t ch2_analysis::LoadTree(Long64_t entry)
{
   if (!fChain) return -5;
   Long64_t centry = fChain->LoadTree(entry);
   if (centry < 0) return centry;
   if (fChain->GetTreeNumber() != fCurrent) {
      fCurrent = fChain->GetTreeNumber();
      Notify();
   }
   return centry;
}

void ch2_analysis::Init(TTree *tree)
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
   fChain->SetBranchAddress("hsxtar", &hsxtar, &b_hsxtar);
   fChain->SetBranchAddress("hsytar", &hsytar, &b_hsytar);
   fChain->SetBranchAddress("hsxptar", &hsxptar, &b_hsxptar);
   fChain->SetBranchAddress("hsyptar", &hsyptar, &b_hsyptar);
   fChain->SetBranchAddress("hsxsv", &hsxsv, &b_hsxsv);
   fChain->SetBranchAddress("hsysv", &hsysv, &b_hsysv);
   fChain->SetBranchAddress("hsxpsv", &hsxpsv, &b_hsxpsv);
   fChain->SetBranchAddress("hsypsv", &hsypsv, &b_hsypsv);
   fChain->SetBranchAddress("hsp", &hsp, &b_hsp);
   fChain->SetBranchAddress("hsdelta", &hsdelta, &b_hsdelta);
   fChain->SetBranchAddress("hsbeta", &hsbeta, &b_hsbeta);
   fChain->SetBranchAddress("hsbeta_k", &hsbeta_k, &b_hsbeta_k);
   fChain->SetBranchAddress("haernhit", &haernhit, &b_haernhit);
   fChain->SetBranchAddress("haernpe1", &haernpe1, &b_haernpe1);
   fChain->SetBranchAddress("haernpe2", &haernpe2, &b_haernpe2);
   fChain->SetBranchAddress("haernpe3", &haernpe3, &b_haernpe3);
   fChain->SetBranchAddress("hacnpe1m", &hacnpe1m, &b_hacnpe1m);
   fChain->SetBranchAddress("hacnpe2m", &hacnpe2m, &b_hacnpe2m);
   fChain->SetBranchAddress("hacnpe3m", &hacnpe3m, &b_hacnpe3m);
   fChain->SetBranchAddress("hact1", &hact1, &b_hact1);
   fChain->SetBranchAddress("hact2", &hact2, &b_hact2);
   fChain->SetBranchAddress("hact3", &hact3, &b_hact3);
   fChain->SetBranchAddress("hact1m", &hact1m, &b_hact1m);
   fChain->SetBranchAddress("hact2m", &hact2m, &b_hact2m);
   fChain->SetBranchAddress("hact3m", &hact3m, &b_hact3m);
   fChain->SetBranchAddress("haernum1", &haernum1, &b_haernum1);
   fChain->SetBranchAddress("haernum2", &haernum2, &b_haernum2);
   fChain->SetBranchAddress("haernum3", &haernum3, &b_haernum3);
   fChain->SetBranchAddress("hwatnhit", &hwatnhit, &b_hwatnhit);
   fChain->SetBranchAddress("hwatnpe1", &hwatnpe1, &b_hwatnpe1);
   fChain->SetBranchAddress("hwatnpe2", &hwatnpe2, &b_hwatnpe2);
   fChain->SetBranchAddress("hwcnpe1m", &hwcnpe1m, &b_hwcnpe1m);
   fChain->SetBranchAddress("hwcnpe2m", &hwcnpe2m, &b_hwcnpe2m);
   fChain->SetBranchAddress("hwct1", &hwct1, &b_hwct1);
   fChain->SetBranchAddress("hwct2", &hwct2, &b_hwct2);
   fChain->SetBranchAddress("hwct1m", &hwct1m, &b_hwct1m);
   fChain->SetBranchAddress("hwct2m", &hwct2m, &b_hwct2m);
   fChain->SetBranchAddress("hwatnum1", &hwatnum1, &b_hwatnum1);
   fChain->SetBranchAddress("hwatnum2", &hwatnum2, &b_hwatnum2);
   fChain->SetBranchAddress("htrkchi2", &htrkchi2, &b_htrkchi2);
   fChain->SetBranchAddress("htrkndf", &htrkndf, &b_htrkndf);
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
   fChain->SetBranchAddress("hscdepo", &hscdepo, &b_hscdepo);
   fChain->SetBranchAddress("hrftime", &hrftime, &b_hrftime);
   fChain->SetBranchAddress("hrfdiff", &hrfdiff, &b_hrfdiff);
   fChain->SetBranchAddress("estimefp", &estimefp, &b_estimefp);
   fChain->SetBranchAddress("etimetar", &etimetar, &b_etimetar);
   fChain->SetBranchAddress("esxfp", &esxfp, &b_esxfp);
   fChain->SetBranchAddress("esyfp", &esyfp, &b_esyfp);
   fChain->SetBranchAddress("esxpfp", &esxpfp, &b_esxpfp);
   fChain->SetBranchAddress("esypfp", &esypfp, &b_esypfp);
   fChain->SetBranchAddress("esxtar", &esxtar, &b_esxtar);
   fChain->SetBranchAddress("esytar", &esytar, &b_esytar);
   fChain->SetBranchAddress("esxptar", &esxptar, &b_esxptar);
   fChain->SetBranchAddress("esyptar", &esyptar, &b_esyptar);
   fChain->SetBranchAddress("esxsv", &esxsv, &b_esxsv);
   fChain->SetBranchAddress("esysv", &esysv, &b_esysv);
   fChain->SetBranchAddress("esxpsv", &esxpsv, &b_esxpsv);
   fChain->SetBranchAddress("esypsv", &esypsv, &b_esypsv);
   fChain->SetBranchAddress("esp", &esp, &b_esp);
   fChain->SetBranchAddress("esdelta", &esdelta, &b_esdelta);
   fChain->SetBranchAddress("etrkchi2", &etrkchi2, &b_etrkchi2);
   fChain->SetBranchAddress("etrkndf", &etrkndf, &b_etrkndf);
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
   fChain->SetBranchAddress("escdepo", &escdepo, &b_escdepo);
   fChain->SetBranchAddress("erftime", &erftime, &b_erftime);
   fChain->SetBranchAddress("erfdiff", &erfdiff, &b_erfdiff);
   fChain->SetBranchAddress("gbeamx", &gbeamx, &b_gbeamx);
   fChain->SetBranchAddress("gbeamy", &gbeamy, &b_gbeamy);
   fChain->SetBranchAddress("gespread", &gespread, &b_gespread);
   fChain->SetBranchAddress("gfbon", &gfbon, &b_gfbon);
   fChain->SetBranchAddress("gferon", &gferon, &b_gferon);
   fChain->SetBranchAddress("ipm3c07x", &ipm3c07x, &b_ipm3c07x);
   fChain->SetBranchAddress("ipm3c07y", &ipm3c07y, &b_ipm3c07y);
   fChain->SetBranchAddress("ipm3c08x", &ipm3c08x, &b_ipm3c08x);
   fChain->SetBranchAddress("ipm3c08y", &ipm3c08y, &b_ipm3c08y);
   fChain->SetBranchAddress("ipm3c11x", &ipm3c11x, &b_ipm3c11x);
   fChain->SetBranchAddress("ipm3c11y", &ipm3c11y, &b_ipm3c11y);
   fChain->SetBranchAddress("ipm3c12x", &ipm3c12x, &b_ipm3c12x);
   fChain->SetBranchAddress("ipm3c12y", &ipm3c12y, &b_ipm3c12y);
   fChain->SetBranchAddress("ipm3c14x", &ipm3c14x, &b_ipm3c14x);
   fChain->SetBranchAddress("ipm3c14y", &ipm3c14y, &b_ipm3c14y);
   fChain->SetBranchAddress("ipm3c16x", &ipm3c16x, &b_ipm3c16x);
   fChain->SetBranchAddress("ipm3c16y", &ipm3c16y, &b_ipm3c16y);
   fChain->SetBranchAddress("ipm3c17x", &ipm3c17x, &b_ipm3c17x);
   fChain->SetBranchAddress("ipm3c17y", &ipm3c17y, &b_ipm3c17y);
   fChain->SetBranchAddress("ipm3c18x", &ipm3c18x, &b_ipm3c18x);
   fChain->SetBranchAddress("ipm3c18y", &ipm3c18y, &b_ipm3c18y);
   fChain->SetBranchAddress("ipm3c20x", &ipm3c20x, &b_ipm3c20x);
   fChain->SetBranchAddress("ipm3c20y", &ipm3c20y, &b_ipm3c20y);
   fChain->SetBranchAddress("hnphys", &hnphys, &b_hnphys);
   fChain->SetBranchAddress("enphys", &enphys, &b_enphys);
   fChain->SetBranchAddress("cnphys", &cnphys, &b_cnphys);
   fChain->SetBranchAddress("eventid", &eventid, &b_eventid);
   fChain->SetBranchAddress("runnum", &runnum, &b_runnum);
   fChain->SetBranchAddress("coinrf", &coinrf, &b_coinrf);
   fChain->SetBranchAddress("cointime", &cointime, &b_cointime);
   fChain->SetBranchAddress("tg_hes", &tg_hes, &b_tg_hes);
   fChain->SetBranchAddress("tg_hks", &tg_hks, &b_tg_hks);
   fChain->SetBranchAddress("tg_g1", &tg_g1, &b_tg_g1);
   fChain->SetBranchAddress("tg_g2", &tg_g2, &b_tg_g2);
   fChain->SetBranchAddress("tg_g3", &tg_g3, &b_tg_g3);
   fChain->SetBranchAddress("tg_g4", &tg_g4, &b_tg_g4);
   fChain->SetBranchAddress("tg_g5", &tg_g5, &b_tg_g5);
   fChain->SetBranchAddress("tg_g6", &tg_g6, &b_tg_g6);
   fChain->SetBranchAddress("tg_unb", &tg_unb, &b_tg_unb);
   fChain->SetBranchAddress("happex", &happex, &b_happex);
   fChain->SetBranchAddress("hlucnhit", &hlucnhit, &b_hlucnhit);
   fChain->SetBranchAddress("hlucnpe1", &hlucnpe1, &b_hlucnpe1);
   fChain->SetBranchAddress("hlcnpe1m", &hlcnpe1m, &b_hlcnpe1m);
   fChain->SetBranchAddress("hlct1", &hlct1, &b_hlct1);
   fChain->SetBranchAddress("hlct1m", &hlct1m, &b_hlct1m);
   fChain->SetBranchAddress("hlucnum1", &hlucnum1, &b_hlucnum1);
   Notify();
}

Bool_t ch2_analysis::Notify()
{
   // The Notify() function is called when a new file is opened. This
   // can be either for a new TTree in a TChain or when when a new TTree
   // is started when using PROOF. It is normally not necessary to make changes
   // to the generated code, but the routine can be extended by the
   // user if needed. The return value is currently not used.

   return kTRUE;
}

void ch2_analysis::Show(Long64_t entry)
{
// Print contents of entry.
// If entry is not specified, print current entry
   if (!fChain) return;
   fChain->Show(entry);
}

bool ch2_analysis::KaonCut()
{
   double m2 = hsp*hsp*(1.0/(hsbeta*hsbeta) - 1.0);

   return (m2 > -0.05 && m2 < 0.4 &&
           (haernpe1 + haernpe2 + haernpe3) < 20 &&
           (hwatnpe1 / gWC1_norm[(Int_t)(hwatnum1*2-2)] + hwatnpe2 / gWC2_norm[(Int_t)(hwatnum1*2-2)]) < 5);
}

#endif