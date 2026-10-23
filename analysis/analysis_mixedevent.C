#ifdef analysis_condensed_cxx
#undef analysis_condensed_cxx
#endif

#include "analysis_condensed.h"

#include <TH1D.h>
#include <TF1.h>
#include <TRandom3.h>
#include <TROOT.h>
#include <TMath.h>
#include <cmath>
#include <map>
#include <vector>

using namespace std;

struct MixEvent {
   Int_t run;
   Double_t pKaon, xpKaon, ypKaon;
   Double_t pElectron, xpElectron, ypElectron;
   Double_t hallcp, beamx, beamy;
};

Double_t QFConv(Double_t *xx, Double_t *par)
{
   Double_t x     = xx[0];
   Double_t a1    = par[0];
   Double_t a2    = par[1];
   Double_t a3    = par[2];
   Double_t sigma = par[3];

   Double_t z = x/sigma;
   Double_t Phi = 0.5*(1.0 + TMath::Erf(z/TMath::Sqrt(2.0)));
   Double_t phi = TMath::Exp(-0.5*z*z)/TMath::Sqrt(2.0*TMath::Pi());
   Double_t I1 = x*Phi + sigma*phi;
   Double_t I2 = (x*x + sigma*sigma)*Phi + x*sigma*phi;
   Double_t I3 = (x*x*x + 3.0*x*sigma*sigma)*Phi + sigma*(x*x + 2.0*sigma*sigma)*phi;

   return a1*I1 + a2*I2 + a3*I3;
}

// ============================================================
// Mixed event analysis for accidental background estimation
// ============================================================
void analysis_condensed::MixedEventAna()
{
   if (fChain == 0) return;
   map<Int_t, vector<MixEvent>> pool;

   int rangel = -200;
   int ranger = +200;
   int nbins = 4*(ranger - rangel);
   TH1D *h_mm     = new TH1D("h_mm","H_{2}O(e,e'K^{+})#Lambda,#Sigma^{0}",nbins,rangel,ranger);
   //TH1D *h_mm     = new TH1D("h_mm","^{52}Cr(e,e'K^{+})^{52}_{#Lambda}V",nbins,rangel,ranger);
   TH1D *h_mm_mix = new TH1D("h_mm_mix", "Accidental background", nbins, rangel, ranger);
   TH1D *h_mm_subbg = new TH1D("h_mm_subbg", "Subtracted background", nbins, rangel, ranger);
   SetHist1D(h_mm, "-B_{#Lambda} [MeV]", "Counts/(0.25 MeV)");
   SetHist1D(h_mm_mix, "-B_{#Lambda} [MeV]", "Counts/(0.25 MeV)", kRed);
   SetHist1D(h_mm_subbg, "-B_{#Lambda} [MeV]", "Counts/(0.25 MeV)");

   Long64_t nentries = fChain->GetEntriesFast();
   for (Long64_t jentry=0; jentry<nentries;jentry++) {
      Long64_t ientry = LoadTree(jentry);
      if (ientry < 0) break;
      fChain->GetEntry(jentry);

      // ===== Missing Mass Calculation =====
      if (KaonCut() && CtimeCut()) {
         Double_t pKaon = kmom; // Momentum of kaon in GeV/c
         Double_t pElectron = emom; // Momentum of scattered electron in GeV/c
         Double_t pBeam = hallcp / 1000.0; // Momentum of beam in GeV/c
         Double_t betaKaon = hsbeta; // Beta of kaon
         Double_t xpKaon = kxpt; // x' slope of kaon at target
         Double_t ypKaon = kypt; // y' slope of kaon at target
         Double_t xpElectron = -expt; // Electron sign convention used in krd.C
         Double_t ypElectron = -eypt; // Electron sign convention used in krd.C
         Double_t m2 = hsp*hsp*(1.0/pow(betaKaon,2) - 1.0); // Mass squared in (GeV/c^2)^2

         Double_t eBeam = sqrt(pBeam*pBeam + gElectronMass*gElectronMass); // Energy of beam in GeV
         Double_t MissingMass = GetMM(eBeam,
                                      xpKaon, ypKaon, pKaon,
                                      xpElectron, ypElectron, pElectron,
                                      gTargetFlag);

         if (MissingMass > -900.0) {
            MissingMass += RasterCorrection(gbeamx, gbeamy, gTargetFlag);
            h_mm->Fill(1000.0*(MissingMass - gLambdaMass));
         }
      }

      // ===== Mixed Event Pool =====
      if (KaonCut() && !CtimeCut()) {
         MixEvent mix;
         mix.run = (Int_t)runnum;

         mix.pKaon  = kmom;
         mix.xpKaon = kxpt;
         mix.ypKaon = kypt;

         mix.pElectron  = emom;
         mix.xpElectron = -expt;
         mix.ypElectron = -eypt;

         mix.hallcp = hallcp;
         mix.beamx  = gbeamx;
         mix.beamy  = gbeamy;

         pool[mix.run].push_back(mix);
      }
   }

   // ===== Mixed Event Analysis =====
   TRandom3 rng(0);
   Int_t nMix = 200;
   for (const auto &rp : pool) {
      const vector<MixEvent> &evs = rp.second;
      size_t N = evs.size();

      for (size_t i=0; i<N; i++) {
         const MixEvent &k = evs[i];

         for (Int_t imix=0; imix<nMix; imix++) {
            size_t j = rng.Integer(N-1);
            const MixEvent &e = evs[j];

            Double_t mpBeam = k.hallcp/1000.0;
            Double_t meBeam = sqrt(mpBeam*mpBeam + gElectronMass*gElectronMass);
            Double_t mMissingMass = GetMM(meBeam,
                                         k.xpKaon, k.ypKaon, k.pKaon,
                                         e.xpElectron, e.ypElectron, e.pElectron,
                                         gTargetFlag);

            if (mMissingMass > -900.0) {
               mMissingMass += RasterCorrection(k.beamx, k.beamy, gTargetFlag);
               h_mm_mix->Fill(1000.0*(mMissingMass-gLambdaMass));
            }
         }
      }
   }

   h_mm_mix->Scale(1.0/nMix/8.0);
   h_mm_subbg->Add(h_mm, h_mm_mix, 1.0, -1.0);
   

   // ============================================================
   // Missing mass fitting
   // ============================================================
   TF1 *f_voigt = new TF1("f_voigt", "[0]*TMath::Voigt(x-[1],[2],[3])", 0.0-2.0,0.0+2.0);

   f_voigt->SetParameters(h_mm_subbg->GetMaximum(),0.0,1.0,0.1);
   h_mm_subbg->Fit(f_voigt,"R");

   Double_t sigma_res = f_voigt->GetParameter(2);

   cout << "Gaussian resolution sigma = " << sigma_res << " MeV" << endl;
   cout << "Gaussian resolution FWHM = " << 2.35482*sigma_res << " MeV" << endl;

   // ===== cubic polynomial convolved with Gaussian resolution =====
   Double_t fitMin = 0.0;
   Double_t fitMax = 60.0;

   //TF1 *f_poly3_conv = new TF1("f_poly3_conv","10+[0]*x+[1]*(x*x+[3]*[3])+[2]*(x*x*x+3*x*[3]*[3])",-20, 20);

   //f_poly3_conv->SetParameters(1.0,0.0,0.0,sigma_res);
   //f_poly3_conv->FixParameter(3,sigma_res);
   //h_mm_subbg->Fit(f_poly3_conv,"R+","",fitMax, fitMin);
   TF1 *f_qf = new TF1("f_qf", QFConv, -10.0, 30.0, 4);

   f_qf->SetParameters(1.0,0.0,0.0,sigma_res);

   f_qf->FixParameter(3, sigma_res);

   h_mm_subbg->Fit(f_qf, "R","", fitMin, fitMax);

   TCanvas *c1 = new TCanvas("c1","Missing mass",1000,1000);
   c1->Divide(2,2);
   h_mm_mix->SetFillStyle(3001);
   c1->cd(1); h_mm->Draw("HIST"); h_mm_mix->Draw("HIST SAME");
   c1->cd(2); h_mm_subbg->Draw(); f_qf->Draw("same");
}