#define analysis_replayed_cxx
#include "analysis_replayed.h"

#include <TH2.h>
#include <TStyle.h>
#include <TCanvas.h>
#include <cmath>
#include <fstream>
#include <iostream>
#include <string>

using namespace std;



// ============================================================
// Focal plane -> tarrget reconstruction
// ============================================================
static Double_t CalcF2T(const Double_t *P,
                        Double_t xf, Double_t xpf,
                        Double_t yf, Double_t ypf,
                        Int_t nMat)
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
   if (!fin) {
      cerr << "ERROR: cannot open optics matrix: " << filename << endl;
      return kFALSE;
   }
   for (Int_t i=0; i<210; i++) {
      Int_t a, b, c, d;
      fin >> P[i] >> a >> b >> c >> d;
      if (!fin) {
         cerr << "ERROR: failed reading optics matrix: "
              << filename << " at coefficient " << i << endl;
         return kFALSE;
      }
   }
   return kTRUE;
}


// ============================================================
// Hard-coded raster correction used in krd.C
// ============================================================
Double_t analysis_replayed::RasterCorrection(Double_t xt, Double_t yt, Int_t TargetFlag)
{
   if (TargetFlag == 7) xt /= 3.0;
   else if (TargetFlag != 1 && TargetFlag != 112) return 0.0;

   Double_t dmm = 0.00441113*xt - 0.0106527*xt*xt
                - 0.57073*xt*xt*xt + 4.20109*xt*xt*xt*xt;

   dmm += 0.000973207*yt - 0.0189651*yt*yt
         - 0.0653128*yt*yt*yt + 1.30222*yt*yt*yt*yt;

   return -dmm;
}

// ============================================================
// Missing mass calculation corresponding to final GetMM()

// Inputs:
//   eBeam  : incident electron total energy [GeV]
//   xpKaon : kaon x' slope
//   ypKaon : kaon y' slope
//   pKaon  : kaon momentum [GeV/c]
//   xpElectron : electron x' slope
//   ypElectron : electron y' slope
//   pElectron  : scattered electron momentum [GeV/c]
//  TargetFlag : target definition
//
// Return:
//   missing mass [GeV/c^2]
// ============================================================
Double_t analysis_replayed::GetMM(Double_t eBeam, Double_t xpKaon, Double_t ypKaon, Double_t pKaon,
               Double_t xpElectron, Double_t ypElectron, Double_t pElectron,
               Int_t TargetFlag)
{
   Double_t mtar = 0.0;
   Double_t mcore = 0.0;

   Double_t EEloss = 0.0;
   Double_t KEloss = 0.0;
   Double_t BEloss = 0.0;

   const Double_t elosscale  = 1.0;
   const Double_t elosscale2 = 0.97;
   const Double_t b_scale    = 1.0;
   const Double_t h2o_scale  = 1.55;

   switch (TargetFlag) {
      case 1: // Hydrogen in Polyethylene. Lambda and Sigma0 production.
         mtar = gProtonMass;
         mcore = 0.0;
         EEloss = 374e-6*elosscale*elosscale2;
         KEloss = 465e-6*elosscale*elosscale2;
         BEloss = 374e-6*elosscale*elosscale2;
         break;

      case 2: // Carbon in Polyethylene. Lambda and Sigma0 production.
         mtar = gC12Mass;
         mcore = gB11Mass;
         EEloss = 374e-6*elosscale*elosscale2;
         KEloss = 465e-6*elosscale*elosscale2;
         BEloss = 374e-6*elosscale*elosscale2;
         break;

      case 7: // Lithium-7.
         mtar = gLi7Mass;
         mcore = gHe6Mass;
         EEloss = 111e-6*elosscale;
         KEloss = 138e-6*elosscale;
         BEloss = 110e-6*elosscale;
         break;

      case 10: // Boron-10.
         mtar = gB10Mass;
         mcore = gBe9Mass;
         EEloss = 34e-6*elosscale*b_scale;
         KEloss = 43e-6*elosscale*b_scale;
         BEloss = 34e-6*elosscale*b_scale;
         break;

      case 12: // Thin Carbon-12.
         mtar = gC12Mass;
         mcore = gB11Mass;
         EEloss = 55e-6*elosscale;
         KEloss = 68e-6*elosscale;
         BEloss = 55e-6*elosscale;
         break;

      case 52: // Chromium-52.
         mtar = gCr52Mass;
         mcore = gV51Mass;
         EEloss = 86e-6*elosscale;
         KEloss = 99e-6*elosscale;
         BEloss = 86e-6*elosscale;
         break;

      case 101: // Hydrogen in Water. Lambda and Sigma0 production.
         mtar = gProtonMass;
         mcore = 0.0;
         EEloss = 402e-6*elosscale*h2o_scale;
         KEloss = 442e-6*elosscale*h2o_scale;
         BEloss = 400e-6*elosscale*h2o_scale;
         break;

      case 112: // Thick Carbon-12.
         mtar = gC12Mass;
         mcore = gB11Mass;
         EEloss = 374e-6*elosscale*elosscale2;
         KEloss = 465e-6*elosscale*elosscale2;
         BEloss = 374e-6*elosscale*elosscale2;
         break;

      default:
         cout << "Unsupported TargetFlag in GetMM(): " << TargetFlag << endl;
         return -999.0;
   }

   // Final beam-angle correction from krd.C
   const Double_t beamang_theta = 5.30e-3;
   const Double_t beamang_phi   = 0.0;

   const Double_t beam_xp = tan(beamang_theta)*cos(beamang_phi);
   const Double_t beam_yp = tan(beamang_theta)*sin(beamang_phi);

   xpKaon -= beam_xp;
   ypKaon -= beam_yp;
   xpElectron += beam_xp;
   ypElectron += beam_yp;

   // Energy-loss correction
   Double_t eKaon = sqrt(pKaon*pKaon + gKaonMass*gKaonMass) + KEloss;
   pKaon = sqrt(eKaon*eKaon - gKaonMass*gKaonMass);

   Double_t eElectron = sqrt(pElectron*pElectron + gElectronMass*gElectronMass) + EEloss;
   pElectron = sqrt(eElectron*eElectron - gElectronMass*gElectronMass);

   eBeam -= BEloss;
   Double_t pBeam = sqrt(eBeam*eBeam - gElectronMass*gElectronMass);

   // Momentum components (x' = px/pz, y' = py/pz)
   Double_t pkz = pKaon / sqrt(1.0 + xpKaon*xpKaon + ypKaon*ypKaon);
   Double_t pkx = pkz * xpKaon;
   Double_t pky = pkz * ypKaon;

   Double_t pez = pElectron / sqrt(1.0 + xpElectron*xpElectron + ypElectron*ypElectron);
   Double_t pex = pez * xpElectron;
   Double_t pey = pez * ypElectron;

   // Missing four-momentum
   Double_t dE  = eBeam + mtar - eKaon - eElectron;
   Double_t dpx = pBeam * sin(beamang_theta) * cos(beamang_phi) - pex - pkx;
   Double_t dpy = pBeam * sin(beamang_theta) * sin(beamang_phi) - pey - pky;
   Double_t dpz = pBeam * cos(beamang_theta) - pez - pkz;

   Double_t mm2 = dE*dE - dpx*dpx - dpy*dpy - dpz*dpz;
   if (mm2 < 0.0) return -999.0;

   return sqrt(mm2) - mcore;
}

void analysis_replayed::Loop()
{
   //* Define Histograms */
   TH1D *h_m2    = new TH1D("h_m2","Mass squared",500,-0.5,1.5);
   TH1D *h_ctime = new TH1D("h_ctime","Coincidence time",500,-70,-40);
   TH1D *h_mm    = new TH1D("h_mm","CH_{2}(e,e'K^{+})#Lambda,#Sigma^{0}",1600,-200,200);
   //TH1D *h_mm    = new TH1D("h_mm","^{52}Cr(e,e'K^{+})^{52}_{#Lambda}V",320,-40,40);

   TH1D *h_kacnpe    = new TH1D("h_kacnpe","Kaon AC NPE (AC1+AC2+AC3)",60,0,60);
   TH1D *h_kwcnpe    = new TH1D("h_kwcnpe","Kaon WC NPE (WC1+WC2)",1000,0,10);
   TH2D *h_kacnpe_m2 = new TH2D("h_kacnpe_m2","Kaon AC vs Mass squared",60,0,60,500,-0.5,1.5);
   TH2D *h_kwcnpe_m2 = new TH2D("h_kwcnpe_m2","Kaon WC vs Mass squared",1000,0,10,500,-0.5,1.5);

   // Kaon kinematics
   TH1D *h_kmom     = new TH1D("h_kmom","Kaon momentum",500,0.5,1.5);
   TH1D *h_ktimetar = new TH1D("h_ktimetar","Kaon time at target",500,-150,-50);
   TH1D *h_ktimefp  = new TH1D("h_ktimefp","Kaon time at focal plane",500,-100,0);
   TH1D *h_kxptar   = new TH1D("h_kxptar","Kaon x' at target",500,-0.3,0.1);
   TH1D *h_kyptar   = new TH1D("h_kyptar","Kaon y' at target",500,-0.2,0.2);

   TH1D *h_kxfp   = new TH1D("h_kxfp","Kaon x at focal plane",500,-100,100);
   TH1D *h_kyfp   = new TH1D("h_kyfp","Kaon y at focal plane",500,-20,20);
   TH1D *h_kxpfp  = new TH1D("h_kxpfp","Kaon x' at focal plane",500,-0.2,0.2);
   TH1D *h_kypfp  = new TH1D("h_kypfp","Kaon y' at focal plane",500,-0.2,0.2);

   // Electron kinematics
   TH1D *h_emom     = new TH1D("h_emom","Electron momentum",500,0.5,1.5);
   TH1D *h_etimetar = new TH1D("h_etimetar","Electron time at target",500,-100,100);
   TH1D *h_etimefp  = new TH1D("h_etimefp","Electron time at focal plane",500,-100,0);
   TH1D *h_exptar   = new TH1D("h_exptar","Electron x' at target",500,-0.3,0.1);
   TH1D *h_eyptar   = new TH1D("h_eyptar","Electron y' at target",500,-0.2,0.2);

   TH1D *h_exfp   = new TH1D("h_exfp","Electron x at focal plane",500,-100,100);
   TH1D *h_eyfp   = new TH1D("h_eyfp","Electron y at focal plane",500,-20,20);
   TH1D *h_expfp  = new TH1D("h_expfp","Electron x' at focal plane",500,-0.2,0.2);
   TH1D *h_eypfp  = new TH1D("h_eypfp","Electron y' at focal plane",500,-0.2,0.2);

   // Kaon kinematics correlations
   TH2D *h_kxptar_kyptar = new TH2D("h_kxptar_kyptar","Kaon x' vs y' at target",500,-0.3,0.1,500,-0.2,0.2);
   TH2D *h_kxfp_kyfp = new TH2D("h_kxfp_kyfp","Kaon x vs y at focal plane",500,-100,100,500,-20,20);
   TH2D *h_kxpfp_kypfp   = new TH2D("h_kxpfp_kypfp","Kaon x' vs y' at focal plane",500,-0.2,0.2,500,-0.2,0.2);
   TH2D *h_kxfp_hxpfp = new TH2D("h_kxfp_hxpfp","Kaon x vs x' at focal plane",500,-100,100,500,-0.2,0.2);
   TH2D *h_kyfp_hypfp = new TH2D("h_kyfp_hypfp","Kaon y vs y' at focal plane",300,-30,30,200,-0.1,0.1);

   // Electron kinematics correlations
   TH2D *h_exptar_eyptar = new TH2D("h_exptar_eyptar","Electron x' vs y' at target",500,-0.3,0.1,500,-0.2,0.2);
   TH2D *h_exfp_eyfp = new TH2D("h_exfp_eyfp","Electron x vs y at focal plane",500,-100,100,500,-20,20);
   TH2D *h_expfp_eypfp = new TH2D("h_expfp_eypfp","Electron x' vs y' at focal plane",500,-0.2,0.2,500,-0.2,0.2);
   TH2D *h_exfp_expfp = new TH2D("h_exfp_expfp","Electron x vs x' at focal plane",500,-100,100,500,-0.2,0.2);
   TH2D *h_eyfp_eypfp = new TH2D("h_eyfp_eypfp","Electron y vs y' at focal plane",300,-30,30,200,-0.1,0.1);

   SetHist1D(h_m2, "Mass squared [(GeV/c^{2})^{2}]", "Counts");
   SetHist1D(h_ctime, "Coincidence time [ns]", "Counts");
   SetHist1D(h_mm, "-B_{#Lambda} [MeV]", "Counts/(0.25 MeV)");

   SetHist1D(h_kacnpe, "NPE(AC1+AC2+AC3)", "Counts");
   SetHist1D(h_kwcnpe, "Normalized NPE(WC1+WC2)", "Counts");
   SetHist2D(h_kacnpe_m2, "NPE(AC1+AC2+AC3)", "Mass squared [(GeV/c^{2})^{2}]");
   SetHist2D(h_kwcnpe_m2, "Normalized NPE(WC1+WC2)", "Mass squared [(GeV/c^{2})^{2}]");

   SetHist1D(h_kmom, "Kaon momentum [(GeV/c)]", "Counts");
   SetHist1D(h_ktimetar, "Time [ns]", "Counts");
   SetHist1D(h_ktimefp, "Time [ns]", "Counts");
   SetHist1D(h_kxptar, "x' angle [mrad]", "Counts");
   SetHist1D(h_kyptar, "y' angle [mrad]", "Counts");

   SetHist1D(h_kxfp, "x-position", "Counts");
   SetHist1D(h_kyfp, "y-position", "Counts");
   SetHist1D(h_kxpfp, "x' angle [mrad]", "Counts");
   SetHist1D(h_kypfp, "y' angle [mrad]", "Counts");

   SetHist1D(h_emom, "Electron momentum [(GeV/c)]", "Counts");
   SetHist1D(h_etimetar, "Time [ns]", "Counts");
   SetHist1D(h_etimefp, "Time [ns]", "Counts");
   SetHist1D(h_exptar, "x' angle [mrad]", "Counts");
   SetHist1D(h_eyptar, "y' angle [mrad]", "Counts");

   SetHist1D(h_exfp, "x-position", "Counts");
   SetHist1D(h_eyfp, "y-position", "Counts");
   SetHist1D(h_expfp, "x' angle [mrad]", "Counts");
   SetHist1D(h_eypfp, "y' angle [mrad]", "Counts");

   SetHist2D(h_kxptar_kyptar, "x' angle [mrad]", "y' angle [mrad]");
   SetHist2D(h_kxfp_kyfp, "x-position", "y-position");
   SetHist2D(h_kxpfp_kypfp, "x' angle [mrad]", "y' angle [mrad]");
   SetHist2D(h_kxfp_hxpfp, "x-position", "x' angle [mrad]");
   SetHist2D(h_kyfp_hypfp, "y-position", "y' angle [mrad]");

   SetHist2D(h_exptar_eyptar, "x' angle [mrad]", "y' angle [mrad]");
   SetHist2D(h_exfp_eyfp, "x-position", "y-position");
   SetHist2D(h_expfp_eypfp, "x' angle [mrad]", "y' angle [mrad]");
   SetHist2D(h_exfp_expfp, "x-position", "x' angle [mrad]");
   SetHist2D(h_eyfp_eypfp, "y-position", "y' angle [mrad]");


   // ============================================================
   // Load tuned matrices listed in reconData.dat
   // Only p, x', y' matrices are needed here.
   // The first two entries (plK/plE) are read only to preserve file order.
   // ============================================================
   Double_t momK[210] = {0.0};
   Double_t xptK[210] = {0.0};
   Double_t yptK[210] = {0.0};
   Double_t momE[210]  = {0.0};
   Double_t xptE[210]  = {0.0};
   Double_t yptE[210]  = {0.0};
   char str_momK[256], str_xptK[256], str_yptK[256];
   char str_momE[256],  str_xptE[256],  str_yptE[256];
   ifstream recon("matrices/reconData.dat");
   if (!recon) {
      cerr << "ERROR: cannot open reconData.dat" << endl;
      return;
   }
   recon >> str_momK >> str_xptK >> str_yptK
         >> str_momE >> str_xptE >> str_yptE;
   if (!recon) {
      cerr << "ERROR: failed reading filenames from reconData.dat" << endl;
      return;
   }
   recon.close();
   if (!ReadMatrix(str_momK, momK)) return;
   if (!ReadMatrix(str_xptK, xptK)) return;
   if (!ReadMatrix(str_yptK, yptK)) return;
   if (!ReadMatrix(str_momE, momE)) return;
   if (!ReadMatrix(str_xptE, xptE)) return;
   if (!ReadMatrix(str_yptE, yptE)) return;
   cout << "Loaded tuned matrices from reconData.dat" << endl;


   //* Event Loop */
   if (fChain == 0) return;
   Long64_t nAccepted = 0;

   Long64_t nentries = fChain->GetEntriesFast();
   for (Long64_t jentry=0; jentry<nentries;jentry++) {
      Long64_t ientry = LoadTree(jentry);
      if (ientry < 0) break;
      fChain->GetEntry(jentry);
      
      // ===== Calculate physical quantities =====
      // Tuned HKS optics from focal-plane variables
      Double_t xK  = hsxfp  / 60.0;
      Double_t xpK = hsxpfp / 0.15;
      Double_t yK  = hsyfp  / 6.0;
      Double_t ypK = hsypfp / 0.01;
      Double_t pKaon  = CalcF2T(momK, xK, xpK, yK, ypK, 6);
      Double_t xpKaon = CalcF2T(xptK, xK, xpK, yK, ypK, 6);
      Double_t ypKaon = CalcF2T(yptK, xK, xpK, yK, ypK, 6);
      // Tuned HES optics from focal-plane variables
      Double_t xE  = esxfp / 25.0;
      Double_t xpE = esxpfp / 0.1;
      Double_t yE  = esyfp / 3.0;
      Double_t ypE = (esypfp + 0.03) / 0.01;
      Double_t pElectron = CalcF2T(momE, xE, xpE, yE, ypE, 6);
      Double_t expt      = CalcF2T(xptE, xE, xpE, yE, ypE, 6);
      Double_t eypt      = CalcF2T(yptE, xE, xpE, yE, ypE, 6);
      // Electron sign convention used by GetMM()/krd.C
      Double_t xpElectron = -expt;
      Double_t ypElectron = -eypt;


      Double_t pBeam = hallcp / 1000.0; // Momentum of beam in GeV/c
      Double_t betaKaon = hsbeta; // Beta of kaon
      Double_t m2 = hsp*hsp*(1.0/pow(betaKaon,2) - 1.0); // Mass squared in (GeV/c^2)^2

      // Special beam-momentum correction used in krd.C
      if ((runnum >= 76222 && runnum <= 76290) ||
          (runnum >= 76310 && runnum <= 76315)) {
         pBeam = 2344.08/1000.0;
      }

      Double_t eBeam = sqrt(pBeam*pBeam + gElectronMass*gElectronMass); // Energy of beam in GeV
      Double_t MissingMass = GetMM(eBeam,
                                   xpKaon, ypKaon, pKaon,
                                   xpElectron, ypElectron, pElectron,
                                   gTargetFlag);
      if (MissingMass > -900.0) {
         MissingMass += RasterCorrection(gbeamx, gbeamy, gTargetFlag);
      }

      // ===== Fill histograms =====
      if (KaonCut() && CtimeCut()) { 
         nAccepted++;
         h_m2->Fill(m2);
         h_ctime->Fill(htimetar - etimetar);
         if (MissingMass > -900.0) {
            h_mm->Fill(1000.0*(MissingMass - gLambdaMass));
         }

         h_kacnpe->Fill(haernpe1 + haernpe2 + haernpe3);
         h_kwcnpe->Fill(hwatnpe1 / gWC1_norm[(Int_t)(hwatnum1*2-2)] + hwatnpe2 / gWC2_norm[(Int_t)(hwatnum1*2-2)]);
         h_kacnpe_m2->Fill(haernpe1 + haernpe2 + haernpe3, m2);
         h_kwcnpe_m2->Fill(hwatnpe1 / gWC1_norm[(Int_t)(hwatnum1*2-2)] + hwatnpe2 / gWC2_norm[(Int_t)(hwatnum1*2-2)], m2);

         h_kmom->Fill(pKaon);
         h_ktimetar->Fill(htimetar);
         h_ktimefp->Fill(hstimefp);
         h_kxptar->Fill(xpKaon);
         h_kyptar->Fill(ypKaon);

         h_kxfp->Fill(hsxfp);
         h_kyfp->Fill(hsyfp);
         h_kxpfp->Fill(hsxpfp);
         h_kypfp->Fill(hsypfp);

         h_emom->Fill(pElectron);
         h_etimetar->Fill(etimetar);
         h_etimefp->Fill(estimefp);
         h_exptar->Fill(expt);
         h_eyptar->Fill(eypt);

         h_exfp->Fill(esxfp);
         h_eyfp->Fill(esyfp);
         h_expfp->Fill(esxpfp);
         h_eypfp->Fill(esypfp);

         h_kxptar_kyptar->Fill(xpKaon, ypKaon);
         h_kxfp_kyfp->Fill(hsxfp, hsyfp);
         h_kxpfp_kypfp->Fill(hsxpfp, hsypfp);
         h_kxfp_hxpfp->Fill(hsxfp, hsxpfp);
         h_kyfp_hypfp->Fill(hsyfp, hsypfp); 

         h_exptar_eyptar->Fill(expt, eypt);
         h_exfp_eyfp->Fill(esxfp, esyfp);
         h_expfp_eypfp->Fill(esxpfp, esypfp);
         h_exfp_expfp->Fill(esxfp, esxpfp);
         h_eyfp_eypfp->Fill(esyfp, esypfp); 

         }
      }
   }

   cout << "Total accepted events: " << nAccepted << endl;

   //* Draw Histograms */
   TCanvas *c1 = new TCanvas("c1","M2/ctime cut & missing mass",1000,1000);
   c1->Divide(2,2);
   c1->cd(1); h_mm->Draw();

   TCanvas *c2 = new TCanvas("c2","Kaon Cherenkov",800,1200);
   c2->Divide(2,3);
   c2->cd(1); h_m2->Draw();
   c2->cd(2); h_ctime->Draw();
   c2->cd(3); gPad->SetLogy(); h_kacnpe->Draw();
   c2->cd(4); gPad->SetLogy(); h_kwcnpe->Draw();
   gStyle->SetPalette(kRainbow);
   c2->cd(5); gPad->SetLogz(); h_kacnpe_m2->Draw("COLZ");
   c2->cd(6); gPad->SetLogz(); h_kwcnpe_m2->Draw("COLZ");

   TCanvas *c3 = new TCanvas("c3","Kinematics",800,1200);
   c3->Divide(2,3);
   c3->cd(1); h_kmom->Draw();
   c3->cd(2); h_emom->Draw();
   c3->cd(3); h_ktimetar->Draw();
   c3->cd(4); h_etimetar->Draw();
   c3->cd(5); h_ktimefp->Draw();
   c3->cd(6); h_etimefp->Draw();

   TCanvas *c4 = new TCanvas("c4","Kaon kinematics",800,1200);
   c4->Divide(2,3);
   c4->cd(1); h_kxptar->Draw();
   c4->cd(2); h_kyptar->Draw();
   c4->cd(3); h_kxfp->Draw();
   c4->cd(4); h_kyfp->Draw();
   c4->cd(5); h_kxpfp->Draw();
   c4->cd(6); h_kypfp->Draw();

   TCanvas *c5 = new TCanvas("c5","Electron kinematics",800,1200);
   c5->Divide(2,3);
   c5->cd(1); h_exptar->Draw();
   c5->cd(2); h_eyptar->Draw();
   c5->cd(3); h_exfp->Draw();
   c5->cd(4); h_eyfp->Draw();
   c5->cd(5); h_expfp->Draw();
   c5->cd(6); h_eypfp->Draw();

   TCanvas *c6 = new TCanvas("c6","Kaon correlations",800,1200);
   c6->Divide(2,3);
   gStyle->SetPalette(kRainbow);
   c6->cd(1); gPad->SetLogz(); h_kxptar_kyptar->Draw("COLZ");
   c6->cd(2); gPad->SetLogz(); h_kxfp_kyfp->Draw("COLZ");
   c6->cd(3); gPad->SetLogz(); h_kxpfp_kypfp->Draw("COLZ");
   c6->cd(4); gPad->SetLogz(); h_kxfp_hxpfp->Draw("COLZ");
   c6->cd(5); gPad->SetLogz(); h_kyfp_hypfp->Draw("COLZ");

   TCanvas *c7 = new TCanvas("c7","Electron correlations",800,1200);
   c7->Divide(2,3);
   gStyle->SetPalette(kRainbow);
   c7->cd(1); gPad->SetLogz(); h_exptar_eyptar->Draw("COLZ");
   c7->cd(2); gPad->SetLogz(); h_exfp_eyfp->Draw("COLZ");
   c7->cd(3); gPad->SetLogz(); h_expfp_eypfp->Draw("COLZ");
   c7->cd(4); gPad->SetLogz(); h_exfp_expfp->Draw("COLZ");
   c7->cd(5); gPad->SetLogz(); h_eyfp_eypfp->Draw("COLZ");
   
   TFile *fin = fChain->GetCurrentFile();
   fin->cd();
   h_emom->Write("h_emom", TObject::kOverwrite);
   fin->Save();
}
