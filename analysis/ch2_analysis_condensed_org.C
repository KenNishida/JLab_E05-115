#define ch2_analysis_condensed_cxx
#include "ch2_analysis_condensed.h"
#include <TH2.h>
#include <TStyle.h>
#include <TCanvas.h>
using namespace std;

void ch2_analysis_condensed::Loop()
{
   //* Define Histograms */
   TH1D *h_m2    = new TH1D("h_m2","Mass squared",500,-0.5,1.5);
   TH1D *h_ctime = new TH1D("h_ctime","Coincidence time",500,-70,-40);
   TH1D *h_mm    = new TH1D("h_mm","Missing mass",500,-0.2,0.2);

   TH1D *h_m2_ref    = new TH1D("h_m2_ref","Mass squared (hmsq branch)",500,-0.5,1.5);
   TH1D *h_ctime_ref = new TH1D("h_ctime_ref","Coincidence time (ctime branch)",500,-70,-40);
   TH1D *h_mm_ref    = new TH1D("h_mm_ref","Missing mass (mm branch)",500,-0.2,0.2);

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
   SetHist1D(h_mm, "Missing mass [(GeV/c^{2})]", "Counts");

   SetHist1D(h_ctime_ref, "Coincidence time [ns]", "Counts");
   SetHist1D(h_mm_ref, "Missing mass [(GeV/c^{2})]", "Counts");
   SetHist1D(h_m2_ref, "Mass squared [(GeV/c^{2})^{2}]", "Counts");

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

   //* Event Loop */
   if (fChain == 0) return;

   Long64_t nentries = fChain->GetEntriesFast();
   for (Long64_t jentry=0; jentry<nentries;jentry++) {
      Long64_t ientry = LoadTree(jentry);
      if (ientry < 0) break;
      fChain->GetEntry(jentry);
      
      // ===== Calculate physical quantities =====
      Double_t pKaon = hsp; // Momentum of kaon in GeV/c
      Double_t pElectron = esp; // Momentum of scattered electron in GeV/c
      Double_t pBeam = hallcp / 1000.0; // Momentum of beam in GeV/c
      Double_t betaKaon = hsbeta; // Beta of kaon
      Double_t xpKaon = atan(hsxptar); // Angle of beam-kaon in radians
      Double_t ypKaon = atan(hsyptar); // Angle of beam-kaon in radians
      Double_t xpElectron = atan(esxptar); // Angle of beam-scattered electron in radians
      Double_t ypElectron = atan(esyptar); // Angle of beam-scattered electron in radians
      Double_t m2 = pKaon*pKaon*(1.0/pow(betaKaon,2) - 1.0); // Mass squared in (GeV/c^2)^2
      Double_t eKaon = sqrt(pow(pKaon,2) + pow(gKaonMass,2)) + elossKaon; // Energy of kaon in GeV
      Double_t eElectron = sqrt(pow(pElectron,2) + pow(gElectronMass,2)) + elossElectron; // Energy of scattered electron in GeV
      Double_t eBeam = pBeam - elossBeam; // Energy of beam in GeV
      Double_t MissingMass = sqrt(pow(eBeam + gProtonMass - eKaon - eElectron, 2) - (pow(pElectron*cos(ypElectron)*sin(xpElectron) + pKaon*cos(ypKaon)*sin(xpKaon), 2) + pow(pElectron*sin(ypElectron) + pKaon*sin(ypKaon), 2) + pow(pBeam - pElectron*cos(ypElectron)*cos(xpElectron) - pKaon*cos(ypKaon)*cos(xpKaon), 2))); // Missing mass in GeV/c^2

      // ===== Fill histograms =====
      if (KaonCut()) { //} && CtimeCut()) {
         h_m2->Fill(m2);
         h_ctime->Fill(htimetar - etimetar);
         h_mm->Fill(MissingMass - gLambdaMass);

         h_m2_ref->Fill(hmsq);
         h_ctime_ref->Fill(ctime);
         h_mm_ref->Fill(mm - gLambdaMass);

         h_kacnpe->Fill(haernpe1 + haernpe2 + haernpe3);
         h_kwcnpe->Fill(hwatnpe1 / gWC1_norm[(Int_t)(hwatnum1*2-2)] + hwatnpe2 / gWC2_norm[(Int_t)(hwatnum1*2-2)]);
         h_kacnpe_m2->Fill(haernpe1 + haernpe2 + haernpe3, m2);
         h_kwcnpe_m2->Fill(hwatnpe1 / gWC1_norm[(Int_t)(hwatnum1*2-2)] + hwatnpe2 / gWC2_norm[(Int_t)(hwatnum1*2-2)], m2);

         h_kmom->Fill(pKaon);
         h_ktimetar->Fill(htimetar);
         h_ktimefp->Fill(hstimefp);
         h_kxptar->Fill(hsxptar);
         h_kyptar->Fill(hsyptar);

         h_kxfp->Fill(hsxfp);
         h_kyfp->Fill(hsyfp);
         h_kxpfp->Fill(hsxpfp);
         h_kypfp->Fill(hsypfp);

         h_emom->Fill(pElectron);
         h_etimetar->Fill(etimetar);
         h_etimefp->Fill(estimefp);
         h_exptar->Fill(esxptar);
         h_eyptar->Fill(esyptar);

         h_exfp->Fill(esxfp);
         h_eyfp->Fill(esyfp);
         h_expfp->Fill(esxpfp);
         h_eypfp->Fill(esypfp);

         h_kxptar_kyptar->Fill(hsxptar, hsyptar);
         h_kxfp_kyfp->Fill(hsxfp, hsyfp);
         h_kxpfp_kypfp->Fill(hsxpfp, hsypfp);
         h_kxfp_hxpfp->Fill(hsxfp, hsxpfp);
         h_kyfp_hypfp->Fill(hsyfp, hsypfp); 

         h_exptar_eyptar->Fill(esxptar, esyptar);
         h_exfp_eyfp->Fill(esxfp, esyfp);
         h_expfp_eypfp->Fill(esxpfp, esypfp);
         h_exfp_expfp->Fill(esxfp, esxpfp);
         h_eyfp_eypfp->Fill(esyfp, esypfp); 
      }
   }


   //* Draw Histograms */
   TCanvas *c1 = new TCanvas("c1","M2/ctime cut & missing mass",800,1200);
   h_mm->GetYaxis()->SetRangeUser(0, 1.1*h_mm_ref->GetMaximum());
   h_mm_ref->GetYaxis()->SetRangeUser(0, 1.1*h_mm_ref->GetMaximum());

   c1->Divide(2,3);
   c1->cd(1); h_m2->Draw();
   c1->cd(2); h_m2_ref->Draw();
   c1->cd(3); h_ctime->Draw();
   c1->cd(4); h_ctime_ref->Draw();
   c1->cd(5); h_mm->Draw();
   c1->cd(6); h_mm_ref->Draw();

   TCanvas *c2 = new TCanvas("c2","Kaon Cherenkov",800,1200);
   c2->Divide(2,3);
   c2->cd(1); gPad->SetLogy(); h_kacnpe->Draw();
   c2->cd(2); gPad->SetLogy(); h_kwcnpe->Draw();
   gStyle->SetPalette(kRainbow);
   c2->cd(3); gPad->SetLogz(); h_kacnpe_m2->Draw("COLZ");
   c2->cd(4); gPad->SetLogz(); h_kwcnpe_m2->Draw("COLZ");

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
   
}










void ch2_analysis_condensed::Calibration()
{
   Int_t nsegment = 12*2 - 1;
   Double_t wc1_norm[nsegment];
   Double_t wc2_norm[nsegment];

   TH1D *h_wc1_npe[nsegment];
   TH1D *h_wc2_npe[nsegment];

   for (Int_t i=0; i<nsegment; i++) {
      h_wc1_npe[i] = new TH1D(Form("h_wc1_npe_%d",i), Form("WC1-NPE_%.1f",1+0.5*i), 200, 0, 200);
      h_wc2_npe[i] = new TH1D(Form("h_wc2_npe_%d",i), Form("WC2-NPE_%.1f",1+0.5*i), 200, 0, 200);
      SetHist1D(h_wc1_npe[i], "NPE", "Counts");
      SetHist1D(h_wc2_npe[i], "NPE", "Counts");
   }

   //* Event Loop */
   if (fChain == 0) return;

   Long64_t nentries = fChain->GetEntriesFast();

   for (Long64_t jentry=0; jentry<nentries; jentry++) {
      Long64_t ientry = LoadTree(jentry);
      if (ientry < 0) break;

      fChain->GetEntry(jentry);

      for (Int_t i=0; i<nsegment; i++) {

         if (KaonCut() && hwatnum1 == 1 + 0.5*i && hwatnpe1 > 0) {
            h_wc1_npe[i]->Fill(hwatnpe1);
         }

         if (KaonCut() && hwatnum2 == 1 + 0.5*i && hwatnpe2 > 0) {
            h_wc2_npe[i]->Fill(hwatnpe2);
         }
      }
   }

   TF1 *fit_wc1[nsegment];
   TF1 *fit_wc2[nsegment];
   //* Gaussian fits */
   for (Int_t i=0; i<nsegment; i++) {
      fit_wc1[i] = new TF1(Form("fit_wc1_%d",i), "gaus", 0, 200);
      fit_wc2[i] = new TF1(Form("fit_wc2_%d",i), "gaus", 0, 200);

      // ---------- WC1 ----------
      Double_t peak1 = h_wc1_npe[i]->GetBinCenter(h_wc1_npe[i]->GetMaximumBin());
      Double_t sigma = 10.0;

      h_wc1_npe[i]->Fit(fit_wc1[i], "RQ", "", peak1 - sigma, peak1 + 2*sigma);
      wc1_norm[i] = fit_wc1[i]->GetParameter(1);

      // ---------- WC2 ----------
      Double_t peak2 = h_wc2_npe[i]->GetBinCenter(h_wc2_npe[i]->GetMaximumBin());

      h_wc2_npe[i]->Fit(fit_wc2[i], "RQ", "", peak2 - sigma, peak2 + 2*sigma);
      wc2_norm[i] = fit_wc2[i]->GetParameter(1);
   }

   TCanvas *c1 = new TCanvas("c1","WC1 NPE",2000,1200);
   c1->Divide(6,4);
   for (Int_t i=0; i<nsegment; i++) {
      c1->cd(i+1);
      h_wc1_npe[i]->Draw(); fit_wc1[i]->Draw("same");
   }
   TCanvas *c2 = new TCanvas("c2","WC2 NPE",2000,1200);
   c2->Divide(6,4);
   for (Int_t i=0; i<nsegment; i++) {
      c2->cd(i+1);
      h_wc2_npe[i]->Draw(); fit_wc2[i]->Draw("same");
   }

   cout << "WC1 {";
   for (Int_t i=0; i<nsegment; i++) {
      cout << " " << fixed << setprecision(2) << wc1_norm[i] << ",";
   }
   cout << "}\n" << "WC2 {";
   for (Int_t i=0; i<nsegment; i++) {
      cout << " " << fixed << setprecision(2) << wc2_norm[i] << ",";
   }
   cout << "}" << endl;

   TH2D *h_wc1_npe_seg    = new TH2D("h_wc1_npe_seg","WC1 NPE vs segment#",24,0.5,12.5,200,0,200);
   TH2D *h_wc2_npe_seg    = new TH2D("h_wc2_npe_seg","WC2 NPE vs segment#",24,0.5,12.5,200,0,200);
   TH2D *h_wc1_nornpe_seg    = new TH2D("h_wc1_nornpe_seg","WC1 Normalized NPE vs segment#",24,0.5,12.5,100,0,3);
   TH2D *h_wc2_nornpe_seg    = new TH2D("h_wc2_nornpe_seg","WC2 Normalized NPE vs segment#",24,0.5,12.5,100,0,3);
   SetHist2D(h_wc1_npe_seg, "Segment#", "NPE");
   SetHist2D(h_wc2_npe_seg, "Segment#", "NPE");
   SetHist2D(h_wc1_nornpe_seg, "Segment#", "Normalized NPE");
   SetHist2D(h_wc2_nornpe_seg, "Segment#", "Normalized NPE");

   h_wc1_npe_seg->Fill(hwatnum1, hwatnpe1);
   h_wc2_npe_seg->Fill(hwatnum2, hwatnpe2);
   h_wc1_nornpe_seg->Fill(hwatnum1, hwatnpe1 / gWC1_norm[(Int_t)(hwatnum1*2-2)]);
   h_wc2_nornpe_seg->Fill(hwatnum2, hwatnpe2 / gWC2_norm[(Int_t)(hwatnum2*2-2)]);

   TCanvas *c3 = new TCanvas("c3","WC NPE",800,800);
   c3->Divide(2,2);
   gStyle->SetPalette(kRainBow);
   c3->cd(1); gPad->SetLogz(); h_wc1_npe_seg->Draw("COLZ");
   c3->cd(2); gPad->SetLogz(); h_wc2_npe_seg->Draw("COLZ");
   c3->cd(3); gPad->SetLogz(); h_wc1_nornpe_seg->Draw("COLZ");
   c3->cd(4); gPad->SetLogz(); h_wc2_nornpe_seg->Draw("COLZ");
}