#include <iostream>
#include <fstream>

void showthePeaks(char* rFileName){
   TCanvas *c = new TCanvas("c","c");
   c->Print("~/pictures/peaks.pdf(","pdf");
   for (int n=0;n<30;n++){
      TFile *f = new TFile(rFileName);
      TH1F *his[4];
      TF1  *fun[4];
      char hname[4][100];
      char fname[4][100];
      sprintf(hname[0],"hl%d",n);
      sprintf(hname[1],"hs%d",n);
      sprintf(hname[2],"hcgs%d",n);
      sprintf(hname[3],"ha%d",n);
      sprintf(fname[0],"fl%d",n);
      sprintf(fname[1],"fs%d",n);
      sprintf(fname[2],"fcgs%d",n);
      sprintf(fname[3],"fa%d",n);
      for (int i=0;i<4;i++){
         his[i] = (TH1F*)f->Get(hname[i]);
         fun[i] =  (TF1*)f->Get(fname[i]);
      }
      TCanvas *c1 = new TCanvas("c1","c1");
      c1->Divide(2,2);
      for (int i=0;i<4;i++){
         c1->cd(i+1);his[i]->Draw();fun[i]->Draw("same");
      }
      c1->Print("~/pictures/peaks.pdf","pdf");
   }
   c->Print("~/pictures/peaks.pdf]","pdf");
}


