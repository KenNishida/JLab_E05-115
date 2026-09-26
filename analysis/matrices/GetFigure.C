void GetFigure(int n){
   TFile *f = new TFile("output.root");
   char str0[100];
   char str1[100];
   char str2[100];
   char str0f[100];
   char str1f[100];
   char str2f[100];
   sprintf(str0f,"output.pdf(");
   sprintf(str1f,"output.pdf]");
   sprintf(str2f,"output.pdf");
   TCanvas *c1 = new TCanvas("c1");
   c1->Divide(2,2);
   c1->cd(1);hl->Draw("");
   c1->cd(2);hs->Draw("");
   c1->cd(3);hbg1->Draw("");
   c1->Print(str0f,"pdf");       
   c1->Clear();       
   c1->Divide(2,2);
   TH1F *h0;
   TH1F *h1;
   TH1F *h2;
   TH1F *h3;
   for (int i=0;i<n;i++){
      sprintf(str0,"hl%d",i);
      sprintf(str1,"hs%d",i);
      sprintf(str2,"hbg1%d",i);
      h0=(TH1F*)f->Get(str0);
      h1=(TH1F*)f->Get(str1);
      h2=(TH1F*)f->Get(str2);
      c1->cd(1);h0->Draw("");
      c1->cd(2);h1->Draw("");
      c1->cd(3);h2->Draw("");
      c1->Print(str2f);
   }
   c1->Print(str1f,"pdf");       

}
