#include<string>
#include<stdio.h>

void loopana(char* date,int itenum)
{
   char filename[100];
   sprintf(filename,"HESmatrices/%s/%d/fit.log",date,itenum);
   cout << filename <<endl;
   ifstream fitlog(filename);
   string s1;
   int n_l=0;
   int n_s=0;
   int n_g=0;
   int n_a=0;
   TGraph *g1=new TGraph();
   TGraph *g2=new TGraph();
   TGraph *g3=new TGraph();
   TGraph *g4=new TGraph();
   TGraph *g5=new TGraph();
   TGraph *g6=new TGraph();
   TGraph *g7=new TGraph();
   TGraph *g8=new TGraph();
   while (getline(fitlog,s1)){
      int l=s1.length();
      int ind_L=s1.find("Lambda");
      int ind_S=s1.find("Sigma");
      int ind_gs=s1.find("12LBgs");
      int ind_a=s1.find("12LBa");
      char* c1[100];
      char* c2[100];
      char* c3[100];
      char* c4[100];
      float mu_l[100],sig_l[100];
      float mu_s[100],sig_s[100];
      float mu_g[100],sig_g[100];
      float mu_a[100],sig_a[100];
      if (ind_L>0){
         //cout <<"lambda"<<" "<< s1 << endl;
         int i=1;
         strtok(s1.c_str()," ");
         while ((c1[i]=strtok(NULL," "))!=NULL){
            //cout << i<<" "<<c1[i]<<endl;
            i++;
         }
         sscanf(c1[2],"%f",&mu_l[n_l]);
         sscanf(c1[3],"%f",&sig_l[n_l]);
         //cout << n_l <<"mu="<<mu_l[n_l]<<"sigma="<<sig_l[n_l]<<endl;
         g1->SetPoint(n_l,n_l,(mu_l[n_l]-1.115683)*1e3+0.7);
         g2->SetPoint(n_l,n_l,sig_l[n_l]*1e3);
         n_l++;
      }
      if (ind_S>0){
         //cout <<"sigma"<<" "<< s1 << endl;
         int i=1;
         strtok(s1.c_str()," ");
         while ((c2[i]=strtok(NULL," "))!=NULL){
            //cout << i<<" "<<c2[i]<<endl;
            i++;
         }
         sscanf(c2[2],"%f",&mu_s[n_s]);
         sscanf(c2[3],"%f",&sig_s[n_s]);
         g3->SetPoint(n_s,n_s,(mu_s[n_s]-1.192642)*1e3+0.7);
         g4->SetPoint(n_s,n_s,sig_s[n_s]*1e3);
         n_s++;
      }
      if (ind_gs>0){
         //cout <<"12LBgs"<<" "<< s1 << endl;
         int i=1;
         strtok(s1.c_str()," ");
         while ((c3[i]=strtok(NULL," "))!=NULL){
            //cout << i<<" "<<c3[i]<<endl;
            i++;
         }
         sscanf(c3[2],"%f",&mu_g[n_g]);
         sscanf(c3[3],"%f",&sig_g[n_g]);
         g5->SetPoint(n_g,n_g,(mu_g[n_g]+0.0114)*1e3+0.7);
         g6->SetPoint(n_g,n_g,sig_g[n_g]*1e3);
         n_g++;
      }
      if (ind_a>0){
         //cout <<"12LBa"<<" "<< s1 << endl;
         int i=1;
         strtok(s1.c_str()," ");
         while ((c4[i]=strtok(NULL," "))!=NULL){
            //cout << i<<" "<<c4[i]<<endl;
            i++;
         }
         sscanf(c4[2],"%f",&mu_a[n_a]);
         sscanf(c4[3],"%f",&sig_a[n_a]);
         g7->SetPoint(n_a,n_a,(mu_a[n_a]+0.0114)*1e3+0.7);
         g8->SetPoint(n_a,n_a,sig_a[n_a]*1e3);
         n_a++;
      }
   }
   fitlog.close();
   
   double max1=1.5;
   double min1=0.1;
   TCanvas *ca1 = new TCanvas("ca1","ca1");
   g1->SetLineWidth(2);
   g1->SetLineColor(kRed);
   g1->SetMaximum(max1);
   g1->SetMinimum(min1);
   g1->SetTitle(";number of iteration;#sigma [MeV]");
   g1->Draw("apl");
   double xmin1=g1->GetXaxis()->GetXmin();
   double xmax1=g1->GetXaxis()->GetXmax();
   g2->SetFillColor(kBlue);
   g2->SetFillStyle(3002);
   g2->SetMarkerStyle(3);
   g2->SetMaximum(max1);
   g2->SetMinimum(min1);
   g2->Draw("B");
   TGaxis *ax1 = new TGaxis(xmax1,min1,xmax1,max1,
         min1-0.8,max1-0.8,510,"+L");
   ax1->SetTitle("#Lambda_{calc}-#Lambda_{PDG} [MeV]");
   ax1->SetTitleColor(kRed);
   ax1->SetLineColor(kRed);
   ax1->SetLabelColor(kRed);
   ax1->Draw();
   
   double max2=1.5;
   double min2=0.1;
   TCanvas *ca2 = new TCanvas("ca2","ca2");
   g3->SetLineWidth(2);
   g3->SetLineColor(kRed);
   g3->SetMaximum(max1);
   g3->SetMinimum(min1);
   g3->SetTitle(";number of iteration;#sigma [MeV]");
   g3->Draw("apl");
   double xmin2=g3->GetXaxis()->GetXmin();
   double xmax2=g3->GetXaxis()->GetXmax();
   g4->SetFillColor(kBlue);
   g4->SetFillStyle(3002);
   g4->SetMarkerStyle(3);
   g4->SetMaximum(max2);
   g4->SetMinimum(min2);
   g4->Draw("B");
   //TGaxis *ax2 = new TGaxis(xmax2,min2,xmax2,ca2->GetUymax(),
   TGaxis *ax2 = new TGaxis(xmax2,min2,xmax2,max2,
         min2-0.8,max2-0.8,510,"+L");
   ax2->SetTitle("#Sigma_{calc}-#Sigma_{PDG} [MeV]");
   ax2->SetTitleColor(kRed);
   ax2->SetLineColor(kRed);
   ax2->SetLabelColor(kRed);
   ax2->Draw();
   
   double max3=1.5;
   double min3=0.1;
   TCanvas *ca3 = new TCanvas("ca3","ca3");
   g5->SetLineWidth(2);
   g5->SetLineColor(kRed);
   g5->SetMaximum(max3);
   g5->SetMinimum(min3);
   g5->SetTitle(";number of iteration;#sigma [MeV]");
   g5->Draw("apl");
   double xmin3=g5->GetXaxis()->GetXmin();
   double xmax3=g5->GetXaxis()->GetXmax();
   g6->SetFillColor(kBlue);
   g6->SetFillStyle(3002);
   g6->SetMarkerStyle(3);
   g6->SetMaximum(max3);
   g6->SetMaximum(min3);
   g6->Draw("B");
   //TGaxis *ax3 = new TGaxis(xmax3,min3,xmax3,ca3->GetUymax(),
   TGaxis *ax3 = new TGaxis(xmax3,min3,xmax3,max3,
         min3-0.8,max3-0.8,510,"+L");
   ax3->SetTitle("^{12}_{#Lambda}B_{calc}-^{12}_{#Lambda}B_{PDG} [MeV]");
   ax3->SetTitleColor(kRed);
   ax3->SetLineColor(kRed);
   ax3->SetLabelColor(kRed);
   ax3->Draw();
   
   double max4=1.5;
   double min4=0.1;
   TCanvas *ca4 = new TCanvas("ca4","ca4");
   g7->SetLineWidth(2);
   g7->SetLineColor(kRed);
   g7->SetMaximum(max4);
   g7->SetMinimum(min4);
   g7->SetTitle(";number of iteration;#sigma [MeV]");
   g7->Draw("apl");
   double xmin4=g5->GetXaxis()->GetXmin();
   double xmax4=g5->GetXaxis()->GetXmax();
   g8->SetFillColor(kBlue);
   g8->SetFillStyle(3002);
   g8->SetMarkerStyle(3);
   g8->SetMaximum(max4);
   g8->SetMaximum(min4);
   g8->Draw("B");
   //TGaxis *ax4 = new TGaxis(xmax4,min4,xmax4,ca4->GetUymax(),
   TGaxis *ax4 = new TGaxis(xmax4,min4,xmax4,max4,
         min4-0.8,max4-0.8,510,"+L");
   ax4->SetTitle("^{12}_{#Lambda}B_{calc}-^{12}_{#Lambda}B_{PDG} [MeV]");
   ax4->SetTitleColor(kRed);
   ax4->SetLineColor(kRed);
   ax4->SetLabelColor(kRed);
   ax4->Draw();
   
   /*char pic_c1[100];
   char pic_c2[100];
   char pic_c3[100];
   char pic_c4[100];
   sprintf(pic_c1,"pictures/mtune_Lfitana_%s-%d.png",date,itenum);
   sprintf(pic_c2,"pictures/mtune_Sfitana_%s-%d.png",date,itenum);
   sprintf(pic_c3,"pictures/mtune_Gfitana_%s-%d.png",date,itenum);
   sprintf(pic_c4,"pictures/mtune_Afitana_%s-%d.png",date,itenum);
   ca1->Print(pic_c1);
   ca2->Print(pic_c2);
   ca3->Print(pic_c3);
   ca4->Print(pic_c4);*/
   
}
