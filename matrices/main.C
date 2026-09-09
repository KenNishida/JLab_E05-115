/*
  "main.C"
  
  Main program of matrix tuning
  Written by D.Kawama
  Modified by Toshi Gogami 
*/

#include "parameter.h"
#include "mtune.h"
#include <fstream>
#include <iostream>
#include <stdlib.h>
#include <math.h>
#include <omp.h>
#include <TRandom.h>
#include <TFile.h>
#include <TF1.h>
#include <TH2F.h>
#include <TSpectrum.h>
#include <TProfile.h>
#include "FitLog.hh"
#include <TCanvas.h>

using namespace std;

int MaxIterate=0;
float gE[nTrig][nParam];
float gK[nTrig][nParam];
float gB[nTrig];
float gxfE[nTrig];
float gxpfE[nTrig];
float gyfE[nTrig];
float gypfE[nTrig];
float gxfK[nTrig];
float gxpfK[nTrig];
float gyfK[nTrig];
float gypfK[nTrig];
float gmomK0[nTrig];
float gmomE0[nTrig];
float gdmm[nTrig];
float gdmomE[nTrig];
float gdxptE[nTrig];
float gdyptE[nTrig];
float gdmomK[nTrig];
float gdxptK[nTrig];
float gdyptK[nTrig];
float mm0[nTrig];
float rasmomE[10];
float rasxptE[10];
float rasyptE[10];
float rasmomK[10];
float rasxptK[10];
float rasyptK[10];
float resMM1;
float resMM2;
float gsmass;
float psmass;
float gsmm;      // Added by Tosh , 28Jan2013
float psmm;      // Added by Tosh , 28Jan2013
float gsmm_high; // Added by Tosh , 5Feb2013
float psmm_high; // Added by Tosh , 5Feb2013
float gsmm_ch2;  // Added by Tosh , 28Feb2013
float gsmm_b10;  // Added by Tosh , 12June2013
float gsmm_li7;  // Added by Tosh , 14Aug2013
float cemm;      // Added by Toshi, 6Nov2013
float cemm_high; // Added by Toshi, 6Nov2013
float mm1_cr52;  // Added by Toshi, 10Jan2014
float w_chisql;
float w_chisql_h2o;
float w_chisqs_h2o;// Added by Tosh , 4Feb2013
float w_chisqs;
float w_chisqgsl;
float w_chisqgsh;
float w_chisqpk;
float w_chisqpe;
float w_chisqgs_ch2;// Added by Tosh , 23Feb2013
float w_chisqgs_b10;// Added by Tosh , 12June2013
float w_chisqgs_li7;// Added by Tosh , 14Aug2013
float w_chisqcel;   // Added by Tosh , 6Nov2013
float w_chisqceh;   // Added by Tosh , 6Nov2013
float w_chisq_cr52; // Added by Tosh , 10Jan2014
int gEvflag[nTrig];
int oa[allParam];
int ob[allParam];
int oc[allParam];
int od[allParam];
int nLambda;
int nLambda_h2o;
int nSigma_h2o;
int nSigma;
int nLB12gsl;
int nLB12gsh;
int nLB12psl;
int nLB12psh;
int nLB12gsch2;
int nLBe10gs;
int nLHe7gs;
int nLB12cel;
int nLB12ceh;
int nLV52_1;
ofstream logfile;
float mincut;
float maxcut;
int cutflag;

int main(int argc, char* argv[]){
  TCanvas*c_dragon = new TCanvas("c_dragon","c_dragon");
  if (argc==2){
    MaxIterate=atoi(argv[1]);
  }
  else {
    cout <<"usage: ./mtune MaxIterate"<<endl;
    return 0;
  }
  float gsmass0=0.;
  float psmass0=0.;
  float cut1a=1.111,cut1b=1.120;    // Lambda in CH2
  float cut2a=1.188,cut2b=1.195;    // Sigma0 in CH2
  float cut3a=1.100,cut3b=1.120;    // 12LB g.s. (low)
  float cut4a=1.120,cut4b=1.130;    // 12LB g.s. (high)
  float cut5a=1.120,cut5b=1.130;    // Lambda in H2O
  float cut6a=1.120,cut6b=1.130;    // 12LB p-shell (low)
  float cut7a=1.120,cut7b=1.130;    // 12LB p-shell (high)
  float cut8a=1.188,cut8b=1.195;    // Sigma0 in H2O
  float cut9a=1.100,cut9b=1.108;    // 12LB g.s. in CH2 target
  //float cut10a=1.114683, cut10b=1.121683; // for 10LBe core excited
  float cut10a = 1.106,cut10b=1.108;// 10LBe g.s.              
  //float cut11a = 1.1069,cut11b=1.113;//7LHe g.s.              
  float cut11a = 1.107,cut11b=1.113;//7LHe g.s.              
  /*
  float cut12a = 1.106183,cut12b= 1.108183;// 12LB ce1 (low)
  float cut13a = 1.106183,cut13b= 1.108183;// 12LB ce1 (high)
  */
  float cut12a = 1.106983,cut12b= 1.108183;// 12LB ce1 (low), 15Nov2013
  float cut13a = 1.106983,cut13b= 1.108183;// 12LB ce1 (high), 15Nov2013
  float cut14a = 1.106983,cut14b= 1.108183;// 52LV_1, 10Jan2014
  
  char ch2fname[300];
  char c12pfname[300];
  char h2ofname[300];
  char c12fname[300];
  char c12_1fname[300];
  char b10fname[300];
  char li7fname[300];
  char cr52fname[300];
  char cMKmom[200];
  char cMKxpt[200];
  char cMKypt[200];
  char cMEmom[200];
  char cMExpt[200];
  char cMEypt[200];
  
  ifstream partune("partune.in");
  partune >>cut1a >>cut1b;  // Lambda in CH2
  partune >>cut2a >>cut2b;  // Sigma0 in CH2
  partune >>cut3a >>cut3b;  // 12LB g.s. (low)
  partune >>cut4a >>cut4b;  // 12LB g.s. (high)
  partune >>cut5a >>cut5b;  // Lambda in H2O
  partune >>cut6a >>cut6b;  // 12LB p-shell (low)
  partune >>cut10a>>cut10b; // 10LBe g.s. 
  partune >>cut11a>>cut11b; //  7LHe g.s.
  partune >>cut12a>>cut12b; // 12LB ce1 (low)
  partune >>cut13a>>cut13b; // 12LB ce1 (low)
  partune >>cut14a>>cut14b; // 52LV 1
  partune >>ch2fname;   // CH2 ROOT file
  partune >>c12fname;   // Low  current C12 ROOT file
  partune >>c12_1fname; // High current C12 ROOT file
  partune >>h2ofname;   // H2O ROOT file
  partune >>c12pfname;  // 12LB from CH2 
  partune >>b10fname;   // 10B ROOT file
  partune >>li7fname;   // 7Li ROOT file
  partune >>cr52fname;  // 52Cr ROOT file
  partune >>cMKmom;
  partune >>cMKxpt;
  partune >>cMKypt;
  partune >>cMEmom;
  partune >>cMExpt;
  partune >>cMEypt;
  partune >>w_chisql>>w_chisqs
	  >>w_chisqgsl>>w_chisqgsh
	  >>w_chisql_h2o>>w_chisqs_h2o;
  partune >>w_chisqpk>>w_chisqpe;
  partune >>w_chisqgs_ch2;
  partune >>w_chisqgs_b10;
  partune >>w_chisqgs_li7;
  partune >>w_chisqcel >> w_chisqceh;
  partune >>w_chisq_cr52;
  partune >>mincut>>maxcut>>cutflag;
  partune >>gsmass0;
  partune.close();
  
  cut7a=cut6a;
  cut7b=cut6b;
  cut8a=cut2a;
  cut8b=cut2b;
  
  resMM1=6.e-4;
  resMM2=2.e-4;
  gsmass=gsmass0;
  psmass=gsmass0-11.e-3;
  gsmm  = gsmass;     // Toshi
  //gsmm  = -11.40 + ml;// Toshi
  psmm  = psmass;     // Toshi
  gsmm_high = gsmass; // Toshi
  psmm_high = psmass; // Toshi
  gsmm_ch2  = gsmass; // Toshi
  gsmm_b10  = -8.8e-3 + ml; // Toshi
  gsmm_li7  = -6.00e-3 + ml; // Toshi 
  cemm      = -8.55e-3 + ml; // Toshi , 6Nov2013
  cemm_high = -8.55e-3 + ml; // Toshi , 6Nov2013
  //mm1_cr52  = -16.0e-3 + ml; // Toshi , 10Jan2014
  mm1_cr52  = 1.0e-3 + ml; // Toshi , 28Jan2014
  //gsmm_li7  = -5.7e-3 + ml; // Toshi 
  //gsmm_li7  = -5.6e-3 + ml; // Toshi 
  //gsmm_li7  = -5.5e-3 + ml; // Toshi 
  //gsmm_li7  = -5.4e-3 + ml; // Toshi 
  //gsmm_li7  = -5.48e-3 + ml; // Toshi 
  //gsmm_li7  = -5.4e-3 + ml; // Toshi 
  
  
  
  double coeff[allParam];
  float cut1[]={cut1a,cut1b};   // Lambda in CH2		
  float cut2[]={cut2a,cut2b};	 // Sigma0 in CH2		
  float cut3[]={cut3a,cut3b};	 // 12LB g.s. (low)		
  float cut4[]={cut4a,cut4b};	 // 12LB g.s. (high)	
  float cut5[]={cut5a,cut5b};	 // Lambda in H2O		
  //float cut5[]={1.114, 1.116};	 // Lambda in H2O		
  float cut6[]={cut6a,cut6b};	 // 12LB p-shell (low)	
  float cut7[]={cut7a,cut7b};	 // 12LB p-shell (high)	
  float cut8[]={cut8a,cut8b};	 // Sigma0 in H2O		
  float cut9[]={cut9a,cut9b};	 // 12LB g.s. in CH2 target	
  float cut10[]={cut10a,cut10b};// 10LBe g.s.              
  float cut11[]={cut11a,cut11b};//  7LHe g.s.              
  float cut12[]={cut12a,cut12b};//  12LB core excited 1 (low)
  float cut13[]={cut13a,cut13b};//  12LB core excited 1 (high)
  float cut14[]={cut14a,cut14b};//  52LV
  char str1[300];  // Lambda in CH2		
  char str2[300];  // Sigma0 in CH2		
  char str3[300];  // 12LB g.s. (low)		
  char str4[300];  // 12LB g.s. (high)	
  char str5[300];  // Lambda in H2O		
  char str6[300];  // 12LB p-shell (low)	
  char str7[300];  // 12LB p-shell (high)	
  char str8[300];  // Sigma0 in H2O		
  char str9[300];  // 12LB g.s. in CH2 target	
  char str10[300]; // 10LBe g.s.              
  char str11[300]; //  7LHe g.s.                
  char str12[300]; //  12LB core excited 1 (low)
  char str13[300]; //  12LB core excited 1 (high)
  char str14[300]; //  52LV
  sprintf(str1,"lambda");
  sprintf(str2,"sigma");
  sprintf(str3,"LB12gsl");
  sprintf(str4,"LB12gsh");
  sprintf(str5,"lambda_h2o");
  sprintf(str6,"LB12ps_low");
  sprintf(str7,"LB12ps_high");
  sprintf(str8,"sigma_h2o");
  sprintf(str9,"LB12gs_ch2");
  sprintf(str10,"LBe10gs");
  sprintf(str11,"LHe7gs");
  sprintf(str12,"LB12ce1_low");
  sprintf(str13,"LB12ce1_high");
  sprintf(str14,"LV52_1");
  TF1 *f1 = new TF1("f1","[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",cut1[0],cut1[1]);
  TF1 *f2 = new TF1("f2","[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",cut2[0],cut2[1]);
  TF1 *f3 = new TF1("f3","[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",cut3[0],cut3[1]);
  TF1 *f4 = new TF1("f4","[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",cut4[0],cut4[1]);
  TF1 *f5 = new TF1("f5","[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",cut5[0],cut5[1]);
  TF1 *f6 = new TF1("f6","[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",cut6[0],cut6[1]);
  TF1 *f7 = new TF1("f7","[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",cut7[0],cut7[1]);
  TF1 *f8 = new TF1("f8","[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",cut8[0],cut8[1]);
  TF1 *f9 = new TF1("f9","[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",cut9[0],cut9[1]);
  TF1 *f10 = new TF1("f10","[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",cut10[0],cut10[1]);
  TF1 *f11 = new TF1("f11","[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",cut11[0],cut11[1]);
  TF1 *f12 = new TF1("f12","[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",cut12[0],cut12[1]);
  TF1 *f13 = new TF1("f13","[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",cut13[0],cut13[1]);
  TF1 *f14 = new TF1("f14","[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",cut14[0],cut14[1]); 
  //TF1* func_mmxt;
  //TF1* func_mmyt;
  //TF1* func_mmxt_;
  //TF1* func_mmyt_;
  
  f1->SetParameters(100,1.115683,8.0e-4,50);
  //f1->SetParLimits(0,0,1000);
  f1->SetParLimits(1,1.115383,1.115983);
  f1->SetParLimits(2,2.0e-4,8.5e-4);
  //f1->SetParLimits(3,30,60);
  f2->SetParameters(50,1.192642,8.0e-4,20);
  // f2->SetParLimits(0,2,100);
  f2->SetParLimits(1,1.192342,1.192942);
  f2->SetParLimits(2,5.0e-4,8.5e-4);
  //f2->SetParLimits(3,10,30);
  
  // ----- 12LB g.s. (low) --------------------
  //f3->SetParameters(0.0,1.104283,5.0e-4,5);
  f3->SetParameters(0.0,1.104283,3.3e-4,5);
  // f3->SetParLimits(0,0,30);
  //f3->SetParLimits(1,1.104,1.1045);
  //f3->SetParLimits(1,1.104183,1.104383); // Toshi , 26June2013
  //f3->SetParLimits(1,1.104233,1.104292); // Toshi , 26June2013
  //f3->SetParLimits(1,1.104163,1.104292); // Toshi , 12Sep2013
  //f3->SetParLimits(1,1.104163,1.104288); // Toshi , 24Sep2013
  //f3->SetParLimits(1,1.104163,1.104283); // Toshi , 24Sep2013
  //f3->SetParLimits(1,1.104163,1.104293); // Toshi , 24Sep2013
  //f3->SetParLimits(1,1.104163,1.104288); // Toshi , 5Oct2013
  //f3->SetParLimits(1,1.104163,1.104283); // Toshi , 5Oct2013
  //f3->SetParLimits(1,1.104263,1.104283); // Toshi , 18Nov2013
  f3->SetParLimits(1,1.104263,1.104293); // Toshi , 24Dec2013(11.39-11.42)
  f3->SetParLimits(2,2.2e-4,4.0e-4);
  //f3->SetParLimits(3,4,10);
  
  // ----- 12LB g.s. (high) --------------------
  f4->SetParameters(0.0,1.104283,5.0e-4,5);
  //f4->SetParameters(50,1.104233,2e-3,30);
  //f4->SetParLimits(0,0,100);
  //f4->SetParLimits(1,1.103,1.105);
  //f4->SetParLimits(1,1.104233,1.104292); // Toshi , 26June2013
  //f4->SetParLimits(1,1.104163,1.104288); // Toshi , 12Sep2013
  //f4->SetParLimits(1,1.104263,1.104283); // Toshi , 18Nov2013
  f4->SetParLimits(1,1.104263,1.104293); // Toshi , 24Dec2013(11.39-11.42)
  f4->SetParLimits(2,2.2e-4,6.0e-4);
  //f4->SetParLimits(3,20,40);
  
  // ----- Lambda (H2O) --------------------
  //f5->SetParameters(50,1.115283,2e-3,10);
  f5->SetParameters(50,1.115683,7.5e-4,10);
  //f5->SetParLimits(0,0,1000);
  f5->SetParLimits(1,1.115383,1.115983);
  //f5->SetParLimits(1,1.115273,1.115293);
  f5->SetParLimits(2,5.0e-4,10.0e-4);
  //f5->SetParLimits(3,0,100);
  
  // ----- 12LB p-shell (low) --------------------
  //f6->SetParameters(10,1.115,2e-3,10);
  f6->SetParameters(10,1.115283,5.0e-4,10);
  //f6->SetParLimits(0,0,30);
  //f6->SetParLimits(1,1.114,1.116);
  f6->SetParLimits(1,1.115273,1.115293);
  f6->SetParLimits(2,2.2e-4,6.0e-4);
  //f6->SetParLimits(3,5,15);
  
  // ----- 12LB p-shell (high) --------------------
  //f7->SetParameters(10,1.115,2e-3,30);
  f7->SetParameters(10,1.115283,5.0e-4,10);
  //f7->SetParLimits(0,0,50);
  f7->SetParLimits(1,1.115273,1.115293);
  //f7->SetParLimits(1,1.114,1.116);
  f7->SetParLimits(2,2.2e-4,6.0e-4);
  //f7->SetParLimits(3,20,40);

  // ----- Sigma0 (H2O) --------------------
  f8->SetParameters(50,1.192642,9.0e-4,20);
  //f8->SetParLimits(0,2,100);
  f8->SetParLimits(1,1.192342,1.192942);
  f8->SetParLimits(2,5.0e-4,10.0e-4);
  //f8->SetParLimits(3,10,30);
  
  // =========== 
  f9->SetParameters(10,1.104,1e-3,5);
  // f3->SetParLimits(0,0,30);
  //f9->SetParLimits(1,1.104,1.1045);
  f9->SetParLimits(1,1.104,1.1048);
  f9->SetParLimits(2,5e-4,2e-3);
  //f3->SetParLimits(3,4,10);
  
  // =========== 10LB g.s. ================
  f10->SetParameters(10,1.107183,3.2e-4,5); // ---- 10LBe g.s.
  //f10->SetParameters(10,1.106983,3.3e-4,5); // ---- 10LBe g.s.                     
  ////f10->SetParLimits(1,1.1067,1.107203);  // Toshi , 21Sep2013                    
  //f10->SetParLimits(1,1.106883,1.107083);  // for test  , 18Nov2013 (-8.8 ~ -8.6 MeV)
  f10->SetParLimits(1,1.107083,1.107283);  // for test  , 25Jan2014 (-8.6 ~ -8.4 MeV)
  f10->SetParLimits(2,2.5e-4,3.8e-4);
  /// --- for core excited states -----
  //f10->SetParameters(10,1.11812,5.5e-4,5); // ---- 10LBe for excited state
  //f10->SetParameters(10,1.17883,5.5e-4,5); // ---- 10LBe for excited state (+2.2 MeV)
  //f10->SetParLimits(1,1.117783,1.117983);  // Toshi , 22Jan2014 (2.1 ~ 2.3 MeV)
  //f10->SetParLimits(1,1.11797,1.11827);  // Toshi , 27Oct2013                    
  //f10->SetParameters(10,1.11802,5.5e-4,5); // ---- 10LBe for excited state (-100keV)
  //f10->SetParLimits(1,1.11787,1.11817);  // Toshi , 28Oct2013                    
  //f10->SetParameters(10,1.117993,5.5e-4,5); // ---- 10LBe for excited state (new)
  //f10->SetParLimits(1,1.11787,1.118);  // Toshi , 28Oct2013(new)
  //f10->SetParLimits(1,1.11787,1.11817);  // Toshi , 28Oct2013  (new)                  
  //f10->SetParLimits(2,5.0e-4,6.5e-4);
  
  // =========== 7LHe g.s. ================
  //f11->SetParameters(10,1.10998,1e-3,5); // ---- 7LHe g.s.
  //f11->SetParameters(10,1.110333,7.5e-4,5);    // Toshi , 24Sep2013_2
  f11->SetParameters(10,1.110233,6.3e-4,5);   // Toshi , 16Oct2013 (-5.5 MeV)
  //f11->SetParameters(10,1.110333,6.3e-4,5);   // Toshi , 31Oct2013 (-5.35 MeV)
  //f11->SetParLimits(1,1.109783,1.109983); // Toshi , 24Sep2013_2
  //f11->SetParLimits(1,1.110183,1.110334); // Toshi , 10Oct2013
  //f11->SetParLimits(1,1.110183,1.110283); // Toshi , 10Oct2013 (-5.5 to -5.4 MeV)
  //f11->SetParLimits(1,1.110183,1.110433); // Toshi , 31Oct2013 (-5.5 to -5.25 MeV)
  //f11->SetParLimits(1,1.110183,1.110383); // Toshi , 4Nov2013 (-5.5 to -5.3 MeV)
  //f11->SetParLimits(1,1.110083,1.110253); // Toshi , 21Nov2013 (-5.6 to -5.43 MeV)
  f11->SetParLimits(1,1.110083,1.110253); // Toshi , 21Nov2013 (-5.6 to -5.43 MeV)
  //f11->SetParLimits(2,2.5e-4,6.7e-4);     // Toshi , 10Oct2013
  f11->SetParLimits(2,4.5e-4,5.5e-4);     // Toshi , 10Oct2013
  
  // =========== 12LB core excited 1 (low) ================
  f12->SetParameters(10,1.107133,6.3e-4,5);   // Toshi , 6Nov2013 (-8.55 MeV)
  f12->SetParLimits(1,1.107033,1.107233); // Toshi , 6Nov2013 (-8.45 to -8.65 MeV)
  f12->SetParLimits(2,2.3e-4,6.7e-4);
  
  // =========== 12LB core excited 1 (high) ================
  f13->SetParameters(10,1.107133,6.3e-4,5);   // Toshi , 6Nov2013 (-8.55 MeV)
  f13->SetParLimits(1,1.107033,1.107233); // Toshi , 6Nov2013 (-8.45 to -8.65 MeV)
  f13->SetParLimits(2,2.3e-4,6.7e-4);
  
  // =========== 52LV ================
  //f14->SetParameters(10,1.099683,6.3e-4,5); // Toshi , 10Jan2014 (-16.0 MeV)
  f14->SetParameters(10,1.100683,6.3e-4,5); // Toshi , 10Jan2014 (-15.0 MeV)
  //f14->SetParameters(10,1.116183,6.3e-4,5); // Toshi , 28Jan2014 (+0.5 MeV)
  //f14->SetParameters(10,1.118683,6.3e-4,5); // Toshi , 28Jan2014 (+3.0 MeV)
  //f14->SetParLimits(1,1.099183,1.100183); // Toshi , 10Jan2014 (-16.5 to -15.5 MeV)
  //f14->SetParLimits(1,1.099383,1.099983); // Toshi , 10Jan2014 (-16.3 to -15.7 MeV)
  //f14->SetParLimits(1,1.115883,1.116483); // Toshi , 28Jan2014 (0.2 to 0.8 MeV)
  //f14->SetParLimits(1,1.118383,1.118983); // Toshi , 3Feb2014 (2.7 to 3.3 MeV)
  f14->SetParLimits(1,1.100483,1.100883);  // Toshi , 10Feb2014 (-15.2 to -14.8 MeV)
  f14->SetParLimits(2,2.3e-4,6.7e-4);
  
  
  
  // ======= Define fitting loggers =========// Toshi,2/Feb2013
  FitLog *f3_log = new FitLog(str3,f3,0);   // 12LB g.s. (low)
  //f3_log->check(); // check
  FitLog *f4_log = new FitLog(str4,f4,0);   // 12LB g.s (high)
  //f4_log->check(); // check
  FitLog *f6_log = new FitLog(str6,f6,0);   // 12LB p-shell (low)
  //f6_log->check(); // check
  FitLog *f7_log = new FitLog(str7,f7,0);   // 12LB p-shell (high)
  //f7_log->check(); // check
  FitLog *f9_log = new FitLog(str9,f9,0);   // 12LB g.s. (CH2)
  //f7_log->check(); // check
  FitLog *f10_log = new FitLog(str10,f10,0);// 10LBe g.s.
  //f10_log->check(); // check
  FitLog *f11_log = new FitLog(str11,f11,0);//  7LHe g.s.
  //f11_log->check(); // check
  FitLog *f12_log = new FitLog(str12,f12,0);// 12LB core excited 1 (low)
  //f12_log->check(); // check
  FitLog *f13_log = new FitLog(str13,f13,0);// 12LB core excited 1 (high)
  //f13_log->check(); // check
  FitLog *f14_log = new FitLog(str14,f14,0);// 52LV
  //f14_log->check(); // check
  // ========================================//
  
  TH1F *h1 = new TH1F("h1","",100, cut1[0]-5e-3,cut1[1]+5e-3);
  TH1F *h2 = new TH1F("h2","",100, cut2[0]-5e-3,cut2[1]+5e-3);
  TH1F *h3 = new TH1F("h3","",100, cut3[0]-5e-3,cut3[1]+5e-3);
  TH1F *h4 = new TH1F("h4","",100, cut4[0]-5e-3,cut4[1]+5e-3);
  TH1F *h5 = new TH1F("h5","",100, cut5[0]-5e-3,cut5[1]+5e-3);
  TH1F *h6 = new TH1F("h6","",100, cut6[0]-5e-3,cut6[1]+5e-3);
  TH1F *h7 = new TH1F("h7","",100, cut7[0]-5e-3,cut7[1]+5e-3);
  TH1F *h8 = new TH1F("h8","",100, cut8[0]-5e-3,cut8[1]+5e-3);
  TH1F *h9 = new TH1F("h9","",100, cut9[0]-5e-3,cut9[1]+5e-3);
  TH1F *h10_= new TH1F("h10_","",100, cut10[0]-5e-3,cut10[1]+5e-3);
  //TH1F *h11_= new TH1F("h11_","",100, cut11[0]-5e-3,cut11[1]+5e-3);
  TH1F *h11_= new TH1F("h11_","",80, cut11[0]-5e-3,cut11[1]+5e-3);
  TH1F *h12_= new TH1F("h12_","",100,cut12[0]-5e-3,cut12[1]+5e-3);
  TH1F *h13_= new TH1F("h13_","",100,cut13[0]-5e-3,cut13[1]+5e-3);
  TH1F *h14_= new TH1F("h14_","",17,cut14[0]-5e-3,cut14[1]+5e-3);
  
  TFile *fCH2 = new TFile(ch2fname);
  TTree *trCH2 = (TTree*)fCH2->Get("h9500") ;
  TFile *fC12 = new TFile(c12fname); // Low current C12
  TTree *trC12 = (TTree*)fC12->Get("h9500") ;
  TFile *fC12_1 = new TFile(c12_1fname); // High current C12
  TTree *trC12_1 = (TTree*)fC12_1->Get("h9500") ;
  TFile *fH2O = new TFile(h2ofname);
  TTree *trH2O = (TTree*)fH2O->Get("h9500") ;
  TFile *fCH2_12LB  = new TFile(c12pfname);
  TTree *trCH2_12LB = (TTree*)fCH2_12LB->Get("h9500") ;
  TFile *fB10  = new TFile(b10fname);
  TTree *trB10 = (TTree*)fB10->Get("h9500") ;
  TFile *fLi7  = new TFile(li7fname);
  TTree *trLi7 = (TTree*)fLi7->Get("h9500") ;
  TFile *fCr52  = new TFile(cr52fname);
  TTree *trCr52 = (TTree*)fCr52->Get("h9500") ;
  
  
  double XFP=0.,XpFP=0.,YFP=0.,YpFP=0.,Mom=0.;
  double XSPLP=0.,YSPLP=0.;
  double Xpt=0.,Ypt=0.,LFP=0.,LEDC=0.,Xt=0.,Yt=0.;
  double XSS=0.,YSS=0.;
  double XpSS=0.,YpSS=0.;
  double XQ1i=0.,YQ1i=0.;
  double XQ2i=0.,YQ2i=0.;
  double XDi=0.,YDi=0.;
  double XDe=0.,YDe=0.;
  double XFP1=0.,YFP1=0.;
  double XpFP1=0.,YpFP1=0.;
  char SPLTrig=0,KTrig=0;
  TFile *fG4=new TFile("uni_15.root");
  TTree *tG4=(TTree*)fG4->Get("tree");
  tG4->SetBranchAddress("EXSPLP",&XSPLP);
  tG4->SetBranchAddress("EYSPLP",&YSPLP);
  tG4->SetBranchAddress("EXSS",&XSS);
  tG4->SetBranchAddress("EXpSS",&XpSS);
  tG4->SetBranchAddress("EYSS",&YSS);
  tG4->SetBranchAddress("EYpSS",&YpSS);
  tG4->SetBranchAddress("ELFP3",&LFP);
  tG4->SetBranchAddress("EXFP3",&XFP);
  tG4->SetBranchAddress("EXpFP3",&XpFP);
  tG4->SetBranchAddress("EYFP3",&YFP);
  tG4->SetBranchAddress("EYpFP3",&YpFP);
  tG4->SetBranchAddress("EXpt",&Xpt);
  tG4->SetBranchAddress("EYpt",&Ypt);
  tG4->SetBranchAddress("EMom",&Mom);
  tG4->SetBranchAddress("ESPLTrig",&SPLTrig);
  int nG4 = tG4->GetEntries();
  
  TFile *ofile = new TFile("output.root","recreate");
  logfile.open("fit.log");
  
  /* === Original Definition of mtclass ===
     mtclass *mt1= new mtclass(str1,trCH2,cut1,f1,h1);
     mtclass *mt2= new mtclass(str2,trCH2,cut2,f2,h2);
     mtclass *mt3= new mtclass(str3,trC12,cut3,f3,h3);
     mtclass *mt4= new mtclass(str4,trC12_1,cut4,f4,h4);
     mtclass *mt5= new mtclass(str5,trH2O,cut5,f5,h5);
     mtclass *mt6= new mtclass(str6,trC12,cut6,f6,h6);
     mtclass *mt7= new mtclass(str7,trC12_1,cut7,f7,h7);
  */
  
  // === Toshi's version ===
  mtclass *mt1= new mtclass(str1,trCH2  ,cut1,f1,h1,100); // Lambda from CH2
  mtclass *mt2= new mtclass(str2,trCH2  ,cut2,f2,h2,101); // Sigma  from CH2
  mtclass *mt3= new mtclass(str3,trC12  ,cut3,f3,h3,1  ); // Low  12LB g.s.
  mtclass *mt4= new mtclass(str4,trC12_1,cut4,f4,h4,1  ); // High 12LB g.s.
  mtclass *mt5= new mtclass(str5,trH2O  ,cut5,f5,h5,110); // Lambda from H2O
  mtclass *mt6= new mtclass(str6,trC12  ,cut6,f6,h6,1  ); // Low  12LB p-shell
  mtclass *mt7= new mtclass(str7,trC12_1,cut7,f7,h7,1  ); // High 12LB p-shell
  mtclass *mt8= new mtclass(str8,trH2O  ,cut8,f8,h8,111); // Sigma from H2O
  mtclass *mt9= new mtclass(str9,trCH2_12LB ,cut9,f9,h9,1  ); // 12LB g.s. from CH2
  mtclass *mt10= new mtclass(str10,trB10 ,cut10,f10,h10_,1000  ); // 10LBe g.s.
  mtclass *mt11= new mtclass(str11,trLi7 ,cut11,f11,h11_, 700  ); //  7LHe g.s.
  mtclass *mt12= new mtclass(str12,trC12  ,cut12,f12,h12_,1  ); // 12LB core excited 1 (low)
  mtclass *mt13= new mtclass(str13,trC12_1,cut13,f13,h13_,1  ); // 12LB core excited 1 (high)
  mtclass *mt14= new mtclass(str14,trCr52,cut14,f14,h14_,1   ); // 52LV
  // ======= Initialize mtclass ==============
  mt1->init();
  mt2->init();
  mt3->init();
  mt4->init();
  mt5->init();
  mt6->init();
  mt7->init();
  mt8->init();
  mt9->init();
  mt10->init();
  mt11->init();
  mt12->init();
  mt13->init();
  mt14->init();
  // ========================================
  
  h1->Write("hl");
  h2->Write("hs");
  h3->Write("hgsl");
  h4->Write("hgsh");
  h5->Write("hlh2o");
  h8->Write("hsh2o");
  h6->Write("hcpsl");
  h7->Write("hcpsh");
  h9->Write("hgsch2");
  h10_->Write("hgsb10");
  h11_->Write("hgsli7");
  h12_->Write("hcel");
  h13_->Write("hceh");
  h14_->Write("hcr52_1");
  
  TH1F* h54 = (TH1F*)mt1->GetMMHistOrg();
  TH1F* h55 = (TH1F*)mt1->GetMMHistBg();
  TH1F* h56 = (TH1F*)mt1->GetMMHistSubt();
  TH1F* h57 = (TH1F*)mt3->GetMMHistOrg();
  TH1F* h58 = (TH1F*)mt3->GetMMHistBg();
  TH1F* h59 = (TH1F*)mt3->GetMMHistSubt();
  TH1F* h60 = (TH1F*)mt10->GetMMHistOrg();
  TH1F* h61 = (TH1F*)mt10->GetMMHistBg();
  TH1F* h62 = (TH1F*)mt10->GetMMHistSubt();
  TH1F* h63 = (TH1F*)mt11->GetMMHistOrg(); // 7LHe
  TH1F* h64 = (TH1F*)mt11->GetMMHistBg();  // 7LHe
  TH1F* h65 = (TH1F*)mt11->GetMMHistSubt();// 7LHe
  TH1F* h66 = (TH1F*)mt11->GetMMHistOrg(); // Lambda of H2O
  TH1F* h67 = (TH1F*)mt11->GetMMHistBg();  // Lambda of H2O
  TH1F* h68 = (TH1F*)mt11->GetMMHistSubt();// Lambda of H2O
  
  TH2F* h_mmxt = (TH2F*)mt1->GetMMXtHist();// MM vs. gbeamx
  TH2F* h_mmyt = (TH2F*)mt1->GetMMYtHist();// MM vs. gbeamy
  TProfile* pro_mmxt = (TProfile*)mt1->GetMMXtProfile();// Profile of (MM vs. gbeamx)
  TProfile* pro_mmyt = (TProfile*)mt1->GetMMYtProfile();// Profile of (MM vs. gbeamy)
  TF1* func_mmxt = (TF1*)mt1->GetMMXtFunc();
  TF1* func_mmyt = (TF1*)mt1->GetMMYtFunc();
  TF1* func_mmxt_= (TF1*)mt1->GetMMXtFunc2();
  TF1* func_mmyt_= (TF1*)mt1->GetMMYtFunc2();
  // ========= Fitting Log (2) ============= //
  FitLog *fmmxt_log = new FitLog("fmmxt",func_mmxt,1);
  //fmmxt_log->check(); // check
  FitLog *fmmyt_log = new FitLog("fmmyt",func_mmyt,1);
  //fmmyt_log->check(); // check
  // ==========================================//
  h54->Write();
  h55->Write();
  h56->Write();
  h57->Write();
  h58->Write();
  h59->Write();
  h60->Write();
  h61->Write();
  h62->Write();
  h63->Write();
  h64->Write();
  h65->Write();
  h_mmxt->Write();   // MM vs. gbeamx 
  h_mmyt->Write();   // MM vs. gbeamy
  pro_mmxt->Write(); // Profile of (MM vs. gbeamx)
  pro_mmyt->Write(); // Profile of (MM vs. gbeamy)
  func_mmxt->Write();// Fitted Function 
  func_mmyt->Write();// Fitted Function
  func_mmxt_->Write(); // mm = [1] * xt + 0.0
  func_mmyt_->Write(); // mm = [1] * yt + 0.0
  TF1* fitfunc1 = (TF1*)mt1->GetFitFunc();  //Lambda
  TF1* fitfunc2 = (TF1*)mt2->GetFitFunc();  //Sigma
  TF1* fitfunc3 = (TF1*)mt3->GetFitFunc();  //12LB g.s. (low current)
  TF1* fitfunc4 = (TF1*)mt6->GetFitFunc();  //12LB p-shell (low current)
  TF1* fitfunc5 = (TF1*)mt9->GetFitFunc();  //12LB s-shell from CH2
  TF1* fitfunc6 = (TF1*)mt10->GetFitFunc(); //10LBe g.s.
  TF1* fitfunc7 = (TF1*)mt11->GetFitFunc(); // 7LHe g.s.
  TF1* fitfunc8 = (TF1*)mt14->GetFitFunc(); //52LV_1
  fitfunc1->Write();
  fitfunc2->Write();
  fitfunc3->Write();
  fitfunc4->Write();
  fitfunc5->Write();
  fitfunc6->Write();
  fitfunc7->Write();
  fitfunc8->Write();
  gsmm      = mt3->Getmu();
  //gsmm  = -11.40 + ml;// Toshi
  gsmm_high = mt4->Getmu();
  gsmm_high = gsmm;//  For test (Toshi , 6Feb2013)
  psmm      = mt6->Getmu();
  psmm_high = mt7->Getmu();
  psmm_high = psmm;//  For test (Toshi , 6Feb2013)
  gsmm_ch2  = mt9->Getmu();
  gsmm_b10  = mt10->Getmu();
  gsmm_li7  = mt11->Getmu(); // switch ON !!!, 21Aug2013, Toshi
  //gsmm_li7  = -5.5e-3 + ml; // for test , Toshi , 7Nov2013 
  cemm      = mt12->Getmu(); // Toshi , 6Nov2013
  cemm_high = mt13->Getmu(); // Toshi , 6Nov2013
  cemm = cemm_high;// for test , Toshi , 6Nov2013
  mm1_cr52 = mt14->Getmu(); // Toshi , 10Jan2014
  // ------------ Just for test -----------
  //cemm = 1.106983;
  //cemm_high = 1.106983; 
  
  // ===== Fitting Logger ===== // Toshi , 2Feb2013
  f3_log->LogWrite(); // 12LB s-shell (Low  current)
  f4_log->LogWrite(); // 12LB s-shell (High current)
  f6_log->LogWrite(); // 12LB p-shell (Low  current)
  f7_log->LogWrite(); // 12LB p-shell (High current)
  f9_log->LogWrite(); // 12LB p-shell (High current)
  f10_log->LogWrite(); // 10LBe s-shell
  f11_log->LogWrite(); //  7LHe s-shell
  f12_log->LogWrite(); // 12LB core excited 1 (low)
  f13_log->LogWrite(); // 12LB core excited 1 (high)
  f14_log->LogWrite(); // 52LV
  fmmxt_log->LogWrite(); // 
  fmmyt_log->LogWrite(); // 
  // ===========================//
  
  nLambda    =mt1->Getnpeak();
  nSigma     =mt2->Getnpeak();
  nLB12gsl   =mt3->Getnpeak();
  nLB12gsh   =mt4->Getnpeak();
  nLambda_h2o=mt5->Getnpeak();
  nSigma_h2o =mt8->Getnpeak();
  nLB12psl   =mt6->Getnpeak();
  nLB12psh   =mt7->Getnpeak();
  nLB12gsch2 =mt9->Getnpeak();
  nLBe10gs   =mt10->Getnpeak();
  nLHe7gs    =mt11->Getnpeak();
  nLB12cel   =mt12->Getnpeak();
  nLB12ceh   =mt13->Getnpeak();
  nLV52_1    =mt14->Getnpeak();
  
  cout << " Lambda (CH2) : " << nLambda << endl;
  cout << " Sigma  (CH2) : " << nSigma  << endl;
  cout << " Lambda (H2O) : " << nLambda_h2o << endl;
  cout << " Sigma  (H2O) : " << nSigma_h2o  << endl;
  cout << " L12B s (low) : " << nLB12gsl  << endl;
  cout << " L12B p (low) : " << nLB12psl  << endl;
  cout << " L12B s (high): " << nLB12gsh  << endl;
  cout << " L12B p (high): " << nLB12psh  << endl;
  cout << " L12B s (CH2) : " << nLB12gsch2<< endl;
  cout << " L10Be s      : " << nLBe10gs  << endl;
  cout << " L7He s       : " << nLHe7gs   << endl;
  cout << " L12B ce (low): " << nLB12cel  << endl;
  cout << " L12B ce(high): " << nLB12ceh  << endl;
  cout << " L52V (1)     : " << nLV52_1   << endl;
  
  const int nAllpeak
    = nLambda + nSigma + nLB12gsl + nLB12gsh + 
    nLambda_h2o + nLB12psl + nLB12psh + nSigma_h2o +
    nLB12gsch2 + nLBe10gs + nLHe7gs + nLB12cel + 
    nLB12ceh + nLV52_1;
  //const int nAllpeak=nLambda+nSigma;
  logfile << "nAllpeak= " << nAllpeak << endl; 
  cout << " --> Sum of events : " << nAllpeak << endl;
  cout << endl;
  
  ifstream data_rasmomE("HESmatrices/momR_11Jul11.dat");
  ifstream data_rasxptE("HESmatrices/xptR_11Jul11.dat");
  ifstream data_rasyptE("HESmatrices/yptR_11Jul11.dat");
  ifstream data_rasmomK("HKSmatrices/momR_11Jul11.dat");
  ifstream data_rasxptK("HKSmatrices/xptR_11Jul11.dat");
  ifstream data_rasyptK("HKSmatrices/yptR_11Jul11.dat");
  for (int i=0;i<10;i++){
    int aaa;
    data_rasmomE >> rasmomE[i]>>aaa >>aaa ;
    data_rasxptE >> rasxptE[i]>>aaa >>aaa ;
    data_rasyptE >> rasyptE[i]>>aaa >>aaa ;
    data_rasmomK >> rasmomK[i]>>aaa >>aaa ;
    data_rasxptK >> rasxptK[i]>>aaa >>aaa ;
    data_rasyptK >> rasyptK[i]>>aaa >>aaa ;
  }
  data_rasmomE.close();
  data_rasxptE.close();
  data_rasyptE.close();
  data_rasmomK.close();
  data_rasxptK.close();
  data_rasyptK.close();
  
  //cout << "Fill Lambda event..."<<endl;
  float exfE[nTrig];
  float expfE[nTrig];
  float eyfE[nTrig];
  float eypfE[nTrig];
  float exfK[nTrig];
  float expfK[nTrig];
  float eyfK[nTrig];
  float eypfK[nTrig];
  float deltal    = 0;
  float deltas    = 0;
  float deltagsl  = 0;
  float deltagsh  = 0;
  float deltah2o  = 0;
  float deltah2os = 0;
  float deltapsl  = 0;
  float deltapsh  = 0;
  float deltagsch2= 0;
  float deltagsb10= 0;
  float deltagsli7= 0;
  float deltacel  = 0;
  float deltaceh  = 0;
  float deltacr52_1 = 0;
  float chi2l     = 0;
  float chi2s     = 0;
  float chi2gsl   = 0;
  float chi2gsh   = 0;
  float chi2h2o   = 0;
  float chi2h2os  = 0;
  float chi2psl   = 0;
  float chi2psh   = 0;
  float chi2gsch2 = 0;
  float chi2gsb10 = 0;
  float chi2gsli7 = 0;
  float chi2cel   = 0;
  float chi2ceh   = 0;
  float chi2cr52_1= 0;
  int nl    = 0;
  int ns    = 0;
  int ngsl  = 0;
  int ngsh  = 0;
  int nh2o  = 0;
  int nh2os = 0;
  int npsl  = 0;
  int npsh  = 0;
  int ngsch2= 0;
  int ngsb10= 0;
  int ngsli7= 0;
  int ncel  = 0;
  int nceh  = 0;
  int ncr52_1 = 0;
  
  cout << "Fill Lambda event..."<<endl;
  for( int i=0 ; i<nLambda ; i++ ){
    float mm=mt1->GetMM0(i);
    float xt=mt1->GetXt(i);
    float yt=mt1->GetYt(i);
    //gdmm[i]=-15e-3*xt;//original
    // ==== by Toshi , 7Aug2012 ====
    //gdmm[i] =0.00770104 * (xt-(-0.000545202)) + -3.79849e-05;
    //gdmm[i]+=0.000222721 * (yt-(0.000735494)) + -2.96895e-05;
    // ==== by Toshi , 24Apr2013
    //gdmm[i] =0+(0.00653387*xt);
    //gdmm[i]+=0+(-0.00120051*yt);
    //gdmm[i] = -1.0*gdmm[i];//reverse the sign
    // ==== by Toshi , 19/May/2013
    //gdmm[i] = 0.0 + ( func_mmxt_->GetParameter(1) * xt );
    //gdmm[i]+= 0.0 + ( func_mmyt_->GetParameter(1) * yt );
    
    // -==== Toshi 6July2013 for test=========
    gdmm[i] = 0.00441113*xt - (0.0106527*xt*xt) 
      - (0.57073*xt*xt*xt) + (4.20109*xt*xt*xt*xt);
    gdmm[i]+= 0.000973207*yt - (0.0189651*yt*yt) 
      - (0.0653128*yt*yt*yt)+ (1.30222*yt*yt*yt*yt);
    gdmm[i] = -1.0*gdmm[i];//reverse the sign
    
    float mu=mt1->Getmu();
    float sigma=mt1->Getsigma();
    if (mm>mu-1.5*sigma&&mm<mu+1.5*sigma){ // for test , 25Oct2013 Toshi
      //if (mm>mu-1.2*sigma&&mm<mu+1.2*sigma){ // for test , 25Oct2013 Toshi
      //if (mm>mu-2.0*sigma&&mm<mu+2.0*sigma){
      gEvflag[i]=100;
      //deltal=(mm+gdmm[i]-(ml-.5e-4))/resMM1;
      //deltal=(mm+gdmm[i]-(ml-0.0e-4))/resMM1;
      //deltal = ( mm + gdmm[i] - ml ) / resMM1; // comment out
      deltal = ( mm  - ml ) / resMM1; // Toshi , 5Sep2013
      chi2l+=deltal*deltal;
      nl++;
    }
    else{
      gEvflag[i]=0;
    }
    gdmomE[i]=corRaster(rasmomE,xt,yt);
    gdxptE[i]=corRaster(rasxptE,xt,yt);
    gdyptE[i]=corRaster(rasyptE,xt,yt);
    gdmomK[i]=corRaster(rasmomK,xt,yt);
    gdxptK[i]=corRaster(rasxptK,xt,yt);
    gdyptK[i]=corRaster(rasyptK,xt,yt);
    gB[i]=mt1->GetBvar(i);
    gmomK0[i]=mt1->GetmomK0(i);
    gmomE0[i]=mt1->GetmomE0(i);
    gxfE[i]=mt1->GetxfE(i);
    gxpfE[i]=mt1->GetxpfE(i);
    gyfE[i]=mt1->GetyfE(i);
    gypfE[i]=mt1->GetypfE(i);
    gxfK[i]=mt1->GetxfK(i);
    gxpfK[i]=mt1->GetxpfK(i);
    gyfK[i]=mt1->GetyfK(i);
    gypfK[i]=mt1->GetypfK(i);
    exfE[i]=1e-4;
    expfE[i]=1e-4;
    eyfE[i]=1e-4;
    eypfE[i]=1e-4;
    exfK[i]=1e-4;
    expfK[i]=1e-4;
    eyfK[i]=1e-4;
    eypfK[i]=1e-4;
    for(int j=0;j<nParam;j++){
      gE[i][j]=mt1->GetEvar(i,j);
      gK[i][j]=mt1->GetKvar(i,j);
    }
  }
  cout << "Fill Sigma0 event..."<<endl;
  for(int i=0;i<nSigma;i++){
    int k=i+nLambda;
    float mm=mt2->GetMM0(i);
    float xt=mt2->GetXt(i);
    float yt=mt2->GetYt(i);
    //gdmm[k]=-15e-3*xt;//original by Kawama
    // ==== by Toshi , 7Aug2012 ====
    //gdmm[i] =0.00770104 * (xt-(-0.000545202)) + -3.79849e-05;
    //gdmm[i]+=0.000222721 * (yt-(0.000735494)) + -2.96895e-05;
    
    // ==== by Toshi , 24Apr2013
    //gdmm[i] =0+(0.00653387*xt);
    //gdmm[i]+=0+(-0.00120051*yt);
    //gdmm[i] = -1.0*gdmm[i];//reverse the sign
    
    // ==== by Toshi , 19/May/2013
    //gdmm[k] = 0.0 + ( func_mmxt_->GetParameter(1) * xt );
    //gdmm[k]+= 0.0 + ( func_mmyt_->GetParameter(1) * yt );
    //gdmm[k] = -1.0*gdmm[k];//reverse the sign
    // -==== Toshi 6July2013 for test=========
    gdmm[k] = 0.00441113*xt - (0.0106527*xt*xt) 
      - (0.57073*xt*xt*xt) + (4.20109*xt*xt*xt*xt);
    gdmm[k]+= 0.000973207*yt - (0.0189651*yt*yt) 
      - (0.0653128*yt*yt*yt)+ (1.30222*yt*yt*yt*yt);
    gdmm[k] = -1.0*gdmm[k];//reverse the sign
    
    float mu=mt2->Getmu();
    float sigma=mt2->Getsigma();
    if (mm>mu-1.5*sigma&&mm<mu+1.5*sigma){ // for test , 25Oct2013 , Toshi
    //if (mm>mu-1.2*sigma&&mm<mu+1.2*sigma){ // for test , 25Oct2013 , Toshi
      //if (mm>mu-1.5*sigma&&mm<mu+1.5*sigma){ // for test , 25Oct2013 , Toshi
      //if (mm>mu-2.0*sigma&&mm<mu+2.0*sigma){
      gEvflag[k]=101;
      //deltas=(mm+gdmm[k]-(ms0+.0e-4))/resMM1;
      //deltas=( mm + gdmm[k] - ms0 ) / resMM1; //original
      //--- raster correction is not needed because
      //--- it's already included.
      deltas=( mm - ms0 ) / resMM1; // Toshi , 5Sep2013
      chi2s+=deltas*deltas;
      ns++;
    }
    else{
      gEvflag[k]=0;
    }
    gdmomE[k]=corRaster(rasmomE,xt,yt);
    gdxptE[k]=corRaster(rasxptE,xt,yt);
    gdyptE[k]=corRaster(rasyptE,xt,yt);
    gdmomK[k]=corRaster(rasmomK,xt,yt);
    gdxptK[k]=corRaster(rasxptK,xt,yt);
    gdyptK[k]=corRaster(rasyptK,xt,yt);
    gB[k]=mt2->GetBvar(i);
    gmomK0[k]=mt2->GetmomK0(i);
    gmomE0[k]=mt2->GetmomE0(i);
    gxfE[k]=mt2->GetxfE(i);
    gxpfE[k]=mt2->GetxpfE(i);
    gyfE[k]=mt2->GetyfE(i);
    gypfE[k]=mt2->GetypfE(i);
    gxfK[k]=mt2->GetxfK(i);
    gxpfK[k]=mt2->GetxpfK(i);
    gyfK[k]=mt2->GetyfK(i);
    gypfK[k]=mt2->GetypfK(i);
    exfE[k]=1e-4;
    expfE[k]=1e-4;
    eyfE[k]=1e-4;
    eypfE[k]=1e-4;
    exfK[k]=1e-4;
    expfK[k]=1e-4;
    eyfK[k]=1e-4;
    eypfK[k]=1e-4;
    for(int j=0;j<nParam;j++){
      gE[k][j]=mt2->GetEvar(i,j);
      gK[k][j]=mt2->GetKvar(i,j);
    }
  }
  cout << "Fill LB12gs event..."<<endl;
  for(int i=0;i<nLB12gsl;i++){
    int k=i+nLambda+nSigma;
    float mm=mt3->GetMM0(i);
    gdmm[k]=0.;
    float mu=mt3->Getmu();
    float sigma=mt3->Getsigma();
    //if (mm>mu-2.0*sigma&&mm<mu+2.0*sigma){
    //if (mm>mu-2.0*sigma&&mm<mu+0.8*sigma){ // For test , Toshi , 31Jan2013
    //if (mm>mu-2.0*sigma&&mm<mu+1.7*sigma){ // For test , Toshi , 31Jan2013
    //if (mm>mu-2.5*sigma&&mm<mu+2.5*sigma){ // For test , Toshi , 22June2013
    if (mm>mu-2.0*sigma&&mm<mu+2.0*sigma){ //
      //if ( mm>mu-0.7e-3 && mm<mu+0.7e-3 ){ // For test , Toshi , 28June2013
      //if (mm>mu-1.0*sigma&&mm<mu+1.0*sigma){ //
      gEvflag[k]=1201;
      //deltagsl=(mm+gdmm[k]-ml+gsmass)/resMM2; //Original
      deltagsl=( mm + gdmm[k] - gsmm ) / resMM2; // Toshi , 28Jan2013
      chi2gsl+=deltagsl*deltagsl;
      ngsl++;
    }
    else{
      gEvflag[k]=0;
    }
    gB[k]=mt3->GetBvar(i);
    gmomK0[k]=mt3->GetmomK0(i);
    gmomE0[k]=mt3->GetmomE0(i);
    gxfE[k]=mt3->GetxfE(i);
    gxpfE[k]=mt3->GetxpfE(i);
    gyfE[k]=mt3->GetyfE(i);
    gypfE[k]=mt3->GetypfE(i);
    gxfK[k]=mt3->GetxfK(i);
    gxpfK[k]=mt3->GetxpfK(i);
    gyfK[k]=mt3->GetyfK(i);
    gypfK[k]=mt3->GetypfK(i);
    exfE[k]=1e-4;
    expfE[k]=1e-4;
    eyfE[k]=1e-4;
    eypfE[k]=1e-4;
    exfK[k]=1e-4;
    expfK[k]=1e-4;
    eyfK[k]=1e-4;
    eypfK[k]=1e-4;
    for(int j=0;j<nParam;j++){
      gE[k][j]=mt3->GetEvar(i,j);
      gK[k][j]=mt3->GetKvar(i,j);
    }
  }
  cout << "Fill LB12gsh event..."<<endl;
  for(int i=0;i<nLB12gsh;i++){
    int k=i+nLambda+nSigma+nLB12gsl;
    float mm=mt4->GetMM0(i);
    gdmm[k]=0.;
    float mu=mt4->Getmu();
    float sigma=mt4->Getsigma();
    if (mm>mu-2.0*sigma&&mm<mu+2.0*sigma){
      //if (mm>mu-1.0*sigma&&mm<mu+1.0*sigma){
      //if (mm>gsmm_high-2.5*sigma&&mm<gsmm_high+2.5*sigma){//22June2013
      //if (mm>gsmm_high-2.0*sigma&&mm<gsmm_high+2.0*sigma){//25June2013
      //if (mm>gsmm_high-1.0*sigma&&mm<gsmm_high+1.0*sigma){//25June2013
      //if ( mm>mu-0.7e-3 && mm<mu+0.7e-3 ){ // For test , Toshi , 28June2013
      gEvflag[k]=1202;
      //deltagsh=( mm + gdmm[k] - ml + gsmass )/resMM2;
      deltagsh=( mm + gdmm[k] - gsmm_high )/resMM2;
      chi2gsh+=deltagsh*deltagsh;
      ngsh++;
    }
    else{
      gEvflag[k]=0;
    }
    /*float xt=(mt4->GetXt(i)-0.63)/10;
      float yt=(mt4->GetYt(i)-0.55)/2.5;
      gdmomE[k]=corRaster(rasmomE,xt,yt);
      gdxptE[k]=corRaster(rasxptE,xt,yt);
      gdyptE[k]=corRaster(rasyptE,xt,yt);
      gdmomK[k]=corRaster(rasmomK,xt,yt);
      gdxptK[k]=corRaster(rasxptK,xt,yt);
      gdyptK[k]=corRaster(rasyptK,xt,yt);*/
    gB[k]=mt4->GetBvar(i);
    gmomK0[k]=mt4->GetmomK0(i);
    gmomE0[k]=mt4->GetmomE0(i);
    gxfE[k]=mt4->GetxfE(i);
    gxpfE[k]=mt4->GetxpfE(i);
    gyfE[k]=mt4->GetyfE(i);
    gypfE[k]=mt4->GetypfE(i);
    gxfK[k]=mt4->GetxfK(i);
    gxpfK[k]=mt4->GetxpfK(i);
    gyfK[k]=mt4->GetyfK(i);
    gypfK[k]=mt4->GetypfK(i);
    exfE[k]=mt4->GetxfE(i);
    exfE[k]=1e-4;
    expfE[k]=1e-4;
    eyfE[k]=1e-4;
    eypfE[k]=1e-4;
    exfK[k]=1e-4;
    expfK[k]=1e-4;
    eyfK[k]=1e-4;
    eypfK[k]=1e-4;
    //cout <<k<<" "<<gxfE[k]<<endl;
    for(int j=0;j<nParam;j++){
      gE[k][j]=mt4->GetEvar(i,j);
      gK[k][j]=mt4->GetKvar(i,j);
    }
  }
  cout << "Fill Lambda_h2o event..."<<endl;
  for(int i=0;i<nLambda_h2o;i++){
    int k=i+nLambda+nSigma+nLB12gsl+nLB12gsh;
    float mm=mt5->GetMM0(i);
    gdmm[k]=0.;
    float mu=mt5->Getmu();
    float sigma=mt5->Getsigma();
    //if (mm>mu-2.0*sigma&&mm<mu+2.0*sigma){
    if (mm>mu-1.7*sigma&&mm<mu+1.7*sigma){//for test , 2Nov2013
      //if (mm>mu-1.5*sigma&&mm<mu+1.5*sigma){//for test , 9July2013
      gEvflag[k]=110;
      //gEvflag[k]=0;
      //deltah2o=( mm + gdmm[k] - ml ) / resMM1;
      deltah2o=( mm + - ml ) / resMM1; // Toshi , 22Oct2013
      chi2h2o+=deltah2o*deltah2o;
      nh2o++;
    }
    else{
      gEvflag[k]=0;
    }
    gB[k]      =mt5->GetBvar(i);
    gmomK0[k]  =mt5->GetmomK0(i);
    gmomE0[k]  =mt5->GetmomE0(i);
    gxfE[k]    =mt5->GetxfE(i);
    gxpfE[k]   =mt5->GetxpfE(i);
    gyfE[k]    =mt5->GetyfE(i);
    gypfE[k]   =mt5->GetypfE(i);
    gxfK[k]    =mt5->GetxfK(i);
    gxpfK[k]   =mt5->GetxpfK(i);
    gyfK[k]    =mt5->GetyfK(i);
    gypfK[k]   =mt5->GetypfK(i);
    exfE[k]    =mt5->GetxfE(i);
    exfE[k]    =1e-4;
    expfE[k]   =1e-4;
    eyfE[k]    =1e-4;
    eypfE[k]   =1e-4;
    exfK[k]    =1e-4;
    expfK[k]   =1e-4;
    eyfK[k]    =1e-4;
    eypfK[k]   =1e-4;
    for(int j=0;j<nParam;j++){
      gE[k][j]=mt5->GetEvar(i,j);
      gK[k][j]=mt5->GetKvar(i,j);
    }
  }
  cout << "Fill 12LB p-shell (low) event..."<<endl;
  for(int i=0;i<nLB12psl;i++){
    int k=i+nLambda+nSigma+nLB12gsl+nLB12gsh+nLambda_h2o;
    gEvflag[k]=1203;
    float xt=mt6->GetXt(i);
    float yt=mt6->GetYt(i);
    float mm=mt6->GetMM0(i);
    gdmm[k]=0.0;
    float mu=mt6->Getmu();
    float sigma=mt6->Getsigma();
    //if (mm>(1.115683-2.e-3)&&mm<1.115683+1.e-3){
    //if (mm>mu-2.0*sigma&&mm<mu+1.7*sigma){ // For test , Toshi , 31Jan2013
    //if (mm>mu-1.2*sigma&&mm<mu+1.2*sigma){ // For test , Toshi , 8June2013
    if (mm>mu-1.0*sigma&&mm<mu+1.0*sigma){ // For test , Toshi , 8June2013
      gEvflag[k]=1203;
      //deltapsl=(mm+gdmm[k]-ml+psmass)/resMM2;
      //deltapsl=( mm + gdmm[k] - psmm )/resMM2;
      deltapsl=( mm - psmm )/resMM2;
      chi2psl+=deltapsl*deltapsl;
      npsl++;
    }
    else{
      gEvflag[k]=0;
    }
    gB[k]=mt6->GetBvar(i);
    gmomK0[k]=mt6->GetmomK0(i);
    gmomE0[k]=mt6->GetmomE0(i);
    gxfE[k]=mt6->GetxfE(i);
    gxpfE[k]=mt6->GetxpfE(i);
    gyfE[k]=mt6->GetyfE(i);
    gypfE[k]=mt6->GetypfE(i);
    gxfK[k]=mt6->GetxfK(i);
    gxpfK[k]=mt6->GetxpfK(i);
    gyfK[k]=mt6->GetyfK(i);
    gypfK[k]=mt6->GetypfK(i);
    exfE[k]=1e-4;
    expfE[k]=1e-4;
    eyfE[k]=1e-4;
    eypfE[k]=1e-4;
    exfK[k]=1e-4;
    expfK[k]=1e-4;
    eyfK[k]=1e-4;
    eypfK[k]=1e-4;
    for(int j=0;j<nParam;j++){
      gE[k][j]=mt6->GetEvar(i,j);
      gK[k][j]=mt6->GetKvar(i,j);
    }
  }
  cout << "Fill 12LB p-shell (high) event..."<<endl;
  for(int i=0;i<nLB12psh;i++){
    int k=i+nLambda+nSigma+nLB12gsl+nLB12gsh+nLambda_h2o+nLB12psl;
    gEvflag[k]=1204;
    float xt=mt7->GetXt(i);
    float yt=mt7->GetYt(i);
    float mm=mt7->GetMM0(i);
    gdmm[k]=0.0*xt;
    float mu=mt7->Getmu();
    float sigma=mt7->Getsigma();
    //if (mm>(1.115683-2.e-3)&&mm<1.115683+1.e-3){
    //if (mm>mu-2.0*sigma&&mm<mu+1.7*sigma){
    //if (mm>mu-1.2*sigma&&mm<mu+1.2*sigma){
    if (mm>mu-1.0*sigma&&mm<mu+1.0*sigma){
      gEvflag[k]=1204;
      //deltapsh=(mm+gdmm[k]-ml+psmass)/resMM2;
      deltapsh = ( mm + gdmm[k] - psmm_high ) / resMM2;
      chi2psh+=deltapsh*deltapsh;
      npsh++;
    }
    else{
      gEvflag[k]=0;
    }
    gB[k]=mt7->GetBvar(i);
    gmomK0[k]=mt7->GetmomK0(i);
    gmomE0[k]=mt7->GetmomE0(i);
    gxfE[k]=mt7->GetxfE(i);
    gxpfE[k]=mt7->GetxpfE(i);
    gyfE[k]=mt7->GetyfE(i);
    gypfE[k]=mt7->GetypfE(i);
    gxfK[k]=mt7->GetxfK(i);
    gxpfK[k]=mt7->GetxpfK(i);
    gyfK[k]=mt7->GetyfK(i);
    gypfK[k]=mt7->GetypfK(i);
    exfE[k]=1e-4;
    expfE[k]=1e-4;
    eyfE[k]=1e-4;
    eypfE[k]=1e-4;
    exfK[k]=1e-4;
    expfK[k]=1e-4;
    eyfK[k]=1e-4;
    eypfK[k]=1e-4;
    for(int j=0;j<nParam;j++){
      gE[k][j]=mt7->GetEvar(i,j);
      gK[k][j]=mt7->GetKvar(i,j);
    }
  }
  cout << "Fill Sigma_h2o event..."<<endl;
  for( int i=0 ; i<nSigma_h2o ;i++ ){
    int k = i 
      + nLambda + nSigma + nLB12gsl
      + nLB12gsh + nLambda_h2o + nLB12psl
      + nLB12psh;
    gEvflag[k] =0;
    gdmm[k]    =0.0;
    float xt=mt8->GetXt(i);
    float yt=mt8->GetYt(i);
    float mm=mt8->GetMM0(i);
    float mu=mt8->Getmu();
    float sigma=mt8->Getsigma();
    //if (mm>(1.115683-2.e-3)&&mm<1.115683+1.e-3){
    //if (mm>mu-2.0*sigma&&mm<mu+2.0*sigma){
    if (mm>mu-1.5*sigma&&mm<mu+1.5*sigma){//for test , 9July2013
      gEvflag[k]=111;
      //deltah2os = ( mm + gdmm[k] - ms0 ) / resMM1;
      deltah2os = ( mm - ms0 ) / resMM1;
      chi2h2os += deltah2os*deltah2os;
      nh2os++;
    }
    else{
      gEvflag[k]=0;
    }
    gB[k]      =mt8->GetBvar(i);
    gmomK0[k]  =mt8->GetmomK0(i);
    gmomE0[k]  =mt8->GetmomE0(i);
    gxfE[k]    =mt8->GetxfE(i);
    gxpfE[k]   =mt8->GetxpfE(i);
    gyfE[k]    =mt8->GetyfE(i);
    gypfE[k]   =mt8->GetypfE(i);
    gxfK[k]    =mt8->GetxfK(i);
    gxpfK[k]   =mt8->GetxpfK(i);
    gyfK[k]    =mt8->GetyfK(i);
    gypfK[k]   =mt8->GetypfK(i);
    exfE[k]    =1e-4;
    expfE[k]   =1e-4;
    eyfE[k]    =1e-4;
    eypfE[k]   =1e-4;
    exfK[k]    =1e-4;
    expfK[k]   =1e-4;
    eyfK[k]    =1e-4;
    eypfK[k]   =1e-4;
    for(int j=0;j<nParam;j++){
      gE[k][j]=mt8->GetEvar(i,j);
      gK[k][j]=mt8->GetKvar(i,j);
    }
  }
  cout << "Fill LB12gsch2 event..."<<endl;
  for( int i=0 ; i<nLB12gsch2 ; i++ ){
    int k = i 
      + nLambda + nSigma + nLB12gsl
      + nLB12gsh + nLambda_h2o + nLB12psl
      + nLB12psh + nSigma_h2o;
    gEvflag[k] =0;
    gdmm[k]    =0.0;
    float xt=mt9->GetXt(i);
    float yt=mt9->GetYt(i);
    float mm=mt9->GetMM0(i);
    float mu=mt9->Getmu();
    float sigma=mt9->Getsigma();
    
    // ==== by Toshi , 19/May/2013
    //gdmm[k] = 0.0 + ( func_mmxt_->GetParameter(1) * xt );
    //gdmm[k]+= 0.0 + ( func_mmyt_->GetParameter(1) * yt );
    //gdmm[k] = -1.0*gdmm[k];//reverse the sign
    // -==== Toshi 6July2013 for test=========
    gdmm[k] = 0.00441113*xt - (0.0106527*xt*xt) 
      - (0.57073*xt*xt*xt) + (4.20109*xt*xt*xt*xt);
    gdmm[k]+= 0.000973207*yt - (0.0189651*yt*yt) 
      - (0.0653128*yt*yt*yt)+ (1.30222*yt*yt*yt*yt);
    gdmm[k] = -1.0*gdmm[k];//reverse the sign
    
    //if (mm>(1.115683-2.e-3)&&mm<1.115683+1.e-3){
    if (mm>mu-2.0*sigma&&mm<mu+1.7*sigma){
      gEvflag[k]=1205;
      deltagsch2 = ( mm + gdmm[k] - gsmm_ch2 ) / resMM2;
      chi2gsch2 += deltagsch2*deltagsch2;
      ngsch2++;
    }
    else{
      gEvflag[k]=0;
    }
    gB[k]     =mt9->GetBvar(i);
    gmomK0[k] =mt9->GetmomK0(i);
    gmomE0[k] =mt9->GetmomE0(i);
    gxfE[k]   =mt9->GetxfE(i);
    gxpfE[k]  =mt9->GetxpfE(i);
    gyfE[k]   =mt9->GetyfE(i);
    gypfE[k]  =mt9->GetypfE(i);
    gxfK[k]   =mt9->GetxfK(i);
    gxpfK[k]  =mt9->GetxpfK(i);
    gyfK[k]   =mt9->GetyfK(i);
    gypfK[k]  =mt9->GetypfK(i);
    exfE[k]   =1e-4;
    expfE[k]  =1e-4;
    eyfE[k]   =1e-4;
    eypfE[k]  =1e-4;
    exfK[k]   =1e-4;
    expfK[k]  =1e-4;
    eyfK[k]   =1e-4;
    eypfK[k]  =1e-4;
    for(int j=0;j<nParam;j++){
      gE[k][j]=mt9->GetEvar(i,j);
      gK[k][j]=mt9->GetKvar(i,j);
    }
  }
  cout << "Fill LBe10gs event..."<<endl;
  for( int i=0 ; i<nLBe10gs ; i++ ){
    int k = i 
      + nLambda + nSigma + nLB12gsl
      + nLB12gsh + nLambda_h2o + nLB12psl
      + nLB12psh + nSigma_h2o + nLB12gsch2;
    gEvflag[k] =0;
    gdmm[k]    =0.0;
    float xt=mt10->GetXt(i);
    float yt=mt10->GetYt(i);
    float mm=mt10->GetMM0(i);
    float mu=mt10->Getmu();
    float sigma=mt10->Getsigma();
    
    // ==== by Toshi , 19/May/2013
    //gdmm[k] = 0.0 + ( func_mmxt_->GetParameter(1) * xt );
    //gdmm[k]+= 0.0 + ( func_mmyt_->GetParameter(1) * yt );
    //gdmm[k] = -1.0*gdmm[k];//reverse the sign
    //
    //if (mm>(1.115683-2.e-3)&&mm<1.115683+1.e-3){
    //if (mm>mu-0.8*sigma&&mm<mu+0.8*sigma){
    //if ( mm>1.106183 && mm<1.108683 ){
    if (mm>mu-1.7*sigma&&mm<mu+1.7*sigma){
      gEvflag[k]=1000;
      deltagsb10 = ( mm + gdmm[k] - gsmm_b10 ) / resMM2;
      chi2gsb10 += deltagsb10*deltagsb10;
      ngsb10++;
    }
    else{
      gEvflag[k]=0;
    }
    gB[k]     =mt10->GetBvar(i);
    gmomK0[k] =mt10->GetmomK0(i);
    gmomE0[k] =mt10->GetmomE0(i);
    gxfE[k]   =mt10->GetxfE(i);
    gxpfE[k]  =mt10->GetxpfE(i);
    gyfE[k]   =mt10->GetyfE(i);
    gypfE[k]  =mt10->GetypfE(i);
    gxfK[k]   =mt10->GetxfK(i);
    gxpfK[k]  =mt10->GetxpfK(i);
    gyfK[k]   =mt10->GetyfK(i);
    gypfK[k]  =mt10->GetypfK(i);
    exfE[k]   =1e4;
    expfE[k]  =1e-4;
    eyfE[k]   =1e-4;
    eypfE[k]  =1e-4;
    exfK[k]   =1e-4;
    expfK[k]  =1e-4;
    eyfK[k]   =1e-4;
    eypfK[k]  =1e-4;
    for(int j=0;j<nParam;j++){
      gE[k][j]=mt10->GetEvar(i,j);
      gK[k][j]=mt10->GetKvar(i,j);
    }
  }
  cout << "Fill LHe7gs event..."<<endl;
  for( int i=0 ; i<nLHe7gs ; i++ ){
    int k = i 
      + nLambda + nSigma + nLB12gsl
      + nLB12gsh + nLambda_h2o + nLB12psl
      + nLB12psh + nSigma_h2o + nLB12gsch2
      + nLBe10gs;
    gEvflag[k] =0;
    gdmm[k]    =0.0;
    float xt=mt11->GetXt(i);
    float yt=mt11->GetYt(i);
    float mm=mt11->GetMM0(i);
    float mu=mt11->Getmu();
    float sigma=mt11->Getsigma();
    
    // ==== by Toshi , 19/May/2013
    //gdmm[k] = 0.0 + ( func_mmxt_->GetParameter(1) * xt );
    //gdmm[k]+= 0.0 + ( func_mmyt_->GetParameter(1) * yt );
    //gdmm[k] = -1.0*gdmm[k];//reverse the sign
    //
    // -==== Toshi 6July2013 for test=========
    xt = xt/3.0;
    gdmm[k] = 0.00441113*xt - (0.0106527*xt*xt) 
      - (0.57073*xt*xt*xt) + (4.20109*xt*xt*xt*xt);
    gdmm[k]+= 0.000973207*yt - (0.0189651*yt*yt) 
      - (0.0653128*yt*yt*yt)+ (1.30222*yt*yt*yt*yt);
    gdmm[k] = -1.0*gdmm[k];//reverse the sign
    //if (mm>(1.115683-2.e-3)&&mm<1.115683+1.e-3){
    
    //if (mm>mu-1.0*sigma&&mm<mu+1.0*sigma){
    if (mm>mu-1.5*sigma&&mm<mu+1.5*sigma){ // Toshi , 7Nov2013
      //if (mm>mu-1.5*sigma&&mm<mu+1.5*sigma){
      //if (mm>mu-2.0*sigma&&mm<mu+2.0*sigma){
      //if (mm>mu-2.2*sigma&&mm<mu+2.0*sigma){
      //if (mm>mu-2.5e-3 && mm<mu+1.5*sigma){ // for test , 18Oct2013
      //if (mm>mu-1.5*sigma&&mm<mu+1.5*sigma){
      //if (mm>mu-1.0*sigma&&mm<mu+1.0*sigma){
      //if (mm>mu-2.0*sigma&&mm<mu+1.7*sigma){
      //if (mm>mu-2.2*sigma&&mm<mu+1.0*sigma){
      gEvflag[k]=700;
      //deltagsli7 = ( mm + gdmm[k] - gsmm_li7 ) / resMM2;
      deltagsli7 = ( mm - gsmm_li7 ) / resMM2; // Toshi , 5Sep2013
      chi2gsli7 += deltagsli7*deltagsli7;
      ngsli7++;
    }
    else{
      gEvflag[k]=0;
    }
    gB[k]     =mt11->GetBvar(i);
    gmomK0[k] =mt11->GetmomK0(i);
    gmomE0[k] =mt11->GetmomE0(i);
    gxfE[k]   =mt11->GetxfE(i);
    gxpfE[k]  =mt11->GetxpfE(i);
    gyfE[k]   =mt11->GetyfE(i);
    gypfE[k]  =mt11->GetypfE(i);
    gxfK[k]   =mt11->GetxfK(i);
    gxpfK[k]  =mt11->GetxpfK(i);
    gyfK[k]   =mt11->GetyfK(i);
    gypfK[k]  =mt11->GetypfK(i);
    exfE[k]   =1e+4;
    expfE[k]  =1e-4;
    eyfE[k]   =1e-4;
    eypfE[k]  =1e-4;
    exfK[k]   =1e-4;
    expfK[k]  =1e-4;
    eyfK[k]   =1e-4;
    eypfK[k]  =1e-4;
    for(int j=0;j<nParam;j++){
      gE[k][j]=mt11->GetEvar(i,j);
      gK[k][j]=mt11->GetKvar(i,j);
    }
  }
  cout << "Fill 12LB core excited 1 (low) event..."<<endl;
  for( int i=0 ; i<nLB12cel ; i++ ){
    int k = i 
      + nLambda + nSigma + nLB12gsl
      + nLB12gsh + nLambda_h2o + nLB12psl
      + nLB12psh + nSigma_h2o + nLB12gsch2
      + nLBe10gs + nLHe7gs;
    gEvflag[k] =0;
    gdmm[k]    =0.0;
    float xt=mt12->GetXt(i);
    float yt=mt12->GetYt(i);
    float mm=mt12->GetMM0(i);
    float mu=mt12->Getmu();
    float sigma=mt12->Getsigma();
    gdmm[k] = 0.0;
    
    if ( mm>mu-2.0*sigma && mm<mu+2.0*sigma ){
    //if ( mm> 1.106983 && mm<1.107983 ){//just for test
    //if ( mm> 1.10583 && mm<1.107983 ){//just for test
      gEvflag[k]=1290;
      deltacel = ( mm - cemm ) / resMM2; 
      chi2cel += deltacel*deltacel;
      ncel++;
    }
    else{
      gEvflag[k]=0;
    }
    gB[k]     =mt12->GetBvar(i);
    gmomK0[k] =mt12->GetmomK0(i);
    gmomE0[k] =mt12->GetmomE0(i);
    gxfE[k]   =mt12->GetxfE(i);
    gxpfE[k]  =mt12->GetxpfE(i);
    gyfE[k]   =mt12->GetyfE(i);
    gypfE[k]  =mt12->GetypfE(i);
    gxfK[k]   =mt12->GetxfK(i);
    gxpfK[k]  =mt12->GetxpfK(i);
    gyfK[k]   =mt12->GetyfK(i);
    gypfK[k]  =mt12->GetypfK(i);
    exfE[k]   =1e+4;
    expfE[k]  =1e-4;
    eyfE[k]   =1e-4;
    eypfE[k]  =1e-4;
    exfK[k]   =1e-4;
    expfK[k]  =1e-4;
    eyfK[k]   =1e-4;
    eypfK[k]  =1e-4;
    for(int j=0;j<nParam;j++){
      gE[k][j]=mt12->GetEvar(i,j);
      gK[k][j]=mt12->GetKvar(i,j);
    }
  }
  
  cout << "Fill 12LB core excited 1 (high) event..."<<endl;
  for( int i=0 ; i<nLB12ceh ; i++ ){
    int k = i 
      + nLambda + nSigma + nLB12gsl
      + nLB12gsh + nLambda_h2o + nLB12psl
      + nLB12psh + nSigma_h2o + nLB12gsch2
      + nLBe10gs + nLHe7gs + nLB12cel;
    gEvflag[k] =0;
    gdmm[k]    =0.0;
    float xt=mt13->GetXt(i);
    float yt=mt13->GetYt(i);
    float mm=mt13->GetMM0(i);
    float mu=mt13->Getmu();
    float sigma=mt13->Getsigma();
    gdmm[k] = 0.0;
    if ( mm>mu-2.0*sigma && mm<mu+2.0*sigma ){
    //if ( mm> 1.106983 && mm<1.107983 ){// just for test (1)
    //if ( mm> 1.10583 && mm<1.107983 ){//just for test (2)
      gEvflag[k]=1291;
      deltaceh = ( mm - cemm_high ) / resMM2; 
      chi2ceh += deltaceh*deltaceh;
      nceh++;
    }
    else{
      gEvflag[k]=0;
    }
    gB[k]     =mt13->GetBvar(i);
    gmomK0[k] =mt13->GetmomK0(i);
    gmomE0[k] =mt13->GetmomE0(i);
    gxfE[k]   =mt13->GetxfE(i);
    gxpfE[k]  =mt13->GetxpfE(i);
    gyfE[k]   =mt13->GetyfE(i);
    gypfE[k]  =mt13->GetypfE(i);
    gxfK[k]   =mt13->GetxfK(i);
    gxpfK[k]  =mt13->GetxpfK(i);
    gyfK[k]   =mt13->GetyfK(i);
    gypfK[k]  =mt13->GetypfK(i);
    exfE[k]   =1e+4;
    expfE[k]  =1e-4;
    eyfE[k]   =1e-4;
    eypfE[k]  =1e-4;
    exfK[k]   =1e-4;
    expfK[k]  =1e-4;
    eyfK[k]   =1e-4;
    eypfK[k]  =1e-4;
    for(int j=0;j<nParam;j++){
      gE[k][j]=mt13->GetEvar(i,j);
      gK[k][j]=mt13->GetKvar(i,j);
    }
  }
  
  cout << "Fill 52LV (1) event..." << endl;
  for( int i=0 ; i<nLV52_1 ; i++ ){
    int k = i 
      + nLambda + nSigma + nLB12gsl
      + nLB12gsh + nLambda_h2o + nLB12psl
      + nLB12psh + nSigma_h2o + nLB12gsch2
      + nLBe10gs + nLHe7gs + nLB12cel + nLB12ceh;
    gEvflag[k] =0;
    gdmm[k]    =0.0;
    float xt=mt14->GetXt(i);
    float yt=mt14->GetYt(i);
    float mm=mt14->GetMM0(i);
    float mu=mt14->Getmu();
    float sigma=mt14->Getsigma();
    gdmm[k] = 0.0;
    if ( mm>mu-2.0*sigma && mm<mu+2.0*sigma ){
    //if ( mm> 1.106983 && mm<1.107983 ){// just for test (1)
    //if ( mm> 1.10583 && mm<1.107983 ){//just for test (2)
      gEvflag[k]=5200;
      deltacr52_1 = ( mm - mm1_cr52 ) / resMM2; 
      chi2cr52_1 += deltacr52_1*deltacr52_1;
      ncr52_1++;
    }
    else{
      gEvflag[k]=0;
    }
    gB[k]     =mt14->GetBvar(i);
    gmomK0[k] =mt14->GetmomK0(i);
    gmomE0[k] =mt14->GetmomE0(i);
    gxfE[k]   =mt14->GetxfE(i);
    gxpfE[k]  =mt14->GetxpfE(i);
    gyfE[k]   =mt14->GetyfE(i);
    gypfE[k]  =mt14->GetypfE(i);
    gxfK[k]   =mt14->GetxfK(i);
    gxpfK[k]  =mt14->GetxpfK(i);
    gyfK[k]   =mt14->GetyfK(i);
    gypfK[k]  =mt14->GetypfK(i);
    exfE[k]   =1e+4;
    expfE[k]  =1e-4;
    eyfE[k]   =1e-4;
    eypfE[k]  =1e-4;
    exfK[k]   =1e-4;
    expfK[k]  =1e-4;
    eyfK[k]   =1e-4;
    eypfK[k]  =1e-4;
    for(int j=0;j<nParam;j++){
      gE[k][j]=mt14->GetEvar(i,j);
      gK[k][j]=mt14->GetKvar(i,j);
    }
  }
  
  cout << "Fill end..."<<endl;
  
  logfile << "chi2l="    << chi2l/nl        << " ..." << nl   <<endl;
  logfile << "chi2s="    << chi2s/ns        << " ..." << ns   <<endl;
  logfile << "chi2h2o="  << chi2h2o/nh2o    << " ..." << nh2o <<endl;
  logfile << "chi2h2os=" << chi2h2os/nh2os  << " ..." << nh2os<<endl;
  logfile << "chi2gsl="  << chi2gsl/ngsl    << " ..." << ngsl <<endl;
  logfile << "chi2gsh="  << chi2gsh/ngsh    << " ..." << ngsh <<endl;
  logfile << "chi2psl="  << chi2psl/npsl    << " ..." << npsl <<endl;
  logfile << "chi2psh="  << chi2psh/npsh    << " ..." << npsh <<endl;
  logfile << "chi2gsch2="<< chi2gsch2/ngsch2<< " ..." << ngsch2<<endl;
  logfile << "chi2gsb10="<< chi2gsb10/ngsb10<< " ..." << ngsb10<<endl;
  logfile << "chi2gsli7="<< chi2gsli7/ngsli7<< " ..." << ngsli7<<endl;
  logfile << "chi2cel="  << chi2cel/ncel    << " ..." << ncel  <<endl;
  logfile << "chi2ceh="  << chi2ceh/nceh    << " ..." << nceh  <<endl;
  logfile << "chi2cr52_1="<<chi2cr52_1/ncr52_1<< " ..." << ncr52_1<<endl;
  
  ifstream MKmom(cMKmom);
  ifstream MEmom(cMEmom);
  ifstream MKxpt(cMKxpt);
  ifstream MExpt(cMExpt);
  ifstream MKypt(cMKypt);
  ifstream MEypt(cMEypt);
  //cout << " OK " << endl;
  for (int i=0;i<nParam;i++){
    int i0=i;
    int i1=i+nParam;
    int i2=i+nParam*2;
    int i3=i+nParam*3;
    int i4=i+nParam*4;
    int i5=i+nParam*5;
    if (i<nParamE){
      MEmom >> coeff[i0] >> oa[i0] >> ob[i0] >> oc[i0] >> od[i0];
      MExpt >> coeff[i2] >> oa[i2] >> ob[i2] >> oc[i2] >> od[i2];
      MEypt >> coeff[i4] >> oa[i4] >> ob[i4] >> oc[i4] >> od[i4]; 
    }
    else{
      coeff[i0]=0.;
      coeff[i2]=0.;
      coeff[i4]=0.;
      oa[i0]=-1; 
      ob[i0]=-1; 
      oc[i0]=-1; 
      od[i0]=-1; 
      oa[i2]=-1; 
      ob[i2]=-1; 
      oc[i2]=-1; 
      od[i2]=-1; 
      oa[i4]=-1; 
      ob[i4]=-1; 
      oc[i4]=-1; 
      od[i4]=-1; 
    }
    if (i<nParamK){
      MKmom >> coeff[i1] >> oa[i1] >> ob[i1] >> oc[i1] >> od[i1]; 
      MKxpt >> coeff[i3] >> oa[i3] >> ob[i3] >> oc[i3] >> od[i3];
      MKypt >> coeff[i5] >> oa[i5] >> ob[i5] >> oc[i5] >> od[i5];
    }
    else{
      coeff[i1]=0.;
      coeff[i3]=0.;
      coeff[i5]=0.; 
      oa[i1]=-1; 
      ob[i1]=-1; 
      oc[i1]=-1; 
      od[i1]=-1; 
      oa[i3]=-1; 
      ob[i3]=-1; 
      oc[i3]=-1; 
      od[i3]=-1; 
      oa[i5]=-1; 
      ob[i5]=-1; 
      oc[i5]=-1; 
      od[i5]=-1; 
    }
  }
  MKmom.close();
  MEmom.close();
  MKxpt.close();
  MExpt.close();
  MKypt.close();
  MEypt.close();
   

   //for debug	
   /*float etest[1][nParam];
   float ktest[1][nParam];
   double cpetest[nParam];
   double cpktest[nParam];
   double cxetest[nParam];
   double cxktest[nParam];
   double cyetest[nParam];
   double cyktest[nParam];
   for (int i=0;i<nParam;i++){
      etest[0][i]=0;
      ktest[0][i]=0;
      int i0=i;
      int i1=i+nParam;
      int i2=i+nParam*2;
      int i3=i+nParam*3;
      int i4=i+nParam*4;
      int i5=i+nParam*5;
      cpetest[i]=coeff[i0];
      cpktest[i]=coeff[i1];
      cxetest[i]=coeff[i2];
      cxktest[i]=coeff[i3];
      cyetest[i]=coeff[i4];
      cyktest[i]=coeff[i5];
   }
   float kxftest= (18.493436-KXFPm)/KXFPr;
   float kxpftest=(0.0589412-KXpFPm)/KXpFPr;
   float kyftest= (2.2395846-KYFPm)/KYFPr;
   float kypftest=(-0.000126-KYpFPm)/KYpFPr;
   float exftest= (-6.953080-EXFPm)/EXFPr;
   float expftest=(-0.034705-EXpFPm)/EXpFPr;
   float eyftest= (-1.330210-EYFPm)/EYFPr;
   float eypftest=(-0.035815-EYpFPm)/EYpFPr;
   GetVar(0,exftest,expftest,eyftest,eypftest,etest);
   GetVar(0,kxftest,kxpftest,kyftest,kypftest,ktest);
   for(int j=0;j<nParam;j++){
      cout <<"j="<<j<< ",cxetest="<<cxetest[j]<<endl;
      cout <<"j="<<j<< ",etest="<<etest[0][j]<<endl;
   }
   float EmomRtest=recfcn(cpetest,0,etest,6);
   float KmomRtest=recfcn(cpktest,0,ktest,6);
   float ExptRtest=recfcn(cxetest,0,etest,6);
   float KxptRtest=recfcn(cxktest,0,ktest,6);
   float EyptRtest=recfcn(cyetest,0,etest,6);
   float KyptRtest=recfcn(cyktest,0,ktest,6);
   float mmRtest=calcMM(2.3441201,EmomRtest,-ExptRtest,-EyptRtest,KmomRtest,KxptRtest,KyptRtest,12);
   cout << "Kmom="<<KmomRtest << endl;
   cout << "Emom="<<EmomRtest << endl;
   cout << "Kxpt="<<KxptRtest << endl;
   cout << "Expt="<<ExptRtest << endl;
   cout << "Kypt="<<KyptRtest << endl;
   cout << "Eypt="<<EyptRtest << endl;
   cout << "MM="<<mmRtest*1e3 << endl;*/

   
   
   float sigmaL=mt1->Getsigma();
   float sigmaS=mt2->Getsigma();
   /*
   float mmR[nAllpeak][50];
   float dl[nAllpeak][50];
   float ds[nAllpeak][50];
   //float dgsl[nAllpeak][100];
   //float da[nAllpeak][100];
   float dgsl[10000][100];
   float gsh[10000][100];
   char tempc[200];
   */
   const int aaaa = nAllpeak;
   //float mmR[nAllpeak][50];
   Float_t mmR[aaaa][50];
   //float dl[nAllpeak][50];
   //float ds[nAllpeak][50];
   //float dgsl[nAllpeak][100];
   //float da[nAllpeak][100];
   //float dgsl[10000][100];
   //float gsh[10000][100];
   char tempc[200];
   cout << " ... Iteraion start ... (" << MaxIterate << " times !!)"<<endl;
   /*TGraph *gdl = new TGraph(MaxIterate);
     TGraph *gds = new TGraph(MaxIterate);
     TGraph *gdgsl = new TGraph(MaxIterate);
   */
   //float EmomR[nAllpeak];
   //float ExptR[nAllpeak];
   //float EyptR[nAllpeak];
   //float KmomR[nAllpeak];
   //float KxptR[nAllpeak];
   //float KyptR[nAllpeak];
   Float_t EmomR[nAllpeak];
   Float_t ExptR[nAllpeak];
   Float_t EyptR[nAllpeak];
   Float_t KmomR[nAllpeak];
   Float_t KxptR[nAllpeak];
   Float_t KyptR[nAllpeak];
   double cpk[nParam];
   double cpe[nParam];
   double cxk[nParam];
   double cxe[nParam];
   double cyk[nParam];
   double cye[nParam];
   float cutL1=1.105;
   float cutL2=1.125;
   float cutS1=1.182;
   float cutS2=1.202;
   float cutgsl1=1.105;
   float cutgsl2=1.125;
   float cutgsh1=1.105;
   float cutgsh2=1.125;
   float cutLh2o1=1.105;
   float cutLh2o2=1.125;
   float cutSh2o1=1.182;
   float cutSh2o2=1.202;
   float cutpsl1=1.105;
   float cutpsl2=1.125;
   float cutpsh1=1.105;
   float cutpsh2=1.125;
   float cutgsch2_1=1.105;
   float cutgsch2_2=1.125;
   float cutgsb10_1;
   float cutgsb10_2;
   float cutgsli7_1;
   float cutgsli7_2;
   Float_t cutcel_1=0.0,cutcel_2=0.0;
   Float_t cutceh_1=0.0,cutceh_2=0.0;
   Float_t cut1cr52_1=0.0,cut2cr52_1=0.0;
   char funcname0[100];
   char funcname1[100];
   char funcname2[100];
   char funcname3[100];
   char funcname4[100];
   char funcname5[100];
   char funcname6[100];
   char funcname7[100];
   char funcname8[100];
   char histname0[100];
   char histname1[100];
   char histname2[100];
   char histname3[100];
   char histname4[100];
   char histname5[100];
   char histname6[100];
   char histname7[100];
   char histname8[100];
   char histname10[100];
   char histname11[100];
   char histname12[100];
   char histname13[100];
   char histname20[100];
   char histname21[100];
   char histname22[100];
   char histname23[100];
   char histname30[100];
   char histname31[100];
   char histname32[100];
   char histname33[100];
   char histname40[100];
   char histname41[100];
   char histname42[100];
   char histname43[100];
   char histname50[100];
   char histname51[100];
   char histname52[100];
   char histname53[100];
   char histname60[100];
   char histname61[100];
   char histname62[100];
   char histname63[100];
   char histname64[100];// Lambda Missing mass - 1.115683 org (Toshi , 9/Jan/2013)
   char histname65[100];// Lambda Missing mass - 1.115683 bg  (Toshi , 9/Jan/2013)
   char histname66[100];// Lambda Missing mass - 1.115683 subt(Toshi , 9/Jan/2013)
   char histname67[100];// 12LB   Missing mass - 1.115683 org (Toshi , 9/Jan/2013)
   char histname68[100];// 12LB   Missing mass - 1.115683 bg  (Toshi , 9/Jan/2013)
   char histname69[100];// 12LB   Missing mass - 1.115683 subt(Toshi , 9/Jan/2013)
   
   for (int k=0;k<MaxIterate;k++){
     cout << "  ### ITERATION ###" << endl;
     cout << "     " << k+1 << "/" << MaxIterate << endl;
     cout << "  #################" << endl;
     sprintf(funcname0,"fl%d",k);
     sprintf(funcname1,"fs%d",k);
     sprintf(funcname2,"fgsl%d",k);
     sprintf(funcname3,"fgsh%d",k);
     sprintf(funcname4,"flh2o%d",k);
     sprintf(funcname5,"fcpsl%d",k);
     sprintf(funcname6,"fcpsh%d",k);
     sprintf(funcname7,"fsh2o%d",k);
     sprintf(funcname8,"fgsch2%d",k);
     sprintf(histname0,"hl%d",k);
     sprintf(histname1,"hs%d",k);
     sprintf(histname2,"hgsl%d",k);
     sprintf(histname3,"hgsh%d",k);
     sprintf(histname4,"hlh2o%d",k);
     sprintf(histname5,"hcpsl%d",k);
     sprintf(histname6,"hcpsh%d",k);
     sprintf(histname7,"hsh2o%d",k);
     sprintf(histname8,"hgsch2%d",k);
     sprintf(histname10,"hdl%d",k);
     sprintf(histname11,"hds%d",k);
     sprintf(histname12,"hdgsl%d",k);
     sprintf(histname13,"hdgsh%d",k);
     sprintf(histname20,"hddl%d",k);
     sprintf(histname21,"hdds%d",k);
     sprintf(histname22,"hddgsl%d",k);
     sprintf(histname23,"hddgsh%d",k);
     sprintf(histname30,"hkdl%d",k);
     sprintf(histname31,"hkds%d",k);
     sprintf(histname32,"hkdgsl%d",k);
     sprintf(histname33,"hkdgsh%d",k);
     sprintf(histname40,"hedl%d",k);
     sprintf(histname41,"heds%d",k);
     sprintf(histname42,"hedgsl%d",k);
     sprintf(histname43,"hedgsh%d",k);
     sprintf(histname50,"hla%d",k);
     sprintf(histname51,"hsa%d",k);
     sprintf(histname52,"hgsla%d",k);
     sprintf(histname53,"hgsha%d",k);
     sprintf(histname60,"hlb%d",k);
     sprintf(histname61,"hsb%d",k);
     sprintf(histname62,"hgslb%d",k);
     sprintf(histname63,"hgshb%d",k);
     sprintf(histname64,"hLS_org%d" ,k);
     sprintf(histname65,"hLS_bg%d"  ,k);
     sprintf(histname66,"hLS_subt%d",k);
     sprintf(histname67,"h12LB_org%d" ,k);
     sprintf(histname68,"h12LB_bg%d"  ,k);
     sprintf(histname69,"h12LB_subt%d",k);
     
     /*
     TH2F *h10 = new TH2F("h10","",100, cut1[0]-2e-3,cut1[1]+2e-3,100,-.01,.01);
     TH2F *h11 = new TH2F("h11","",100, cut2[0]-2e-3,cut2[1]+2e-3,100,-.01,.01);
     TH2F *h12 = new TH2F("h12","",100, cut3[0]-2e-3-ml,cut3[1]+2e-3-ml,100,-.01,.01);
     TH2F *h13 = new TH2F("h13","",100, cut3[0]-2e-3,cut3[1]+2e-3,100,-.01,.01);
     TH1F *h20 = new TH1F("h20","",150, -.005,.005);
     TH1F *h21 = new TH1F("h21","",150, -.005,.005);
     TH1F *h22 = new TH1F("h22","",150, -.005,.005);
     TH1F *h23 = new TH1F("h23","",150, -.005,.005);
     TH2F *h30 = new TH2F("h30","",100, 1.0,1.4,100, -.010,.010);
     TH2F *h31 = new TH2F("h31","",100, 1.0,1.4,100, -.010,.010);
     TH2F *h32 = new TH2F("h32","",100, 1.0,1.4,100, -.010,.010);
     TH2F *h33 = new TH2F("h33","",100, 1.0,1.4,100, -.010,.010);
     TH2F *h40 = new TH2F("h40","",100, 0.65,1.05,100, -.010,.010);
     TH2F *h41 = new TH2F("h41","",100, 0.65,1.05,100, -.010,.010);
     TH2F *h42 = new TH2F("h42","",100, 0.65,1.05,100, -.010,.010);
     TH2F *h43 = new TH2F("h43","",100, 0.65,1.05,100, -.010,.010);
     TH2F *h50 = new TH2F("h50","",500, -60., 60.,500, -6., 6.);
     TH2F *h51 = new TH2F("h51","",500, -0.2, 0.2,500, -.06, 0.);
     TH2F *h52 = new TH2F("h52","",500, -0.2, 0.2,500, -.1, 0.);
     TH2F *h53 = new TH2F("h53","",500, -60., 60.,500, .7, 1.);
     */
     sprintf(tempc,"h10_%d",k);
     TH2F *h10 = new TH2F(tempc,"",100, cut1[0]-2e-3,cut1[1]+2e-3,100,-.01,.01);
     sprintf(tempc,"h11_%d",k);
     TH2F *h11 = new TH2F(tempc,"",100, cut2[0]-2e-3,cut2[1]+2e-3,100,-.01,.01);
     sprintf(tempc,"h12_%d",k);
     TH2F *h12 = new TH2F(tempc,"",100, cut3[0]-2e-3-ml,cut3[1]+2e-3-ml,100,-.01,.01);
     sprintf(tempc,"h13_%d",k);
     TH2F *h13 = new TH2F(tempc,"",100, cut3[0]-2e-3,cut3[1]+2e-3,100,-.01,.01);
     sprintf(tempc,"h20_%d",k);
     TH1F *h20 = new TH1F(tempc,"",150, -.005,.005);
     sprintf(tempc,"h21_%d",k);
     TH1F *h21 = new TH1F(tempc,"",150, -.005,.005);
     sprintf(tempc,"h22_%d",k);
     TH1F *h22 = new TH1F(tempc,"",150, -.005,.005);
     sprintf(tempc,"h23_%d",k);
     TH1F *h23 = new TH1F(tempc,"",150, -.005,.005);
     sprintf(tempc,"h30_%d",k);
     TH2F *h30 = new TH2F(tempc,"",100, 1.0,1.4,100, -.010,.010);
     sprintf(tempc,"h31_%d",k);
     TH2F *h31 = new TH2F(tempc,"",100, 1.0,1.4,100, -.010,.010);
     sprintf(tempc,"h32_%d",k);
     TH2F *h32 = new TH2F(tempc,"",100, 1.0,1.4,100, -.010,.010);
     sprintf(tempc,"h33_%d",k);
     TH2F *h33 = new TH2F(tempc,"",100, 1.0,1.4,100, -.010,.010);
     sprintf(tempc,"h40_%d",k);
     TH2F *h40 = new TH2F(tempc,"",100, 0.65,1.05,100, -.010,.010);
     sprintf(tempc,"h41_%d",k);
     TH2F *h41 = new TH2F(tempc,"",100, 0.65,1.05,100, -.010,.010);
     sprintf(tempc,"h42_%d",k);
     TH2F *h42 = new TH2F(tempc,"",100, 0.65,1.05,100, -.010,.010);
     sprintf(tempc,"h43_%d",k);
     TH2F *h43 = new TH2F(tempc,"",100, 0.65,1.05,100, -.010,.010);
     sprintf(tempc,"h50_%d",k);
     TH2F *h50 = new TH2F(tempc,"",500, -60., 60.,500, -6., 6.);
     sprintf(tempc,"h51_%d",k);
     TH2F *h51 = new TH2F(tempc,"",500, -0.2, 0.2,500, -.06, 0.);
     sprintf(tempc,"h52_%d",k);
     TH2F *h52 = new TH2F(tempc,"",500, -0.2, 0.2,500, -.1, 0.);
     sprintf(tempc,"h53_%d",k);
     TH2F *h53 = new TH2F(tempc,"",500, -60., 60.,500, .7, 1.);
     /*TH1F *h100 = new TH1F("h100","",100, cut1[0]-2e-3,cut1[1]+2e-3);
       TH1F *h101 = new TH1F("h101","",100, cut2[0]-2e-3,cut2[1]+2e-3);
       TH1F *h1201 = new TH1F("h1201","",100, cut3[0]-2e-3-ml,cut3[1]+2e-3-ml);
       TH1F *h1202 = new TH1F("h1202","",100, cut4[0]-2e-3-ml,cut4[1]+2e-3-ml);
     */
     sprintf(tempc,"h100_%d",k);
     TH1F *h100  = new TH1F(tempc ,"",100, cut1[0]-2e-3,cut1[1]+2e-3);
     sprintf(tempc,"h101_%d",k);
     TH1F *h101  = new TH1F(tempc ,"",100, cut2[0]-2e-3,cut2[1]+2e-3);
     sprintf(tempc,"h1201_%d",k);
     TH1F *h1201 = new TH1F(tempc ,"",100, -0.020,-0.005);
     sprintf(tempc,"h1202_%d",k);
     TH1F *h1202 = new TH1F(tempc ,"",100, -0.020,-0.005);
     sprintf(tempc,"h110_%d",k);
     TH1F *h110  = new TH1F(tempc ,"",100, cut5[0]-2e-3,cut5[1]+2e-3);
     sprintf(tempc,"h111_%d",k);
     TH1F *h111  = new TH1F(tempc ,"",100, cut8[0]-2e-3,cut8[1]+2e-3);
     sprintf(tempc,"h1203_%d",k);
     TH1F *h1203 = new TH1F(tempc ,"",100, -0.010,0.005);
     sprintf(tempc,"h1204_%d",k);
     TH1F *h1204 = new TH1F(tempc ,"",100, -0.010,0.005);
     sprintf(tempc,"h1205_%d",k);
     TH1F *h1205 = new TH1F(tempc ,"",100, -0.020,-0.005);
     sprintf(tempc,"h1000_%d",k);
     //TH1F *h1000 = new TH1F(tempc ,"",100, cut10[0]-ml-2.0e-3,cut10[0]-ml+2.0e-3);
     //TH1F *h1000 = new TH1F(tempc ,"",100, -8.5e-3 - 5.0e-3,-8.5e3 + 5.0e-3);
     //TH1F *h1000 = new TH1F(tempc ,"",50, -8.5e-3 - 5.0e-3,-8.5e3 + 5.0e-3);
     TH1F *h1000 = new TH1F(tempc ,"",25, -8.5e-3 - 5.0e-3 , -8.5e-3 + 5.0e-3);//g.s.
     //TH1F *h1000 = new TH1F(tempc ,"",25, 2.2e-3 - 5.0e-3 , 2.2e-3 + 5.0e-3);//core excited(Toshi,22Jan2014)
     sprintf(tempc,"h700_%d",k);
     TH1F *h700  = new TH1F(tempc ,"", 25, -5.7e-3 - 5.0e-3 , -5.7e-3 + 5.0e-3);
     sprintf(tempc,"h1290_%d",k);
     TH1F *h1290 = new TH1F(tempc ,"", 50, -8.5e-3 - 5.0e-3 , -8.5e-3 + 5.0e-3);
     sprintf(tempc,"h1291_%d",k);
     TH1F *h1291 = new TH1F(tempc ,"", 50, -8.5e-3 - 5.0e-3 , -8.5e-3 + 5.0e-3);
     sprintf(tempc,"h5200_%d",k);
     //TH1F *h5200 = new TH1F(tempc ,"", 20, -16.0e-3 - 10.0e-3 , -16.0e-3 + 10.0e-3);
     TH1F *h5200 = new TH1F(tempc ,"", 20, 0.5e-3 - 10.0e-3 , 0.5e-3 + 10.0e-3);// 28Jan2014, Toshi
     /*
     TH1F *h100  = new TH1F("h100" ,"",100, cut1[0]-2e-3,cut1[1]+2e-3);
     TH1F *h101  = new TH1F("h101" ,"",100, cut2[0]-2e-3,cut2[1]+2e-3);
     TH1F *h1201 = new TH1F("h1201","",100, -0.020,-0.005);
     TH1F *h1202 = new TH1F("h1202","",100, -0.020,-0.005);
     TH1F *h110  = new TH1F("h110" ,"",100, cut5[0]-2e-3,cut5[1]+2e-3);
     TH1F *h111  = new TH1F("h111" ,"",100, cut8[0]-2e-3,cut8[1]+2e-3);
     TH1F *h1203 = new TH1F("h1203","",100, -0.010,0.005);
     TH1F *h1204 = new TH1F("h1204","",100, -0.010,0.005);
     */
     /*TH1F *h111 = new TH1F("h111","",100, cut2[0]-2e-3,cut2[1]+2e-3);
       TH1F *h1211 = new TH1F("h1211","",100, cut3[0]-2e-3-ml,cut3[1]+2e-3-ml);
       TH1F *h120 = new TH1F("h120","",100, cut1[0]-2e-3,cut1[1]+2e-3);
       TH1F *h121 = new TH1F("h121","",100, cut2[0]-2e-3,cut2[1]+2e-3);
       TH1F *h1221 = new TH1F("h1221","",100, cut3[0]-2e-3-ml,cut3[1]+2e-3-ml);*/
     sprintf(tempc,"f100_%d",k);
     //TF1 *f100 = new TF1("f100",
     TF1 *f100 = new TF1(tempc,
			 //"[0]*exp(-pow((x-[1])/[2],2)/2)+[3]*exp(-pow((x-[4])/[5],2)/2)",
			 //"[0]*exp(-pow((x-[1])/[2],2)/2)+[3]*x*x+[4]*x+[5]",
			 "[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",
			 cut1[0]-2e-3,cut1[1]+2e-3);
     sprintf(tempc,"f101_%d",k);
     //TF1 *f101 = new TF1("f101",
     TF1 *f101 = new TF1(tempc,
			 //"[0]*exp(-pow((x-[1])/[2],2)/2)+[3]*exp(-pow((x-[4])/[5],2)/2)",
			 //"[0]*exp(-pow((x-[1])/[2],2)/2)+[3]*x*x+[4]*x+[5]",
			 "[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",
			 cut2[0]-2e-3,cut2[1]+2e-3);
     sprintf(tempc,"f1201_%d",k);
     //TF1 *f1201 = new TF1("f1201",
     TF1 *f1201 = new TF1(tempc,
			  "[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",
			  //"[0]*exp(-pow((x-[1])/[2],2)/2)+[3]*x*x+[4]*x+[5]",
			  cut3[0]-2e-3-ml,cut3[1]+2e-3-ml);
     sprintf(tempc,"f1202_%d",k);
     //TF1 *f1202 = new TF1("f1202",
     TF1 *f1202 = new TF1(tempc,
			  "[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",
			  //"[0]*exp(-pow((x-[1])/[2],2)/2)+[3]*(x-[4])**2+[5]",
			  //"[0]*exp(-pow((x-[1])/[2],2)/2)+[3]*exp(-pow((x-[4])/[5],2)/2)",
			  //"[0]*exp(-pow((x-[1])/[2],2)/2)+[3]*x*x+[4]*x+[5]",
			  cut4[0]-2e-3-ml,cut4[1]+2e-3-ml);
     sprintf(tempc,"f110_%d",k);
     //TF1 *f110 = new TF1("f110",
     TF1 *f110 = new TF1(tempc,
			 "[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",
			 cut5[0]-2e-3,cut5[1]+2e-3);
     sprintf(tempc,"f111_%d",k);
     //TF1 *f111 = new TF1("f111",
     TF1 *f111 = new TF1(tempc,
			 "[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",
			 cut8[0]-2e-3,cut8[1]+2e-3);
     sprintf(tempc,"f1203_%d",k);
     TF1 *f1203 = new TF1(tempc,
			  "[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",
			  cut6[0]-2e-3-ml,cut6[1]+2e-3-ml);
     sprintf(tempc,"f1204_%d",k);
     //TF1 *f1204 = new TF1("f1204",
     TF1 *f1204 = new TF1(tempc,
			  "[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",
			  cut7[0]-2e-3-ml,cut7[1]+2e-3-ml);
     sprintf(tempc,"f1205_%d",k);
     //TF1 *f1205 = new TF1("f1205",
     TF1 *f1205 = new TF1(tempc,
			  "[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",
			  cut9[0]-2e-3-ml,cut9[1]+2e-3-ml);
     sprintf(tempc,"f1000_%d",k);
     TF1 *f1000 = new TF1(tempc,
			  "[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",
			  cut10[0]-2e-3-ml,cut10[1]+2e-3-ml);
     sprintf(tempc,"f700_%d",k);
     TF1 *f700 = new TF1(tempc,
			 "[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",
			 cut11[0]-2e-3-ml,cut11[1]+2e-3-ml);
     sprintf(tempc,"f1290_%d",k);
     TF1 *f1290 = new TF1(tempc,
			  "[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",
			  cut12[0]-2e-3-ml,cut12[1]+2e-3-ml);
     sprintf(tempc,"f1291_%d",k);
     TF1 *f1291 = new TF1(tempc,
			  "[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",
			  cut13[0]-2e-3-ml,cut13[1]+2e-3-ml);
     sprintf(tempc,"f5200_%d",k);
     TF1 *f5200 = new TF1(tempc,
			  "[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",
			  cut14[0]-2e-3-ml,cut14[1]+2e-3-ml);
     
     logfile <<"Iteration : "<< k <<endl; 
     
     // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     //double chi2=mtune(coeff, k);  // Matrix tuning !!!!!
     // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     //double chi2  =mtune2(coeff, k , 1);  // Matrix tuning !!!!!
     //double chi2_2=mtune2(coeff, k , 0);  // Matrix tuning !!!!!
     // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     double chi2=mtune2(coeff, k , 0);  // Matrix tuning !!!!!
     //double chi2=mtune2(coeff, k , 1);  // Matrix tuning !!!!!//angle
     
     
     logfile <<"minimization chi2="<< chi2 <<endl;
     //logfile <<"minimization chi2_2="<< chi2_2 <<endl; 
     
     for (int i=0;i<nParam;i++){
       int i0=i;
       int i1=i+nParam;
       int i2=i+nParam*2;
       int i3=i+nParam*3;
       int i4=i+nParam*4;
       int i5=i+nParam*5;
       cpe[i]=coeff[i0];
       cpk[i]=coeff[i1];
       cxe[i]=coeff[i2];
       cxk[i]=coeff[i3];
       cye[i]=coeff[i4];
       cyk[i]=coeff[i5];
     }
     chi2l=0;
     nl=0;
     for (int i=0;i<nLambda;i++){
       //if (gEvflag[i]==100){
       EmomR[i]=recfcn(cpe,i,gE,6);
       KmomR[i]=recfcn(cpk,i,gK,6);
       ExptR[i]=recfcn(cxe,i,gE,6);
       KxptR[i]=recfcn(cxk,i,gK,6);
       EyptR[i]=recfcn(cye,i,gE,6);
       KyptR[i]=recfcn(cyk,i,gK,6);
       /*EmomR[i]-=gdmomE[i];
         ExptR[i]-=gdxptE[i];
         EyptR[i]-=gdyptE[i];
         KmomR[i]-=gdmomK[i];
         KxptR[i]-=gdxptK[i];
         KyptR[i]-=gdyptK[i];*/
       mmR[i][k]=calcMM(gB[i],EmomR[i],-ExptR[i],-EyptR[i],KmomR[i],KxptR[i],KyptR[i],1)+gdmm[i];
       h100->Fill(mmR[i][k]);
       if(gEvflag[i]==100){
	 //deltal=(mmR[i][k]-(ml-5e-4))/resMM1;
	 //deltal=(mmR[i][k]-(ml-0.0e-4))/resMM1;
	 deltal=( mmR[i][k] - ml )/resMM1;
	 chi2l+=deltal*deltal;
	 nl++;
       }
       /*if (k==0){
	 mm0[i]=mmR[i][k];
         }
         else{
	 dl[i][k]=fabs(mmR[i][k]-ml)-fabs(mmR[i][k-1]-ml);
	 h20->Fill(dl[i][k]);
         }
         if (fabs(mmR[i][k]-mm0[i])<.001){
	 h110->Fill(mmR[i][k]);
         }
         else{
	 h120->Fill(mmR[i][k]);
         }
         h10->Fill(mm0[i],mmR[i][k]-mm0[i]);
         h30->Fill(KmomR[i],mmR[i][k]-mm0[i]);
         h40->Fill(EmomR[i],mmR[i][k]-mm0[i]);*/
     }
     chi2s=0;
     ns=0;
     for (int i=0;i<nSigma;i++){
       int j=i+nLambda;
       //if (gEvflag[j]==101){
       EmomR[j]=recfcn(cpe,j,gE,6);
       KmomR[j]=recfcn(cpk,j,gK,6);
       ExptR[j]=recfcn(cxe,j,gE,6);
       KxptR[j]=recfcn(cxk,j,gK,6);
       EyptR[j]=recfcn(cye,j,gE,6);
       KyptR[j]=recfcn(cyk,j,gK,6);
       /*EmomR[j]-=gdmomE[j];
         ExptR[j]-=gdxptE[j];
         EyptR[j]-=gdyptE[j];
         KmomR[j]-=gdmomK[j];
         KxptR[j]-=gdxptK[j];
         KyptR[j]-=gdyptK[j];*/
       mmR[j][k]=calcMM(gB[j],EmomR[j],-ExptR[j],-EyptR[j],KmomR[j],KxptR[j],KyptR[j],1)+gdmm[j];
       h101->Fill(mmR[j][k]);
       if(gEvflag[j]==101){
	 //deltas=(mmR[j][k]-(ms0+.0e-4))/resMM1;
	 deltas = ( mmR[j][k] - ms0 ) / resMM1;
	 chi2s += deltas*deltas;
	 ns++;
       }
       /*if (k==0){
	 mm0[j]=mmR[j][k];
	 }
         else{
	 ds[j][k]=fabs(mmR[j][k]-ml)-fabs(mmR[j][k-1]-ml);
	 h21->Fill(ds[j][k]);
         }
         if (fabs(mmR[j][k]-mm0[i])<.001){
	 h111->Fill(mmR[j][k]);
         }
         else{
	 h121->Fill(mmR[j][k]);
         }
         h11->Fill(mm0[j],mmR[j][k]-mm0[j]);
         h31->Fill(KmomR[j],mmR[j][k]-mm0[j]);
         h41->Fill(EmomR[j],mmR[j][k]-mm0[j]);*/
     }
     chi2gsl=0.;
     ngsl=0;
     for (int i=0;i<nLB12gsl;i++){
       int j=i+nLambda+nSigma;
       EmomR[j]=recfcn(cpe,j,gE,6);
       KmomR[j]=recfcn(cpk,j,gK,6);
       ExptR[j]=recfcn(cxe,j,gE,6);
       KxptR[j]=recfcn(cxk,j,gK,6);
       EyptR[j]=recfcn(cye,j,gE,6);
       KyptR[j]=recfcn(cyk,j,gK,6);
       mmR[j][k]=calcMM(gB[j],EmomR[j],-ExptR[j],-EyptR[j],KmomR[j],KxptR[j],KyptR[j],12)-ml+gdmm[j];
       h1201->Fill(mmR[j][k]);
       if(gEvflag[j]==1201){
	 //deltagsl=(mmR[j][k]+gsmass)/resMM2;
	 //deltagsl = (mmR[j][k] - gsmm )  / resMM2;
	 deltagsl = ( mmR[j][k] - gsmm + ml )  / resMM2;
	 chi2gsl += deltagsl*deltagsl;
	 ngsl++;
       }
       /*if (k==0){
	 mm0[j]=mmR[j][k];
         }
         else{
	 dgsl[j][k]=fabs(mmR[j][k])-fabs(mmR[j][k-1]);
	 h22->Fill(dgsl[j][k]);
         }
         if (fabs(mmR[j][k]-mm0[i])<0.1){
	 h1211->Fill(mmR[j][k]);
         }
         else{
	 h1221->Fill(mmR[j][k]);
         }
         h12->Fill(mm0[j],mmR[j][k]-mm0[j]);
         h32->Fill(KmomR[j],mmR[j][k]-mm0[j]);
         h42->Fill(EmomR[j],mmR[j][k]-mm0[j]);*/
     }
     chi2gsh=0.;
     ngsh=0;
     for (int i=0;i<nLB12gsh;i++){
       int j=i+nLambda+nSigma+nLB12gsl;
       EmomR[j]=recfcn(cpe,j,gE,6);
       KmomR[j]=recfcn(cpk,j,gK,6);
       ExptR[j]=recfcn(cxe,j,gE,6);
       KxptR[j]=recfcn(cxk,j,gK,6);
       EyptR[j]=recfcn(cye,j,gE,6);
       KyptR[j]=recfcn(cyk,j,gK,6);
       mmR[j][k]=calcMM(gB[j],EmomR[j],-ExptR[j],-EyptR[j],KmomR[j],KxptR[j],KyptR[j],12)-ml+gdmm[j];
       h1202->Fill(mmR[j][k]);
       if(gEvflag[j]==1202){
	 deltagsh = ( mmR[j][k] - gsmm_high + ml ) / resMM2;
	 chi2gsh += deltagsh*deltagsh;
	 ngsh++;
       }
       /*if (k==0){
	 mm0[j]=mmR[j][k];
         }
         else{
	 gsh[j][k]=fabs(mmR[j][k])-fabs(mmR[j][k-1]);
	 h23->Fill(gsh[j][k]);
         }
         if (fabs(mmR[j][k]-mm0[i])<.001){
	 h111->Fill(mmR[j][k]);
	 }
	 else{
	 h121->Fill(mmR[j][k]);
	 }
         h13->Fill(mm0[j],mmR[j][k]-mm0[j]);
         h33->Fill(KmomR[j],mmR[j][k]-mm0[j]);
         h43->Fill(EmomR[j],mmR[j][k]-mm0[j]);*/
     }
     chi2h2o = 0.0;
     nh2o = 0;
     for (int i=0;i<nLambda_h2o;i++){
       int j=i+nLambda+nSigma+nLB12gsl+nLB12gsh;
       EmomR[j]=recfcn(cpe,j,gE,6);
       KmomR[j]=recfcn(cpk,j,gK,6);
       ExptR[j]=recfcn(cxe,j,gE,6);
       KxptR[j]=recfcn(cxk,j,gK,6);
       EyptR[j]=recfcn(cye,j,gE,6);
       KyptR[j]=recfcn(cyk,j,gK,6);
       mmR[j][k]=calcMM(gB[j],EmomR[j],-ExptR[j],-EyptR[j],KmomR[j],KxptR[j],KyptR[j],101) + gdmm[j];
       if(gEvflag[j]==110){
	 deltah2o = ( mmR[j][k] - ml ) / resMM1;
	 chi2h2o += deltah2o * deltah2o;
	 nh2o++;
       }
       h110->Fill(mmR[j][k]);
     }
     chi2psl=0;
     npsl=0;
     for (int i=0;i<nLB12psl;i++){
       int j=i+nLambda+nSigma+nLB12gsl+nLB12gsh+nLambda_h2o;
       EmomR[j]=recfcn(cpe,j,gE,6);
       KmomR[j]=recfcn(cpk,j,gK,6);
       ExptR[j]=recfcn(cxe,j,gE,6);
       KxptR[j]=recfcn(cxk,j,gK,6);
       EyptR[j]=recfcn(cye,j,gE,6);
       KyptR[j]=recfcn(cyk,j,gK,6);
       //mmR[j][k]=calcMM(gB[j],EmomR[j],-ExptR[j],-EyptR[j],KmomR[j],KxptR[j],KyptR[j],12)-ml+gdmm[i];
       mmR[j][k]=calcMM(gB[j],EmomR[j],-ExptR[j],-EyptR[j],KmomR[j],KxptR[j],KyptR[j],12)-ml+gdmm[j];
       if(gEvflag[j]==1203){
	 deltapsl = ( mmR[j][k] - psmm + ml ) / resMM2;
	 chi2psl += deltapsl*deltapsl;
	 npsl++;
       }
       h1203->Fill(mmR[j][k]);
     }
     chi2psh=0;
     npsh=0;
     for (int i=0;i<nLB12psh;i++){
       int j=i+nLambda+nSigma+nLB12gsl+nLB12gsh+nLambda_h2o+nLB12psl;
       EmomR[j]=recfcn(cpe,j,gE,6);
       KmomR[j]=recfcn(cpk,j,gK,6);
       ExptR[j]=recfcn(cxe,j,gE,6);
       KxptR[j]=recfcn(cxk,j,gK,6);
       EyptR[j]=recfcn(cye,j,gE,6);
       KyptR[j]=recfcn(cyk,j,gK,6);
       //mmR[j][k]=calcMM(gB[j],EmomR[j],-ExptR[j],-EyptR[j],KmomR[j],KxptR[j],KyptR[j],12)-ml+gdmm[i];
       mmR[j][k]=calcMM(gB[j],EmomR[j],-ExptR[j],-EyptR[j],KmomR[j],KxptR[j],KyptR[j],12)-ml+gdmm[j];
       h1204->Fill(mmR[j][k]);
       if(gEvflag[j]==1204){
	 deltapsh = ( mmR[j][k] - psmm_high + ml ) / resMM2;
	 chi2psh += deltapsh*deltapsh;
	 npsh++;
       }
     }
     chi2h2os=0;
     nh2os=0;
     for ( int i=0 ; i<nSigma_h2o ; i++ ){
       int j = i 
	 + nLambda + nSigma + nLB12gsl
	 + nLB12gsh + nLambda_h2o + nLB12psl
	 + nLB12psh;
       EmomR[j]=recfcn(cpe,j,gE,6);
       KmomR[j]=recfcn(cpk,j,gK,6);
       ExptR[j]=recfcn(cxe,j,gE,6);
       KxptR[j]=recfcn(cxk,j,gK,6);
       EyptR[j]=recfcn(cye,j,gE,6);
       KyptR[j]=recfcn(cyk,j,gK,6);
       //mmR[j][k]=calcMM(gB[j],EmomR[j],-ExptR[j],-EyptR[j],KmomR[j],KxptR[j],KyptR[j],101) + gdmm[i];
       mmR[j][k]=calcMM(gB[j],EmomR[j],-ExptR[j],-EyptR[j],KmomR[j],KxptR[j],KyptR[j],101) + gdmm[j];
       h111->Fill(mmR[j][k]);
       if( gEvflag[j]==111 ){
	 deltah2os = ( mmR[j][k] - ms0 ) / resMM1;
	 chi2h2os += deltah2os*deltah2os;
	 nh2os++;
       }
     }
     chi2gsch2=0;
     ngsch2=0;
     for ( int i=0 ; i<nLB12gsch2 ; i++ ){
       int j = i 
	 + nLambda + nSigma + nLB12gsl
	 + nLB12gsh + nLambda_h2o + nLB12psl
	 + nLB12psh + nSigma_h2o;
       EmomR[j]=recfcn(cpe,j,gE,6);
       KmomR[j]=recfcn(cpk,j,gK,6);
       ExptR[j]=recfcn(cxe,j,gE,6);
       KxptR[j]=recfcn(cxk,j,gK,6);
       EyptR[j]=recfcn(cye,j,gE,6);
       KyptR[j]=recfcn(cyk,j,gK,6);
       //mmR[j][k]=calcMM(gB[j],EmomR[j],-ExptR[j],-EyptR[j],KmomR[j],KxptR[j],KyptR[j],112) -ml + gdmm[i];
       mmR[j][k]=calcMM(gB[j],EmomR[j],-ExptR[j],-EyptR[j],KmomR[j],KxptR[j],KyptR[j],112) -ml + gdmm[j];
       h1205->Fill(mmR[j][k]);
       if( gEvflag[j]==1205 ){
	 deltagsch2 = ( mmR[j][k] - gsmm_ch2 + ml ) / resMM2;
	 chi2gsch2 += deltagsch2*deltagsch2;
	 ngsch2++;
       }
     }
     chi2gsb10=0;
     ngsb10=0;
     for ( int i=0 ; i<nLBe10gs ; i++ ){
       int j = i 
	 + nLambda + nSigma + nLB12gsl
	 + nLB12gsh + nLambda_h2o + nLB12psl
	 + nLB12psh + nSigma_h2o + nLB12gsch2;
       EmomR[j]=recfcn(cpe,j,gE,6);
       KmomR[j]=recfcn(cpk,j,gK,6);
       ExptR[j]=recfcn(cxe,j,gE,6);
       KxptR[j]=recfcn(cxk,j,gK,6);
       EyptR[j]=recfcn(cye,j,gE,6);
       KyptR[j]=recfcn(cyk,j,gK,6);
       //mmR[j][k]=calcMM(gB[j],EmomR[j],-ExptR[j],-EyptR[j],KmomR[j],KxptR[j],KyptR[j],10) -ml + gdmm[i];
       mmR[j][k]=calcMM(gB[j],EmomR[j],-ExptR[j],-EyptR[j],KmomR[j],KxptR[j],KyptR[j],10) -ml + gdmm[j];
       h1000->Fill(mmR[j][k]);
       if( gEvflag[j]==1000 ){
	 deltagsb10 = ( mmR[j][k] - gsmm_b10 + ml ) / resMM2;
	 chi2gsb10 += deltagsb10*deltagsb10;
	 ngsb10++;
       }
     }
     chi2gsli7=0;
     ngsli7=0;
     for ( int i=0 ; i<nLHe7gs ; i++ ){
       int j = i 
	 + nLambda + nSigma + nLB12gsl
	 + nLB12gsh + nLambda_h2o + nLB12psl
	 + nLB12psh + nSigma_h2o + nLB12gsch2
	 + nLBe10gs;
       EmomR[j]=recfcn(cpe,j,gE,6);
       KmomR[j]=recfcn(cpk,j,gK,6);
       ExptR[j]=recfcn(cxe,j,gE,6);
       KxptR[j]=recfcn(cxk,j,gK,6);
       EyptR[j]=recfcn(cye,j,gE,6);
       KyptR[j]=recfcn(cyk,j,gK,6);
       //mmR[j][k]=calcMM(gB[j],EmomR[j],-ExptR[j],-EyptR[j],KmomR[j],KxptR[j],KyptR[j],10) -ml + gdmm[i];
       mmR[j][k]=calcMM(gB[j],EmomR[j],-ExptR[j],-EyptR[j],KmomR[j],KxptR[j],KyptR[j],7) -ml + gdmm[j];
       h700->Fill(mmR[j][k]);
       if( gEvflag[j]==700 ){
	 deltagsli7 = ( mmR[j][k] - gsmm_li7 + ml ) / resMM2;
	 chi2gsli7 += deltagsli7*deltagsli7;
	 ngsli7++;
       }
     }
     chi2cel=0;
     ncel=0;
     for ( int i=0 ; i<nLB12cel ; i++ ){
       int j = i 
	 + nLambda + nSigma + nLB12gsl
	 + nLB12gsh + nLambda_h2o + nLB12psl
	 + nLB12psh + nSigma_h2o + nLB12gsch2
	 + nLBe10gs + nLHe7gs;
       EmomR[j]=recfcn(cpe,j,gE,6);
       KmomR[j]=recfcn(cpk,j,gK,6);
       ExptR[j]=recfcn(cxe,j,gE,6);
       KxptR[j]=recfcn(cxk,j,gK,6);
       EyptR[j]=recfcn(cye,j,gE,6);
       KyptR[j]=recfcn(cyk,j,gK,6);
       //mmR[j][k]=calcMM(gB[j],EmomR[j],-ExptR[j],-EyptR[j],KmomR[j],KxptR[j],KyptR[j],10) -ml + gdmm[i];
       mmR[j][k]=calcMM(gB[j],EmomR[j],-ExptR[j],-EyptR[j],KmomR[j],KxptR[j],KyptR[j],12) -ml + gdmm[j];
       h1290->Fill(mmR[j][k]);
       if( gEvflag[j]==1290 ){
	 deltacel = ( mmR[j][k] - cemm + ml ) / resMM2;
	 chi2cel += deltacel*deltacel;
	 ncel++;
       }
     }
     chi2ceh=0;
     nceh=0;
     for ( int i=0 ; i<nLB12ceh ; i++ ){
       int j = i 
	 + nLambda + nSigma + nLB12gsl
	 + nLB12gsh + nLambda_h2o + nLB12psl
	 + nLB12psh + nSigma_h2o + nLB12gsch2
	 + nLBe10gs + nLHe7gs + nLB12cel;
       EmomR[j]=recfcn(cpe,j,gE,6);
       KmomR[j]=recfcn(cpk,j,gK,6);
       ExptR[j]=recfcn(cxe,j,gE,6);
       KxptR[j]=recfcn(cxk,j,gK,6);
       EyptR[j]=recfcn(cye,j,gE,6);
       KyptR[j]=recfcn(cyk,j,gK,6);
       //mmR[j][k]=calcMM(gB[j],EmomR[j],-ExptR[j],-EyptR[j],KmomR[j],KxptR[j],KyptR[j],10) -ml + gdmm[i];
       mmR[j][k]=calcMM(gB[j],EmomR[j],-ExptR[j],-EyptR[j],KmomR[j],KxptR[j],KyptR[j],12) -ml + gdmm[j];
       h1291->Fill(mmR[j][k]);
       if( gEvflag[j]==1291 ){
	 deltaceh = ( mmR[j][k] - cemm_high + ml ) / resMM2;
	 chi2ceh += deltaceh*deltaceh;
	 nceh++;
       }
     }
     
     chi2cr52_1 = 0.0;
     ncr52_1    = 0;
     for ( int i=0 ; i<nLV52_1 ; i++ ){
       int j = i 
	 + nLambda + nSigma + nLB12gsl
	 + nLB12gsh + nLambda_h2o + nLB12psl
	 + nLB12psh + nSigma_h2o + nLB12gsch2
	 + nLBe10gs + nLHe7gs + nLB12cel + nLB12ceh;
       EmomR[j]=recfcn(cpe,j,gE,6);
       KmomR[j]=recfcn(cpk,j,gK,6);
       ExptR[j]=recfcn(cxe,j,gE,6);
       KxptR[j]=recfcn(cxk,j,gK,6);
       EyptR[j]=recfcn(cye,j,gE,6);
       KyptR[j]=recfcn(cyk,j,gK,6);
       //mmR[j][k]=calcMM(gB[j],EmomR[j],-ExptR[j],-EyptR[j],KmomR[j],KxptR[j],KyptR[j],10) -ml + gdmm[i];
       mmR[j][k]=calcMM(gB[j],EmomR[j],-ExptR[j],-EyptR[j],KmomR[j],KxptR[j],KyptR[j],52) -ml + gdmm[j];
       h5200->Fill(mmR[j][k]);
       if( gEvflag[j]==5200 ){
	 deltacr52_1 = ( mmR[j][k] - mm1_cr52 + ml ) / resMM2;
	 chi2cr52_1 += deltacr52_1*deltacr52_1;
	 ncr52_1++;
       }
     }

     
     logfile << "chi2l="    << chi2l/nl        << " ..." << nl   <<endl;
     logfile << "chi2s="    << chi2s/ns        << " ..." << ns   <<endl;
     logfile << "chi2l_h2o="<< chi2h2o/nh2o    << " ..." << nh2o <<endl;
     logfile << "chi2s_h2o="<< chi2h2os/nh2os  << " ..." << nh2os<<endl;
     logfile << "chi2gsl="  << chi2gsl/ngsl    << " ..." << ngsl <<endl;
     logfile << "chi2gsh="  << chi2gsh/ngsh    << " ..." << ngsh <<endl;
     logfile << "chi2psl="  << chi2psl/npsl    << " ..." << npsl <<endl;
     logfile << "chi2psh="  << chi2psh/npsh    << " ..." << npsh <<endl;
     logfile << "chi2gsch2="<< chi2gsch2/ngsch2<< " ..." << ngsch2<<endl;
     logfile << "chi2gsb10="<< chi2gsb10/ngsb10<< " ..." << ngsb10<<endl;
     logfile << "chi2gsli7="<< chi2gsli7/ngsli7<< " ..." << ngsli7<<endl;
     logfile << "chi2cel="  << chi2cel/ncel    << " ..." << ncel  <<endl;
     logfile << "chi2ceh="  << chi2ceh/nceh    << " ..." << nceh  <<endl;
     logfile << "chi2cr52_1="<< chi2cr52_1/ncr52_1<< " ..." << ncr52_1 <<endl;
     
     //f100->SetParameters(100,1.115,1e-3,0.,1.115,10e-3); 2g
     f100->SetParameters(100,1.115683,7.5e-4,0.,30.);
     //f100->SetParLimits(0,0.,500.);
     //f100->SetParLimits(1,1.110,1.120);
     f100->SetParLimits(1,1.115383,1.115983);
     f100->SetParLimits(2,4.5e-4,8.5e-4);
     //f100->SetParLimits(3,20,60);
     //f101->SetParameters(100,1.192,1e-3,0.,1.192,10e-3); 2g
     f101->SetParameters(100,1.192642,8.2e-4,0.,15.);
     //f101->SetParLimits(0,0.,100.);
     f101->SetParLimits(1,1.192342,1.192942);
     f101->SetParLimits(2,4.5e-4,8.5e-4);
     //f101->SetParLimits(3,10,20);
     f1201->SetParameters(10,-0.0114,3.0e-4,5.);
     //f1201->SetParLimits(0,0.,20.);
     //f1201->SetParLimits(1,-0.01142,-0.01139);
     //f1201->SetParLimits(1,-0.01152,-0.01139);//16Dec2013
     f1201->SetParLimits(1,-0.01147,-0.01135);//16Dec2013
     f1201->SetParLimits(2,2.0e-4,3.8e-4);
     //f1201->SetParLimits(2,2.5e-4,6.0e-4);
     //f1201->SetParLimits(3,2.,8.);
     //f1202->SetParameters(10,-0.0114,5.0e-4,25.);
     f1202->SetParameters(10,-0.0114,3.5e-4,25.);
     //f1202->SetParLimits(0,5.,25.);
     //f1202->SetParLimits(1,-0.01142,-0.01139);
     f1202->SetParLimits(1,-0.01148,-0.01135);
     //f1202->SetParLimits(2,2.5e-4,6.0e-4);
     f1202->SetParLimits(2,2.0e-4,4.0e-4);
     //f1202->SetParLimits(3,20.,30.);
     f110->SetParameters(50,1.115683,7.5e-4,0.,5.);
     //f110->SetParLimits(0,0.,500.);
     f110->SetParLimits(1,1.114683,1.116683);
     f110->SetParLimits(2,5.0e-4,8.5e-4);
     //f110->SetParLimits(3,10,20);
     f111->SetParameters(100,1.192642,7.5e-4,0.,15.);
     //f111->SetParLimits(0,0.,100.);
     f111->SetParLimits(1,1.191642,1.193642);
     f111->SetParLimits(2,5.0e-4,8.5e-4);
     //f111->SetParLimits(3,10,20);
     f1203->SetParameters(10,-0.0004,5.0e-4,8.);
     //f1203->SetParLimits(0,0.,20.);
     f1203->SetParLimits(1,-0.00042,-0.00038);
     f1203->SetParLimits(2,2.2e-4,6.0e-4);
     //f1203->SetParLimits(3,5.,10.);
     f1204->SetParameters(10,-0.0004,5.0e-4,35.);
     //f1204->SetParLimits(0,0.,50.);
     f1204->SetParLimits(1,-0.00042,-0.00038);
     f1204->SetParLimits(2,2.2e-4,6.0e-4);
     //f1204->SetParLimits(3,30.,40.);
     /*f1202->SetParameters(10,-0.011,5e-4,1e5,-0.011,10.);
       f1202->SetParLimits(0,5.,10.);
       f1202->SetParLimits(1,-0.012,-0.010);
       f1202->SetParLimits(2,1e-4,2e-3);
       f1202->SetParLimits(3,-1e8,0.);
       f1202->SetParLimits(4,-0.012,-0.010);
       f1202->SetParLimits(5,0.,10.);*/
     /*f1202->SetParameters(10,-0.011,5e-4,10,-0.011,2e-3);
       f1202->SetParLimits(0,0.,20.);
       f1202->SetParLimits(1,-0.015,-0.010);
       f1202->SetParLimits(2,1e-4,1e-3);
       f1202->SetParLimits(3,0.,10.);
       f1202->SetParLimits(4,-0.015,-0.010);
       f1202->SetParLimits(5,1e-3,3e-3);*/
     f1205->SetParameters(10,-0.0114,9.0e-3,5.);
     //f1201->SetParLimits(0,0.,20.);
     f1205->SetParLimits(1,-0.0115,-0.0113);
     f1205->SetParLimits(2,2.5e-4,10.0e-4);
     
     // ~~~~~~ 10LBe g.s. ~~~~~~~~~~~~~~~~~~~~~~~~~
     //f1000->SetParameters(10,-8.8e-3,3.8e-3,5.);// for 10LBe g.s.
     f1000->SetParameters(10,-8.5e-3,3.5e-4,5.);// for 10LBe g.s.
     //f1000->SetParLimits(1,-9.1e-3,-8.1e-3);
     //f1000->SetParLimits(1,-8.9e-3,-8.4e-3);    
     f1000->SetParLimits(1,-8.8e-3,-8.48e-3); // for 10LBe g.s.
     //f1000->SetParameters(10,2.437e-3,7.3e-4,5.);// for 10LBe excited state
     //f1000->SetParameters(10,2.2e-3,7.3e-4,5.);// for 10LBe excited state
     //f1000->SetParLimits(1,2.2867e-3,2.5867e-3); // for 10LBe excited state
     //f1000->SetParLimits(1,2.1e-3,2.3e-3); // for 10LBe excited state
     //f1000->SetParLimits(1,2.1e-3,2.3e-3); // for 10LBe excited state
     f1000->SetParLimits(2,2.8e-4,3.7e-4);// 
     
     // ~~~~~~ 7LHe g.s. ~~~~~~~~~~~~~~~~~~~~~~~~~
     f700->SetParameters(10,-5.4e-3,5.8e-4,5.);
     //f700->SetParLimits(1,-5.6e-3,-5.35e-3);
     f700->SetParLimits(1,-5.6e-3,-5.35e-3); // 13Jan2014, Toshi
     //f700->SetParLimits(1,-5.5e-3,-5.35e-3);
     //f700->SetParLimits(1,-5.5e-3,-5.15e-3);
     f700->SetParLimits(2,4.0e-4,6.0e-4);
     
     // ~~~~~~ 12LB core excited 1 (low) ~~~~~~~~~~~~~~~~~~~~~~~~~
     f1290->SetParameters(10,-8.55e-3,6.0e-4,5.);
     //f700->SetParLimits(1,-5.5e-3,-5.35e-3);
     f1290->SetParLimits(1,-8.65e-3,-8.45e-3);
     f1290->SetParLimits(2,2.5e-4,8.5e-4);
     
     // ~~~~~~ 12LB core excited 1 (high) ~~~~~~~~~~~~~~~~~~~~~~~~~
     f1291->SetParameters(10,-8.55e-3,6.0e-4,5.);
     //f700->SetParLimits(1,-5.5e-3,-5.35e-3);
     f1291->SetParLimits(1,-8.65e-3,-8.45e-3);
     f1291->SetParLimits(2,2.5e-4,8.5e-4);
     
     // ~~~~~~ 52LV  ~~~~~~~~~~~~~~~~~~~~~~~~~
     //f5200->SetParameters(10,-16.0e-3,6.0e-4,5.);
     f5200->SetParameters(10,-15.0e-3,6.0e-4,5.);
     //f5200->SetParameters(10,0.5e-3,6.0e-4,5.);
     //f5200->SetParameters(10,3.0e-3,6.0e-4,5.);
     //f5200->SetParLimits(1,-16.3e-3,-15.7e-3);
     //f5200->SetParLimits(1,0.2e-3,0.8e-3);
     //f5200->SetParLimits(1,2.7e-3,3.3e-3);
     f5200->SetParLimits(1,-15.2e-3,-14.8e-3);     
     f5200->SetParLimits(2,3.3e-4,7.0e-4);
     
     int maxbin=h1202->GetMaximumBin();
     double bincent = h1202->GetBinCenter(maxbin);
     
     //h100->Fit("f100","0q");
     //h101->Fit("f101","0q");
     ////h1201->Fit("f1201","0");
     ////h1201->Fit("f1201","0","",-0.0125,-0.0095);
     //h1201->Fit("f1201","0q","",-0.014,-0.010);
     //h1202->Fit("f1202","0q","",-0.014,-0.010);
     //h1203->Fit("f1203","0q","",-0.003,0.001);
     //h1204->Fit("f1204","0q","",-0.003,0.001);
     //h1205->Fit("f1205","0q","",-0.014,-0.010);
     
     cout << " Fitting..." << endl;
     cout << "   (lambda)" << endl;
     sprintf(tempc,"f100_%d",k);
     h100->Fit(tempc,"0qB","",1.114,1.118);
     cout << "   (Sigma0)" << endl;
     sprintf(tempc,"f101_%d",k);
     cout << "   (Sigma0)" << endl;
     h101->Fit(tempc,"0qB","",1.191,1.194);
     cout << "   (lambda H2O)" << endl;
     sprintf(tempc,"f110_%d",k);
     h110->Fit(tempc,"0qB","",1.114,1.117);
     cout << "   (Sigma H2O)" << endl;
     sprintf(tempc,"f111_%d",k);
     h111->Fit(tempc,"0qB","",1.191,1.194);
     cout << "   (LB12gsl)" << endl;
     sprintf(tempc,"f1201_%d",k);
     //h1201->Fit(tempc,"0qB","",-0.014,-0.010);
     //h1201->Fit(tempc,"0qB","",-0.012,-0.0105); // Toshi , 6Nov2013
     h1201->Fit(tempc,"0qB","",-0.012,-0.011); // Toshi , 6Nov2013
     cout << "   (LB12gsh)" << endl;
     sprintf(tempc,"f1202_%d",k);
     //h1202->Fit(tempc,"0qB","",-0.014,-0.010);
     //h1202->Fit(tempc,"0qB","",-0.012,-0.0105); // Toshi , 6Nov2013
     h1202->Fit(tempc,"0qB","",-0.0125,-0.0105); // Toshi , 6Nov2013
     cout << "   (LB12pl)" << endl;
     sprintf(tempc,"f1203_%d",k);
     //h1203->Fit(tempc,"0qB","",-0.003,0.001);
     h1203->Fit(tempc,"0qB","",-0.001,0.0005); // Toshi , 6Nov2013
     cout << "   (LB12ph)" << endl;
     sprintf(tempc,"f1204_%d",k);
     //h1204->Fit(tempc,"0qB","",-0.003,0.001);
     h1204->Fit(tempc,"0qB","",-0.001,0.0005); // Toshi , 6Nov2013
     cout << "   (LB12gs CH2)" << endl;
     sprintf(tempc,"f1205_%d",k);
     h1205->Fit(tempc,"0qB","",-0.014,-0.010);
     cout << "   (LBe10gs)" << endl;
     sprintf(tempc,"f1000_%d",k);
     h1000->Fit(tempc,"0qB","",-9.13e-3,-8.35e-3);
     //h1000->Fit(tempc,"0qB","",1.0e-3,2.0e-3); // excited state , Toshi , 6Nov2013
     cout << "   (LHe7gs)" << endl;
     sprintf(tempc,"f700_%d",k);
     //h700->Fit(tempc,"0qB","",-5.5e-3,-4.5e-3); // Toshi , 6Nov2013
     h700->Fit(tempc,"0qB","",-6.5e-3,-4.5e-3); // Toshi , 23Dec2013
     cout << "   (LB12 ce)" << endl;
     sprintf(tempc,"f1290_%d",k);
     h1290->Fit(tempc,"0qB","",-9.55e-3,-7.55e-3); // Toshi , 6Nov2013
     sprintf(tempc,"f1291_%d",k);
     h1291->Fit(tempc,"0qB","",-9.55e-3,-7.55e-3); // Toshi , 6Nov2013
     sprintf(tempc,"f5200_%d",k);
     //h5200->Fit(tempc,"0qB","",-17.5e-3,-14.5e-3); // Toshi , 10Jan2014
     h5200->Fit(tempc,"0qB","",1.0e-3,5.0e-3); // Toshi , 10Jan2014
     
     
     h100->Write(histname0);
     h101->Write(histname1);
     h1201->Write(histname2);
     h1202->Write(histname3);
     h110->Write(histname4);
     h111->Write(histname7);
     h1203->Write(histname5);
     h1204->Write(histname6);
     h1205->Write(histname8);
     h1000->Write();
     h700->Write();
     h5200->Write();
     f100->Write(funcname0);
     f101->Write(funcname1);
     f1201->Write(funcname2);
     f1202->Write(funcname3);
     f110->Write(funcname4);
     f111->Write(funcname7);
     f1203->Write(funcname5);
     f1204->Write(funcname6);
     f1205->Write(funcname8);
     f1000->Write();
     f700->Write();
     f1290->Write();
     f1291->Write();
     f5200->Write();

     /*h110->Write(histname50);
       h111->Write(histname51);
       h1211->Write(histname52);
       h120->Write(histname60);
       h121->Write(histname61);
       h1221->Write(histname62);*/
     /*h10->Write(histname10);
       h11->Write(histname11);
       h12->Write(histname12);
       h13->Write(histname13);
       h20->Write(histname20);
       h21->Write(histname21);
       h22->Write(histname22);
       h23->Write(histname23);
       h30->Write(histname30);
       h31->Write(histname31);
       h32->Write(histname32);
       h33->Write(histname33);
       h40->Write(histname40);
       h41->Write(histname41);
       h42->Write(histname42);
       h43->Write(histname43);*/
     float a100=f100->GetParameter(0);
     float mu100=f100->GetParameter(1);
     float sigma100=f100->GetParameter(2);
     float chi100=f100->GetChisquare();
     float a101=f101->GetParameter(0);
     float mu101=f101->GetParameter(1);
     float sigma101=f101->GetParameter(2);
     float chi101=f101->GetChisquare();
     float a1201=f1201->GetParameter(0);
     float mu1201=f1201->GetParameter(1);
     float sigma1201=f1201->GetParameter(2);
     float chi1201=f1201->GetChisquare();
     float a1202=f1202->GetParameter(0);
     float mu1202=f1202->GetParameter(1);
     float sigma1202=f1202->GetParameter(2);
     float chi1202=f1202->GetChisquare();
     float a110=f110->GetParameter(0);
     float mu110=f110->GetParameter(1);
     float sigma110=f110->GetParameter(2);
     float chi110=f110->GetChisquare();
     float a111=f111->GetParameter(0);
     float mu111=f111->GetParameter(1);
     float sigma111=f111->GetParameter(2);
     float chi111=f111->GetChisquare();
     float a1203=f1203->GetParameter(0);
     float mu1203=f1203->GetParameter(1);
     float sigma1203=f1203->GetParameter(2);
     float chi1203=f1203->GetChisquare();
     float a1204    =f1204->GetParameter(0);
     float mu1204   =f1204->GetParameter(1);
     float sigma1204=f1204->GetParameter(2);
     float chi1204  =f1204->GetChisquare();
     float a1205    =f1205->GetParameter(0);
     float mu1205   =f1205->GetParameter(1);
     float sigma1205=f1205->GetParameter(2);
     float chi1205  =f1205->GetChisquare();
     float a1000    =f1000->GetParameter(0);
     float mu1000   =f1000->GetParameter(1);
     float sigma1000=f1000->GetParameter(2);
     float chi1000  =f1000->GetChisquare();
     float a700    =f700->GetParameter(0);
     float mu700   =f700->GetParameter(1);
     float sigma700=f700->GetParameter(2);
     float chi700  =f700->GetChisquare();
     Float_t a1290    =f1290->GetParameter(0);
     Float_t mu1290   =f1290->GetParameter(1);
     Float_t sigma1290=f1290->GetParameter(2);
     Float_t chi1290  =f1290->GetChisquare();
     Float_t a1291    =f1291->GetParameter(0);
     Float_t mu1291   =f1291->GetParameter(1);
     Float_t sigma1291=f1291->GetParameter(2);
     Float_t chi1291  =f1291->GetChisquare();
     Float_t a5200    =f5200->GetParameter(0);
     Float_t mu5200   =f5200->GetParameter(1);
     Float_t sigma5200=f5200->GetParameter(2);
     Float_t chi5200  =f5200->GetChisquare();
     //mu1201-=(ml-mu100);
     //mu1202-=(ml-mu100);
     //resMM1=min(fabs(sigma100),fabs(sigma101));
     //gsmass=(gsmass*(k+1)-mu1201)/(k+2.);
     //psmass=(psmass*(k+1)-mu1203)/(k+2.);
     /*cutL1=mu100-1.5*fabs(sigma100);
       cutL2=mu100+1.5*fabs(sigma100);
       cutS1=mu101-2.0*fabs(sigma101);
       cutS2=mu101+2.0*fabs(sigma101);
       cutgsl1=mu1201-3.0*fabs(sigma1201);
       cutgsl2=mu1201+3.0*fabs(sigma1201);
       cutgsh1=mu1202-3.0*fabs(sigma1202);
       cutgsh2=mu1202+3.0*fabs(sigma1202);
       cutLh2o1=mu110-1.5*fabs(sigma110);
       cutLh2o2=mu110+1.5*fabs(sigma110);*/
     cutL1=mu100-2.0*fabs(sigma100);
     cutL2=mu100+2.0*fabs(sigma100);
     cutS1=mu101-2.0*fabs(sigma101);
     cutS2=mu101+2.0*fabs(sigma101);
     cutgsl1=mu1201-2.0*fabs(sigma1201);
     cutgsl2=mu1201+2.0*fabs(sigma1201);
     cutgsh1=mu1202-2.0*fabs(sigma1202);
     cutgsh2=mu1202+2.0*fabs(sigma1202);
     cutLh2o1=mu110-2.0*fabs(sigma110);
     cutLh2o2=mu110+2.0*fabs(sigma110);
     cutSh2o1=mu111-2.0*fabs(sigma111);
     cutSh2o2=mu111+2.0*fabs(sigma111);
     cutgsch2_1 = mu1205-2.0*fabs(sigma1205);
     cutgsch2_2 = mu1205+2.0*fabs(sigma1205);
     cutpsl1=mu1203-10e-4;
     cutpsl2=mu1203+10e-4;
     cutpsh1=mu1204-10e-4;
     cutpsh2=mu1204+10e-4;
     //cutgsb10_1 = mu1000-2.0*fabs(sigma1000);
     //cutgsb10_2 = mu1000+1.8*fabs(sigma1000);
     //cutgsb10_1 = mu1000-0.7e+3;
     //cutgsb10_2 = mu1000+0.7e+3;
     cutgsb10_1 = mu1000 - 2.0*fabs(sigma1000);
     cutgsb10_2 = mu1000 + 2.0*fabs(sigma1000);
     //cutgsli7_1 = mu700-0.7e+3;
     //cutgsli7_2 = mu700+0.7e+3;
     cutgsli7_1 = mu700 - 2.0*fabs(sigma700);
     cutgsli7_2 = mu700 - 2.0*fabs(sigma700);
     cutcel_1 = mu1290 - 2.0*fabs(sigma1290);
     cutcel_2 = mu1290 + 2.0*fabs(sigma1290);
     cutceh_1 = mu1291 - 2.0*fabs(sigma1291);
     cutceh_2 = mu1291 + 2.0*fabs(sigma1291);
     cut1cr52_1 = mu5200 - 2.0*fabs(sigma5200);
     cut2cr52_1 = mu5200 + 2.0*fabs(sigma5200);
     
     logfile << k <<" gsmass_mean " << gsmass <<endl;
     logfile << k <<" psmass_mean " << psmass <<endl;
     logfile << k <<" Lambda:(mu,sigma,chi2) " 
	     << mu100    << " " 
	     << sigma100 << " "
	     << chi100   << " "
	     << "[" << cutL1 << "," << cutL2 << "]" << endl;
     logfile << k << " nL "<<a100*fabs(sigma100)*sqrt(2*3.14)*100/(cut1[1]-cut1[0]+4e-3)<<endl;
     logfile << k <<" Sigma:(mu,sigma,chi2) " 
	     <<   mu101   << " " 
	     <<  sigma101 << " "
	     <<  chi101   << " "
	     << "["<<cutS1<<","<<cutS2<<"]"<<endl;
     logfile << k << " nS "<<a101*fabs(sigma101)*sqrt(2*3.14)*100/(cut2[1]-cut2[0]+4e-3)<<endl;
     logfile << k <<" 12LBgsl:(mu,sigma,chi2) " << mu1201 << " " 
	     <<  sigma1201 <<" "
	     <<  chi1201 <<endl;
     logfile << k << " ngsl "<<a1201*fabs(sigma1201)*sqrt(2*3.14)*100/(cut3[1]-cut3[0]+4e-3)<<endl;
     logfile << k <<" 12LBgsh:(mu,sigma,chi2) " << mu1202 << " " 
	     <<  sigma1202 <<" "
	     <<  chi1202 <<endl;
     logfile << k << " ngsh "<<a1202*fabs(sigma1202)*sqrt(2*3.14)*100/(cut4[1]-cut4[0]+4e-3)<<endl;
     logfile << k <<" Lh2o:(mu,sigma,chi2) " << mu110 << " " 
	     << sigma110 <<" "
	     << chi110 <<" "
	     << "["<<cutLh2o1<<","<<cutLh2o2<<"]"<<endl;
     logfile << k <<" Sh2o:(mu,sigma,chi2) " << mu111 << " " 
	     << sigma111 <<" "
	     << chi111 <<" "
	     << "["<<cutSh2o1<<","<<cutSh2o2<<"]"<<endl;
     logfile << k <<" 12LBpsl:(mu,sigma,chi2) " << mu1203 << " " 
	     <<  sigma1203 <<" "
	     <<  chi1203 <<endl;
     logfile << k <<" 12LBpsh:(mu,sigma,chi2) " << mu1204 << " " 
	     <<  sigma1204 <<" "
	     <<  chi1204 <<endl;
     logfile << k <<" 12LBgsch2:(mu,sigma,chi2) " << mu1205 << " " 
	     <<  sigma1205 <<" "
	     <<  chi1205 <<endl;
     logfile << k <<" 10LBegsb10:(mu,sigma,chi2) " << mu1000 << " " 
	     <<  sigma1000 <<" "
	     <<  chi1000 <<endl;
     logfile << k <<" 7LHegsli7:(mu,sigma,chi2) " << mu700 << " " 
	     <<  sigma700 <<" "
	     <<  chi700 <<endl;
     logfile << k <<" 12LBcel:(mu,sigma,chi2) " << mu1290 << " " 
	     <<  sigma1290 <<" "
	     <<  chi1290 <<endl;
     logfile << k <<" 12LBceh:(mu,sigma,chi2) " << mu1291 << " " 
	     <<  sigma1291 <<" "
	     <<  chi1291 <<endl;
     logfile << k <<" 52LV_1:(mu,sigma,chi2) " << mu5200 << " " 
	     <<  sigma5200 <<" "
	     <<  chi5200 <<endl;
     
     for (int i=0;i<nAllpeak;i++){
       if (gEvflag[i]==100){
	 if (mmR[i][k]>cutL1 && mmR[i][k]<cutL2){
	   gEvflag[i]=100;
	 }
	 else {gEvflag[i]=0;}
       }
       else if (gEvflag[i]==101){
	 if (mmR[i][k]>cutS1 && mmR[i][k]<cutS2){
	   gEvflag[i]=101;
	 }
	 else {gEvflag[i]=0;}
       }
       else if (gEvflag[i]==1201){
	 if (mmR[i][k]>cutgsl1 && mmR[i][k]<cutgsl2){
	   gEvflag[i]=1201;
	 }
	 else {gEvflag[i]=0;}
       }
       else if (gEvflag[i]==1202){
	 if (mmR[i][k]>cutgsh1 && mmR[i][k]<cutgsh2){
	   gEvflag[i]=1202;
	 }
	 else {gEvflag[i]=0;}
       }
       else if (gEvflag[i]==110){
	 if (mmR[i][k]>cutLh2o1 && mmR[i][k]<cutLh2o2){
	   gEvflag[i]=110;
	 }
	 else {gEvflag[i]=0;}
       }
       else if (gEvflag[i]==1203){
	 if (mmR[i][k]>cutpsl1 && mmR[i][k]<cutpsl2){
	   gEvflag[i]=1203;
	 }
	 else {gEvflag[i]=0;}
       }
       else if (gEvflag[i]==1204){
	 if (mmR[i][k]>cutpsh1 && mmR[i][k]<cutpsh2){
	   gEvflag[i]=1204;
	 }
	 else {gEvflag[i]=0;}
       }
       else if (gEvflag[i]==111){
	 if (mmR[i][k]>cutSh2o1 && mmR[i][k]<cutSh2o2){
	   gEvflag[i]=111;
	 }
	 else {gEvflag[i]=0;}
       }
       else if (gEvflag[i]==1205){
	 if (mmR[i][k]>cutgsch2_1 && mmR[i][k]<cutgsch2_2){
	   gEvflag[i]=1205;
	 }
	 else {gEvflag[i]=0;}
       }
       else if (gEvflag[i]==1000){
	 if (mmR[i][k]>cutgsb10_1 && mmR[i][k]<cutgsb10_2){
	   gEvflag[i]=1000;
	 }
	 else {gEvflag[i]=0;}
       }
       else if (gEvflag[i]==700){
	 if (mmR[i][k]>cutgsli7_1 && mmR[i][k]<cutgsli7_2){
	   gEvflag[i]=700;
	 }
	 else {gEvflag[i]=0;}
       }
       else if (gEvflag[i]==1290){
	 if ( mmR[i][k]>cutcel_1 && mmR[i][k]<cutcel_2 ){
	   gEvflag[i]=1290;
	 }
	 else {gEvflag[i]=0;}
       }
       else if (gEvflag[i]==1291){
	 if ( mmR[i][k]>cutceh_1 && mmR[i][k]<cutceh_2 ){
	   gEvflag[i]=1291;
	 }
	 else {gEvflag[i]=0;}
       }
       else if (gEvflag[i]==5200){
	 if ( mmR[i][k]>cut1cr52_1 && mmR[i][k]<cut2cr52_1 ){
	   gEvflag[i]=5200;
	 }
	 else {gEvflag[i]=0;}
       }
     }
     char cMxfE[100];
     char cMxpfE[100];
     char cMyfE[100];
     char cMypfE[100];
     sprintf(cMxfE,"xfE_%d.dat",k);
     sprintf(cMxpfE,"xpfE_%d.dat",k);
     sprintf(cMyfE,"yfE_%d.dat",k);
     sprintf(cMypfE,"ypfE_%d.dat",k);
     const int maxevent=2000;
     double exf[maxevent];
     double expf[maxevent];
     double eyf[maxevent];
     double eypf[maxevent];
     double eexf[maxevent];
     double eexpf[maxevent];
     double eeyf[maxevent];
     double eeypf[maxevent];
     double expt[maxevent];
     double eypt[maxevent];
     double emom[maxevent];
     double cxfe[nParamF];
     double cxpfe[nParamF];
     double cyfe[nParamF];
     double cypfe[nParamF];
     int Nevent=0;
     int N=0;
     for (int i=0;i<nG4;i++){
       tG4->GetEntry(i);
       if (SPLTrig==1 && fabs(XFP)<20&&fabs(XpFP)<.1&&fabs(YFP)<6 && N<maxevent){
	 exf[N]=-XFP;
	 expf[N]=-XpFP;
	 eyf[N]=-YFP;
	 eypf[N]=-YpFP;
	 eexf[N]=1e-4;
	 eexpf[N]=1e-4;
	 eeyf[N]=1e-4;
	 eeypf[N]=1e-4;
	 double xpt=calcf2t(cxe,(-XFP-EXFPm)/EXFPr,(-XpFP-EXpFPm)/EXpFPr,
			    (-YFP-EYFPm)/EYFPr,(-YpFP-EYpFPm)/EYpFPr);
	 double ypt=calcf2t(cye,(-XFP-EXFPm)/EXFPr,(-XpFP-EXpFPm)/EXpFPr,
			    (-YFP-EYFPm)/EYFPr,(-YpFP-EYpFPm)/EYpFPr);
	 double mom=calcf2t(cpe,(-XFP-EXFPm)/EXFPr,(-XpFP-EXpFPm)/EXpFPr,
			    (-YFP-EYFPm)/EYFPr,(-YpFP-EYpFPm)/EYpFPr);
	 expt[N]=(xpt-EXptm)/EXptr;
	 eypt[N]=(ypt+EYptm)/EYptr;
	 emom[N]=(mom-EMomm)/EMomr;
	 //cout << N<<" "<<ExptR[j]<<" "<<EyptR[j]<<" "<<EmomR[j]<<endl;
	 //cout << i<<" "<<expt[N]<<" "<<eypt[N]<<" "<<emom[N]<<endl;
	 //cout << i<<" "<<xpt<<" "<<ypt<<" "<<mom<<endl;
	 //cout << i<<" "<<exf[N]<<" "<<expf[N]<<" "<<eyf[N]<<" "<<eypf[N]<<endl;
	 exf[N]=gRandom->Gaus(exf[N],exf[N]*1e-4);
	 expf[N]=gRandom->Gaus(expf[N],expf[N]*1e-4);
	 eyf[N]=gRandom->Gaus(eyf[N],eyf[N]*1e-4);
	 eypf[N]=gRandom->Gaus(eypf[N],eypf[N]*1e-4);
	 N++;
	 h50->Fill(-XFP,-YFP);
	 h51->Fill(-XpFP,-YpFP);
	 h52->Fill(xpt,ypt);
	 h53->Fill(XFP,mom);
       }
     }
     Nevent=N;
     calcMatrix(Nevent, exf, eexf, expt, eypt, emom, cxfe,cMxfE);
     calcMatrix(Nevent, expf, eexpf, expt, eypt, emom, cxpfe,cMxpfE);
     calcMatrix(Nevent, eyf, eeyf, expt, eypt, emom, cyfe, cMyfE);
     calcMatrix(Nevent, eypf, eeypf, expt, eypt, emom, cypfe,cMypfE);
     h50->Write();
     h51->Write();
     h52->Write();
     h53->Write();
     /*delete h1;
       delete h2;
       delete h3;
       delete h4;
       delete h10;
       delete h11;
       delete h12;
       delete h13;
       delete h20;
       delete h21;
       delete h22;
       delete h23;
       delete h30;
       delete h31;
       delete h32;
       delete h33;
       delete h40;
       delete h41;
       delete h42;
       delete h43;
       delete h50;
       delete h51;
       delete h52;
       delete h53;
       delete h100;
       delete h101;
       delete h1201; 
       delete h1202;
       delete h110;
       delete h111;
       delete h1211; 
       delete h120;
       delete h121;
       delete h1221; 
       delete f100;
       delete f101;
       delete f1201;
       delete f1202;*/
   }
   ofile->Write();
   ofile->Close();
   return 0;
}
