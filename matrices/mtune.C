/*

  -- mtune.C --
  Class of "mtclass" is defined here.
  Originally made by D.Kawama
  Modified by T.Gogami
  
*/

//#include "parameter.h"
#include "mtune.h"
#include <fstream>
#include <iostream>
#include <TH1F.h>
#include <TF1.h>
#include <TF2.h>
#include <TFile.h>
#include <TTree.h>
#include <TMinuit.h>
#include <TProfile.h>
#include <TSpectrum.h>
#include <math.h>
#include <omp.h>
#include "FitLog.hh"

using namespace std;
extern int Evflag[nTrig];
extern int FCNFlag;
extern float resMM;
extern int oa[allParam];
extern int ob[allParam];
extern int oc[allParam];
extern int od[allParam];
extern ofstream logfile;
extern float mincut;
extern float maxcut;
extern int cutflag;

mtclass::mtclass(char* str, TTree* tree,
		 float* cut, TF1* fun,
		 TH1F *hist, int hflag )
  :hyperflag(hflag)
{
  sprintf(peakName,str);
  cout << " Set mtclass : " << peakName << endl;
  Tree=tree;
  cutL_=cut[0];
  cutU_=cut[1];
  fun_=fun;
  hist_=hist;
  nentries =(int)Tree->GetEntries();
  //hyperflag = hflag;
  
  sprintf(histname_org ,"mmhist_%s_org" ,str);
  sprintf(histname_bg  ,"mmhist_%s_bg"  ,str);
  sprintf(histname_subt,"mmhist_%s_subt",str);
  sprintf(fitfuncname,"fitf_%s",str);
  fun_ -> SetName(fitfuncname);
  //   if( hyperflag ==1 ){
  //     mmhist_org = new TH1F(histname_org,histname_org,
  //			   //100 , -0.04 , 0.04 );
  //			   100 , -0.04+1.115683 , 0.04+1.115683 );
  //     
  //   }
  //   else {
  //     mmhist_org = new TH1F(histname_org,histname_org,
  //			   //400 , -0.2 , 0.2 );
  //			   400 , -0.2+1.115683 , 0.2+1.115683 );
  //   }
  if( hyperflag==100 || hyperflag==101 || 
      hyperflag==110 || hyperflag==111 ){
    mmhist_org = new TH1F(histname_org,histname_org,
			  400 , -0.2+1.115683 , 0.2+1.115683 );
    
  }
  else if( hyperflag==1000 ){ // 10LBe g.s. (10B Target)                       
    mmhist_org = new TH1F(histname_org,histname_org,
                          75 , -0.02+1.115683 , 0.01+1.115683 );
    //                                                                         
  }
  else if( hyperflag==700 ){ //  7LHe g.s. (7Li Target)                       
    mmhist_org = new TH1F(histname_org,histname_org,
                          75 , -0.02+1.115683 , 0.01+1.115683 );
    //                                                                         
  }
  else if( hyperflag==5200 ){ //  52LV (1) (52Cr Target)                       
    mmhist_org = new TH1F(histname_org,histname_org,
                          60 , -0.050+1.115683 , 0.010+1.115683 );
    //                                                                         
  }
  else {
    mmhist_org = new TH1F(histname_org,histname_org,
			  //100 , -0.04+1.115683 , 0.04+1.115683 );//800keV/bin
			  200 , -0.04+1.115683 , 0.04+1.115683 );//400keV/bin    
    //
  }
  mmhist_org->GetXaxis()->SetTitle("M^{2}_{X} - M^{2}_{#Lambda}");
  mmhist_bg = (TH1F*)mmhist_org->Clone();
  //mmhist_bg->SetName("mmhist_bg");
  mmhist_bg->SetName(histname_bg);
  
  // ======== For Raster Corrections ========= //
  // ======== ( T.Gogami , 18May2013) ======== //
  sprintf(hname_mmxt,"h_mmxt_%s",str);
  sprintf(hname_mmyt,"h_mmyt_%s",str);
  if(hyperflag==100){
    //--- Histograms ---
    mm_xt = new TH2F(hname_mmxt,hname_mmxt,
		     200,-0.05,0.05,
		     200,-0.01,0.01);
    mm_yt = new TH2F(hname_mmyt,hname_mmyt,
		     200,-0.15,0.15,
		     200,-0.01,0.01);
    //--- Profiles ---
    double pxmin,pxmax;
    double pymin,pymax;
    int pn = 100;
    pxmin = mm_xt->GetXaxis()->GetXmin();
    pxmax = mm_xt->GetXaxis()->GetXmax();
    pro_mmxt = new TProfile("pro_mmxt",
			    "profile_mmxt",
			    pn,pxmin,pxmax);
    pymin = mm_yt->GetXaxis()->GetXmin();
    pymax = mm_yt->GetXaxis()->GetXmax();
    pro_mmyt = new TProfile("pro_mmyt",
			    "profile_mmyt",
			    pn,pymin,pymax);
    func_mmxt = new TF1("func_mmxt","[0]+[1]*x",
			mm_xt->GetXaxis()->GetXmin(),
			mm_xt->GetXaxis()->GetXmax() );
    func_mmyt = new TF1("func_mmyt","[0]+[1]*x",
			mm_yt->GetXaxis()->GetXmin(),
			mm_yt->GetXaxis()->GetXmax() );
    npar_fmmxt = func_mmxt->GetNpar();
    npar_fmmyt = func_mmyt->GetNpar();
    for(int i=0 ; i<npar_fmmxt ; i++){
      func_mmxt->SetParameter(i,0.0);
    }
    //func_mmxt->SetParameters(0.0,0.0);
    func_mmxt->SetLineColor(1);
    func_mmxt->SetLineWidth(1);
    for(int i=0 ; i<npar_fmmyt ; i++){
      func_mmyt->SetParameter(i,0.0);
    }
    //func_mmyt->SetParameters(0.0,0.0);
    func_mmyt->SetLineColor(1);
    func_mmyt->SetLineWidth(1);
    
    // --- [0]=0 --- //
    func_mmxt_ = new TF1("func_mmxt_",func_mmxt->GetExpFormula(),
			 mm_xt->GetXaxis()->GetXmin(),
			 mm_xt->GetXaxis()->GetXmax() );
    func_mmyt_ = new TF1("func_mmyt_",func_mmyt->GetExpFormula(),
			 mm_yt->GetXaxis()->GetXmin(),
			 mm_yt->GetXaxis()->GetXmax() );
    for(int i=0 ; i<npar_fmmxt ; i++){
      func_mmxt_->SetParameter(i,0.0);
    }
    //func_mmxt_->SetParameters(0.0,0.0);
    func_mmxt_->SetLineColor(2);
    func_mmxt_->SetLineWidth(2);
    for(int i=0 ; i<npar_fmmyt ; i++){
      func_mmyt_->SetParameter(i,0.0);
    }
    //func_mmyt_->SetParameters(0.0,0.0);
    func_mmyt_->SetLineColor(2);
    func_mmyt_->SetLineWidth(2);
  }
  /////////////////////////////////////////////////
}
mtclass::mtclass(char* str, TTree* tree,
		 float* cut, TF1* fun,
		 TH1F *hist )
{
   sprintf(peakName,str);
   Tree=tree;
   cutL_=cut[0];
   cutU_=cut[1];
   fun_=fun;
   hist_=hist;
   nentries =(int)Tree->GetEntries();
   hyperflag = 1;
   sprintf(fitfuncname,"fitf_%s",str);
   fun_ -> SetName(fitfuncname);
   if( hyperflag ==1 ){
     mmhist_org = new TH1F("mmhist_org","mmhist_org",
			   100 , -0.04 , 0.04 );
   }
   else {
     mmhist_org = new TH1F("mmhist_org","mmhist_org",
			   400 , -0.2 , 0.2 );
     mmhist_org->GetXaxis()->SetTitle("M^{2}_{X} - M^{2}_{#Lambda}");
   }
   mmhist_org->GetXaxis()->SetTitle("M^{2}_{X} - M^{2}_{#Lambda}");
   mmhist_bg = (TH1F*)mmhist_org->Clone();
   mmhist_bg->SetName("mmhist_bg");
}

mtclass::~mtclass(){
}

int mtclass::init(){
  cout << endl;
  cout << " mtclass::init() for " << peakName << endl;
  float EXFP=0.,EXpFP=0.,EYFP=0.,EYpFP=0.;
  float EMom=0.,EXpt=0.,EYpt=0.;
  float EXSS=0.,EYSS=0.;
  float KXFP=0.,KXpFP=0.,KYFP=0.,KYpFP=0.;
  float KMom=0.,KXpt=0.,KYpt=0.;
  float xt=0.,yt=0.;
  float EXFPSS=0.,EXpFPSS=0.,EYFPSS=0.,EYpFPSS=0.;
  float EMomSS=0.,EXptSS=0.,EYptSS=0.;
  float KXFPSS=0.,KXpFPSS=0.,KYFPSS=0.,KYpFPSS=0.;
  float KMomSS=0.,KXptSS=0.,KYptSS=0.;
  float exf=0.,expf=0.,eyf=0.,eypf=0.;
  float kxf=0.,kxpf=0.,kyf=0.,kypf=0.;
  float haernpe1=0.,haernpe2=0.,haernpe3=0.;
  float hwatnpe1=0.,hwatnpe2=0.;
  float hwatnkn1=0.,hwatnkn2=0.;
  float hwatnum1=0.,hwatnum2=0.;
  float htrkchi2=0.,htrkndf=0.;
  float etrkchi2=0.,etrkndf=0.;
  float hsbeta=0.,hsbeta_k=0.,hmsq=0.;
  float res=0.;
  float mm=0.,ctime=0.,hallcp=0.;
  float ETheta=0.;
  Char_t Trig=0;
  int evflag=0;
  int mmflag=0;
  float runnum=0;
  float eventid=0;
  int nflags=0;
  int run[6000],eid[6000];
  int j=0;
  char cMKmom[100];
  char cMEmom[100];
  char cMKxpt[100];
  char cMExpt[100];
  char cMKypt[100];
  char cMEypt[100];
  char input[100];
  
  /*cout << peakName << endl;
    if (strcmp(peakName,"A")!=0 || strcmp(peakName,"lambda_h2o")!=0){
    strcpy(input,peakName);
    strcat(input,".in");
    //cout << input << endl;
    ifstream data(input);
    while(!data.eof()){
    data >> run[j]>>eid[j];
    j++;
    }
    nflags=j;
    }*/
  j=0;
  
  Tree->SetBranchAddress("esxfp",&EXFP);
  Tree->SetBranchAddress("esxpfp",&EXpFP);
  Tree->SetBranchAddress("esyfp",&EYFP);
  Tree->SetBranchAddress("esypfp",&EYpFP);
  Tree->SetBranchAddress("emom",&EMom);
  Tree->SetBranchAddress("expt",&EXpt);
  Tree->SetBranchAddress("eypt",&EYpt);
  /*Tree->SetBranchAddress("esp",&EMom);
    Tree->SetBranchAddress("esxptar",&EXpt);
    Tree->SetBranchAddress("esyptar",&EYpt);*/
  Tree->SetBranchAddress("esxsv",&EXSS);
  Tree->SetBranchAddress("esysv",&EYSS);
  Tree->SetBranchAddress("etheta",&ETheta);
  Tree->SetBranchAddress("hsxfp",&KXFP);
  Tree->SetBranchAddress("hsxpfp",&KXpFP);
  Tree->SetBranchAddress("hsyfp",&KYFP);
  Tree->SetBranchAddress("hsypfp",&KYpFP);
  Tree->SetBranchAddress("kmom",&KMom);
  Tree->SetBranchAddress("kxpt",&KXpt);
  Tree->SetBranchAddress("kypt",&KYpt);
  /*Tree->SetBranchAddress("hsp",&KMom);
    Tree->SetBranchAddress("hsxptar",&KXpt);
    Tree->SetBranchAddress("hsyptar",&KYpt);*/
  Tree->SetBranchAddress("hsbeta",&hsbeta);
  Tree->SetBranchAddress("hsbeta_k",&hsbeta_k);
  Tree->SetBranchAddress("hmsq",&hmsq);
  Tree->SetBranchAddress("haernpe1",&haernpe1);
  Tree->SetBranchAddress("haernpe2",&haernpe2);
  Tree->SetBranchAddress("haernpe3",&haernpe3);
  Tree->SetBranchAddress("hwatnum1",&hwatnum1);
  Tree->SetBranchAddress("hwatnum2",&hwatnum2);
  Tree->SetBranchAddress("hwatnpe1",&hwatnpe1);
  Tree->SetBranchAddress("hwatnpe2",&hwatnpe2);
  Tree->SetBranchAddress("hwatnkn1",&hwatnkn1);
  Tree->SetBranchAddress("hwatnkn2",&hwatnkn2);
  Tree->SetBranchAddress("htrkchi2",&htrkchi2);
  Tree->SetBranchAddress("htrkndf",&htrkndf);
  Tree->SetBranchAddress("etrkchi2",&etrkchi2);
  Tree->SetBranchAddress("etrkndf",&etrkndf);
  Tree->SetBranchAddress("hallcp",&hallcp);
  Tree->SetBranchAddress("ctime",&ctime);
  Tree->SetBranchAddress("gbeamx",&xt);
  Tree->SetBranchAddress("gbeamy",&yt);
  Tree->SetBranchAddress("mm",&mm);
  Tree->SetBranchAddress("eventid",&eventid);
  Tree->SetBranchAddress("runnum",&runnum);
  cout << " Number of Event = " << nentries <<endl;
  for (int i=0; i<nentries; i++){
    Tree->GetEntry(i);
    mmflag=0;
    
    /*if (strcmp(peakName,"A")!=0){
      for (int k=0;k<nflags;k++){
      if (runnum==run[k]&&eventid==eid[k]){
      mmflag=1;
      break;
      }
      }
      }*/
    
    if (mm>cutL_ && mm< cutU_) mmflag=1;
    else mmflag=0;
    
    int cutflaglocal=0;
    //if (cutflag==0 && KXpFP<maxcut && KXpFP>mincut){
    if (cutflag==0 && KXFP<maxcut && KXFP>mincut){
      cutflaglocal=1;
    }
    //else if (cutflag==1 && (KXpFP>maxcut || KXpFP<mincut)){
    else if (cutflag==1 && (KXFP>maxcut || KXFP<mincut)){
      cutflaglocal=1;
    }
    if(hyperflag==110 || hyperflag==111){// Water target
      //if( fabs( ctime + 54.7 ) < 0.8 ){
      if( fabs( ctime + 54.7 ) < 1.0 ){
	timecut = true;
      }
      else timecut = false;
    }
    else if ( hyperflag==5200){
      if ( ( fabs(ctime+54.6)<1.0 && 75976.5<runnum && runnum<76049.5 ) ||
	   ( fabs(ctime+54.9)<1.0 && 76716.5<runnum && runnum<76820.5 ) ||
	   ( fabs(ctime+55.0)<1.0 && 76845.5<runnum && runnum<76937.5 ) || 
	   ( fabs(ctime+55.0)<1.0 && 76947.5<runnum && runnum<77064.5 ) ||
	   ( fabs(ctime+54.4)<1.0 && runnum>77064.5) ){
	timecut = true;
      }
      else timecut = false;
    }
    else{
      //if( fabs( ctime + 54.45 ) < 0.9 ){
      if( fabs( ctime + 54.4 ) < 1.0 ){
	timecut = true;
      }
      else timecut = false;
    }
    if( hyperflag!=5200){
      if( pow(haernpe1+haernpe2+haernpe3,2.0)/(10.0*10.0) + pow(hmsq-0.25,2.0)/(0.25*0.25)<1.0 && 
	  pow(hwatnkn1+hwatnkn2-2.0,2.0)/(0.7*0.7) + pow(hmsq-0.25,2.0)/(0.25*0.25)<1.0)
	{cherecut=true;}
      else cherecut=false;
    }
    else if (hyperflag==5200){
      if( haernpe1+haernpe2+haernpe3 < 5.0 && 
	  hwatnkn1+hwatnkn2 > 0.8 && 
	  fabs(hmsq-0.25)<0.30 )
	{cherecut=true;}
      else cherecut=false;
    }
    if (//mmflag==1  
	//0.75 < EMom && EMom < 0.93 && // For test (Toshi,2Feb2013)
	//1.05 < KMom && KMom < 1.25 && // For test (Toshi,2Feb2013)
	0.70 < EMom && EMom < 0.98 && // For test (Toshi,15June2013)
	1.00 < KMom && KMom < 1.30 && // For test (Toshi,15June2013)
	fabs(KXpFP-2.0e-3*KXFP)<.12    //toshi
	&& KYpFP-1.5e-3*KYFP<0.014     //toshi (to cut e,e+ events)
	&& KYpFP-1.5e-3*KYFP>-0.013    //toshi (to cut e,e+ events)
	//&& pow(haernpe1+haernpe2+haernpe3,2.0)/(10.0*10.0) + pow(hmsq-0.25,2.0)/(0.25*0.25)<1.0//toshi
	//&& pow(hwatnkn1+hwatnkn2-2.0,2.0)/(0.7*0.7) + pow(hmsq-0.25,2.0)/(0.25*0.25)<1.0//toshi for CH2 (pi cut)
	&& cherecut // Toshi , 17Sep2013
	&& fabs(KYpt)<0.08 && -0.2<KXpt && KXpt<0.0 //
	//&& fabs(KYpt+0.05)<0.07 && fabs(EXpt)<0.15 // Wrong !!!!
	&& fabs(EYpt+0.055)<0.07 && fabs(EXpt)<0.15 // Correct !!!! (Toshi,13June2013)
	&& timecut == true
	//&& fabs(ctime+54.45)<0.7//toshi
	//&& etrkchi2<5 && htrkchi2<5. //toshi
	//&& etrkchi2<20. && htrkchi2<20. //toshi
	//&& etrkchi2<50. && htrkchi2<50. // Toshi , 24Apr2013
	//&& etrkchi2<111. && htrkchi2<111. // Toshi , 4Sep2013 for test
	&& etrkchi2<200.0 && htrkchi2<200.0 // Toshi , 17Sep2013 for test
	//&& etrkchi2<400. && htrkchi2<400. // Toshi , 4Sep2013 for test
	&& cutflaglocal==1 ){
      
      //mmhist_org -> Fill(mm-1.115683);
      mmhist_org -> Fill( mm );
      if( fabs(mm-1.115683)<0.01 && hyperflag==100 ){
	mm_xt -> Fill(xt,mm-1.115683);
	mm_yt -> Fill(yt,mm-1.115683);
	pro_mmxt->Fill(xt,mm-1.115683);
	pro_mmyt->Fill(yt,mm-1.115683);
      }
      
      if( mmflag==1 ){
	//if (mmflag==1 )npeak++;
	npeak++;
	exf  =(EXFP-EXFPm)/EXFPr;   //[cm]
	expf =(EXpFP-EXpFPm)/EXpFPr;//[rad]
	eyf  =(EYFP-EYFPm)/EYFPr;   //[cm]
	eypf =(EYpFP-EYpFPm)/EYpFPr;//[rad]
	kxf  =(KXFP-KXFPm)/KXFPr;   //[cm]
	kxpf =(KXpFP-KXpFPm)/KXpFPr;//[rad]
	kyf  =(KYFP-KYFPm)/KYFPr;   //[cm]
	kypf =(KYpFP-KYpFPm)/KYpFPr;//[rad]
	Exf[j]=EXFP;
	Expf[j]=EXpFP;
	Eyf[j]=EYFP;
	Eypf[j]=EYpFP;
	Kxf[j]=KXFP;
	Kxpf[j]=KXpFP;
	Kyf[j]=KYFP;
	Kypf[j]=KYpFP;
	Xt[j]=xt;
	Yt[j]=yt;
	Bvar[j]=hallcp*1e-3;
	Emom[j]=EMom;
	Expt[j]=EXpt;
	Eypt[j]=EYpt;
	Kmom[j]=KMom;
	Kxpt[j]=KXpt;
	Kypt[j]=KYpt;
	EXHoleID[j]=-1;
	EYHoleID[j]=-1;
	KXHoleID[j]=-1;
	KYHoleID[j]=-1;
	MM0[j]=mm;
	GetVar(j, exf, expf, eyf, eypf, Evar);
	GetVar(j, kxf, kxpf, kyf, kypf, Kvar);
	hist_->Fill(mm);
	j++;
      }
    }
  }
  npeak=j;
  logfile << "Number of " << peakName << "=" << npeak << endl;
  cout << " --> Selected " << npeak << endl;
  
  // ====== Toshi , 5/Jan/2013 ========= //
  //   --- Background subtraction---     //
  TSpectrum* s = new TSpectrum(10,1.0);
  int np = s->Search(mmhist_org,2,"",0.3);
  //s->Print();
  int nbin_hist_ = mmhist_org->GetXaxis()->GetNbins();
  const int nbin_hist = nbin_hist_;
  float source[nbin_hist];
  for(int i=0 ; i<nbin_hist ; i++) source[i]=mmhist_org->GetBinContent(i+1);
  if( hyperflag==1000 ){
    s->Background(source , nbin_hist , 30 ,
                  TSpectrum::kBackDecreasingWindow,
                  TSpectrum::kBackOrder2,
                  kTRUE,
                  TSpectrum::kBackSmoothing3,
                  kFALSE);
  }
  else{
    //s->Background(source , nbin_hist , 20 ,
    s->Background(source , nbin_hist , 25 ,
		  TSpectrum::kBackDecreasingWindow,
		  TSpectrum::kBackOrder4,
		  kTRUE,
		  TSpectrum::kBackSmoothing3,
		  kFALSE);
  }
  double histmin , histmax;
  histmin = mmhist_org->GetXaxis()->GetXmin();
  histmax = mmhist_org->GetXaxis()->GetXmax();
  //TH1F* hist_bg = new TH1F("hist_bg","hist_bg",nbin_hist,histmin,histmax);
  for(int i=0 ; i<nbin_hist ; i++) mmhist_bg -> SetBinContent(i+1,source[i]);
  mmhist_subt = (TH1F*)mmhist_org->Clone();
  //mmhist_subt->SetName("mmhist_subt");
  mmhist_subt->SetName(histname_subt);
  mmhist_subt->Add(mmhist_bg,-1);
  
  if( hyperflag==100 ){
    // ==== Raster Correction ============ //
    // ==== (Toshi Gogami , 19/May/2013) = //
    pro_mmxt->Fit(func_mmxt,"Nq","");
    pro_mmyt->Fit(func_mmyt,"Nq","");
    for(int i=0 ; i<npar_fmmxt ; i++){
      if(i==1){
	func_mmxt_->SetParameter( i , func_mmxt->GetParameter(i) );
      }
    }
    for(int i=0 ; i<npar_fmmyt ; i++){
      if(i==1){
	func_mmyt_->SetParameter( i , func_mmyt->GetParameter(i) );
      }
    }
  }
  


//  if ( hyperflag == 100 ){
//    // ==== Raster Correction ============ //
//    // ==== (Toshi Gogami , 19/May/2013) = //
//    double pxmin,pxmax;
//    double pymin,pymax;
//    int pn = 100;
//    pxmin = mm_xt->GetXaxis()->GetXmin();
//    pxmax = mm_xt->GetYaxis()->GetXmax();
//    pro_mmxt = new TProfile("pro_mmxt",
//			    "profile_mmxt",
//			    pn,pxmin,pxmax);
//    pymin = mm_yt->GetXaxis()->GetXmin();
//    pymax = mm_yt->GetYaxis()->GetXmax();
//    pro_mmyt = new TProfile("pro_mmyt",
//			    "profile_mmyt",
//			    pn,pymin,pymax);
//    pro_mmxt->Project(hname_mmxt);
//    pro_mmyt->Project(hname_mmyt);
//    
//    
//  }
  
  //hist_->Fit(fun_,"0");
  fun_->SetParName(0,"Coefficient");
  fun_->SetParName(1,"Mean");
  fun_->SetParName(2,"Sigma");
  fun_->SetParName(3,"Base");
  //mmhist_subt->Fit(fun_,"0","",cutL_,cutU_);// Toshi , 9/Jan/2013
  if(hyperflag==700){
    mmhist_subt->Fit(fun_,"B0","",1.108683,1.111183);// Toshi , 10/Oct/2013 for test
  }
  else{
    mmhist_subt->Fit(fun_,"B0","",cutL_,cutU_);// Toshi , 16/June/2013
  }
  
  mu   = fun_->GetParameter(1);
  sigma= fun_->GetParameter(2);
  chi  = fun_->GetChisquare();
  logfile << "mu=" << mu << " " 
	  << "sigma=" << sigma <<" "
	  << "chi2=" << chi <<endl;
  cout << " mean-ml , sigma [ MeV/c2 ] = " << mu*1.0e3 - 1115.683 << " , "
       << sigma*1.0e3 << endl;
  
  return 0;
  
  /*TreeSSE->SetBranchAddress("EXFP",&EXFPSS);
    TreeSSE->SetBranchAddress("EXpFP",&EXpFPSS);
    TreeSSE->SetBranchAddress("EYFP",&EYFPSS);
    TreeSSE->SetBranchAddress("EYpFP",&EYpFPSS);
    TreeSSE->SetBranchAddress("EMom",&EMomSS);
    TreeSSE->SetBranchAddress("EXpt",&EXptSS);
    TreeSSE->SetBranchAddress("EYpt",&EYptSS);
    for (int i=nLS;i<(nLS+NSSE);i++){
    EXHoleID[i]=-1;
    EYHoleID[i]=-1;
    TreeSSE->GetEntry(i-nLS);
    Emom[i]=EMomSS;
    Expt[i]=EXptSS;
    Eypt[i]=EYptSS;
    exf=(EXFPSS-EXFPm)/EXFPr;//[cm]
    Expf=(EXpFPSS-EXpFPm)/EXpFPr;//[rad]
    Eyf=(EYFPSS-EYFPm)/EYFPr;//[cm]
    Eypf=(EYpFPSS-EYpFPm)/EYpFPr;//[rad]
    GetVar(i, exf, Expf, Eyf, Eypf, 0);
    Evflag[i]=-1;
    float zzz=-sin(0.0028)*EXFPSS+cos(0.0028)*EXpFPSS;
    for (int l=0;l<EnHoleX;l++){
    float b=0.097-0.018*l;
    float w=0.007;
    if(fabs(zzz-b)<w){
    EXHoleID[i]=l;
    }
    }
    zzz=-sin(0.0021)*EYFPSS+cos(0.0021)*EYpFPSS;
    for (int l=0;l<EnHoleY;l++){
    float b=1.1e-2-0.0038*l;
    float w=0.0006;
    if(fabs(zzz-b)<w){
    EYHoleID[i]=l;
    }
    }
    }
    TreeSSK->SetBranchAddress("KXFP",&KXFPSS);
    TreeSSK->SetBranchAddress("KXpFP",&KXpFPSS);
    TreeSSK->SetBranchAddress("KYFP",&KYFPSS);
    TreeSSK->SetBranchAddress("KYpFP",&KYpFPSS);
    TreeSSK->SetBranchAddress("KMom",&KMomSS);
    TreeSSK->SetBranchAddress("KXpt",&KXptSS);
    TreeSSK->SetBranchAddress("KYpt",&KYptSS);
    for (int i=nLS+NSSE;i<(nLS+NSSE+NSSK);i++){
    KXHoleID[i]=-1;
    KYHoleID[i]=-1;
    TreeSSK->GetEntry(i-nLS-NSSE);
    Kmom[i]=KMomSS;
    Kxpt[i]=KXptSS;
    Kypt[i]=KYptSS;
    Kxf=(KXFPSS-KXFPm)/KXFPr;//[cm]
    Kxpf=(KXpFPSS-KXpFPm)/KXpFPr;//[rad]
    Kyf=(KYFPSS-KYFPm)/KYFPr;//[cm]
    Kypf=(KYpFPSS-KYpFPm)/KYpFPr;//[rad]
    GetVar(i, Kxf, Kxpf, Kyf, Kypf, 1);
    Evflag[i]=-2;
    float zzz=-sin(0.0015)*KXFPSS+cos(0.0015)*KXpFPSS;
    for (int l=0;l<KnHoleX;l++){
    float b=0.089-0.017*l;
    float w=0.008;
    if(fabs(zzz-b)<w){
    KXHoleID[i]=l;
    }
    }
    zzz=-sin(0.0021)*KYFPSS+cos(0.0021)*KYpFPSS;
    for (int l=0;l<KnHoleY;l++){
    float b=0.0075-0.0025*l;
    float w=0.0005;
    if(fabs(zzz-b)<w){
    KYHoleID[i]=l;
    }
    }
    }*/
}
