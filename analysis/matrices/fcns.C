/*
  
  -- fcns.C --
  Originally made by D.Kawama
  Modified by T.Gogami
 
*/

#include <fstream>
#include <iostream>
#include <TH1F.h>
#include <TF1.h>
#include <TFile.h>
#include <TTree.h>
#include <TMinuit.h>
#include <TRandom.h>
#include <math.h>
#include <omp.h>
#include "mtune.h"
#include "parameter.h"
using namespace std;
//int omp_get_num_threads(void);
extern float gE[nTrig][nParam];
extern float gK[nTrig][nParam];
extern float gB[nTrig];
extern float gmomE0[nTrig];
extern float gmomK0[nTrig];
extern float gdmm[nTrig];
extern float gdmomE[nTrig];
extern float gdxptE[nTrig];
extern float gdyptE[nTrig];
extern float gdmomK[nTrig];
extern float gdxptK[nTrig];
extern float gdyptK[nTrig];
extern int gEvflag[nTrig];
extern float resMM1;
extern float resMM2;
extern float gsmass;
extern float gsmm;     // 12LB g.s. (low)     Reference 
extern float psmm;     // 12LB p-shell (low)  Referecne
extern float gsmm_high;// 12LB g.s. (high)    Reference 
extern float psmm_high;// 12LB p-shell (high) Reference
extern float gsmm_ch2; // 12LB g.s. (CH2)     Referecne
extern float gsmm_b10; // 10LBe g.s.          Referecne
extern float gsmm_li7; //  7LHe g.s.          Referecne
extern float cemm;     // 12LB core excited1  Referecne
extern float cemm_high;// 12LB core excited1  Referecne
extern float mm1_cr52; // 52LV Reference
extern float rasxptE[10];
extern float rasyptE[10];
extern float rasmomK[10];
extern float rasxptK[10];
extern float rasyptK[10];
extern float w_chisql;
extern float w_chisql_h2o;
extern float w_chisqs_h2o;
extern float w_chisqs;
extern float w_chisqgsl;
extern float w_chisqgsh;
extern float w_chisqgs_ch2;
extern float w_chisqgs_b10;
extern float w_chisqgs_li7;
extern float w_chisqpk;
extern float w_chisqpe;
extern float w_chisqcel;
extern float w_chisqceh;
extern float w_chisq_cr52; // Added by Tosh , 10Jan2014
int FCNFlag;
int EOFlag;
extern int oa[allParam];
extern int ob[allParam];
extern int oc[allParam];
extern int od[allParam];
extern int nLambda;
extern int nSigma;
extern int nLB12gsl;
extern int nLB12gsh;
extern int nLB12psl;
extern int nLB12psh;
extern int nLambda_h2o;
extern int nSigma_h2o;
extern int nLB12gsch2;
extern int nLBe10gs;
extern int nLHe7gs;
extern int nLB12cel;
extern int nLB12ceh;
extern int nLV52_1;
extern ofstream logfile;
TH1F *h1;
TH1F *h2;

void fcn(int &npar, double*gin, double&f,double*par, int iflag)
{
   float chisql     = 0.0;
   float chisql_h2o = 0.0;
   float chisqs_h2o = 0.0;
   float chisqs     = 0.0;
   float chisqgsl   = 0.0;
   float chisqgsh   = 0.0;
   float chisqpsl   = 0.0;
   float chisqpsh   = 0.0;
   float chisqgsch2 = 0.0; // Toshi , 28Feb2013
   float chisqgsb10 = 0.0; // Toshi , 12June2013
   float chisqgsli7 = 0.0; // Toshi , 14Aug2013
   float chisqcel   = 0.0; // Toshi ,  6Nov1013
   float chisqceh   = 0.0; // Toshi ,  6Nov1013
   float chisqcr52_1= 0.0; // Toshi ,  6Nov1013
   float chisqpe    = 0.0;
   float chisqpk    = 0.0;
   float chisqxe    = 0.0;
   float chisqye    = 0.0;
   float chisqxk    = 0.0;
   float chisqyk    = 0.0;
   float deltal     = 0.0;
   float deltal_h2o = 0.0;
   float deltas_h2o = 0.0;
   float deltas     = 0.0;
   float deltagsl   = 0.0;
   float deltagsh   = 0.0;
   float deltapsl   = 0.0;
   float deltapsh   = 0.0;
   float deltagsch2 = 0.0; // Toshi , 28Feb2013
   float deltagsb10 = 0.0; // Toshi , 12June2013
   float deltagsli7 = 0.0; // Toshi , 14Aug2013
   float deltacel   = 0.0; // Toshi ,  6Nov2013
   float deltaceh   = 0.0; // Toshi ,  6Nov2013
   float deltacr52_1= 0.0; // Toshi ,  10Jan2014
   float deltape    = 0.0;
   float deltapk    = 0.0;
   float deltaxe    = 0.0;
   float deltaye    = 0.0;
   float deltaxk    = 0.0;
   float deltayk    = 0.0;
   float meanl      = 0.0;
   float means      = 0.0;
   float meangsl    = 0.0;
   float meangsh    = 0.0;
   double cpe[nParam];
   double cpk[nParam];
   double cxe[nParam];
   double cxk[nParam];
   double cye[nParam];
   double cyk[nParam];
   float deltaL[42];
   float chisqL[42];
   float deltaL_=0.0;
   float chisqL_=0.0;
   int nL_=0;
   int nL[42];
   int iL=0;
   //float mmR=0.;
   int l    =1;
   int l_h2o=1;
   int s_h2o=1;
   int s    =1;
   int gsl  =1;
   int gsh  =1;
   int psl  =1;
   int psh  =1;
   int gsch2=1;
   int gsb10=1;
   int gsli7=1;
   int cel  =1;
   int ceh  =1;
   int cr52_1 =1;
   int SSxe =1;
   int SSye =1;
   int SSxk =1;
   int SSyk =1;
   int ngs=0;
   /*TH1F *hl = new TH1F("hl","",100,1.112, 1.118);
   TH1F *hs = new TH1F("hs","",100,1.189, 1.195);
   TH1F *hgsl = new TH1F("hgsl","",100,-0.015, -0.009);
   TH1F *hgsh = new TH1F("hgsh","",100,-0.015, -0.009);*/

   for (int i=0;i<42;i++){
      deltaL[i]=0.;
      chisqL[i]=0.;
      nL[i]=0;
   }

   f=0;
   for (int i=0;i<nParam;i++){
      int i0=i;
      int i1=i+nParam;
      int i2=i+nParam*2;
      int i3=i+nParam*3;
      int i4=i+nParam*4;
      int i5=i+nParam*5;
      cpe[i]=par[i0];
      cpk[i]=par[i1];
      cxe[i]=par[i2];
      cxk[i]=par[i3];
      cye[i]=par[i4];
      cyk[i]=par[i5];
   }
#ifdef _OPENMP   
#pragma omp parallel 
#endif
   {
#ifdef _OPENMP   
#pragma omp sections
#endif
     {
#ifdef _OPENMP   // ############ LAMBDA (CH2) ###############################
#pragma omp section
#endif
       {
	 for (int i=0;i<nLambda;i++){
	   //if (FCNFlag==1 && EOFlag==0){
	   if (gEvflag[i]==100){
	     float EmomR=recfcn(cpe,i,gE,6);
	     float KmomR=recfcn(cpk,i,gK,6);
	     float ExptR=recfcn(cxe,i,gE,6);
	     float KxptR=recfcn(cxk,i,gK,6);
	     float EyptR=recfcn(cye,i,gE,6);
	     float KyptR=recfcn(cyk,i,gK,6);
	     /*EmomR-=gdmomE[i];
	       ExptR-=gdxptE[i];
	       EyptR-=gdyptE[i];
	       KmomR-=gdmomK[i];
	       KxptR-=gdxptK[i];
	       KyptR-=gdyptK[i];*/
	     float mmR=calcMM(gB[i],EmomR,-ExptR,-EyptR,KmomR,KxptR,KyptR,1)+gdmm[i];
	     //hl->Fill(mmR);
	     //HKS momentum
	     
	     // === By T.Gogami === //
	     deltaL_  = ( mmR - ml ) / resMM1;
	     chisqL_ += deltaL_ * deltaL_;
	     nL_++;
	     
	   }
	 }
       }
#ifdef _OPENMP   // ############ SIGMA (CH2) ###############################
#pragma omp section
#endif
       {
	 for (int i=nLambda;i<nLambda+nSigma;i++){
	   if (gEvflag[i]==101){
	     float EmomR=recfcn(cpe,i,gE,6);
                  float KmomR=recfcn(cpk,i,gK,6);
                  float ExptR=recfcn(cxe,i,gE,6);
                  float KxptR=recfcn(cxk,i,gK,6);
                  float EyptR=recfcn(cye,i,gE,6);
                  float KyptR=recfcn(cyk,i,gK,6);
                  /*EmomR-=gdmomE[i];
                  ExptR-=gdxptE[i];
                  EyptR-=gdyptE[i];
                  KmomR-=gdmomK[i];
                  KxptR-=gdxptK[i];
                  KyptR-=gdyptK[i];*/
                  float mmR=calcMM(gB[i],EmomR,-ExptR,-EyptR,KmomR,KxptR,KyptR,1)+gdmm[i];
                  //hs->Fill(mmR);
                  if ((mmR-ms0)>0){
                     deltas = (mmR-(ms0+.0e-4))/resMM1;
                  }
                  else{
		    deltas = (mmR-(ms0+.0e-4))/resMM1;
                  }
                  chisqs += deltas*deltas;
                  means+=mmR-(ms0+.0e-4);
                  s++;
                  //chisqpk += (KmomR-gmomK0[i])*(KmomR-gmomK0[i])/(1.2e-3)/(1.2e-3);
                  //chisqpe += (EmomR-gmomE0[i])*(EmomR-gmomE0[i])/(0.84e-3)/(0.84e-3);
	   }// if
	 }// for
//       }
//#ifdef _OPENMP   // ############ 12LB g.s. (Low) ###############################
//#pragma omp section
//#endif
//       {
	 for (int i = nLambda+nSigma ;
	      i < nLambda+nSigma+nLB12gsl ;
	      i++ ){
	   if (gEvflag[i]==1201){
	     float EmomR=recfcn(cpe,i,gE,6);
	     float KmomR=recfcn(cpk,i,gK,6);
	     float ExptR=recfcn(cxe,i,gE,6);
	     float KxptR=recfcn(cxk,i,gK,6);
	     float EyptR=recfcn(cye,i,gE,6);
	     float KyptR=recfcn(cyk,i,gK,6);
	     float mmR=calcMM(gB[i],EmomR,-ExptR,-EyptR,KmomR,KxptR,KyptR,12);
	     //hgsl->Fill(mmR-ml);
	     //deltagsl = (mmR-ml+gsmass)/resMM2;
	     deltagsl = (mmR-gsmm)/resMM2; // Toshi
	     chisqgsl += deltagsl*deltagsl;
	     //meangsl+=mmR-ml+gsmass;
	     meangsl+=mmR-ml+gsmass;
	     gsl++;
	     //chisqpk += (KmomR-gmomK0[i])*(KmomR-gmomK0[i])/(gmomK0[i]*1e-3)/(gmomK0[i]*1e-3);
	     //chisqpe += (EmomR-gmomE0[i])*(EmomR-gmomE0[i])/(gmomE0[i]*1e-3)/(gmomE0[i]*1e-3);
	     /*if (mmR-ml<-0.01){
	       ngs++;
	       }*/
	   }
	 }
	 //       }
//       
//#ifdef _OPENMP   // ############ 12LB g.s. (High) ###############################
//#pragma omp section
//#endif
//       {
	 for (int i=nLambda+nSigma+nLB12gsl;
	      i<nLambda+nSigma+nLB12gsl+nLB12gsh;i++){
	   if (gEvflag[i]==1202){
	     float EmomR=recfcn(cpe,i,gE,6);
	     float KmomR=recfcn(cpk,i,gK,6);
	     float ExptR=recfcn(cxe,i,gE,6);
	     float KxptR=recfcn(cxk,i,gK,6);
	     float EyptR=recfcn(cye,i,gE,6);
	     float KyptR=recfcn(cyk,i,gK,6);
	     float mmR=calcMM(gB[i],EmomR,-ExptR,-EyptR,KmomR,KxptR,KyptR,12);
	     //hgsh->Fill(mmR-ml);
	     //deltagsh = (mmR-ml+gsmass)/resMM2/1.;
	     deltagsh = ( mmR - gsmm_high ) / resMM2;// Toshi , 5Feb2013
	     chisqgsh += deltagsh*deltagsh;
	     meangsh+=mmR-ml+gsmass;
	     gsh++;
	   }
	 }
//	 //}
////#ifdef _OPENMP   // ############ LAMBDA (H2O) ###############################
////#pragma omp section
////#endif
////       {
	 for (int i = nLambda+nSigma+nLB12gsl+nLB12gsh ;
	      i < nLambda+nSigma+nLB12gsl+nLB12gsh+nLambda_h2o;
	      i++ ){
	   if (gEvflag[i]==110){
	     float EmomR=recfcn(cpe,i,gE,6);
	     float KmomR=recfcn(cpk,i,gK,6);
	     float ExptR=recfcn(cxe,i,gE,6);
	     float KxptR=recfcn(cxk,i,gK,6);
	     float EyptR=recfcn(cye,i,gE,6);
	     float KyptR=recfcn(cyk,i,gK,6);
	     float mmR=calcMM(gB[i],EmomR,-ExptR,-EyptR,KmomR,KxptR,KyptR,101);
	     if ((mmR-ml)>0){
	       deltal_h2o = (mmR-ml)/resMM1/1.0;
	     }
	     else{
	       deltal_h2o = (mmR-ml)/resMM1;
	     }
	     chisql_h2o += deltal_h2o*deltal_h2o;
	     l_h2o++;
	   }
	 }
//       }
////#ifdef _OPENMP   // ############ 12LB p-shell (Low) ###############################
////#pragma omp section
////#endif
////       {
	 for (int i = nLambda+nSigma+nLB12gsl+nLB12gsh+nLambda_h2o ; 
	      i < nLambda+nSigma+nLB12gsl+nLB12gsh+nLambda_h2o+nLB12psl ; 
	      i++ ){
	   if (gEvflag[i]==1203){
	     float EmomR=recfcn(cpe,i,gE,6);
	     float KmomR=recfcn(cpk,i,gK,6);
	     float ExptR=recfcn(cxe,i,gE,6);
	     float KxptR=recfcn(cxk,i,gK,6);
	     float EyptR=recfcn(cye,i,gE,6);
	     float KyptR=recfcn(cyk,i,gK,6);
	     float mmR=calcMM(gB[i],EmomR,-ExptR,-EyptR,KmomR,KxptR,KyptR,12)+gdmm[i];
	     //deltapsl = (mmR-ml+gsmass-11.e-3)/sqrt(resMM2*resMM2);
	     deltapsl = ( mmR - psmm ) / resMM2; // Toshi
	     chisqpsl += deltapsl*deltapsl;
	     psl++;
	   }
	 }
//       }
//#ifdef _OPENMP   // ############ 12LB p-shell (High) ###############################
//#pragma omp section
//#endif
//       {
	 for (int i = nLambda+nSigma+nLB12gsl+nLB12gsh+nLambda_h2o+nLB12psl;
	      i < nLambda+nSigma+nLB12gsl+nLB12gsh+nLambda_h2o+nLB12psl+nLB12psh;
	      i++ ){
	   if (gEvflag[i]==1204){
	     float EmomR=recfcn(cpe,i,gE,6);
	     float KmomR=recfcn(cpk,i,gK,6);
	     float ExptR=recfcn(cxe,i,gE,6);
	     float KxptR=recfcn(cxk,i,gK,6);
	     float EyptR=recfcn(cye,i,gE,6);
	     float KyptR=recfcn(cyk,i,gK,6);
	     float mmR=calcMM(gB[i],EmomR,-ExptR,-EyptR,KmomR,KxptR,KyptR,12)+gdmm[i];
	     //deltapsh = (mmR-ml+gsmass-11.e-3)/sqrt(resMM2*resMM2);
	     deltapsh = ( mmR - psmm_high ) / resMM2 ; // Toshi , 5Feb2013
	     chisqpsh += deltapsh*deltapsh;
	     psh++;
	   }
	 }
////       }
////#ifdef _OPENMP   // ############ SIGMA (H2O) ###############################
////#pragma omp section
////#endif
////       {
	 for (int i = nLambda+nSigma+nLB12gsl+nLB12gsh+nLambda_h2o+nLB12psl+nLB12psh;
	      i < nLambda+nSigma+nLB12gsl+nLB12gsh+nLambda_h2o+nLB12psl+nLB12psh+nSigma_h2o;
	      i++ ){
	   if (gEvflag[i]==111){
	     float EmomR=recfcn(cpe,i,gE,6);
	     float KmomR=recfcn(cpk,i,gK,6);
	     float ExptR=recfcn(cxe,i,gE,6);
	     float KxptR=recfcn(cxk,i,gK,6);
	     float EyptR=recfcn(cye,i,gE,6);
	     float KyptR=recfcn(cyk,i,gK,6);
	     float mmR=calcMM(gB[i],EmomR,-ExptR,-EyptR,KmomR,KxptR,KyptR,101);
	     //hgsl->Fill(mmR-ml);
	     //deltagsl = (mmR-ml+gsmass)/resMM2;
	     deltas_h2o  = ( mmR-ms0 ) / resMM1; // Toshi
	     chisqs_h2o += deltas_h2o*deltas_h2o;
	     //meangsl+=mmR-ml+gsmass;
	     //meangsl+=mmR-ml+gsmass;
	     s_h2o++;
	     //chisqpk += (KmomR-gmomK0[i])*(KmomR-gmomK0[i])/(gmomK0[i]*1e-3)/(gmomK0[i]*1e-3);
	     //chisqpe += (EmomR-gmomE0[i])*(EmomR-gmomE0[i])/(gmomE0[i]*1e-3)/(gmomE0[i]*1e-3);
	     /*if (mmR-ml<-0.01){
	       ngs++;
	       }*/
	   }
	 }
////       }
////#ifdef _OPENMP   // ############ 12LB s-shell (CH2) ###############################
////#pragma omp section
////#endif
////       {
//	 for (int i = nLambda+nSigma+nLB12gsl+nLB12gsh+nLambda_h2o+nLB12psl+nLB12psh+nSigma_h2o;
//	      i < nLambda+nSigma+nLB12gsl+nLB12gsh+nLambda_h2o
//		+nLB12psl+nLB12psh+nSigma_h2o+nLB12gsch2 ;
//	      i++ ){
//	   if (gEvflag[i]==1205){
//	     float EmomR=recfcn(cpe,i,gE,6);
//	     float KmomR=recfcn(cpk,i,gK,6);
//	     float ExptR=recfcn(cxe,i,gE,6);
//	     float KxptR=recfcn(cxk,i,gK,6);
//	     float EyptR=recfcn(cye,i,gE,6);
//	     float KyptR=recfcn(cyk,i,gK,6);
//	     float mmR=calcMM(gB[i],EmomR,-ExptR,-EyptR,KmomR,KxptR,KyptR,112)+gdmm[i];
//	     //deltapsh = (mmR-ml+gsmass-11.e-3)/sqrt(resMM2*resMM2);
//	     deltagsch2 = ( mmR - gsmm_ch2 ) / resMM2 ; // Toshi , 5Feb2013
//	     chisqgsch2 += deltagsch2*deltagsch2;
//	     gsch2++;
//	   }
//	 }
////       }
////#ifdef _OPENMP   // ############ 10LBe s-shell ###############################
////#pragma omp section
////#endif
////       {
	 for (int i = nLambda+nSigma+nLB12gsl+nLB12gsh
		+nLambda_h2o+nLB12psl+nLB12psh+nSigma_h2o
		+nLB12gsch2;
	      i < nLambda+nSigma+nLB12gsl+nLB12gsh+nLambda_h2o
		+nLB12psl+nLB12psh+nSigma_h2o+nLB12gsch2+nLBe10gs;
	      i++ ){
	   if (gEvflag[i]==1000){
	     float EmomR=recfcn(cpe,i,gE,6);
	     float KmomR=recfcn(cpk,i,gK,6);
	     float ExptR=recfcn(cxe,i,gE,6);
	     float KxptR=recfcn(cxk,i,gK,6);
	     float EyptR=recfcn(cye,i,gE,6);
	     float KyptR=recfcn(cyk,i,gK,6);
	     float mmR=calcMM(gB[i],EmomR,-ExptR,-EyptR,KmomR,KxptR,KyptR,10)+gdmm[i];
	     //deltapsh = (mmR-ml+gsmass-11.e-3)/sqrt(resMM2*resMM2);
	     deltagsb10 = ( mmR - gsmm_b10 ) / resMM2 ; // Toshi , 5Feb2013
	     chisqgsb10 += deltagsb10*deltagsb10;
	     gsb10++;
	   }// if 
	 }// for
//       }// section
//#ifdef _OPENMP   // ############ 7LHe s-shell ###############################
//#pragma omp section
//#endif
//       {
	 for (int i=nLambda+nSigma+nLB12gsl
		+nLB12gsh+nLambda_h2o+nLB12psl
		+nLB12psh+nSigma_h2o+nLB12gsch2+nLBe10gs;
	      i < nLambda+nSigma+nLB12gsl+nLB12gsh
		+nLambda_h2o+nLB12psl+nLB12psh
		+nSigma_h2o+nLB12gsch2+nLBe10gs+nLHe7gs;
	      i++ ){
	   if (gEvflag[i]==700){
	     float EmomR=recfcn(cpe,i,gE,6);
	     float KmomR=recfcn(cpk,i,gK,6);
	     float ExptR=recfcn(cxe,i,gE,6);
	     float KxptR=recfcn(cxk,i,gK,6);
	     float EyptR=recfcn(cye,i,gE,6);
	     float KyptR=recfcn(cyk,i,gK,6);
	     float mmR=calcMM(gB[i],EmomR,-ExptR,-EyptR,KmomR,KxptR,KyptR,7)+gdmm[i];
	     //deltapsh = (mmR-ml+gsmass-11.e-3)/sqrt(resMM2*resMM2);
	     deltagsli7 = ( mmR - gsmm_li7 ) / resMM2 ; // Toshi , 5Feb2013
	     chisqgsli7 += deltagsli7*deltagsli7;
	     gsli7++;
	   }// if
	 }// for
//       }//section
//#ifdef _OPENMP   // ############ 12LB core excited state1 (low) ###############################
//#pragma omp section
//#endif
//       {
//	 for (int i=nLambda+nSigma+nLB12gsl
//		+nLB12gsh+nLambda_h2o+nLB12psl
//		+nLB12psh+nSigma_h2o+nLB12gsch2
//		+nLBe10gs+nLHe7gs;
//	      i < nLambda+nSigma+nLB12gsl+nLB12gsh
//		+nLambda_h2o+nLB12psl+nLB12psh
//		+nSigma_h2o+nLB12gsch2+nLBe10gs
//		+nLHe7gs+nLB12cel;
//	      i++ ){
//	   if (gEvflag[i]==1290){
//	     float EmomR=recfcn(cpe,i,gE,6);
//	     float KmomR=recfcn(cpk,i,gK,6);
//	     float ExptR=recfcn(cxe,i,gE,6);
//	     float KxptR=recfcn(cxk,i,gK,6);
//	     float EyptR=recfcn(cye,i,gE,6);
//	     float KyptR=recfcn(cyk,i,gK,6);
//	     float mmR=calcMM(gB[i],EmomR,-ExptR,-EyptR,KmomR,KxptR,KyptR,12)+gdmm[i];
//	     deltacel = ( mmR - cemm ) / resMM2 ;
//	     chisqcel += deltacel * deltacel;
//	     cel++;
//	   }// if
//	 }// for
	 //       }//section
//#ifdef _OPENMP   // ############ 12LB core excited state1 (high) ###############################
//#pragma omp section
//#endif
//       {
//	 for (int i=nLambda+nSigma+nLB12gsl
//		+nLB12gsh+nLambda_h2o+nLB12psl
//		+nLB12psh+nSigma_h2o+nLB12gsch2
//		+nLBe10gs+nLHe7gs+nLB12cel;
//	      i < nLambda+nSigma+nLB12gsl+nLB12gsh
//		+nLambda_h2o+nLB12psl+nLB12psh
//		+nSigma_h2o+nLB12gsch2+nLBe10gs
//		+nLHe7gs+nLB12cel+nLB12ceh;
//	      i++ ){
//	   if (gEvflag[i]==1291){
//	     float EmomR=recfcn(cpe,i,gE,6);
//	     float KmomR=recfcn(cpk,i,gK,6);
//	     float ExptR=recfcn(cxe,i,gE,6);
//	     float KxptR=recfcn(cxk,i,gK,6);
//	     float EyptR=recfcn(cye,i,gE,6);
//	     float KyptR=recfcn(cyk,i,gK,6);
//	     float mmR=calcMM(gB[i],EmomR,-ExptR,-EyptR,KmomR,KxptR,KyptR,12)+gdmm[i];
//	     deltaceh = ( mmR - cemm_high ) / resMM2 ;
//	     chisqceh += deltaceh * deltaceh;
//	     ceh++;
//	   }// if
//	 }// for

//       }//section
//     }//sections
//#ifdef _OPENMP   // ############ 52LV (1) ###############################
//#pragma omp section
//#endif
//       {
//	 for (int i=nLambda+nSigma+nLB12gsl
//		+nLB12gsh+nLambda_h2o+nLB12psl
//		+nLB12psh+nSigma_h2o+nLB12gsch2
//		+nLBe10gs+nLHe7gs+nLB12cel+nLB12ceh;
//	      i < nLambda+nSigma+nLB12gsl+nLB12gsh
//		+nLambda_h2o+nLB12psl+nLB12psh
//		+nSigma_h2o+nLB12gsch2+nLBe10gs
//		+nLHe7gs+nLB12cel+nLB12ceh+nLV52_1;
//	      i++ ){
//	   if (gEvflag[i]==5200){
//	     float EmomR=recfcn(cpe,i,gE,6);
//	     float KmomR=recfcn(cpk,i,gK,6);
//	     float ExptR=recfcn(cxe,i,gE,6);
//	     float KxptR=recfcn(cxk,i,gK,6);
//	     float EyptR=recfcn(cye,i,gE,6);
//	     float KyptR=recfcn(cyk,i,gK,6);
//	     float mmR=calcMM(gB[i],EmomR,-ExptR,-EyptR,KmomR,KxptR,KyptR,52)+gdmm[i];
//	     deltacr52_1 = ( mmR - mm1_cr52 ) / resMM2 ;
//	     chisqcr52_1 += deltacr52_1*deltacr52_1;
//	     cr52_1++;
//	     //cout << mmR -1.115683<< endl;
//	   }// if
//	 }// for

       }//section
     }//sections
   }//parallel
   //TF1 *fl = new TF1("fl", "[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",1.112,1.118);
   //TF1 *fs = new TF1("fs","[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",1.189,1.195);
   //TF1 *fgsl = new TF1("fgsl","[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",-0.015,-0.009);
   //TF1 *fgsh = new TF1("fgsh","[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",-0.015,-0.009);
   /*fl->SetParameters(100,1.115,1e-3,0.,10.);
   fl->SetParLimits(0,0.,500.);
   fl->SetParLimits(1,1.110,1.120);
   fl->SetParLimits(2,3e-4,3e-3);
      }//sections
   }//parallel
   //TF1 *fl = new TF1("fl", "[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",1.112,1.118);
   //TF1 *fs = new TF1("fs","[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",1.189,1.195);
   TF1 *fgsl = new TF1("fgsl","[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",-0.015,-0.009);
   TF1 *fgsh = new TF1("fgsh","[0]*exp(-pow((x-[1])/[2],2)/2)+[3]",-0.015,-0.009);
   fl->SetParameters(100,1.115,1e-3,0.,10.);
   fl->SetParLimits(0,0.,500.);
   fl->SetParLimits(1,1.110,1.120);
   fl->SetParLimits(2,3e-4,3e-3);
   fl->SetParLimits(3,0,20);
   fs->SetParameters(100,1.192,1e-3,0.,2.);
   fs->SetParLimits(0,0.,100.);
   fs->SetParLimits(1,1.187,1.197);
   fs->SetParLimits(2,5e-4,3e-3);
   fs->SetParLimits(3,0,5);*/
   /*fgsl->SetParameters(10,-0.0115,1e-3,0.);
   fgsl->SetParLimits(0,0.,100.);
   fgsl->SetParLimits(1,-0.012,-0.010);
   fgsl->SetParLimits(2,1e-4,3e-3);
   fgsl->SetParLimits(3,1.,2.);
   fgsh->SetParameters(10,-0.0115,5e-4,10.);
   fgsh->SetParLimits(0,5.,20.);
   fgsh->SetParLimits(1,-0.012,-0.010);
   fgsh->SetParLimits(2,1e-4,2e-3);
   fgsh->SetParLimits(3,10.,12.);
   //hl->Fit("fl","0Q");
   //hs->Fit("fs","0Q");
   hgsl->Fit("fgsl","0Q","",-0.013,-0.010);
   hgsh->Fit("fgsh","0Q","",-0.013,-0.010);
   float al=fl->GetParameter(0);
   float mul=fl->GetParameter(1);
   float sigmal=fl->GetParameter(2);
   float chil=fl->GetChisquare();
   float as=fs->GetParameter(0);
   float mus=fs->GetParameter(1);
   float sigmas=fs->GetParameter(2);
   float chis=fs->GetChisquare();
   float agsl=fgsl->GetParameter(0);
   float mugsl=fgsl->GetParameter(1);
   float sigmagsl=fgsl->GetParameter(2);
   float chigsl=fgsl->GetChisquare();
   float agsh=fgsh->GetParameter(0);
   float mugsh=fgsh->GetParameter(1);
   float sigmagsh=fgsh->GetParameter(2);
   float chigsh=fgsh->GetChisquare();*/

   /*cout <<"sigmal "<< sigmal <<endl;
   cout <<"sigmas "<< sigmas <<endl;
   cout <<"sigmagsl "<< sigmagsl <<endl;
   cout <<"sigmagsh "<< sigmagsh <<endl;
   cout <<"chil "<< chil <<endl;
   cout <<"chis "<< chis <<endl;
   cout <<"chigsl "<< chigsl <<endl;
   cout <<"chigsh "<< chigsh <<endl;*/

   /*delete hl;
   delete hs;
   delete hgsl;
   delete hgsh;
   //delete fl;
   //delete fs;
   delete fgsl;
   delete fgsh;*/
      
   /*cout <<"lambda"<<chisql/l<<endl;
   cout <<"sigma"<<chisqs/s<<endl;
   cout <<"gsl"<<chisqgsl/gsl<<endl;
   cout <<"gsh"<<chisqgsh/gsh<<endl;
   cout <<"psl"<<chisqpsl/psl<<endl;
   cout <<"psh"<<chisqpsh/psh<<endl;*/
   //cout <<sigmagsl/resMM2<<" "<<sigmagsh/resMM2<<endl;
   //cout <<l<<" "<<s<<" "<<gs<<endl;
   //cout <<w_chisql<<" "<<w_chisqs<<" "<<w_chisqgsl<<endl;
   //f=chisql/l*w_chisql+chisqs/s*w_chisqs+chisqgs/gs*w_chisqgsl;
   //cout<<"HKS"<<meanLkx[0]/nLkx[0]<<" "<<meanLkx[1]/nLkx[1]<<" "<<meanLkx[2]/nLkx[2]<<endl;
   //cout<<"HES"<<meanLex[0]/nLex[0]<<" "<<meanLex[1]/nLex[1]<<" "<<meanLex[2]/nLex[2]<<endl;
   /*cout<<"meanl="<<meanl/l/5e-5<<endl;
   cout<<"means="<<means/s/5e-5<<endl;*/

   //f=chisql/l*w_chisql+chisqs/s*w_chisqs+
   // ==========  Original ==============
   f=0.;

//   for (int i=0;i<42;i++){
//      if(nL[i]!=0){
//         f+=chisqL[i]/nL[i]*w_chisql/42.;
//      }
//   }
//   f+=chisqs/s*w_chisqs+
//      chisqgsl/gsl*w_chisqgsl+
//      chisqgsh/gsh*w_chisqgsh+
//      chisql_h2o/l_h2o*w_chisql_h2o;

   // Toshi version ============
   if (nL_!=0 && w_chisql!=0)   f += chisqL_/nL_  * w_chisql;
   if (s!=0 && w_chisqs!=0)     f += chisqs/s     * w_chisqs;
   if (gsl!=0 && w_chisqgsl!=0) f += chisqgsl/gsl * w_chisqgsl;
   if (psl!=0 && w_chisqpk!=0)  f += chisqpsl/psl * w_chisqpk;
   if (gsh!=0 && w_chisqgsh!=0) f += chisqgsh/gsh * w_chisqgsh;
   if (psh!=0 && w_chisqpe!=0)  f += chisqpsh/psh * w_chisqpe;
   if (l_h2o!=0 && w_chisql_h2o!=0)  f += chisql_h2o/l_h2o * w_chisql_h2o;
   if (s_h2o!=0 && w_chisqs_h2o!=0)  f += chisqs_h2o/s_h2o * w_chisqs_h2o;
   if (gsch2!=0 && w_chisqgs_ch2!=0) f += chisqgsch2/gsch2 * w_chisqgs_ch2;
   if (gsb10!=0 && w_chisqgs_b10!=0) f += chisqgsb10/gsb10 * w_chisqgs_b10;
   if (gsli7!=0 && w_chisqgs_li7!=0) f += chisqgsli7/gsli7 * w_chisqgs_li7;
   if (cel!=0 && w_chisqcel!=0) f += chisqcel/cel * w_chisqcel;
   if (ceh!=0 && w_chisqceh!=0) f += chisqceh/ceh * w_chisqceh;
   if (cr52_1!=0 && w_chisq_cr52!=0) f += chisqcr52_1/cr52_1 * w_chisq_cr52;
   
   // ~~~~~~~~~~~ 21Nov2013 , Toshi ~~~~~~~~~~~~~~~^
   //f= f/12.0;
   //f = f/7.2;
   //f = f/10.0;
   f = f/9.3;

   /*f+=fabs(meanLkx[0]/nLkx[0]-meanLkx[1]/nLkx[1])/50e-6+
      fabs(meanLkx[1]/nLkx[1]-meanLkx[2]/nLkx[2])/50e-6+
      fabs(meanLkx[2]/nLkx[2]-meanLkx[0]/nLkx[0])/50e-6+
      fabs(meanLex[0]/nLex[0]-meanLex[1]/nLex[1])/50e-6+
      fabs(meanLex[1]/nLex[1]-meanLex[2]/nLex[2])/50e-6+
      fabs(meanLex[2]/nLex[2]-meanLex[0]/nLex[0])/50e-6;*/
   //w_chisql_h2o=0.5; 
   /*if (chigsh<200){
      f=chisql/l*w_chisql+chisqs/s*w_chisqs+
         chisqgsl/gsl*w_chisqgsl+
         chisqgsh/gsh*w_chisqgsh+
         chisql_h2o/l_h2o*w_chisql_h2o+
         //chisqps/ps*w_chisqgsl+
         (sigmagsh/resMM2+(mugsl-mugsh)*(mugsl-mugsh)/1e-4/1e-4)*w_chisqpe;
   }
   else{
      f=chisql/l*w_chisql+chisqs/s*w_chisqs+
         chisqgsl/gsl*w_chisqgsl+
         chisqgsh/gsh*w_chisqgsh+
         chisql_h2o/l_h2o*w_chisql_h2o;
         //chisqps/ps*w_chisqgsl+
   }*/
      //sigmagsl/resMM2*w_chisqpk+sigmagsh/resMM2*w_chisqpe;
}

void fcn2(int &npar, double*gin, double&f,double*par, int iflag)
{
   float chisql     = 0.0;
   float chisql_h2o = 0.0;
   float chisqs_h2o = 0.0;
   float chisqs     = 0.0;
   float chisqgsl   = 0.0;
   float chisqgsh   = 0.0;
   float chisqpsl   = 0.0;
   float chisqpsh   = 0.0;
   float chisqgsch2 = 0.0; // Toshi , 28Feb2013
   float chisqgsb10 = 0.0; // Toshi , 12June2013
   float chisqgsli7 = 0.0; // Toshi , 14Aug2013
   float chisqpe    = 0.0;
   float chisqpk    = 0.0;
   float chisqxe    = 0.0;
   float chisqye    = 0.0;
   float chisqxk    = 0.0;
   float chisqyk    = 0.0;
   float deltal     = 0.0;
   float deltal_h2o = 0.0;
   float deltas_h2o = 0.0;
   float deltas     = 0.0;
   float deltagsl   = 0.0;
   float deltagsh   = 0.0;
   float deltapsl   = 0.0;
   float deltapsh   = 0.0;
   float deltagsch2 = 0.0; // Toshi , 28Feb2013
   float deltagsb10 = 0.0; // Toshi , 12June2013
   float deltagsli7 = 0.0; // Toshi , 14Aug2013
   float deltape    = 0.0;
   float deltapk    = 0.0;
   float deltaxe    = 0.0;
   float deltaye    = 0.0;
   float deltaxk    = 0.0;
   float deltayk    = 0.0;
   float meanl      = 0.0;
   float means      = 0.0;
   float meangsl    = 0.0;
   float meangsh    = 0.0;
   double cpe[nParam];
   double cpk[nParam];
   double cxe[nParam];
   double cxk[nParam];
   double cye[nParam];
   double cyk[nParam];
   float deltaL[42];
   float chisqL[42];
   float deltaL_=0.0;
   float chisqL_=0.0;
   int nL_=0;
   int nL[42];
   int iL=0;
   //float mmR=0.;
   int l    =1;
   int l_h2o=1;
   int s_h2o=1;
   int s    =1;
   int gsl  =1;
   int gsh  =1;
   int psl  =1;
   int psh  =1;
   int gsch2=1;
   int gsb10=1;
   int gsli7=1;
   int SSxe =1;
   int SSye =1;
   int SSxk =1;
   int SSyk =1;
   int ngs=0;
   /*TH1F *hl = new TH1F("hl","",100,1.112, 1.118);
   TH1F *hs = new TH1F("hs","",100,1.189, 1.195);
   TH1F *hgsl = new TH1F("hgsl","",100,-0.015, -0.009);
   TH1F *hgsh = new TH1F("hgsh","",100,-0.015, -0.009);*/

   for (int i=0;i<42;i++){
      deltaL[i]=0.;
      chisqL[i]=0.;
      nL[i]=0;
   }


   f=0;
   for (int i=0;i<nParam;i++){
      int i0=i;
      int i1=i+nParam;
      int i2=i+nParam*2;
      int i3=i+nParam*3;
      int i4=i+nParam*4;
      int i5=i+nParam*5;
      cpe[i]=par[i0];
      cpk[i]=par[i1];
      cxe[i]=par[i2];
      cxk[i]=par[i3];
      cye[i]=par[i4];
      cyk[i]=par[i5];
   }
#ifdef _OPENMP   
#pragma omp parallel 
#endif
   {
#ifdef _OPENMP   
#pragma omp sections
#endif
     {
#ifdef _OPENMP   // ############ LAMBDA (CH2) ###############################
#pragma omp section
#endif
       {
	 for (int i=0;i<nLambda;i++){
	   //if (FCNFlag==1 && EOFlag==0){
	   if (gEvflag[i]==100){
	     float EmomR=recfcn(cpe,i,gE,6);
                  float KmomR=recfcn(cpk,i,gK,6);
                  float ExptR=recfcn(cxe,i,gE,6);
                  float KxptR=recfcn(cxk,i,gK,6);
                  float EyptR=recfcn(cye,i,gE,6);
                  float KyptR=recfcn(cyk,i,gK,6);
                  /*EmomR-=gdmomE[i];
		    ExptR-=gdxptE[i];
		    EyptR-=gdyptE[i];
		    KmomR-=gdmomK[i];
		    KxptR-=gdxptK[i];
		    KyptR-=gdyptK[i];*/
                  float mmR=calcMM(gB[i],EmomR,-ExptR,-EyptR,KmomR,KxptR,KyptR,1)+gdmm[i];
                  //hl->Fill(mmR);
                  //HKS momentum
		  
		  // === By T.Gogami === //
		  deltaL_  = ( mmR - ml ) / resMM1;
                  chisqL_ += deltaL_ * deltaL_;
                  nL_++;
		  
                  meanl+=mmR-ml;
                  l++;
		  
	   }
	 }
       }
#ifdef _OPENMP   // ############ SIGMA (CH2) ###############################
#pragma omp section
#endif
       {
	 for (int i=nLambda;i<nLambda+nSigma;i++){
	   if (gEvflag[i]==101){
	     float EmomR=recfcn(cpe,i,gE,6);
                  float KmomR=recfcn(cpk,i,gK,6);
                  float ExptR=recfcn(cxe,i,gE,6);
                  float KxptR=recfcn(cxk,i,gK,6);
                  float EyptR=recfcn(cye,i,gE,6);
                  float KyptR=recfcn(cyk,i,gK,6);
                  /*EmomR-=gdmomE[i];
                  ExptR-=gdxptE[i];
                  EyptR-=gdyptE[i];
                  KmomR-=gdmomK[i];
                  KxptR-=gdxptK[i];
                  KyptR-=gdyptK[i];*/
                  float mmR=calcMM(gB[i],EmomR,-ExptR,-EyptR,KmomR,KxptR,KyptR,1)+gdmm[i];
                  //hs->Fill(mmR);
                  if ((mmR-ms0)>0){
                     deltas = (mmR-(ms0+.0e-4))/resMM1;
                  }
                  else{
		    deltas = (mmR-(ms0+.0e-4))/resMM1;
                  }
                  chisqs += deltas*deltas;
                  means+=mmR-(ms0+.0e-4);
                  s++;
                  //chisqpk += (KmomR-gmomK0[i])*(KmomR-gmomK0[i])/(1.2e-3)/(1.2e-3);
                  //chisqpe += (EmomR-gmomE0[i])*(EmomR-gmomE0[i])/(0.84e-3)/(0.84e-3);
	   }
	 }
	 //}
//#ifdef _OPENMP   // ############ 12LB g.s. (Low) ###############################
//#pragma omp section
//#endif
//       {
//	 for (int i = nLambda+nSigma ;
//	      i < nLambda+nSigma+nLB12gsl ;
//	      i++ ){
//	   if (gEvflag[i]==1201){
//	     float EmomR=recfcn(cpe,i,gE,6);
//	     float KmomR=recfcn(cpk,i,gK,6);
//	     float ExptR=recfcn(cxe,i,gE,6);
//	     float KxptR=recfcn(cxk,i,gK,6);
//	     float EyptR=recfcn(cye,i,gE,6);
//	     float KyptR=recfcn(cyk,i,gK,6);
//	     float mmR=calcMM(gB[i],EmomR,-ExptR,-EyptR,KmomR,KxptR,KyptR,12);
//	     //hgsl->Fill(mmR-ml);
//	     //deltagsl = (mmR-ml+gsmass)/resMM2;
//	     deltagsl = (mmR-gsmm)/resMM2; // Toshi
//	     chisqgsl += deltagsl*deltagsl;
//	     //meangsl+=mmR-ml+gsmass;
//	     meangsl+=mmR-ml+gsmass;
//	     gsl++;
//	     //chisqpk += (KmomR-gmomK0[i])*(KmomR-gmomK0[i])/(gmomK0[i]*1e-3)/(gmomK0[i]*1e-3);
//	     //chisqpe += (EmomR-gmomE0[i])*(EmomR-gmomE0[i])/(gmomE0[i]*1e-3)/(gmomE0[i]*1e-3);
//	     /*if (mmR-ml<-0.01){
//	       ngs++;
//	       }*/
//	   }
//	 }
//       }
//       
//#ifdef _OPENMP   // ############ 12LB g.s. (High) ###############################
//#pragma omp section
//#endif
//       {
//	 for (int i=nLambda+nSigma+nLB12gsl;
//	      i<nLambda+nSigma+nLB12gsl+nLB12gsh;i++){
//	   if (gEvflag[i]==1202){
//	     float EmomR=recfcn(cpe,i,gE,6);
//	     float KmomR=recfcn(cpk,i,gK,6);
//	     float ExptR=recfcn(cxe,i,gE,6);
//	     float KxptR=recfcn(cxk,i,gK,6);
//	     float EyptR=recfcn(cye,i,gE,6);
//	     float KyptR=recfcn(cyk,i,gK,6);
//	     float mmR=calcMM(gB[i],EmomR,-ExptR,-EyptR,KmomR,KxptR,KyptR,12);
//	     //hgsh->Fill(mmR-ml);
//	     //deltagsh = (mmR-ml+gsmass)/resMM2/1.;
//	     deltagsh = ( mmR - gsmm_high ) / resMM2;// Toshi , 5Feb2013
//	     chisqgsh += deltagsh*deltagsh;
//	     meangsh+=mmR-ml+gsmass;
//	     gsh++;
//	   }
//	 }
//       }
//#ifdef _OPENMP   // ############ LAMBDA (H2O) ###############################
//#pragma omp section
//#endif
//       {
	 for (int i = nLambda+nSigma+nLB12gsl+nLB12gsh ;
	      i < nLambda+nSigma+nLB12gsl+nLB12gsh+nLambda_h2o;
	      i++ ){
	   if (gEvflag[i]==110){
	     float EmomR=recfcn(cpe,i,gE,6);
	     float KmomR=recfcn(cpk,i,gK,6);
	     float ExptR=recfcn(cxe,i,gE,6);
	     float KxptR=recfcn(cxk,i,gK,6);
	     float EyptR=recfcn(cye,i,gE,6);
	     float KyptR=recfcn(cyk,i,gK,6);
	     float mmR=calcMM(gB[i],EmomR,-ExptR,-EyptR,KmomR,KxptR,KyptR,101);
	     //if ((mmR-ml)>0){
	     //  deltal_h2o = (mmR-ml)/resMM1;
	     //}
	     //else{
	     //  deltal_h2o = (mmR-ml)/resMM1;
	     //}
	     deltal_h2o = (mmR-ml)/resMM1;
	     chisql_h2o += deltal_h2o*deltal_h2o;
	     l_h2o++;
	   }
	 }
//       }
//#ifdef _OPENMP   // ############ 12LB p-shell (Low) ###############################
//#pragma omp section
//#endif
//       {
//	 for (int i = nLambda+nSigma+nLB12gsl+nLB12gsh+nLambda_h2o ; 
//	      i < nLambda+nSigma+nLB12gsl+nLB12gsh+nLambda_h2o+nLB12psl ; 
//	      i++ ){
//	   if (gEvflag[i]==1203){
//	     float EmomR=recfcn(cpe,i,gE,6);
//	     float KmomR=recfcn(cpk,i,gK,6);
//	     float ExptR=recfcn(cxe,i,gE,6);
//	     float KxptR=recfcn(cxk,i,gK,6);
//	     float EyptR=recfcn(cye,i,gE,6);
//	     float KyptR=recfcn(cyk,i,gK,6);
//	     float mmR=calcMM(gB[i],EmomR,-ExptR,-EyptR,KmomR,KxptR,KyptR,12)+gdmm[i];
//	     //deltapsl = (mmR-ml+gsmass-11.e-3)/sqrt(resMM2*resMM2);
//	     deltapsl = ( mmR - psmm ) / resMM2; // Toshi
//	     chisqpsl += deltapsl*deltapsl;
//	     psl++;
//	   }
//	 }
//       }
//#ifdef _OPENMP   // ############ 12LB p-shell (High) ###############################
//#pragma omp section
//#endif
//       {
//	 for (int i = nLambda+nSigma+nLB12gsl+nLB12gsh+nLambda_h2o+nLB12psl;
//	      i < nLambda+nSigma+nLB12gsl+nLB12gsh+nLambda_h2o+nLB12psl+nLB12psh;
//	      i++ ){
//	   if (gEvflag[i]==1204){
//	     float EmomR=recfcn(cpe,i,gE,6);
//	     float KmomR=recfcn(cpk,i,gK,6);
//	     float ExptR=recfcn(cxe,i,gE,6);
//	     float KxptR=recfcn(cxk,i,gK,6);
//	     float EyptR=recfcn(cye,i,gE,6);
//	     float KyptR=recfcn(cyk,i,gK,6);
//	     float mmR=calcMM(gB[i],EmomR,-ExptR,-EyptR,KmomR,KxptR,KyptR,12)+gdmm[i];
//	     //deltapsh = (mmR-ml+gsmass-11.e-3)/sqrt(resMM2*resMM2);
//	     deltapsh = ( mmR - psmm_high ) / resMM2 ; // Toshi , 5Feb2013
//	     chisqpsh += deltapsh*deltapsh;
//	     psh++;
//	   }
//	 }
//       }
//#ifdef _OPENMP   // ############ SIGMA (H2O) ###############################
//#pragma omp section
//#endif
//	 {
	 for (int i = nLambda+nSigma+nLB12gsl+nLB12gsh+nLambda_h2o+nLB12psl+nLB12psh;
	      i < nLambda+nSigma+nLB12gsl+nLB12gsh+nLambda_h2o+nLB12psl+nLB12psh+nSigma_h2o;
	      i++ ){
	   if (gEvflag[i]==111){
	     float EmomR=recfcn(cpe,i,gE,6);
	     float KmomR=recfcn(cpk,i,gK,6);
	     float ExptR=recfcn(cxe,i,gE,6);
	     float KxptR=recfcn(cxk,i,gK,6);
	     float EyptR=recfcn(cye,i,gE,6);
	     float KyptR=recfcn(cyk,i,gK,6);
	     float mmR=calcMM(gB[i],EmomR,-ExptR,-EyptR,KmomR,KxptR,KyptR,101);
	     //hgsl->Fill(mmR-ml);
	     //deltagsl = (mmR-ml+gsmass)/resMM2;
	     deltas_h2o  = ( mmR-ms0 ) / resMM1; // Toshi
	     chisqs_h2o += deltas_h2o*deltas_h2o;
	     //meangsl+=mmR-ml+gsmass;
	     //meangsl+=mmR-ml+gsmass;
	     s_h2o++;
	     //chisqpk += (KmomR-gmomK0[i])*(KmomR-gmomK0[i])/(gmomK0[i]*1e-3)/(gmomK0[i]*1e-3);
	     //chisqpe += (EmomR-gmomE0[i])*(EmomR-gmomE0[i])/(gmomE0[i]*1e-3)/(gmomE0[i]*1e-3);
	     /*if (mmR-ml<-0.01){
	       ngs++;
	       }*/
	   } // if
	 } // for
//       }
//#ifdef _OPENMP   // ############ 12LB s-shell (CH2) ###############################
//#pragma omp section
//#endif
//       {
//	 for (int i = nLambda+nSigma+nLB12gsl+nLB12gsh+nLambda_h2o+nLB12psl+nLB12psh+nSigma_h2o;
//	      i < nLambda+nSigma+nLB12gsl+nLB12gsh+nLambda_h2o
//		+nLB12psl+nLB12psh+nSigma_h2o+nLB12gsch2 ;
//	      i++ ){
//	   if (gEvflag[i]==1205){
//	     float EmomR=recfcn(cpe,i,gE,6);
//	     float KmomR=recfcn(cpk,i,gK,6);
//	     float ExptR=recfcn(cxe,i,gE,6);
//	     float KxptR=recfcn(cxk,i,gK,6);
//	     float EyptR=recfcn(cye,i,gE,6);
//	     float KyptR=recfcn(cyk,i,gK,6);
//	     float mmR=calcMM(gB[i],EmomR,-ExptR,-EyptR,KmomR,KxptR,KyptR,112)+gdmm[i];
//	     //deltapsh = (mmR-ml+gsmass-11.e-3)/sqrt(resMM2*resMM2);
//	     deltagsch2 = ( mmR - gsmm_ch2 ) / resMM2 ; // Toshi , 5Feb2013
//	     chisqgsch2 += deltagsch2*deltagsch2;
//	     gsch2++;
//	   }
//	 }
//       }
//#ifdef _OPENMP   // ############ 10LBe s-shell ###############################
//#pragma omp section
//#endif
//       {
//	 for (int i = nLambda+nSigma+nLB12gsl
//		+nLB12gsh+nLambda_h2o+nLB12psl
//		+nLB12psh+nSigma_h2o+nLB12gsch2;
//	      i < nLambda+nSigma+nLB12gsl+nLB12gsh
//		+nLambda_h2o+nLB12psl+nLB12psh
//		+nSigma_h2o+nLB12gsch2+nLBe10gs;
//	      i++ ){
//	   if (gEvflag[i]==1000){
//	     float EmomR=recfcn(cpe,i,gE,6);
//	     float KmomR=recfcn(cpk,i,gK,6);
//	     float ExptR=recfcn(cxe,i,gE,6);
//	     float KxptR=recfcn(cxk,i,gK,6);
//	     float EyptR=recfcn(cye,i,gE,6);
//	     float KyptR=recfcn(cyk,i,gK,6);
//	     float mmR=calcMM(gB[i],EmomR,-ExptR,-EyptR,KmomR,KxptR,KyptR,10)+gdmm[i];
//	     //deltapsh = (mmR-ml+gsmass-11.e-3)/sqrt(resMM2*resMM2);
//	     deltagsb10 = ( mmR - gsmm_b10 ) / resMM2 ; // Toshi , 5Feb2013
//	     chisqgsb10 += deltagsb10*deltagsb10;
//	     gsb10++;
//	   }
//	 }
//       }
//
//#ifdef _OPENMP   // ############ 7LHe s-shell ###############################
//#pragma omp section
//#endif
//       {
//	 for (int i=nLambda+nSigma+nLB12gsl
//		+nLB12gsh+nLambda_h2o+nLB12psl
//		+nLB12psh+nSigma_h2o+nLB12gsch2+nLBe10gs;
//	      i < nLambda+nSigma+nLB12gsl+nLB12gsh
//		+nLambda_h2o+nLB12psl+nLB12psh
//		+nSigma_h2o+nLB12gsch2+nLBe10gs+nLHe7gs;
//	      i++ ){
//	   if (gEvflag[i]==700){
//	     float EmomR=recfcn(cpe,i,gE,6);
//	     float KmomR=recfcn(cpk,i,gK,6);
//	     float ExptR=recfcn(cxe,i,gE,6);
//	     float KxptR=recfcn(cxk,i,gK,6);
//	     float EyptR=recfcn(cye,i,gE,6);
//	     float KyptR=recfcn(cyk,i,gK,6);
//	     float mmR=calcMM(gB[i],EmomR,-ExptR,-EyptR,KmomR,KxptR,KyptR,7)+gdmm[i];
//	     //deltapsh = (mmR-ml+gsmass-11.e-3)/sqrt(resMM2*resMM2);
//	     deltagsli7 = ( mmR - gsmm_li7 ) / resMM2 ; // Toshi , 5Feb2013
//	     chisqgsli7 += deltagsli7*deltagsli7;
//	     gsli7++;
//	     
//	   }// if
//	 }// for
       }//section
       
     }//sections
   }//parallel

   f = 0.0;
   // Toshi version ============
   if(nL_!=0 && w_chisql!=0) f += chisqL_/nL_  * w_chisql;
   if(s  !=0 && w_chisqs!=0) f += chisqs/s     * w_chisqs;
   //f += chisqgsl/gsl * w_chisqgsl;
   //f += chisqpsl/psl * w_chisqpk;
   //f += chisqgsh/gsh * w_chisqgsh;
   //f += chisqpsh/psh * w_chisqpe;
   if(l_h2o!=0 && w_chisql_h2o!=0 ) f += chisql_h2o/l_h2o * w_chisql_h2o;
   if(s_h2o!=0 && w_chisqs_h2o!=0 ) f += chisqs_h2o/s_h2o * w_chisqs_h2o;
   //f += chisqgsch2/gsch2 * w_chisqgs_ch2;
   //if(gsb10!=0 && w_chisqgs_b10!=0) f += chisqgsb10/gsb10 * w_chisqgs_b10;
   if(gsli7!=0 && w_chisqgs_li7!=0) f += chisqgsli7/gsli7 * w_chisqgs_li7;
   // ~~~~~~ 21Nov2013 ~~~~~~
   // 1 + 1 + 2 + 1 = 5 for today
   //f = f/5.0;
   f = f/3.0;
   
}

double mtune(double* c, int k)
{
  double arglist[10];
  int ierflg = 0;
   EOFlag=k%2;
   TMinuit *gMinuit = new TMinuit(allParam);
   gMinuit->SetFCN(fcn);
   arglist[0] = 1;
   gMinuit->mnexcm("SET ERR", arglist, 1, ierflg);
   gMinuit->SetPrintLevel(-1);

   bool IsConst[allParam];
   char pName[100];
   double e=0.,chi2=0.;
   float ctmp[allParam];

   float Start[allParam];
   float Step[allParam];
   float ULim[allParam];
   float LLim[allParam];

   clock_t t1,t2;

   int momflag=1;
   int ypflag=0;
   int xpflag=0;
   /*if (k%5==0&&k!=0){
     xpflag=1;
     ypflag=1;
     momflag=1;
     }*/
   /*else{
     xpflag=0;
     ypflag=0;
     momflag=1;
     }*/


   /*const int N=6;
     float Ep1[N]={0,0,0,0,1,0};
     float Ep2[N]={0,0,0,0,0,1};
     float Ep3[N]={0,0,0,0,0,1};
     float Ep4[N]={0,0,0,0,0,1};
     float Kp1[N]={0,0,0,0,1,0};
     float Kp2[N]={0,0,0,0,0,1};
     float Kp3[N]={0,0,0,0,0,1};
     float Kp4[N]={0,0,0,0,0,1};
     float Ex1[N]={0,0,xpflag,0,0,0};
     float Ex2[N]={0,0,xpflag,0,0,0};
     float Ex3[N]={0,0,xpflag,0,0,0};
     float Ex4[N]={0,0,xpflag,0,0,0};
     float Kx1[N]={0,0,0,xpflag,0,0};
     float Kx2[N]={0,0,0,xpflag,0,0};
     float Kx3[N]={0,0,0,xpflag,0,0};
     float Kx4[N]={0,0,0,xpflag,0,0};
     float Ey1[N]={ypflag,0,0,0,0,0};
     float Ey2[N]={ypflag,0,0,0,0,0};
     float Ey3[N]={ypflag,0,0,0,0,0};
     float Ey4[N]={ypflag,0,0,0,0,0};
     float Ky1[N]={0,ypflag,0,0,0,0};
     float Ky2[N]={0,ypflag,0,0,0,0};
     float Ky3[N]={0,ypflag,0,0,0,0};
     float Ky4[N]={0,ypflag,0,0,0,0};*/
   /*const int N=4;
     float Ep1[N]={1,0,0,0};
     float Ep2[N]={0,1,0,0};
     float Ep3[N]={0,1,0,0};
     float Ep4[N]={0,1,0,0};
     float Ep5[N]={0,0,momflag,0};
     float Kp1[N]={1,0,0,0};
     float Kp2[N]={0,0,0,1};
     float Kp3[N]={0,0,0,1};
     float Kp4[N]={0,0,0,1};
     float Kp5[N]={0,0,0,0};
     float Ex1[N]={0,0,0,0};
     float Ex2[N]={0,0,0,0};
     float Ex3[N]={0,0,0,0};
     float Ex4[N]={0,0,0,0};
     float Ex5[N]={0,0,0,0};
     float Kx1[N]={0,0,0,0};
     float Kx2[N]={0,0,0,0};
     float Kx3[N]={0,0,0,0};
     float Kx4[N]={0,0,0,0};
     float Kx5[N]={0,0,0,0};
     float Ey1[N]={0,0,0,0};
     float Ey2[N]={0,0,0,0};
     float Ey3[N]={0,0,0,0};
     float Ey4[N]={0,0,0,0};
     float Ey5[N]={0,0,0,0};
     float Ky1[N]={0,0,0,0};
     float Ky2[N]={0,0,0,0};
     float Ky3[N]={0,0,0,0};
     float Ky4[N]={0,0,0,0};
     float Ky5[N]={0,0,0,0};*/
   /*const int N=1;
     float Ep1[N]={0};
     float Ep2[N]={0};
     float Ep3[N]={0};
     float Ep4[N]={0};
     float Ep5[N]={0};
     float Ep6[N]={0};
     float Kp1[N]={0};
     float Kp2[N]={0};
     float Kp3[N]={0};
     float Kp4[N]={0};
     float Kp5[N]={0};
     float Kp6[N]={0};
     float Ex1[N]={0};
     float Ex2[N]={0};
     float Ex3[N]={0};
     float Ex4[N]={0};
     float Ex5[N]={0};
     float Ex6[N]={0};
     float Kx1[N]={0};
     float Kx2[N]={0};
     float Kx3[N]={0};
     float Kx4[N]={0};
     float Kx5[N]={0};
     float Kx6[N]={0};
     float Ey1[N]={0};
     float Ey2[N]={0};
     float Ey3[N]={0};
     float Ey4[N]={0};
     float Ey5[N]={0};
     float Ey6[N]={0};
     float Ky1[N]={0};
     float Ky2[N]={0};
     float Ky3[N]={0};
     float Ky4[N]={0};
     float Ky5[N]={0};
     float Ky6[N]={0};*/
   /*const int N=12;
   int Ep1[N]={1,0,0,0,0,0,0,0,0,0,0,0};
   int Ep2[N]={0,1,0,0,0,0,0,0,0,0,0,0};
   int Ep3[N]={0,0,1,0,0,0,0,0,0,0,0,0};
   int Ep4[N]={0,0,0,1,0,0,0,0,0,0,0,0};
   int Ep5[N]={0,0,0,0,1,0,0,0,0,0,0,0};
   int Ep6[N]={0,0,0,0,0,1,0,0,0,0,0,0};
   int Kp1[N]={0,0,0,0,0,0,1,0,0,0,0,0};
   int Kp2[N]={0,0,0,0,0,0,0,1,0,0,0,0};
   int Kp3[N]={0,0,0,0,0,0,0,0,1,0,0,0};
   int Kp4[N]={0,0,0,0,0,0,0,0,0,1,0,0};
   int Kp5[N]={0,0,0,0,0,0,0,0,0,0,1,0};
   int Kp6[N]={0,0,0,0,0,0,0,0,0,0,0,1};
   int Ex1[N]={0,0,0,0,0,0,0,0,0,0,0,0};
   int Ex2[N]={0,0,0,0,0,0,0,0,0,0,0,0};
   int Ex3[N]={0,0,0,0,0,0,0,0,0,0,0,0};
   int Ex4[N]={0,0,0,0,0,0,0,0,0,0,0,0};
   int Ex5[N]={0,0,0,0,0,0,0,0,0,0,0,0};
   int Ex6[N]={0,0,0,0,0,0,0,0,0,0,0,0};
   int Kx1[N]={0,0,0,0,0,0,0,0,0,0,0,0};
   int Kx2[N]={0,0,0,0,0,0,0,0,0,0,0,0};
   int Kx3[N]={0,0,0,0,0,0,0,0,0,0,0,0};
   int Kx4[N]={0,0,0,0,0,0,0,0,0,0,0,0};
   int Kx5[N]={0,0,0,0,0,0,0,0,0,0,0,0};
   int Kx6[N]={0,0,0,0,0,0,0,0,0,0,0,0};
   int Ey1[N]={0,0,0,0,0,0,0,0,0,0,0,0};
   int Ey2[N]={0,0,0,0,0,0,0,0,0,0,0,0};
   int Ey3[N]={0,0,0,0,0,0,0,0,0,0,0,0};
   int Ey4[N]={0,0,0,0,0,0,0,0,0,0,0,0};
   int Ey5[N]={0,0,0,0,0,0,0,0,0,0,0,0};
   int Ey6[N]={0,0,0,0,0,0,0,0,0,0,0,0};
   int Ky1[N]={0,0,0,0,0,0,0,0,0,0,0,0};
   int Ky2[N]={0,0,0,0,0,0,0,0,0,0,0,0};
   int Ky3[N]={0,0,0,0,0,0,0,0,0,0,0,0};
   int Ky4[N]={0,0,0,0,0,0,0,0,0,0,0,0};
   int Ky5[N]={0,0,0,0,0,0,0,0,0,0,0,0};
   int Ky6[N]={0,0,0,0,0,0,0,0,0,0,0,0};*/
   /*const int N=24;
   int Ep1[N]={1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ep2[N]={0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ep3[N]={0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ep4[N]={0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ep5[N]={0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ep6[N]={0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Kp1[N]={0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Kp2[N]={0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Kp3[N]={0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Kp4[N]={0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Kp5[N]={0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Kp6[N]={0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ex1[N]={0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0};
   int Ex2[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0};
   int Ex3[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0};
   int Ex4[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0};
   int Ex5[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0};
   int Ex6[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0};
   int Kx1[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0};
   int Kx2[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0};
   int Kx3[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0};
   int Kx4[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0};
   int Kx5[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0};
   int Kx6[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1};
   int Ey1[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ey2[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ey3[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ey4[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ey5[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ey6[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ky1[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ky2[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ky3[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ky4[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ky5[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ky6[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};*/
   /*const int N=36;
   int Ep1[N]={1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ep2[N]={0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ep3[N]={0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ep4[N]={0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ep5[N]={0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ep6[N]={0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Kp1[N]={0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Kp2[N]={0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Kp3[N]={0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Kp4[N]={0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Kp5[N]={0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Kp6[N]={0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ex1[N]={0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ex2[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ex3[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ex4[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ex5[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ex6[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Kx1[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Kx2[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Kx3[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Kx4[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Kx5[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Kx6[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ey1[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0};
   int Ey2[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0};
   int Ey3[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0};
   int Ey4[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0};
   int Ey5[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0};
   int Ey6[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0};
   int Ky1[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0};
   int Ky2[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0};
   int Ky3[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0};
   int Ky4[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0};
   int Ky5[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0};
   int Ky6[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1};*/
   
   // const int N=7;
   // int Ep1[N]={1,0,0,0,0,0,0};
   // int Ep2[N]={0,1,0,0,0,0,0};
   // int Ep3[N]={0,1,0,0,0,0,0};
   // int Ep4[N]={0,1,0,0,0,0,0};
   // int Ep5[N]={0,0,0,1,0,0,0};
   // int Ep6[N]={0,0,0,0,0,1,0};
   // int Kp1[N]={1,0,0,0,0,0,0};
   // int Kp2[N]={0,0,1,0,0,0,0};
   // int Kp3[N]={0,0,1,0,0,0,0};
   // int Kp4[N]={0,0,1,0,0,0,0};
   // int Kp5[N]={0,0,0,0,1,0,0};
   // int Kp6[N]={0,0,0,0,0,0,1};
   // int Ex1[N]={0,0,0,0,0,0,0};
   // int Ex2[N]={0,0,0,0,0,0,0};
   // int Ex3[N]={0,0,0,0,0,0,0};
   // int Ex4[N]={0,0,0,0,0,0,0};
   // int Ex5[N]={0,0,0,0,0,0,0};
   // int Ex6[N]={0,0,0,0,0,0,0};
   // int Kx1[N]={0,0,0,0,0,0,0};
   // int Kx2[N]={0,0,0,0,0,0,0};
   // int Kx3[N]={0,0,0,0,0,0,0};
   // int Kx4[N]={0,0,0,0,0,0,0};
   // int Kx5[N]={0,0,0,0,0,0,0};
   // int Kx6[N]={0,0,0,0,0,0,0};
   // int Ey1[N]={0,0,0,0,0,0,0};
   // int Ey2[N]={0,0,0,0,0,0,0};
   // int Ey3[N]={0,0,0,0,0,0,0};
   // int Ey4[N]={0,0,0,0,0,0,0};
   // int Ey5[N]={0,0,0,0,0,0,0};
   // int Ey6[N]={0,0,0,0,0,0,0};
   // int Ky1[N]={0,0,0,0,0,0,0};
   // int Ky2[N]={0,0,0,0,0,0,0};
   // int Ky3[N]={0,0,0,0,0,0,0};
   // int Ky4[N]={0,0,0,0,0,0,0};
   // int Ky5[N]={0,0,0,0,0,0,0};
   // int Ky6[N]={0,0,0,0,0,0,0};
   
//   const int N=7; // Toshi , 29/Jan/2013 
//   int Ep1[N]={1,1,0,0,0,0,0};
//   int Ep2[N]={0,1,1,0,0,0,0};
//   int Ep3[N]={0,0,1,1,0,0,0};
//   int Ep4[N]={0,0,0,1,1,0,0};
//   int Ep5[N]={0,0,0,0,1,1,0};
//   int Ep6[N]={0,0,0,0,0,1,1};
//   int Kp1[N]={1,1,0,0,0,0,0};
//   int Kp2[N]={0,1,1,0,0,0,0};
//   int Kp3[N]={0,0,1,1,0,0,0};
//   int Kp4[N]={0,0,0,1,1,0,0};
//   int Kp5[N]={0,0,0,0,1,1,0};
//   int Kp6[N]={0,0,0,0,0,1,1};
//   int Ex1[N]={0,0,0,0,0,0,0};
//   int Ex2[N]={0,0,0,0,0,0,0};
//   int Ex3[N]={0,0,0,0,0,0,0};
//   int Ex4[N]={0,0,0,0,0,0,0};
//   int Ex5[N]={0,0,0,0,0,0,0};
//   int Ex6[N]={0,0,0,0,0,0,0};
//   int Kx1[N]={0,0,0,0,0,0,0};
//   int Kx2[N]={0,0,0,0,0,0,0};
//   int Kx3[N]={0,0,0,0,0,0,0};
//   int Kx4[N]={0,0,0,0,0,0,0};
//   int Kx5[N]={0,0,0,0,0,0,0};
//   int Kx6[N]={0,0,0,0,0,0,0};
//   int Ey1[N]={0,0,0,0,0,0,0};
//   int Ey2[N]={0,0,0,0,0,0,0};
//   int Ey3[N]={0,0,0,0,0,0,0};
//   int Ey4[N]={0,0,0,0,0,0,0};
//   int Ey5[N]={0,0,0,0,0,0,0};
//   int Ey6[N]={0,0,0,0,0,0,0};
//   int Ky1[N]={0,0,0,0,0,0,0};
//   int Ky2[N]={0,0,0,0,0,0,0};
//   int Ky3[N]={0,0,0,0,0,0,0};
//   int Ky4[N]={0,0,0,0,0,0,0};
//   int Ky5[N]={0,0,0,0,0,0,0};
//   int Ky6[N]={0,0,0,0,0,0,0};
   
   const int N=7; // Toshi , 29/Jan/2013 
   int Ep1[N]={1,1,0,0,0,0,0};
   int Ep2[N]={0,1,1,0,0,0,0};
   int Ep3[N]={0,0,0,1,0,0,0};
   int Ep4[N]={0,0,0,0,1,0,0};
   int Ep5[N]={0,0,0,0,1,0,0};
   int Ep6[N]={0,0,0,0,0,1,0};
   int Kp1[N]={1,1,0,0,0,0,0};
   int Kp2[N]={0,1,1,0,0,0,0};
   int Kp3[N]={0,0,0,1,0,0,0};
   int Kp4[N]={0,0,0,0,1,0,0};
   int Kp5[N]={0,0,0,0,0,1,0};
   int Kp6[N]={0,0,0,0,0,0,1};
   int Ex1[N]={0,0,0,0,0,0,0};
   int Ex2[N]={0,0,0,0,0,0,0};
   int Ex3[N]={0,0,0,0,0,0,0};
   int Ex4[N]={0,0,0,0,0,0,0};
   int Ex5[N]={0,0,0,0,0,0,0};
   int Ex6[N]={0,0,0,0,0,0,0};
   int Kx1[N]={0,0,0,0,0,0,0};
   int Kx2[N]={0,0,0,0,0,0,0};
   int Kx3[N]={0,0,0,0,0,0,0};
   int Kx4[N]={0,0,0,0,0,0,0};
   int Kx5[N]={0,0,0,0,0,0,0};
   int Kx6[N]={0,0,0,0,0,0,0};
   int Ey1[N]={0,0,0,0,0,0,0};
   int Ey2[N]={0,0,0,0,0,0,0};
   int Ey3[N]={0,0,0,0,0,0,0};
   int Ey4[N]={0,0,0,0,0,0,0};
   int Ey5[N]={0,0,0,0,0,0,0};
   int Ey6[N]={0,0,0,0,0,0,0};
   int Ky1[N]={0,0,0,0,0,0,0};
   int Ky2[N]={0,0,0,0,0,0,0};
   int Ky3[N]={0,0,0,0,0,0,0};
   int Ky4[N]={0,0,0,0,0,0,0};
   int Ky5[N]={0,0,0,0,0,0,0};
   int Ky6[N]={0,0,0,0,0,0,0};
   /*
   int Ex1[N]={1,1,0,0,0,0,0};
   int Ex2[N]={0,1,1,0,0,0,0};
   int Ex3[N]={0,0,1,1,0,0,0};
   int Ex4[N]={0,0,0,1,1,0,0};
   int Ex5[N]={0,0,0,0,1,1,0};
   int Ex6[N]={0,0,0,0,0,1,1};
   int Kx1[N]={1,1,0,0,0,0,0};
   int Kx2[N]={0,1,1,0,0,0,0};
   int Kx3[N]={0,0,1,1,0,0,0};
   int Kx4[N]={0,0,0,1,1,0,0};
   int Kx5[N]={0,0,0,0,1,1,0};
   int Kx6[N]={0,0,0,0,0,1,1};
   int Ey1[N]={1,1,0,0,0,0,0};
   int Ey2[N]={0,1,1,0,0,0,0};
   int Ey3[N]={0,0,1,1,0,0,0};
   int Ey4[N]={0,0,0,1,1,0,0};
   int Ey5[N]={0,0,0,0,1,1,0};
   int Ey6[N]={0,0,0,0,0,1,1};
   int Ky1[N]={1,1,0,0,0,0,0};
   int Ky2[N]={0,1,1,0,0,0,0};
   int Ky3[N]={0,0,1,1,0,0,0};
   int Ky4[N]={0,0,0,1,1,0,0};
   int Ky5[N]={0,0,0,0,1,1,0};
   int Ky6[N]={0,0,0,0,0,1,1};
   */
   /*const int N=19;
   int Ep1[N]={1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ep2[N]={0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ep3[N]={0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ep4[N]={0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ep5[N]={0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0};
   int Ep6[N]={0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0};
   int Kp1[N]={1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Kp2[N]={0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Kp3[N]={0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Kp4[N]={0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Kp5[N]={0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0};
   int Kp6[N]={0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0};
   int Ex1[N]={1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ex2[N]={0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ex3[N]={0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ex4[N]={0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ex5[N]={0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0};
   int Ex6[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0};
   int Kx1[N]={1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Kx2[N]={0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Kx3[N]={0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Kx4[N]={0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Kx5[N]={0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0};
   int Kx6[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0};
   int Ey1[N]={1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ey2[N]={0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ey3[N]={0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ey4[N]={0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ey5[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0};
   int Ey6[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0};
   int Ky1[N]={1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ky2[N]={0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ky3[N]={0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ky4[N]={0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ky5[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0};
   int Ky6[N]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1};*/
   /*const int N=13;
   int Ep1[N]={1,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ep2[N]={0,1,0,0,0,0,0,0,0,0,0,0,0};
   int Ep3[N]={0,1,0,0,0,0,0,0,0,0,0,0,0};
   int Ep4[N]={0,1,0,0,0,0,0,0,0,0,0,0,0};
   int Ep5[N]={0,0,0,0,0,1,0,0,0,0,0,0,0};
   int Ep6[N]={0,0,0,0,0,0,0,1,0,0,0,0,0};
   int Kp1[N]={1,0,0,0,0,0,0,0,0,0,0,0,0};
   int Kp2[N]={0,0,1,0,0,0,0,0,0,0,0,0,0};
   int Kp3[N]={0,0,1,0,0,0,0,0,0,0,0,0,0};
   int Kp4[N]={0,0,1,0,0,0,0,0,0,0,0,0,0};
   int Kp5[N]={0,0,0,0,0,0,1,0,0,0,0,0,0};
   int Kp6[N]={0,0,0,0,0,0,0,0,1,0,0,0,0};
   int Ex1[N]={0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ex2[N]={0,0,0,1,0,0,0,0,0,0,0,0,0};
   int Ex3[N]={0,0,0,1,0,0,0,0,0,0,0,0,0};
   int Ex4[N]={0,0,0,1,0,0,0,0,0,0,0,0,0};
   int Ex5[N]={0,0,0,0,0,0,0,0,0,1,0,0,0};
   int Ex6[N]={0,0,0,0,0,0,0,0,0,0,0,1,0};
   int Kx1[N]={0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Kx2[N]={0,0,0,0,1,0,0,0,0,0,0,0,0};
   int Kx3[N]={0,0,0,0,1,0,0,0,0,0,0,0,0};
   int Kx4[N]={0,0,0,0,1,0,0,0,0,0,0,0,0};
   int Kx5[N]={0,0,0,0,0,0,0,0,0,0,1,0,0};
   int Kx6[N]={0,0,0,0,0,0,0,0,0,0,0,0,1};
   int Ey1[N]={0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ey2[N]={0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ey3[N]={0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ey4[N]={0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ey5[N]={0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ey6[N]={0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ky1[N]={0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ky2[N]={0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ky3[N]={0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ky4[N]={0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ky5[N]={0,0,0,0,0,0,0,0,0,0,0,0,0};
   int Ky6[N]={0,0,0,0,0,0,0,0,0,0,0,0,0};*/

   for (int j=0;j<N;j++){
      /*if (Ep1[j]==1 || Ep2[j]==1 || Kp1[j]==1 || Kp2[j]==1){FCNFlag=1;}
      else if (Ex1[j]==1 || Ex2[j]==1 || Kx1[j]==1 || Kx2[j]==1){FCNFlag=2;}
      else if (Ey1[j]==1 || Ey2[j]==1 || Ky1[j]==1 || Ky2[j]==1){FCNFlag=3;}
      else {FCNFlag=0;}*/
      t1=clock();
      for (int i=0;i<nParam;i++){
         int i0=i;
         int i1=i+nParam;
         int i2=i+nParam*2;
         int i3=i+nParam*3;
         int i4=i+nParam*4;
         int i5=i+nParam*5;
         /*if (oa[i]+ob[i]+oc[i]+od[i]>1){
           c[i0]=gRandom->Gaus(c[i0],fabs(c[i0])*1e-3);
           c[i1]=gRandom->Gaus(c[i1],fabs(c[i1])*1e-3);
           c[i2]=gRandom->Gaus(c[i2],fabs(c[i2])*1e-3);
           c[i3]=gRandom->Gaus(c[i3],fabs(c[i3])*1e-3);
           c[i4]=gRandom->Gaus(c[i4],fabs(c[i4])*1e-3);
           c[i5]=gRandom->Gaus(c[i5],fabs(c[i5])*1e-3);
           }*/
         switch (oa[i]+ob[i]+oc[i]+od[i]){
            case -4:
               //HES mom
               ULim[i0]=0.;
               LLim[i0]=0.;
               Start[i0]=0.;
               Step[i0]=0.;
               //HKS mom
               ULim[i1]=0.;
               LLim[i1]=0.;
               Start[i1]=0.;
               Step[i1]=0.;
               //HES xpt 
               ULim[i2]=0.;
               LLim[i2]=0.;
               Start[i2]=0.;
               Step[i2]=0.;
               //HKS xpt 
               ULim[i3]=0.;
               LLim[i3]=0.;
               Start[i3]=0.;
               Step[i3]=0.;
               //HES ypt 
               ULim[i4]=0.;
               LLim[i4]=0.;
               Start[i4]=0.;
               Step[i4]=0.;
               //HKS ypt 
               ULim[i5]=0.;
               LLim[i5]=0.;
               Start[i5]=0.;
               Step[i5]=0.;
               break;

            case 0:
               //HES mom
               //ULim[i0]=0.8415;
               //LLim[i0]=0.8425;
               ULim[i0]=0.843;
               LLim[i0]=0.840;
               Start[i0]=c[i0];
               //Step[i0]=1e-3;
               Step[i0]=1.0;
               //HKS mom
               ULim[i1]=1.185;
               LLim[i1]=1.182;
               //ULim[i1]=1.193;
               //LLim[i1]=1.191;
               Start[i1]=c[i1];
               //Step[i1]=1e-3;
               Step[i1]=1.0;
               //HES xpt 
               ULim[i2]=-0.0;
               LLim[i2]=-0.03;
               Start[i2]=c[i2];
               //Step[i2]=1e-6*xpflag;
               //Step[i2]=1e-4;
               //Step[i2]=c[i2];
               Step[i2]=0.0;
               //HKS xpt 
               ULim[i3]=-0.09;
               LLim[i3]=-0.12;
               Start[i3]=c[i3];
               //Step[i3]=1e-6*xpflag;
               //Step[i3]=1e-4;
               //Step[i3]=c[i3];
               Step[i3]=0.0;
               //HES ypt 
               ULim[i4]=-0.050;
               LLim[i4]=-0.080;
               Start[i4]=c[i4];
               //Step[i4]=1e-6*ypflag;
               //Step[i4]=1e-4;
               Step[i4]=0.0;
               //HKS ypt 
               ULim[i5]=0.02;
               LLim[i5]=-0.02;
               Start[i5]=c[i5];
               //Step[i5]=1e-6*ypflag;
               //Step[i5]=1e-4;
               Step[i5]=0.0;
               break;

            case 1:
               //HES mom
               /*ULim[i0]=0.10;
                 LLim[i0]=-0.001;*/
               //ULim[1]=0.11;
               ULim[1]=0.110;
               LLim[1]=0.099;
               ULim[2]=0.005;
               LLim[2]=-0.005;
               ULim[3]=0.0050;
               LLim[3]=-0.0050;
               ULim[4]=0.002;
               LLim[4]=-0.002;
               Start[i0]=c[i0];
               Step[i0]=Ep1[j];
               //Step[1]=0.;
               //Step[2]=0.;
               //HKS mom
               //ULim[i1]=0.2;
               //LLim[i1]=-0.05;
               //ULim[1+nParam]=0.18;
               ULim[1+nParam]=0.175;
               LLim[1+nParam]=0.165;
               ULim[2+nParam]=0.01;
               LLim[2+nParam]=-0.01;
               ULim[3+nParam]=0.005;
               LLim[3+nParam]=-0.005;
               ULim[4+nParam]=0.002;
               LLim[4+nParam]=-0.002;
               Start[i1]=c[i1];
               Step[i1]=Kp1[j];
               //Step[i1]=1e-4*Kp1[j];
               //Step[1+nParam]=0.;
               //Step[2+nParam]=0.;
               //HES xpt 
               ULim[i2]=0.15;
               LLim[i2]=-0.15;
               Start[i2]=c[i2];
               Step[i2]=Ex1[j]*1e-4;
               //Step[i2]=0.;
               //HKS xpt 
               ULim[i3]=0.1;
               LLim[i3]=-0.1;
               Start[i3]=c[i3];
               Step[i3]=Kx1[j]*1e-4;
               //Step[i3]=0.;
               //HES ypt 
               ULim[i4]=0.05;
               LLim[i4]=-0.05;
               Start[i4]=c[i4];
               Step[i4]=Ey1[j]*1e-4;
               //Step[i4]=0.;
               //HKS ypt 
               ULim[i5]=0.05;
               LLim[i5]=-0.05;
               Start[i5]=c[i5];
               Step[i5]=Ky1[j]*1e-4;
               //Step[i5]=0.;
               break;

            case 2:
               //HES mom
               Start[i0]=c[i0];
               //ULim[i0]=0.1;
               //LLim[i0]=-0.1;
               ULim[i0]=Start[i0]+0.1;
               LLim[i0]=Start[i0]-0.1;
               Step[i0]=Ep2[j];
               //HKS mom
               Start[i1]=c[i1];
               ULim[i1]=Start[i1]+0.1;
               LLim[i1]=Start[i1]-0.1;
               Step[i1]=Kp2[j];
               //HES xpt 
               Start[i2]=c[i2];
               ULim[i2]=Start[i2]+0.1;
               LLim[i2]=Start[i2]-0.1;
               Step[i2]=Ex2[j]*1e-4;
               //HKS xpt 
               Start[i3]=c[i3];
               ULim[i3]=Start[i3]+0.1;
               LLim[i3]=Start[i3]-0.1;
               Step[i3]=Kx2[j]*1e-4;
               //HES ypt 
               Start[i4]=c[i4];
               ULim[i4]=Start[i4]+0.1;
               LLim[i4]=Start[i4]-0.1;
               Step[i4]=Ey2[j]*1e-4;
               //HKS ypt 
               Start[i5]=c[i5];
               ULim[i5]=Start[i5]+0.1;
               LLim[i5]=Start[i5]-0.1;
               Step[i5]=Ky2[j]*1e-4;
               break;

            case 3:
               //HES mom
               Start[i0]=c[i0];
               ULim[i0]=Start[i0]+0.1;
               LLim[i0]=Start[i0]-0.1;
               Step[i0]=Ep3[j];
               //HKS mom
               Start[i1]=c[i1];
               ULim[i1]=Start[i1]+0.1;
               LLim[i1]=Start[i1]-0.1;
               Step[i1]=Kp3[j];
               //HES xpt 
               Start[i2]=c[i2];
               ULim[i2]=Start[i2]+0.1;
               LLim[i2]=Start[i2]-0.1;
               Step[i2]=Ex3[j]*1e-4;
               //HKS xpt 
               Start[i3]=c[i3];
               ULim[i3]=Start[i3]+0.1;
               LLim[i3]=Start[i3]-0.1;
               Step[i3]=Kx3[j]*1e-4;
               //HES ypt 
               Start[i4]=c[i4];
               ULim[i4]=Start[i4]+0.1;
               LLim[i4]=Start[i4]-0.1;
               Step[i4]=Ey3[j]*1e-4;
               //HKS ypt 
               Start[i5]=c[i5];
               ULim[i5]=Start[i5]+0.1;
               LLim[i5]=Start[i5]-0.1;
               Step[i5]=Ky3[j]*1e-4;
               break;

            case 4:
               //HES mom
               Start[i0]=c[i0];
               ULim[i0]=Start[i0]+0.1;
               LLim[i0]=Start[i0]-0.1;
               Step[i0]=Ep4[j];
               //HKS mom
               Start[i1]=c[i1];
               ULim[i1]=Start[i1]+0.1;
               LLim[i1]=Start[i1]-0.1;
               Step[i1]=Kp4[j];
               //HES xpt 
               Start[i2]=c[i2];
               ULim[i2]=Start[i2]+0.1;
               LLim[i2]=Start[i2]-0.1;
               Step[i2]=Ex4[j]*1e-4;
               //HKS xpt 
               Start[i3]=c[i3];
               ULim[i3]=Start[i3]+0.1;
               LLim[i3]=Start[i3]-0.1;
               Step[i3]=Kx4[j]*1e-4;
               //HES ypt 
               Start[i4]=c[i4];
               ULim[i4]=Start[i4]+0.1;
               LLim[i4]=Start[i4]-0.1;
               Step[i4]=Ey4[j]*1e-4;
               //HKS ypt 
               Start[i5]=c[i5];
               ULim[i5]=Start[i5]+0.1;
               LLim[i5]=Start[i5]-0.1;
               Step[i5]=Ky4[j]*1e-4;
               break;

            case 5:
               //HES mom
               Start[i0]=c[i0];
               ULim[i0]=Start[i0]+0.1;
               LLim[i0]=Start[i0]-0.1;
               Step[i0]=Ep5[j];
               //HKS mom
               Start[i1]=c[i1];
               ULim[i1]=Start[i1]+0.1;
               LLim[i1]=Start[i1]-0.1;
               Step[i1]=Kp5[j];
               //HES xpt 
               Start[i2]=c[i2];
               ULim[i2]=Start[i2]+0.1;
               LLim[i2]=Start[i2]-0.1;
               Step[i2]=Ex5[j]*1e-4;
               //HKS xpt 
               Start[i3]=c[i3];
               ULim[i3]=Start[i3]+0.1;
               LLim[i3]=Start[i3]-0.1;
               Step[i3]=Kx5[j]*1e-4;
               //HES ypt 
               Start[i4]=c[i4];
               ULim[i4]=Start[i4]+0.1;
               LLim[i4]=Start[i4]-0.1;
               Step[i4]=Ey5[j]*1e-4;
               //HKS ypt 
               Start[i5]=c[i5];
               ULim[i5]=Start[i5]+0.1;
               LLim[i5]=Start[i5]-0.1;
               Step[i5]=Ky5[j]*1e-4;
               break;

            default:
               //HES mom
               Start[i0]=c[i0];
               ULim[i0]=Start[i0]+0.1;
               LLim[i0]=Start[i0]-0.1;
               if (oc[i0]+od[i0]<5){
                  Step[i0]=Ep6[j];
               }
               else{
                  Step[i0]=0;
               }
               //HKS mom
               Start[i1]=c[i1];
               ULim[i1]=Start[i1]+0.1;
               LLim[i1]=Start[i1]-0.1;
               if (oc[i1]+od[i1]<5){
                  Step[i1]=Kp6[j];
               }
               else{
                  Step[i1]=0;
               }
               //HES xpt 
               Start[i2]=c[i2];
               ULim[i2]=Start[i2]+0.1;
               LLim[i2]=Start[i2]-0.1;
               if (oc[i2]+od[i2]<5){
                  Step[i2]=Ex6[j]*1e-4;
               }
               else{
                  Step[i2]=0;
               }
               //HKS xpt 
               Start[i3]=c[i3];
               ULim[i3]=Start[i3]+0.1;
               LLim[i3]=Start[i3]-0.1;
               if (oc[i3]+od[i3]<5){
                  Step[i3]=Kx6[j]*1e-4;
               }
               else{
                  Step[i3]=0;
               }
               //HES ypt 
               Start[i4]=c[i4];
               ULim[i4]=Start[i4]+0.1;
               LLim[i4]=Start[i4]-0.1;
               Step[i4]=Ey6[j]*1e-4;
               //HKS ypt 
               Start[i5]=c[i5];
               ULim[i5]=Start[i5]+0.1;
               LLim[i5]=Start[i5]-0.1;
               Step[i5]=Ky6[j]*1e-4;
               break;
               /*if (fabs(c[i0])<1e-3) Step[i0]=0;
               if (fabs(c[i1])<1e-3) Step[i1]=0;
               if (fabs(c[i2])<1e-3) Step[i2]=0;
               if (fabs(c[i3])<1e-3) Step[i3]=0;
               if (fabs(c[i4])<1e-3) Step[i4]=0;
               if (fabs(c[i5])<1e-3) Step[i5]=0;*/
         }
         /*if (oa[i]+ob[i]<oc[i]+od[i]){
         //HES mom
         ULim[i0]=1e-3;
         LLim[i0]=-1e-3;
         Start[i0]=c[i0];
         Step[i0]=1e-6;
         //HKS mom
         ULim[i1]=1e-3;
         LLim[i1]=-1e-3;
         Start[i1]=c[i1];
         Step[i1]=1e-6;
         }*/
         /*if (fabs(Start[i])<1e-4){
           ULim[i]=1e-4;
           LLim[i]=-1e-4;
           Start[i]=0.;
           Step[i]=0.;
           }*/
      }

      arglist[0] = 0;
      // stra 1 : standard
      // stra 2 : try to improve minimum
      gMinuit->mnexcm("set stra 2", arglist, 0, ierflg);
      for (int i=0;i<allParam;i++){
         sprintf(pName,"p%d",i+1);
         gMinuit->mnparm(i,pName,Start[i],Step[i],
               LLim[i],ULim[i],ierflg);
      }

      //arglist[0] = 1e10;
      //arglist[1] = 1e-4;
      //arglist[0] = 1e4;
      //arglist[1] = 1e-3;//edm=arglist[1]*1e-3
      //arglist[0] = 1e3;
      //arglist[1] = 0.1;//edm=arglist[1]*1e-3
      arglist[0] = 1e3;
      //arglist[1] = 1e-5;//edm=arglist[1]*1e-3
      //gMinuit->mnexcm("MINIMUM", arglist, 2, ierflg);
      gMinuit->mnexcm("MINIMUM", arglist, 1, ierflg);
      /*if (k%1==0){
        arglist[0] = 1e4;
        gMinuit->mnexcm("IMProve", arglist, 1, ierflg);
        arglist[0] = 1e4;
        arglist[1] = 1e-3;//edm=arglist[1]*1e-3
        gMinuit->mnexcm("MINIMUM", arglist, 2, ierflg);
        }*/
      t2=clock();
      double amin,edm,errdef;
      int nvpar,nparx,icstat;
      gMinuit->mnstat(amin,edm,errdef,nvpar,nparx,icstat);
      gMinuit->mnprin(0,amin);
      if (amin>0) chi2=amin;
      for (int i=0;i<allParam;i++){
         gMinuit->GetParameter(i,c[i],e);
         //coeff[i]=c[i];
      }
      //cout << "k=" <<k<<", j="<<j<<", time="<<(float)(t2-t1)/(float)CLOCKS_PER_SEC<<endl;
   }

   //double chi2=0;
   //double* grad; 
   //gMinuit->Eval(allParam, grad, chi2, c, 1);

   char cMEp[50];
   char cMKp[50];
   char cMEx[50];
   char cMKx[50];
   char cMEy[50];
   char cMKy[50];
   sprintf(cMEp,"HES_mom_%d.dat",k);
   sprintf(cMKp,"HKS_mom_%d.dat",k);
   sprintf(cMEx,"HES_xpt_%d.dat",k);
   sprintf(cMKx,"HKS_xpt_%d.dat",k);
   sprintf(cMEy,"HES_ypt_%d.dat",k);
   sprintf(cMKy,"HKS_ypt_%d.dat",k);
   ofstream MEmom(cMEp);
   ofstream MKmom(cMKp);
   ofstream MExpt(cMEx);
   ofstream MKxpt(cMKx);
   ofstream MEypt(cMEy);
   ofstream MKypt(cMKy);
   for (int i=0;i<nParam;i++){
      int i0=i;
      int i1=i+nParam;
      int i2=i+nParam*2;
      int i3=i+nParam*3;
      int i4=i+nParam*4;
      int i5=i+nParam*5;
      MEmom << c[i0] 
         <<" "<< oa[i0]
         <<" "<< ob[i0]
         <<" "<< oc[i0]
         <<" "<< od[i0]
         <<endl;
      MKmom << c[i1] 
         <<" "<< oa[i1]
         <<" "<< ob[i1]
         <<" "<< oc[i1]
         <<" "<< od[i1]
         <<endl;
      MExpt << c[i2] 
         <<" "<< oa[i2]
         <<" "<< ob[i2]
         <<" "<< oc[i2]
         <<" "<< od[i2]
         <<endl;
      MKxpt << c[i3] 
         <<" "<< oa[i3]
         <<" "<< ob[i3]
         <<" "<< oc[i3]
         <<" "<< od[i3]
         <<endl;
      MEypt << c[i4] 
         <<" "<< oa[i4]
         <<" "<< ob[i4]
         <<" "<< oc[i4]
         <<" "<< od[i4]
         <<endl;
      MKypt << c[i5] 
         <<" "<< oa[i5]
         <<" "<< ob[i5]
         <<" "<< oc[i5]
         <<" "<< od[i5]
         <<endl;
   }

   return chi2;
}

//double mtune(double* c, int k)
double mtune2(double* c, int k , int flag)
{
  float ang = 0;
  float mom = 0;
  double arglist[10];
  int ierflg = 0;
  EOFlag=k%2;
  TMinuit *gMinuit = new TMinuit(allParam);
  if(flag==0){
    gMinuit->SetFCN(fcn);
    ang = 0.0;
    mom = 5.0e-4;
  }
  else{
    gMinuit->SetFCN(fcn2);
    ang = 5.0e-4;
    mom = 0.0;
  }
  
  arglist[0] = 1;
  //arglist[0] = 2;
  gMinuit->mnexcm("SET ERR", arglist, 1, ierflg);
  gMinuit->SetPrintLevel(-1);

  bool IsConst[allParam];
  char pName[100];
  double e=0.,chi2=0.;
  float ctmp[allParam];
  
  float Start[allParam];
  float Step[allParam];
  float ULim[allParam];
  float LLim[allParam];

  clock_t t1,t2;
  
  int momflag=1;
  int ypflag=0;
  int xpflag=0;
   
//  const int N=7; // Toshi , 29/Jan/2013 
//  //int Ep1[N]={1,1,0,0,0,0,0};
//  //int Ep2[N]={0,1,1,0,0,0,0};
//  //int Ep3[N]={0,0,0,1,0,0,0};
//  //int Ep4[N]={0,0,0,0,1,0,0};
//  //int Ep5[N]={0,0,0,0,1,0,0};
//  //int Ep6[N]={0,0,0,0,0,1,0};
//  //int Kp1[N]={1,1,0,0,0,0,0};
//  //int Kp2[N]={0,1,1,0,0,0,0};
//  //int Kp3[N]={0,0,0,1,0,0,0};
//  //int Kp4[N]={0,0,0,0,1,0,0};
//  //int Kp5[N]={0,0,0,0,0,1,0};
//  //int Kp6[N]={0,0,0,0,0,0,1};
//  float Ep1[N]={mom,mom,0,0,0,0,0};
//  float Ep2[N]={0,mom,mom,0,0,0,0};
//  float Ep3[N]={0,0,0,mom,0,0,0};
//  float Ep4[N]={0,0,0,0,mom,0,0};
//  float Ep5[N]={0,0,0,0,0,mom,0};
//  float Ep6[N]={0,0,0,0,0,0,mom};
//  float Kp1[N]={mom,mom,0,0,0,0,0};
//  float Kp2[N]={0,mom,mom,0,0,0,0};
//  float Kp3[N]={0,0,0,mom,0,0,0};
//  float Kp4[N]={0,0,0,0,mom,0,0};
//  float Kp5[N]={0,0,0,0,0,mom,0};
//  float Kp6[N]={0,0,0,0,0,0,mom};
//  ///float Kp1[N]={0,0,0,0,0,mom,mom};
//  ///float Kp2[N]={0,0,0,0,mom,mom,0};
//  ///float Kp3[N]={0,0,0,mom,0,0,0};
//  ///float Kp4[N]={0,0,mom,0,0,0,0};
//  ///float Kp5[N]={0,mom,0,0,0,0,0};
//  ///float Kp6[N]={mom,0,0,0,0,0,0};
//  float Ex0[N]={ang,0,0,0,0,0,0};
//  float Ex1[N]={0,ang,0,0,0,0,0};
//  float Ex2[N]={0,0,ang,0,0,0,0};
//  float Ex3[N]={0,0,0,ang,0,0,0};
//  float Ex4[N]={0,0,0,0,ang,0,0};
//  float Ex5[N]={0,0,0,0,0,ang,0};
//  float Ex6[N]={0,0,0,0,0,0,ang};
//  float Kx0[N]={0,ang,0,0,0,0,0};
//  float Kx1[N]={0,0,ang,0,0,0,0};
//  float Kx2[N]={0,0,0,ang,0,0,0};
//  float Kx3[N]={0,0,0,0,ang,0,0};
//  float Kx4[N]={0,0,0,0,0,ang,0};
//  float Kx5[N]={0,0,0,0,0,0,ang};
//  float Kx6[N]={ang,0,0,0,0,0,0};
//  float Ey0[N]={0,0,ang,0,0,0,0};
//  float Ey1[N]={0,0,0,ang,0,0,0};
//  float Ey2[N]={0,0,0,0,ang,0,0};
//  float Ey3[N]={0,0,0,0,0,ang,0};
//  float Ey4[N]={0,0,0,0,0,0,ang};
//  float Ey5[N]={ang,0,0,0,0,0,0};
//  float Ey6[N]={0,ang,0,0,0,0,0};
//  float Ky0[N]={0,0,0,ang,0,0,0};
//  float Ky1[N]={0,0,0,0,0,ang,0};
//  float Ky2[N]={0,0,0,0,0,0,ang};
//  float Ky3[N]={ang,0,0,0,0,0,0};
//  float Ky4[N]={0,ang,0,0,0,0,0};
//  float Ky5[N]={0,0,ang,0,0,0,0};
//  float Ky6[N]={0,0,0,ang,0,0,0};
  
  const int N=4; // Toshi , 29/Jan/2013 
  //int Ep1[N]={1,1,0,0,0,0,0};
  //int Ep2[N]={0,1,1,0,0,0,0};
  //int Ep3[N]={0,0,0,1,0,0,0};
  //int Ep4[N]={0,0,0,0,1,0,0};
  //int Ep5[N]={0,0,0,0,1,0,0};
  //int Ep6[N]={0,0,0,0,0,1,0};
  //int Kp1[N]={1,1,0,0,0,0,0};
  //int Kp2[N]={0,1,1,0,0,0,0};
  //int Kp3[N]={0,0,0,1,0,0,0};
  //int Kp4[N]={0,0,0,0,1,0,0};
  //int Kp5[N]={0,0,0,0,0,1,0};
  //int Kp6[N]={0,0,0,0,0,0,1};
  float Ep0[N]={mom,0,0,0};
  float Ep1[N]={mom,0,0,0};
  float Ep2[N]={0,mom,0,0};
  float Ep3[N]={0,mom,0,0};
  float Ep4[N]={0,0,mom,0};
  float Ep5[N]={0,0,mom,0};
  float Ep6[N]={0,0,0,mom};
  float Kp0[N]={mom,0,0,0};
  float Kp1[N]={mom,0,0,0};
  float Kp2[N]={0,mom,0,0};
  float Kp3[N]={0,mom,0,0};
  float Kp4[N]={0,0,mom,0};
  float Kp5[N]={0,0,mom,0};
  float Kp6[N]={0,0,0,mom};
  ///float Kp1[N]={0,0,0,0,0,mom,mom};
  ///float Kp2[N]={0,0,0,0,mom,mom,0};
  ///float Kp3[N]={0,0,0,mom,0,0,0};
  ///float Kp4[N]={0,0,mom,0,0,0,0};
  ///float Kp5[N]={0,mom,0,0,0,0,0};
  ///float Kp6[N]={mom,0,0,0,0,0,0};
  float Ex0[N]={ang,0,0,0};
  float Ex1[N]={ang,0,0,0};
  float Ex2[N]={0,ang,0,0};
  float Ex3[N]={0,ang,0,0};
  float Ex4[N]={0,0,ang,0};
  float Ex5[N]={0,0,ang,0};
  float Ex6[N]={0,0,0,ang};
  float Kx0[N]={ang,0,0,0};	 
  float Kx1[N]={ang,0,0,0};
  float Kx2[N]={0,ang,0,0};
  float Kx3[N]={0,ang,0,0};
  float Kx4[N]={0,0,ang,0};
  float Kx5[N]={0,0,ang,0};
  float Kx6[N]={0,0,0,ang};
  float Ey0[N]={ang,0,0,0};	 
  float Ey1[N]={ang,0,0,0};
  float Ey2[N]={0,ang,0,0};
  float Ey3[N]={0,ang,0,0};
  float Ey4[N]={0,0,ang,0};
  float Ey5[N]={0,0,ang,0};
  float Ey6[N]={0,0,0,ang};
  float Ky0[N]={ang,0,0,0};	 
  float Ky1[N]={ang,0,0,0};
  float Ky2[N]={0,ang,0,0};
  float Ky3[N]={0,ang,0,0};
  float Ky4[N]={0,0,ang,0};
  float Ky5[N]={0,0,ang,0};
  float Ky6[N]={0,0,0,ang};
  
  for (int j=0;j<N;j++){
    /*if (Ep1[j]==1 || Ep2[j]==1 || Kp1[j]==1 || Kp2[j]==1){FCNFlag=1;}
      else if (Ex1[j]==1 || Ex2[j]==1 || Kx1[j]==1 || Kx2[j]==1){FCNFlag=2;}
      else if (Ey1[j]==1 || Ey2[j]==1 || Ky1[j]==1 || Ky2[j]==1){FCNFlag=3;}
      else {FCNFlag=0;}*/
    t1=clock();
    for (int i=0;i<nParam;i++){
      int i0=i;
      int i1=i+nParam;
      int i2=i+nParam*2;
      int i3=i+nParam*3;
      int i4=i+nParam*4;
      int i5=i+nParam*5;
      /*if (oa[i]+ob[i]+oc[i]+od[i]>1){
	c[i0]=gRandom->Gaus(c[i0],fabs(c[i0])*1e-3);
	c[i1]=gRandom->Gaus(c[i1],fabs(c[i1])*1e-3);
	c[i2]=gRandom->Gaus(c[i2],fabs(c[i2])*1e-3);
	c[i3]=gRandom->Gaus(c[i3],fabs(c[i3])*1e-3);
	c[i4]=gRandom->Gaus(c[i4],fabs(c[i4])*1e-3);
	c[i5]=gRandom->Gaus(c[i5],fabs(c[i5])*1e-3);
	}*/
      switch (oa[i]+ob[i]+oc[i]+od[i]){
      case -4:
	//HES mom
	ULim[i0]=0.;
	LLim[i0]=0.;
	Start[i0]=0.;
	Step[i0]=0.;
	//HKS mom
	ULim[i1]=0.;
	LLim[i1]=0.;
	Start[i1]=0.;
	Step[i1]=0.;
	//HES xpt 
	ULim[i2]=0.;
	LLim[i2]=0.;
	Start[i2]=0.;
	Step[i2]=0.;
	//HKS xpt 
	ULim[i3]=0.;
	LLim[i3]=0.;
	Start[i3]=0.;
	Step[i3]=0.;
	//HES ypt 
	ULim[i4]=0.;
	LLim[i4]=0.;
	Start[i4]=0.;
	Step[i4]=0.;
	//HKS ypt 
	ULim[i5]=0.;
	LLim[i5]=0.;
	Start[i5]=0.;
	Step[i5]=0.;
	break;
	
      case 0:
	// ---- HES mom ----
	//ULim[i0]=0.8415;
	//LLim[i0]=0.8425;
	ULim[i0]=0.843;
	LLim[i0]=0.840;
	Start[i0]=c[i0];
	//Step[i0]=1e-3;
	//Step[i0]=1.0; // org
	Step[i0]=Ep0[j]; // Toshi
	// ---- HKS mom ----
	ULim[i1]=1.185;
	LLim[i1]=1.182;
	//ULim[i1]=1.193;
	//LLim[i1]=1.191;
	Start[i1]=c[i1];
	//Step[i1]=1e-3;
	//Step[i1]=1.0; //org
	Step[i1]=Kp0[j]; //Toshi
	// ---- HES xpt ----
	ULim[i2]=-0.0;
	LLim[i2]=-0.03;
	Start[i2]=c[i2];
	//Step[i2]=1e-6*xpflag;
	//Step[i2]=1e-4;
	//Step[i2]=c[i2];
	//Step[i2]=0.0;
	Step[i2]=Ex0[j];
	// ---- HKS xpt ----
	ULim[i3]=-0.09;
	LLim[i3]=-0.12;
	Start[i3]=c[i3];
	//Step[i3]=1e-6*xpflag;
	//Step[i3]=1e-4;
	//Step[i3]=c[i3];
	//Step[i3]=0.0;
	Step[i3]=Kx0[j];
	// ---- HES ypt ----
	ULim[i4]=-0.050;
	LLim[i4]=-0.080;
	Start[i4]=c[i4];
	//Step[i4]=1e-6*ypflag;
	//Step[i4]=1e-4;
	//Step[i4]=0.0;
	Step[i4]=Ey0[j];
	// ---- HKS ypt ----
	ULim[i5]=0.02;
	LLim[i5]=-0.02;
	Start[i5]=c[i5];
	//Step[i5]=1e-6*ypflag;
	//Step[i5]=1e-4;
	//Step[i5]=0.0;
	Step[i5]=Ky0[j];
	break;
	//// --- HES mom ---
	////ULim[i0]=0.8415;
	////LLim[i0]=0.8425;
	//Start[i0]=c[i0];
	//ULim[i0]=Start[i0] + 0.05;
	//LLim[i0]=Start[i0] - 0.05;
	////Step[i0]=1e-3;
	//Step[i0]=1.0;
	//// --- HKS mom ---
	//Start[i1]=c[i1];
	//ULim[i1]=Start[i1] + 0.05;
	//LLim[i1]=Start[i1] - 0.05;
	//Step[i1]=1.0;
	//// --- HES xpt ---
	//Start[i2]=c[i2];
	//ULim[i2]=Start[i2] + 0.05;
	//LLim[i2]=Start[i2] - 0.05;
	//Step[i2]=Ex0[j];
	//// --- HKS xpt ---
	//Start[i3]=c[i3];
	//ULim[i3]=Start[i3] + 0.05;
	//LLim[i3]=Start[i3] - 0.05;
	//Step[i3]=Kx0[j];
	//// --- HES ypt ---
	//Start[i4]=c[i4];
	//ULim[i4]=Start[i4] + 0.05;
	//LLim[i4]=Start[i4] - 0.05;
	//Step[i4]=Ey0[j];
	//// --- HKS ypt ---
	//Start[i5]=c[i5];
	//ULim[i5]=Start[i5] + 0.05;
	//LLim[i5]=Start[i5] - 0.05;
	//Step[i5]=Ky0[j];
	//break;
	
      case 1:
	//   // ---- HES mom ----
	//   /*ULim[i0]=0.10;
	//     LLim[i0]=-0.001;*/
	//   //ULim[1]=0.11;
	//   ULim[1]=0.110;
	//   LLim[1]=0.099;
	//   ULim[2]=0.005;
	//   LLim[2]=-0.005;
	//   ULim[3]=0.0050;
	//   LLim[3]=-0.0050;
	//   ULim[4]=0.002;
	//   LLim[4]=-0.002;
	//   Start[i0]=c[i0];
	//   Step[i0]=Ep1[j];
	//   //Step[1]=0.;
	//   //Step[2]=0.;
	//   // ---- HKS mom ----
	//   //ULim[i1]=0.2;
	//   //LLim[i1]=-0.05;
	//   //ULim[1+nParam]=0.18;
	//   ULim[1+nParam]=0.175;
	//   LLim[1+nParam]=0.165;
	//   ULim[2+nParam]=0.01;
	//   LLim[2+nParam]=-0.01;
	//   ULim[3+nParam]=0.005;
	//   LLim[3+nParam]=-0.005;
	//   ULim[4+nParam]=0.002;
	//   LLim[4+nParam]=-0.002;
	//   Start[i1]=c[i1];
	//   Step[i1]=Kp1[j];
	//   //Step[i1]=1e-4*Kp1[j];
	//   //Step[1+nParam]=0.;
	//   //Step[2+nParam]=0.;
	//   // ---- HES xpt  ----
	//   ULim[i2]=0.15;
	//   LLim[i2]=-0.15;
	//   Start[i2]=c[i2];
	//   Step[i2]=Ex1[j]*1e-4;
	//   //Step[i2]=0.;
	//   // ---- HKS xpt ----
	//   ULim[i3]=0.1;
	//   LLim[i3]=-0.1;
	//   Start[i3]=c[i3];
	//   Step[i3]=Kx1[j]*1e-4;
	//   //Step[i3]=0.;
	//   // ---- HES ypt ----
	//   ULim[i4]=0.05;
	//   LLim[i4]=-0.05;
	//   Start[i4]=c[i4];
	//   Step[i4]=Ey1[j]*1e-4;
	//   //Step[i4]=0.;
	//   // ---- HKS ypt ----
	//   ULim[i5]=0.05;
	//   LLim[i5]=-0.05;
	//   Start[i5]=c[i5];
	//   Step[i5]=Ky1[j]*1e-4;
	//   //Step[i5]=0.;
	//   break;
	// --- HES mom ---
	/*ULim[i0]=0.10;
	  LLim[i0]=-0.001;*/
	//ULim[1]=0.11;
	//ULim[1]=0.110;
	//LLim[1]=0.099;
	//ULim[2]=0.005;
	//LLim[2]=-0.005;
	//ULim[3]=0.0050;
	//LLim[3]=-0.0050;
	//ULim[4]=0.002;
	//LLim[4]=-0.002;
	Start[i0]=c[i0];
	ULim[i0]=Start[i0] + 0.05;
	LLim[i0]=Start[i0] - 0.05;
	Step[i0]=Ep1[j];
	// --- HKS mom ---
	//ULim[i1]=0.2;
	//LLim[i1]=-0.05;
	//ULim[1+nParam]=0.18;
	//ULim[1+nParam]=0.175;
	//LLim[1+nParam]=0.165;
	//ULim[2+nParam]=0.01;
	//LLim[2+nParam]=-0.01;
	//ULim[3+nParam]=0.005;
	//LLim[3+nParam]=-0.005;
	//ULim[4+nParam]=0.002;
	//LLim[4+nParam]=-0.002;
	Start[i1]=c[i1];
	ULim[i1]=Start[i1] + 0.05;
	LLim[i1]=Start[i1] - 0.05;
	Step[i1]=Kp1[j];
	// --- HES xpt ---
	Start[i2]=c[i2];
	ULim[i2]=Start[i2] + 0.05;
	LLim[i2]=Start[i2] - 0.05;
	Step[i2]=Ex1[j];
	// --- HKS xpt ---
	Start[i3]=c[i3];
	ULim[i3]=Start[i3] + 0.05;
	LLim[i3]=Start[i3] - 0.05;
	Step[i3]=Kx1[j];
	// --- HES ypt ---
	Start[i4]=c[i4];
	ULim[i4]=Start[i4] + 0.05;
	LLim[i4]=Start[i4] - 0.05;
	Step[i4]=Ey1[j];
	// --- HKS ypt ---
	Start[i5]=c[i5];
	ULim[i5]=Start[i5] + 0.05;
	LLim[i5]=Start[i5] - 0.05;
	Step[i5]=Ky1[j];
	break;
	
      case 2:
	//HES mom
	Start[i0]=c[i0];
	//ULim[i0]=0.1;
	//LLim[i0]=-0.1;
	ULim[i0]=Start[i0]+0.05;
	LLim[i0]=Start[i0]-0.05;
	Step[i0]=Ep2[j];
	//HKS mom
	Start[i1]=c[i1];
	ULim[i1]=Start[i1]+0.05;
	LLim[i1]=Start[i1]-0.05;
	Step[i1]=Kp2[j];
	//HES xpt 
	Start[i2]=c[i2];
	ULim[i2]=Start[i2]+0.05;
	LLim[i2]=Start[i2]-0.05;
	Step[i2]=Ex2[j];
	//HKS xpt 
	Start[i3]=c[i3];
	ULim[i3]=Start[i3]+0.05;
	LLim[i3]=Start[i3]-0.05;
	Step[i3]=Kx2[j];
	//HES ypt 
	Start[i4]=c[i4];
	ULim[i4]=Start[i4]+0.05;
	LLim[i4]=Start[i4]-0.05;
	Step[i4]=Ey2[j];
	//HKS ypt 
	Start[i5]=c[i5];
	ULim[i5]=Start[i5]+0.05;
	LLim[i5]=Start[i5]-0.05;
	Step[i5]=Ky2[j];
	break;
	
      case 3:
	//HES mom
	Start[i0]=c[i0];
	ULim[i0]=Start[i0]+0.05;
	LLim[i0]=Start[i0]-0.05;
	Step[i0]=Ep3[j];
	//HKS mom
	Start[i1]=c[i1];
	ULim[i1]=Start[i1]+0.05;
	LLim[i1]=Start[i1]-0.05;
	Step[i1]=Kp3[j];
	//HES xpt 
	Start[i2]=c[i2];
	ULim[i2]=Start[i2]+0.05;
	LLim[i2]=Start[i2]-0.05;
	Step[i2]=Ex3[j];
	//HKS xpt 
	Start[i3]=c[i3];
	ULim[i3]=Start[i3]+0.05;
	LLim[i3]=Start[i3]-0.05;
	Step[i3]=Kx3[j];
	//HES ypt 
	Start[i4]=c[i4];
	ULim[i4]=Start[i4]+0.05;
	LLim[i4]=Start[i4]-0.05;
	Step[i4]=Ey3[j];
	//HKS ypt 
	Start[i5]=c[i5];
	ULim[i5]=Start[i5]+0.05;
	LLim[i5]=Start[i5]-0.05;
	Step[i5]=Ky3[j];
	break;
	
      case 4:
	//HES mom
	Start[i0]=c[i0];
	ULim[i0]=Start[i0]+0.05;
	LLim[i0]=Start[i0]-0.05;
	Step[i0]=Ep4[j];
	//HKS mom
	Start[i1]=c[i1];
	ULim[i1]=Start[i1]+0.05;
	LLim[i1]=Start[i1]-0.05;
	Step[i1]=Kp4[j];
	//HES xpt 
	Start[i2]=c[i2];
	ULim[i2]=Start[i2]+0.05;
	LLim[i2]=Start[i2]-0.05;
	Step[i2]=Ex4[j];
	//HKS xpt 
	Start[i3]=c[i3];
	ULim[i3]=Start[i3]+0.05;
	LLim[i3]=Start[i3]-0.05;
	Step[i3]=Kx4[j];
	//HES ypt 
	Start[i4]=c[i4];
	ULim[i4]=Start[i4]+0.05;
	LLim[i4]=Start[i4]-0.05;
	Step[i4]=Ey4[j];
	//HKS ypt 
	Start[i5]=c[i5];
	ULim[i5]=Start[i5]+0.05;
	LLim[i5]=Start[i5]-0.05;
	Step[i5]=Ky4[j];
	break;
	
      case 5:
	//HES mom
	Start[i0]=c[i0];
	ULim[i0]=Start[i0]+0.05;
	LLim[i0]=Start[i0]-0.05;
	Step[i0]=Ep5[j];
	//HKS mom
	Start[i1]=c[i1];
	ULim[i1]=Start[i1]+0.05;
	LLim[i1]=Start[i1]-0.05;
	Step[i1]=Kp5[j];
	//HES xpt 
	Start[i2]=c[i2];
	ULim[i2]=Start[i2]+0.05;
	LLim[i2]=Start[i2]-0.05;
	Step[i2]=Ex5[j];
	//HKS xpt 
	Start[i3]=c[i3];
	ULim[i3]=Start[i3]+0.05;
	LLim[i3]=Start[i3]-0.05;
	Step[i3]=Kx5[j];
	//HES ypt 
	Start[i4]=c[i4];
	ULim[i4]=Start[i4]+0.05;
	LLim[i4]=Start[i4]-0.05;
	Step[i4]=Ey5[j];
	//HKS ypt 
	Start[i5]=c[i5];
	ULim[i5]=Start[i5]+0.05;
	LLim[i5]=Start[i5]-0.05;
	Step[i5]=Ky5[j];
	break;
	
      default:
	//HES mom
	Start[i0]=c[i0];
	ULim[i0]=Start[i0]+0.05;
	LLim[i0]=Start[i0]-0.05;
	Step[i0]=Ep6[j];
	//if (oc[i0]+od[i0]<5){
	//  Step[i0]=Ep6[j];
	//}
	//else{
	//  Step[i0]=0;
	//}
	//HKS mom
	Start[i1]=c[i1];
	ULim[i1]=Start[i1]+0.05;
	LLim[i1]=Start[i1]-0.05;
	Step[i1]=Kp6[j];
	//if (oc[i1]+od[i1]<5){
	//  Step[i1]=Kp6[j];
	//}
	//else{
	//  Step[i1]=0;
	//}
	//HES xpt 
	Start[i2]=c[i2];
	ULim[i2]=Start[i2]+0.05;
	LLim[i2]=Start[i2]-0.05;
	Step[i2]=Ex6[j];
	//if (oc[i2]+od[i2]<5){
	//  Step[i2]=Ex6[j];
	//}
	//else{
	//  Step[i2]=0;
	//}
	//HKS xpt 
	Start[i3]=c[i3];
	ULim[i3]=Start[i3]+0.05;
	LLim[i3]=Start[i3]-0.05;
	Step[i3]=Kx6[j];
	//if (oc[i3]+od[i3]<5){
	//  Step[i3]=Kx6[j];
	//}
	//else{
	//  Step[i3]=0;
	//}
	//HES ypt 
	Start[i4]=c[i4];
	ULim[i4]=Start[i4]+0.05;
	LLim[i4]=Start[i4]-0.05;
	Step[i4]=Ey6[j];
	//HKS ypt 
	Start[i5]=c[i5];
	ULim[i5]=Start[i5]+0.05;
	LLim[i5]=Start[i5]-0.05;
	Step[i5]=Ky6[j];
	break;
	/*if (fabs(c[i0])<1e-3) Step[i0]=0;
	  if (fabs(c[i1])<1e-3) Step[i1]=0;
	  if (fabs(c[i2])<1e-3) Step[i2]=0;
	  if (fabs(c[i3])<1e-3) Step[i3]=0;
	  if (fabs(c[i4])<1e-3) Step[i4]=0;
	  if (fabs(c[i5])<1e-3) Step[i5]=0;*/
      }
      /*if (oa[i]+ob[i]<oc[i]+od[i]){
      //HES mom
      ULim[i0]=1e-3;
      LLim[i0]=-1e-3;
      Start[i0]=c[i0];
      Step[i0]=1e-6;
      //HKS mom
      ULim[i1]=1e-3;
      LLim[i1]=-1e-3;
      Start[i1]=c[i1];
      Step[i1]=1e-6;
      }*/
      /*if (fabs(Start[i])<1e-4){
	ULim[i]=1e-4;
	LLim[i]=-1e-4;
	Start[i]=0.;
	Step[i]=0.;
	}*/
    }
    
    arglist[0] = 2;
    // stra 1 : standard
    // stra 2 : try to improve minimum
    //gMinuit->mnexcm("set stra 2", arglist, 0, ierflg);
    gMinuit->mnexcm("SET STR", arglist , 1 , ierflg);// Toshi , 17/June/2013
    for (int i=0;i<allParam;i++){
      sprintf(pName,"p%d",i+1);
      gMinuit->mnparm(i,pName,Start[i],Step[i],
		      LLim[i],ULim[i],ierflg);
    }
    
    //arglist[0] = 1e10;
    //arglist[1] = 1e-4;
    //arglist[0] = 1e4;
    //arglist[1] = 1e-3;//edm=arglist[1]*1e-3
    //arglist[0] = 1e3;
    //arglist[1] = 0.1;//edm=arglist[1]*1e-3
    //arglist[0] = 1.0e+3; // Max calls
    //arglist[0] = 2.0e+3; // Max calls
    //arglist[0] = 3000.0; // Max calls
    arglist[0] = 20000.0; // Max calls
    arglist[1] = 0.1; // 
    //arglist[1] = 1e-5;//edm=arglist[1]*1e-3
    //gMinuit->mnexcm("MINIMUM", arglist, 2, ierflg);
    //gMinuit->mnexcm("MINIMUM", arglist, 1, ierflg);
   gMinuit->mnexcm("MINImize", arglist, 2, ierflg); // Toshi , 17/June/2013
   

   //arglist[0] = 3000.0;
   arglist[0] = 20000.0;
   //gMinuit->mnexcm("HESse",arglist,1,ierflg);// Toshi , 25/June/2013

   /*if (k%1==0){
     arglist[0] = 1e4;
     gMinuit->mnexcm("IMProve", arglist, 1, ierflg);
      arglist[0] = 1e4;
      arglist[1] = 1e-3;//edm=arglist[1]*1e-3
      gMinuit->mnexcm("MINIMUM", arglist, 2, ierflg);
      }*/
    t2=clock();
    double amin,edm,errdef;
    int nvpar,nparx,icstat;
    gMinuit->mnstat(amin,edm,errdef,nvpar,nparx,icstat);
    gMinuit->mnprin(0,amin);
    if (amin>0) chi2=amin;
    for (int i=0;i<allParam;i++){
      gMinuit->GetParameter(i,c[i],e);
      //coeff[i]=c[i];
    }
    //cout << "k=" <<k<<", j="<<j<<", time="<<(float)(t2-t1)/(float)CLOCKS_PER_SEC<<endl;
  }
  
  //double chi2=0;
  //double* grad; 
  //gMinuit->Eval(allParam, grad, chi2, c, 1);
  
  char cMEp[50];
  char cMKp[50];
  char cMEx[50];
  char cMKx[50];
  char cMEy[50];
  char cMKy[50];
  sprintf(cMEp,"HES_mom_%d.dat",k);
  sprintf(cMKp,"HKS_mom_%d.dat",k);
  sprintf(cMEx,"HES_xpt_%d.dat",k);
  sprintf(cMKx,"HKS_xpt_%d.dat",k);
  sprintf(cMEy,"HES_ypt_%d.dat",k);
  sprintf(cMKy,"HKS_ypt_%d.dat",k);
  ofstream MEmom(cMEp);
  ofstream MKmom(cMKp);
  ofstream MExpt(cMEx);
  ofstream MKxpt(cMKx);
  ofstream MEypt(cMEy);
  ofstream MKypt(cMKy);
  for (int i=0;i<nParam;i++){
    int i0=i;
    int i1=i+nParam;
    int i2=i+nParam*2;
    int i3=i+nParam*3;
    int i4=i+nParam*4;
    int i5=i+nParam*5;
    MEmom << c[i0] 
	  <<" "<< oa[i0]
	  <<" "<< ob[i0]
	  <<" "<< oc[i0]
	  <<" "<< od[i0]
	  <<endl;
    MKmom << c[i1] 
	  <<" "<< oa[i1]
	  <<" "<< ob[i1]
	  <<" "<< oc[i1]
	  <<" "<< od[i1]
	  <<endl;
    MExpt << c[i2] 
	  <<" "<< oa[i2]
	  <<" "<< ob[i2]
	  <<" "<< oc[i2]
	  <<" "<< od[i2]
	  <<endl;
    MKxpt << c[i3] 
	  <<" "<< oa[i3]
	  <<" "<< ob[i3]
	  <<" "<< oc[i3]
	  <<" "<< od[i3]
	  <<endl;
    MEypt << c[i4] 
	  <<" "<< oa[i4]
	  <<" "<< ob[i4]
	  <<" "<< oc[i4]
	  <<" "<< od[i4]
	  <<endl;
    MKypt << c[i5] 
	  <<" "<< oa[i5]
	  <<" "<< ob[i5]
	  <<" "<< oc[i5]
	  <<" "<< od[i5]
	  <<endl;
  }
  
  return chi2;
}

void GetVar(int event, float xf, float xpf, float yf, float ypf, float var[][nParam])
{
   float Y=0.;
   float x=1.; 
   float v[5] = {1,xf,xpf,yf,ypf};
   int npar=0;
   int I[4];
   int n=0;

   for (n=0;n<nMat+1;n++){
      for (I[0]=0;I[0]<n+1;I[0]++){
         for (I[1]=0;I[1]<n+1;I[1]++){
            for (I[2]=0;I[2]<n+1;I[2]++){
               for (I[3]=0;I[3]<n+1;I[3]++){
                  int a=I[3],b=I[2],c=I[1],d=I[0];

                  if (a+b+c+d==n){
                     //if ( a<=nXf && b<=nXpf && c<=nYf && d<=nYpf){
                     x = pow(xf,float(a))*pow(xpf,float(b))*pow(yf,float(c))*pow(ypf,float(d)); 
                     //}
                     //else{
                     //    x = 0.;
                     //}
                     var[event][npar]=x;
                     npar++;
                  }

               }
            }
         }
      }
   }
}

float recfcn(double *par, int event, float var[][nParam], int n)
{
   float Y=0.;
   int np=0;
   switch(n)
   {
      case 1:np=5;
      case 2:np=15;
      case 3:np=35;
      case 4:np=70;
      case 5:np=126;
      default: np=210;
   }
   for (int j=0;j<np;j++){
      Y+=par[j]*var[event][j];
   }
   return Y;
}


float calcMM(float Ei,float pe, float xpe,float ype,
      float pk, float xpk,float ypk,int TargetFlag)
{
   float mm=0.;
   //float Ei = 2.344;
   float mtar = 0.;
   float mcore = 0.;
   int N=0;
   int Nentries=0;
   float KEloss=0.;
   float EEloss=0.;
   float BEloss=0.;
   float dmm=0.;
   //float elosscale = 1.5;//toshi
   float elosscale = 1.0;//toshi
   //float elosscale = 1.1;//toshi
   //float elosscale = 0.9;//toshi
   //float elosscale = 0.6;//toshi
   //float elosscale2 = 0.60;//toshi
   //float elosscale2 = 1.30;//toshi
   //float elosscale2 = 1.00;//toshi
   //float elosscale2 = 1.164;//toshi
   //float elosscale2 = 0.996797;// Toshi , 4June2013
   //float elosscale2 = 0.967883; // Toshi , 15June2013
   //float elosscale2 = 0.86522;// Toshi , 12Sep2013
   //float elosscale2 = 0.868055;// Toshi , 13Sep2013, 11.52 MeV
   //float elosscale2 = 0.747682;// Toshi , 13Sep2013, 11.4 MeV
   //float elosscale2 = 1.15;// Toshi , 15Sep2013 for test, 11.8 MeV
   //float elosscale2 = 0.890462;// Toshi , 17Sep2013, 10.5 MeV for test
   //float elosscale2 = 1.00;// Toshi , 17Dec2013 for test !!!!!! 
   float elosscale2 = 0.97;// Toshi , 17Dec2013 for test !!!!!! 
   //float elosscale2 = 0.890462;// Toshi , 17Sep2013, 10.5 MeV for test
   
   float h2o_scale = 1.55; // Toshi , 25/Dec/2013 for test
   //float h2o_scale = 1.38; // Toshi , 12/Nov/2013 for test
   //float h2o_scale = 1.35;
   //float h2o_scale = 1.33; // Toshi , 12/Nov/2013 for test
   //float h2o_scale = 1.30; // Toshi , 12/Nov/2013 for test
   //float h2o_scale = 1.25; // Toshi , 12/Nov/2013 for test
   //float h2o_scale = 1.20; // Toshi , 12/Nov/2013 for test
   
   
   switch(TargetFlag)
     {
     case 1:
       mtar=mp;
       mcore=0.;
       //EEloss=440e-6*0.8;
       //KEloss=502e-6*0.8;
       //BEloss=436e-6*0.8;
       //EEloss=372e-6*elosscale*elosscale2;// Toshi , 2/Dec/2012
       //KEloss=429e-6*elosscale*elosscale2;// Toshi , 2/Dec/2012
       //BEloss=372e-6*elosscale*elosscale2;// Toshi , 2/Dec/2012
       EEloss=374e-6*elosscale*elosscale2;// Toshi , 16/Dec/2013
       KEloss=465e-6*elosscale*elosscale2;// Toshi , 16/Dec/2013
       BEloss=374e-6*elosscale*elosscale2;// Toshi , 16/Dec/2013
       //dmm=-120e-6;
       break;	
     case 7:
       mtar=mLi7;
       mcore=mHe6;
       //EEloss=133e-6;//kawama
       //KEloss=151e-6;//kawama
       //BEloss=133e-6;//kawama
       //EEloss=89e-6; // Toshi , 9/Nov/2012
       //KEloss=118e-6;// Toshi , 9/Nov/2012
       //BEloss=103e-6;// Toshi , 9/Nov/2012
       //EEloss=111e-6*elosscale;// Toshi , 2/Dec/2012
       //KEloss=125e-6*elosscale;// Toshi , 2/Dec/2012
       //BEloss=110e-6*elosscale;// Toshi , 2/Dec/2012
       EEloss=111e-6*elosscale;// Toshi , 16/Dec/2013
       KEloss=138e-6*elosscale;// Toshi , 16/Dec/2013
       BEloss=110e-6*elosscale;// Toshi , 16/Dec/2013
       break;	
     case 9:
       mtar=mBe9;
       mcore=mLi8;
       //BEloss=177e-6;
       //EEloss=179e-6;
       //KEloss=138e-6;
       //BEloss=89e-6; // Toshi , 9/Nov/2012
       //EEloss=118e-6;// Toshi , 9/Nov/2012
       //KEloss=103e-6;// Toshi , 9/Nov/2012
       //EEloss=111e-6*elosscale;// Toshi , 2/Dec/2012
       //KEloss=126e-6*elosscale;// Toshi , 2/Dec/2012
       //BEloss=110e-6*elosscale;// Toshi , 2/Dec/2012
       EEloss=111e-6*elosscale;// Toshi , 28Jan2014
       KEloss=140e-6*elosscale;// Toshi , 28Jan2014
       BEloss=110e-6*elosscale;// Toshi , 28Jan2014
       break;	
     case 10:
       mtar=mB10;
       mcore=mBe9;
       //EEloss=45e-6;//kawama
       //KEloss=52e-6;//kawama
       //BEloss=45e-6;//kawama
       //EEloss=27e-6;// Toshi , 9/Nov/2012
       //KEloss=37e-6;// Toshi , 9/Nov/2012
       //BEloss=31e-6;// Toshi , 9/Nov/2012
       //EEloss=34e-6*elosscale;// Toshi , 2/Dec/2012
       //KEloss=38e-6*elosscale;// Toshi , 2/Dec/2012
       //BEloss=34e-6*elosscale;// Toshi , 2/Dec/2012
       EEloss=34e-6*elosscale;// Toshi , 16/Dec/2013
       KEloss=43e-6*elosscale;// Toshi , 16/Dec/2013
       BEloss=34e-6*elosscale;// Toshi , 16/Dec/2013
       break;	
     case 12:
       mtar=mC12;
       mcore=mB11;
       //EEloss=90e-6;//kawama
       //KEloss=103e-6;//kawama
       //BEloss=89e-6;//kawama
       ///EEloss=58e-6;// Toshi , 9/Nov/2012
       ///KEloss=77e-6;// Toshi , 9/Nov/2012
       ///BEloss=66e-6;// Toshi , 9/Nov/2012
       //EEloss=71e-6*elosscale;// Toshi , 2/Dec/2012
       //KEloss=81e-6*elosscale;// Toshi , 2/Dec/2012
       //BEloss=71e-6*elosscale;// Toshi , 2/Dec/2012
       EEloss=55e-6*elosscale;// Toshi , 2/Dec/2012
       KEloss=68e-6*elosscale;// Toshi , 2/Dec/2012
       BEloss=55e-6*elosscale;// Toshi , 2/Dec/2012
       break;	
     case 52:
       mtar=mCr52;
       mcore=mV51;
       //EEloss=94e-6;//kawama
       //KEloss=99e-6;//kawama
       //BEloss=93e-6;//kawama
       //EEloss=71e-6;// Toshi , 9/Nov/2012
       //KEloss=88e-6;// Toshi , 9/Nov/2012
       //BEloss=79e-6;// Toshi , 9/Nov/2012
       //EEloss=86e-6*elosscale;// Toshi , 2/Dec/2012
       //KEloss=90e-6*elosscale;// Toshi , 2/Dec/2012
       //BEloss=86e-6*elosscale;// Toshi , 2/Dec/2012
       //~~~~~154.0 mg/cm2 ~~~~~~
       EEloss=86e-6*elosscale;// Toshi , 16/Dec/2013
       KEloss=99e-6*elosscale;// Toshi , 16/Dec/2013
       BEloss=86e-6*elosscale;// Toshi , 16/Dec/2013
       //~~~~~134.0 mg/cm2 ~~~~~~
       //EEloss=74e-6*elosscale;// Toshi , 16/Dec/2013
       //KEloss=86e-6*elosscale;// Toshi , 16/Dec/2013
       //BEloss=74e-6*elosscale;// Toshi , 16/Dec/2013
       break;	
     case 101:
       mtar=mp;
       mcore=0.;
       //EEloss=442e-6*1.6;//kawama, (rough. caution!)
       //KEloss=487e-6*1.6;//kawama
       //BEloss=439e-6*1.6;//kawama
       ///EEloss=373e-6;// Toshi , 9/Nov/2012
       ///KEloss=408e-6;// Toshi , 9/Nov/2012
       ///BEloss=371e-6;// Toshi , 9/Nov/2012
       //EEloss=402e-6*elosscale;// Toshi , 2/Dec/2012
       //KEloss=442e-6*elosscale;// Toshi , 2/Dec/2012
       //BEloss=400e-6*elosscale;// Toshi , 2/Dec/2012
       EEloss=402e-6*elosscale*h2o_scale;// Toshi , 2/Dec/2012
       KEloss=442e-6*elosscale*h2o_scale;// Toshi , 2/Dec/2012
       BEloss=400e-6*elosscale*h2o_scale;// Toshi , 2/Dec/2012
       
       break;	
     case 112:
       mtar=mC12;
       mcore=mB11;
       //EEloss=296e-6*elosscale;// Toshi , 9/Nov/2012
       //KEloss=416e-6*elosscale;// Toshi , 9/Nov/2012
       //BEloss=349e-6*elosscale;// Toshi , 9/Nov/2012
       //EEloss=372e-6*elosscale*elosscale2;// Toshi , 2/Dec/2012
       //KEloss=429e-6*elosscale*elosscale2;// Toshi , 2/Dec/2012
       //BEloss=372e-6*elosscale*elosscale2;// Toshi , 2/Dec/2012
       EEloss=374e-6*elosscale*elosscale2;// Toshi , 16/Dec/2013
       KEloss=465e-6*elosscale*elosscale2;// Toshi , 16/Dec/2013
       BEloss=374e-6*elosscale*elosscale2;// Toshi , 16/Dec/2013
       //dmm=-120e-6;
       break;	
     default:
       cout << "put correct TargetFlag"<<endl;	
       return 0.;
       break;
     }
   
   // ==== Should add angles offsets of xp and yp here ===== //
   //float beamang_theta = 0.0;
   //float beamang_theta = 2.38743e-3;// the first results (Toshi,27May2013)
   //float beamang_theta = 3.91811e-3; // the second results (Toshi,28May2013)
   //float beamang_theta = 2.48098e-3; // ( Toshi,29May2013 )
   //float beamang_theta = 3.79551e-3; // ( Toshi , 4June2013 )
   //float beamang_theta = 3.06066e-3; // ( Toshi , 15June2013 )
   //float beamang_theta = 3.89069e-3; // ( Toshi , 12Sep2013 ), 11.5 MeV
   //float beamang_theta = 4.25934e-3; // ( Toshi , 13Sep2013 ), 11.4 MeV
   //float beamang_theta = 4.62799e-3; // ( Toshi , 13Sep2013 ), 11.3 MeV
   //float beamang_theta = 5.55e-3; // ( Toshi , 13Sep2013 ), 10.5 MeV for test
   //float beamang_theta = 5.30*1.0e-3; // test
   //float beamang_theta = 1.0*1.0e-3;
   //float beamang_theta = 2.0*1.0e-3;
   //float beamang_theta = 3.0*1.0e-3;
   //float beamang_theta = 4.0*1.0e-3;
   //float beamang_theta = 5.0*1.0e-3; // test
   float beamang_theta = 5.30*1.0e-3; // test
   //float beamang_theta = 6.0*1.0e-3;
   //float beamang_theta = 7.0*1.0e-3;
   float beamang_phi = 0.0;
   //float beamang_phi = 3.14159/2.0;     // PI/2.0 : 1.6
   //float beamang_phi = 3.14159;         // PI : 3.1
   //float beamang_phi = 3.14159*3.0/2.0; // 3/2*PI : 4.7
   //float beamang_phi = 3.14159*2.0;     // 2*PI : 6.3
   
   float beam_xp = tan(beamang_theta)*cos(beamang_phi);
   float beam_yp = tan(beamang_theta)*sin(beamang_phi);
   xpk = xpk - beam_xp;
   ypk = ypk - beam_yp;
   xpe = xpe + beam_xp;
   ype = ype + beam_yp;
   //xpe = xpe - beam_xp; // Toshi , 29/May/2013
   //ype = ype - beam_yp; // Toshi , 29/May/2013
  
   double Ek = sqrt(pk*pk+mk*mk)+KEloss;
   pk=sqrt(Ek*Ek-mk*mk);
   double pkz = pk/sqrt(1+xpk*xpk+ypk*ypk);
   double pkx = pkz*xpk;
   double pky = pkz*ypk;
   double Ee = sqrt(pe*pe+me*me)+EEloss;
   pe=sqrt(Ee*Ee-me*me);
   double pez = pe/sqrt(1+xpe*xpe+ype*ype);
   double pex = pez*xpe;
   double pey = pez*ype;
   Ei=Ei-BEloss;
   double pi = sqrt(Ei*Ei-me*me);
   double dE = (Ei+mtar-Ek-Ee);
   double dE2 = dE*dE;
   
   //---- Beam angle correction by D.Kawama ----
   //---- Need to be checked....
   //double dpx = (pi*sin(3.7e-3)-pex-pkx);
   //double dpx = (pi*cos(-4.0e-3)*sin(6.5e-3)-pex-pkx);
   //double dpy = (pi*sin(-4.0e-3)-pey-pky);
   //double dpz = (pi*cos(-4.0e-3)*cos(6.5e-3)-pez-pkz);
   
   
   // by Toshi for test
   //double dpx = (-pex-pkx);
   //double dpy = (-pey-pky);
   //double dpz = (pi-pez-pkz);
   //---- Toshi for test----- (26Apr2013)
  //float dpx = (-pex-pkx);
  //float dpy = (-pey-pky);
  //float dpz = pi-pez-pkz;
  
   // by Toshi for test
  float dpx = (pi*sin(beamang_theta)*cos(beamang_phi)-pex-pkx);
  float dpy = (pi*sin(beamang_theta)*sin(beamang_phi)-pey-pky);
  float dpz =  pi*cos(beamang_theta)-pez-pkz;
   
   double dp2 = dpx*dpx+dpy*dpy+dpz*dpz;
   mm = sqrt(dE2-dp2)-mcore+dmm;
   //mm-=(-xpe*3e-2+0.5*(-ype+0.1)*(-ype+0.1));
   return mm;
}

double corRaster(float* P, double xt, double yt)
{
   double Y=0.;
   double x=1.; 
   int npar=0;
   int nmat=3;
   int nXt=3,nYt=3;
   int a=0,b=0;

   for (int n=0;n<nmat+1;n++){
      for (b=0;b<n+1;b++){
         for (a=0;a<n+1;a++){
            if (a+b==n){
               if (a<=nXt && b<=nYt){
                  x = pow(xt,double(a))*pow(yt,double(b));
               }
               else{
                  x = 0.;
               }
               Y += x*P[npar];
               npar++;
            }
         }
      }
   }

   return Y;
}

double calcf2t(double* P, double xf, double xpf,double yf, double ypf)
{
   double Y=0.;
   double x=1.; 
   int npar=0;
   int a=0,b=0,c=0,d=0;

   for (int n=0;n<nMat+1;n++){
      for (d=0;d<n+1;d++){
         for (c=0;c<n+1;c++){
            for (b=0;b<n+1;b++){
               for (a=0;a<n+1;a++){

                  if (a+b+c+d==n){
                     //if (a<=nXf && b<=nXpf && c<=nYf && d<=nYpf){
                        x = pow(xf,double(a))*pow(xpf,double(b))*
                           pow(yf,double(c))*pow(ypf,double(d));
                     //}
                     //else{
                        //x = 0.;
                     //}
                     Y += x*P[npar];
                     npar++;
                  }

               }
            }
         }
      }
   }

   return Y;
}

double calct2f(double* P, double xpt, double ypt, double mom)
{

   double Y=0.;
   double x=1.; 
   Int_t npar=0;
   Int_t a=0,b=0,c=0;

   for (Int_t n=0;n<nMatF+1;n++){
      for (c=0;c<n+1;c++){
         for (b=0;b<n+1;b++){
            for (a=0;a<n+1;a++){

               if (a+b+c==n){
                  if (a<=nXptF && b<=nYptF && c<=nMomF ){
                     x = pow(xpt,double(a))*pow(ypt,double(b))*
                        pow(mom,double(c));
                  }
                  else{
                     x = 0.;
                  }
                  Y += x*P[npar];
                  npar++;
               }

            }
         }
      }
   }

   return Y;
}

float Gaus(float x, float mu, float sigma, float aa)
{
   return aa/sqrt(2*3.1415)/sigma*exp(-pow((x-mu)/sigma,2)/2);
}

float GausP1(float x, float mu, float sigma, float aa, float p1, float p2)
{
   return aa/sqrt(2*3.1415)/sigma*exp(-pow((x-mu)/sigma,2)/2)+p1*x+p2;
}


