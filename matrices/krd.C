/*

  krd.C
  modified by Toshi , 19/May/2013
  
*/

#define coin_cxx
#include <iostream>
#include <fstream>
#include <TH2.h>
#include <TStyle.h>
#include <TCanvas.h>
#include <TVector3.h>
#include <TLorentzVector.h>
#include <math.h>
#include "coin.h"
#include "krd.h"
#include "constant.h"
using namespace std;
extern char* ofilename;

void coin::Loop()
{
   if (fChain == 0) return;

   Long64_t nentries = fChain->GetEntriesFast();

   Long64_t nbytes = 0, nb = 0;
   float kmom=0.;
   float kxpt=0.;
   float kypt=0.;
   float kxss=0.;
   float kyss=0.;
   float emom=0.;
   float expt=0.;
   float eypt=0.;
   float exss=0.;
   float eyss=0.;
   float x=0.,xp=0.;
   float y=0.,yp=0.;
   float xt=0.,yt=0.;
   float mom=0.;
   float mm=0.;
   float ctime=0.;
   float q2=0.,omega=0.;
   float etheta=0.,ephi=0.;
   float ktheta=0.,kphi=0.;
   float gktheta=0.,getheta=0.;
   float kthetaCM=0.,ethetaCM=0.;
   float gkthetaCM=0.,gethetaCM=0.;
   float mtar=0.,mcore=0.,Wgk=0.;
   int Nkaon=0;
   int TargetFlag=0;
   float plK[70];
   float plE[70];
   float momK0[210];
   float xptK0[210];
   float yptK0[210];
   float momK1[210];
   float xptK1[210];
   float yptK1[210];
   float momE[210];
   float xptE[210];
   float yptE[210];
   float xssE[70];
   float yssE[70];
   float xssK[70];
   float yssK[70];
   float rasmomE[10];
   float rasxptE[10];
   float rasyptE[10];
   float rasmomK[10];
   float rasxptK[10];
   float rasyptK[10];
   char str_plK[100];
   char str_plE[100];
   char str_momK0[100];
   char str_xptK0[100];
   char str_yptK0[100];
   char str_momK1[100];
   char str_xptK1[100];
   char str_yptK1[100];
   char str_xssK[100];
   char str_yssK[100];
   char str_rasmomE[100];
   char str_rasxptE[100];
   char str_rasyptE[100];
   char str_rasmomK[100];
   char str_rasxptK[100];
   char str_rasyptK[100];
   char str_momE[100];
   char str_xptE[100];
   char str_yptE[100];
   char str_xssE[100];
   char str_yssE[100];

   cout << "Now creating "<<ofilename << endl;
   TFile* newfile = new TFile(ofilename,"create");
   //TFile* newfile = new TFile("tmp.root","recreate");
   TTree* newtree = fChain->CloneTree(0);
   newtree->Branch("kmom",&kmom,"kmom/F");
   newtree->Branch("kxpt",&kxpt,"kxpt/F");
   newtree->Branch("kypt",&kypt,"kypt/F");
   newtree->Branch("ktheta",&ktheta,"ktheta/F");
   newtree->Branch("gktheta",&gktheta,"gktheta/F");
   newtree->Branch("gkthetaCM",&gkthetaCM,"gkthetaCM/F");
   newtree->Branch("kphi",&kphi,"kphi/F");
   newtree->Branch("kxss",&kxss,"kxss/F");
   newtree->Branch("kyss",&kyss,"kyss/F");
   newtree->Branch("emom",&emom,"emom/F");
   newtree->Branch("expt",&expt,"expt/F");
   newtree->Branch("eypt",&eypt,"eypt/F");
   newtree->Branch("etheta",&etheta,"etheta/F");
   newtree->Branch("getheta",&getheta,"getheta/F");
   newtree->Branch("gethetaCM",&gethetaCM,"gethetaCM/F");
   newtree->Branch("ephi",&ephi,"ephi/F");
   newtree->Branch("exss",&exss,"exss/F");
   newtree->Branch("eyss",&eyss,"eyss/F");
   newtree->Branch("mm",&mm,"mm/F");
   newtree->Branch("ctime",&ctime,"ctime/F");
   newtree->Branch("omega",&omega,"omega/F");
   newtree->Branch("q2",&q2,"q2/F");
   newtree->Branch("Wgk",&Wgk,"Wgk/F");

   // ===================================//
   // ======= Raster Corrections ========//
   // ======= (T.Gogami , 19/May/2013) ==//
   //cout << " ---Raster Corrections---" << endl;
   const int nrc = 20;
   double rcparx[nrc];
   double rcpary[nrc] , temp;
   char rc_fnamex[300],rc_fnamey[300];
   char tempc[300];
   int nrcx,nrcy;
   ifstream* data_mmxt = new ifstream("../fitlog/fmmxt_fit.dat");
   ifstream* data_mmyt = new ifstream("../fitlog/fmmyt_fit.dat");
   // --- mm vs. gbeamx --- //
   if(!*data_mmxt){
     ifstream* data_mmxt_ = new ifstream("fmmxt_fit.dat");
     if(!*data_mmxt_){
       cout << " There is no input file for Raster Correction (x) " << endl;
     }
     else{
       *data_mmxt_ >> tempc >> rc_fnamex;
       *data_mmxt_ >> nrcx;
       for(int j=0 ; j<nrcx ; j++){
	 *data_mmxt_ >> rcparx[j] >> temp;
       }
     }
     data_mmxt_->close();
   }
   else{
     *data_mmxt >> tempc >> rc_fnamex;
     *data_mmxt >> nrcx;
     for(int j=0 ; j<nrcx ; j++){
       *data_mmxt >> rcparx[j] >> temp;
     }
   }
   data_mmxt->close();
   // --- mm vs. gbeamy --- //
   if(!*data_mmyt){
     ifstream* data_mmyt_ = new ifstream("fmmyt_fit.dat");
     if(!*data_mmyt_){
       cout << " There is no input file for Raster Correction (y) " << endl;
     }
     else{
       *data_mmyt_ >> tempc >> rc_fnamey;
       *data_mmyt_ >> nrcy;
       for(int j=0 ; j<nrcy ; j++){
	 *data_mmyt_ >> rcpary[j] >> temp;
       }
     }
     data_mmyt_->close();
   }
   else{
     *data_mmyt >> tempc >> rc_fnamey;
     *data_mmyt >> nrcy;
     for(int j=0 ; j<nrcy ; j++){
       *data_mmyt >> rcpary[j] >> temp;
     }
   }
   data_mmyt->close();
   //for(int i =0 ; i<2 ; i++){
   //  cout << rcparx[i] <<  " " << rcpary[i] << endl;
   //}
   // =============== END (Raster Correction) =====//
   
   
   ifstream data("reconData.dat");
   data >> str_plK;
   data >> str_plE;
   data >> str_momK0;
   data >> str_xptK0;
   data >> str_yptK0;
   data >> str_momK1;
   data >> str_xptK1;
   data >> str_yptK1;
   data >> str_momE;
   data >> str_xptE;
   data >> str_yptE;
   data >> str_xssE;
   data >> str_yssE;
   data >> str_xssK;
   data >> str_yssK;
   data >> str_rasmomE;
   data >> str_rasxptE;
   data >> str_rasyptE;
   data >> str_rasmomK;
   data >> str_rasxptK;
   data >> str_rasyptK;
   data >> TargetFlag;
   data.close();
   ifstream data_plK(str_plK);
   ifstream data_plE(str_plE);
   ifstream data_momK0(str_momK0);
   ifstream data_xptK0(str_xptK0);
   ifstream data_yptK0(str_yptK0);
   ifstream data_momK1(str_momK1);
   ifstream data_xptK1(str_xptK1);
   ifstream data_yptK1(str_yptK1);
   ifstream data_momE(str_momE);
   ifstream data_xptE(str_xptE);
   ifstream data_yptE(str_yptE);
   ifstream data_xssE(str_xssE);
   ifstream data_yssE(str_yssE);
   ifstream data_xssK(str_xssK);
   ifstream data_yssK(str_yssK);
   ifstream data_rasmomE(str_rasmomE);
   ifstream data_rasxptE(str_rasxptE);
   ifstream data_rasyptE(str_rasyptE);
   ifstream data_rasmomK(str_rasmomK);
   ifstream data_rasxptK(str_rasxptK);
   ifstream data_rasyptK(str_rasyptK);
   for (int i=0;i<35;i++){
      plK[i]=0.;
      plE[i]=0.;
      xssE[i]=0.;
      yssE[i]=0.;
      xssK[i]=0.;
      yssK[i]=0.;
   }
   for (int i=0;i<210;i++){
      momK0[i]=0.;
      momK1[i]=0.;
      xptK0[i]=0.;
      xptK1[i]=0.;
      yptK0[i]=0.;
      yptK1[i]=0.;
      momE[i]=0.;
      xptE[i]=0.;
      yptE[i]=0.;
   }
   for (int i=0;i<10;i++){
      rasmomE[i]=0.;
      rasxptE[i]=0.;
      rasyptE[i]=0.;
      rasmomK[i]=0.;
      rasxptK[i]=0.;
      rasyptK[i]=0.;
   }
   for (int i=0;i<35;i++){
      int aaa;
      data_plK >> plK[i]>>aaa >>aaa >>aaa >>aaa;
      data_plE >> plE[i]>>aaa >>aaa >>aaa >>aaa;
      data_xssE >> xssE[i]>>aaa >>aaa >>aaa;
      data_yssE >> yssE[i]>>aaa >>aaa >>aaa;
      data_xssK >> xssK[i]>>aaa >>aaa >>aaa;
      data_yssK >> yssK[i]>>aaa >>aaa >>aaa;
      //cout << pK[i] <<" "<<pE[i]<<endl;
   }
   for (int i=0;i<210;i++){
      int aaa;
      data_momK0 >> momK0[i]>>aaa >>aaa >>aaa >>aaa;
      data_momK1 >> momK1[i]>>aaa >>aaa >>aaa >>aaa;
      data_xptK0 >> xptK0[i]>>aaa >>aaa >>aaa >>aaa;
      data_xptK1 >> xptK1[i]>>aaa >>aaa >>aaa >>aaa;
      data_yptK0 >> yptK0[i]>>aaa >>aaa >>aaa >>aaa;
      data_yptK1 >> yptK1[i]>>aaa >>aaa >>aaa >>aaa;
      data_momE >> momE[i]>>aaa >>aaa >>aaa >>aaa;
      data_xptE >> xptE[i]>>aaa >>aaa >>aaa >>aaa;
      data_yptE >> yptE[i]>>aaa >>aaa >>aaa >>aaa;
   }
   for (int i=0;i<10;i++){
      int aaa;
      data_rasmomE >> rasmomE[i]>>aaa >>aaa ;
      data_rasxptE >> rasxptE[i]>>aaa >>aaa ;
      data_rasyptE >> rasyptE[i]>>aaa >>aaa ;
      data_rasmomK >> rasmomK[i]>>aaa >>aaa ;
      data_rasxptK >> rasxptK[i]>>aaa >>aaa ;
      data_rasyptK >> rasyptK[i]>>aaa >>aaa ;
   }
   data_plK.close();
   data_plE.close();
   data_momK0.close();
   data_xptK0.close();
   data_yptK0.close();
   data_momK1.close();
   data_xptK1.close();
   data_yptK1.close();
   data_momE.close();
   data_xptE.close();
   data_yptE.close();
   data_xssE.close();
   data_yssE.close();
   data_xssK.close();
   data_yssK.close();
   data_rasmomE.close();
   data_rasxptE.close();
   data_rasyptE.close();
   data_rasmomK.close();
   data_rasxptK.close();
   data_rasyptK.close();

   bool momcut = false;
   for (Long64_t jentry=0; jentry<nentries;jentry++) {
     Long64_t ientry = LoadTree(jentry);
     if (ientry < 0) break;
     nb = fChain->GetEntry(jentry);   nbytes += nb;
     // if (Cut(ientry) < 0) continue;
     //if ( ((hwatnpe1+hwatnpe2>70.)|| 
     //      (hwatnum1<=6 && hwatnum2<=6 && hwatnpe1+hwatnpe2>20.)) && 
     if ( 
	 (hwatnkn1+hwatnkn2>0.5) &&
	 haernpe1+haernpe2+haernpe3<20. &&
	 fabs(hmsq-0.25)<0.35//&& 
	 //hsxpfp<2.5e-3*hsxfp+0.15//&&
	 //((TargetFlag!=1 && fabs(0.7*hsxfp+esxfp)<30)||TargetFlag==1) 
	 //&& runnum>=76050 && runnum<76825
	  ){
       htimetar-=calcf2t(plK,hsxfp,hsxpfp,hsyfp,hsypfp,3);
       etimetar-=calcf2t(plE,esxfp,esxpfp,esyfp,esypfp,3);
       ctime=htimetar-etimetar;
       if (fabs(ctime+54.95)<8){
	 x=(hsxfp-0)/60.;
	 xp=(hsxpfp+0.0)/0.15;
	 y=(hsyfp-0.0)/6.;
	 yp=(hsypfp-0)/0.01;
	 //kmom=1.18219+x*0.172137;
	 //if (hsxpfp>-0.1 && hsxpfp<0.05){
	 //if (hsxfp>-20. && hsxfp<20.){
	 //if (hsxfp>-20. ){
	 kmom=calcf2t(momK0,x,xp,y,yp,6);
	 kxpt=calcf2t(xptK0,x,xp,y,yp,6);
	 kypt=calcf2t(yptK0,x,xp,y,yp,6);
	 /*}
	   else{
	   kmom=calcf2t(momK1,x,xp,y,yp,6);
	   kxpt=calcf2t(xptK1,x,xp,y,yp,6);
	   kypt=calcf2t(yptK1,x,xp,y,yp,6);
	   }*/
	 /*kmom=hsp;
	   kxpt=hsxptar;
	   kypt=hsyptar;*/
	 //kxpt-=(kxpt+0.15)*0.06;
	 ktheta=acos(1/sqrt(1+kxpt*kxpt+kypt*kypt));
	 //kphi=atan2(kxpt,kypt);
	 kphi=atan2(kypt,kxpt);//Toshi , 22/Sep/2012
	 xp=(kxpt-0)/0.1;
	 yp=(kypt-0)/0.05;
	 mom=(kmom-1.2)/0.15;
	 kxss=calct2s(xssK,xp,yp,mom,4);
	 kyss=calct2s(yssK,xp,yp,mom,4);
	 x=(esxfp-0)/25.;
	 xp=(esxpfp+0.0)/0.1;
	 y=(esyfp-0)/3.;
	 yp=(esypfp+0.03)/0.01;
	 /*x=(-4.929629-0)/25.;
	   xp=(-0.000945+0.0)/0.1;
	   y=(-0.242557-0)/3.;
	   yp=(-0.024784+0.03)/0.01;*/
	 emom=calcf2t(momE,x,xp,y,yp,6);
	 //emom=0.84159+x*0.0979697;
	 expt=calcf2t(xptE,x,xp,y,yp,6);
	 eypt=calcf2t(yptE,x,xp,y,yp,6);
	 //cout << expt << endl;
	 /*emom=esp;
	   expt=esxptar;
	   eypt=esyptar;*/
	 etheta=acos(1/sqrt(1+expt*expt+eypt*eypt));
	 //ephi=atan2(expt,eypt);
	 ephi=atan2(eypt,expt);//toshi , 22/Sep/2012
	 xp=(expt-0)/0.1;
	 yp=(eypt+0.06)/0.03;
	 mom=(emom-0.844)/0.2;
	 exss=calct2s(xssE,xp,yp,mom,4);
	 eyss=calct2s(yssE,xp,yp,mom,4);
	 // ========== Momentum cut (Toshi , 5Feb2013) ======= ///
	 if( TargetFlag==1 || TargetFlag==112 || TargetFlag==101 ){
	   if(emom>-0.9*kmom+1.75 && emom<-0.9*kmom+1.98){
	     momcut = true;
	   }
	   else momcut = false; 
	 }
	 else{
	   if(emom>-0.9*kmom+1.87 && emom<-0.9*kmom+1.98){
	     momcut = true;
	   }
	   else momcut = false; 
	 }
	 // ==================================================== ///
	 //if( momcut == true ){
	 if( momcut == true || momcut==false){ // for mixed event analysis
	   float dmm=0.;
	   //if (TargetFlag==1 ||TargetFlag==112 || TargetFlag==7){
	   if ( TargetFlag==1 ||TargetFlag==112 ){ 
	     xt=gbeamx;
	     yt=gbeamy;
	     /*kmom-=corRaster(rasmomK,xt,yt);
	       kxpt-=corRaster(rasxptK,xt,yt);
	       kypt-=corRaster(rasyptK,xt,yt);
               emom-=corRaster(rasmomE,xt,yt);
               expt-=corRaster(rasxptE,xt,yt);
               eypt-=corRaster(rasyptE,xt,yt);*/
	     //dmm=-15e-3*xt; // original by Kawama
	     //dmm=0.0;//toshi for test
	     
	     // ==== by Toshi , 7Aug2012
	     //dmm =0.00770104 * (xt-(-0.000545202)) + -3.79849e-05;
	     //dmm+=0.000222721 * (yt-(0.000735494)) + -2.96895e-05;
	     // ==== by Toshi , 24Apr2013
	     //dmm =0+(0.00653387*xt);
	     //dmm+=0+(-0.00120051*yt);
	     //dmm = -1.0*dmm;//reverse the sign 
	     //rcparx[i]
	     // ==== by Toshi , 20May2013
	     //dmm =0.0 + ( rcparx[1] * xt );
	     //dmm+=0.0 + ( rcpary[1] * yt );
	     //dmm = -1.0*dmm;//reverse the sign 
	     // -==== Toshi 6July2013 for test=========
	     dmm = 0.00441113*xt - (0.0106527*xt*xt) 
	       - (0.57073*xt*xt*xt) + (4.20109*xt*xt*xt*xt);
	     dmm+= 0.000973207*yt - (0.0189651*yt*yt) 
	       - (0.0653128*yt*yt*yt)+ (1.30222*yt*yt*yt*yt);
	     dmm = -1.0*dmm;//reverse the sign
	   }
	   if ( TargetFlag==7 ){
	     xt=gbeamx/3.0;
	     yt=gbeamy;
	     // ==== by Toshi , 7Aug2012
	     //dmm =0.00770104 * (xt-(-0.000545202)) + -3.79849e-05;
	     //dmm+=0.000222721 * (yt-(0.000735494)) + -2.96895e-05;
	     // ==== by Toshi , 24Apr2013
	     //dmm =0+(0.00653387*xt);
	     //dmm+=0+(-0.00120051*yt);
	     //dmm = -1.0*dmm;//reverse the sign 
	     //rcparx[i]
	     // ==== by Toshi , 20May2013
	     //dmm =0.0 + ( rcparx[1] * xt/3.0 );
	     //dmm+=0.0 + ( rcpary[1] * yt );
	     //dmm = -1.0*dmm;//reverse the sign
	     // -==== Toshi 6July2013 for test=========should be divieded by 3.0 for gbeamx
//	     dmm = 0.00441113*xt - (0.0106527*xt*xt) 
//	       - (0.57073*xt*xt*xt) + (4.20109*xt*xt*xt*xt);
	     dmm = ( 0.00441113*xt - (0.0106527*xt*xt) 
		     - (0.57073*xt*xt*xt) + (4.20109*xt*xt*xt*xt) );
	     dmm+= 0.000973207*yt - (0.0189651*yt*yt) 
	       - (0.0653128*yt*yt*yt)+ (1.30222*yt*yt*yt*yt);
	     dmm = -1.0*dmm;//reverse the sign
	   }
	   if ((runnum>=76222 && runnum<=76290)||(runnum>=76310 && runnum<=76315)){
	     hallcp=2344.08;
	   }
	   mm=GetMM(hallcp*1e-3, kxpt, kypt, kmom, 
		    -expt, -eypt, emom,TargetFlag)+dmm;
	   //mm-=(expt*3e-2+0.5*(eypt+0.1)*(eypt+0.1));
	   //cout<< mm << endl;
	   switch(TargetFlag)
	     {
	     case 1:
	       mtar=mp;
	       mcore=0.;
	       break;	
	     case 101:
	       mtar=mp;
	       mcore=0.;
	       break;	
	     case 102:
	       mtar=mp;
	       mcore=0.;
	       break;	
	     case 112:
	       mtar=mC12;
	       mcore=mB11;
	       break;	
	     case 6:
	       mtar=mLi6;
	       mcore=mHe5;
	       break;	
	     case 7:
	       mtar=mLi7;
	       mcore=mHe6;
	       break;
	     case 9:
	       mtar=mBe9;
	       mcore=mLi8;
	       break;	
	     case 10:
	       mtar=mB10;
	       mcore=mBe9;
	       break;	
	     case 12:
	       mtar=mC12;
	       mcore=mB11;
	       break;	
	     case 52:
	       mtar=mCr52;
	       mcore=mV51;
	       break;	
	     default:
	       cout << "put correct TargetFlag"<<endl;	
	       break;
	     }
	   float Ei=sqrt(hallcp*1e-3*hallcp*1e-3+me*me);
	   omega=sqrt(hallcp*1e-3*hallcp*1e-3+me*me)-
	     sqrt(emom*emom-me*me);
	   float qx=-emom*expt/sqrt(1+expt*expt+eypt*eypt);
	   float qy=-emom*eypt/sqrt(1+expt*expt+eypt*eypt);
	   float qz=hallcp*1e-3-emom/sqrt(1+expt*expt+eypt*eypt);
	   //q2=omega*omega-(qx*qx+qy*qy+qz*qz);
	   TLorentzVector vq(qx,qy,qz,omega);
	   q2=vq.Mag2();
	   float ex=-qx;
	   float ey=-qy;
	   float ez=emom/sqrt(1+expt*expt+eypt*eypt);
	   float ee=sqrt(emom*emom+me*me);
	   TLorentzVector ve(ex,ey,ez,ee);
	   float kx=kmom*kxpt/sqrt(1+kxpt*kxpt+kypt*kypt);
	   float ky=kmom*kypt/sqrt(1+kxpt*kxpt+kypt*kypt);
	   float kz=kmom/sqrt(1+kxpt*kxpt+kypt*kypt);
	   float ke=sqrt(kmom*kmom+mk*mk);
	   TLorentzVector vk(kx,ky,kz,ke);
	   gktheta=vk.Angle(vq.Vect());
	   getheta=ve.Angle(vq.Vect());
	   float beta=hallcp*1e-3/(Ei+mtar);
	   float gamma=1/sqrt(1-beta*beta);
	   TLorentzVector vqCM(qx,qy,gamma*(qz-beta*omega),
			       gamma*(omega-beta*qz));
	   TLorentzVector veCM(ex,ey,gamma*(ez-beta*ee),
			       gamma*(ee-beta*qz));
	   TLorentzVector vkCM(kx,ky,gamma*(kz-beta*ke),
			       gamma*(ke-beta*qz));
	   gkthetaCM=vkCM.Angle(vqCM.Vect());
	   gethetaCM=veCM.Angle(vqCM.Vect());
	   Wgk = sqrt(2*omega*mtar+mtar*mtar);
	   Nkaon++;
	   newtree->Fill();
	 }
       }
     }
   }
   cout << "Select "<<Nkaon <<" kaon events ..."<< endl;
   
   //newtree->Print();
   newtree->Write();
   //delete newfile;
}

float GetMM(float Ei,float xpk,float ypk,float pk,
	    float xpe,float ype,float pe,int TargetFlag)
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
  float elosscale = 1.0;//toshi
  //float elosscale = 1.5;//toshi
  //float elosscale = 1.1;//toshi
  //float elosscale = 0.9;//toshi
  //float elosscale = 0.8;//toshi
  //float elosscale = 0.7;//toshi
  //float elosscale = 0.6;//toshi
  //float elosscale2 = 1.3;//toshi
  //float elosscale2 = 1.2;//toshi
  //float elosscale2 = 1.1;//toshi
  //float elosscale2 = 1.0;//toshi
  //float elosscale2 = 0.9;//toshi
  //float elosscale2 = 0.8;//toshi
  //float elosscale2 = 0.7;//toshi
  //float elosscale2 = 1.164;// The first Results (Toshi,27May2013)
  //float elosscale2 = 1.164;// The first Results (Toshi,27May2013)
  //float elosscale2 = 0.996271;// The socond Results (Toshi,31May2013)
  //float elosscale2 = 0.996797;// Toshi , 4June2013
  //float elosscale2 = 0.967883; // Toshi , 15June2013
  //float elosscale2 = 1.15;// Toshi , 15Sep2013 for test, 11.8 MeV
  //float elosscale2 = 0.86522;// Toshi , 12Sep2013, 11.5 MeV
  //float elosscale2 = 0.868055;// Toshi , 13Sep2013, 11.52 MeV
  //float elosscale2 = 0.747682;// Toshi , 13Sep2013, 11.4 MeV
  //=====
  //float elosscale2 = 0.890462;// Toshi , 17Sep2013, 10.5 MeV for test
  //====
  //float elosscale2 = 1.0;// Toshi , 17Dec2013 for test !!!!!! 
  float elosscale2 = 0.97;// Toshi , 17Dec2013 for test !!!!!! 

  float b_scale = 1.0; // Toshi , 12/Sep/2013 for test
  //float h2o_scale = 1.38; // + 150 keV 
  //float h2o_scale = 1.35; // + 100 keV 
  //float h2o_scale = 1.30; // + 150 keV 
  //float h2o_scale = 1.33; // + 150 keV 
  //float h2o_scale = 1.25; // + 150 keV 
  //float h2o_scale = 1.20; // 
  //float h2o_scale = 1.15; // + 200 keV 
  //float h2o_scale = 1.00; // + 200 keV 
  //float h2o_scale = 1.40; // + 200 keV 
  //float h2o_scale = 1.50; // + 200 keV 
  float h2o_scale = 1.55; // Toshi , 25Dec2013
  
  switch(TargetFlag)
    {
    case 1:
      mtar=mp;
      mcore=0.;
      //EEloss=440e-6*0.8;
      //KEloss=502e-6*0.8;
      //BEloss=436e-6*0.8;
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
      EEloss=111e-6*elosscale;// Toshi , 2/Dec/2012
      KEloss=126e-6*elosscale;// Toshi , 2/Dec/2012
      BEloss=110e-6*elosscale;// Toshi , 2/Dec/2012
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
      //EEloss=34e-6*elosscale*b_scale;// Toshi , 2/Dec/2012
      //KEloss=38e-6*elosscale*b_scale;// Toshi , 2/Dec/2012
      //BEloss=34e-6*elosscale*b_scale;// Toshi , 2/Dec/2012
      EEloss=34e-6*elosscale*b_scale;// Toshi , 16/Dec/2013
      KEloss=43e-6*elosscale*b_scale;// Toshi , 16/Dec/2013
      BEloss=34e-6*elosscale*b_scale;// Toshi , 16/Dec/2013
      break;	
    case 12:
      mtar=mC12;
      mcore=mB11;
      //EEloss=90e-6;//kawama
      //KEloss=103e-6;//kawama
      //BEloss=89e-6;//kawama
      //EEloss=58e-6;// Toshi , 9/Nov/2012
      //KEloss=77e-6;// Toshi , 9/Nov/2012
      //BEloss=66e-6;// Toshi , 9/Nov/2012
      //EEloss=71e-6*elosscale;// Toshi , 2/Dec/2012
      //KEloss=81e-6*elosscale;// Toshi , 2/Dec/2012
      //BEloss=71e-6*elosscale;// Toshi , 2/Dec/2012
      EEloss=55e-6*elosscale;// Toshi , 16/Dec/2013
      KEloss=68e-6*elosscale;// Toshi , 16/Dec/2013
      BEloss=55e-6*elosscale;// Toshi , 16/Dec/2013
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
      //EEloss=373e-6;// Toshi , 9/Nov/2012
      //KEloss=408e-6;// Toshi , 9/Nov/2012
      //BEloss=371e-6;// Toshi , 9/Nov/2012
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
      ///EEloss=296e-6*elosscale;// Toshi , 9/Nov/2012
      ///KEloss=416e-6*elosscale;// Toshi , 9/Nov/2012
      ///BEloss=349e-6*elosscale;// Toshi , 9/Nov/2012
      //EEloss=372e-6*elosscale*elosscale2;// Toshi , 2/Dec/2012
      //KEloss=429e-6*elosscale*elosscale2;// Toshi , 2/Dec/2012
      //BEloss=372e-6*elosscale*elosscale2;// Toshi , 2/Dec/2012
      EEloss=374e-6*elosscale*elosscale2;// Toshi , 16/Dec/2013
      KEloss=465e-6*elosscale*elosscale2;// Toshi , 16/Dec/2013
      BEloss=374e-6*elosscale*elosscale2;// Toshi , 16/Dec/2013
      //dmm=-120e-6;
      break;	
    default:
      cout << " .... Please Put Correct TargetFlag."<<endl;	
      return 0.;
      break;
    }
  
  //float beamang_theta = 0.0;
  //float beamang_theta = 2.38743e-3; // the first results (Toshi,27May2013)
  //float beamang_theta = 3.91811e-3; // the second results (Toshi,28May2013)
  //float beamang_theta = 2.48098e-3; // ( 1st, Toshi,29May2013 )
  //float beamang_theta = 3.84556e-3; // ( 2nd, Toshi,29May2013 )
  //float beamang_theta = 3.79551e-3; // ( Toshi , 4June2013 )
  //float beamang_theta = 3.06066e-3; // ( Toshi , 15June2013 )
  //float beamang_theta = 4.25934e-3; // ( Toshi , 12Sep2013 )
  //float beamang_theta = 3.89069e-3; // ( Toshi , 12Sep2013 ), 11.5 MeV
  //float beamang_theta = 4.25934e-3; // ( Toshi , 13Sep2013 ), 11.4 MeV
  //float beamang_theta = 4.62799e-3; // ( Toshi , 13Sep2013 ), 11.3 MeV
  //float beamang_theta = 4.25934e-3; // ( Toshi , 13Sep2013 ), 11.4 MeV
  //float beamang_theta = 5.55e-3; // ( Toshi , 13Sep2013 ), 10.5 MeV for test
  float beamang_theta = 5.30e-3; // ( Toshi , 13Sep2013 ), test
  //float beamang_theta = 5.00e-3; // ( Toshi , 13Sep2013 ), test
  //float beamang_theta = 1.0*1.0e-3;
  //float beamang_theta = 2.0*1.0e-3;
  //float beamang_theta = 3.0*1.0e-3;
  //float beamang_theta = 4.0*1.0e-3;
  //float beamang_theta = 5.0*1.0e-3;
  //float beamang_theta = 6.0*1.0e-3;
  //float beamang_theta = 6.2*1.0e-3;
  //float beamang_theta = 6.3*1.0e-3;
  //float beamang_theta = 6.5*1.0e-3;
  //float beamang_theta = 6.3*1.0e-3;
  //float beamang_theta = 6.7*1.0e-3;
  //float beamang_theta = 7.0*1.0e-3;
  float beamang_phi = 0.0;
  //float beamang_phi = 3.14159/2.0;     // PI/2.0 : 1.6
  //float beamang_phi = 3.14159;         // PI : 3.1
  //float beamang_phi = 3.14159*3.0/2.0; // 3/2*PI : 4.7
  //float beamang_phi = 3.14159*2.0;     // 2*PI : 6.3
  
  //float beamang_phi = 3.14159/4.0;     // PI/2.0 : 1.6
  //float beamang_phi = 3.14159/16.0*15.0;     // PI/2.0 : 1.6
  
  float beam_xp = tan(beamang_theta)*cos(beamang_phi);
  float beam_yp = tan(beamang_theta)*sin(beamang_phi);
  xpk = xpk - beam_xp;
  ypk = ypk - beam_yp;
  xpe = xpe + beam_xp;
  ype = ype + beam_yp;
  //xpe = xpe - beam_xp; // Toshi , 29/May/2013
  //ype = ype - beam_yp; // Toshi , 29/May/2013
    
  float Ek = sqrt(pk*pk+mk*mk)+KEloss;
  pk=sqrt(Ek*Ek-mk*mk);
  float pkz = pk/sqrt(1+xpk*xpk+ypk*ypk);
  float pkx = pkz*xpk;
  float pky = pkz*ypk;
  float Ee = sqrt(pe*pe+me*me)+EEloss;
  pe=sqrt(Ee*Ee-me*me);
  float pez = pe/sqrt(1+xpe*xpe+ype*ype);
  float pex = pez*xpe;
  float pey = pez*ype;
  Ei=Ei-BEloss;
  float pi = sqrt(Ei*Ei-me*me);
  float dE = (Ei+mtar-Ek-Ee);
  float dE2 = dE*dE;
  /*float dpx = (pi*sin(6.0e-3)-pex-pkx);
    float dpy = (-pey-pky);
    float dpz = (pi*cos(6.0e-3)-pez-pkz);*/
  
  //---- Kawama ------
  //float dpx = (pi*cos(-4.0e-3)*sin(6.5e-3)-pex-pkx);
  //float dpy = (pi*sin(-4.0e-3)-pey-pky);
  //float dpz = (pi*cos(-4.0e-3)*cos(6.5e-3)-pez-pkz);
  
  //---- Toshi for test----- (26Apr2013)
  //float dpx = (-pex-pkx);
  //float dpy = (-pey-pky);
  //float dpz = pi-pez-pkz;
  
  // by Toshi for test
  float dpx = (pi*sin(beamang_theta)*cos(beamang_phi)-pex-pkx);
  float dpy = (pi*sin(beamang_theta)*sin(beamang_phi)-pey-pky);
  float dpz =  pi*cos(beamang_theta)-pez-pkz;
  
  float dp2 = dpx*dpx+dpy*dpy+dpz*dpz;
  
  mm = sqrt(dE2-dp2)-mcore+dmm;
  return mm;
}

/*const int nMat=3;
  const int nXf=3;
  const int nXpf=3;
  const int nYf=3;
  const int nYpf=3;*/
//////////////////////////////////////////////////
float calcf2t(float* P, float xf, float xpf, 
      float yf, float ypf, int nMat)
//////////////////////////////////////////////////
{

   float Y=0.;
   float x=1.; 
   Int_t npar=0;
   Int_t a=0,b=0,c=0,d=0;

   for (int n=0;n<nMat+1;n++){
      for (d=0;d<n+1;d++){
         for (c=0;c<n+1;c++){
            for (b=0;b<n+1;b++){
               for (a=0;a<n+1;a++){

                  if (a+b+c+d==n){
                     //if (a<=nXf && b<=nXpf && c<=nYf && d<=nYpf){
                     x = pow(xf,float(a))*pow(xpf,float(b))*
                        pow(yf,float(c))*pow(ypf,float(d));
                     //}
                     //else{
                     //  x = 0.;
                     //}
                     //cout <<npar<<" "<< x <<endl;
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

//////////////////////////////////////////////////
float calct2s(float* P, float xpt, float ypt, float mom, int nMat)
   //////////////////////////////////////////////////
{

   float Y=0.;
   float x=1.; 
   Int_t npar=0;
   Int_t a=0,b=0,c=0;

   for (Int_t n=0;n<nMat+1;n++){
      for (c=0;c<n+1;c++){
         for (b=0;b<n+1;b++){
            for (a=0;a<n+1;a++){

               if (a+b+c==n){
                  //if (a<=nXpt && b<=nYpt && c<=nMom ){
                  x = pow(xpt,float(a))*pow(ypt,float(b))*
                     pow(mom,float(c));
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

   return Y;
}

/////////////////////////////////////////////////
float corRaster(float* P, float xt, float yt)
   /////////////////////////////////////////////////
{

   float Y=0.;
   float x=1.; 
   Int_t npar=0;
   Int_t nMat=3;
   Int_t nXt=3,nYt=3;
   Int_t a=0,b=0;

   for (Int_t n=0;n<nMat+1;n++){
      for (b=0;b<n+1;b++){
         for (a=0;a<n+1;a++){
            if (a+b==n){
               if (a<=nXt && b<=nYt){
                  x = pow(xt,float(a))*pow(yt,float(b));
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

