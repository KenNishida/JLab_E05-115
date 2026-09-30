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

   for (Long64_t jentry=0; jentry<nentries;jentry++) {
      Long64_t ientry = LoadTree(jentry);
      if (ientry < 0) break;
      nb = fChain->GetEntry(jentry);   nbytes += nb;
      // if (Cut(ientry) < 0) continue;
      //if ( ((hwatnpe1+hwatnpe2>70.)|| 
      //      (hwatnum1<=6 && hwatnum2<=6 && hwatnpe1+hwatnpe2>20.)) && 
      if ( 
            (hwatnkn1+hwatnkn2>0.5) &&
            haernpe1+haernpe2+haernpe3<50. &&
            fabs(hmsq-0.25)<0.4&& 
            hsxpfp<3e-3*hsxfp+0.3 
            //&& runnum>=76050 && runnum<76825
         ){
         htimetar-=calcf2t(plK,hsxfp,hsxpfp,hsyfp,hsypfp,3);
         etimetar-=calcf2t(plE,esxfp,esxpfp,esyfp,esypfp,3);
         ctime=htimetar-etimetar;
         if (fabs(ctime+54.95)<10){
            x=(hsxfp-0)/60.;
            xp=(hsxpfp+0.0)/0.15;
            y=(hsyfp-0.0)/6.;
            yp=(hsypfp-0)/0.01;
            //kmom=1.18219+x*0.172137;
            //if (hsxpfp>-0.1 && hsxpfp<0.05){
            //if (hsxfp>-20. && hsxfp<20.){
            if (hsxfp>-20. ){
               kmom=calcf2t(momK0,x,xp,y,yp,6);
               kxpt=calcf2t(xptK0,x,xp,y,yp,6);
               kypt=calcf2t(yptK0,x,xp,y,yp,6);
            }
            else{
               kmom=calcf2t(momK1,x,xp,y,yp,6);
               kxpt=calcf2t(xptK1,x,xp,y,yp,6);
               kypt=calcf2t(yptK1,x,xp,y,yp,6);
            }
            /*kmom=hsp;
              kxpt=hsxptar;
              kypt=hsyptar;*/
            //kxpt-=(kxpt+0.15)*0.06;
            ktheta=acos(1/sqrt(1+kxpt*kxpt+kypt*kypt));
            kphi=atan2(kxpt,kypt);
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
            ephi=atan2(expt,eypt);
            xp=(expt-0)/0.1;
            yp=(eypt+0.06)/0.03;
            mom=(emom-0.844)/0.2;
            exss=calct2s(xssE,xp,yp,mom,4);
            eyss=calct2s(yssE,xp,yp,mom,4);
            if (TargetFlag==1 ||TargetFlag==112 || TargetFlag==7){
               xt=(gbeamx-0.63)/10;
               yt=(gbeamy-0.55)/2.5;
               kmom-=corRaster(rasmomK,xt,yt);
               kxpt-=corRaster(rasxptK,xt,yt);
               kypt-=corRaster(rasyptK,xt,yt);
               emom-=corRaster(rasmomE,xt,yt);
               expt-=corRaster(rasxptE,xt,yt);
               eypt-=corRaster(rasyptE,xt,yt);
            }
            mm=GetMM(hallcp*1e-3, kxpt, kypt, kmom, 
                  -expt, -eypt, emom,TargetFlag);
            mm-=(expt*3e-2+0.5*(eypt+0.1)*(eypt+0.1));
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
            if ((runnum>=76222 && runnum<=76290)||(runnum>=76310 && runnum<=76315)){
               hallcp=2344.08;
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
   cout << "Select "<<Nkaon <<" kaon events ..."<< endl;

   //newtree->Print();
   newtree->Write();
   //delete newfile;
}

double GetMM(double Ei,double xpk,double ypk,double pk,
      double xpe,double ype,double pe,int TargetFlag)
{
   double mm=0.;
   //double Ei = 2.344;
   double mtar = 0.;
   double mcore = 0.;
   int N=0;
   int Nentries=0;
   double KEloss=0.;
   double EEloss=0.;
   double BEloss=0.;

   switch(TargetFlag)
   {
      case 1:
         mtar=mp;
         mcore=0.;
         /*EEloss=730e-6;
           KEloss=805e-6;
           BEloss=750e-6;*/
         EEloss=351e-6;
         KEloss=301e-6;
         BEloss=363e-6;
         break;	
      case 101:
         mtar=mp;
         mcore=0.;
         EEloss=351e-6;
         KEloss=301e-6;
         BEloss=363e-6;
         break;	
      case 112:
         mtar=mC12;
         mcore=mB11;
         EEloss=351e-6;
         KEloss=301e-6;
         BEloss=363e-6;
         break;	
      case 6:
         mtar=mLi6;
         mcore=mHe5;
         break;	
      case 7:
         mtar=mLi7;
         mcore=mHe6;
         EEloss=173e-6;
         KEloss=149e-6;
         BEloss=178e-6;
         break;	
      case 10:
         mtar=mB10;
         mcore=mBe9;
         EEloss=49e-6;
         KEloss=41e-6;
         BEloss=51e-6;
         break;	
      case 12:
         mtar=mC12;
         mcore=mB11;
         EEloss=107e-6;
         KEloss=88e-6;
         BEloss=110e-6;
         break;	
      case 28:
         mtar=mSi28;
         mcore=mAl27;
         break;	
      case 52:
         mtar=mCr52;
         mcore=mV51;
         EEloss=96e-6;
         KEloss=75e-6;
         BEloss=99e-6;
         break;	
      case 89:
         mtar=mY89;
         mcore=mSr88;
         break;
      default:
         cout << "put correct TargetFlag"<<endl;	
         return 0.;
         break;
   }

   double Ek = sqrt(pk*pk+mk*mk)+KEloss;
   //pk=sqrt(Ek*Ek-mk*mk);
   double pkz = pk/sqrt(1+xpk*xpk+ypk*ypk);
   double pkx = pkz*xpk;
   double pky = pkz*ypk;
   double Ee = sqrt(pe*pe+me*me)+EEloss;
   //pe=sqrt(Ee*Ee-me*me);
   double pez = pe/sqrt(1+xpe*xpe+ype*ype);
   double pex = pez*xpe;
   double pey = pez*ype;
   Ei=Ei-BEloss;
   double pi = sqrt(Ei*Ei-me*me);
   double dE = (Ei+mtar-Ek-Ee);
   double dE2 = dE*dE;
   double dpx = (pi*sin(0.0)-pex-pkx);
   double dpy = (-pey-pky);
   double dpz = (pi*cos(0.0)-pez-pkz);
   double dp2 = dpx*dpx+dpy*dpy+dpz*dpz;
   mm = sqrt(dE2-dp2)-mcore;
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
double corRaster(float* P, double xt, double yt)
   /////////////////////////////////////////////////
{

   double Y=0.;
   double x=1.; 
   Int_t npar=0;
   Int_t nMat=3;
   Int_t nXt=3,nYt=3;
   Int_t a=0,b=0;

   for (Int_t n=0;n<nMat+1;n++){
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

