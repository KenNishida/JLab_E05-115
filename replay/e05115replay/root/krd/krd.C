#define coin_cxx
#include <iostream>
#include <fstream>
#include <TH2.h>
#include <TStyle.h>
#include <TCanvas.h>
#include <math.h>
#include "coin.h"
#include "krd.h"
#include "constant.h"
using namespace std;


void coin::Loop()
{
//   In a ROOT session, you can do:
//      Root > .L coin.C
//      Root > coin t
//      Root > t.GetEntry(12); // Fill t data members with entry number 12
//      Root > t.Show();       // Show values of entry 12
//      Root > t.Show(16);     // Read and show values of entry 16
//      Root > t.Loop();       // Loop on all entries
//

//     This is the loop skeleton where:
//    jentry is the global entry number in the chain
//    ientry is the entry number in the current Tree
//  Note that the argument to GetEntry must be:
//    jentry for TChain::GetEntry
//    ientry for TTree::GetEntry and TBranch::GetEntry
//
//       To read only selected branches, Insert statements like:
// METHOD1:
//    fChain->SetBranchStatus("*",0);  // disable all branches
//    fChain->SetBranchStatus("branchname",1);  // activate branchname
// METHOD2: replace line
//    fChain->GetEntry(jentry);       //read all branches
//by  b_branchname->GetEntry(ientry); //read only this branch
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
	float mom=0.;
	float mm=0.;
	float ctime=0.;
	int Nkaon=0;
	int TargetFlag=0;
	float plK[70];
	float plE[70];
	float momK[70];
	float momE[70];
	float xptK[70];
	float xptE[70];
	float yptK[70];
	float yptE[70];
	float xssE[70];
	float yssE[70];
	float xssK[70];
	float yssK[70];
	char str_plK[100];
	char str_plE[100];
	char str_momK[100];
	char str_xptK[100];
	char str_yptK[100];
	char str_xssK[100];
	char str_yssK[100];
	char str_momE[100];
	char str_xptE[100];
	char str_yptE[100];
	char str_xssE[100];
	char str_yssE[100];

	TFile* newfile = new TFile("tmp.root","recreate");
	TTree* newtree = fChain->CloneTree(0);
	newtree->Branch("kmom",&kmom,"kmom/F");
	newtree->Branch("kxpt",&kxpt,"kxpt/F");
	newtree->Branch("kypt",&kypt,"kypt/F");
	newtree->Branch("kxss",&kxss,"kxss/F");
	newtree->Branch("kyss",&kyss,"kyss/F");
	newtree->Branch("emom",&emom,"emom/F");
	newtree->Branch("expt",&expt,"expt/F");
	newtree->Branch("eypt",&eypt,"eypt/F");
	newtree->Branch("exss",&exss,"exss/F");
	newtree->Branch("eyss",&eyss,"eyss/F");
	newtree->Branch("mm",&mm,"mm/F");
	newtree->Branch("ctime",&ctime,"ctime/F");
 
	ifstream data("reconData.dat");
	data >> str_plK;
	data >> str_plE;
 	data >> str_momK;
 	data >> str_xptK;
 	data >> str_yptK;
 	data >> str_momE;
 	data >> str_xptE;
 	data >> str_yptE;
 	data >> str_xssE;
 	data >> str_yssE;
 	data >> str_xssK;
 	data >> str_yssK;
 	data >> TargetFlag;
	data.close();
	ifstream data_plK(str_plK);
 	ifstream data_plE(str_plE);
 	ifstream data_momK(str_momK);
 	ifstream data_xptK(str_xptK);
 	ifstream data_yptK(str_yptK);
 	ifstream data_momE(str_momE);
 	ifstream data_xptE(str_xptE);
 	ifstream data_yptE(str_yptE);
 	ifstream data_xssE(str_xssE);
 	ifstream data_yssE(str_yssE);
 	ifstream data_xssK(str_xssK);
 	ifstream data_yssK(str_yssK);
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
 	for (int i=0;i<70;i++){
 		int aaa;
		data_momK >> momK[i]>>aaa >>aaa >>aaa >>aaa;
 		data_momE >> momE[i]>>aaa >>aaa >>aaa >>aaa;
		data_xptK >> xptK[i]>>aaa >>aaa >>aaa >>aaa;
 		data_xptE >> xptE[i]>>aaa >>aaa >>aaa >>aaa;
		data_yptK >> yptK[i]>>aaa >>aaa >>aaa >>aaa;
 		data_yptE >> yptE[i]>>aaa >>aaa >>aaa >>aaa;
	}
 	data_plK.close();
 	data_plE.close();
 	data_momK.close();
 	data_momE.close();
 	data_xptK.close();
 	data_xptE.close();
 	data_yptK.close();
 	data_yptE.close();
 	data_xssE.close();
 	data_yssE.close();
 	data_xssK.close();
 	data_yssK.close();

   for (Long64_t jentry=0; jentry<nentries;jentry++) {
      Long64_t ientry = LoadTree(jentry);
      if (ientry < 0) break;
      nb = fChain->GetEntry(jentry);   nbytes += nb;
      // if (Cut(ientry) < 0) continue;
		if ( ((hwatnpe1+hwatnpe2>70.)|| 
		      (hwatnum1<=6 && hwatnum2<=6 && hwatnpe1+hwatnpe2>20.)) && 
			 	haernpe1+haernpe2+haernpe3<30. &&
			 	fabs(hsbeta-1)<0.22&& 
			 	hsxpfp<3e-3*hsxfp+0.2 ){
				x=(hsxfp-0)/60.;
				xp=(hsxpfp-0)/0.15;
				y=(hsyfp-0)/6.;
				yp=(hsypfp-0)/0.01;
				kmom=calcf2t(momK,x,xp,y,yp,4);
				kxpt=calcf2t(xptK,x,xp,y,yp,4);
				kypt=calcf2t(yptK,x,xp,y,yp,4);
				xp=(kxpt-0)/0.1;
				yp=(kypt-0)/0.05;
				mom=(kmom-1.2)/0.15;
				kxss=calct2s(xssK,xp,yp,mom,4);
				kyss=calct2s(yssK,xp,yp,mom,4);
				x=(esxfp-0)/25.;
				xp=(esxpfp-0)/0.1;
				y=(esyfp-0)/3.;
				yp=(esypfp-0)/0.01;
				emom=calcf2t(momE,x,xp,y,yp,4);
				expt=calcf2t(xptE,x,xp,y,yp,4);
				eypt=calcf2t(yptE,x,xp,y,yp,4);
				xp=(expt-0)/0.1;
				yp=(eypt+0.06)/0.03;
				mom=(emom-0.844)/0.2;
				exss=calct2s(xssE,xp,yp,mom,4);
				eyss=calct2s(yssE,xp,yp,mom,4);
				mm=GetMM(hallcp*1e-3, kxpt, kypt, kmom, 
							-expt, -eypt, emom,TargetFlag);
				ctime=htimetar-
				(etimetar-calcf2t(plE,esxfp,esxpfp,esyfp,esypfp,3));
				//cout<< mm << endl;
				Nkaon++;
				newtree->Fill();
		}
   }
	cout << runnum<<" "<<Nkaon << endl;

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
		EEloss=250e-6;
		KEloss=470e-6;
  		BEloss=200e-6;
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
	case 28:
		mtar=mSi28;
		mcore=mAl27;
		break;	
	case 52:
		mtar=mCr52;
		mcore=mV51;
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
 double pkz = pk/sqrt(1+xpk*xpk+ypk*ypk);
 double pkx = pkz*xpk;
 double pky = pkz*ypk;
 double Ee = sqrt(pe*pe+me*me)+EEloss;
 double pez = pe/sqrt(1+xpe*xpe+ype*ype);
 double pex = pez*xpe;
 double pey = pez*ype;
 double pi = sqrt(Ei*Ei-me*me);
 double dE = (Ei+mtar-Ek-Ee);
 double dE2 = dE*dE;
 double dpx = (-pex-pkx);
 double dpy = (-pey-pky);
 double dpz = (pi-pez-pkz);
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

  for (int n=0;n<nMat;n++){
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
