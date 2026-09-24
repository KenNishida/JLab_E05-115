#include <iostream>
#include <fstream>
#include <math.h>
#include "include/evdisplay.h"

void evdisplay(char* filename){
	char s[2];
	int irep;
	TCanvas *c1 = new TCanvas("c1","c1");
	c1->SetFillColor(kBlack);
	TView3D *v1 = new TView3D();
	v1->Front();
	TFile *file = new TFile(filename);
	TTree *tree=(TTree*)file->Get("h9500");

	const int nZ =9;
	int flag=0;
	int start=0,end=0;
	int tg_g1=0;
	float pointX[nZ];	
	float pointY[nZ];	
	float pointZ[nZ]={z_DC1,z_DC2,z_1X,z_AC1,z_AC2,z_AC3,
							z_2X,z_WC1,z_WC2,};
	float z_FP = (z_DC1+z_DC2)/2.;	
	Float_t hsnco1=0,hsnco2=0;
	Float_t haernum1=0,haernum2=0,haernum3=0;
	Float_t hwatnum1=0,hwatnum2=0;
	Float_t hsxfp=0,hsxpfp=0;
	Float_t hsyfp=0,hsypfp=0;
	tree->SetBranchAddress("hsnco1",&hsnco1);
	tree->SetBranchAddress("hsnco2",&hsnco2);
	tree->SetBranchAddress("haernum1",&haernum1);
	tree->SetBranchAddress("haernum2",&haernum2);
	tree->SetBranchAddress("haernum3",&haernum3);
	tree->SetBranchAddress("hwatnum1",&hwatnum1);
	tree->SetBranchAddress("hwatnum2",&hwatnum2);
	tree->SetBranchAddress("hsxfp",&hsxfp);
	tree->SetBranchAddress("hsxpfp",&hsxpfp);
	tree->SetBranchAddress("hsyfp",&hsyfp);
	tree->SetBranchAddress("hsypfp",&hsypfp);
	tree->SetBranchAddress("tg_g1",&tg_g1);
	Int_t Nevent=tree->GetEntries();
	end=Nevent;

	cout << "Do you want event by event display? [y or n]"<<endl;	
	gets(s);
	if (s[0]!='y' && s[0]!='n'){
		cout<<"Put y or n"<<endl;
		gets(s);
	}
	if (s[0]=='y') flag==0;
	else if(s[0]=='n'){
		flag=1;
		//cout <<"Put start event# and end event#"<<endl;
		cout <<"Put start# (0 is first event):"<<endl;
		scanf("%d",&start);
		cout <<"Put end#:"<<endl;
		scanf("%d",&end);
		strcpy(DrawOption,"same");
	}
	SetGeometry();
	for (int i=start;i<end;i++){
		tree->GetEntry(i);
		//cout << int(hsnco1) <<" "<<int(haernum1)<<" "<<int(haernum2)
		//<<" "<<int(haernum3)<<" "<<int(hsnco2)<<" "<<int(hwatnum1)
		//<<" "<<int(hwatnum2)<<endl;
		if (hsxpfp>3e-3*hsxfp+0.2 && tg_g1>0){
			for (int j=0;j<nZ;j++){
				pointX[j]=hsxfp+hsxpfp*(pointZ[j]-z_FP);
				pointY[j]=hsyfp+hsypfp*(pointZ[j]-z_FP);
			}
			InitGeometry();
			DrawGeometry(int(hsnco1),int(haernum1),int(haernum2),
			int(haernum3),int(hsnco2),int(hwatnum1),int(hwatnum2),
			nZ,pointX,pointY,pointZ);
			if (flag==0){
				cout << "Type <CR> to continue, q to quit, s to overwrite tracks ::"<<endl;
				gets(s);
				if(s[0]=='q') break;
				else if (s[0]=='s') strcpy(DrawOption,"same");
			}
		}
		//else strcpy(DrawOption," ");
	}
	HKSworld->CloseGeometry();
}


void SetGeometry(){
	HKSworld = 
	new TGeoManager("world","the simplest geometry");
	TGeoMaterial *mat = new TGeoMaterial("vacuum",0,0,0);
	TGeoMedium *med = new TGeoMedium("vacuum",1,mat);
	
	HKSDets = gGeoManager->MakeBox("HKSDets",med,100.,50.,300.);
	
	Int_t nNode;
	DC1 = gGeoManager->MakeBox("DC1",med,120./2.,30./2.,2./2.);
	DC1 ->SetFillColor(kGray);
	DC1 ->SetLineColor(kGray);
	HKSDets->AddNode(DC1,nNode,new TGeoTranslation(-5,0.,z_DC1));
	DC2 = gGeoManager->MakeBox("DC2",med,120./2.,30./2.,2./2.);
	DC2 ->SetFillColor(kGray);
	DC2 ->SetLineColor(kGray);
	HKSDets->AddNode(DC2,nNode,new TGeoTranslation(-5,0.,z_DC2));
	for (int i=0;i<17;i++){
		TOF1X[i] = gGeoManager->MakeBox("TOF1X",med,7.5/2.,30./2.,2./2.);
		TOF1X[i] ->SetFillColor(kGreen);
		TOF1X[i] ->SetLineColor(kGreen);
		Double_t x = 7.5*(-8+i)-5;
		nNode = i+1;
		HKSDets->AddNode(TOF1X[i],nNode,new TGeoTranslation(x,0.,z_1X));
	}
	for (int i=0;i<7;i++){
		AC1[i] = gGeoManager->MakeBox("AC1",med,24.14/2.,46./2.,31./2.);
		AC1[i] ->SetFillColor(kCyan);
		AC1[i] ->SetLineColor(kCyan);
		Double_t x = 24.14*(-3+i)-1.2;
		nNode = i+18;
		HKSDets->AddNode(AC1[i],nNode,new TGeoTranslation(x,0.,z_AC1));
	}
	for (int i=0;i<7;i++){
		AC2[i] = gGeoManager->MakeBox("AC2",med,24.14/2.,46./2.,31./2.);
		AC2[i] ->SetFillColor(kCyan);
		AC2[i] ->SetLineColor(kCyan);
		Double_t x = 24.14*(-3+i)-5.0;
		nNode = i+25;
		HKSDets->AddNode(AC2[i],nNode,new TGeoTranslation(x,0.,z_AC2));
	}
	for (int i=0;i<7;i++){
		AC3[i] = gGeoManager->MakeBox("AC3",med,24.14/2.,46./2.,31./2.);
		AC3[i] ->SetFillColor(kCyan);
		AC3[i] ->SetLineColor(kCyan);
		Double_t x = 24.14*(-3+i)-8.8;
		nNode = i+32;
		HKSDets->AddNode(AC3[i],nNode,new TGeoTranslation(x,0.,z_AC3));
	}
	for (int i=0;i<18;i++){
		TOF2X[i] = gGeoManager->MakeBox("TOF2X",med,9.5/2.,35./2.,2./2.);
		TOF2X[i] ->SetFillColor(kGreen);
		TOF2X[i] ->SetLineColor(kGreen);
		Double_t x = 9.5*(-8.5+i)-5;
		nNode = i+39;
		HKSDets->AddNode(TOF2X[i],nNode,new TGeoTranslation(x,0.,z_2X));
	}
	for (int i=0;i<12;i++){
		WC1[i] = gGeoManager->MakeBox("WC1",med,15.6/2.,35./2.,8./2.);
		WC1[i] ->SetFillColor(kBlue);
		WC1[i] ->SetLineColor(kBlue);
		Double_t x = 15.6*(-5.5+i)-5;
		nNode = i+57;
		HKSDets->AddNode(WC1[i],nNode,new TGeoTranslation(x,0.,z_WC1));
	}
	for (int i=0;i<12;i++){
		WC2[i] = gGeoManager->MakeBox("WC2",med,15.6/2.,35./2.,8./2.);
		WC2[i] ->SetFillColor(kBlue);
		WC2[i] ->SetLineColor(kBlue);
		Double_t x = 15.6*(-5.5+i)-7.8;
		nNode = i+69;
		HKSDets->AddNode(WC2[i],nNode,new TGeoTranslation(x,0.,z_WC2));
	}
	gGeoManager->SetTopVolume(HKSDets);
}

void InitGeometry(){
	for (int i=0;i<17;i++){
		TOF1X[i] ->SetFillColor(kGreen);
		TOF1X[i] ->SetLineColor(kGreen);
	}
	for (int i=0;i<7;i++){
		AC1[i] ->SetFillColor(kCyan);
		AC1[i] ->SetLineColor(kCyan);
	}
	for (int i=0;i<7;i++){
		AC2[i] ->SetFillColor(kCyan);
		AC2[i] ->SetLineColor(kCyan);
	}
	for (int i=0;i<7;i++){
		AC3[i] ->SetFillColor(kCyan);
		AC3[i] ->SetLineColor(kCyan);
	}
	for (int i=0;i<18;i++){
		TOF2X[i] ->SetFillColor(kGreen);
		TOF2X[i] ->SetLineColor(kGreen);
	}
	for (int i=0;i<12;i++){
		WC1[i] ->SetFillColor(kBlue);
		WC1[i] ->SetLineColor(kBlue);
	}
	for (int i=0;i<12;i++){
		WC2[i] ->SetFillColor(kBlue);
		WC2[i] ->SetLineColor(kBlue);
	}
}

	
void DrawGeometry(int hit_1X,int hit_AC1,int hit_AC2,
int hit_AC3,int hit_2X,int hit_WC1,int hit_WC2,
int N, float *X, float *Y, float *Z){
	if (hit_1X!=0){
 		TOF1X[hit_1X-1]->SetLineWidth(2);
 		TOF1X[hit_1X-1]->SetLineColor(kRed);
	}
	if (hit_AC1!=0){
 		AC1[hit_AC1-1]->SetLineWidth(2);
 		AC1[hit_AC1-1]->SetLineColor(kRed);
	}
	if (hit_AC2!=0){
 		AC2[hit_AC2-1]->SetLineWidth(2);
 		AC2[hit_AC2-1]->SetLineColor(kRed);
	}
	if (hit_AC3!=0){
 		AC3[hit_AC3-1]->SetLineWidth(2);
 		AC3[hit_AC3-1]->SetLineColor(kRed);
	}
	if (hit_2X!=0){
 		TOF2X[hit_2X-1]->SetLineWidth(2);
 		TOF2X[hit_2X-1]->SetLineColor(kRed);
	}
	if (hit_WC1!=0){
 		WC1[hit_WC1-1]->SetLineWidth(2);
 		WC1[hit_WC1-1]->SetLineColor(kRed);
	}
	if (hit_WC2!=0){
 		WC2[hit_WC2-1]->SetLineWidth(2);
 		WC2[hit_WC2-1]->SetLineColor(kRed);
	}

	Int_t track_index = HKSworld->AddTrack(1,55,0);
	TVirtualGeoTrack *track = HKSworld->GetTrack(track_index);
	for (int i=0;i<N;i++){
		track->AddPoint(X[i],Y[i],Z[i],i);
	}
	HKSworld->SetCurrentTrack(track);
		
	HKSDets->Draw(DrawOption);
	track->Draw("");
}


