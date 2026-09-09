#include <iostream>
#include <fstream>

void check1X(int runnum=74120){
	char str[100];
	sprintf(str,"run# = %d",runnum);
	alias(runnum);
	//KH1Xdectdc();
	//KH1Xdecadc();
	KH1Xrawtdc();
	//	KH1Xrawadc();
}

void KH1Xrawadc(){
	TCanvas *c11 = new TCanvas("c11","c11");
	TCanvas *c12 = new TCanvas("c12","c12");
	TCanvas *c13 = new TCanvas("c13","c13");
	TCanvas *c14 = new TCanvas("c14","c14");
	c11->Divide(3,3);
	c11->cd(1);gPad->SetLogy();hscinrawposadc1_01->Draw();
	c11->cd(2);gPad->SetLogy();hscinrawposadc1_02->Draw();
	c11->cd(3);gPad->SetLogy();hscinrawposadc1_03->Draw();
	c11->cd(4);gPad->SetLogy();hscinrawposadc1_04->Draw();
	c11->cd(5);gPad->SetLogy();hscinrawposadc1_05->Draw();
	c11->cd(6);gPad->SetLogy();hscinrawposadc1_06->Draw();
	c11->cd(7);gPad->SetLogy();hscinrawposadc1_07->Draw();
	c11->cd(8);gPad->SetLogy();hscinrawposadc1_08->Draw();
	c11->cd(9);gPad->SetLogy();hscinrawposadc1_09->Draw();
	c11->Print("K1X_rawposadc1.png");
	c12->Divide(3,3);
	c12->cd(1);gPad->SetLogy();hscinrawposadc1_10->Draw();
	c12->cd(2);gPad->SetLogy();hscinrawposadc1_11->Draw();
	c12->cd(3);gPad->SetLogy();hscinrawposadc1_12->Draw();
	c12->cd(4);gPad->SetLogy();hscinrawposadc1_13->Draw();
	c12->cd(5);gPad->SetLogy();hscinrawposadc1_14->Draw();
	c12->cd(6);gPad->SetLogy();hscinrawposadc1_15->Draw();
	c12->cd(7);gPad->SetLogy();hscinrawposadc1_16->Draw();
	c12->cd(8);gPad->SetLogy();hscinrawposadc1_17->Draw();
	c12->Print("K1X_rawposadc2.png");
	c13->Divide(3,3);
	c13->cd(1);gPad->SetLogy();hscinrawnegadc1_01->Draw();
	c13->cd(2);gPad->SetLogy();hscinrawnegadc1_02->Draw();
	c13->cd(3);gPad->SetLogy();hscinrawnegadc1_03->Draw();
	c13->cd(4);gPad->SetLogy();hscinrawnegadc1_04->Draw();
	c13->cd(5);gPad->SetLogy();hscinrawnegadc1_05->Draw();
	c13->cd(6);gPad->SetLogy();hscinrawnegadc1_06->Draw();
	c13->cd(7);gPad->SetLogy();hscinrawnegadc1_07->Draw();
	c13->cd(8);gPad->SetLogy();hscinrawnegadc1_08->Draw();
	c13->cd(9);gPad->SetLogy();hscinrawnegadc1_09->Draw();
	c13->Print("K1X_rawnegadc1.png");
	c14->Divide(3,3);
	c14->cd(1);gPad->SetLogy();hscinrawnegadc1_10->Draw();
	c14->cd(2);gPad->SetLogy();hscinrawnegadc1_11->Draw();
	c14->cd(3);gPad->SetLogy();hscinrawnegadc1_12->Draw();
	c14->cd(4);gPad->SetLogy();hscinrawnegadc1_13->Draw();
	c14->cd(5);gPad->SetLogy();hscinrawnegadc1_14->Draw();
	c14->cd(6);gPad->SetLogy();hscinrawnegadc1_15->Draw();
	c14->cd(7);gPad->SetLogy();hscinrawnegadc1_16->Draw();
	c14->cd(8);gPad->SetLogy();hscinrawnegadc1_17->Draw();
	c14->Print("K1X_rawnegadc2.png");
}

void KH1Xrawtdc(){
	TCanvas *c21 = new TCanvas("c21","c21");
	TCanvas *c22 = new TCanvas("c22","c22");
	TCanvas *c23 = new TCanvas("c23","c23");
	TCanvas *c24 = new TCanvas("c24","c24");
	c21->Divide(3,3);
	c21->cd(1);hscinrawpostdc1_01->Draw();
	c21->cd(2);hscinrawpostdc1_02->Draw();
	c21->cd(3);hscinrawpostdc1_03->Draw();
	c21->cd(4);hscinrawpostdc1_04->Draw();
	c21->cd(5);hscinrawpostdc1_05->Draw();
	c21->cd(6);hscinrawpostdc1_06->Draw();
	c21->cd(7);hscinrawpostdc1_07->Draw();
	c21->cd(8);hscinrawpostdc1_08->Draw();
	c21->cd(9);hscinrawpostdc1_09->Draw();
	c21->Print("K1X_rawpostdc1.png");
	c22->Divide(3,3);
	c22->cd(1);hscinrawpostdc1_10->Draw();
	c22->cd(2);hscinrawpostdc1_11->Draw();
	c22->cd(3);hscinrawpostdc1_12->Draw();
	c22->cd(4);hscinrawpostdc1_13->Draw();
	c22->cd(5);hscinrawpostdc1_14->Draw();
	c22->cd(6);hscinrawpostdc1_15->Draw();
	c22->cd(7);hscinrawpostdc1_16->Draw();
	c22->cd(8);hscinrawpostdc1_17->Draw();
	c22->Print("K1X_rawpostdc2.png");
	c23->Divide(3,3);
	c23->cd(1);hscinrawnegtdc1_01->Draw();
	c23->cd(2);hscinrawnegtdc1_02->Draw();
	c23->cd(3);hscinrawnegtdc1_03->Draw();
	c23->cd(4);hscinrawnegtdc1_04->Draw();
	c23->cd(5);hscinrawnegtdc1_05->Draw();
	c23->cd(6);hscinrawnegtdc1_06->Draw();
	c23->cd(7);hscinrawnegtdc1_07->Draw();
	c23->cd(8);hscinrawnegtdc1_08->Draw();
	c23->cd(9);hscinrawnegtdc1_09->Draw();
	c23->Print("K1X_rawnegtdc1.png");
	c24->Divide(3,3);
	c24->cd(1);hscinrawnegtdc1_10->Draw();
	c24->cd(2);hscinrawnegtdc1_11->Draw();
	c24->cd(3);hscinrawnegtdc1_12->Draw();
	c24->cd(4);hscinrawnegtdc1_13->Draw();
	c24->cd(5);hscinrawnegtdc1_14->Draw();
	c24->cd(6);hscinrawnegtdc1_15->Draw();
	c24->cd(7);hscinrawnegtdc1_16->Draw();
	c24->cd(8);hscinrawnegtdc1_17->Draw();
	c24->Print("K1X_rawnegtdc2.png");
}

void KH1Xdecadc(){
	TCanvas *c31 = new TCanvas("c31","c31");
	TCanvas *c32 = new TCanvas("c32","c32");
	TCanvas *c33 = new TCanvas("c33","c33");
	TCanvas *c34 = new TCanvas("c34","c34");
	c31->Divide(3,3);
	c31->cd(1);hscindecposadc1_01->Draw();
	c31->cd(2);hscindecposadc1_02->Draw();
	c31->cd(3);hscindecposadc1_03->Draw();
	c31->cd(4);hscindecposadc1_04->Draw();
	c31->cd(5);hscindecposadc1_05->Draw();
	c31->cd(6);hscindecposadc1_06->Draw();
	c31->cd(7);hscindecposadc1_07->Draw();
	c31->cd(8);hscindecposadc1_08->Draw();
	c31->cd(9);hscindecposadc1_09->Draw();
	c31->Print("K1X_decposadc1.png");
	c32->Divide(3,3);
	c32->cd(1);hscindecposadc1_10->Draw();
	c32->cd(2);hscindecposadc1_11->Draw();
	c32->cd(3);hscindecposadc1_12->Draw();
	c32->cd(4);hscindecposadc1_13->Draw();
	c32->cd(5);hscindecposadc1_14->Draw();
	c32->cd(6);hscindecposadc1_15->Draw();
	c32->cd(7);hscindecposadc1_16->Draw();
	c32->cd(8);hscindecposadc1_17->Draw();
	c32->Print("K1X_decposadc2.png");
	c33->Divide(3,3);
	c33->cd(1);hscindecnegadc1_01->Draw();
	c33->cd(2);hscindecnegadc1_02->Draw();
	c33->cd(3);hscindecnegadc1_03->Draw();
	c33->cd(4);hscindecnegadc1_04->Draw();
	c33->cd(5);hscindecnegadc1_05->Draw();
	c33->cd(6);hscindecnegadc1_06->Draw();
	c33->cd(7);hscindecnegadc1_07->Draw();
	c33->cd(8);hscindecnegadc1_08->Draw();
	c33->cd(9);hscindecnegadc1_09->Draw();
	c33->Print("K1X_decnegadc1.png");
	c34->Divide(3,3);
	c34->cd(1);hscindecnegadc1_10->Draw();
	c34->cd(2);hscindecnegadc1_11->Draw();
	c34->cd(3);hscindecnegadc1_12->Draw();
	c34->cd(4);hscindecnegadc1_13->Draw();
	c34->cd(5);hscindecnegadc1_14->Draw();
	c34->cd(6);hscindecnegadc1_15->Draw();
	c34->cd(7);hscindecnegadc1_16->Draw();
	c34->cd(8);hscindecnegadc1_17->Draw();
	c34->Print("K1X_decnegadc2.png");
}

void KH1Xdectdc(){
	TCanvas *c41 = new TCanvas("c41","c41");
	TCanvas *c42 = new TCanvas("c42","c42");
	TCanvas *c43 = new TCanvas("c43","c43");
	TCanvas *c44 = new TCanvas("c44","c44");
	c41->Divide(3,3);
	c41->cd(1);hscindecpostdc1_01->Draw();
	c41->cd(2);hscindecpostdc1_02->Draw();
	c41->cd(3);hscindecpostdc1_03->Draw();
	c41->cd(4);hscindecpostdc1_04->Draw();
	c41->cd(5);hscindecpostdc1_05->Draw();
	c41->cd(6);hscindecpostdc1_06->Draw();
	c41->cd(7);hscindecpostdc1_07->Draw();
	c41->cd(8);hscindecpostdc1_08->Draw();
	c41->cd(9);hscindecpostdc1_09->Draw();
	c41->Print("K1X_decpostdc1.png");
	c42->Divide(3,3);
	c42->cd(1);hscindecpostdc1_10->Draw();
	c42->cd(2);hscindecpostdc1_11->Draw();
	c42->cd(3);hscindecpostdc1_12->Draw();
	c42->cd(4);hscindecpostdc1_13->Draw();
	c42->cd(5);hscindecpostdc1_14->Draw();
	c42->cd(6);hscindecpostdc1_15->Draw();
	c42->cd(7);hscindecpostdc1_16->Draw();
	c42->cd(8);hscindecpostdc1_17->Draw();
	c42->Print("K1X_decpostdc2.png");
	c43->Divide(3,3);
	c43->cd(1);hscindecnegtdc1_01->Draw();
	c43->cd(2);hscindecnegtdc1_02->Draw();
	c43->cd(3);hscindecnegtdc1_03->Draw();
	c43->cd(4);hscindecnegtdc1_04->Draw();
	c43->cd(5);hscindecnegtdc1_05->Draw();
	c43->cd(6);hscindecnegtdc1_06->Draw();
	c43->cd(7);hscindecnegtdc1_07->Draw();
	c43->cd(8);hscindecnegtdc1_08->Draw();
	c43->cd(9);hscindecnegtdc1_09->Draw();
	c43->Print("K1X_decnegtdc1.png");
	c44->Divide(3,3);
	c44->cd(1);hscindecnegtdc1_10->Draw();
	c44->cd(2);hscindecnegtdc1_11->Draw();
	c44->cd(3);hscindecnegtdc1_12->Draw();
	c44->cd(4);hscindecnegtdc1_13->Draw();
	c44->cd(5);hscindecnegtdc1_14->Draw();
	c44->cd(6);hscindecnegtdc1_15->Draw();
	c44->cd(7);hscindecnegtdc1_16->Draw();
	c44->cd(8);hscindecnegtdc1_17->Draw();
	c44->Print("K1X_decnegtdc2.png");
}


void alias(int runnum)
{
	char rFileName[100];
	char aFileName[100];
	char aaa[100];
	char bbb[100];
	char newHistName[100];
	char oldHistName[100];
	char histnum[100];
	FILE *fp;
	
	sprintf(rFileName,"%d.root",runnum);
	sprintf(aFileName,"../paw/paw%d.kumac",runnum);
	TFile *f = new TFile(rFileName);
	fp=fopen(aFileName,"r");
	if (fp == NULL){
		cout << "No File!!!" <<endl;
		return;
	}
	while(fgets(aaa, 256, fp) != NULL){
		sscanf(aaa,"%s %s %s",bbb,newHistName,histnum);
		//cout << newHistName << " " << histnum << endl;
		strcpy(oldHistName,"h");
		strcat(oldHistName,histnum);
		TH1F *htmp = (TH1F*)f->Get(oldHistName);
		htmp->SetName(newHistName);	
	}
	
}


