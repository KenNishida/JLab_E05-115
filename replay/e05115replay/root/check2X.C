#include <iostream>
#include <fstream>

void check2X(int runnum=74120){
	char str[100];
	sprintf(str,"run# = %d",runnum);
	alias(runnum);
	//KH2Xdectdc();
	//KH2Xdecadc();
	KH2Xrawtdc();
	//	KH2Xrawadc();
}

void KH2Xrawadc(){
	TCanvas *c11 = new TCanvas("c11","c11");
	TCanvas *c12 = new TCanvas("c12","c12");
	TCanvas *c13 = new TCanvas("c13","c13");
	TCanvas *c14 = new TCanvas("c14","c14");
	c11->Divide(3,3);
	c11->cd(1);gPad->SetLogy();hscinrawposadc3_01->Draw();
	c11->cd(2);gPad->SetLogy();hscinrawposadc3_02->Draw();
	c11->cd(3);gPad->SetLogy();hscinrawposadc3_03->Draw();
	c11->cd(4);gPad->SetLogy();hscinrawposadc3_04->Draw();
	c11->cd(5);gPad->SetLogy();hscinrawposadc3_05->Draw();
	c11->cd(6);gPad->SetLogy();hscinrawposadc3_06->Draw();
	c11->cd(7);gPad->SetLogy();hscinrawposadc3_07->Draw();
	c11->cd(8);gPad->SetLogy();hscinrawposadc3_08->Draw();
	c11->cd(9);gPad->SetLogy();hscinrawposadc3_09->Draw();
	c11->Print("K2X_rawposadc1.gif");
	c12->Divide(3,3);
	c12->cd(1);gPad->SetLogy();hscinrawposadc3_10->Draw();
	c12->cd(2);gPad->SetLogy();hscinrawposadc3_11->Draw();
	c12->cd(3);gPad->SetLogy();hscinrawposadc3_12->Draw();
	c12->cd(4);gPad->SetLogy();hscinrawposadc3_13->Draw();
	c12->cd(5);gPad->SetLogy();hscinrawposadc3_14->Draw();
	c12->cd(6);gPad->SetLogy();hscinrawposadc3_15->Draw();
	c12->cd(7);gPad->SetLogy();hscinrawposadc3_16->Draw();
	c12->cd(8);gPad->SetLogy();hscinrawposadc3_17->Draw();
	c12->cd(9);gPad->SetLogy();hscinrawposadc3_18->Draw();
	c12->Print("K2X_rawposadc2.gif");
	c13->Divide(3,3);
	c13->cd(1);gPad->SetLogy();hscinrawnegadc3_01->Draw();
	c13->cd(2);gPad->SetLogy();hscinrawnegadc3_02->Draw();
	c13->cd(3);gPad->SetLogy();hscinrawnegadc3_03->Draw();
	c13->cd(4);gPad->SetLogy();hscinrawnegadc3_04->Draw();
	c13->cd(5);gPad->SetLogy();hscinrawnegadc3_05->Draw();
	c13->cd(6);gPad->SetLogy();hscinrawnegadc3_06->Draw();
	c13->cd(7);gPad->SetLogy();hscinrawnegadc3_07->Draw();
	c13->cd(8);gPad->SetLogy();hscinrawnegadc3_08->Draw();
	c13->cd(9);gPad->SetLogy();hscinrawnegadc3_09->Draw();
	c13->Print("K2X_rawnegadc1.gif");
	c14->Divide(3,3);
	c14->cd(1);gPad->SetLogy();hscinrawnegadc3_10->Draw();
	c14->cd(2);gPad->SetLogy();hscinrawnegadc3_11->Draw();
	c14->cd(3);gPad->SetLogy();hscinrawnegadc3_12->Draw();
	c14->cd(4);gPad->SetLogy();hscinrawnegadc3_13->Draw();
	c14->cd(5);gPad->SetLogy();hscinrawnegadc3_14->Draw();
	c14->cd(6);gPad->SetLogy();hscinrawnegadc3_15->Draw();
	c14->cd(7);gPad->SetLogy();hscinrawnegadc3_16->Draw();
	c14->cd(8);gPad->SetLogy();hscinrawnegadc3_17->Draw();
	c14->cd(9);gPad->SetLogy();hscinrawnegadc3_18->Draw();
	c14->Print("K2X_rawnegadc2.gif");
}

void KH2Xrawtdc(){
	TCanvas *c21 = new TCanvas("c21","c21");
	TCanvas *c22 = new TCanvas("c22","c22");
	TCanvas *c23 = new TCanvas("c23","c23");
	TCanvas *c24 = new TCanvas("c24","c24");
	c21->Divide(3,3);
	c21->cd(1);hscinrawpostdc3_01->Draw();
	c21->cd(2);hscinrawpostdc3_02->Draw();
	c21->cd(3);hscinrawpostdc3_03->Draw();
	c21->cd(4);hscinrawpostdc3_04->Draw();
	c21->cd(5);hscinrawpostdc3_05->Draw();
	c21->cd(6);hscinrawpostdc3_06->Draw();
	c21->cd(7);hscinrawpostdc3_07->Draw();
	c21->cd(8);hscinrawpostdc3_08->Draw();
	c21->cd(9);hscinrawpostdc3_09->Draw();
	c21->Print("K2X_rawpostdc1.gif");
	c22->Divide(3,3);
	c22->cd(1);hscinrawpostdc3_10->Draw();
	c22->cd(2);hscinrawpostdc3_11->Draw();
	c22->cd(3);hscinrawpostdc3_12->Draw();
	c22->cd(4);hscinrawpostdc3_13->Draw();
	c22->cd(5);hscinrawpostdc3_14->Draw();
	c22->cd(6);hscinrawpostdc3_15->Draw();
	c22->cd(7);hscinrawpostdc3_16->Draw();
	c22->cd(8);hscinrawpostdc3_17->Draw();
	c22->cd(9);hscinrawpostdc3_18->Draw();
	
	c22->Print("K2X_rawpostdc2.gif");
	c23->Divide(3,3);
	c23->cd(1);hscinrawnegtdc3_01->Draw();
	c23->cd(2);hscinrawnegtdc3_02->Draw();
	c23->cd(3);hscinrawnegtdc3_03->Draw();
	c23->cd(4);hscinrawnegtdc3_04->Draw();
	c23->cd(5);hscinrawnegtdc3_05->Draw();
	c23->cd(6);hscinrawnegtdc3_06->Draw();
	c23->cd(7);hscinrawnegtdc3_07->Draw();
	c23->cd(8);hscinrawnegtdc3_08->Draw();
	c23->cd(9);hscinrawnegtdc3_09->Draw();
	c23->Print("K2X_rawnegtdc1.gif");
	c24->Divide(3,3);
	c24->cd(1);hscinrawnegtdc3_10->Draw();
	c24->cd(2);hscinrawnegtdc3_11->Draw();
	c24->cd(3);hscinrawnegtdc3_12->Draw();
	c24->cd(4);hscinrawnegtdc3_13->Draw();
	c24->cd(5);hscinrawnegtdc3_14->Draw();
	c24->cd(6);hscinrawnegtdc3_15->Draw();
	c24->cd(7);hscinrawnegtdc3_16->Draw();
	c24->cd(8);hscinrawnegtdc3_17->Draw();
	c24->cd(9);hscinrawnegtdc3_18->Draw();

	c24->Print("K2X_rawnegtdc2.gif");
}

void KH2Xdecadc(){
	TCanvas *c31 = new TCanvas("c31","c31");
	TCanvas *c32 = new TCanvas("c32","c32");
	TCanvas *c33 = new TCanvas("c33","c33");
	TCanvas *c34 = new TCanvas("c34","c34");
	c31->Divide(3,3);
	c31->cd(1);hscindecposadc3_01->Draw();
	c31->cd(2);hscindecposadc3_02->Draw();
	c31->cd(3);hscindecposadc3_03->Draw();
	c31->cd(4);hscindecposadc3_04->Draw();
	c31->cd(5);hscindecposadc3_05->Draw();
	c31->cd(6);hscindecposadc3_06->Draw();
	c31->cd(7);hscindecposadc3_07->Draw();
	c31->cd(8);hscindecposadc3_08->Draw();
	c31->cd(9);hscindecposadc3_09->Draw();
	c31->Print("K2X_decposadc1.gif");
	c32->Divide(3,3);
	c32->cd(1);hscindecposadc3_10->Draw();
	c32->cd(2);hscindecposadc3_11->Draw();
	c32->cd(3);hscindecposadc3_12->Draw();
	c32->cd(4);hscindecposadc3_13->Draw();
	c32->cd(5);hscindecposadc3_14->Draw();
	c32->cd(6);hscindecposadc3_15->Draw();
	c32->cd(7);hscindecposadc3_16->Draw();
	c32->cd(8);hscindecposadc3_17->Draw();
	c32->cd(9);hscindecposadc3_18->Draw();

	c32->Print("K2X_decposadc2.gif");
	c33->Divide(3,3);
	c33->cd(1);hscindecnegadc3_01->Draw();
	c33->cd(2);hscindecnegadc3_02->Draw();
	c33->cd(3);hscindecnegadc3_03->Draw();
	c33->cd(4);hscindecnegadc3_04->Draw();
	c33->cd(5);hscindecnegadc3_05->Draw();
	c33->cd(6);hscindecnegadc3_06->Draw();
	c33->cd(7);hscindecnegadc3_07->Draw();
	c33->cd(8);hscindecnegadc3_08->Draw();
	c33->cd(9);hscindecnegadc3_09->Draw();
	c33->Print("K2X_decnegadc1.gif");
	c34->Divide(3,3);
	c34->cd(1);hscindecnegadc3_10->Draw();
	c34->cd(2);hscindecnegadc3_11->Draw();
	c34->cd(3);hscindecnegadc3_12->Draw();
	c34->cd(4);hscindecnegadc3_13->Draw();
	c34->cd(5);hscindecnegadc3_14->Draw();
	c34->cd(6);hscindecnegadc3_15->Draw();
	c34->cd(7);hscindecnegadc3_16->Draw();
	c34->cd(8);hscindecnegadc3_17->Draw();
	c34->cd(9);hscindecnegadc3_18->Draw();

	c34->Print("K2X_decnegadc2.gif");
}

void KH2Xdectdc(){
	TCanvas *c41 = new TCanvas("c41","c41");
	TCanvas *c42 = new TCanvas("c42","c42");
	TCanvas *c43 = new TCanvas("c43","c43");
	TCanvas *c44 = new TCanvas("c44","c44");
	c41->Divide(3,3);
	c41->cd(1);hscindecpostdc3_01->Draw();
	c41->cd(2);hscindecpostdc3_02->Draw();
	c41->cd(3);hscindecpostdc3_03->Draw();
	c41->cd(4);hscindecpostdc3_04->Draw();
	c41->cd(5);hscindecpostdc3_05->Draw();
	c41->cd(6);hscindecpostdc3_06->Draw();
	c41->cd(7);hscindecpostdc3_07->Draw();
	c41->cd(8);hscindecpostdc3_08->Draw();
	c41->cd(9);hscindecpostdc3_09->Draw();
	c41->Print("K2X_decpostdc1.gif");
	c42->Divide(3,3);
	c42->cd(1);hscindecpostdc3_10->Draw();
	c42->cd(2);hscindecpostdc3_11->Draw();
	c42->cd(3);hscindecpostdc3_12->Draw();
	c42->cd(4);hscindecpostdc3_13->Draw();
	c42->cd(5);hscindecpostdc3_14->Draw();
	c42->cd(6);hscindecpostdc3_15->Draw();
	c42->cd(7);hscindecpostdc3_16->Draw();
	c42->cd(8);hscindecpostdc3_17->Draw();
	c42->cd(9);hscindecpostdc3_18->Draw();
	c42->Print("K2X_decpostdc2.gif");
	c43->Divide(3,3);
	c43->cd(1);hscindecnegtdc3_01->Draw();
	c43->cd(2);hscindecnegtdc3_02->Draw();
	c43->cd(3);hscindecnegtdc3_03->Draw();
	c43->cd(4);hscindecnegtdc3_04->Draw();
	c43->cd(5);hscindecnegtdc3_05->Draw();
	c43->cd(6);hscindecnegtdc3_06->Draw();
	c43->cd(7);hscindecnegtdc3_07->Draw();
	c43->cd(8);hscindecnegtdc3_08->Draw();
	c43->cd(9);hscindecnegtdc3_09->Draw();
	c43->Print("K2X_decnegtdc1.gif");
	c44->Divide(3,3);
	c44->cd(1);hscindecnegtdc3_10->Draw();
	c44->cd(2);hscindecnegtdc3_11->Draw();
	c44->cd(3);hscindecnegtdc3_12->Draw();
	c44->cd(4);hscindecnegtdc3_13->Draw();
	c44->cd(5);hscindecnegtdc3_14->Draw();
	c44->cd(6);hscindecnegtdc3_15->Draw();
	c44->cd(7);hscindecnegtdc3_16->Draw();
	c44->cd(8);hscindecnegtdc3_17->Draw();
	c44->cd(9);hscindecnegtdc3_18->Draw();

	c44->Print("K2X_decnegtdc2.gif");
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


