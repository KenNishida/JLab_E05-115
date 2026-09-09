#include <iostream>
#include <fstream>

void checkWat(int runnum=74120){
	char str[100];
	sprintf(str,"run# = %d",runnum);
	alias(runnum);
	//Hwatdectdc();
	//Hwatdecadc();
	//Hwatrawtdc();
	Hwatrawadc();
}

void Hwatrawadc(){
	TCanvas *c11 = new TCanvas("c11","c11");
	TCanvas *c12 = new TCanvas("c12","c12");
	TCanvas *c13 = new TCanvas("c13","c13");
	TCanvas *c14 = new TCanvas("c14","c14");
	c11->Divide(4,3);
	c11->cd(1);gPad->SetLogy();hwatrawposadc1_01->Draw();
	c11->cd(2);gPad->SetLogy();hwatrawposadc1_02->Draw();
	c11->cd(3);gPad->SetLogy();hwatrawposadc1_03->Draw();
	c11->cd(4);gPad->SetLogy();hwatrawposadc1_04->Draw();
	c11->cd(5);gPad->SetLogy();hwatrawposadc1_05->Draw();
	c11->cd(6);gPad->SetLogy();hwatrawposadc1_06->Draw();
	c11->cd(7);gPad->SetLogy();hwatrawposadc1_07->Draw();
	c11->cd(8);gPad->SetLogy();hwatrawposadc1_08->Draw();
	c11->cd(9);gPad->SetLogy();hwatrawposadc1_09->Draw();
	c11->cd(10);gPad->SetLogy();hwatrawposadc1_10->Draw();
	c11->cd(11);gPad->SetLogy();hwatrawposadc1_11->Draw();
	c11->cd(12);gPad->SetLogy();hwatrawposadc1_12->Draw();
	c11->Print("Wat_rawposadc1.ps");

	c12->Divide(4,3);
	c12->cd(1);gPad->SetLogy();hwatrawposadc2_01->Draw();
	c12->cd(2);gPad->SetLogy();hwatrawposadc2_02->Draw();
	c12->cd(3);gPad->SetLogy();hwatrawposadc2_03->Draw();
	c12->cd(4);gPad->SetLogy();hwatrawposadc2_04->Draw();
	c12->cd(5);gPad->SetLogy();hwatrawposadc2_05->Draw();
	c12->cd(6);gPad->SetLogy();hwatrawposadc2_06->Draw();
	c12->cd(7);gPad->SetLogy();hwatrawposadc2_07->Draw();
	c12->cd(8);gPad->SetLogy();hwatrawposadc2_08->Draw();
	c12->cd(9);gPad->SetLogy();hwatrawposadc2_09->Draw();
	c12->cd(10);gPad->SetLogy();hwatrawposadc2_10->Draw();
	c12->cd(11);gPad->SetLogy();hwatrawposadc2_11->Draw();
	c12->cd(12);gPad->SetLogy();hwatrawposadc2_12->Draw();
	c12->Print("Wat_rawposadc2.ps");

	c13->Divide(4,3);
	c13->cd(1);gPad->SetLogy();hwatrawnegadc1_01->Draw();
	c13->cd(2);gPad->SetLogy();hwatrawnegadc1_02->Draw();
	c13->cd(3);gPad->SetLogy();hwatrawnegadc1_03->Draw();
	c13->cd(4);gPad->SetLogy();hwatrawnegadc1_04->Draw();
	c13->cd(5);gPad->SetLogy();hwatrawnegadc1_05->Draw();
	c13->cd(6);gPad->SetLogy();hwatrawnegadc1_06->Draw();
	c13->cd(7);gPad->SetLogy();hwatrawnegadc1_07->Draw();
	c13->cd(8);gPad->SetLogy();hwatrawnegadc1_08->Draw();
	c13->cd(9);gPad->SetLogy();hwatrawnegadc1_09->Draw();
	c13->cd(10);gPad->SetLogy();hwatrawnegadc1_10->Draw();
	c13->cd(11);gPad->SetLogy();hwatrawnegadc1_11->Draw();
	c13->cd(12);gPad->SetLogy();hwatrawnegadc1_12->Draw();
	c13->Print("Wat_rawnegadc1.ps");

	c14->Divide(4,3);
	c14->cd(1);gPad->SetLogy();hwatrawnegadc2_01->Draw();
	c14->cd(2);gPad->SetLogy();hwatrawnegadc2_02->Draw();
	c14->cd(3);gPad->SetLogy();hwatrawnegadc2_03->Draw();
	c14->cd(4);gPad->SetLogy();hwatrawnegadc2_04->Draw();
	c14->cd(5);gPad->SetLogy();hwatrawnegadc2_05->Draw();
	c14->cd(6);gPad->SetLogy();hwatrawnegadc2_06->Draw();
	c14->cd(7);gPad->SetLogy();hwatrawnegadc2_07->Draw();
	c14->cd(8);gPad->SetLogy();hwatrawnegadc2_08->Draw();
	c14->cd(9);gPad->SetLogy();hwatrawnegadc2_09->Draw();
	c14->cd(10);gPad->SetLogy();hwatrawnegadc2_10->Draw();
	c14->cd(11);gPad->SetLogy();hwatrawnegadc2_11->Draw();
	c14->cd(12);gPad->SetLogy();hwatrawnegadc2_12->Draw();
	c14->Print("Wat_rawnegadc2.ps");


}

void Hwatrawtdc(){
	TCanvas *c21 = new TCanvas("c21","c21");
	TCanvas *c22 = new TCanvas("c22","c22");
	TCanvas *c23 = new TCanvas("c23","c23");
	TCanvas *c24 = new TCanvas("c24","c24");
	c21->Divide(4,3);
	c21->cd(1);hwatrawpostdc1_01->Draw();
	c21->cd(2);hwatrawpostdc1_02->Draw();
	c21->cd(3);hwatrawpostdc1_03->Draw();
	c21->cd(4);hwatrawpostdc1_04->Draw();
	c21->cd(5);hwatrawpostdc1_05->Draw();
	c21->cd(6);hwatrawpostdc1_06->Draw();
	c21->cd(7);hwatrawpostdc1_07->Draw();
	c21->cd(8);hwatrawpostdc1_08->Draw();
	c21->cd(9);hwatrawpostdc1_09->Draw();
	c21->cd(10);hwatrawpostdc1_10->Draw();
	c21->cd(11);hwatrawpostdc1_11->Draw();
	c21->cd(12);hwatrawpostdc1_12->Draw();
	c21->Print("wat_rawpostdc1.ps");
		
	c22->Divide(4,3);
	c22->cd(1);hwatrawpostdc2_01->Draw();
	c22->cd(2);hwatrawpostdc2_02->Draw();
	c22->cd(3);hwatrawpostdc2_03->Draw();
	c22->cd(4);hwatrawpostdc2_04->Draw();
	c22->cd(5);hwatrawpostdc2_05->Draw();
	c22->cd(6);hwatrawpostdc2_06->Draw();
	c22->cd(7);hwatrawpostdc2_07->Draw();
	c22->cd(8);hwatrawpostdc2_08->Draw();
	c22->cd(9);hwatrawpostdc2_09->Draw();
	c22->cd(10);hwatrawpostdc2_10->Draw();
	c22->cd(11);hwatrawpostdc2_11->Draw();
	c22->cd(12);hwatrawpostdc2_12->Draw();
	c22->Print("wat_rawpostdc2.ps");
	
	c23->Divide(4,3);
	c23->cd(1);hwatrawnegtdc1_01->Draw();
	c23->cd(2);hwatrawnegtdc1_02->Draw();
	c23->cd(3);hwatrawnegtdc1_03->Draw();
	c23->cd(4);hwatrawnegtdc1_04->Draw();
	c23->cd(5);hwatrawnegtdc1_05->Draw();
	c23->cd(6);hwatrawnegtdc1_06->Draw();
	c23->cd(7);hwatrawnegtdc1_07->Draw();
	c23->cd(8);hwatrawnegtdc1_08->Draw();
	c23->cd(9);hwatrawnegtdc1_09->Draw();
	c23->cd(10);hwatrawnegtdc1_10->Draw();
	c23->cd(11);hwatrawnegtdc1_11->Draw();
	c23->cd(12);hwatrawnegtdc1_12->Draw();
	c23->Print("wat_rawnegtdc1.ps");
		
	c24->Divide(4,3);
	c24->cd(1);hwatrawnegtdc2_01->Draw();
	c24->cd(2);hwatrawnegtdc2_02->Draw();
	c24->cd(3);hwatrawnegtdc2_03->Draw();
	c24->cd(4);hwatrawnegtdc2_04->Draw();
	c24->cd(5);hwatrawnegtdc2_05->Draw();
	c24->cd(6);hwatrawnegtdc2_06->Draw();
	c24->cd(7);hwatrawnegtdc2_07->Draw();
	c24->cd(8);hwatrawnegtdc2_08->Draw();
	c24->cd(9);hwatrawnegtdc2_09->Draw();
	c24->cd(10);hwatrawnegtdc2_10->Draw();
	c24->cd(11);hwatrawnegtdc2_11->Draw();
	c24->cd(12);hwatrawnegtdc2_12->Draw();
	c24->Print("wat_rawnegtdc2.ps");
	
	}

void Hwatdecadc(){
	TCanvas *c31 = new TCanvas("c31","c31");
	TCanvas *c32 = new TCanvas("c32","c32");
	TCanvas *c33 = new TCanvas("c33","c33");
	//	TCanvas *c34 = new TCanvas("c34","c34");

}

void Hwatdectdc(){
	TCanvas *c41 = new TCanvas("c41","c41");
	TCanvas *c42 = new TCanvas("c42","c42");
	TCanvas *c43 = new TCanvas("c43","c43");
	TCanvas *c44 = new TCanvas("c44","c44");

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


