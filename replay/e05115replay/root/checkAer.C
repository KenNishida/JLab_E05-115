#include <iostream>
#include <fstream>

void checkAer(int runnum=74120){
	char str[100];
	sprintf(str,"run# = %d",runnum);
	alias(runnum);
	//Haerdectdc();
	//Haerdecadc();
	//Haerrawtdc();
	Haerrawadc();
}

void Haerrawadc(){
	TCanvas *c11 = new TCanvas("c11","c11");
	TCanvas *c12 = new TCanvas("c12","c12");
	TCanvas *c120 = new TCanvas("c120","c120");

	TCanvas *c13 = new TCanvas("c13","c13");
	TCanvas *c14 = new TCanvas("c14","c14");
	TCanvas *c140 = new TCanvas("c140","c140");
	c11->Divide(3,3);
	c11->cd(1);gPad->SetLogy();haerrawposadc1_1->Draw();
	c11->cd(2);gPad->SetLogy();haerrawposadc1_2->Draw();
	c11->cd(3);gPad->SetLogy();haerrawposadc1_3->Draw();
	c11->cd(4);gPad->SetLogy();haerrawposadc1_4->Draw();
	c11->cd(5);gPad->SetLogy();haerrawposadc1_5->Draw();
	c11->cd(6);gPad->SetLogy();haerrawposadc1_6->Draw();
	c11->cd(7);gPad->SetLogy();haerrawposadc1_7->Draw();
	
	c11->Print("aer_rawposadc1.ps");

	c12->Divide(3,3);
	c12->cd(1);gPad->SetLogy();haerrawposadc2_1->Draw();
	c12->cd(2);gPad->SetLogy();haerrawposadc2_2->Draw();
	c12->cd(3);gPad->SetLogy();haerrawposadc2_3->Draw();
	c12->cd(4);gPad->SetLogy();haerrawposadc2_4->Draw();
	c12->cd(5);gPad->SetLogy();haerrawposadc2_5->Draw();
	c12->cd(6);gPad->SetLogy();haerrawposadc2_6->Draw();
	c12->cd(7);gPad->SetLogy();haerrawposadc2_7->Draw();
	c12->Print("aer_rawposadc2.ps");

	c120->Divide(3,3);
	c120->cd(1);gPad->SetLogy();haerrawposadc3_1->Draw();
	c120->cd(2);gPad->SetLogy();haerrawposadc3_2->Draw();
	c120->cd(3);gPad->SetLogy();haerrawposadc3_3->Draw();
	c120->cd(4);gPad->SetLogy();haerrawposadc3_4->Draw();
	c120->cd(5);gPad->SetLogy();haerrawposadc3_5->Draw();
	c120->cd(6);gPad->SetLogy();haerrawposadc3_6->Draw();
	c120->cd(7);gPad->SetLogy();haerrawposadc3_7->Draw();
	c120->Print("aer_rawposadc3.ps");

	c13->Divide(3,3);
	c13->cd(1);gPad->SetLogy();haerrawnegadc1_1->Draw();
	c13->cd(2);gPad->SetLogy();haerrawnegadc1_2->Draw();
	c13->cd(3);gPad->SetLogy();haerrawnegadc1_3->Draw();
	c13->cd(4);gPad->SetLogy();haerrawnegadc1_4->Draw();
	c13->cd(5);gPad->SetLogy();haerrawnegadc1_5->Draw();
	c13->cd(6);gPad->SetLogy();haerrawnegadc1_6->Draw();
	c13->cd(7);gPad->SetLogy();haerrawnegadc1_7->Draw();
	c13->Print("aer_rawnegadc1.ps");

	c14->Divide(3,3);
	c14->cd(1);gPad->SetLogy();haerrawnegadc2_1->Draw();
	c14->cd(2);gPad->SetLogy();haerrawnegadc2_2->Draw();
	c14->cd(3);gPad->SetLogy();haerrawnegadc2_3->Draw();
	c14->cd(4);gPad->SetLogy();haerrawnegadc2_4->Draw();
	c14->cd(5);gPad->SetLogy();haerrawnegadc2_5->Draw();
	c14->cd(6);gPad->SetLogy();haerrawnegadc2_6->Draw();
	c14->cd(7);gPad->SetLogy();haerrawnegadc2_7->Draw();
	c14->Print("aer_rawnegadc2.ps");

	c140->Divide(3,3);
	c140->cd(1);gPad->SetLogy();haerrawnegadc3_1->Draw();
	c140->cd(2);gPad->SetLogy();haerrawnegadc3_2->Draw();
	c140->cd(3);gPad->SetLogy();haerrawnegadc3_3->Draw();
	c140->cd(4);gPad->SetLogy();haerrawnegadc3_4->Draw();
	c140->cd(5);gPad->SetLogy();haerrawnegadc3_5->Draw();
	c140->cd(6);gPad->SetLogy();haerrawnegadc3_6->Draw();
	c140->cd(7);gPad->SetLogy();haerrawnegadc3_7->Draw();
	c140->Print("aer_rawnegadc3.ps");


}

void Haerrawtdc(){
	TCanvas *c21 = new TCanvas("c21","c21");
	TCanvas *c22 = new TCanvas("c22","c22");
	TCanvas *c23 = new TCanvas("c23","c23");
	TCanvas *c24 = new TCanvas("c24","c24");
	c21->Divide(4,3);
	c21->cd(1);haerrawpostdc1_01->Draw();
	c21->cd(2);haerrawpostdc1_02->Draw();
	c21->cd(3);haerrawpostdc1_03->Draw();
	c21->cd(4);haerrawpostdc1_04->Draw();
	c21->cd(5);haerrawpostdc1_05->Draw();
	c21->cd(6);haerrawpostdc1_06->Draw();
	c21->cd(7);haerrawpostdc1_07->Draw();
	c21->cd(8);haerrawpostdc1_08->Draw();
	c21->cd(9);haerrawpostdc1_09->Draw();
	c21->cd(10);haerrawpostdc1_10->Draw();
	c21->cd(11);haerrawpostdc1_11->Draw();
	c21->cd(12);haerrawpostdc1_12->Draw();	c21->cd(14);hwatrawpostdc1_14->Draw();
	c21->cd(15);hwatrawpostdc1_15->Draw();
	c21->cd(16);hwatrawpostdc1_16->Draw();();
	c21->Print("aer_rawpostdc1.ps");
		
	c22->Divide(4,3);
	c22->cd(1);haerrawpostdc2_01->Draw();
	c22->cd(2);haerrawpostdc2_02->Draw();
	c22->cd(3);haerrawpostdc2_03->Draw();
	c22->cd(4);haerrawpostdc2_04->Draw();
	c22->cd(5);haerrawpostdc2_05->Draw();
	c22->cd(6);haerrawpostdc2_06->Draw();
	c22->cd(7);haerrawpostdc2_07->Draw();
	c22->cd(8);haerrawpostdc2_08->Draw();
	c22->cd(9);haerrawpostdc2_09->Draw();
	c22->cd(10);haerrawpostdc2_10->Draw();
	c22->cd(11);haerrawpostdc2_11->Draw();
	c22->cd(12);haerrawpostdc2_12->Draw();
	c22->Print("aer_rawpostdc2.ps");
	
	c23->Divide(4,3);
	c23->cd(1);haerrawnegtdc1_01->Draw();
	c23->cd(2);haerrawnegtdc1_02->Draw();
	c23->cd(3);haerrawnegtdc1_03->Draw();
	c23->cd(4);haerrawnegtdc1_04->Draw();
	c23->cd(5);haerrawnegtdc1_05->Draw();
	c23->cd(6);haerrawnegtdc1_06->Draw();
	c23->cd(7);haerrawnegtdc1_07->Draw();
	c23->cd(8);haerrawnegtdc1_08->Draw();
	c23->cd(9);haerrawnegtdc1_09->Draw();
	c23->cd(10);haerrawnegtdc1_10->Draw();
	c23->cd(11);haerrawnegtdc1_11->Draw();
	c23->cd(12);haerrawnegtdc1_12->Draw();
	c23->Print("aer_rawnegtdc1.ps");
		
	c24->Divide(4,3);
	c24->cd(1);haerrawnegtdc2_01->Draw();
	c24->cd(2);haerrawnegtdc2_02->Draw();
	c24->cd(3);haerrawnegtdc2_03->Draw();
	c24->cd(4);haerrawnegtdc2_04->Draw();
	c24->cd(5);haerrawnegtdc2_05->Draw();
	c24->cd(6);haerrawnegtdc2_06->Draw();
	c24->cd(7);haerrawnegtdc2_07->Draw();
	c24->cd(8);haerrawnegtdc2_08->Draw();
	c24->cd(9);haerrawnegtdc2_09->Draw();
	c24->cd(10);haerrawnegtdc2_10->Draw();
	c24->cd(11);haerrawnegtdc2_11->Draw();
	c24->cd(12);haerrawnegtdc2_12->Draw();
	c24->Print("aer_rawnegtdc2.ps");
	
	}

void Haerdecadc(){
	TCanvas *c31 = new TCanvas("c31","c31");
	TCanvas *c32 = new TCanvas("c32","c32");
	TCanvas *c33 = new TCanvas("c33","c33");
	//	TCanvas *c34 = new TCanvas("c34","c34");

}

void Haerdectdc(){
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


