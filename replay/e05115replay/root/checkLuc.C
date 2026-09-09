#include <iostream>
#include <fstream>

void checkLuc(int runnum=74210){
	char str[100];
	sprintf(str,"run# = %d",runnum);
	alias(runnum);
	//HLucdectdc();
	//HLucdecadc();
	HLucrawtdc();
	//HLucrawadc();
}

void HLucrawadc(){
	TCanvas *c11 = new TCanvas("c11","c11");
	TCanvas *c12 = new TCanvas("c12","c12");
	TCanvas *c13 = new TCanvas("c13","c13");
	//	TCanvas *c14 = new TCanvas("c14","c14");
	c11->Divide(4,4);
	c11->cd(1);gPad->SetLogy();hlucrawposadc1_01->Draw();
	c11->cd(2);gPad->SetLogy();hlucrawposadc1_02->Draw();
	c11->cd(3);gPad->SetLogy();hlucrawposadc1_03->Draw();
	c11->cd(4);gPad->SetLogy();hlucrawposadc1_04->Draw();
	c11->cd(5);gPad->SetLogy();hlucrawposadc1_05->Draw();
	c11->cd(6);gPad->SetLogy();hlucrawposadc1_06->Draw();
	c11->cd(7);gPad->SetLogy();hlucrawposadc1_07->Draw();
	c11->cd(8);gPad->SetLogy();hlucrawposadc1_08->Draw();
	c11->cd(9);gPad->SetLogy();hlucrawposadc1_09->Draw();
	c11->cd(10);gPad->SetLogy();hlucrawposadc1_10->Draw();
	c11->cd(11);gPad->SetLogy();hlucrawposadc1_11->Draw();
	c11->cd(12);gPad->SetLogy();hlucrawposadc1_12->Draw();
	c11->cd(13);gPad->SetLogy();hlucrawposadc1_13->Draw();
	c11->cd(14);gPad->SetLogy();hlucrawposadc1_14->Draw();
	c11->cd(15);gPad->SetLogy();hlucrawposadc1_15->Draw();
	c11->cd(16);gPad->SetLogy();hlucrawposadc1_16->Draw();
	c11->Print("Luc_rawposadc.gif");

	c12->Divide(4,4);
	c12->cd(1);gPad->SetLogy();hlucrawnegadc1_01->Draw();
	c12->cd(2);gPad->SetLogy();hlucrawnegadc1_02->Draw();
	c12->cd(3);gPad->SetLogy();hlucrawnegadc1_03->Draw();
	c12->cd(4);gPad->SetLogy();hlucrawnegadc1_04->Draw();
	c12->cd(5);gPad->SetLogy();hlucrawnegadc1_05->Draw();
	c12->cd(6);gPad->SetLogy();hlucrawnegadc1_06->Draw();
	c12->cd(7);gPad->SetLogy();hlucrawnegadc1_07->Draw();
	c12->cd(8);gPad->SetLogy();hlucrawnegadc1_08->Draw();
	c12->cd(9);gPad->SetLogy();hlucrawnegadc1_09->Draw();
	c12->cd(10);gPad->SetLogy();hlucrawnegadc1_10->Draw();
	c12->cd(11);gPad->SetLogy();hlucrawnegadc1_11->Draw();
	c12->cd(12);gPad->SetLogy();hlucrawnegadc1_12->Draw();
	c12->cd(13);gPad->SetLogy();hlucrawnegadc1_13->Draw();
	c12->cd(14);gPad->SetLogy();hlucrawnegadc1_14->Draw();
	c12->cd(15);gPad->SetLogy();hlucrawnegadc1_15->Draw();
	c12->cd(16);gPad->SetLogy();hlucrawnegadc1_16->Draw();
	c12->Print("Luc_rawnegadc.gif");

	c13->Divide(4,4);
	c13->cd(1);gPad->SetLogy();hlucrawtotadc1_01->Draw();
	c13->cd(2);gPad->SetLogy();hlucrawtotadc1_02->Draw();
	c13->cd(3);gPad->SetLogy();hlucrawtotadc1_03->Draw();
	c13->cd(4);gPad->SetLogy();hlucrawtotadc1_04->Draw();
	c13->cd(5);gPad->SetLogy();hlucrawtotadc1_05->Draw();
	c13->cd(6);gPad->SetLogy();hlucrawtotadc1_06->Draw();
	c13->cd(7);gPad->SetLogy();hlucrawtotadc1_07->Draw();
	c13->cd(8);gPad->SetLogy();hlucrawtotadc1_08->Draw();
	c13->cd(9);gPad->SetLogy();hlucrawtotadc1_09->Draw();
	c13->cd(10);gPad->SetLogy();hlucrawtotadc1_10->Draw();
	c13->cd(11);gPad->SetLogy();hlucrawtotadc1_11->Draw();
	c13->cd(12);gPad->SetLogy();hlucrawtotadc1_12->Draw();
	c13->cd(13);gPad->SetLogy();hlucrawtotadc1_13->Draw();
	c13->cd(14);gPad->SetLogy();hlucrawtotadc1_14->Draw();
	c13->cd(15);gPad->SetLogy();hlucrawtotadc1_15->Draw();
	c13->cd(16);gPad->SetLogy();hlucrawtotadc1_16->Draw();
	c13->Print("Luc_rawsumadc.gif");


}

void HLucrawtdc(){
	TCanvas *c21 = new TCanvas("c21","c21");
	TCanvas *c22 = new TCanvas("c22","c22");
	TCanvas *c23 = new TCanvas("c23","c23");
	//	TCanvas *c24 = new TCanvas("c24","c24");
	c21->Divide(4,4);
	c21->cd(1);hlucrawpostdc1_01->Draw();
	c21->cd(2);hlucrawpostdc1_02->Draw();
	c21->cd(3);hlucrawpostdc1_03->Draw();
	c21->cd(4);hlucrawpostdc1_04->Draw();
	c21->cd(5);hlucrawpostdc1_05->Draw();
	c21->cd(6);hlucrawpostdc1_06->Draw();
	c21->cd(7);hlucrawpostdc1_07->Draw();
	c21->cd(8);hlucrawpostdc1_08->Draw();
	c21->cd(9);hlucrawpostdc1_09->Draw();
	c21->cd(10);hlucrawpostdc1_10->Draw();
	c21->cd(11);hlucrawpostdc1_11->Draw();
	c21->cd(12);hlucrawpostdc1_12->Draw();
	c21->cd(13);hlucrawpostdc1_13->Draw();
	c21->cd(14);hlucrawpostdc1_14->Draw();
	c21->cd(15);hlucrawpostdc1_15->Draw();
	c21->cd(16);hlucrawpostdc1_16->Draw();
	c21->Print("Luc_rawpostdc.gif");
		
	c22->Divide(4,4);
	c22->cd(1);hlucrawnegtdc1_01->Draw();
	c22->cd(2);hlucrawnegtdc1_02->Draw();
	c22->cd(3);hlucrawnegtdc1_03->Draw();
	c22->cd(4);hlucrawnegtdc1_04->Draw();
	c22->cd(5);hlucrawnegtdc1_05->Draw();
	c22->cd(6);hlucrawnegtdc1_06->Draw();
	c22->cd(7);hlucrawnegtdc1_07->Draw();
	c22->cd(8);hlucrawnegtdc1_08->Draw();
	c22->cd(9);hlucrawnegtdc1_09->Draw();
	c22->cd(10);hlucrawnegtdc1_10->Draw();
	c22->cd(11);hlucrawnegtdc1_11->Draw();
	c22->cd(12);hlucrawnegtdc1_12->Draw();
	c22->cd(13);hlucrawnegtdc1_13->Draw();
	c22->cd(14);hlucrawnegtdc1_14->Draw();
	c22->cd(15);hlucrawnegtdc1_15->Draw();
	c22->cd(16);hlucrawnegtdc1_16->Draw();
	c22->Print("Luc_rawnegtdc.gif");
	
	c23->Divide(4,4);
	c23->cd(1);hlucrawtottdc1_01->Draw();
	c23->cd(2);hlucrawtottdc1_02->Draw();
	c23->cd(3);hlucrawtottdc1_03->Draw();
	c23->cd(4);hlucrawtottdc1_04->Draw();
	c23->cd(5);hlucrawtottdc1_05->Draw();
	c23->cd(6);hlucrawtottdc1_06->Draw();
	c23->cd(7);hlucrawtottdc1_07->Draw();
	c23->cd(8);hlucrawtottdc1_08->Draw();
	c23->cd(9);hlucrawtottdc1_09->Draw();
	c23->cd(10);hlucrawtottdc1_10->Draw();
	c23->cd(11);hlucrawtottdc1_11->Draw();
	c23->cd(12);hlucrawtottdc1_12->Draw();
	c23->cd(13);hlucrawtottdc1_13->Draw();
	c23->cd(14);hlucrawtottdc1_14->Draw();
	c23->cd(15);hlucrawtottdc1_15->Draw();
	c23->cd(16);hlucrawtottdc1_16->Draw();
	c22->Print("Luc_rawsumtdc.gif");
	
	}

void HLucdecadc(){
	TCanvas *c31 = new TCanvas("c31","c31");
	TCanvas *c32 = new TCanvas("c32","c32");
	TCanvas *c33 = new TCanvas("c33","c33");
	//	TCanvas *c34 = new TCanvas("c34","c34");
	c31->Divide(4,4);
	c31->cd(1);hlucdecposadc1_01->Draw();
	c31->cd(2);hlucdecposadc1_02->Draw();
	c31->cd(3);hlucdecposadc1_03->Draw();
	c31->cd(4);hlucdecposadc1_04->Draw();
	c31->cd(5);hlucdecposadc1_05->Draw();
	c31->cd(6);hlucdecposadc1_06->Draw();
	c31->cd(7);hlucdecposadc1_07->Draw();
	c31->cd(8);hlucrawposadc1_08->Draw();
	c31->cd(9);hlucdecposadc1_09->Draw();
	c31->cd(10);hlucdecposadc1_10->Draw();
	c31->cd(11);hlucdecposadc1_11->Draw();
	c31->cd(12);hlucdecposadc1_12->Draw();
	c31->cd(13);hlucdecposadc1_13->Draw();
	c31->cd(14);hlucdecposadc1_14->Draw();
	c31->cd(15);hlucdecposadc1_15->Draw();
	c31->cd(16);hlucdecposadc1_16->Draw();

	c32->Divide(4,4);
	c32->cd(1);hlucdecnegadc1_01->Draw();
	c32->cd(2);hlucdecnegadc1_02->Draw();
	c32->cd(3);hlucdecnegadc1_03->Draw();
	c32->cd(4);hlucdecnegadc1_04->Draw();
	c32->cd(5);hlucdecnegadc1_05->Draw();
	c32->cd(6);hlucdecnegadc1_06->Draw();
	c32->cd(7);hlucdecnegadc1_07->Draw();
	c32->cd(8);hlucdecnegadc1_08->Draw();
	c32->cd(9);hlucdecnegadc1_09->Draw();
	c32->cd(10);hlucdecnegadc1_10->Draw();
	c32->cd(11);hlucdecnegadc1_11->Draw();
	c32->cd(12);hlucdecnegadc1_12->Draw();
	c32->cd(13);hlucdecnegadc1_13->Draw();
	c32->cd(14);hlucdecnegadc1_14->Draw();
	c32->cd(15);hlucdecnegadc1_15->Draw();
	c32->cd(16);hlucdecnegadc1_16->Draw();
	
	c33->Divide(4,4);
	c33->cd(1);hlucdectotadc1_01->Draw();
	c33->cd(2);hlucdectotadc1_02->Draw();
	c33->cd(3);hlucdectotadc1_03->Draw();
	c33->cd(4);hlucdectotadc1_04->Draw();
	c33->cd(5);hlucdectotadc1_05->Draw();
	c33->cd(6);hlucdectotadc1_06->Draw();
	c33->cd(7);hlucdectotadc1_07->Draw();
	c33->cd(8);hlucdectotadc1_08->Draw();
	c33->cd(9);hlucdectotadc1_09->Draw();
	c33->cd(10);hlucdectotadc1_10->Draw();
	c33->cd(11);hlucdectotadc1_11->Draw();
	c33->cd(12);hlucdectotadc1_12->Draw();
	c33->cd(13);hlucdectotadc1_13->Draw();
	c33->cd(14);hlucdectotadc1_14->Draw();
	c33->cd(15);hlucdectotadc1_15->Draw();
	c33->cd(16);hlucdectotadc1_16->Draw();
}

void HLucdectdc(){
	TCanvas *c41 = new TCanvas("c41","c41");
	TCanvas *c42 = new TCanvas("c42","c42");
	TCanvas *c43 = new TCanvas("c43","c43");
	TCanvas *c44 = new TCanvas("c44","c44");
		c31->Divide(4,4);
	c41->cd(1);hlucdecpostdc1_01->Draw();
	c41->cd(2);hlucdecpostdc1_02->Draw();
	c41->cd(3);hlucdecpostdc1_03->Draw();
	c41->cd(4);hlucdecpostdc1_04->Draw();
	c41->cd(5);hlucdecpostdc1_05->Draw();
	c41->cd(6);hlucdecpostdc1_06->Draw();
	c41->cd(7);hlucdecpostdc1_07->Draw();
	c41->cd(8);hlucrawpostdc1_08->Draw();
	c41->cd(9);hlucdecpostdc1_09->Draw();
	c41->cd(10);hlucdecpostdc1_10->Draw();
	c41->cd(11);hlucdecpostdc1_11->Draw();
	c41->cd(12);hlucdecpostdc1_12->Draw();
	c41->cd(13);hlucdecpostdc1_13->Draw();
	c41->cd(14);hlucdecpostdc1_14->Draw();
	c41->cd(15);hlucdecpostdc1_15->Draw();
	c41->cd(16);hlucdecpostdc1_16->Draw();

	c42->Divide(4,4);
	c42->cd(1);hlucdecnegtdc1_01->Draw();
	c42->cd(2);hlucdecnegtdc1_02->Draw();
	c42->cd(3);hlucdecnegtdc1_03->Draw();
	c42->cd(4);hlucdecnegtdc1_04->Draw();
	c42->cd(5);hlucdecnegtdc1_05->Draw();
	c42->cd(6);hlucdecnegtdc1_06->Draw();
	c42->cd(7);hlucdecnegtdc1_07->Draw();
	c42->cd(8);hlucdecnegtdc1_08->Draw();
	c42->cd(9);hlucdecnegtdc1_09->Draw();
	c42->cd(10);hlucdecnegtdc1_10->Draw();
	c42->cd(11);hlucdecnegtdc1_11->Draw();
	c42->cd(12);hlucdecnegtdc1_12->Draw();
	c42->cd(13);hlucdecnegtdc1_13->Draw();
	c42->cd(14);hlucdecnegtdc1_14->Draw();
	c42->cd(15);hlucdecnegtdc1_15->Draw();
	c42->cd(16);hlucdecnegtdc1_16->Draw();
	
	c43->Divide(4,4);
	c43->cd(1);hlucdectottdc1_01->Draw();
	c43->cd(2);hlucdectottdc1_02->Draw();
	c43->cd(3);hlucdectottdc1_03->Draw();
	c43->cd(4);hlucdectottdc1_04->Draw();
	c43->cd(5);hlucdectottdc1_05->Draw();
	c43->cd(6);hlucdectottdc1_06->Draw();
	c43->cd(7);hlucdectottdc1_07->Draw();
	c43->cd(8);hlucdectottdc1_08->Draw();
	c43->cd(9);hlucdectottdc1_09->Draw();
	c43->cd(10);hlucdectottdc1_10->Draw();
	c43->cd(11);hlucdectottdc1_11->Draw();
	c43->cd(12);hlucdectottdc1_12->Draw();
	c43->cd(13);hlucdectottdc1_13->Draw();
	c43->cd(14);hlucdectottdc1_14->Draw();
	c43->cd(15);hlucdectottdc1_15->Draw();
	c43->cd(16);hlucdectottdc1_16->Draw();
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


