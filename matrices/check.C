#include <iostream>
#include <fstream>

char filename[100]; 
char filename1[100]; 
char filename2[100]; 

void check(int runnum=10000){
	gROOT->ForceStyle(kTRUE);
	gStyle->SetHistFillStyle(3002);
	gStyle->SetHistFillColor(40);
	char str[100];
	sprintf(str,"run# = %d",runnum);
	sprintf(filename,"run%d.pdf",runnum);
	sprintf(filename1,"run%d.pdf(",runnum);
	sprintf(filename2,"run%d.pdf]",runnum);
	TCanvas *c1 = new TCanvas("c1","c1");
   TPaveText* pt1 = new TPaveText(0.2,0.5,0.8,0.7);
	TText *t1 = pt1->AddText("E05115 histograms");
	TText *t2 = pt1->AddText(str);
	pt1->Draw();
   c1->Print(filename1,"pdf");       
   c1->Clear();       
	alias(runnum);
	KDC();
	KHODO();
	WC();
	AC();
	EDC();
	EHODO();
	c1->Print(filename2,"pdf");
}


void KDC(){
	c1->Divide(1,2);
	hdcrawtothitzoom->SetMinimum(0.);
	c1->cd(1);hdcrawtothitzoom->Draw();
	c1->cd(2);hdcrawtdcall->Draw();
	c1->Print(filename);
	c1->Clear();  
	
	c1->Divide(2,3);
	hdcrawhitpat01->SetMinimum(0.);
	hdcrawhitpat02->SetMinimum(0.);
	hdcrawhitpat03->SetMinimum(0.);
	hdcrawhitpat04->SetMinimum(0.);
	hdcrawhitpat05->SetMinimum(0.);
	hdcrawhitpat06->SetMinimum(0.);
	c1->cd(1);hdcrawhitpat01->Draw();
	c1->cd(2);hdcrawhitpat02->Draw();
	c1->cd(3);hdcrawhitpat03->Draw();
	c1->cd(4);hdcrawhitpat04->Draw();
	c1->cd(5);hdcrawhitpat05->Draw();
	c1->cd(6);hdcrawhitpat06->Draw();
	c1->Print(filename);
	c1->Clear();  
	
	c1->Divide(2,3);
	hdcrawhitpat07->SetMinimum(0.);
	hdcrawhitpat08->SetMinimum(0.);
	hdcrawhitpat09->SetMinimum(0.);
	hdcrawhitpat10->SetMinimum(0.);
	hdcrawhitpat11->SetMinimum(0.);
	hdcrawhitpat12->SetMinimum(0.);
	c1->cd(1);hdcrawhitpat07->Draw();
	c1->cd(2);hdcrawhitpat08->Draw();
	c1->cd(3);hdcrawhitpat09->Draw();
	c1->cd(4);hdcrawhitpat10->Draw();
	c1->cd(5);hdcrawhitpat11->Draw();
	c1->cd(6);hdcrawhitpat12->Draw();
	c1->Print(filename);
	c1->Clear();  
	
	c1->Divide(2,3);
	hdcrawlayertdc01->SetMinimum(0.);
	hdcrawlayertdc02->SetMinimum(0.);
	hdcrawlayertdc03->SetMinimum(0.);
	hdcrawlayertdc04->SetMinimum(0.);
	hdcrawlayertdc05->SetMinimum(0.);
	hdcrawlayertdc06->SetMinimum(0.);
	c1->cd(1);hdcrawlayertdc01->Draw();
	c1->cd(2);hdcrawlayertdc02->Draw();
	c1->cd(3);hdcrawlayertdc03->Draw();
	c1->cd(4);hdcrawlayertdc04->Draw();
	c1->cd(5);hdcrawlayertdc05->Draw();
	c1->cd(6);hdcrawlayertdc06->Draw();
	c1->Print(filename);
	c1->Clear();  
	
	c1->Divide(2,3);
	hdcrawlayertdc07->SetMinimum(0.);
	hdcrawlayertdc08->SetMinimum(0.);
	hdcrawlayertdc09->SetMinimum(0.);
	hdcrawlayertdc10->SetMinimum(0.);
	hdcrawlayertdc11->SetMinimum(0.);
	hdcrawlayertdc12->SetMinimum(0.);
	c1->cd(1);hdcrawlayertdc07->Draw();
	c1->cd(2);hdcrawlayertdc08->Draw();
	c1->cd(3);hdcrawlayertdc09->Draw();
	c1->cd(4);hdcrawlayertdc10->Draw();
	c1->cd(5);hdcrawlayertdc11->Draw();
	c1->cd(6);hdcrawlayertdc12->Draw();
	c1->Print(filename);
	c1->Clear();  
	
}

void KHODO(){
	c1->Divide(1,2);
	hscindectothits->SetMinimum(0.);
	hscinlayer->SetMinimum(0.);
	c1->cd(1);hscindectothits->Draw();
	c1->cd(2);hscinlayer->Draw();
	c1->Print(filename);
	c1->Clear();  
 	
	c1->Divide(1,3);
	hscincounters1->SetMinimum(0.);  
 	hscincounters2->SetMinimum(0.);  
 	hscincounters3->SetMinimum(0.);
	c1->cd(1);hscincounters1->Draw();  
 	c1->cd(2);hscincounters2->Draw();  
 	c1->cd(3);hscincounters3->Draw();
	c1->Print(filename);
	c1->Clear();  
 	
	c1->Divide(2,3);
	c1->cd(1);hscinrawadchitpatpos1->SetMinimum(0.);
	hscinrawadchitpatpos1->Draw();
	c1->cd(2);hscinrawadchitpatneg1->SetMinimum(0.);
	hscinrawadchitpatneg1->Draw();
	c1->cd(3);hscinrawadchitpatpos2->SetMinimum(0.);
	hscinrawadchitpatpos2->Draw();
	c1->cd(4);hscinrawadchitpatneg2->SetMinimum(0.);
	hscinrawadchitpatneg2->Draw();
 	c1->cd(5);hscinrawadchitpatpos3->SetMinimum(0.);
	hscinrawadchitpatpos3->Draw();
 	c1->cd(6);hscinrawadchitpatneg3->SetMinimum(0.);
	hscinrawadchitpatneg3->Draw();
	c1->Print(filename);
	c1->Clear();  
	
	c1->Divide(2,3);
	c1->cd(1);hscinrawtdchitpatpos1->SetMinimum(0.);
	hscinrawtdchitpatpos1->Draw();
	c1->cd(2);hscinrawtdchitpatneg1->SetMinimum(0.);
	hscinrawtdchitpatneg1->Draw();
	c1->cd(3);hscinrawtdchitpatpos2->SetMinimum(0.);
	hscinrawtdchitpatpos2->Draw();
	c1->cd(4);hscinrawtdchitpatneg2->SetMinimum(0.);
	hscinrawtdchitpatneg2->Draw();
 	c1->cd(5);hscinrawtdchitpatpos3->SetMinimum(0.);
	hscinrawtdchitpatpos3->Draw();
 	c1->cd(6);hscinrawtdchitpatneg3->SetMinimum(0.);
	hscinrawtdchitpatneg3->Draw();
	c1->Print(filename);
	c1->Clear();  
	
	c1->Divide(2,3);
	hscinpatadcpos1->SetMinimum(0.);
 	hscinpatadcneg1->SetMinimum(0.);  
 	hscinpatadcpos2->SetMinimum(0.);  
 	hscinpatadcneg2->SetMinimum(0.);  
 	hscinpatadcpos3->SetMinimum(0.);  
 	hscinpatadcneg3->SetMinimum(0.);  
	c1->cd(1);hscinpatadcpos1->Draw();
 	c1->cd(2);hscinpatadcneg1->Draw();  
 	c1->cd(3);hscinpatadcpos2->Draw();  
 	c1->cd(4);hscinpatadcneg2->Draw();  
 	c1->cd(5);hscinpatadcpos3->Draw();  
 	c1->cd(6);hscinpatadcneg3->Draw();  
	c1->Print(filename);
	c1->Clear();  
 	
	c1->Divide(2,3);
	hscinpattdcpos1->SetMinimum(0.);
 	hscinpattdcneg1->SetMinimum(0.);  
 	hscinpattdcpos2->SetMinimum(0.);  
 	hscinpattdcneg2->SetMinimum(0.);  
 	hscinpattdcpos3->SetMinimum(0.);  
 	hscinpattdcneg3->SetMinimum(0.);  
	c1->cd(1);hscinpattdcpos1->Draw();
	c1->cd(2);hscinpattdcneg1->Draw();
	c1->cd(3);hscinpattdcpos2->Draw();
	c1->cd(4);hscinpattdcneg2->Draw();
 	c1->cd(5);hscinpattdcpos3->Draw();
 	c1->cd(6);hscinpattdcneg3->Draw();
	c1->Print(filename);
	c1->Clear();  

	/*c1->Divide(2,3);
	c1->cd(1);hscinsumpostdc1->Draw();
	c1->cd(2);hscinsumnegtdc1->Draw();
	c1->cd(3);hscinsumpostdc2->Draw();
	c1->cd(4);hscinsumnegtdc2->Draw();
 	c1->cd(5);hscinsumpostdc3->Draw();
 	c1->cd(6);hscinsumnegtdc3->Draw();
	c1->Print(filename);
	c1->Clear();*/  
	
	c1->Divide(1,3);
	hscinpatadcboth1->SetMinimum(0.);
 	hscinpatadcboth2->SetMinimum(0.);
 	hscinpatadcboth3->SetMinimum(0.);
	c1->cd(1);hscinpatadcboth1->Draw();
 	c1->cd(2);hscinpatadcboth2->Draw();
 	c1->cd(3);hscinpatadcboth3->Draw();
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(1,3);
	hscinpattdcboth1->SetMinimum(0.);
 	hscinpattdcboth2->SetMinimum(0.);
 	hscinpattdcboth3->SetMinimum(0.);
 	c1->cd(1);hscinpattdcboth1->Draw();
 	c1->cd(2);hscinpattdcboth2->Draw();
 	c1->cd(3);hscinpattdcboth3->Draw();
	c1->Print(filename);
	c1->Clear();  
 	
	c1->Divide(2,3);
	DrawADC(hscinrawposadc1_01, hscinrawnegadc1_01, 1);
	DrawADC(hscinrawposadc1_02, hscinrawnegadc1_02, 2);
	DrawADC(hscinrawposadc1_03, hscinrawnegadc1_03, 3);
	DrawADC(hscinrawposadc1_04, hscinrawnegadc1_04, 4);
	DrawADC(hscinrawposadc1_05, hscinrawnegadc1_05, 5);
	DrawADC(hscinrawposadc1_06, hscinrawnegadc1_06, 6);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawADC(hscinrawposadc1_07, hscinrawnegadc1_07, 1);
	DrawADC(hscinrawposadc1_08, hscinrawnegadc1_08, 2);
	DrawADC(hscinrawposadc1_09, hscinrawnegadc1_09, 3);
	DrawADC(hscinrawposadc1_10, hscinrawnegadc1_10, 4);
	DrawADC(hscinrawposadc1_11, hscinrawnegadc1_11, 5);
	DrawADC(hscinrawposadc1_12, hscinrawnegadc1_12, 6);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawADC(hscinrawposadc1_13, hscinrawnegadc1_13, 1);
	DrawADC(hscinrawposadc1_14, hscinrawnegadc1_14, 2);
	DrawADC(hscinrawposadc1_15, hscinrawnegadc1_15, 3);
	DrawADC(hscinrawposadc1_16, hscinrawnegadc1_16, 4);
	DrawADC(hscinrawposadc1_17, hscinrawnegadc1_17, 5);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawADC(hscinrawposadc2_01, hscinrawnegadc2_01, 1);
	DrawADC(hscinrawposadc2_02, hscinrawnegadc2_02, 2);
	DrawADC(hscinrawposadc2_03, hscinrawnegadc2_03, 3);
	DrawADC(hscinrawposadc2_04, hscinrawnegadc2_04, 4);
	DrawADC(hscinrawposadc2_05, hscinrawnegadc2_05, 5);
	DrawADC(hscinrawposadc2_06, hscinrawnegadc2_06, 6);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawADC(hscinrawposadc2_07, hscinrawnegadc2_07, 1);
	DrawADC(hscinrawposadc2_08, hscinrawnegadc2_08, 2);
	DrawADC(hscinrawposadc2_09, hscinrawnegadc2_09, 3);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawADC(hscinrawposadc3_01, hscinrawnegadc3_01, 1);
	DrawADC(hscinrawposadc3_02, hscinrawnegadc3_02, 2);
	DrawADC(hscinrawposadc3_03, hscinrawnegadc3_03, 3);
	DrawADC(hscinrawposadc3_04, hscinrawnegadc3_04, 4);
	DrawADC(hscinrawposadc3_05, hscinrawnegadc3_05, 5);
	DrawADC(hscinrawposadc3_06, hscinrawnegadc3_06, 6);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawADC(hscinrawposadc3_07, hscinrawnegadc3_07, 1);
	DrawADC(hscinrawposadc3_08, hscinrawnegadc3_08, 2);
	DrawADC(hscinrawposadc3_09, hscinrawnegadc3_09, 3);
	DrawADC(hscinrawposadc3_10, hscinrawnegadc3_10, 4);
	DrawADC(hscinrawposadc3_11, hscinrawnegadc3_11, 5);
	DrawADC(hscinrawposadc3_12, hscinrawnegadc3_12, 6);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawADC(hscinrawposadc3_13, hscinrawnegadc3_13, 1);
	DrawADC(hscinrawposadc3_14, hscinrawnegadc3_14, 2);
	DrawADC(hscinrawposadc3_15, hscinrawnegadc3_15, 3);
	DrawADC(hscinrawposadc3_16, hscinrawnegadc3_16, 4);
	DrawADC(hscinrawposadc3_17, hscinrawnegadc3_17, 5);
	DrawADC(hscinrawposadc3_18, hscinrawnegadc3_18, 6);
	c1->Print(filename);
	c1->Clear();  
	
	c1->Divide(2,3);
	DrawTDC(hscinrawpostdc1_01, hscinrawnegtdc1_01, 1);
	DrawTDC(hscinrawpostdc1_02, hscinrawnegtdc1_02, 2);
	DrawTDC(hscinrawpostdc1_03, hscinrawnegtdc1_03, 3);
	DrawTDC(hscinrawpostdc1_04, hscinrawnegtdc1_04, 4);
	DrawTDC(hscinrawpostdc1_05, hscinrawnegtdc1_05, 5);
	DrawTDC(hscinrawpostdc1_06, hscinrawnegtdc1_06, 6);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawTDC(hscinrawpostdc1_07, hscinrawnegtdc1_07, 1);
	DrawTDC(hscinrawpostdc1_08, hscinrawnegtdc1_08, 2);
	DrawTDC(hscinrawpostdc1_09, hscinrawnegtdc1_09, 3);
	DrawTDC(hscinrawpostdc1_10, hscinrawnegtdc1_10, 4);
	DrawTDC(hscinrawpostdc1_11, hscinrawnegtdc1_11, 5);
	DrawTDC(hscinrawpostdc1_12, hscinrawnegtdc1_12, 6);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawTDC(hscinrawpostdc1_13, hscinrawnegtdc1_13, 1);
	DrawTDC(hscinrawpostdc1_14, hscinrawnegtdc1_14, 2);
	DrawTDC(hscinrawpostdc1_15, hscinrawnegtdc1_15, 3);
	DrawTDC(hscinrawpostdc1_16, hscinrawnegtdc1_16, 4);
	DrawTDC(hscinrawpostdc1_17, hscinrawnegtdc1_17, 5);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawTDC(hscinrawpostdc2_01, hscinrawnegtdc2_01, 1);
	DrawTDC(hscinrawpostdc2_02, hscinrawnegtdc2_02, 2);
	DrawTDC(hscinrawpostdc2_03, hscinrawnegtdc2_03, 3);
	DrawTDC(hscinrawpostdc2_04, hscinrawnegtdc2_04, 4);
	DrawTDC(hscinrawpostdc2_05, hscinrawnegtdc2_05, 5);
	DrawTDC(hscinrawpostdc2_06, hscinrawnegtdc2_06, 6);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawTDC(hscinrawpostdc2_07, hscinrawnegtdc2_07, 1);
	DrawTDC(hscinrawpostdc2_08, hscinrawnegtdc2_08, 2);
	DrawTDC(hscinrawpostdc2_09, hscinrawnegtdc2_09, 3);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawTDC(hscinrawpostdc3_01, hscinrawnegtdc3_01, 1);
	DrawTDC(hscinrawpostdc3_02, hscinrawnegtdc3_02, 2);
	DrawTDC(hscinrawpostdc3_03, hscinrawnegtdc3_03, 3);
	DrawTDC(hscinrawpostdc3_04, hscinrawnegtdc3_04, 4);
	DrawTDC(hscinrawpostdc3_05, hscinrawnegtdc3_05, 5);
	DrawTDC(hscinrawpostdc3_06, hscinrawnegtdc3_06, 6);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawTDC(hscinrawpostdc3_07, hscinrawnegtdc3_07, 1);
	DrawTDC(hscinrawpostdc3_08, hscinrawnegtdc3_08, 2);
	DrawTDC(hscinrawpostdc3_09, hscinrawnegtdc3_09, 3);
	DrawTDC(hscinrawpostdc3_10, hscinrawnegtdc3_10, 4);
	DrawTDC(hscinrawpostdc3_11, hscinrawnegtdc3_11, 5);
	DrawTDC(hscinrawpostdc3_12, hscinrawnegtdc3_12, 6);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawTDC(hscinrawpostdc3_13, hscinrawnegtdc3_13, 1);
	DrawTDC(hscinrawpostdc3_14, hscinrawnegtdc3_14, 2);
	DrawTDC(hscinrawpostdc3_15, hscinrawnegtdc3_15, 3);
	DrawTDC(hscinrawpostdc3_16, hscinrawnegtdc3_16, 4);
	DrawTDC(hscinrawpostdc3_17, hscinrawnegtdc3_17, 5);
	DrawTDC(hscinrawpostdc3_18, hscinrawnegtdc3_18, 6);
	c1->Print(filename);
	c1->Clear();  

	
}


void AC()
{
	c1->Divide(2,3);
	c1->cd(1);haerrawadchitpatpos1->SetMinimum(0.);
	haerrawadchitpatpos1->Draw();
	c1->cd(2);haerrawadchitpatneg1->SetMinimum(0.);
	haerrawadchitpatneg1->Draw();
	c1->cd(3);haerrawadchitpatpos2->SetMinimum(0.);
	haerrawadchitpatpos2->Draw();
	c1->cd(4);haerrawadchitpatneg2->SetMinimum(0.);
	haerrawadchitpatneg2->Draw();
	c1->cd(5);haerrawadchitpatpos3->SetMinimum(0.);
	haerrawadchitpatpos3->Draw();
	c1->cd(6);haerrawadchitpatneg3->SetMinimum(0.);
	haerrawadchitpatneg3->Draw();
	c1->Print(filename);
	c1->Clear();  
	
	c1->Divide(2,3);
	c1->cd(1);haerrawtdchitpatpos1->SetMinimum(0.);
	haerrawtdchitpatpos1->Draw();
	c1->cd(2);haerrawtdchitpatneg1->SetMinimum(0.);
	haerrawtdchitpatneg1->Draw();
	c1->cd(3);haerrawtdchitpatpos2->SetMinimum(0.);
	haerrawtdchitpatpos2->Draw();
	c1->cd(4);haerrawtdchitpatneg2->SetMinimum(0.);
	haerrawtdchitpatneg2->Draw();
	c1->cd(5);haerrawtdchitpatpos3->SetMinimum(0.);
	haerrawtdchitpatpos3->Draw();
	c1->cd(6);haerrawtdchitpatneg3->SetMinimum(0.);
	haerrawtdchitpatneg3->Draw();
	c1->Print(filename);
	c1->Clear();  
	
	c1->Divide(2,4);
	DrawADC(haerrawposadc1_1,haerrawnegadc1_1,1);
	DrawADC(haerrawposadc1_2,haerrawnegadc1_2,2);
	DrawADC(haerrawposadc1_3,haerrawnegadc1_3,3);
	DrawADC(haerrawposadc1_4,haerrawnegadc1_4,4);
	DrawADC(haerrawposadc1_5,haerrawnegadc1_5,5);
	DrawADC(haerrawposadc1_6,haerrawnegadc1_6,6);
	DrawADC(haerrawposadc1_7,haerrawnegadc1_7,7);
	c1->Print(filename);
	c1->Clear();  
	
	c1->Divide(2,4);
	DrawADC(haerrawposadc2_1,haerrawnegadc2_1,1);
	DrawADC(haerrawposadc2_2,haerrawnegadc2_2,2);
	DrawADC(haerrawposadc2_3,haerrawnegadc2_3,3);
	DrawADC(haerrawposadc2_4,haerrawnegadc2_4,4);
	DrawADC(haerrawposadc2_5,haerrawnegadc2_5,5);
	DrawADC(haerrawposadc2_6,haerrawnegadc2_6,6);
	DrawADC(haerrawposadc2_7,haerrawnegadc2_7,7);
	c1->Print(filename);
	c1->Clear(); 

	c1->Divide(2,4);
	DrawADC(haerrawposadc3_1,haerrawnegadc3_1,1);
	DrawADC(haerrawposadc3_2,haerrawnegadc3_2,2);
	DrawADC(haerrawposadc3_3,haerrawnegadc3_3,3);
	DrawADC(haerrawposadc3_4,haerrawnegadc3_4,4);
	DrawADC(haerrawposadc3_5,haerrawnegadc3_5,5);
	DrawADC(haerrawposadc3_6,haerrawnegadc3_6,6);
	DrawADC(haerrawposadc3_7,haerrawnegadc3_7,7);
	c1->Print(filename);
	c1->Clear();   	

	//AC TDC
	c1->Divide(2,4);
	DrawTDC(haerrawpostdc1_1,haerrawnegtdc1_1,1);
	DrawTDC(haerrawpostdc1_2,haerrawnegtdc1_2,2);
	DrawTDC(haerrawpostdc1_3,haerrawnegtdc1_3,3);
	DrawTDC(haerrawpostdc1_4,haerrawnegtdc1_4,4);
	DrawTDC(haerrawpostdc1_5,haerrawnegtdc1_5,5);
	DrawTDC(haerrawpostdc1_6,haerrawnegtdc1_6,6);
	DrawTDC(haerrawpostdc1_7,haerrawnegtdc1_7,7);
	c1->Print(filename);
	c1->Clear();  
	
	c1->Divide(2,4);
	DrawTDC(haerrawpostdc2_1,haerrawnegtdc2_1,1);
	DrawTDC(haerrawpostdc2_2,haerrawnegtdc2_2,2);
	DrawTDC(haerrawpostdc2_3,haerrawnegtdc2_3,3);
	DrawTDC(haerrawpostdc2_4,haerrawnegtdc2_4,4);
	DrawTDC(haerrawpostdc2_5,haerrawnegtdc2_5,5);
	DrawTDC(haerrawpostdc2_6,haerrawnegtdc2_6,6);
	DrawTDC(haerrawpostdc2_7,haerrawnegtdc2_7,7);
	c1->Print(filename);
	c1->Clear(); 

	c1->Divide(2,4);
	DrawTDC(haerrawpostdc3_1,haerrawnegtdc3_1,1);
	DrawTDC(haerrawpostdc3_2,haerrawnegtdc3_2,2);
	DrawTDC(haerrawpostdc3_3,haerrawnegtdc3_3,3);
	DrawTDC(haerrawpostdc3_4,haerrawnegtdc3_4,4);
	DrawTDC(haerrawpostdc3_5,haerrawnegtdc3_5,5);
	DrawTDC(haerrawpostdc3_6,haerrawnegtdc3_6,6);
	DrawTDC(haerrawpostdc3_7,haerrawnegtdc3_7,7);
	c1->Print(filename);
	c1->Clear();   
}

void WC()
{
	c1->Divide(2,2);
	c1->cd(1);hwatrawadchitpatpos1->SetMinimum(0.);
	hwatrawadchitpatpos1->Draw();
	c1->cd(2);hwatrawadchitpatneg1->SetMinimum(0.);
	hwatrawadchitpatneg1->Draw();
	c1->cd(3);hwatrawadchitpatpos2->SetMinimum(0.);
	hwatrawadchitpatpos2->Draw();
	c1->cd(4);hwatrawadchitpatneg2->SetMinimum(0.);
	hwatrawadchitpatneg2->Draw();
	c1->Print(filename);
	c1->Clear();  
	
	c1->Divide(2,2);
	c1->cd(1);hwatrawtdchitpatpos1->SetMinimum(0.);
	hwatrawtdchitpatpos1->Draw();
	c1->cd(2);hwatrawtdchitpatneg1->SetMinimum(0.);
	hwatrawtdchitpatneg1->Draw();
	c1->cd(3);hwatrawtdchitpatpos2->SetMinimum(0.);
	hwatrawtdchitpatpos2->Draw();
	c1->cd(4);hwatrawtdchitpatneg2->SetMinimum(0.);
	hwatrawtdchitpatneg2->Draw();
	c1->Print(filename);
	c1->Clear();  

	//WC 1-1 to 1-12 adc
	c1->Divide(2,3);
	DrawADC(hwatrawposadc1_01,hwatrawnegadc1_01,1);
	DrawADC(hwatrawposadc1_02,hwatrawnegadc1_02,2);
	DrawADC(hwatrawposadc1_03,hwatrawnegadc1_03,3);
	DrawADC(hwatrawposadc1_04,hwatrawnegadc1_04,4);
	DrawADC(hwatrawposadc1_05,hwatrawnegadc1_05,5);
	DrawADC(hwatrawposadc1_06,hwatrawnegadc1_06,6);
	c1->Print(filename);
	c1->Clear(); 
 
	c1->Divide(2,3);
	DrawADC(hwatrawposadc1_07,hwatrawnegadc1_07,1);
	DrawADC(hwatrawposadc1_08,hwatrawnegadc1_08,2);
	DrawADC(hwatrawposadc1_09,hwatrawnegadc1_09,3);
	DrawADC(hwatrawposadc1_10,hwatrawnegadc1_10,4);
	DrawADC(hwatrawposadc1_11,hwatrawnegadc1_11,5);
	DrawADC(hwatrawposadc1_12,hwatrawnegadc1_12,6);
	c1->Print(filename);
	c1->Clear();  

	//WC 2-1 to 1-12 adc
	c1->Divide(2,3);
	DrawADC(hwatrawposadc2_01,hwatrawnegadc2_01,1);
	DrawADC(hwatrawposadc2_02,hwatrawnegadc2_02,2);
	DrawADC(hwatrawposadc2_03,hwatrawnegadc2_03,3);
	DrawADC(hwatrawposadc2_04,hwatrawnegadc2_04,4);
	DrawADC(hwatrawposadc2_05,hwatrawnegadc2_05,5);
	DrawADC(hwatrawposadc2_06,hwatrawnegadc2_06,6);
	c1->Print(filename);
	c1->Clear(); 
 
	c1->Divide(2,3);
	DrawADC(hwatrawposadc2_07,hwatrawnegadc2_07,1);
	DrawADC(hwatrawposadc2_08,hwatrawnegadc2_08,2);
	DrawADC(hwatrawposadc2_09,hwatrawnegadc2_09,3);
	DrawADC(hwatrawposadc2_10,hwatrawnegadc2_10,4);
	DrawADC(hwatrawposadc2_11,hwatrawnegadc2_11,5);
	DrawADC(hwatrawposadc2_12,hwatrawnegadc2_12,6);
	c1->Print(filename);
	c1->Clear();  

	//WC 1-1 to 1-12 tdc
	c1->Divide(2,3);
	c1->cd(1);hwatrawpostdc1_01->Draw();
	c1->cd(2);hwatrawpostdc1_02->Draw();
	c1->cd(3);hwatrawpostdc1_03->Draw();
	c1->cd(4);hwatrawpostdc1_04->Draw();
	c1->cd(5);hwatrawpostdc1_05->Draw();
	c1->cd(6);hwatrawpostdc1_06->Draw();
	c1->Print(filename);
	c1->Clear(); 
 
	c1->Divide(2,3);
	c1->cd(1);hwatrawpostdc1_07->Draw();
	c1->cd(2);hwatrawpostdc1_08->Draw();
	c1->cd(3);hwatrawpostdc1_09->Draw();
	c1->cd(4);hwatrawpostdc1_10->Draw();
	c1->cd(5);hwatrawpostdc1_11->Draw();
	c1->cd(6);hwatrawpostdc1_12->Draw();
	c1->Print(filename);
	c1->Clear();  

	//WC 2-1 to 1-12 tdc
	c1->Divide(2,3);
	c1->cd(1);hwatrawpostdc2_01->Draw();
	c1->cd(2);hwatrawpostdc2_02->Draw();
	c1->cd(3);hwatrawpostdc2_03->Draw();
	c1->cd(4);hwatrawpostdc2_04->Draw();
	c1->cd(5);hwatrawpostdc2_05->Draw();
	c1->cd(6);hwatrawpostdc2_06->Draw();
	c1->Print(filename);
	c1->Clear(); 
 
	c1->Divide(2,3);
	c1->cd(1);hwatrawpostdc2_07->Draw();
	c1->cd(2);hwatrawpostdc2_08->Draw();
	c1->cd(3);hwatrawpostdc2_09->Draw();
	c1->cd(4);hwatrawpostdc2_10->Draw();
	c1->cd(5);hwatrawpostdc2_11->Draw();
	c1->cd(6);hwatrawpostdc2_12->Draw();
	c1->Print(filename);
	c1->Clear();  

}

void EDC(){
	c1->Divide(1,2);
	c1->cd(1);edc1rawtothitzoom->Draw();
	c1->cd(2);edc1rawtdcall->Draw();
	c1->Print(filename);
	c1->Clear();  
	
	c1->Divide(2,3);
	edc1rawhitpat01->SetMinimum(0.);
	edc1rawhitpat02->SetMinimum(0.);
	edc1rawhitpat03->SetMinimum(0.);
	edc1rawhitpat04->SetMinimum(0.);
	edc1rawhitpat05->SetMinimum(0.);
	c1->cd(1);edc1rawhitpat01->Draw();
	c1->cd(2);edc1rawhitpat02->Draw();
	c1->cd(3);edc1rawhitpat03->Draw();
	c1->cd(4);edc1rawhitpat04->Draw();
	c1->cd(5);edc1rawhitpat05->Draw();
	c1->Print(filename);
	c1->Clear();  
	
	c1->Divide(2,3);
	edc1rawhitpat06->SetMinimum(0.);
	edc1rawhitpat07->SetMinimum(0.);
	edc1rawhitpat08->SetMinimum(0.);
	edc1rawhitpat09->SetMinimum(0.);
	edc1rawhitpat10->SetMinimum(0.);
	c1->cd(1);edc1rawhitpat06->Draw();
	c1->cd(2);edc1rawhitpat07->Draw();
	c1->cd(3);edc1rawhitpat08->Draw();
	c1->cd(4);edc1rawhitpat09->Draw();
	c1->cd(5);edc1rawhitpat10->Draw();
	c1->Print(filename);
	c1->Clear();  
	
	c1->Divide(2,3);
	c1->cd(1);edc1rawlayertdc01->Draw();
	c1->cd(2);edc1rawlayertdc02->Draw();
	c1->cd(3);edc1rawlayertdc03->Draw();
	c1->cd(4);edc1rawlayertdc04->Draw();
	c1->cd(5);edc1rawlayertdc05->Draw();
	c1->Print(filename);
	c1->Clear();  
	
	c1->Divide(2,3);
	c1->cd(1);edc1rawlayertdc06->Draw();
	c1->cd(2);edc1rawlayertdc07->Draw();
	c1->cd(3);edc1rawlayertdc08->Draw();
	c1->cd(4);edc1rawlayertdc09->Draw();
	c1->cd(5);edc1rawlayertdc10->Draw();
	c1->Print(filename);
	c1->Clear();  
	
	c1->Divide(1,2);
	c1->cd(1);edc2rawtothitzoom->Draw();
	c1->cd(2);edc2rawtdcall->Draw();
	c1->Print(filename);
	c1->Clear();  
	
	c1->Divide(2,3);
	edc2rawhitpat01->SetMinimum(0.);
	edc2rawhitpat02->SetMinimum(0.);
	edc2rawhitpat03->SetMinimum(0.);
	edc2rawhitpat04->SetMinimum(0.);
	edc2rawhitpat05->SetMinimum(0.);
	edc2rawhitpat06->SetMinimum(0.);
	c1->cd(1);edc2rawhitpat01->Draw();
	c1->cd(2);edc2rawhitpat02->Draw();
	c1->cd(3);edc2rawhitpat03->Draw();
	c1->cd(4);edc2rawhitpat04->Draw();
	c1->cd(5);edc2rawhitpat05->Draw();
	c1->cd(6);edc2rawhitpat06->Draw();
	c1->Print(filename);
	c1->Clear();  
	
	c1->Divide(2,3);
	c1->cd(1);edc2rawlayertdc01->Draw();
	c1->cd(2);edc2rawlayertdc02->Draw();
	c1->cd(3);edc2rawlayertdc03->Draw();
	c1->cd(4);edc2rawlayertdc04->Draw();
	c1->cd(5);edc2rawlayertdc05->Draw();
	c1->cd(6);edc2rawlayertdc06->Draw();
	c1->Print(filename);
	c1->Clear();  
	
}

void EHODO()
{
	c1->Divide(2,2);
	c1->cd(1);escinrawadchitpatpos1->SetMinimum(0.);
	escinrawadchitpatpos1->Draw();
	c1->cd(2);escinrawadchitpatneg1->SetMinimum(0.);
	escinrawadchitpatneg1->Draw();
	c1->cd(3);escinrawadchitpatpos2->SetMinimum(0.);
	escinrawadchitpatpos2->Draw();
	c1->cd(4);escinrawadchitpatneg2->SetMinimum(0.);
	escinrawadchitpatneg2->Draw();
	c1->Print(filename);
	c1->Clear();  
	
	c1->Divide(2,2);
	c1->cd(1);escinrawtdchitpatpos1->SetMinimum(0.);
	escinrawtdchitpatpos1->Draw();
	c1->cd(2);escinrawtdchitpatneg1->SetMinimum(0.);
	escinrawtdchitpatneg1->Draw();
	c1->cd(3);escinrawtdchitpatpos2->SetMinimum(0.);
	escinrawtdchitpatpos2->Draw();
	c1->cd(4);escinrawtdchitpatneg2->SetMinimum(0.);
	escinrawtdchitpatneg2->Draw();
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawADC(escinrawposadc1_01,escinrawnegadc1_01,1);
	DrawADC(escinrawposadc1_02,escinrawnegadc1_02,2);
	DrawADC(escinrawposadc1_03,escinrawnegadc1_03,3);
	DrawADC(escinrawposadc1_04,escinrawnegadc1_04,4);
	DrawADC(escinrawposadc1_05,escinrawnegadc1_05,5);
	DrawADC(escinrawposadc1_06,escinrawnegadc1_06,6);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawADC(escinrawposadc1_07,escinrawnegadc1_07,1);
	DrawADC(escinrawposadc1_08,escinrawnegadc1_08,2);
	DrawADC(escinrawposadc1_09,escinrawnegadc1_09,3);
	DrawADC(escinrawposadc1_10,escinrawnegadc1_10,4);
	DrawADC(escinrawposadc1_11,escinrawnegadc1_11,5);
	DrawADC(escinrawposadc1_12,escinrawnegadc1_12,6);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawADC(escinrawposadc1_13,escinrawnegadc1_13,1);
	DrawADC(escinrawposadc1_14,escinrawnegadc1_14,2);
	DrawADC(escinrawposadc1_15,escinrawnegadc1_15,3);
	DrawADC(escinrawposadc1_16,escinrawnegadc1_16,4);
	DrawADC(escinrawposadc1_17,escinrawnegadc1_17,5);
	DrawADC(escinrawposadc1_18,escinrawnegadc1_18,6);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawADC(escinrawposadc1_19,escinrawnegadc1_19,1);
	DrawADC(escinrawposadc1_20,escinrawnegadc1_20,2);
	DrawADC(escinrawposadc1_21,escinrawnegadc1_21,3);
	DrawADC(escinrawposadc1_22,escinrawnegadc1_22,4);
	DrawADC(escinrawposadc1_23,escinrawnegadc1_23,5);
	DrawADC(escinrawposadc1_24,escinrawnegadc1_24,6);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawADC(escinrawposadc1_25,escinrawnegadc1_25,1);
	DrawADC(escinrawposadc1_26,escinrawnegadc1_26,2);
	DrawADC(escinrawposadc1_27,escinrawnegadc1_27,3);
	DrawADC(escinrawposadc1_28,escinrawnegadc1_28,4);
	DrawADC(escinrawposadc1_29,escinrawnegadc1_29,5);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawADC(escinrawposadc2_01,escinrawnegadc2_01,1);
	DrawADC(escinrawposadc2_02,escinrawnegadc2_02,2);
	DrawADC(escinrawposadc2_03,escinrawnegadc2_03,3);
	DrawADC(escinrawposadc2_04,escinrawnegadc2_04,4);
	DrawADC(escinrawposadc2_05,escinrawnegadc2_05,5);
	DrawADC(escinrawposadc2_06,escinrawnegadc2_06,6);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawADC(escinrawposadc2_07,escinrawnegadc2_07,1);
	DrawADC(escinrawposadc2_08,escinrawnegadc2_08,2);
	DrawADC(escinrawposadc2_09,escinrawnegadc2_09,3);
	DrawADC(escinrawposadc2_10,escinrawnegadc2_10,4);
	DrawADC(escinrawposadc2_11,escinrawnegadc2_11,5);
	DrawADC(escinrawposadc2_12,escinrawnegadc2_12,6);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawADC(escinrawposadc2_13,escinrawnegadc2_13,1);
	DrawADC(escinrawposadc2_14,escinrawnegadc2_14,2);
	DrawADC(escinrawposadc2_15,escinrawnegadc2_15,3);
	DrawADC(escinrawposadc2_16,escinrawnegadc2_16,4);
	DrawADC(escinrawposadc2_17,escinrawnegadc2_17,5);
	DrawADC(escinrawposadc2_18,escinrawnegadc2_18,6);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawADC(escinrawposadc2_19,escinrawnegadc2_19,1);
	DrawADC(escinrawposadc2_20,escinrawnegadc2_20,2);
	DrawADC(escinrawposadc2_21,escinrawnegadc2_21,3);
	DrawADC(escinrawposadc2_22,escinrawnegadc2_22,4);
	DrawADC(escinrawposadc2_23,escinrawnegadc2_23,5);
	DrawADC(escinrawposadc2_24,escinrawnegadc2_24,6);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawADC(escinrawposadc2_25,escinrawnegadc2_25,1);
	DrawADC(escinrawposadc2_26,escinrawnegadc2_26,2);
	DrawADC(escinrawposadc2_27,escinrawnegadc2_27,3);
	DrawADC(escinrawposadc2_28,escinrawnegadc2_28,4);
	DrawADC(escinrawposadc2_29,escinrawnegadc2_29,5);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawTDC(escinrawpostdc1_01,escinrawnegtdc1_01,1);
	DrawTDC(escinrawpostdc1_02,escinrawnegtdc1_02,2);
	DrawTDC(escinrawpostdc1_03,escinrawnegtdc1_03,3);
	DrawTDC(escinrawpostdc1_04,escinrawnegtdc1_04,4);
	DrawTDC(escinrawpostdc1_05,escinrawnegtdc1_05,5);
	DrawTDC(escinrawpostdc1_06,escinrawnegtdc1_06,6);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawTDC(escinrawpostdc1_07,escinrawnegtdc1_07,1);
	DrawTDC(escinrawpostdc1_08,escinrawnegtdc1_08,2);
	DrawTDC(escinrawpostdc1_09,escinrawnegtdc1_09,3);
	DrawTDC(escinrawpostdc1_10,escinrawnegtdc1_10,4);
	DrawTDC(escinrawpostdc1_11,escinrawnegtdc1_11,5);
	DrawTDC(escinrawpostdc1_12,escinrawnegtdc1_12,6);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawTDC(escinrawpostdc1_13,escinrawnegtdc1_13,1);
	DrawTDC(escinrawpostdc1_14,escinrawnegtdc1_14,2);
	DrawTDC(escinrawpostdc1_15,escinrawnegtdc1_15,3);
	DrawTDC(escinrawpostdc1_16,escinrawnegtdc1_16,4);
	DrawTDC(escinrawpostdc1_17,escinrawnegtdc1_17,5);
	DrawTDC(escinrawpostdc1_18,escinrawnegtdc1_18,6);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawTDC(escinrawpostdc1_19,escinrawnegtdc1_19,1);
	DrawTDC(escinrawpostdc1_20,escinrawnegtdc1_20,2);
	DrawTDC(escinrawpostdc1_21,escinrawnegtdc1_21,3);
	DrawTDC(escinrawpostdc1_22,escinrawnegtdc1_22,4);
	DrawTDC(escinrawpostdc1_23,escinrawnegtdc1_23,5);
	DrawTDC(escinrawpostdc1_24,escinrawnegtdc1_24,6);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawTDC(escinrawpostdc1_25,escinrawnegtdc1_25,1);
	DrawTDC(escinrawpostdc1_26,escinrawnegtdc1_26,2);
	DrawTDC(escinrawpostdc1_27,escinrawnegtdc1_27,3);
	DrawTDC(escinrawpostdc1_28,escinrawnegtdc1_28,4);
	DrawTDC(escinrawpostdc1_29,escinrawnegtdc1_29,5);
	c1->Print(filename);
	c1->Clear();  

  
	c1->Divide(2,3);
	DrawTDC(escinrawpostdc2_01,escinrawnegtdc2_01,1);
	DrawTDC(escinrawpostdc2_02,escinrawnegtdc2_02,2);
	DrawTDC(escinrawpostdc2_03,escinrawnegtdc2_03,3);
	DrawTDC(escinrawpostdc2_04,escinrawnegtdc2_04,4);
	DrawTDC(escinrawpostdc2_05,escinrawnegtdc2_05,5);
	DrawTDC(escinrawpostdc2_06,escinrawnegtdc2_06,6);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawTDC(escinrawpostdc2_07,escinrawnegtdc2_07,1);
	DrawTDC(escinrawpostdc2_08,escinrawnegtdc2_08,2);
	DrawTDC(escinrawpostdc2_09,escinrawnegtdc2_09,3);
	DrawTDC(escinrawpostdc2_10,escinrawnegtdc2_10,4);
	DrawTDC(escinrawpostdc2_11,escinrawnegtdc2_11,5);
	DrawTDC(escinrawpostdc2_12,escinrawnegtdc2_12,6);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawTDC(escinrawpostdc2_13,escinrawnegtdc2_13,1);
	DrawTDC(escinrawpostdc2_14,escinrawnegtdc2_14,2);
	DrawTDC(escinrawpostdc2_15,escinrawnegtdc2_15,3);
	DrawTDC(escinrawpostdc2_16,escinrawnegtdc2_16,4);
	DrawTDC(escinrawpostdc2_17,escinrawnegtdc2_17,5);
	DrawTDC(escinrawpostdc2_18,escinrawnegtdc2_18,6);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawTDC(escinrawpostdc2_19,escinrawnegtdc2_19,1);
	DrawTDC(escinrawpostdc2_20,escinrawnegtdc2_20,2);
	DrawTDC(escinrawpostdc2_21,escinrawnegtdc2_21,3);
	DrawTDC(escinrawpostdc2_22,escinrawnegtdc2_22,4);
	DrawTDC(escinrawpostdc2_23,escinrawnegtdc2_23,5);
	DrawTDC(escinrawpostdc2_24,escinrawnegtdc2_24,6);
	c1->Print(filename);
	c1->Clear();  

	c1->Divide(2,3);
	DrawTDC(escinrawpostdc2_25,escinrawnegtdc2_25,1);
	DrawTDC(escinrawpostdc2_26,escinrawnegtdc2_26,2);
	DrawTDC(escinrawpostdc2_27,escinrawnegtdc2_27,3);
	DrawTDC(escinrawpostdc2_28,escinrawnegtdc2_28,4);
	DrawTDC(escinrawpostdc2_29,escinrawnegtdc2_29,5);
	c1->Print(filename);
	c1->Clear();  


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
	
	//sprintf(rFileName,"/hbook/%d.root",runnum);
	//sprintf(aFileName,"/hbook/paw%d.kumac",runnum);
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

void DrawADC(TH1F* ha, TH1F* hb, int nPad)
{
	double na = ha->GetMaximum();
	double nb = hb->GetMaximum();
	c1->cd(nPad);gPad->SetLogy();
	hb->SetLineColor(2);
	hb->SetFillStyle(0);
	if (na>nb){
		ha->Draw();
		hb->Draw("sames");
	}
	else{
		hb->Draw();
		ha->Draw("sames");
	}

}

void DrawTDC(TH1F* ha, TH1F* hb, int nPad)
{
	double na = ha->GetMaximum();
	double nb = hb->GetMaximum();
	c1->cd(nPad);
	hb->SetLineColor(2);
	hb->SetFillStyle(0);
	if (na>nb){
		ha->Draw();
		hb->Draw("sames");
	}
	else{
		hb->Draw();
		ha->Draw("sames");
	}

}

