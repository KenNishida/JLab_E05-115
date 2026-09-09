#include <iostream>
#include <fstream>

void BrowseHists(int runnum=10000){
	char str[100];
	sprintf(str,"run# = %d",runnum);
	alias(runnum);
	TBrowser *b1 = new TBrowser();
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
	sprintf(aFileName,"../paw//paw%d.kumac",runnum);
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


