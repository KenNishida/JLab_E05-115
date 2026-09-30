#define coin_cxx
#include "include/coin.h"
#include "include/coinhist.h"
#include <TH1.h>
#include <TH2.h>
#include <TStyle.h>
#include <TCanvas.h>

void coinhist(){
	
	/*char filename[30];
	char runnum[30];
	printf ("Enter run number:");
	scanf("%s",&runnum);
	strcpy(filename,"coin");
	strcat(filename,runnum);
	strcat(filename,".root");
	printf("%s \n",filename);*/
	TFile *f = new TFile("coin59400.root");
	TTree *tree = (TTree*)f->Get("tree");
	coin tmp(tree);
	tmp.Loop();
	
	h->Draw();
}

void coin::Loop()
{
	if (fChain == 0) return;
   
	//fChain->Project("h","hsp");

   Long64_t nentries = fChain->GetEntriesFast();

   Long64_t nbytes = 0, nb = 0;
   for (Long64_t jentry=0; jentry<nentries;jentry++) {
      Long64_t ientry = LoadTree(jentry);
      if (ientry < 0) break;
      nb = fChain->GetEntry(jentry);   nbytes += nb;
      // if (Cut(ientry) < 0) continue;
		h->Fill(hsp);
			//cout << hsp << endl;
   }
}
