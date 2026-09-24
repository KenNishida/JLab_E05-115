#include <iostream>
#include <fstream>

void tofK(int runnum=10000, int aaa=0, int bbb=0){
	char str[100];
	sprintf(str,"run# = %d",runnum);
	char rFileName[100];

	Int_t nEnt=0,n=0;
	Float_t sctothits=0, scgevent=0;
	Float_t scadcp=0.,scadcn=0.;
	Float_t sctdcp=0.,sctdcn=0.;
	Float_t scla=0, scco=0;
	Float_t tof=0.,fpath=0.,beta=0.;
	Float_t tdc1=0.,tdc2=0.,tdc3=0.,tdc4=0.;
	Float_t adc1=0.,adc2=0.,adc3=0.,adc4=0.;
	Int_t co1=0.,co2=0.,co3=0.,co4=0.;
	Int_t tof1flag=0;
	Int_t tof2flag=0;
	Int_t tof3flag=0;
	Int_t tof4flag=0;
	
	Float_t pos1[17] = {
   -61.5 -5.0,
   -53.8 -5.0,
   -46.1 -5.0,
   -38.4 -5.0,
   -30.8 -5.0,
   -23.1 -5.0,
   -15.4 -5.0,
    -7.7 -5.0,
     0.0 -5.0,
     7.6 -5.0,
    15.3 -5.0,
    23.0 -5.0,
    30.7 -5.0,
    38.3 -5.0,
    46.0 -5.0,
    53.7 -5.0,
    61.4 -5.0};
  	Float_t pos3[18]   = {
	-81.3 -7.5,
   -71.8 -7.5,
   -62.2 -7.5,
   -52.6 -7.5,
   -43.1 -7.5,
   -33.5 -7.5,
   -23.9 -7.5,
   -14.4 -7.5,
    -4.8 -7.5,
     4.8 -7.5,
    14.4 -7.5,
    23.9 -7.5,
    33.5 -7.5,
    43.1 -7.5,
    52.6 -7.5,
    62.2 -7.5,
    71.8 -7.5,
    81.3 -7.5};
   
	
	sprintf(rFileName,"hscin%d.root",runnum);
	//sprintf(rFileName,"/ntuple/hscin%d.root",runnum);
	TFile *f = new TFile(rFileName);
	TTree *tree = (TTree*)f->Get("h9022");
 	tree->SetBranchAddress("gevent",&scgevent);	
 	tree->SetBranchAddress("tothits",&sctothits);	
 	tree->SetBranchAddress("la",&scla);	
 	tree->SetBranchAddress("co",&scco);
 	tree->SetBranchAddress("adcpd",&scadcp);
 	tree->SetBranchAddress("adcnd",&scadcn);
 	tree->SetBranchAddress("timep",&sctdcp);
 	tree->SetBranchAddress("timen",&sctdcn);
	nEnt=tree->GetEntries();
   
 
   TH1F *h1 = new TH1F("h1","1X meantime",100, -10, 10);
   TH1F *h2 = new TH1F("h2","2X meantime",100, -10, 10);
   TH1F *h3 = new TH1F("h3","tof",200, 0., 10.);
   TH1F *h4 = new TH1F("h4","beta",200, 0.5, 1.5);
   TH2F *h5 = new TH2F("h5","hitpat",17, 0.5, 17.5, 18, 0.5, 18.5);
   TH2F *h6 = new TH2F("h6","tof-co1x",17, 0.5, 17.5, 200, -20., 5.);
   TH2F *h7 = new TH2F("h7","tof-ADC",100, 0., 10., 200, 0., 15000.);
   TH1F *h1x[17];
   TH1F *h2x[18];
	for (int i=0;i<17;i++){
		char str1[100];
		char str2[100];
		sprintf(str1,"h1x%d",i+1);
		sprintf(str2,"1x-%d time diff",i+1);
		h1x[i] = new TH1F(str1,str2,200, -20, 0);
	}	
	for (int i=0;i<18;i++){
		char str1[100];
		char str2[100];
		sprintf(str1,"h2x%d",i+1);
		sprintf(str2,"2x-%d time diff",i+1);
		h2x[i] = new TH1F(str1,str2,100, -10, 30);
	}	
		
	while (n<nEnt){
		tree->GetEntry(n);
		tof=-10000;
		tof1flag=0;
		tof2flag=0;
		tof3flag=0;
		tof4flag=0;
		for (Int_t j=0;j<sctothits;j++){
			tree->GetEntry(n);
			scco=int(scco);
			scla=int(scla);
//			if (scla==1 && scco==aaa 
			if (scla==1 
					  && scadcp>500 && scadcn>500 
					  && abs(sctdcp-sctdcn)<5
					  ){
				adc1=(scadcp+scadcn);
				tdc1=(sctdcp+sctdcn)/2.;
				//tdc1=(sctdcp+sctdcn)/2.-60./sqrt(adc1);
				tof1flag=1;
				co1=scco;
			}
			else if (scla==2 
					  && scadcp>1000 && scadcn>1000  
					  && abs(sctdcp-sctdcn)<5
					  ){
				tdc2=(sctdcp+sctdcn)/2.;
				adc2=(scadcp+scadcn);
				tof2flag=1;
				co2=scco;
			}
//			else if (scla==3 && scco==bbb  
			else if (scla==3   
					   && scadcp>500 && scadcn>500 
					  && abs(sctdcp-sctdcn)<5
						){
				adc3=(scadcp+scadcn);
				tdc3=(sctdcp+sctdcn)/2.;
				//tdc3=(sctdcp+sctdcn)/2.-60./sqrt(adc3);
				tof3flag=1;
				co3=scco;
			}
			else if (scla==4   
					   && scadcp>1000 && scadcn>1000 
					   && sctdcp>-20  && sctdcn>-20  
					   && sctdcp<20 && sctdcp<20){
				tdc4=(sctdcp+sctdcn)/2.;
				adc4=(scadcp+scadcn);
				tof4flag=1;
				co4=scco;
			}
			n++;
		}

		if (tof1flag==1 && tof3flag==1 && abs(co1-co3)<=5){
			fpath= sqrt((pos1[co1]-pos3[co3])*(pos1[co1]-pos3[co3])+150*150)*0.01;
			tof = (tdc3-tdc1+5.)*1e-9;
			beta = fpath/tof/3e8;
			//h1->Fill(tdc1);
			//h2->Fill(tdc3);
			h3->Fill(tof*1e9);
			h4->Fill(beta);
			//h5->Fill(co1,co3);
			//h6->Fill(co3,tdc3);
			h7->Fill(tof*1e9,adc3);
			h2x[(co3-1)]->Fill(tof*1e9);
		}
		else if (tof1flag==1 && tof2flag==1){
			tof = (tdc2-tdc1)*1e-9;
			h1x[co1-1]->Fill(tof*1e9);
		}
	}
	TCanvas *c1 = new TCanvas("c1","c1");
	h3->Draw();
	//TCanvas *c2 = new TCanvas("c2","c2");
	//h4->Draw();
	//TCanvas *c3 = new TCanvas("c3","c3");
	//h5->Draw();
	TCanvas *c2 = new TCanvas("c2","c2");
	ofstream data2("offset_2x.dat");
	for (int i=0;i<18;i++){
		TF1 *fun1 = new TF1("fun1","gaus");
		char s[1];
		h2x[i]->Fit(fun1,"","");
		double mean=fun1->GetParameter(1);
		data2 << mean/0.056 << endl;
		c2->Update();
		cout << "Type <CR> to continue, q to quit ::"<<endl;
		gets(s);
		if(s[0]=='q') break;
	}
	data2.close();
	
}


