void comp(){
	TFile *f1 = new TFile("56088m.root");
	TFile *f2 = new TFile("56088s.root");

	TH1F *hm1 = (TH1F*)f1->Get("h215");
	TH1F *hs1 = (TH1F*)f2->Get("h215");
	

	hs1->SetLineColor(kRed);
	hs1->Draw();
	hm1->Draw("same");

}

/*void comp(){
	TFile *f1 = new TFile("coin60000m.root");
	TFile *f2 = new TFile("coin60000s.root");

	TTree *tm = (TTree*)f1->Get("h9500");
	TTree *ts = (TTree*)f2->Get("h9500");
	


}*/
