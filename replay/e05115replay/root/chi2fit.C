
void chi2fit(char* filename){
	TCanvas *c1 = new TCanvas("c1","c1");
	TFile *file = new TFile(filename);
	TF1 *f1 = new TF1("f1","[0]*x*x*exp(-x/[1]/2)",0,10);
   f1->SetParameters(1e3,1.);	
	h443->Fit("f1");

	double chi20=300.;
	double alpha = f1->GetParameter(1);
	double chi2 = 300./sqrt(alpha);
	cout << "plane resolution is "<<chi2 <<"(um)"<< endl;
}
