void showLinearity(){
	TFile *f = new TFile("output.root");
	TCanvas *c1 = new TCanvas("c1","c1");
	c1->Divide(2,5);
	c1->cd(1);hdl1->Draw();
	c1->cd(2);hdl2->Draw();
	c1->cd(3);hdl3->Draw();
	c1->cd(4);hdl4->Draw();
	c1->cd(5);hdl5->Draw();
	c1->cd(6);hdl6->Draw();
	c1->cd(7);hdl7->Draw();
	c1->cd(8);hdl8->Draw();
	c1->cd(9);hdl9->Draw();
	c1->cd(10);hdl10->Draw();
	
	TCanvas *c2 = new TCanvas("c2","c2");
	c2->Divide(2,5);
	c2->cd(1);hdcgs1->Draw();
	c2->cd(2);hdcgs2->Draw();
	c2->cd(3);hdcgs3->Draw();
	c2->cd(4);hdcgs4->Draw();
	c2->cd(5);hdcgs5->Draw();
	c2->cd(6);hdcgs6->Draw();
	c2->cd(7);hdcgs7->Draw();
	c2->cd(8);hdcgs8->Draw();
	c2->cd(9);hdcgs9->Draw();
	c2->cd(10);hdcgs10->Draw();
}
