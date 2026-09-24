#include <fstream.h>
#include "parameter.hh"


void mklambda()
{
  Int_t event=0;
  Double_t xt=0., xpe=0., yt=0., ype=0.,pe=0., zt=0.;
  Double_t xpk=0.,ypk=0., pk=0.,mm=0.;
  //Double_t Ei = 2.467;
  Double_t Ei = 2.344;
  Double_t mtar = mp;
  ifstream seedfile("lambda.seed");
  TFile *f = new TFile("lambda.root","recreate");
  TTree *tree = new TTree("tree_lambda","tree sample");
  tree->Branch("event",&event,"event/I");
  tree->Branch("xt",&xt,"xt/D");
  tree->Branch("xpe",&xpe,"xpe/D");
  tree->Branch("yt",&yt,"yt/D");
  tree->Branch("ype",&ype,"ype/D");
  tree->Branch("pe",&pe,"pe/D");
  tree->Branch("zt",&zt,"zt/D");
  tree->Branch("xpk",&xpk,"xpk/D");
  tree->Branch("ypk",&ypk,"ypk/D");
  tree->Branch("pk",&pk,"pk/D");
  tree->Branch("mm",&mm,"mm/D");
  Int_t N=0;

  while(!seedfile.eof()){
    seedfile >> event >> xt >> xpe >> yt >> ype >> pe >> zt
             >> xpk >> ypk >> pk ;
    xpk=xpk*1e-3;
    ypk=ypk*1e-3;
    xpe=xpe*1e-3;
    ype=ype*1e-3;
    Double_t Ek = sqrt(pk*pk+mk*mk);
    Double_t pkz = pk/sqrt(1+xpk*xpk+ypk*ypk);
    Double_t pkx = pkz*xpk;
    Double_t pky = pkz*ypk;
    Double_t Ee = sqrt(pe*pe+me*me);
    Double_t pez = pe/sqrt(1+xpe*xpe+ype*ype);
    Double_t pex = pez*xpe;
    Double_t pey = pez*ype;
    Double_t pi = sqrt(Ei**2-me**2);
    Double_t dE = (Ei+mtar-Ek-Ee);
    Double_t dE2 = dE*dE;
    Double_t dpx = (-pex-pkx);
    Double_t dpy = (-pey-pky);
    Double_t dpz = (pi-pez-pkz);
    Double_t dp2 = dpx*dpx+dpy*dpy+dpz*dpz;
    Double_t mm = sqrt(dE2-dp2);
    //cout << pk << endl;
    tree->Fill();
  }
  seedfile.close();
  TH1F *h1 = new TH1F("h1", "missing mass",100, 1114, 1116);
  tree->Project("h1","mm*1e3");
  h1->Fit("gaus");

  f->Write();
  f->Close();
}
