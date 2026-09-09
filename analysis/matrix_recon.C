#include <TFile.h>
#include <TTree.h>
#include <TSystem.h>

#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <cmath>
#include <algorithm>
#include "analysis_param.h"
using namespace std;

struct MatTerm {
    double c[4]; int e[4];
};


bool ReadMatrix(const char *name, vector<MatTerm> &mat)
{
    ifstream fin(name);
    if (!fin) {
        cerr << "Cannot open: " << name << endl;
        return false;
    }

    string line;
    while (getline(fin, line)) {
        stringstream ss(line);
        MatTerm t;
        if (ss >> t.c[0] >> t.c[1] >> t.c[2] >> t.c[3] >> t.e[0] >> t.e[1] >> t.e[2] >> t.e[3]) {
            mat.push_back(t);
        }
    }

    return true;
}

void CalcMatrix(const vector<MatTerm> &mat, double x, double xp, double y, double yp, double out[4])
{
    for (int i = 0; i < 4; i++)
        out[i] = 0.0;

    for (const auto &t : mat) {
        double v = pow(x,t.e[0])*pow(xp,t.e[1])*pow(y,t.e[2])* pow(yp,t.e[3]);

        for (int i = 0; i < 4; i++)
            out[i] += t.c[i] * v;
    }
}


void matrix_recon()
{
    //const char *input  = "../rootfiles/gogami_condensed/ch2_9-3.root";
    //const char *output = "./recon_rootfiles/ch2_9-3_recon.root";
    const char *input  = "../rootfiles/ch2_test.root";
    const char *output = "./recon_rootfiles/ch2_test_recon.root";
    const char *hksmat = "./matrices/HKSmatrices/hks_recon_coeff.dat";
    const char *hesmat = "./matrices/HESmatrices/hes_recon_coeff.dat";

    vector<MatTerm> hmat;
    vector<MatTerm> emat;

    if (!ReadMatrix(hksmat, hmat)) return;
    if (!ReadMatrix(hesmat, emat)) return;

    TFile *fin = TFile::Open(input);
    if (!fin || fin->IsZombie()) {
        cerr << "Cannot open " << input << endl;
        return;
    }

    TTree *T = (TTree *)fin->Get("h9500");
    if (!T) {
        cerr << "Cannot find h9500" << endl;
        return;
    }

    // Input branches
    Float_t hstimefp;
    Float_t hsxfp;
    Float_t hsxpfp;
    Float_t hsyfp;
    Float_t hsypfp;

    Float_t estimefp;
    Float_t esxfp;
    Float_t esxpfp;
    Float_t esyfp;
    Float_t esypfp;

    T->SetBranchAddress("hstimefp", &hstimefp);
    T->SetBranchAddress("hsxfp",    &hsxfp);
    T->SetBranchAddress("hsxpfp",   &hsxpfp);
    T->SetBranchAddress("hsyfp",    &hsyfp);
    T->SetBranchAddress("hsypfp",   &hsypfp);

    T->SetBranchAddress("estimefp", &estimefp);
    T->SetBranchAddress("esxfp",    &esxfp);
    T->SetBranchAddress("esxpfp",   &esxpfp);
    T->SetBranchAddress("esyfp",    &esyfp);
    T->SetBranchAddress("esypfp",   &esypfp);

    // Output ROOT file
    TFile *fout = new TFile(output, "RECREATE");
    TTree *Tout = T->CloneTree(0);

    // Reconstructed quantities
    Float_t hsp_recon, hsxptar_recon, hsyptar_recon, hspath_recon, htimetar_recon;
    Float_t esp_recon, esxptar_recon, esyptar_recon, espath_recon, etimetar_recon;
    Float_t ctime_recon;

    Tout->Branch("hsp_recon",      &hsp_recon,      "hsp_recon/F");
    Tout->Branch("hsxptar_recon",  &hsxptar_recon,  "hsxptar_recon/F");
    Tout->Branch("hsyptar_recon",  &hsyptar_recon,  "hsyptar_recon/F");
    Tout->Branch("hspath_recon",   &hspath_recon,   "hspath_recon/F");
    Tout->Branch("htimetar_recon", &htimetar_recon, "htimetar_recon/F");

    Tout->Branch("esp_recon",      &esp_recon,      "esp_recon/F");
    Tout->Branch("esxptar_recon",  &esxptar_recon,  "esxptar_recon/F");
    Tout->Branch("esyptar_recon",  &esyptar_recon,  "esyptar_recon/F");
    Tout->Branch("espath_recon",   &espath_recon,   "espath_recon/F");
    Tout->Branch("etimetar_recon", &etimetar_recon, "etimetar_recon/F");

    Tout->Branch("ctime_recon",    &ctime_recon,    "ctime_recon/F");

    Long64_t N = T->GetEntries();

    // Event loop
    for (Long64_t i = 0; i < N; i++) {

        T->GetEntry(i);

        // HKS
        double x  = hsxfp  / 60.0;
        double xp = hsxpfp / 0.15;
        double y  = hsyfp  / 6.0;
        double yp = hsypfp / 0.01;
        double h[4];
        CalcMatrix(hmat, x, xp, y, yp, h);

        hsp_recon     = h[0];
        hsxptar_recon = tan(h[1]);
        hsyptar_recon = tan(h[2]);
        hspath_recon  = h[3];

        double betaK = hsp_recon / sqrt(hsp_recon*hsp_recon + gKaonMass*gKaonMass);
        htimetar_recon = hstimefp - hspath_recon/(gSpeedOfLight * max(0.001, betaK));

        // HES
        x  = esxfp  / 25.0;
        xp = esxpfp / 0.1;
        y  = esyfp  / 3.0;
        yp = esypfp / 0.01;
        double e[4];
        CalcMatrix(emat, x, xp, y, yp, e);

        esp_recon     = e[0];
        esxptar_recon = tan(e[1]);
        esyptar_recon = tan(e[2]);
        espath_recon  = e[3];
        etimetar_recon = estimefp - espath_recon/gSpeedOfLight;

        // Coincidence time
        ctime_recon = htimetar_recon - etimetar_recon;

        Tout->Fill();

        if (i % 50000 == 0)
            cout << i << " / " << N << endl;
    }

    fout->cd();
    Tout->Write();

    fout->Close();
    fin->Close();
}