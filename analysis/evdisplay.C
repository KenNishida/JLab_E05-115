#include <iostream>
#include <cmath>
#include <algorithm>

#include <TFile.h>
#include <TTree.h>
#include <TCanvas.h>
#include <TGeoManager.h>
#include <TGeoMaterial.h>
#include <TGeoMedium.h>
#include <TGeoVolume.h>
#include <TGeoMatrix.h>
#include <TGeoBBox.h>
#include <TGeoTube.h>
#include <TGeoCompositeShape.h>
#include <TString.h>
#include <TVector3.h>
#include <TColor.h>

#include "evdisplay.h"

using namespace std;

TGeoMedium* hksMedium = nullptr;
TGeoMedium* hksTrackMedium = nullptr;
TGeoMedium* hksContainerMedium = nullptr;
TGeoMedium* hksStructureMedium = nullptr;
TGeoMedium* hksEnvelopeMedium = nullptr;

namespace {

TGeoVolume* AddBox(const char* name, double sx, double sy, double sz,
                   double x, double y, double z, TGeoMedium* medium,
                   int color, int transparency)
{
    TGeoVolume* vol = HKSworld->MakeBox(name, medium, sx/2., sy/2., sz/2.);
    vol->SetLineColor(color);
    vol->SetFillColor(color);
    vol->SetLineWidth(1);
    vol->SetVisibility(kTRUE);
    vol->SetTransparency(transparency);
    HKSDets->AddNode(vol, 1, new TGeoTranslation(x, y, z));
    return vol;
}

TGeoVolume* AddHollowBox(const char* name,
                         double outerX, double outerY, double outerZ,
                         double innerX, double innerY, double innerZ,
                         double x, double y, double z,
                         int color, int transparency)
{
    TString outerName = Form("%s_outer_shape", name);
    TString innerName = Form("%s_inner_shape", name);
    TString shapeName = Form("%s_shape", name);

    new TGeoBBox(outerName.Data(), outerX/2., outerY/2., outerZ/2.);
    const double innerHalfZ = max(innerZ/2., outerZ/2. + 0.01);
    new TGeoBBox(innerName.Data(), innerX/2., innerY/2., innerHalfZ);

    TString expression = Form("%s-%s", outerName.Data(), innerName.Data());
    TGeoCompositeShape* shape = new TGeoCompositeShape(shapeName.Data(), expression.Data());
    TGeoVolume* vol = new TGeoVolume(name, shape, hksStructureMedium);

    vol->SetLineColor(color);
    vol->SetFillColor(color);
    vol->SetLineWidth(1);
    vol->SetVisibility(kTRUE);
    vol->SetTransparency(transparency);
    HKSDets->AddNode(vol, 1, new TGeoTranslation(x, y, z));
    return vol;
}

TGeoVolume* AddTubeAlongX(const char* name, double rmin, double rmax, double length,
                          double x, double y, double z, int color, int transparency)
{
    TGeoVolume* vol = HKSworld->MakeTube(name, hksStructureMedium, rmin, rmax, length/2.);
    vol->SetLineColor(color);
    vol->SetFillColor(color);
    vol->SetLineWidth(1);
    vol->SetVisibility(kTRUE);
    vol->SetTransparency(transparency);

    TGeoRotation* rot = new TGeoRotation();
    rot->RotateY(90.0);
    HKSDets->AddNode(vol, 1, new TGeoCombiTrans(x, y, z, rot));
    return vol;
}

void AddDipoleExit()
{
    // HKS dipole gap exit: 159 x 20 cm, center shifted +3.7 cm in x.
    AddBox("DipoleExitGap", 159.0, 20.0, 0.30,
           3.7, 0.0, z_DExit, hksStructureMedium, kGray+2, 82);
}

void AddChamberExitGeometry()
{
    // Chamber extension directly after the dipole exit.
    AddHollowBox("ChamberExtension2",
                 133.2, 21.20, 35.0, 131.2, 20.00, 35.0,
                 0.0, 0.0, z_DExit + 17.5, kOrange-3, 78);

    AddHollowBox("ChamberExtension",
                 147.4, 35.4, 140.0, 132.0, 21.2, 140.0,
                 0.0, 0.0, z_DExit + 105.0, kGray, 82);

    // Chamber extension flanges.
    AddHollowBox("CEF1",
                 147.4, 35.4, 1.2, 133.2, 24.2, 2.2,
                 0.0, 0.0, z_DExit + 33.20, kGray+1, 70);
    AddHollowBox("CEF2",
                 147.4, 35.4, 1.2, 134.0, 26.6, 2.2,
                 0.0, 0.0, z_DExit + 34.40, kGray+1, 72);

    // NMR ports and flanges.
    const double zNMR = z_DExit + 20.00;
    const double xTube = 144.45/2.0;
    const double flangeOffset = 7.375;

    AddTubeAlongX("NMR_High_Tube", 2.22/2.0, 6.05/2.0, 11.25,
                  -xTube, 0.0, zNMR, kGray+1, 65);
    AddTubeAlongX("NMR_High_Flange", 2.22/2.0, 11.40/2.0, 3.50,
                  -(xTube + flangeOffset), 0.0, zNMR, kGray+1, 65);
    AddTubeAlongX("NMR_Low_Tube", 2.22/2.0, 6.05/2.0, 11.25,
                  +xTube, 0.0, zNMR, kGray+1, 65);
    AddTubeAlongX("NMR_Low_Flange", 2.22/2.0, 11.40/2.0, 3.50,
                  +(xTube + flangeOffset), 0.0, zNMR, kGray+1, 65);
}

void AddGeant4Detectors()
{
    // DC1 / DC2.
    AddBox("DC1", 130.0, 30.5, 5.13,
           x_DC1, y_DC1, z_DC1, hksMedium, kGray+1, 60);
    AddBox("DC2", 130.0, 30.5, 5.13,
           x_DC2, y_DC2, z_DC2, hksMedium, kGray+1, 60);

    // HTOF1X: 17 bars.
    AddBox("TOF1X_Envelope", 127.5, 30.0, 2.0,
           x_1X, y_1X, z_1X, hksEnvelopeMedium, kGreen+2, 92);
    for (int i = 0; i < 17; ++i) {
        const double xLocal = 60.0 - 7.5*i;
        AddBox(Form("TOF1X_%02d", i+1), 7.5, 30.0, 2.0,
               x_1X + xLocal, y_1X, z_1X, hksMedium, kGreen+2, 60);
    }

    // AC1 / AC2 / AC3: seven visible segments at the Geant4 x positions.
    struct ACGeom { const char* name; double x, y, z; };
    const ACGeom acs[] = {
        {"AC1", x_AC1, y_AC1, z_AC1},
        {"AC2", x_AC2, y_AC2, z_AC2},
        {"AC3", x_AC3, y_AC3, z_AC3}
    };

    for (const auto& ac : acs) {
        for (int i = 0; i < 7; ++i) {
            const double xLocal = 60.6 - 20.2*i;
            AddBox(Form("%s_%02d", ac.name, i+1), 20.0, 40.2, 30.2,
                   ac.x + xLocal, ac.y, ac.z, hksMedium, kCyan+1, 70);
        }
    }

    // HTOF2X: 18 bars.
    AddBox("TOF2X_Envelope", 171.0, 35.0, 2.0,
           x_2X, y_2X, z_2X, hksEnvelopeMedium, kGreen+2, 92);
    for (int i = 0; i < 18; ++i) {
        const double xLocal = 80.75 - 9.5*i;
        AddBox(Form("TOF2X_%02d", i+1), 9.5, 35.0, 2.0,
               x_2X + xLocal, y_2X, z_2X, hksMedium, kGreen+2, 60);
    }

    // WC1 / WC2: 12 active blocks each, no outer envelope.
    for (int i = 0; i < 12; ++i) {
        const double xLocal = 85.8 - 15.6*i;
        AddBox(Form("WC1_%02d", i+1), 15.0, 35.0, 8.0,
               x_WC1 + xLocal, y_WC1, z_WC1, hksMedium, kBlue, 60);
        AddBox(Form("WC2_%02d", i+1), 15.0, 35.0, 8.0,
               x_WC2 + xLocal, y_WC2, z_WC2, hksMedium, kBlue, 60);
    }
}

} // namespace

void SetGeometry()
{
    HKSworld = new TGeoManager("HKS", "HKS Event Display");

    TGeoMaterial* detectorMat  = new TGeoMaterial("DetectorVacuum", 0, 0, 0);
    TGeoMaterial* trackMat     = new TGeoMaterial("TrackVacuum", 0, 0, 0);
    TGeoMaterial* containerMat = new TGeoMaterial("ContainerVacuum", 0, 0, 0);
    TGeoMaterial* structureMat = new TGeoMaterial("StructureVacuum", 0, 0, 0);
    TGeoMaterial* envelopeMat  = new TGeoMaterial("EnvelopeVacuum", 0, 0, 0);

    hksMedium          = new TGeoMedium("DetectorMedium", 1, detectorMat);
    hksTrackMedium     = new TGeoMedium("TrackMedium", 2, trackMat);
    hksContainerMedium = new TGeoMedium("ContainerMedium", 3, containerMat);
    hksStructureMedium = new TGeoMedium("StructureMedium", 4, structureMat);
    hksEnvelopeMedium  = new TGeoMedium("EnvelopeMedium", 5, envelopeMat);

    HKSDets = HKSworld->MakeBox("HKSDets", hksContainerMedium, 110., 60., 350.);
    HKSDets->SetVisibility(kFALSE);
    HKSDets->SetVisDaughters(kTRUE);
    HKSDets->SetTransparency(100);

    AddDipoleExit();
    AddChamberExitGeometry();
    AddGeant4Detectors();

    HKSworld->SetTopVolume(HKSDets);
}

void AddTrackGeometry(int id, double x1, double y1, double z1,
                      double x2, double y2, double z2)
{
    TVector3 p1(x1, y1, z1), p2(x2, y2, z2);
    TVector3 direction = p2 - p1;
    const double length = direction.Mag();
    if (length <= 0. || !isfinite(length)) return;

    const TVector3 center = 0.5 * (p1 + p2);
    const TVector3 u = direction.Unit();
    const TVector3 ex = u.Orthogonal().Unit();
    const TVector3 ey = u.Cross(ex).Unit();

    TGeoVolume* track = HKSworld->MakeTube(Form("Track_%d", id),
                                           hksTrackMedium, 0., 0.15, length/2.);
    track->SetLineColor(kRed);
    track->SetFillColor(kRed);
    track->SetVisibility(kTRUE);
    track->SetTransparency(0);

    Double_t matrix[9] = {
        ex.X(), ey.X(), u.X(),
        ex.Y(), ey.Y(), u.Y(),
        ex.Z(), ey.Z(), u.Z()
    };

    TGeoRotation* rotation = new TGeoRotation();
    rotation->SetMatrix(matrix);
    HKSDets->AddNode(track, id+1,
        new TGeoCombiTrans(center.X(), center.Y(), center.Z(), rotation));
}

void evdisplay(const char* filename, int startEvent, int nEvents)
{
    TString filepath = Form("../rootfiles/%s", filename);
    TFile* file = TFile::Open(filepath.Data(), "READ");
    if (!file || file->IsZombie()) {
        cerr << "Cannot open ROOT file" << endl;
        if (file) { file->Close(); delete file; }
        return;
    }

    TTree* tree = nullptr;
    file->GetObject("h9500", tree);
    if (!tree) {
        cerr << "Cannot find TTree h9500" << endl;
        file->Close();
        delete file;
        return;
    }

    Float_t hsxfp=0, hsxpfp=0, hsyfp=0, hsypfp=0;
    if (tree->SetBranchAddress("hsxfp", &hsxfp) < 0 ||
        tree->SetBranchAddress("hsxpfp", &hsxpfp) < 0 ||
        tree->SetBranchAddress("hsyfp", &hsyfp) < 0 ||
        tree->SetBranchAddress("hsypfp", &hsypfp) < 0) {
        file->Close();
        delete file;
        return;
    }

    const Long64_t total = tree->GetEntries();
    const Long64_t first = max<Long64_t>(0, startEvent);
    const Long64_t last = min<Long64_t>(total, first + max(0, nEvents));

    SetGeometry();

    // Straight focal-plane extrapolation to the HKS dipole exit for display only.
    const double zFP = (z_DC1 + z_DC2)/2.;
    const double zMin = z_DExit;
    const double zMax = max(z_DC1, z_WC2);
    int nDrawn = 0;

    for (Long64_t i = first; i < last; ++i) {
        if (tree->GetEntry(i) <= 0) continue;
        if (!isfinite(hsxfp) || !isfinite(hsxpfp) ||
            !isfinite(hsyfp) || !isfinite(hsypfp)) continue;

        const double x1 = hsxfp + hsxpfp * (zMin - zFP);
        const double y1 = hsyfp + hsypfp * (zMin - zFP);
        const double x2 = hsxfp + hsxpfp * (zMax - zFP);
        const double y2 = hsyfp + hsypfp * (zMax - zFP);
        AddTrackGeometry(nDrawn, x1, y1, zMin, x2, y2, zMax);
        ++nDrawn;
    }

    HKSworld->CloseGeometry();
    HKSworld->SetVisLevel(4);
    HKSworld->SetMaxVisNodes(100000);
    HKSworld->SetVisOption(0);

    TCanvas* c = new TCanvas("cHKS", "HKS owner canvas", 320, 220);
    c->cd();
    HKSDets->Draw("ogl");
    c->Modified();
    c->Update();

    file->Close();
    delete file;
}
