#ifndef ANALYSIS_PARAM_H
#define ANALYSIS_PARAM_H

Double_t gKaonMass = 0.493667;
Double_t gPionMass = 0.139570;
Double_t gProtonMass = 0.938272;
Double_t gElectronMass = 0.000511;
Double_t gLambdaMass = 1.115683;
Double_t gSigmaMass = 1.192642;

Double_t gHe6Mass = 5.60554;
Double_t gLi7Mass = 6.53383;
Double_t gBe9Mass = 8.39275;
Double_t gB10Mass = 9.32444;
Double_t gB11Mass = 10.25255;
Double_t gC12Mass = 11.17486;
Double_t gV51Mass = 47.44224;
Double_t gCr52Mass = 48.37001;

Double_t gSpeedOfLight = 29.9792458; // cm/ns

// ===== Target =====
// 1: Hydrogen in Polyethylene (CH2) for Lambda and Sigma0 production
// 2: Carbon in Polyethylene (CH2) for Lambda and Sigma0 production
// 7: Lithium-7
// 10: Boron-10
// 12: Thin Carbon-12
// 52: Chromium-52
// 101: Hydrogen in Water for Lambda and Sigma0 production.
// 112: Thick Carbon-12.
Int_t gTargetFlag = 1;

// ===== Condensed ROOT files =====
TString gCH2File   = "ch2_9-3.root";
TString gLi7File   = "li7_9-3.root";
TString gB10File   = "b10_9-3.root";
TString gC12File   = "c12_20uA_9-3.root";
TString gCr52File  = "cr52_9-3.root";
TString gH2OFile   = "h2o_9-3.root";

// ===== Analysis Parameters =====
// WC normalization  for ch2_9-3.root
Double_t gWC1_norm[23] = {19.92, 21.00, 23.43, 18.70, 19.53, 18.35, 19.97, 19.41, 20.40, 30.96, 33.28, 57.44, 57.72, 66.72, 67.20, 53.03, 52.48, 64.62, 63.77, 66.88, 66.15, 93.77, 99.00}; 
Double_t gWC2_norm[23] = {20.94, 19.25, 21.36, 17.77, 18.56, 19.71, 22.97, 21.97, 23.17, 22.24, 22.66, 22.58, 70.62, 69.30, 68.04, 69.89, 70.91, 76.14, 77.84, 64.49, 63.50, 58.19, 58.78};
// Normalization factor for WC1. Obtained from calibration using CH2 ntuple file (ch2_test.root)
//Double_t gWC1_norm[23] = {-41.91, 37.90, 36.15, 32.90, 31.60, 29.64, 31.02, 31.76, 37.05, 37.46, 39.17, 40.97, 62.84, 62.80, 65.34, 65.16, 68.85, 66.24, 64.29, 63.97, 69.47, 69.14, 66.03};
//Double_t gWC2_norm[23] = {21.74, -12.43, 32.69, 32.57, 35.08, 33.13, 34.05, 34.17, 35.66, 36.27, 36.75, 39.47, 57.07, 58.04, 62.85, 61.43, 53.37, 58.46, 58.10, 62.10, 70.27, 66.43, 54.52};

#endif // ANALYSIS_PARAM_H