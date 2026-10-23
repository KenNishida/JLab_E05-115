#ifndef EVDISPLAY_EXT_H
#define EVDISPLAY_EXT_H

#include <TGeoManager.h>
#include <TGeoVolume.h>
#include <TGeoMatrix.h>

void evdisplay(const char* filename, int startEvent = 0, int nEvents = 100);
void SetGeometry();

// -----------------------------------------------------------------------------
// HKS downstream local coordinate used by this event display
//   z = 0 : center of HTOF1X
//   +z    : downstream
// -----------------------------------------------------------------------------
const Double_t z_DExit = -306.24;

const Double_t x_DC1 = -5.0;
const Double_t y_DC1 = -2.0;
const Double_t z_DC1 = 187.04 - 306.24;

const Double_t x_DC2 = -5.0;
const Double_t y_DC2 = -2.0;
const Double_t z_DC2 = 287.04 - 306.24;

const Double_t x_1X  = -5.0;
const Double_t y_1X  =  0.0;
const Double_t z_1X  =  0.0;

const Double_t x_AC1 = x_1X +  7.5;
const Double_t y_AC1 = 0.0;
const Double_t z_AC1 = 42.218;

const Double_t x_AC2 = x_AC1 - 7.5;
const Double_t y_AC2 = 0.0;
const Double_t z_AC2 = z_AC1 + 30.8;

const Double_t x_AC3 = x_AC2 - 7.5;
const Double_t y_AC3 = 0.0;
const Double_t z_AC3 = z_AC2 + 30.8;

const Double_t x_2X  = x_1X - 2.5;
const Double_t y_2X  = 0.0;
const Double_t z_2X  = 150.0;

const Double_t x_WC1 = x_1X + 0.275;
const Double_t y_WC1 = 0.0;
const Double_t z_WC1 = 161.184;

const Double_t x_WC2 = x_WC1 - 7.95;
const Double_t y_WC2 = 0.0;
const Double_t z_WC2 = z_WC1 + 10.897;

TGeoManager* HKSworld = nullptr;
TGeoVolume*  HKSDets  = nullptr;

#endif