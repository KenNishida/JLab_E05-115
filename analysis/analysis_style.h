#ifndef ANALYSIS_STYLE_H
#define ANALYSIS_STYLE_H

#include "TH1.h"
#include "TH2.h"
#include "TString.h"
#include "TColor.h"

inline void SetHist1D(
    TH1D* h,
    TString xtitle = "",
    TString ytitle = "",
    int hColor = kBlue
)
{
    h->SetStats(0);

    if (xtitle != "") h->GetXaxis()->SetTitle(xtitle);
    if (ytitle != "") h->GetYaxis()->SetTitle(ytitle);

    h->GetXaxis()->SetMaxDigits(3);
    h->GetYaxis()->SetMaxDigits(3);

    h->GetXaxis()->SetNdivisions(510);
    h->GetYaxis()->SetNdivisions(510);

    h->GetXaxis()->CenterTitle(true);
    h->GetYaxis()->CenterTitle(true);

    h->GetXaxis()->SetTitleSize(0.05);
    h->GetYaxis()->SetTitleSize(0.045);

    h->GetXaxis()->SetLabelSize(0.045);
    h->GetYaxis()->SetLabelSize(0.040);

    h->GetXaxis()->SetTitleOffset(0.9);
    h->GetYaxis()->SetTitleOffset(1.10);

    h->SetLineColor(hColor);
    h->SetFillColor(hColor);
    h->SetFillStyle(3003);
}

inline void SetHist2D(
    TH2* h,
    TString xtitle = "",
    TString ytitle = "",
    TString ztitle = ""
)
{
    h->SetStats(0);

    h->GetXaxis()->SetTitle(xtitle);
    h->GetYaxis()->SetTitle(ytitle);

    h->GetXaxis()->CenterTitle(true);
    h->GetYaxis()->CenterTitle(true);

    h->GetXaxis()->SetTitleSize(0.05);
    h->GetYaxis()->SetTitleSize(0.05);

    h->GetXaxis()->SetLabelSize(0.050);
    h->GetYaxis()->SetLabelSize(0.040);

    h->GetXaxis()->SetTitleOffset(0.9);
    h->GetYaxis()->SetTitleOffset(0.95);
}
#endif