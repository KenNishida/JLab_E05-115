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

    h->SetMinimum(0);
    
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
    h->SetLineWidth(1);
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

inline void SetPlotGraph(
    TGraph* g,
    TString title = "",
    TString xtitle = "",
    TString ytitle = "",
    int gColor = kBlue
)
{
    g->SetStats(0);

    if (title != "")  g->SetTitle(title);
    if (xtitle != "") g->GetXaxis()->SetTitle(xtitle);
    if (ytitle != "") g->GetYaxis()->SetTitle(ytitle);

    g->GetXaxis()->SetMaxDigits(3);
    g->GetYaxis()->SetMaxDigits(3);

    g->GetXaxis()->SetNdivisions(510);
    g->GetYaxis()->SetNdivisions(510);

    g->GetXaxis()->CenterTitle(true);
    g->GetYaxis()->CenterTitle(true);

    g->GetXaxis()->SetTitleSize(0.05);
    g->GetYaxis()->SetTitleSize(0.045);

    g->GetXaxis()->SetLabelSize(0.045);
    g->GetYaxis()->SetLabelSize(0.040);

    g->GetXaxis()->SetTitleOffset(0.9);
    g->GetYaxis()->SetTitleOffset(1.10);

    g->SetMarkerColorAlpha(gColor, 0.15);
    g->SetMarkerStyle(1);
    g->SetMarkerSize(0.15);
}
#endif