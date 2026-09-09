/*
  RootHelper.hh
*/

#ifndef RootHelper_h
#define RootHelper_h 1

#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TTree.h"
#include "TString.h"


///////////////////////////////////////////////////////
inline void HBTree(const char *name, const char *title)
//////////////////////////////////////////////////////
{
  new TTree( name, title);
}


#endif
