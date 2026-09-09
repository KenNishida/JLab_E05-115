/*
  HKSVisManager.hh

  2003/Nov/3
  H.Nomura
*/

#ifndef HKS_VIS_MANAGER_H
#define HKS_VIS_MANAGER_H

#include "G4VisManager.hh"

class HKSVisManager : public G4VisManager {
public:
  HKSVisManager();
  virtual ~HKSVisManager();

private:
  virtual void RegisterGraphicsSystems();

};

#endif
