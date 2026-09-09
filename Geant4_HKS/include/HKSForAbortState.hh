#ifndef HKSForAbortState_H
#define HKSForAbortState_H 1

#include "G4VStateDependent.hh"

class HKSForAbortState : public G4VStateDependent
{
 public:
  HKSForAbortState():
  ~HKSForAbortState():

  virtual G4bool Notify(G4ApplicationState requiredState)
};

#endif
