/*
  HKSRunAction.hh

  2003/Nov/3
  H.Nomura
*/

#ifndef HKS_RUN_ACTION_H
#define HKS_RUN_ACTION_H

#include "G4UserRunAction.hh"


class G4Run;
class HKSAnalysis;

class HKSRunAction : public G4UserRunAction {
public:
  HKSRunAction(HKSAnalysis *analysisManager=0);
  virtual ~HKSRunAction();

  virtual void BeginOfRunAction(const G4Run* aRun);
  virtual void EndOfRunAction(const G4Run* aRun);

private:
  HKSAnalysis *anaMan;
};

#endif
