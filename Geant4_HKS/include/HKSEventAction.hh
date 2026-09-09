/*
  HKSEventAction.hh

  2003/Nov/3
  H.Nomura
*/

#ifndef HKS_EVENT_ACTION_H
#define HKS_EVENT_ACTION_H 

#include "globals.hh"
#include "G4UserEventAction.hh"

class G4Event;
class HKSAnalysis;

class HKSEventAction : public G4UserEventAction {
public:
  HKSEventAction(HKSAnalysis *analysisManager=0 );
  virtual ~HKSEventAction();

  virtual void BeginOfEventAction(const G4Event* anEvent);
  virtual void EndOfEventAction(const G4Event* anEvent);

private:
  HKSAnalysis *anaMan;
  G4bool fAlwaysDraw;

  //G4int TargetColID;
  //G4int VDColID[7];
  //  G4int HT1xColID, HT1yColID;

};

#endif
