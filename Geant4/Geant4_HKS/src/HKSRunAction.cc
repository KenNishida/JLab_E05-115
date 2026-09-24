/*
  HKSRunAction.cc

  D.Kawama
*/

#include "globals.hh"
#include "G4ios.hh"
#include "G4Run.hh"
#include "G4UImanager.hh"
#include "G4VVisManager.hh"

#include "HKSRunAction.hh"
#include "HKSAnalysis.hh"

/////////////////////////////////////////////////////////
HKSRunAction::HKSRunAction(HKSAnalysis *analysisManager)
  : G4UserRunAction(), anaMan(analysisManager)
/////////////////////////////////////////////////////////
{
}

//////////////////////////////
HKSRunAction::~HKSRunAction()
//////////////////////////////
{
}

///////////////////////////////////////////////////////
void HKSRunAction::BeginOfRunAction(const G4Run* aRun)
//////////////////////////////////////////////////////
{  
  // if(G4VVisManager::GetConcreteInstance()){
  //  G4UImanager *UI = G4UImanager::GetUIpointer();
  //  UI->ApplyCommand("/vis/scene/notifyHandlers");
  // }
  anaMan->BeginOfRun(aRun);
}

/////////////////////////////////////////////////////
void HKSRunAction::EndOfRunAction(const G4Run* aRun)
/////////////////////////////////////////////////////
{
  // if(G4VVisManager::GetConcreteInstance()){
  // G4UImanager *UI = G4UImanager::GetUIpointer();
  //  UI->ApplyCommand("/vis/viewer/update");
  // }
  if(anaMan) anaMan->EndOfRun(aRun);
}

