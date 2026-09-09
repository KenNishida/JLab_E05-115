/*
  HESEventAction.cc

  M.Kawai
*/

#include "G4RunManager.hh"
#include "globals.hh"
#include "G4Event.hh"
#include "G4VVisManager.hh"
#include "G4TrajectoryContainer.hh"
#include "G4VTrajectory.hh"
#include "G4UImanager.hh"
#include "G4ios.hh"
#include "G4HCofThisEvent.hh"
#include "G4SDManager.hh"

#include "HKSEventAction.hh"
#include "HKSAnalysis.hh"


#include <iomanip>
#include <sstream>

/////////////////////////////////////////////////////////////
HKSEventAction::HKSEventAction(HKSAnalysis *analysisManager)
  : G4UserEventAction(), anaMan(analysisManager)
/////////////////////////////////////////////////////////////
{
  //for( int i=1; i<=14; ++i ) VDColID[i] = -1;
  //TargetColID = -1;
  fAlwaysDraw = true;
}

////////////////////////////////////
HKSEventAction::~HKSEventAction()
///////////////////////////////////
{
}

////////////////////////////////////////////////////////////////
void HKSEventAction::BeginOfEventAction(const G4Event* anEvent)
///////////////////////////////////////////////////////////////
{
  /*G4SDManager* SDManager= G4SDManager::GetSDMpointer();

  if( TargetColID<0 ){
    TargetColID = SDManager->GetCollectionID(G4String("TargetCollection"));
  }
  
  for( int i=0; i<=14; ++i ){
    if( VDColID[i]<0 ){
      std::ostringstream id;
      id << i;
      G4String Name = G4String("VD")+id.str().c_str()+"-Collection";
      VDColID[i] = SDManager->GetCollectionID( Name );
    }
  }*/

  if(anaMan) anaMan->BeginOfEvent(anEvent);
  //CLHEP::HepRandom::showEngineStatus();
  //  G4cout << "EventAction::BeginOfEvent" << G4endl;  
}

///////////////////////////////////////////////////////////////
void HKSEventAction::EndOfEventAction(const G4Event* anEvent)
//////////////////////////////////////////////////////////////
{

  anaMan->EndOfEvent(anEvent);

  /*G4VVisManager *visManager = G4VVisManager::GetConcreteInstance();
  //G4bool fTriggered = anaMan->GetTriggerStatus();
  //if(visManager && fTriggered){
  if(visManager && fAlwaysDraw){
  //if(visManager && (fAlwaysDraw || fTriggered)){
    // Trajectories
    G4TrajectoryContainer *trajectoryContainer =
      anEvent->GetTrajectoryContainer();
    if(trajectoryContainer){
      G4int nTrajectories = trajectoryContainer->entries();
      // for(G4int i=0; i<nTrajectories; i++){
      //	G4VTrajectory *trj = (*trajectoryContainer)[i];
      //	trj->DrawTrajectory(50);
      //      }
      for(G4int i=0; i<nTrajectories; i++){
			G4VTrajectory *trj = 
	  			((*(anEvent->GetTrajectoryContainer()))[i]);
			visManager->Draw(*trj,50);
      }
    }

    // get "Hit Collection of This Event"
//    G4HCofThisEvent* HCTE= anEvent->GetHCofThisEvent();
//    if(HCTE){
//      G4VHitsCollection *HKSTargetHit = HCTE->GetHC( TargetColID );
//        if( HKSTargetHit ) HKSTargetHit-> DrawAllHits();
//     }
    
  }*/
}

