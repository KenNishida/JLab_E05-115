/*
  Geant4 Simulation for HES Spectrometer
  main program

  D.Kawama
*/

#include "G4RunManager.hh"
#include "G4UImanager.hh"
#include "G4UIterminal.hh"
#include "G4UItcsh.hh"
#include "Randomize.hh"

#include "HESDetectorConstruction.hh"
#include "HESPhysicsList.hh"
#include "HESPrimaryGeneratorAction.hh"
#include "HESRunAction.hh"
#include "HESEventAction.hh"
#include "HESAnalysis.hh"
#include "HESTrackingAction.hh"
#include "HESSteppingAction.hh"
#include "HESParamMan.hh"

#include "G4String.hh"

#ifdef G4UI_USE_XM
#include "G4UIXm.hh"
#endif

#ifdef G4VIS_USE
//#include "HESVisManager.hh"
#include "G4VisExecutive.hh"
#endif

using namespace std;

//////////////////
// main program //
//////////////////
int main( int argc, char** argv)
{
  G4bool shellFlag=false;
  G4String macFile = "mac/gun.mac";
  const char *paramFile = "input/test.in";

  if( argc==1 ){
    shellFlag = true;
  }
  else if( argc<3 ){
    G4cout << "program [macro file] [parameter file]"
	   << G4endl;
    exit(-1);
  }
  else{
    macFile = argv[1];
    paramFile = argv[2];
  }

  // parameter manager
  HESParamMan *paramMan = new HESParamMan( paramFile );

  // run manager
  G4RunManager *runManager = new G4RunManager;

 // particle & physics processes
  HESPhysicsList *physicsList = new HESPhysicsList;
  runManager->SetUserInitialization( physicsList );

  // initialize visualization
#ifdef G4VIS_USE
  //G4VisManager *visManager = new HESVisManager;
  G4VisManager *visManager = new G4VisExecutive;
  visManager->Initialize();
#endif

  // detector setup
  HESDetectorConstruction *detector = new HESDetectorConstruction;
  runManager->SetUserInitialization( detector );
  // analyzer 
  HESAnalysis *analysisManager = new HESAnalysis();
  
  // primary generator
  HESPrimaryGeneratorAction *primaryGenerator =
    new HESPrimaryGeneratorAction( detector, analysisManager );
  // user action run & event
  HESRunAction *runAction = new HESRunAction( analysisManager );
  HESEventAction *eventAction = new HESEventAction( analysisManager );
  HESTrackingAction *trackAction = new HESTrackingAction();
  HESSteppingAction *steppingAction = new HESSteppingAction();

  runManager->SetUserAction( primaryGenerator );
  runManager->SetUserAction( runAction );
  runManager->SetUserAction( eventAction );
  runManager->SetUserAction( trackAction );
  runManager->SetUserAction( steppingAction );
  runManager->Initialize();

  G4UImanager *UI = G4UImanager::GetUIpointer();

  G4String command = "/control/execute " + macFile;
  if( shellFlag ){
    G4UItcsh    *shell = new G4UItcsh;
    G4UIsession *session = new G4UIterminal( shell );
    G4cout << "Begin Session \n"; 
    session->SessionStart();
    G4cout << "End Session" << G4endl;
    delete shell;
    delete session;
  }
  else {
    UI->ApplyCommand( command );
  }

  delete analysisManager;

#ifdef G4VIS_USE
  delete visManager;
#endif

  delete runManager;
  delete paramMan;

  return 0;
}

