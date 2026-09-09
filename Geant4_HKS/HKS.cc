/*
  Geant4 Simulation for HKS Spectrometer
  main program

  M.Kawai
*/

#include "G4RunManager.hh"
#include "G4UImanager.hh"
#include "G4UIterminal.hh"
#include "G4UItcsh.hh"
#include "Randomize.hh"
#include "GetNumberFromKernelEntropyPool.hh"

#include "HKSDetectorConstruction.hh"
#include "HKSPhysicsList.hh"
#include "HKSPrimaryGeneratorAction.hh"
#include "HKSRunAction.hh"
#include "HKSEventAction.hh"
#include "HKSAnalysis.hh"
#include "HKSTrackingAction.hh"
#include "HKSSteppingAction.hh"
#include "HKSParamMan.hh"

#include "G4String.hh"

#ifdef G4UI_USE_XM
#include "G4UIXm.hh"
#endif

#include "G4VisExecutive.hh"

//////////////////
// main program //
//////////////////
int main( int argc, char** argv)
{
  
  //  ====== Choose the Random Engine ========= //
  //CLHEP::HepRandom::setTheSeed(time(NULL));
  int initSeed=GetIntFromKernelEntropyPool()&0x7FFFFFFF;
  CLHEP::HepRandom::setTheSeed(initSeed);
  int startSeed=CLHEP::HepRandom::getTheSeed();
  G4cout << "*** Initial Seed = " << startSeed << G4endl;
  CLHEP::HepRandom::showEngineStatus();
  //HepRandom::setTheEngine(new RanecuEngine);
  
  G4bool shellFlag=false;
  //G4String macFile = "mac/gun.mac";
  //const char *paramFile = "input/uni.in";
  G4String macFile = "gun.mac";
  const char *paramFile = "uni.in";

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
  
  G4cout<<"param"<<G4endl;
  // parameter manager
  HKSParamMan *paramMan = new HKSParamMan( paramFile );
  
  G4cout<<"run"<<G4endl;
  // run manager
  G4RunManager *runManager = new G4RunManager;

  G4cout<<"physicslist"<<G4endl;
  // particle & physics processes
  HKSPhysicsList *physicsList = new HKSPhysicsList;
  
  G4cout<<"SetUserInitialization( physicsList )"<<G4endl;
  runManager->SetUserInitialization( physicsList );


  // initialize visualization
#ifdef G4VIS_USE
  G4cout<<"vis"<<G4endl;
  G4VisManager *visManager = new G4VisExecutive;
  G4cout<<"visInit"<<G4endl;
  visManager->Initialize();
#endif

  G4cout<<"detector"<<G4endl;
  // detector setup
  HKSDetectorConstruction *detector = new HKSDetectorConstruction;
  G4cout<<"detectorInit"<<G4endl;
  runManager->SetUserInitialization( detector );
  
  G4cout<<"analyzer"<<G4endl;
  // analyzer 
  HKSAnalysis *analysisManager = new HKSAnalysis();
  
  G4cout<<"primaryGenerator"<<G4endl;
  // primary generator
  HKSPrimaryGeneratorAction *primaryGenerator =
    new HKSPrimaryGeneratorAction( detector, analysisManager );

  G4cout<<"runaction"<<G4endl;
  // user action run & event
  HKSRunAction *runAction = new HKSRunAction( analysisManager );
  
  G4cout<<"eventAction"<<G4endl;
  HKSEventAction *eventAction = new HKSEventAction( analysisManager );
  
  G4cout<<"trackAction"<<G4endl;
  HKSTrackingAction *trackAction = new HKSTrackingAction();
  
  G4cout<<"SteppingAction"<<G4endl;
  HKSSteppingAction *steppingAction = new HKSSteppingAction();
  
  G4cout<<"runMprimaryGen"<<G4endl;
  runManager->SetUserAction( primaryGenerator );
  G4cout<<"runMrunAction"<<G4endl;
  runManager->SetUserAction( runAction );
  G4cout<<"runMeventAction"<<G4endl;
  runManager->SetUserAction( eventAction );
  G4cout<<"runMtrackAction"<<G4endl;
  runManager->SetUserAction( trackAction );
  G4cout<<"runMsteppingAction"<<G4endl;
  runManager->SetUserAction( steppingAction );
  G4cout<<"runMInit"<<G4endl;
  runManager->Initialize();

  G4cout<<"UI"<<G4endl;
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
    G4cout<<"UIApplyCommand"<<G4endl;
    UI->ApplyCommand( command );

  }

  delete analysisManager;

#ifdef G4VIS_USE
  delete visManager;
#endif

  delete runManager;
  delete paramMan;

  //delete physicsList;
  //delete detector;
  //delete primaryGenerator;
  //delete runAction;
  //delete eventAction;
  //delete trackAction;
  //delete steppingAction;
  //delete UI;
  
  return 0;

}

