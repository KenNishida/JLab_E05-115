/*
  HKSSteppingAction.cc

  D.Kawama
*/

#include "HKSSteppingAction.hh"
#include "HKSGlobalSize.hh"
#include "HKSField.hh"

#include "G4Step.hh"
#include "G4FieldManager.hh"
#include "G4TransportationManager.hh"
#include "G4VTouchable.hh"
#include "G4VPhysicalVolume.hh"


////////////////////////////////////////
HKSSteppingAction::HKSSteppingAction()
  : G4UserSteppingAction(), field_(0)
    ///////////////////////////////////////
{
}

///////////////////////////////////////
HKSSteppingAction::~HKSSteppingAction()
///////////////////////////////////////
{
}

//////////////////////////////////////////////////////////////////
void HKSSteppingAction::UserSteppingAction( const G4Step *aStep )
/////////////////////////////////////////////////////////////////
{
  G4ThreeVector pos = aStep->GetPostStepPoint()->GetPosition();
  const G4VTouchable *theTouchable =
    aStep->GetPreStepPoint()->GetTouchable();
  G4VPhysicalVolume *Vol = theTouchable->GetVolume(); 
  G4String volName = Vol->GetName(); 
  G4Track *track = aStep->GetTrack();
  G4String pname= track->GetDefinition()->GetParticleName();
  G4double kin=track->GetKineticEnergy();
  //G4TrackStatus status = track->GetTrackStatus();
  //G4cout << " "<<pname <<" " << kin/MeV <<" "<< pos << G4endl;
  /*if (fabs(pos.x()) > WorldSizeX ||
    fabs(pos.y()) > WorldSizeY ||
    fabs(pos.z()) > WorldSizeZ )
    {
    track->SetTrackStatus(fKillTrackAndSecondaries);
    }*/
  
  if ( 
      volName == "Splitter"
      || volName == "Colbox"
      || volName == "Q1"
      || volName == "Q2"
      || volName == "D"
      ||	((pname=="e+" || pname=="e-") && kin<100.*MeV)
      || kin<1.*MeV
      || pos.x()>100.*cm
      || fabs(pos.y()) > WorldSizeY 
      || pos.z()<-50.*cm 
      || pos.z() > WorldSizeZ 
       )
    {
      //track->SetTrackStatus(fStopAndKill);
      track->SetTrackStatus(fKillTrackAndSecondaries);
    }
  
  
#ifdef DEBUG
  //   if( !field_ ){
  //     G4FieldManager *fieldMan =
  //       G4TransportationManager::GetTransportationManager()->GetFieldManager();
  //     field_ = dynamic_cast<const HKSField *>(fieldMan->GetDetectorField());
  //   }
  if( field_ ) field_->PushInQueue( 1, pos );
#endif
}
