/*
  HKSTrackingAction.cc

  D.Kawama
*/

#include "HKSTrackingAction.hh"
#include "HKSTrajectory.hh"
#include "HKSField.hh"

#include "G4Track.hh"
#include "G4TrackingManager.hh"

///////////////////////////////////////
HKSTrackingAction::HKSTrackingAction()
  : G4UserTrackingAction()
///////////////////////////////////////  
{
}

////////////////////////////////////////
HKSTrackingAction::~HKSTrackingAction()
////////////////////////////////////////
{
}

///////////////////////////////////////////////////////////////////////
void HKSTrackingAction::PreUserTrackingAction( const G4Track *aTrack )
///////////////////////////////////////////////////////////////////////
{
  G4bool stored = true;
  HKSTrajectory *trj = new HKSTrajectory( aTrack );
  fpTrackingManager-> SetTrajectory(trj );
  fpTrackingManager-> SetStoreTrajectory( stored );

}

////////////////////////////////////////////////////////////////////////
void HKSTrackingAction::PostUserTrackingAction( const G4Track *aTrack )
////////////////////////////////////////////////////////////////////////
{
  HKSTrajectory *trj = dynamic_cast<HKSTrajectory*>
    ( fpTrackingManager-> GimmeTrajectory() );
  if( trj ){
    trj-> SetPositionAtLast( aTrack-> GetPosition() );
    trj-> SetMomentumAtLast( aTrack-> GetMomentum() );
  }

}
