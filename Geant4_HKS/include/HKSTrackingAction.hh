/*
  HKSTrackingAction.hh

  2003/Nov/3
  H.Nomura
*/

#ifndef HKSTrackingAction_h

#define HKSTrackingAction_h

#include "G4UserTrackingAction.hh"

class G4Track;

class HKSTrackingAction : public G4UserTrackingAction
{
public:
  HKSTrackingAction();
  HKSTrackingAction( const G4Track* aTrack );
  ~HKSTrackingAction();

  void PreUserTrackingAction( const G4Track* aTrack );
  void PostUserTrackingAction( const G4Track* aTrack );

};

#endif
