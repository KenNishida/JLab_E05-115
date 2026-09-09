/*
  HKSTransportation.hh

  2003/Nov/3
  H.Nomura
*/

#ifndef HKSTransportation_h

#define HKSTransportation_h 1

#include "G4Transportation.hh"

class HKSTransportation : public G4Transportation
{
public:
  HKSTransportation()
    : G4Transportation() {}
  ~HKSTransportation() {}

  G4double AlongStepGetPhysicalInteractionLength( const G4Track & track, 
						  G4double previousStepSize,
						  G4double currentMinimumStep, 
						  G4double & currentSafety,
						  G4GPILSelection* selection );

};

#endif
