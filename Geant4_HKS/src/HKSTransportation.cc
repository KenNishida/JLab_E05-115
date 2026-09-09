/*
  HKSTransportation.cc

  D.Kawama
*/

#include "HKSTransportation.hh"

////////////////////////////////////////////////////////////////////
G4double HKSTransportation::
AlongStepGetPhysicalInteractionLength( const G4Track & track, 
				       G4double previousStepSize,
				       G4double currentMinimumStep, 
				       G4double & currentSafety,
				       G4GPILSelection *selection )
////////////////////////////////////////////////////////////////////		  
{
  if( this-> DoesGlobalFieldExist() &&
      currentMinimumStep>2.*cm){
    currentMinimumStep = 2.*cm;
  }

  return G4Transportation::
    AlongStepGetPhysicalInteractionLength( track, previousStepSize, 
					   currentMinimumStep,
					   currentSafety, selection );
  
}
