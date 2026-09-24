/*
  Hkssshit.cc

  H.Nomura 2004/Sep/22
*/

#include "G4VVisManager.hh"
#include "G4Circle.hh"
#include "G4Colour.hh"
#include "G4VisAttributes.hh"
#include "G4ios.hh"

#include "HKSSSHit.hh"

#include <iomanip>

HKSSSHit::HKSSSHit( G4int holeID,
		    const G4ThreeVector &lPos,
		    const G4ThreeVector &lMom )
{
  Hole_ = holeID;
  lPos_ = lPos;
  lMom_ = lMom;
}

HKSSSHit::~HKSSSHit()
{
}

G4Allocator<HKSSSHit> HKSSSHitAllocator;

void HKSSSHit::Draw()
{
}

void HKSSSHit::Print()
{
}
