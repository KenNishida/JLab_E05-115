/*
  HKSACHit.cc

  H.Nomura 2003/Dec/31
*/

#include "HKSACHit.hh"

#include "G4VVisManager.hh"
#include "G4Circle.hh"
#include "G4Colour.hh"
#include "G4VisAttributes.hh"
#include "G4ParticleDefinition.hh"

#include <iomanip>

HKSACHit::HKSACHit( G4int wallID, G4int SegID, 
		    const G4ThreeVector &gPos,
		    const G4ThreeVector &gMom, 
		    const G4double time,
		    const G4String &name,
		    const G4ParticleDefinition *particle,
		    const G4double velocity )
  : trackNo_(0), lPos_(0), lMom_(0), edep_(0),
    fSignal_(false), Length_(0), npe_(0)
{
  wallID_ = wallID;
  segID_ = SegID;
  gPos_ = gPos;
  gMom_ = gMom;
  Time_ = time;
  part_ = particle;
  beta_ = velocity;
  name_ = name;
}

// HKSACHit::HKSACHit()
// {
// }

G4Allocator<HKSACHit> HKSACHitAllocator;

void HKSACHit::Draw()
{
  G4VVisManager* pVisManager = 
    G4VVisManager::GetConcreteInstance();
  if( pVisManager ){
    G4Circle circle(gPos_);
    circle.SetScreenSize( 3.0 ); // in pixels
    circle.SetFillStyle( G4Circle::filled );
    G4Colour colour( 0., 0., 1. ); //blue
    G4VisAttributes attribs( colour );
    circle.SetVisAttributes( attribs );
    pVisManager->Draw( circle );
  }
}

void HKSACHit::Print()
{
  G4cout << "AC-" << wallID_+1 << "(" << segID_ 
	 << ") Time: " << Time_/ns << " [ns] ; Beta: "
	 << beta_ << G4endl;
}

