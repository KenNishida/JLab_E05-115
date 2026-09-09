/*
  HKSWCHit.cc

  H.Nomura 2003/Dec/31
*/

#include "HKSWCHit.hh"
#include "G4VVisManager.hh"
#include "G4Circle.hh"
#include "G4VisAttributes.hh"
#include "G4ParticleDefinition.hh"

#include <iomanip>

HKSWCHit::HKSWCHit( G4int wallID, G4int SegID, 
		    const G4ThreeVector &gPos,
		    const G4ThreeVector &gMom, 
		    const G4double time,
		    const G4String &name,		
		    const G4ParticleDefinition *particle,
		    G4double velocity )
  : trackNo_(0), lPos_(0), lMom_(0), edep_(0),
    Length_(0), npe_(0)
{
  wallID_ = wallID;
  segID_ = SegID;
  gPos_ = gPos;
  gMom_ = gMom;
  Time_ = time;
  beta_ = velocity;
  fSignal_ = false;
  part_ = particle;
  name_ = name;
}

G4Allocator<HKSWCHit> HKSWCHitAllocator;

void HKSWCHit::Draw()
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
    pVisManager-> Draw( circle );
  }
}

void HKSWCHit::Print()
{
  G4cout << "WC-" << wallID_+1 << " " << segID_ 
	 << " Time: " << Time_/ns << " [ns] ; Beta: "
	 << beta_ << G4endl;
}

