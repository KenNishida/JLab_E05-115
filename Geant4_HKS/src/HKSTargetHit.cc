/*
  HKSTargetHit.cc

  D.Kawama
*/

#include "HKSTargetHit.hh"

#include "G4VVisManager.hh"
#include "G4Circle.hh"
#include "G4Colour.hh"
#include "G4VisAttributes.hh"

#include <iomanip>

/////////////////////////////
HKSTargetHit::HKSTargetHit()
/////////////////////////////
  : edep_(0.), time_(-10.), trackNo_(0)
{
}

////////////////////////////////////////////////////////
HKSTargetHit::HKSTargetHit( const HKSTargetHit &right )
  : G4VHit()
///////////////////////////////////////////////////////
{
  edep_ = right.edep_;
  time_ = right.time_;
  pos_ = right.pos_;
  mom_ = right.mom_;
  trackNo_ = right.trackNo_;
}


///////////////////////////////
HKSTargetHit::~HKSTargetHit()
///////////////////////////////
{
}

/////////////////////////////////////////////////
G4Allocator<HKSTargetHit> HKSTargetHitAllocator;
/////////////////////////////////////////////////

//////////////////////////
void HKSTargetHit::Draw() 
//////////////////////////
{
/*  G4VVisManager* pVisManager = 
    G4VVisManager::GetConcreteInstance();
  if( pVisManager ){
    G4Circle circle(pos_);
    circle.SetScreenSize( 5.0 ); // in pixels
    circle.SetFillStyle( G4Circle::filled );
//    G4Colour colour( 0., 0., 1. ); //blue
    G4Colour colour( 1., 0., 0. ); //blue
    G4VisAttributes attribs( colour );
    circle.SetVisAttributes( attribs );
    pVisManager-> Draw( circle );
  }*/
}

///////////////////////////
void HKSTargetHit::Print() 
///////////////////////////
{
  G4cout << "Target: " << "Edep = " << edep_/MeV << " [MeV]"
	 << ", Time = " << time_/ns << " [ns]" << G4endl;
}
