/*
  HKSTOFHit.cc

  H.Nomura
  2003/Dec/12
*/

#include "HKSTOFHit.hh"

#include "G4VVisManager.hh"
#include "G4Colour.hh"
#include "G4Circle.hh"
#include "G4VisAttributes.hh"
#include "G4ParticleDefinition.hh"

HKSTOFHit::HKSTOFHit( G4int tofNo, G4int id, const G4ThreeVector &gPos,
		      const G4ThreeVector &gMom,
		      const G4ParticleDefinition *particle,
		      const G4String &name,
		      const G4double time )
				:trackNo_(0), time_(0), eDep_(0), Length_(0), 
				 lPos_(0), lMom_(0) 
{
  tofId_ = tofNo;
  segId_ = id;
  gPos_ = gPos;
  gMom_ = gMom;
  time_ = time;
  partDef_ = particle;
  name_ = name;
}

/*HKSTOFHit::HKSTOFHit( const HKSTOFHit &right )
  : G4VHit()
{
  tofId_ = right.tofId_;
  segId_ = right.segId_;
  time_ = right.time_;
  gPos_ = right.gPos_;
  gMom_ = right.gMom_;
  partDef_ = right.partDef_;
  eDep_ = right.eDep_;
  trackNo_ = right.trackNo_;
  lPos_ = right.lPos_;
  lMom_ = right.lMom_;
}*/

HKSTOFHit::~HKSTOFHit()
{
}

G4Allocator<HKSTOFHit> HKSTOFHitAllocator;

void HKSTOFHit::Draw( void )
{
  G4VVisManager *pVisMan = G4VVisManager::GetConcreteInstance();
  if( pVisMan ){
    G4Circle circle( gPos_);
    circle.SetScreenSize( 3.0 );
    circle.SetFillStyle( G4Circle::filled );
    G4Colour colour( 0., 1., 0. );
    G4VisAttributes VisAtt( colour );
    circle.SetVisAttributes( VisAtt );
    pVisMan-> Draw( circle );
  }
}

void HKSTOFHit::Print( void )
{
  G4cout << "TOF" << "(" << segId_ << ") : "
	 << partDef_-> GetParticleName() << " ( "
	 << time_/ns << " [ns] )" << G4endl;
}
