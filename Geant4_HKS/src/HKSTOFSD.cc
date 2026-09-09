/*
  HKSTOFSD.cc

  2003/Dec/12
  H.Nomura
*/

#include "G4HCofThisEvent.hh"
#include "G4VPhysicalVolume.hh"
#include "G4Step.hh"
#include "G4Track.hh"
#include "G4VTouchable.hh"
#include "G4TouchableHistory.hh"
#include "G4TouchableHandle.hh"
#include "G4VVisManager.hh"

#include "HKSTOFSD.hh"
#include "BeamLineElement.hh"

#include <sstream>

const double TimeSeparationThreshold = 0.05*ns;

HKSTOFSD::HKSTOFSD( G4String name )
  : G4VSensitiveDetector( name ),
    thTimeSep( TimeSeparationThreshold )
{
 
 for( int i=0; i<3; i++ ){
    std::ostringstream id;
    id << i;
    G4String Name = G4String("TOF")+id.str().c_str()+"-Collection";
    collectionName.insert( Name );
    elem_[i] = 0;
  }

}

HKSTOFSD::~HKSTOFSD()
{
}

void HKSTOFSD::SetElement( int id, BeamLineElement *elem )
{
  
 if( id >= 0 && id <3 ) elem_[id] = elem;

}

void HKSTOFSD::Initialize( G4HCofThisEvent *HCE )
{
   for( int i=0; i<3; ++i ){
    TOFCollection[i] =
      new HKSTOFHitsCollection( SensitiveDetectorName,
			       collectionName[i] );
    G4int HCID = GetCollectionID(i);
    HCE->AddHitsCollection( HCID, TOFCollection[i] );
  }
  edep=0.;
}

G4bool HKSTOFSD::ProcessHits( G4Step *aStep, G4TouchableHistory* )
{

  //edep = aStep-> GetTotalEnergyDeposit();
  //if( edep == 0.0 ) return true;
  G4Track *aTrack = aStep-> GetTrack();
  const G4VTouchable *theTouchable = 
    aStep-> GetPreStepPoint()-> GetTouchable();
  G4VPhysicalVolume *TOF = theTouchable-> GetVolume();
  G4int segID = TOF->GetCopyNo();
  G4int tofID = -1;
  G4String SDname = TOF->GetName();
  if( SDname == "HTOF 1x" ) tofID = 0;
  else if( SDname == "HTOF 1y" ) tofID = 1;
  else if( SDname == "HTOF 2x" ) tofID = 2;
  G4double Charge=aStep->GetTrack()->GetDefinition()->GetPDGCharge();

  if( tofID >= 0 && tofID < 3 && elem_[tofID] && Charge!=0 ){
    G4ThreeVector gPos = aStep-> GetPreStepPoint()-> GetPosition();
    G4double hitTime = aTrack-> GetGlobalTime();
    G4int trackNo = aTrack-> GetTrackID();
    G4ThreeVector gMom = aTrack-> GetMomentum();
    G4ParticleDefinition *particle =
    aTrack-> GetDynamicParticle()-> GetDefinition();
    G4String particlename =particle-> GetParticleName();
	 G4ThreeVector lPos = elem_[tofID]-> GetLocalPosition( gPos );
    G4ThreeVector lMom = elem_[tofID]-> GetLocalDirection( gMom );
    G4int nHits = TOFCollection[tofID]-> entries();
  	 G4double tLength = aTrack-> GetTrackLength();
    
	 
	 for( G4int i=0; i<nHits; i++ ){
      HKSTOFHit *aHit = (*TOFCollection[tofID])[i];
      if( segID == aHit->GetSegID() &&
	  		tofID == aHit->GetTOFID() ){
			aHit->AddEdep( edep );
			return true;
      }
    }
    
    HKSTOFHit *aHit = new HKSTOFHit( tofID, segID, gPos, gMom,
				     particle, particlename, hitTime );
	 aHit->AddEdep( edep );
	 aHit-> SetLength( tLength );
    aHit-> SetTrackNo( trackNo );
    aHit-> SetLPos( lPos );
    aHit-> SetLMom( lMom );
    
    TOFCollection[tofID]-> insert( aHit );
   
  }
  return true;
}

void HKSTOFSD::EndOfEvent( G4HCofThisEvent* )
{
}

void HKSTOFSD::clear()
{
  for( int i=0; i<3; ++i ){
  G4int nHits = TOFCollection[i]-> entries();
  for( G4int j=0; j<nHits; j++ )
    delete (*TOFCollection[i])[j];
  }
}

void HKSTOFSD::DrawAll()
{
  G4VVisManager *pVisMan = G4VVisManager::GetConcreteInstance();

  if( pVisMan ){
     for( int i=0; i<3; ++i ){
    G4int nHits = TOFCollection[i]-> entries();
    for( G4int j=0; j<nHits; j++ ){
      (*TOFCollection[i])[j]-> Draw();
    }
     }
  }
}

void HKSTOFSD::PrintAll()
{
  for( int i=0; i<3; ++i ){
  G4int nHits = TOFCollection[i]-> entries();
  for( G4int j=0; j<nHits; j++ ){
    (*TOFCollection[i])[j]-> Print();
  }
  }
  
}
