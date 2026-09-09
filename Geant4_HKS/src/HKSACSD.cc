/*
  HKSACSD.cc

  H.Nomura 2003/Dec/31
*/

#include "G4HCofThisEvent.hh"
#include "G4VPhysicalVolume.hh"
#include "G4Step.hh"
#include "G4Track.hh"
#include "G4VTouchable.hh"
#include "G4TouchableHistory.hh"
#include "G4TouchableHandle.hh"
#include "G4VVisManager.hh"

#include "HKSACSD.hh"
#include "BeamLineElement.hh"

#include <sstream>
const G4double Win = 1.05;
const G4double ThresholdVelocity = 1./Win;
const G4double lmin = 300./1000000000.; // wave length at minimum
const G4double lmax = 600./1000000000.; // wave length at maximum
const G4double CE = 0.15; // light collection efficiency of a box  
const G4double QE = 0.2; // quantum efficiency of a PMT

HKSACSD::HKSACSD( G4String name )
  : G4VSensitiveDetector(name),
    thBeta( ThresholdVelocity )
{
  for( int i=0; i<3; i++ ){
    std::ostringstream id;
    id << i;
    G4String Name = G4String("AC")+id.str().c_str()+"-Collection";
    collectionName.insert( Name );
  
    //G4String Name = "ACCollection";
    //collectionName.insert( Name );
  elem_[i] = 0;
  }
}

HKSACSD::~HKSACSD()
{
}

void HKSACSD::SetElement( int id, BeamLineElement *elem )
{
  if( id >= 0 && id <=2 ) elem_[id] = elem;
}


void HKSACSD::Initialize( G4HCofThisEvent *HCE )
{
  for( int i=0; i<3; ++i ){
    ACCollection[i] =
      new HKSACHitsCollection( SensitiveDetectorName,
			       collectionName[i] );
    G4int HCID = GetCollectionID(i);
    HCE->AddHitsCollection( HCID, ACCollection[i] );
  }
}

G4bool HKSACSD::ProcessHits( G4Step *aStep, G4TouchableHistory* )
{
  G4Track *aTrack = aStep->GetTrack();
  //G4double edep = aStep->GetTotalEnergyDeposit();
  //  if( edep == 0.0 ) return true;

  const G4VTouchable *theTouchable =
    aStep->GetPreStepPoint()->GetTouchable();
  G4VPhysicalVolume *AC = theTouchable->GetVolume();
  G4int SegID = AC->GetCopyNo();
//   G4VPhysicalVolume *Wall = AC->GetMother();
  G4String SDname = AC->GetName();
  G4int acID = -1;
  if( SDname == "AC1" ) acID = 0;
  else if( SDname == "AC2" ) acID = 1;
  else if( SDname == "AC3" ) acID = 2;
  G4double Charge=aStep->GetTrack()->GetDefinition()->GetPDGCharge();

  G4double npe = 0.0;
  G4double StepLength = 0.;
  G4double velocity = aTrack->GetVelocity()/c_light;
  if( acID>=0 && acID<3 && elem_[acID]  && Charge!=0){
    G4ThreeVector hitpos = aStep->GetPreStepPoint()->GetPosition();
    G4ThreeVector hitmom = aTrack->GetMomentum();
    G4double hittime = aTrack->GetGlobalTime();
    G4int trackNo = aTrack->GetTrackID();
    G4ParticleDefinition *particle =
      aTrack->GetDynamicParticle()->GetDefinition();
    G4String particlename =particle-> GetParticleName();

    G4ThreeVector lPos = elem_[acID]->GetLocalPosition( hitpos );
    G4ThreeVector lMom = elem_[acID]->GetLocalDirection( hitmom );

    G4int nHits = ACCollection[acID]->entries();
    StepLength = aStep->GetStepLength();
    edep = aStep-> GetTotalEnergyDeposit();
    if( velocity >= thBeta ) {
      npe = 2.*pi/137.*(1.-1./(Win*Win*velocity*velocity))*
	(StepLength/m)*(1./lmin-1./lmax)*CE*QE;
    }
    
    HKSACHit *aHit;
    for( G4int i=0; i<nHits; i++ ){   // if there is entry, scan and evaluate  whether same counter or not//
      aHit = (*ACCollection[acID])[i]; 
      if( SegID == aHit->GetSegID() && 
	  acID == aHit->GetACID() ){  
	// if same counter , add current information //
	aHit-> AddEdep( edep );
	aHit-> AddLength( StepLength );
	aHit-> AddNpe( npe );
	return true; 
      }
    }
    // if there is no hit before, or there is no same counter hit, insert current hit in Collectopn//
    aHit = new HKSACHit( acID, SegID, hitpos, hitmom,
			 hittime, particlename, particle, velocity );
    aHit-> SetTrackNo( trackNo );
    aHit-> SetLPos( lPos );
    aHit-> SetLMom( lMom );
    aHit-> SetEdep( edep );
    aHit-> SetLength( StepLength );
    aHit-> SetNpe( npe );
    ACCollection[acID]-> insert( aHit );
    return true; 
  }
  return true;
}

void HKSACSD::EndOfEvent( G4HCofThisEvent* )
{
}

void HKSACSD::clear()
{
  for( int i=0; i<3; ++i ){
    G4int nHits = ACCollection[i]->entries();
    for( G4int j=0; j<nHits; ++j ){
      delete (*ACCollection[i])[j];
    }
  }
}

void HKSACSD::DrawAll()
{
  G4VVisManager *pVisMan = G4VVisManager::GetConcreteInstance();

  if( pVisMan ){
    for( int j=0; j<3; ++j ){
      G4int nHits = ACCollection[j]->entries();
      for( G4int i=0; i<nHits; i++ ){
	(*ACCollection[j])[i]->Draw();
      }
    }
  }
}

void HKSACSD::PrintAll()
{
  for( int j=0; j<3; ++j ){
    G4int nHits = ACCollection[j]->entries();
    for( G4int i=0; i<nHits; i++ ){
      (*ACCollection[j])[i]->Print();
    }
  }
}
