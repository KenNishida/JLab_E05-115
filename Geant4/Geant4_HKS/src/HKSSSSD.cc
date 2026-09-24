/*
  HKSSSSD.cc
  definition Sensitive Detector for SieveSlit
  2004/Sep/21 H.Nomura
*/

#include "G4HCofThisEvent.hh"
#include "G4VPhysicalVolume.hh"
#include "G4Step.hh"
#include "G4Track.hh"
#include "G4VTouchable.hh"
#include "G4VVisManager.hh"

#include "HKSSSSD.hh"
#include "BeamLineElement.hh"

#include <sstream>

HKSSSSD::HKSSSSD( G4String name )
  : G4VSensitiveDetector(name)
{
  collectionName.insert(G4String("SSCollection"));
}

HKSSSSD::~HKSSSSD()
{
}

void HKSSSSD::Initialize( G4HCofThisEvent *HCE)
{
  SSCollection = 
    new HKSSSHitsCollection( SensitiveDetectorName,
			     collectionName[0] );
  G4int HCID = GetCollectionID(0);
  HCE->AddHitsCollection( HCID, SSCollection );
}

G4bool HKSSSSD::ProcessHits( G4Step *aStep, G4TouchableHistory* )
{
  G4Track *aTrack = aStep->GetTrack();
  const G4VTouchable *theTouchable = 
    aStep->GetPreStepPoint()->GetTouchable();
  G4VPhysicalVolume *Hole = theTouchable->GetVolume();
  if( elem_ ){
    G4int ID = Hole->GetCopyNo();
    G4int FileID = ID%10;
    G4int RowID = G4int( ID/10 );

    G4ThreeVector gPos = aStep->GetPreStepPoint()->GetPosition();
    G4ThreeVector gMom = aTrack->GetMomentum();

    G4ThreeVector lPos = elem_->GetLocalPosition( gPos );
    G4ThreeVector lMom = elem_->GetLocalDirection( gMom );

    HKSSSHit *aHit = new HKSSSHit( ID, lPos, lMom );
    aHit->SetFile( FileID );
    aHit->SetRow( RowID );

    SSCollection->insert( aHit );
    return true;
  }

  return true;
}

void HKSSSSD::EndOfEvent( G4HCofThisEvent* )
{
}

void HKSSSSD::clear()
{
  G4int nHits = SSCollection->entries();
  for( G4int j=0; j<nHits; j++ ){
    delete (*SSCollection)[j];
  }
}

void HKSSSSD::DrawAll()
{
  G4VVisManager *pVisMan = G4VVisManager::GetConcreteInstance();

  if( pVisMan ){
    G4int nHits = SSCollection->entries();
    for( G4int j=0; j<nHits; j++ ){
      (*SSCollection)[j]->Draw();
    }
  }
}

void HKSSSSD::PrintAll()
{
  G4int nHits = SSCollection->entries();
  for( G4int j=0; j<nHits; j++ ){
    (*SSCollection)[j]->Print();
  }
}
