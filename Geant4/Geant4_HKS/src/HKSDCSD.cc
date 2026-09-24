/*
  HKSDCSD.cc
  definition hit collection of Drift Chamber 

  H.Nomura 2004/Jan/6
*/

#include "G4HCofThisEvent.hh"
#include "G4VPhysicalVolume.hh"
#include "G4Step.hh"
#include "G4Track.hh"
#include "G4VTouchable.hh"
#include "G4TouchableHistory.hh"
#include "G4TouchableHandle.hh"
#include "G4VVisManager.hh"

#include "HKSDCSD.hh"
#include "BeamLineElement.hh"

#include <sstream>

const double EnergyDepositThreshold = 0.1*keV;

HKSDCSD::HKSDCSD( G4String name )
  : G4VSensitiveDetector( name ), 
    thEdep( EnergyDepositThreshold )
{
  for( int i=0; i<2; i++ ){
    std::ostringstream id;
    id << i;
    G4String Name = G4String("DC")+id.str().c_str()+"-Collection";
    collectionName.insert( Name );
    elem_[i] = 0;
  }
}


HKSDCSD::~HKSDCSD()
{
}
void HKSDCSD::SetElement( int id, BeamLineElement *elem )
{
  if( id >= 0 && id <2 ) elem_[id] = elem;
}


void HKSDCSD::Initialize( G4HCofThisEvent *HCE )
{
  for( int i=0; i<2; i++ ){
    DCCollection[i] = 
      new HKSDCHitsCollection( SensitiveDetectorName,
			       collectionName[0] );
  G4int HCID = GetCollectionID(i);
  HCE->AddHitsCollection( HCID, DCCollection[i] );
}
}
G4bool HKSDCSD::ProcessHits( G4Step *aStep, G4TouchableHistory* )
{
  G4double edep = aStep->GetTotalEnergyDeposit();

  G4Track *aTrack = aStep->GetTrack();
  const G4VTouchable *theTouchable = 
    aStep->GetPreStepPoint()->GetTouchable();
  G4VPhysicalVolume *DClayer = theTouchable->GetVolume();
  G4String hitName = DClayer->GetName();
  G4int DCno = -1;
  if      ( hitName=="KDC1") DCno = 0;
  else if ( hitName=="KDC2") DCno = 1;
  G4double Charge=aStep->GetTrack()->GetDefinition()->GetPDGCharge();

  if( DCno>=0 && DCno<2 && elem_[DCno] && Charge!=0){
    G4int hitLayer = DClayer->GetCopyNo();
    G4ThreeVector hitPos = aStep->GetPreStepPoint()->GetPosition();
    G4ThreeVector hitMom =  aTrack->GetMomentum();
    G4int trackNo = aTrack->GetTrackID();
    G4double hitTime = aTrack->GetGlobalTime();

    G4ThreeVector lPos = elem_[DCno]->GetLocalPosition( hitPos );
    G4ThreeVector lMom = elem_[DCno]->GetLocalDirection( hitMom );

    G4double theta = 30.*degree;
    G4double X = lPos.x();
    G4double U = lPos.x()*cos(theta)+lPos.y()*sin(theta);
    G4double V = lPos.x()*cos(-theta)+lPos.y()*sin(-theta);

    G4int nHits = DCCollection[DCno]->entries();
    for( G4int i=0; i<nHits; i++ ){
      HKSDCHit *aHit = (*DCCollection[DCno])[i];
      if( DCno == aHit->GetDCID() &&
	  hitLayer == aHit->GetLayerID() ){
	aHit->AddEdep( edep );
	return true;
      }
    }

    HKSDCHit *aHit = 
      new HKSDCHit( DCno, hitLayer, hitPos, hitMom, trackNo, hitTime );
    aHit->SetEdep( edep );
    aHit->SetLPos( lPos );
    aHit->SetLMom( lMom );
    aHit->SetXplane( X );
    aHit->SetUplane( U );
    aHit->SetVplane( V );

    DCCollection[DCno]->insert( aHit );

    return true;
  }

  return true;
}

void HKSDCSD::EndOfEvent( G4HCofThisEvent* )
{
}

void HKSDCSD::clear()
{
  for( G4int i=0; i<2; i++ ){
    G4int nHits = DCCollection[i]->entries();
    for( G4int j=0; j<nHits; j++ ){
      delete (*DCCollection[i])[j];
    }
  }
}
void HKSDCSD::DrawAll()
{
  G4VVisManager *pVisMan = G4VVisManager::GetConcreteInstance();

  if( pVisMan ){
    for( int j=0; j<2; j++ ){
      G4int nHits = DCCollection[j]->entries();
      for( G4int i=0; i<nHits; i++ ){
	(*DCCollection[j])[i]->Draw();
      }
    }
  }
}

void HKSDCSD::PrintAll()
{
 
  for( int j=0; j<2; j++ ){
    G4int nHits = DCCollection[j]->entries();
    for( G4int i=0; i<nHits; i++ ){
      (*DCCollection[j])[i]->Print();
    }
  } 
}
