/*
  HksWCSD.cc

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

#include "HKSWCSD.hh"
#include "BeamLineElement.hh"
//#include "HKSParamMan.hh"

#include <sstream>

const G4double Win = 1.33;
const G4double ThresholdVelocity = 1./Win;
const G4double lmin = 300./1000000000.; // wave length at minimum
const G4double lmax = 600./1000000000.; // wave length at maximum
const G4double CE = 0.30; // light collection efficiency of a box;
const G4double QE = 0.15; // quantum efficiency of a PMT

HKSWCSD::HKSWCSD( G4String name )
  : G4VSensitiveDetector(name),
    thBeta( ThresholdVelocity )
{
  for( int i=0; i<2; i++ ){
    std::ostringstream id;
    id << i;
    G4String Name = G4String("WC")+id.str().c_str()+"-Collection";
    collectionName.insert( Name );
    elem_[i] = 0;
  }
}

HKSWCSD::~HKSWCSD()
{
}

void HKSWCSD::SetElement( int id, BeamLineElement *elem )
{
  if( id >= 0 && id <2 ) elem_[id] = elem;
}

void HKSWCSD::Initialize( G4HCofThisEvent *HCE )
{
  for( int i=0; i<2; ++i ){
    WCCollection[i] =
      new HKSWCHitsCollection( SensitiveDetectorName,
			       collectionName[i] );
    G4int HCID = GetCollectionID(i);
    HCE-> AddHitsCollection( HCID, WCCollection[i] );
  }
}

G4bool HKSWCSD::ProcessHits( G4Step *aStep, G4TouchableHistory* )
{
  G4Track *aTrack = aStep-> GetTrack();
  //G4double edep = aStep-> GetTotalEnergyDeposit();
  //  if( edep == 0.0 ) return true;
  //  HKSParamMan *paramMan = HKSParamMan::GetParamMan();
  const G4VTouchable *theTouchable =
    aStep-> GetPreStepPoint()-> GetTouchable();
  G4VPhysicalVolume *WC = theTouchable-> GetVolume();
  G4int SegID = WC-> GetCopyNo();
  //   G4VPhysicalVolume *Wall = WC-> GetMother();
  G4String SDname = WC->GetName();
  G4int wcID = -1;
  if( SDname == "WC1" ) wcID = 0;
  else if( SDname == "WC2" ) wcID = 1;
  G4double velocity = aTrack-> GetVelocity()/c_light;
  G4double npe = 0.0;
  G4double StepLength = 0.;
  G4double Charge=aStep->GetTrack()->GetDefinition()->GetPDGCharge();
  if( wcID>=0 && wcID<2 && elem_[wcID]  && Charge!=0 ){
    G4ThreeVector hitpos = aStep-> GetPreStepPoint()-> GetPosition();
    G4ThreeVector hitmom = aTrack-> GetMomentum();
    G4double hittime = aTrack-> GetGlobalTime();
    G4int trackNo = aTrack-> GetTrackID();
    G4ParticleDefinition *particle =
    aTrack-> GetDynamicParticle()-> GetDefinition();
    G4String particlename =particle-> GetParticleName();

    G4ThreeVector lPos = elem_[wcID]-> GetLocalPosition( hitpos );
    G4ThreeVector lMom = elem_[wcID]-> GetLocalDirection( hitmom );

    const G4int nHits = WCCollection[wcID]-> entries();      // this is the number of hit in layer
    StepLength = aStep->GetStepLength();
    edep = aStep-> GetTotalEnergyDeposit();
    if( velocity >= thBeta ) {
      //	  npe += 2.*pi/137.*(1.-1./1.33/(thBeta)/1.33/(thBeta))*
      //	    (StepLength/cm)*(1/0.0000003-1/0.0000006)*CE*QE;
      npe = 2.*pi/137.*(1.-1./(Win*Win*velocity*velocity))*
	(StepLength/m)*(1./lmin-1./lmax)*CE*QE;
    }
    
    HKSWCHit *aHit;  
    for( G4int i=0; i<nHits; i++ ){   // if there is entry, scan and scan whether same counter or not//
      aHit = (*WCCollection[wcID])[i]; 
      if( SegID == aHit->GetSegID() && 
	  wcID == aHit->GetWCID() ){  
	// if same counter , add current information //
	aHit-> AddEdep( edep );
	aHit-> AddLength( StepLength );
	aHit-> AddNpe( npe );
	return true; 
      }
    }
    // if there is no hit before, or there is no same counter hit, insert current hit in Collectopn//
    aHit = new HKSWCHit( wcID, SegID, hitpos, hitmom,
			 hittime, particlename, particle, velocity );
    aHit-> SetTrackNo( trackNo );
    aHit-> SetLPos( lPos );
    aHit-> SetLMom( lMom );
    aHit-> SetEdep( edep );
    aHit-> SetLength( StepLength );
    aHit-> SetNpe( npe );
    WCCollection[wcID]-> insert( aHit );
    return true; 
  }
  return true;
}

void HKSWCSD::EndOfEvent( G4HCofThisEvent* )
{
}

void HKSWCSD::clear()
{
  for( int i=0; i<2; ++i ){
    G4int nHits = WCCollection[i]-> entries();
    for( G4int j=0; j<nHits; ++j ){
      delete (*WCCollection[i])[j];
    }
  }
}

void HKSWCSD::DrawAll()
{
  G4VVisManager *pVisMan = G4VVisManager::GetConcreteInstance();

  if( pVisMan ){
    for( int j=0; j<2; ++j ){
      G4int nHits = WCCollection[j]-> entries();
      for( G4int i=0; i<nHits; i++ ){
	(*WCCollection[j])[i]-> Draw();
      }
    }
  }
}

void HKSWCSD::PrintAll()
{
  for( int j=0; j<2; ++j ){
    G4int nHits = WCCollection[j]-> entries();
    for( G4int i=0; i<nHits; i++ ){
      (*WCCollection[j])[i]-> Print();
    }
  }
}
