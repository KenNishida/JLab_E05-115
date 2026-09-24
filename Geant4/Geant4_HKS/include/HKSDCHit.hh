/*
  HKSDCHit.hh

  H.Nomura 2004/Jan/6
*/

#ifndef HKSDCHit_h

#define HKSDCHit_h 1

#include "G4VHit.hh"
#include "G4THitsCollection.hh"
#include "G4ThreeVector.hh"
#include "G4Allocator.hh"

class HKSDCHit : public G4VHit
{
public:
  HKSDCHit( G4int dcID, G4int layerID,
	    const G4ThreeVector &gPos,
	    const G4ThreeVector &lPos,
	    G4int track, G4double time );
  ~HKSDCHit();

private:
  HKSDCHit( const HKSDCHit & right );
  HKSDCHit & operator == ( const HKSDCHit & );

public:
  int operator == ( const HKSDCHit & ) const
  { return 0; }

  inline void * operator new( size_t size );
  inline void operator delete( void *aHit );

  void Draw();
  void Print();

private:
  G4int DC_, Layer_, trackID_;
  G4double edep_, Time_, lX, lU, lV;
  G4ThreeVector gPos_, gMom_, lPos_, lMom_;
  G4bool fSignal_;

public:
  void SetDCID( G4int id ) { DC_ = id; }
  void SetLayerID( G4int id ) { Layer_ = id; }
  void SetTrackNo( G4int id ) { trackID_ = id; }
  void SetGPos( const G4ThreeVector & pos ) { gPos_ = pos; }
  void SetGMom( const G4ThreeVector & mom ) { gMom_ = mom; }
  void SetLPos( const G4ThreeVector & pos ) { lPos_ = pos; }
  void SetLMom( const G4ThreeVector & mom ) { lMom_ = mom; }
  void SetEdep( G4double E ) { edep_ = E; }
  void AddEdep( G4double E ) { edep_ += E; }
  void SetTime( G4double T ) { Time_ = T; }
  void SetXplane( G4double x ) { lX = x;};
  void SetUplane( G4double u ) { lU = u;};
  void SetVplane( G4double v ) { lV = v;};
  void SetTrueSignal() { fSignal_ = true; }
  void SetFalseSignal() { fSignal_ = false; }

  G4int GetDCID( void ) const { return DC_; }
  G4int GetLayerID( void ) const { return Layer_; }
  G4int GetTrackNo( void ) const { return trackID_; }
  const G4ThreeVector & GetGPos() const { return gPos_; }
  const G4ThreeVector & GetGMom() const { return gMom_; }
  const G4ThreeVector & GetLPos() const { return lPos_; }
  const G4ThreeVector & GetLMom() const { return lMom_; }
  G4double GetEdep() const { return edep_; }
  G4double GetTime() const { return Time_; }
  G4double GetXplane() const { return lX; }
  G4double GetUplane() const { return lU; }
  G4double GetVplane() const { return lV; }
  G4bool IsTrueSignal() const { return fSignal_; }

};

typedef G4THitsCollection<HKSDCHit> HKSDCHitsCollection;

extern G4Allocator<HKSDCHit> HKSDCHitAllocator;

inline void* HKSDCHit::operator new( size_t )
{
  return static_cast<void*>( HKSDCHitAllocator.MallocSingle() );
}

inline void HKSDCHit::operator delete( void* aHit )
{
  HKSDCHitAllocator.FreeSingle
    ( static_cast<HKSDCHit *>( aHit ) );
}

#endif
