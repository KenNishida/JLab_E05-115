/*
  HKSWCHit.hh

  H.Nomura  2003/Dec/31

*/

#ifndef HKSWCHit_h

#define HKSWCHit_h 1

#include "G4VHit.hh"
#include "G4THitsCollection.hh"
#include "G4ThreeVector.hh"
#include "G4Allocator.hh"

class G4ParticleDefinition;
class HKSWCHit : public G4VHit
{
public:
  HKSWCHit( G4int wallID, G4int SegID,
	    const G4ThreeVector &gPos, const G4ThreeVector &gMom,
	    const G4double time, 
	    const G4String &name,
	    const G4ParticleDefinition *paricle,
	    G4double velocity);
  ~HKSWCHit(){}

private:
  HKSWCHit( const HKSWCHit & right );
  HKSWCHit & operator = ( const HKSWCHit & );

public:
  int operator == ( const HKSWCHit & ) const
  { return 0; }

  inline void* operator new( size_t size );
  inline void operator delete( void* aHit );

  void Draw();
  void Print();

private:
  G4int wallID_, segID_;
  G4int trackNo_;
  G4ThreeVector gPos_ , gMom_;
  G4ThreeVector lPos_, lMom_;
  G4double edep_, Time_, beta_;
  G4bool fSignal_;
  const G4ParticleDefinition *part_;
//   G4ParticleDefinition *part_;
  G4double Length_;
  G4double npe_;
  G4String name_;

public:
  void SetWCID( G4int id ) { wallID_ = id; }
  void SetSegID( G4int id ) { segID_ = id; }
  void SetTrackNo( G4int id ) { trackNo_ = id; }
  void SetGPos( const G4ThreeVector & pos ) { gPos_ = pos; }
  void SetGMom( const G4ThreeVector & mom ) { gMom_ = mom; }
  void SetLPos( const G4ThreeVector & pos ) { lPos_ = pos; }
  void SetLMom( const G4ThreeVector & mom ) { lMom_ = mom; }
  void SetEdep( G4double E ) { edep_ = E; }
  void AddEdep( G4double E ) { edep_ += E; }
  void SetTime( G4double T ) { Time_ = T; }
  void SetBeta( G4double v ) { beta_ = v; }
  void SetTrueSignal() { fSignal_ = true; }
  void SetFalseSignal() { fSignal_ = false; }
  void SetParticle( G4ParticleDefinition *par ) { part_=par; }
  void SetLength( G4double len ) { Length_=len; }
  void AddLength( G4double len ) { Length_+=len; }
  void SetNpe( G4double N ) { npe_ = N; }
  void AddNpe( G4double N ) { npe_ += N; }
  void SetParticle( G4String &name ) { name_ = name; }

  G4int GetWCID( void ) const { return wallID_; }
  G4int GetSegID( void ) const { return segID_; }
  G4int GetTrackNo( void ) const { return trackNo_; }
  const G4ThreeVector & GetGPos() const { return gPos_; }
  const G4ThreeVector & GetGMom() const { return gMom_; }
  const G4ThreeVector & GetLPos() const { return lPos_; }
  const G4ThreeVector & GetLMom() const { return lMom_; }
  G4double GetEdep() const { return edep_; }
  G4double GetTime() const { return Time_; }
  G4double GetBeta() const { return beta_; }
  G4bool IsTrueSignal() const { return fSignal_; }
  const G4ParticleDefinition *GetParticle() const { return part_; }
  G4double GetLength() const { return Length_; }
  G4double GetNpe() const { return npe_; }
  const G4String &GetParticleName() const { return name_;}
};

typedef G4THitsCollection<HKSWCHit> HKSWCHitsCollection;

extern G4Allocator<HKSWCHit> HKSWCHitAllocator;

inline void* HKSWCHit::operator new( size_t )
{
  return static_cast<void*>( HKSWCHitAllocator.MallocSingle() );
}

inline void HKSWCHit::operator delete( void* aHit )
{
  HKSWCHitAllocator.FreeSingle
    ( static_cast<HKSWCHit *>( aHit ) );
}

#endif
