/*
  HKSTOFHit.hh

  H.Nomura
  2003/Dec/12
*/

#ifndef HKSTOFHit_h
#define HKSTOFHit_h 1

#include "G4VHit.hh"
#include "G4THitsCollection.hh"
#include "G4ThreeVector.hh"
#include "G4Allocator.hh"

class G4ParticleDefinition;

class HKSTOFHit : public G4VHit
{
public:
  HKSTOFHit( G4int tofNo, G4int id, const G4ThreeVector &gPos, 
	     const G4ThreeVector &gMom,
	     const G4ParticleDefinition *particle,
	     const G4String &name,	
	     const G4double time );

  ~HKSTOFHit();

private:
  HKSTOFHit( const HKSTOFHit &right );
  HKSTOFHit & operator = ( const HKSTOFHit & );

public:
  int operator == ( const HKSTOFHit & ) const
  { return 0; }

  inline void *operator new( size_t );
  inline void operator delete( void *aHit );

  void Draw( void );
  void Print( void );

private:
  G4int segId_, trackNo_;
  G4int tofId_;
  G4double time_, eDep_, Length_;
  G4ThreeVector gPos_, gMom_, lPos_, lMom_;
  G4String name_;
  const G4ParticleDefinition *partDef_;

public:
  void SetSegID( G4int id ) { segId_ = id; }
  void SetTOFID( G4int id ) { tofId_ = id; }
  void SetEdep( G4double Edep ) { eDep_ = Edep; }
  void AddEdep( G4double Edep ) { eDep_ += Edep; }
  void SetLength( G4double Length ) { Length_ = Length; }
  void AddLength( G4double Length ) { Length_ += Length; }
  void SetTime( G4double Time ) { time_ = Time; }
  void SetGPos( G4ThreeVector &pos ) { gPos_ = pos; }
  void SetGMom( G4ThreeVector &mom ) { gMom_ = mom; }
  void SetLPos( G4ThreeVector &pos ) { lPos_ = pos; }
  void SetLMom( G4ThreeVector &mom ) { lMom_ = mom; }
  void SetTrackNo( G4int no ){ trackNo_ = no; }
  void SetParticle( G4String &name ) { name_ = name; }
  void SetParticle( G4ParticleDefinition *part ) { partDef_ = part; }

  G4int GetSegID() const { return segId_; }
  G4int GetTOFID() const { return tofId_; }
  G4int GetTrackNo() const { return trackNo_; }
  const G4ThreeVector &GetGPos() const { return gPos_; }
  const G4ThreeVector &GetLPos() const { return lPos_; }
  const G4ThreeVector &GetGMom() const { return gMom_; }
  const G4ThreeVector &GetLMom() const { return lMom_; }
  const G4double &GetLength() const { return Length_; }
  G4double GetEdep() const { return eDep_; }
  G4double GetTime() const { return time_; }
  const G4String & GetParticleName() const { return name_;}
  const G4ParticleDefinition *GetParticle() const { return partDef_; }

};

typedef G4THitsCollection<HKSTOFHit> HKSTOFHitsCollection;

extern G4Allocator<HKSTOFHit> HKSTOFHitAllocator;

inline void * HKSTOFHit::operator new( size_t )
{
  return static_cast<void *>( HKSTOFHitAllocator.MallocSingle());
}

inline void HKSTOFHit::operator delete( void *aHit )
{
  HKSTOFHitAllocator.FreeSingle( static_cast<HKSTOFHit *>(aHit) );
}

#endif
