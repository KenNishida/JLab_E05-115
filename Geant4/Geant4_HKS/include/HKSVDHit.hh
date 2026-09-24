/*
  HKSVDHit.hh

  D.Kawama
*/

#ifndef HKSVDHit_h

#define HKSVDHit_h 1

#include "G4VHit.hh"
#include "G4THitsCollection.hh"
#include "G4ThreeVector.hh"
#include "G4Allocator.hh"

class G4ParticleDefinition;

class HKSVDHit : public G4VHit
{
public:
  HKSVDHit( G4int id, const G4ThreeVector &gPos, const G4ThreeVector &gMom,
	    G4double edep, 
		 G4ThreeVector Vertex_,
		 G4String &name);
  ~HKSVDHit();

private:
  HKSVDHit( const HKSVDHit &  );
  HKSVDHit & operator = ( const HKSVDHit & );

public:
  int operator == ( const HKSVDHit & ) const
  { return 0; }

  inline void* operator new( size_t size );
  inline void operator delete( void* aHit );

  void Draw() ;
  void Print();

private:
  G4int VDId_;
  G4ThreeVector gPos_ , gMom_;
  G4ThreeVector lPos_, lMom_;
  G4double Edep_;
  G4double tLength_;
  G4ThreeVector Vertex_;
  G4String name_;
  const G4ParticleDefinition *partDef_;

public:
  void SetGPos( const G4ThreeVector & pos ) { gPos_ = pos; }
  void SetLPos( const G4ThreeVector & pos ) { lPos_ = pos; }
  void SetLMom( const G4ThreeVector & mom ) { lMom_ = mom; }
  void SetEdep( const G4double & Sedep ) { Edep_ = Sedep; }
  void AddEdep( const G4double & Aedep ) { Edep_ = Aedep; }
  void SetVertex( const G4ThreeVector & ver ) { Vertex_ = ver; }
  void SettLength( const G4double & tlen ) { tLength_ = tlen; }
  void SetParticle( G4String &name ) { name_ = name; }
  void SetParticle( G4ParticleDefinition *part ) { partDef_ = part; }

  const G4ThreeVector & GetPos() const { return lPos_; }
  const G4ThreeVector & GetGPos() const { return gPos_; }
  const G4ThreeVector & GetMom() const { return lMom_; }
  const G4ThreeVector & GetGMom() const { return gMom_; } 
  const G4double & GetEdep() const { return Edep_; } 
  G4int GetVDid( void ) const { return VDId_; }
  const G4ThreeVector & GetVertex() const { return Vertex_; } 
  const G4ParticleDefinition *GetParticle() const { return partDef_; }
  const G4String & GetParticleName() const { return name_;}
  const G4double & GettLength() const { return tLength_; }

};

typedef G4THitsCollection<HKSVDHit> HKSVDHitsCollection;

extern G4Allocator<HKSVDHit> HKSVDHitAllocator;

inline void* HKSVDHit::operator new( size_t )
{
  return static_cast<void*>( HKSVDHitAllocator.MallocSingle() );
}

inline void HKSVDHit::operator delete( void* aHit )
{
  HKSVDHitAllocator.FreeSingle
    ( static_cast<HKSVDHit *>( aHit ) );
}

#endif
