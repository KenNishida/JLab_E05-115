/*
  HKSSSHit.hh

  H.Nomura 2004/Jan/6
*/

#ifndef HKSSSHit_h

#define HKSSSHit_h 1

#include "G4VHit.hh"
#include "G4THitsCollection.hh"
#include "G4ThreeVector.hh"
#include "G4Allocator.hh"

class HKSSSHit : public G4VHit
{
public:
  HKSSSHit( G4int holeID,
	    const G4ThreeVector &lPos,
	    const G4ThreeVector &lMom );
  ~HKSSSHit();

private:
  HKSSSHit( const HKSSSHit & right );
  HKSSSHit & operator == ( const HKSSSHit & );

public:
  int operator == ( const HKSSSHit & ) const
  { return 0; }

  inline void * operator new( size_t size );
  inline void operator delete( void *aHit );

  void Draw();
  void Print();

private:
  G4int Hole_, File_, Row_;
  G4ThreeVector lPos_, lMom_;

public:
  void SetHoleID( G4int id ) { Hole_ = id; }
  void SetFile( G4int id ) { File_ = id; }
  void SetRow( G4int id ) { Row_ = id; }
  void SetLPos( const G4ThreeVector & pos ) { lPos_ = pos; }
  void SetLMom( const G4ThreeVector & mom ) { lMom_ = mom; }

  G4int GetSSID( void ) const { return Hole_; }
  G4int GetFile( void ) const { return File_; }
  G4int GetRow( void )  const { return Row_; }
  const G4ThreeVector & GetLPos() const { return lPos_; }
  const G4ThreeVector & GetLMom() const { return lMom_; }

};

typedef G4THitsCollection<HKSSSHit> HKSSSHitsCollection;

extern G4Allocator<HKSSSHit> HKSSSHitAllocator;

inline void* HKSSSHit::operator new( size_t )
{
  return static_cast<void*>( HKSSSHitAllocator.MallocSingle() );
}

inline void HKSSSHit::operator delete( void* aHit )
{
  HKSSSHitAllocator.FreeSingle
    ( static_cast<HKSSSHit *>( aHit ) );
}

#endif
