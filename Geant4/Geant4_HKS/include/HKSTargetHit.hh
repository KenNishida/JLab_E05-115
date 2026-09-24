/*
  HKSTargetHit.hh

  2003/Oct/31
  H.Nomura
*/

#ifndef HKSTargetHit_h

#define HKSTargetHit_h 1

#include "G4VHit.hh"
#include "G4THitsCollection.hh"
#include "G4ThreeVector.hh"
#include "G4Allocator.hh"

class HKSTargetHit : public G4VHit
{
public:
  HKSTargetHit();
  ~HKSTargetHit();

private:
  HKSTargetHit( const HKSTargetHit & right );
  HKSTargetHit & operator = ( const HKSTargetHit & );

public:
  int operator == ( const HKSTargetHit & ) const
  { return 0; }

  inline void* operator new( size_t size );
  inline void operator delete( void* aHit );

  void Draw();
  void Print();

private:
  G4double edep_;
  G4double time_;
  G4ThreeVector pos_;
  G4ThreeVector mom_;
  G4int trackNo_;
  G4bool fSignal_;

public:
  void SetEdep( G4double Sedep ) { edep_ = Sedep; }
  void AddEdep( G4double Aedep ) { edep_ +=Aedep; }
  void SetTime( G4double Stime ) { time_ = Stime; }
  void SetPos( const G4ThreeVector & pos ) { pos_ = pos; }
  void SetMom( const G4ThreeVector & mom ) { mom_ = mom; }
  void SetTrackNo( G4int No ) { trackNo_ = No; }
  void SetTrueSignal() { fSignal_ = true; }
  void SetFalseSignal() { fSignal_ = false; }

  G4double GetEdep() const { return edep_; }
  G4double GetTime() const { return time_; }
  const G4ThreeVector & GetPos() const { return pos_; }
  const G4ThreeVector & GetMom() const { return mom_; }
  G4int GetTrackNo() const { return trackNo_; }
  G4bool IsTrueSignal() const { return fSignal_; }

};

typedef G4THitsCollection<HKSTargetHit> HKSTargetHitsCollection;

extern G4Allocator<HKSTargetHit> HKSTargetHitAllocator;

inline void* HKSTargetHit::operator new( size_t )
{
  return static_cast<void*>( HKSTargetHitAllocator.MallocSingle() );
}

inline void HKSTargetHit::operator delete( void* aHit )
{
  HKSTargetHitAllocator.FreeSingle
    ( static_cast<HKSTargetHit *>( aHit ) );
}

#endif
