/*
  HKSTrajectory.hh

  2003/Oct/31
  H.Nomura
*/

#ifndef HKSTrajectory_h

#define HKSTrajectory_h 1

#include "globals.hh"
#include "G4VTrajectory.hh"
#include "G4ThreeVector.hh"
#include "G4Allocator.hh"
//#include "g4std/vector"
#include <vector>

class G4Track;
class G4Step;

typedef std::vector <G4VTrajectoryPoint*> TrajectoryPointContainer;

class HKSTrajectory : public G4VTrajectory
{
public:
  HKSTrajectory();
  HKSTrajectory( const G4Track *aTrack );
  ~HKSTrajectory();

  inline void* operator new( std::size_t );
  inline void operator delete( void* );
  int operator == ( const HKSTrajectory &right ) const
  { return ( this == &right ); }


private:
  TrajectoryPointContainer* positionRecord;
  G4int trackID_;
  G4int parentID_;
  G4String particleName_;
  G4double PDGCharge_;
#ifndef Geant44
  G4int PDGEncoding_;
#endif

  G4ThreeVector gpos_, dpos_;
  G4ThreeVector gmom_, dmom_;

public:
  void ShowTrajectory( std::ostream& os ) const;
  void DrawTrajectory( G4int i_mode=0 ) const;
  void AppendStep( const G4Step *aStep );
  int GetPointEntries() const
  { return positionRecord-> size(); }
  G4VTrajectoryPoint* GetPoint( int i ) const
  { return (*positionRecord)[i]; }
  void MergeTrajectory( G4VTrajectory* secondTrajectory );

  G4int GetTrackID( void )  const { return trackID_;  }
  G4int GetParentID( void ) const { return parentID_; }
#ifdef GEANT44
  const G4String & GetParticleName( void ) const
  { return particleName_; }
  G4double GetPDGCharge( void ) const { return PDGCharge_; }
#else
  G4String GetParticleName( void ) const { return particleName_; }
  G4double GetCharge( void ) const { return PDGCharge_; }
  G4int GetPDGEncoding( void ) const { return PDGEncoding_; }
  G4ThreeVector GetInitialMomentum() const { return gmom_; }
#endif

  const G4ThreeVector & GetPositionAtFirst( void ) const 
  { return gpos_; }
  const G4ThreeVector & GetPositionAtLast( void ) const 
  { return dpos_; }
  const G4ThreeVector & GetMomentumAtFirst( void ) const 
  { return gmom_; }
  const G4ThreeVector & GetMomentumAtLast( void ) const 
  { return dmom_; }

  void SetPositionAtLast( const G4ThreeVector &pos ){ dpos_=pos; }
  void SetMomentumAtLast( const G4ThreeVector &mom ){ dmom_=mom; }

};

extern G4Allocator<HKSTrajectory> HKSTrajectoryAllocator;

inline void* HKSTrajectory::operator new( std::size_t )
{
  return static_cast<void*>( HKSTrajectoryAllocator.MallocSingle() );
}

inline void HKSTrajectory::operator delete( void *aHit )
{
  HKSTrajectoryAllocator.FreeSingle
    ( static_cast<HKSTrajectory*>( aHit ) );
}

#endif

