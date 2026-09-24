/*
  HKSACSD.hh

  H.Nomura  2003/Dec/31
*/

#ifndef HKSACSD_h

#define HKSACSD_h 1

#include "G4VSensitiveDetector.hh"
#include "HKSACHit.hh"

class G4Step;
class G4HCofThisEvent;
class G4TouchableHistory;
class BeamLineElement;

class HKSACSD : public G4VSensitiveDetector
{
public:
  HKSACSD( G4String name );
  ~HKSACSD();

  void Initialize( G4HCofThisEvent *HCE );
  G4bool ProcessHits( G4Step* aStep, G4TouchableHistory* ROhist );
  void EndOfEvent( G4HCofThisEvent* HCE );

  void DrawAll();
  void PrintAll();
  void clear();

  void SetElement( int id, BeamLineElement *elem);
  //void SetElement( BeamLineElement *elem )
  //{ elem_ = elem; }

private:
  HKSACHitsCollection* ACCollection[3];
//   HKSACHitsCollection* ACCollection;
  //BeamLineElement *elem_;
  BeamLineElement *elem_[2];
  G4double thBeta;
  G4double edep;
};

#endif
