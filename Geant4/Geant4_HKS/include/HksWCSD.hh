/*
  HksWCSD.hh

  H.Nomura  2003/Dec/31
*/

#ifndef HksWCSD_h

#define HksWCSD_h 1

#include "G4VSensitiveDetector.hh"
#include "HksWCHit.hh"

class G4Step;
class G4HCofThisEvent;
class G4TouchableHistory;
class BeamLineElement;

class HksWCSD : public G4VSensitiveDetector
{
public:
  HksWCSD( G4String name );
  ~HksWCSD();

  void Initialize( G4HCofThisEvent *HCE );
  G4bool ProcessHits( G4Step* aStep, G4TouchableHistory* ROhist );
  void EndOfEvent( G4HCofThisEvent* HCE );

  void DrawAll();
  void PrintAll();
  void clear();

  void SetElement( int id, BeamLineElement *elem);

private:
  HksWCHitsCollection* WCCollection[2];
  BeamLineElement *elem_[2];
  G4double thBeta;
};

#endif
