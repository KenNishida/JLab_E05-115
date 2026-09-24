/*
  HKSTargetSD.hh

  D.Kawama
*/

#ifndef HKSTargetSD_h

#define HKSTargetSD_h 1

#include "G4VSensitiveDetector.hh"
#include "HKSTargetHit.hh"

class G4Step;
class G4HCofThisEvent;
class G4TouchableHistory;
class BeamLineElement;

class HKSTargetSD : public G4VSensitiveDetector
{
public:
  HKSTargetSD( G4String name );
  virtual ~HKSTargetSD();

  virtual void Initialize( G4HCofThisEvent *HCE );
  virtual G4bool ProcessHits( G4Step* aStep, G4TouchableHistory* ROhist );
  virtual void EndOfEvent( G4HCofThisEvent* HCE );

  virtual void DrawAll() ;
  virtual void PrintAll();
  void clear();

  void SetElement( BeamLineElement *elem )
  { elem_ = elem; }

private:
  HKSTargetHitsCollection* TargetCollection;
  G4double edep;
  BeamLineElement *elem_;
};

#endif
