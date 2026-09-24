/*
  HKSDCSD.hh

  H.Nomura 2003/Jan/6
*/

#ifndef HKSDCSD_h

#define HKSDCSD_h 1

#include "G4VSensitiveDetector.hh"
#include "HKSDCHit.hh"

class G4Step;
class G4HCofThisEvent;
class G4TouchableHistory;
class BeamLineElement;

class HKSDCSD : public G4VSensitiveDetector
{
public:
  HKSDCSD( G4String name );
  ~HKSDCSD();

  void Initialize( G4HCofThisEvent *HCE );
  G4bool ProcessHits( G4Step* aStep, G4TouchableHistory* ROhist );
  void EndOfEvent( G4HCofThisEvent* HCE );

  void DrawAll();
  void PrintAll();
  void clear();

  void SetElement( int id, BeamLineElement *elem);

private:
  HKSDCHitsCollection* DCCollection[2];
  BeamLineElement *elem_[2];
  //HKSDCHitsCollection* DCCollection;
  //  BeamLineElement *elem_;
  G4double thEdep;

public:
  //void SetElement( BeamLineElement *elem)
  //{ elem_ = elem; }

};

#endif
