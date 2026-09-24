/*
  HKSTOFSD.hh

  2003/Oct/29
  H.Nomura
*/

#ifndef HKSTOFSD_h

#define HKSTOFSD_h 1

#include "G4VSensitiveDetector.hh"
#include "HKSTOFHit.hh"

class G4Step;
class G4HCofThisEvent;
class G4TouchableHistory;
class BeamLineElement;

class HKSTOFSD : public G4VSensitiveDetector
{
public:
  HKSTOFSD( G4String name );
  ~HKSTOFSD();

  void Initialize( G4HCofThisEvent *HCE );
  G4bool ProcessHits( G4Step* aStep, G4TouchableHistory* ROhist );
  void EndOfEvent( G4HCofThisEvent* HCE );

  void DrawAll();
  void PrintAll();
  void clear();

  //  void SetElement( BeamLineElement *elem );

  void SetElement( int id, BeamLineElement *elem);


private:
  HKSTOFHitsCollection *TOFCollection[3];
  BeamLineElement *elem_[3];
  G4double thTimeSep;
  G4double edep;

};

#endif
