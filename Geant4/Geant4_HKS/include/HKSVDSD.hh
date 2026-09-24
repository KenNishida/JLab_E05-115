/*
  HKSVDSD.hh

  D.Kawama
*/

#ifndef HKSVDSD_h

#define HKSVDSD_h 1

#include "G4VSensitiveDetector.hh"
#include "HKSVDHit.hh"

class G4Step;
class G4HCofThisEvent;
class G4TouchableHistory;
class BeamLineElement;

class HKSVDSD : public G4VSensitiveDetector
{
public:
  HKSVDSD( G4String name );
  virtual ~HKSVDSD();

  void Initialize( G4HCofThisEvent *HCE );
  G4bool ProcessHits( G4Step* aStep, G4TouchableHistory* ROhist );
   void EndOfEvent( G4HCofThisEvent* HCE );

   void DrawAll() ;
   void PrintAll();
  void clear();

  void SetElement( int id, BeamLineElement *elem);

private:
  HKSVDHitsCollection* VDCollection[15];
  BeamLineElement *elem_[15];
};

#endif
