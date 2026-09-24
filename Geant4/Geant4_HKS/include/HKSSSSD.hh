/*
  HKSSSSD.hh

  H.Nomura 2004/Sep/22
*/

#ifndef HKSSSSD_h
#define HKSSSSD_h 1

#include "G4VSensitiveDetector.hh"
#include "HKSSSHit.hh"

class G4Step;
class G4HCofThisEvent;
class G4TouchableHistory;
class BeamLineElement;

class HKSSSSD : public G4VSensitiveDetector
{
public:
  HKSSSSD( G4String name );
  ~HKSSSSD();

  void Initialize( G4HCofThisEvent *HCE );
  G4bool ProcessHits( G4Step *aStep,
		      G4TouchableHistory *ROhist );
  void EndOfEvent( G4HCofThisEvent *HCE );

  void DrawAll();
  void PrintAll();
  void clear();

private:
  HKSSSHitsCollection *SSCollection;
  BeamLineElement *elem_;

public:
  void SetElement( BeamLineElement *elem )
  { elem_ = elem; }

};

#endif
