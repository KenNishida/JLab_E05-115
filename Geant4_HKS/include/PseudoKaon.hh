/*
  PseudoKaon.hh

  H.Nomura 2004/Jan/7
*/

#ifndef PseudoKaon_h
#define PseudoKaon_h 1

#include "globals.hh"
#include "G4ios.hh"
#include "G4VBoson.hh"

//#########################################
//###         Pseudo Kaon Plus          ###
//#########################################

class PseudoKaon : public G4VBoson
{
private:
  static PseudoKaon thePseudoKaon;

private:
  PseudoKaon( const G4String  aName,  G4double     mass,
	      G4double        width,  G4double   charge,
	      G4int           iSpin,  G4int     iParity,
	      G4int    iConjugation,  G4int    iIsospin,
	      G4int       iIsospin3,  G4int     gParity,
	      const G4String &pType,  G4int      lepton,
	      G4int          baryon,  G4int    encoding,
	      G4bool         stable,  G4double lifetime,
	      G4DecayTable *decaytable );

public:
  virtual ~PseudoKaon(){}

  static PseudoKaon * PsKaonDefinition();
  static PseudoKaon * PsKaon();

};

#endif
