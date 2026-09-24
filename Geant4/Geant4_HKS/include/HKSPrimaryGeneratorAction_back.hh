/*
  HKSPrimaryGeneratorAction.hh

  D.Kawama
*/

#ifndef HKSPrimaryGeneratorAction_h

#define HKSPrimaryGeneratorAction_h 1

#include "globals.hh"
#include "G4VUserPrimaryGeneratorAction.hh"
#include "G4ThreeVector.hh"


class G4ParticleGun;
class G4Event;

class HKSDetectorConstruction;
class HKSAnalysis;

class HKSPrimaryGeneratorAction : public G4VUserPrimaryGeneratorAction
{
public:
  HKSPrimaryGeneratorAction( HKSDetectorConstruction* detector,
			     HKSAnalysis* analysisManager = 0 );
  ~HKSPrimaryGeneratorAction();
  
  virtual void GeneratePrimaries( G4Event* anEvent );
  
private:
  HKSAnalysis* anaMan;
  G4ParticleGun *gunParticle;
  void GenerateBeam( G4Event* anEvent );
  void GenParUni( G4Event* anEvent );
  void GenFromFile( G4Event* anEvent );

  FILE *dataFile;

};

#endif
